-module(chat_history).

-export([routes/0,
         store_messages/1,
         get_messages/1]).

routes() ->
  [
   {"/history", chat_history_h, []}
  ].


get_store() ->
  {ok, Barrel} = barrel:open_barrel(
                           application:get_env(chat_history, store, <<"chat_history">>)
                          ),
  Barrel.

store_messages(Messages0) ->
  Messages =
    [#{ <<"id">> => <<"message-", (barrel_id:binary_id(62))/binary>>,
        <<"message">> => Message }
     || Message <- Messages0],

  io:format("~p~n", [Messages]),

  barrel:save_docs(get_store(), Messages).

get_messages(Cursor) ->
  Path = [<<"message">>, <<"id">>],
  Options0 = #{ begin_key => Path,
                end_key => Path ++ [<< 16#ff, 16#ff >>] },

  {Options, Add} = case Cursor of
                     {Count, before, CID} ->
                       { Options0#{end_key => Path ++ [CID],
                                   end_or_equal => false,
                                   limit => Count,
                                   reverse => true }, fun add_rev/2 };
                     {infinity, 'after', CID} ->
                       { Options0#{begin_key => Path ++ [CID],
                                   begin_or_equal => false }, fun add_fwd/2 };
                     {Count, 'after', CID} ->
                       { Options0#{begin_key => Path ++ [CID],
                                   begin_or_equal => false,
                                   limit => Count,
                                   reverse => false }, fun add_fwd/2 };
                     {Count, most_recent} ->
                       { Options0#{ limit => Count,
                                    reverse => true }, fun add_rev/2 }
                   end,
  {ok, Barrel} = barrel:open_barrel(<<"chat_history">>),
  FilterFun =
    fun
      (Row = #{ id := <<"message-", _MsgId/binary >> = DocId }, Msgs) ->
        io:format("got message ~p~n", [Row]),
        case barrel:fetch_doc(Barrel, DocId, Options) of
          {ok, Msg} ->
             {ok, Add(Msg, Msgs)};
          _Else ->
            skip
        end;
      (_Doc, _Acc) ->
        stop
    end,
  %ok = barrel_view:await_refresh(<<"chat_history">>, <<"messages">>, 10000),
  Messages = barrel:fold_view(<<"chat_history">>, <<"messages">>, FilterFun, [], Options),
  ok = barrel_view:await_refresh(<<"chat_history">>, <<"messages">>, 10000),
  Messages.

add_fwd(Msg, Msgs) -> [Msg | Msgs].
add_rev(Msg, Msgs) -> Msgs ++ [Msg].


