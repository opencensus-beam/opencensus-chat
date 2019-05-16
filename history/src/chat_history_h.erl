-module(chat_history_h).

-export([init/2]).
-export([terminate/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([from_messages_json_v1/2]).
-export([to_messages_json_v1/2]).
-export([is_authorized/2]).

-record(state, {

               }).

-define(MFA_SPAN_NAME,
        iolist_to_binary(
          io_lib:format("~s:~s/~p",
                        [?MODULE_STRING, ?FUNCTION_NAME, ?FUNCTION_ARITY]))).

-define(START_SPAN,
        _ = ocp:with_child_span(?MFA_SPAN_NAME)
       ).

-define(START_SPAN(Attrs),
        _ = ocp:with_child_span(?MFA_SPAN_NAME, Attrs)
       ).

-define(START_SPAN(SpanName, Attrs),
        _ = ocp:with_child_span(SpanName, Attrs)
       ).

-define(FINISH_SPAN,
        ocp:finish_span()
       ).


-define(INLINE_SPAN(SpanName, Attrs),
        ?START_SPAN(SpanName, Attrs), ?FINISH_SPAN
       ).
-define(INLINE_SPAN(Attrs),
        ?START_SPAN(Attrs), ?FINISH_SPAN
       ).

-define(INLINE_SPAN,
        ?START_SPAN, ?FINISH_SPAN
       ).

-define(WITH_SPAN(Body),
        begin
          ?START_SPAN,
          try
            Body
          after
            ?FINISH_SPAN
          end
        end
       ).
-define(WITH_SPAN(Attrs, Body),
        begin
          ?START_SPAN(Attrs),
          try
            Body
          after
            ?FINISH_SPAN
          end
        end
       ).
-define(WITH_SPAN(SpanName, Attrs, Body),
        begin
          ?START_SPAN(SpanName, Attrs),
          try
            Body
          after
            ?FINISH_SPAN
          end
        end
       ).

init(Req, _) ->
  Method = cowboy_req:method(Req),
  Uri = iolist_to_binary(cowboy_req:uri(Req)),
  ?START_SPAN(#{
                <<"log">> => iolist_to_binary(
                               io_lib:format(<<"Handler ~p, method: ~p, uri: ~p">>,
                                             [?MODULE, Method, Uri])),
                <<"method">> => Method,
                <<"uri">> => Uri,
                <<"headers">> => jsx:encode(
                                   maps:without([<<"authorization">>], cowboy_req:headers(Req)))
               }),
  {cowboy_rest, Req, #state{}}.

terminate(_, _, _) ->
  ?FINISH_SPAN,
  ok.

allowed_methods(Req, State) ->
  ?INLINE_SPAN(#{
                 <<"log">> => <<"check allowed methods">>
                }),
  {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
  ?INLINE_SPAN(#{
                 <<"log">> => <<"check content types accepted">>
                }),
  {[
    {<<"application/json">>, from_messages_json_v1}
   ], Req, State}.

content_types_provided(Req, State) ->
  ?INLINE_SPAN(#{
                 <<"log">> => <<"check content types provided">>
                }),
  {[
    {<<"application/json">>, to_messages_json_v1}
   ], Req, State}.

is_authorized(Req, State) ->
  ?START_SPAN,
  Res = {true, Req, State#state{}},
  ?FINISH_SPAN,
  Res.

from_messages_json_v1(Req0, State = #state{}) ->
  ?START_SPAN(#{
                <<"log">> => <<"extract messages from json">>
               }),
  {ok, ReqBody, Req1} = cowboy_req:read_body(Req0),
  Res = case chat_messages:from_json(ReqBody) of
          {error, _} ->
            _ = ocp:put_attribute(<<"messages-saved">>, <<"FALSE">>),
            Req2 = cowboy_req:reply(400, Req1),
            {stop, Req2, State};
          {ok, Messages} ->
            case chat_history:store_messages(Messages) of
              {ok, _} ->
                _ = ocp:put_attribute(<<"messages-saved">>, <<"TRUE">>),
                Req2 = cowboy_req:set_resp_body("OK", Req1),
                {true, Req2, State};
              _ ->
                _ = ocp:put_attribute(<<"messages-saved">>, <<"FALSE">>),
                Req2 = cowboy_req:reply(500, Req1),
                {stop, Req2, State}
            end
        end,
  ?FINISH_SPAN,
  Res.

to_messages_json_v1(Req0, State=#state{}) ->
  ?START_SPAN(#{
                <<"log">> => <<"convert messages to json">>
               }),
  QsVals0 = cowboy_req:parse_qs(Req0),
  Res = case params(QsVals0) of
          {ok, Cursor} ->
            case chat_history:get_messages(Cursor) of
              [] ->
                %% Return a 204 No Content when the list is empty.
                Req = cowboy_req:reply(204, Req0),
                {stop, Req, State};
              Messages ->
                JSON = chat_messages:to_json(Messages),
                ocp:put_attribute(<<"messages">>, JSON),
                {JSON, Req0, State}
            end;
          {error, _Reason} ->
            Req = cowboy_req:reply(400, Req0),
            {stop, Req, State}
        end,
  ?FINISH_SPAN,
  Res.

params(QsVals) ->
  ?START_SPAN(#{
                <<"log">> => <<"prepare params">>
               }),
  Res = try
          Cursor = get_cursor(QsVals),
          ok = validate_cursor(Cursor),
          {ok, Cursor}
        catch
          throw:{invalid_parameter, ParamName} ->
            Reason = param_validation_problem(ParamName),
            ocp:put_attribute(<<"log">>, <<"params validation: ERROR">>),
            ocp:put_attribute(<<"error">>, jsx:encode(Reason)),
            {error, Reason}
        end,
  ?FINISH_SPAN,
  Res.

get_cursor(QsVals) ->
  Count = try
            case proplists:get_value(<<"count">>, QsVals) of
              undefined -> 5;
              Count0 -> binary_to_integer(Count0)
            end
          catch
            error:badarg ->
              throw_invalid_param(count)
          end,
  case proplists:get_value(<<"after">>, QsVals) of
    undefined ->
      case proplists:get_value(<<"before">>, QsVals) of
        undefined -> {Count, most_recent};
        Before -> {Count, before, Before}
      end;
    After -> {Count, 'after', After}
  end.

validate_cursor({Count, _, _}) when Count =< 0 ->
  throw_invalid_param(count);

validate_cursor(_) ->
  ok.

-spec throw_invalid_param(ParamName :: atom()) -> no_return().
throw_invalid_param(ParamName) ->
  throw({invalid_parameter, ParamName}).

param_validation_problem(count) ->
  invalid_parameter_count;

%% @TODO deal with unmapped errors
param_validation_problem(_ParamName) ->
  invalid_parameter.
