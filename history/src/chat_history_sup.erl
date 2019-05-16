-module(chat_history_sup).
-vsn("1.0.0").
-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(term()) -> ignore | {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init(_Args) ->

  %% start barrel
  Name = application:get_env(chat_history, store, <<"chat_history">>),
  ok = maybe_create_barrel(barrel:create_barrel(Name)),

  {ok, _Pid} = barrel:start_view(Name,<<"messages">>, barrel_ars_view, #{}),

  Restart_Strategy = one_for_one,
  Max_Restarts = 5,
  In_Seconds = 10,
  Children = [],
  {ok, {
        {Restart_Strategy, Max_Restarts, In_Seconds},
        Children
       }}.


maybe_create_barrel(ok) -> ok;
maybe_create_barrel({error, barrel_already_exists}) -> ok;
maybe_create_barrel(Error) -> Error.
