-module(chat_history_app).
-vsn("1.0.0").
-behaviour(application).

-export([
    start/2,
    stop/1
]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_Type, _Args) ->
    webserver(),
    chat_history_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    _ = lager:info("Stopping Webserver (~p).", [_State]),
    ok = cowboy:stop_listener(chat_history),
    ok.

-spec env(atom(), any()) -> term().
env(Key, Default) ->
    application:get_env(chat_history, Key, Default).

%% @doc Start up the webserver.
-spec webserver() -> ok.
webserver() ->
    Port         = env(port, 4001),
    Acceptors    = env(number_of_acceptors, 10),

    _ = lager:info("Starting Webserver on port ~p with ~p acceptors each.",
                   [Port, Acceptors]),

    ProtoOpts = #{
        env => #{dispatch => dispatch()},
        middlewares => [opencensus_cowboy2_context, cowboy_router, cowboy_handler],
        metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
        stream_handlers => [cowboy_metrics_h, cowboy_tracer_h, cowboy_stream_h]
    },

    {ok, _Pid1} = cowboy:start_clear(
        chat_history,
        [{port, as_integer(Port)}, {num_acceptors, Acceptors}],
        ProtoOpts
    ),
    ok.

load_routes() ->
    [{"/metrics/[:registry]", prometheus_cowboy2_handler, []} | chat_history:routes()].

dispatch() ->
    Routes = load_routes(),
    cowboy_router:compile([{'_', Routes}]).

as_integer(N) when is_integer(N) -> N;
as_integer(N) when is_list(N) -> list_to_integer(N).
