-module(erebus_http_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/metrics", erebus_http_metrics_handler, []}]}
    ]),
    cowboy:start_http(
        erebus_http,
        100,
        [{port, 9000}],
        [
            {env, [{dispatch, Dispatch}]}
        ]
    ),
    erebus_http_sup:start_link().


stop(_State) ->
    ok.
