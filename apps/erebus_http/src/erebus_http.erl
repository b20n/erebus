-module(erebus_http).

-export([
    start/0,
    stop/0
]).

start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(erebus_http).


stop() ->
    ok.
