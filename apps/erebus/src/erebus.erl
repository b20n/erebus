-module(erebus).

-include_lib("erebus/include/erebus.hrl").

-export([
    start/0,
    stop/0,
    update/2
]).

start() ->
    application:start(gproc),
    application:start(erebus).


stop() ->
    ok.


-spec update(Metric, Points) -> ok | {error, Error} when
    Metric :: metric(),
    Points :: any(),
    Error :: malformed_metric.

update(#metric{id=undefined, int=undefined}, _) ->
    {error, malformed_metric};
update(Metric, Points) ->
    {ok, Pid} = erebus_ring:find_or_create(Metric),
    ok = erebus_cache:update(Pid, Points).
