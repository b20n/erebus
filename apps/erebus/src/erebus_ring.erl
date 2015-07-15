-module(erebus_ring).

-include_lib("erebus/include/erebus.hrl").

-export([
    find_or_create/1
]).

%% TODO: Distributed systems
-spec find_or_create(Metric) -> {ok, Pid} when
    Metric :: metric(),
    Pid :: pid().

find_or_create(#metric{id=undefined, int=Props}=M0) ->
    M1 = M0#metric{id=base64:encode(term_to_binary(lists:sort(Props)))},
    find_or_create(M1);
find_or_create(#metric{id=ID}=M) ->
    case gproc:where({n, l, ID}) of
        undefined ->
            erebus_cache_sup:start_cache(M);
        Pid ->
            {ok, Pid}
    end.
