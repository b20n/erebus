-module(erebus_cache_sup).
-behaviour(supervisor).

-include_lib("erebus/include/erebus.hrl").

-export([start_cache/1]).

-export([start_link/0, init/1]).

-spec start_cache(Metric) -> {ok, Pid} when
    Metric :: metric(),
    Pid :: pid().

start_cache(Metric) ->
    Spec = {
        Metric#metric.id,
        {erebus_cache, start_link, [Metric]},
        transient, 5000, worker, [erebus_cache]
    },
    case supervisor:start_child(?MODULE, Spec) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Other ->
            Other
    end.


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    {ok, {{one_for_one, 10, 10}, []}}.
