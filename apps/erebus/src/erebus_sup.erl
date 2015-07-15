-module(erebus_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CacheSup = {
        erebus_cache_sup,
        {erebus_cache_sup, start_link, []},
        permanent, infinity, supervisor, [erebus_cache_sup]
    },
    {ok, {{one_for_one, 5, 10}, [CacheSup]}}.

