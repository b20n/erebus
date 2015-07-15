-module(erebus_config).
%% TODO
%% -behaviour(gen_server).

-include_lib("erebus/include/erebus.hrl").

-export([
    windows/1
]).

-spec windows(Metric) -> Windows when
    Metric :: metric(),
    Windows :: list(window()).

%% TODO: regexes on props, etc etc
windows(_Metric) ->
    [
        #window{
            i = 10,
            mem = 86400,
            disk = 86400,
            codec = pfor
        }
    ].
