-module(pfor).
-on_load(init/0).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    new/1,
    update/3,
    read/3,
    to_list/1
]).

-include_lib("erebus/include/erebus.hrl").

-opaque pfor() :: term().
-export_type([pfor/0]).

-define(
    NOTLOADED,
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})
).

-spec new(Interval) -> Pfor when
    Interval :: interval(),
    Pfor :: pfor().

new(_) ->
    ?NOTLOADED.


-spec update(Pfor, Timestamp, Value) -> ok when
    Pfor :: pfor(),
    Timestamp :: timestamp(),
    Value :: value().

update(_, _, _) ->
    ?NOTLOADED.


-spec read(Pfor, From, Until) -> Read when
    Pfor :: pfor(),
    From :: timestamp(),
    Until :: timestamp(),
    Read :: any(). %% TODO

read(_, _, _) ->
    ?NOTLOADED.


-spec to_list(Pfor) -> List when
    Pfor :: pfor(),
    List :: any(). %% TODO

to_list(_) ->
    ?NOTLOADED.


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EBinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EBinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "pfor"), 0).
