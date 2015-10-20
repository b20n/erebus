-module(geras).
-on_load(init/0).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    new/1,
    update/3,
    read/3
]).

-include_lib("erebus/include/erebus.hrl").

-opaque geras() :: term().
-export_type([geras/0]).

-define(
    NOTLOADED,
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})
).

-spec new(Interval) -> Geras when
    Interval :: interval(),
    Geras :: geras().

new(_) ->
    ?NOTLOADED.


-spec update(Geras, Timestamp, Value) -> ok when
    Geras :: geras(),
    Timestamp :: timestamp(),
    Value :: value().

update(_, _, _) ->
    ?NOTLOADED.


-spec read(Geras, From, Until) -> Read when
    Geras :: geras(),
    From :: timestamp(),
    Until :: timestamp(),
    Read :: any(). %% TODO

read(_, _, _) ->
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
    erlang:load_nif(filename:join(PrivDir, "geras"), 0).
