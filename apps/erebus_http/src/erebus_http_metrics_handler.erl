-module(erebus_http_metrics_handler).

-include_lib("erebus/include/erebus.hrl").

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req0, State) ->
    Updates = [{#metric{id = <<"asdf">>}, [{1234, 12}, {1235, 10}]}],
    {Code, Body} = case update(Updates) of
        ok -> {204, <<>>};
        Else -> {400, Else}
    end,
    {ok, Req1} = cowboy_req:reply(
        Code,
        [],
        Body,
        Req0
    ),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

update([]) ->
    ok;
update([{Metric, Points}|Us]) ->
    case erebus:update(Metric, Points) of
        {error, _}=E ->
            E;
        ok ->
            update(Us)
    end.
