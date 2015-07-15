-module(erebus_cache).
-behaviour(gen_server).

-include_lib("erebus/include/erebus.hrl").

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    update/2
]).

-record(st, {
    metric :: metric(),
    windows :: list(window())
}).

update(Pid, Points) ->
    gen_server:call(Pid, {update, Points}).


start_link(Metric) ->
    gen_server:start_link(?MODULE, Metric, []).


init(Metric) ->
    true = gproc:reg({n, l, Metric#metric.id}, ignored),
    Windows = lists:map(
        fun(W) -> W#window{data=codecs:new(W)} end,
        erebus_config:windows(Metric)
    ),
    {ok, #st{metric=Metric, windows=Windows}}.


handle_call({update, Points}, _From, #st{windows=Ws}=State) ->
    ok = lists:foreach(fun(W) -> ok = codecs:update(W, Points) end, Ws),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
