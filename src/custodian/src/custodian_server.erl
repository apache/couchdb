% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_server).
-behaviour(gen_server).
-include_lib("mem3/include/mem3.hrl").

% public api.
-export([start_link/0, truly_down/0]).

% gen_server api.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2]).

% private records.
-record(state, {
    heartbeat,
    down
}).

% public functions.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

truly_down() ->
    gen_server:call(?MODULE, truly_down).

% gen_server functions.
init(_) ->
    net_kernel:monitor_nodes(true),
    {ok, update_heartbeat(init_down(#state{}))}.

handle_call(truly_down, _From, #state{down=Down}=State) ->
    {reply, {ok, truly_down(Down)}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    {noreply, not_down(Node, State)};
handle_info({nodedown, Node}, State) ->
    {noreply, down(Node, State)};
handle_info(heartbeat, State) ->
    spawn(fun() -> custodian:scan() end),
    {noreply, update_heartbeat(State)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions

init_down(#state{}=State) ->
    Now = os:timestamp(),
    Down = dict:from_list([{Node, Now} ||
        Node <- mem3:nodes() -- [node()|nodes()]]),
    State#state{down=Down}.

not_down(Node, #state{down=Down}=State) ->
    State#state{down=dict:erase(Node, Down)}.

down(Node, #state{down=Down}=State) ->
    State#state{down=dict:store(Node, os:timestamp(), Down)}.

update_heartbeat(#state{heartbeat=undefined}=State) ->
    HeartbeatSecs = list_to_integer(
        couch_config:get("custodian", "heartbeat_secs", "60")),
    TimerRef = erlang:send_after(HeartbeatSecs * 1000, self(), heartbeat),
    State#state{heartbeat=TimerRef};
update_heartbeat(#state{heartbeat=TimerRef}=State) ->
    erlang:cancel_timer(TimerRef),
    update_heartbeat(State#state{heartbeat=undefined}).

truly_down(Down) ->
    Now = os:timestamp(),
    TimeoutSecs = list_to_integer(
        couch_config:get("custodian", "timeout_secs", "172800")),
    Fun = fun(Node, LastSeen, Acc) ->
        case timer:now_diff(Now, LastSeen) > TimeoutSecs * 1000000 of
            true ->
                [Node|Acc];
            false ->
                Acc
        end
    end,
    dict:fold(Fun, [], Down).
