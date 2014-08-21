% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
-module(rexi_buffer).

-behaviour(gen_server).
-vsn(1).

%  gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([
    send/2,
    start_link/1
]).

-record(state, {
    buffer = queue:new(),
    sender = nil,
    count = 0,
    max_count
}).

start_link(ServerId) ->
    gen_server:start_link({local, ServerId}, ?MODULE, nil, []).

send(Dest, Msg) ->
    Server = list_to_atom(lists:concat([rexi_buffer, "_", get_node(Dest)])),
    gen_server:cast(Server, {deliver, Dest, Msg}).


init(_) ->
    %% TODO Leverage os_mon to discover available memory in the system
    Max = list_to_integer(config:get("rexi", "buffer_count", "2000")),
    {ok, #state{max_count = Max}}.

handle_call(erase_buffer, _From, State) ->
    {reply, ok, State#state{buffer = queue:new(), count = 0}, 0};

handle_call(get_buffered_count, _From, State) ->
    {reply, State#state.count, State, 0}.

handle_cast({deliver, Dest, Msg}, #state{buffer = Q, count = C} = State) ->
    couch_stats:increment_counter([rexi, buffered]),
    Q2 = queue:in({Dest, Msg}, Q),
    case should_drop(State) of
    true ->
        couch_stats:increment_counter([rexi, dropped]),
            {noreply, State#state{buffer = queue:drop(Q2)}, 0};
    false ->
            {noreply, State#state{buffer = Q2, count = C+1}, 0}
    end.

handle_info(timeout, #state{sender = nil, buffer = {[],[]}, count = 0}=State) ->
    {noreply, State};
handle_info(timeout, #state{sender = nil, count = C} = State) when C > 0 ->
    #state{buffer = Q, count = C} = State,
    {{value, {Dest, Msg}}, Q2} = queue:out_r(Q),
    NewState = State#state{buffer = Q2, count = C-1},
    case erlang:send(Dest, Msg, [noconnect, nosuspend]) of
        ok when C =:= 1 ->
            % We just sent the last queued messsage, we'll use this opportunity
            % to hibernate the process and run a garbage collection
            {noreply, NewState, hibernate};
        ok when C > 1 ->
            % Use a zero timeout to recurse into this handler ASAP
            {noreply, NewState, 0};
        _Else ->
            % We're experiencing delays, keep buffering internally
            Sender = spawn_monitor(erlang, send, [Dest, Msg]),
            {noreply, NewState#state{sender = Sender}}
    end;
handle_info(timeout, State) ->
    % Waiting on a sender to return
    {noreply, State};

handle_info({'DOWN', Ref, _, Pid, _}, #state{sender = {Pid, Ref}} = State) ->
    {noreply, State#state{sender = nil}, 0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, {state, Buffer, Sender, Count}, _Extra) ->
    Max = list_to_integer(config:get("rexi", "buffer_count", "2000")),
    {ok, #state{buffer=Buffer, sender=Sender, count=Count, max_count=Max}};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

should_drop(#state{count = Count, max_count = Max}) ->
    Count >= Max.

get_node({_, Node}) when is_atom(Node) ->
    Node;
get_node(Pid) when is_pid(Pid) ->
    node(Pid).
