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
-module(rexi_gov_manager).

-behaviour(gen_server).
-vsn(1).
-behaviour(config_listener).

% API
-export([start_link/0, send/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([handle_config_change/5]).

-record(state, {node_timers = ets:new(timers, [set]),
                nodeout_timeout = 2000,
                pid_spawn_max = 10000}).


% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Dest, Msg) ->
    case erlang:send(Dest, Msg, [noconnect, nosuspend]) of
    ok -> ok;
    _ ->
        % treat nosuspend and noconnect the same
        {ok, Governor} = get_governor(get_node(Dest)),
        gen_server:cast(Governor, {spawn_and_track, Dest, Msg})
    end.

get_node({_, Node}) when is_atom(Node) ->
    Node;
get_node(Pid) when is_pid(Pid) ->
    node(Pid).

get_governor(Node) ->
    case ets:lookup(govs, Node) of
    [{Node, Gov}] ->
        {ok, Gov};
    [] ->
        gen_server:call(?MODULE, {get_governor, Node})
    end.

% gen_server callbacks

init([]) ->
    ets:new(govs, [named_table, set, {read_concurrency, true}]),
    net_kernel:monitor_nodes(true),
    NodeOutTimeout = config:get("rexi","nodeout_timeout","500"),
    PidSpawnMax = config:get("rexi","pid_spawn_max", "10000"),
    State = #state{
        nodeout_timeout = list_to_integer(NodeOutTimeout),
        pid_spawn_max = list_to_integer(PidSpawnMax)
    },
    config:listen_for_changes(?MODULE, State),
    {ok, State}.

handle_config_change("rexi", "nodeout_timeout", Value, _, State) ->
    IntValue = list_to_integer(Value),
    %% Setting the timeout is cheap, no need to check if it actually changed
    gen_server:call(?MODULE, {set_timeout, IntValue}),
    {ok, State#state{nodeout_timeout = IntValue}};
handle_config_change("rexi", "pid_spawn_max", Value, _, State) ->
    IntValue = list_to_integer(Value),
    %% Setting the timeout is cheap, no need to check if it actually changed
    gen_server:call(?MODULE, {set_spawn_max, IntValue}),
    {ok, State#state{pid_spawn_max = IntValue}};
handle_config_change(_, _, _, _, State) ->
    {ok, State}.

handle_call({set_timeout, TO}, _, #state{nodeout_timeout = Old} = State) ->
    {reply, Old, State#state{nodeout_timeout = TO}};
handle_call({set_spawn_max, Max}, _, #state{pid_spawn_max = Old} = State) ->
    {reply, Old, State#state{pid_spawn_max = Max}};
handle_call({get_governor, Node}, _From,
            #state{pid_spawn_max = PidSpawnMax} = State) ->
    case ets:lookup(govs, Node) of
    [] ->
        {ok, Gov} = gen_server:start_link(rexi_governor, [PidSpawnMax], []),
        ets:insert(govs, {Node, Gov});
    [{Node, Gov}] ->
        Gov
    end,
    {reply, {ok, Gov}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, #state{node_timers = Timers,
                                   pid_spawn_max = PidSpawnMax} = State) ->
    case ets:lookup(Timers, Node) of
    [{Node, TRef}] ->
        erlang:cancel_timer(TRef),
        ets:delete(Timers, Node);
    _ ->
        ok
    end,
    case ets:lookup(govs, Node) of
    [{Node, _}] ->
        ok;
    [] ->
        {ok, Gov} = gen_server:start_link(rexi_governor, [PidSpawnMax], []),
        ets:insert(govs, {Node, Gov})
    end,
    {noreply, State};

handle_info({nodedown, Node}, #state{node_timers = Timers,
                                     nodeout_timeout = NodeTimeout} = State) ->
    case ets:lookup(Timers, Node) of
    [] ->
        TRef = erlang:send_after(NodeTimeout, self(), {nodeout, Node}),
        ets:insert(Timers, {Node, TRef}),
        {noreply, State};
    _ ->
        {noreply, State}
    end;

handle_info({nodeout, Node}, #state{node_timers = Timers} = State) ->
    % check for race with node up
    case ets:member(Timers, Node) of
    true ->
        ets:delete(Timers, Node),
        case ets:lookup(govs, Node) of
        [] ->
            ok;
        [{Node, Governor}] ->
            gen_server:cast(Governor, nodeout)
        end;
    false ->
        ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions
