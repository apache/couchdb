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

-module(fabric_db_update_listener).

-export([go/4, start_update_notifier/1, stop/1, wait_db_updated/1]).
-export([handle_db_event/3]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

-record(worker, {
    ref,
    node,
    pid
}).

-record(cb_state, {
    client_pid,
    client_ref,
    notify
}).

-record(acc, {
    parent,
    state,
    shards
}).

go(Parent, ParentRef, DbName, Timeout) ->
    Shards = mem3:shards(DbName),
    Notifiers = start_update_notifiers(Shards),
    MonRefs = lists:usort([rexi_utils:server_pid(N) || #worker{node = N} <- Notifiers]),
    RexiMon = rexi_monitor:start(MonRefs),
    MonPid = start_cleanup_monitor(self(), Notifiers),
    %% This is not a common pattern for rexi but to enable the calling
    %% process to communicate via handle_message/3 we "fake" it as a
    %% a spawned worker.
    Workers = [#worker{ref=ParentRef, pid=Parent} | Notifiers],
    Acc = #acc{
        parent = Parent,
        state = unset,
        shards = Shards
    },
    Resp = try
        receive_results(Workers, Acc, Timeout)
    after
        rexi_monitor:stop(RexiMon),
        stop_cleanup_monitor(MonPid)
    end,
    case Resp of
        {ok, _} -> ok;
        {error, Error} -> erlang:error(Error);
        Error -> erlang:error(Error)
    end.

start_update_notifiers(Shards) ->
    EndPointDict = lists:foldl(fun(#shard{node=Node, name=Name}, Acc) ->
        dict:append(Node, Name, Acc)
    end, dict:new(), Shards),
    lists:map(fun({Node, DbNames}) ->
        Ref = rexi:cast(Node, {?MODULE, start_update_notifier, [DbNames]}),
        #worker{ref=Ref, node=Node}
    end, dict:to_list(EndPointDict)).

% rexi endpoint
start_update_notifier(DbNames) ->
    {Caller, Ref} = get(rexi_from),
    Notify = config:get("couchdb", "maintenance_mode", "false") /= "true",
    State = #cb_state{client_pid = Caller, client_ref = Ref, notify = Notify},
    Options = [{parent, Caller}, {dbnames, DbNames}],
    couch_event:listen(?MODULE, handle_db_event, State, Options).

handle_db_event(_DbName, updated, #cb_state{notify = true} = St) ->
    erlang:send(St#cb_state.client_pid, {St#cb_state.client_ref, db_updated}),
    {ok, St};
handle_db_event(_DbName, deleted, St) ->
    {stop, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.

start_cleanup_monitor(Parent, Notifiers) ->
    spawn(fun() ->
        Ref = erlang:monitor(process, Parent),
        cleanup_monitor(Parent, Ref, Notifiers)
    end).

stop_cleanup_monitor(MonPid) ->
    MonPid ! {self(), stop}.

cleanup_monitor(Parent, Ref, Notifiers) ->
    receive
        {'DOWN', Ref, _, _, _} ->
            stop_update_notifiers(Notifiers);
        {Parent, stop} ->
            stop_update_notifiers(Notifiers);
        Else ->
            couch_log:error("Unkown message in ~w :: ~w", [?MODULE, Else]),
            stop_update_notifiers(Notifiers),
            exit(Parent, {unknown_message, Else})
    end.

stop_update_notifiers(Notifiers) ->
    [rexi:kill(Node, Ref) || #worker{node=Node, ref=Ref} <- Notifiers].

stop({Pid, Ref}) ->
    erlang:send(Pid, {Ref, done}).

wait_db_updated({Pid, Ref}) ->
    MonRef = erlang:monitor(process, Pid),
    erlang:send(Pid, {Ref, get_state}),
    receive
        {state, Pid, State} ->
            erlang:demonitor(MonRef, [flush]),
            State;
        {'DOWN', MonRef, process, Pid, Reason} ->
            throw({changes_feed_died, Reason})
    after 300000 ->
        ?MODULE:wait_db_updated({Pid, Ref})
    end.

receive_results(Workers, Acc0, Timeout) ->
    Fun = fun handle_message/3,
    case rexi_utils:recv(Workers, #worker.ref, Fun, Acc0, infinity, Timeout) of
    {timeout, #acc{state=updated}=Acc} ->
        receive_results(Workers, Acc, Timeout);
    {timeout, #acc{state=waiting}=Acc} ->
        erlang:send(Acc#acc.parent, {state, self(), timeout}),
        receive_results(Workers, Acc#acc{state=unset}, Timeout);
    {timeout, Acc} ->
        receive_results(Workers, Acc#acc{state=timeout}, Timeout);
    {_, Acc} ->
        {ok, Acc}
    end.


handle_message({rexi_DOWN, _, {_, Node}, _}, _Worker, Acc) ->
    handle_error(Node, {nodedown, Node}, Acc);
handle_message({rexi_EXIT, _Reason}, Worker, Acc) ->
    handle_error(Worker#worker.node, {worker_exit, Worker}, Acc);
handle_message({gen_event_EXIT, Node, Reason}, _Worker, Acc) ->
    handle_error(Node, {gen_event_EXIT, Node, Reason}, Acc);
handle_message(db_updated, _Worker, #acc{state=waiting}=Acc) ->
    % propagate message to calling controller
    erlang:send(Acc#acc.parent, {state, self(), updated}),
    {ok, Acc#acc{state=unset}};
handle_message(db_updated, _Worker, Acc) ->
    {ok, Acc#acc{state=updated}};
handle_message(get_state, _Worker, #acc{state=unset}=Acc) ->
    {ok, Acc#acc{state=waiting}};
handle_message(get_state, _Worker, Acc) ->
    erlang:send(Acc#acc.parent, {state, self(), Acc#acc.state}),
    {ok, Acc#acc{state=unset}};
handle_message(done, _, _) ->
    {stop, ok}.


handle_error(Node, Reason, #acc{shards = Shards} = Acc) ->
    Rest = lists:filter(fun(#shard{node = N}) -> N /= Node end, Shards),
    case fabric_view:is_progress_possible([{R, nil} || R <- Rest]) of
        true ->
            {ok, Acc#acc{shards = Rest}};
        false ->
            {error, Reason}
    end.
