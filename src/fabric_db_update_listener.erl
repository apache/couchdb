% Copyright 2010 Cloudant
%
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

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

-record(worker, {
    ref,
    node,
    pid
}).

-record(acc, {
    parent,
    state
}).

go(Parent, ParentRef, DbName, Timeout) ->
    Notifiers = start_update_notifiers(DbName),
    MonRefs = lists:usort([{rexi_server, Node} || {Node, _Ref} <- Notifiers]),
    RexiMon = rexi_monitor:start(MonRefs),
    MonPid = start_cleanup_monitor(self(), Notifiers),
    %% This is not a common pattern for rexi but to enable the calling
    %% process to communicate via handle_message/3 we "fake" it as a
    %% a spawned worker.
    Workers = [#worker{ref=ParentRef, pid=Parent} | Notifiers],
    Resp = try
        receive_results(Workers, #acc{parent=Parent, state=unset}, Timeout)
    after
        rexi_monitor:stop(RexiMon),
        stop_cleanup_monitor(MonPid)
    end,
    case Resp of
        {ok, _} -> ok;
        {error, Error} -> erlang:error(Error);
        Error -> erlang:error(Error)
    end.

start_update_notifiers(DbName) ->
    EndPointDict = lists:foldl(fun(#shard{node=Node, name=Name}, Acc) ->
        dict:append(Node, Name, Acc)
    end, dict:new(), mem3:shards(DbName)),
    lists:map(fun({Node, DbNames}) ->
        Ref = rexi:cast(Node, {?MODULE, start_update_notifier, [DbNames]}),
        #worker{ref=Ref, node=Node}
    end, dict:to_list(EndPointDict)).

% rexi endpoint
start_update_notifier(DbNames) ->
    {Caller, Ref} = get(rexi_from),
    Fun = fun({_, X}) ->
        case lists:member(X, DbNames) of
            true -> erlang:send(Caller, {Ref, db_updated});
            false -> ok
        end
    end,
    Id = {couch_db_update_notifier, make_ref()},
    ok = gen_event:add_sup_handler(couch_db_update, Id, Fun),
    receive {gen_event_EXIT, Id, Reason} ->
        rexi:reply({gen_event_EXIT, node(), Reason})
    end.

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
            twig:log(error, "Unkown message in ~w :: ~w", [?MODULE, Else]),
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


handle_message({rexi_DOWN, _, {_, Node}, _}, _Worker, _Acc) ->
    {error, {nodedown, Node}};
handle_message({rexi_EXIT, _Reason}, Worker, _Acc) ->
    {error, {worker_exit, Worker}};
handle_message({gen_event_EXIT, Node, Reason}, _Worker, _Acc) ->
    {error, {gen_event_exit, Node, Reason}};
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



