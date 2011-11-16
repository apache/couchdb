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

-module(couch_task_status).
-behaviour(gen_server).

% This module is used to track the status of long running tasks.
% Long running tasks register themselves, via a call to add_task/1, and then
% update their status properties via update/1. The status of a task is a
% list of properties. Each property is a tuple, with the first element being
% either an atom or a binary and the second element must be an EJSON value. When
% a task updates its status, it can override some or all of its properties.
% The properties {started_on, UnitTimestamp}, {updated_on, UnixTimestamp} and
% {pid, ErlangPid} are automatically added by this module.
% When a tracked task dies, its status will be automatically removed from
% memory. To get the tasks list, call the all/0 function.

-export([start_link/0, stop/0]).
-export([all/0, add_task/1, update/1, get/1, set_update_frequency/1]).
-export([is_task_added/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("couch_db.hrl").

-define(set(L, K, V), lists:keystore(K, 1, L, {K, V})).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:cast(?MODULE, stop).


all() ->
    gen_server:call(?MODULE, all).


add_task(Props) ->
    put(task_status_update, {{0, 0, 0}, 0}),
    Ts = timestamp(),
    TaskProps = lists:ukeysort(
        1, [{started_on, Ts}, {updated_on, Ts} | Props]),
    put(task_status_props, TaskProps),
    gen_server:call(?MODULE, {add_task, TaskProps}).


is_task_added() ->
    is_list(erlang:get(task_status_props)).


set_update_frequency(Msecs) ->
    put(task_status_update, {{0, 0, 0}, Msecs * 1000}).


update(Props) ->
    MergeProps = lists:ukeysort(1, Props),
    TaskProps = lists:ukeymerge(1, MergeProps, erlang:get(task_status_props)),
    put(task_status_props, TaskProps),
    maybe_persist(TaskProps).


get(Props) when is_list(Props) ->
    TaskProps = erlang:get(task_status_props),
    [couch_util:get_value(P, TaskProps) || P <- Props];
get(Prop) ->
    TaskProps = erlang:get(task_status_props),
    couch_util:get_value(Prop, TaskProps).


maybe_persist(TaskProps0) ->
    {LastUpdateTime, Frequency} = erlang:get(task_status_update),
    case timer:now_diff(Now = now(), LastUpdateTime) >= Frequency of
    true ->
        put(task_status_update, {Now, Frequency}),
        TaskProps = ?set(TaskProps0, updated_on, timestamp(Now)),
        gen_server:cast(?MODULE, {update_status, self(), TaskProps});
    false ->
        ok
    end.


init([]) ->
    % read configuration settings and register for configuration changes
    ets:new(?MODULE, [ordered_set, protected, named_table]),
    {ok, nil}.


terminate(_Reason,_State) ->
    ok.


handle_call({add_task, TaskProps}, {From, _}, Server) ->
    case ets:lookup(?MODULE, From) of
    [] ->
        true = ets:insert(?MODULE, {From, TaskProps}),
        erlang:monitor(process, From),
        {reply, ok, Server};
    [_] ->
        {reply, {add_task_error, already_registered}, Server}
    end;
handle_call(all, _, Server) ->
    All = [
        [{pid, ?l2b(pid_to_list(Pid))} | TaskProps]
        ||
        {Pid, TaskProps} <- ets:tab2list(?MODULE)
    ],
    {reply, All, Server}.


handle_cast({update_status, Pid, NewProps}, Server) ->
    case ets:lookup(?MODULE, Pid) of
    [{Pid, _CurProps}] ->
        ?LOG_DEBUG("New task status for ~p: ~p", [Pid, NewProps]),
        true = ets:insert(?MODULE, {Pid, NewProps});
    _ ->
        % Task finished/died in the meanwhile and we must have received
        % a monitor message before this call - ignore.
        ok
    end,
    {noreply, Server};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, Server) ->
    %% should we also erlang:demonitor(_MonitorRef), ?
    ets:delete(?MODULE, Pid),
    {noreply, Server}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


timestamp() ->
    timestamp(now()).

timestamp({Mega, Secs, _}) ->
    Mega * 1000000 + Secs.
