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

% This module allows is used to track the status of long running tasks.
% Long running tasks register (add_task/3) then update their status (update/1)
% and the task and status is added to tasks list. When the tracked task dies
% it will be automatically removed the tracking. To get the tasks list, use the
% all/0 function

-export([start_link/0, stop/0]).
-export([all/0, add_task/3, update/1, update/2, set_update_frequency/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-import(couch_util, [to_binary/1]).

-include("couch_db.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:cast(?MODULE, stop).


all() ->
    gen_server:call(?MODULE, all).


add_task(Type, TaskName, StatusText) ->
    put(task_status_update, {{0, 0, 0}, 0}),
    Msg = {
        add_task,
        to_binary(Type),
        to_binary(TaskName),
        to_binary(StatusText)
    },
    gen_server:call(?MODULE, Msg).


set_update_frequency(Msecs) ->
    put(task_status_update, {{0, 0, 0}, Msecs * 1000}).


update(StatusText) ->
    update("~s", [StatusText]).

update(Format, Data) ->
    {LastUpdateTime, Frequency} = get(task_status_update),
    case timer:now_diff(Now = now(), LastUpdateTime) >= Frequency of
    true ->
        put(task_status_update, {Now, Frequency}),
        Msg = ?l2b(io_lib:format(Format, Data)),
        gen_server:cast(?MODULE, {update_status, self(), Msg});
    false ->
        ok
    end.


init([]) ->
    % read configuration settings and register for configuration changes
    ets:new(?MODULE, [ordered_set, protected, named_table]),
    {ok, nil}.


terminate(_Reason,_State) ->
    ok.


handle_call({add_task, Type, TaskName, StatusText}, {From, _}, Server) ->
    case ets:lookup(?MODULE, From) of
    [] ->
        true = ets:insert(?MODULE, {From, {Type, TaskName, StatusText}}),
        erlang:monitor(process, From),
        {reply, ok, Server};
    [_] ->
        {reply, {add_task_error, already_registered}, Server}
    end;
handle_call(all, _, Server) ->
    All = [
        [
            {type, Type},
            {task, Task},
            {status, Status},
            {pid, ?l2b(pid_to_list(Pid))}
        ]
        ||
        {Pid, {Type, Task, Status}} <- ets:tab2list(?MODULE)
    ],
    {reply, All, Server}.


handle_cast({update_status, Pid, StatusText}, Server) ->
    [{Pid, {Type, TaskName, _StatusText}}] = ets:lookup(?MODULE, Pid),
    ?LOG_DEBUG("New task status for ~s: ~s",[TaskName, StatusText]),
    true = ets:insert(?MODULE, {Pid, {Type, TaskName, StatusText}}),
    {noreply, Server};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, Server) ->
    %% should we also erlang:demonitor(_MonitorRef), ?
    ets:delete(?MODULE, Pid),
    {noreply, Server}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

