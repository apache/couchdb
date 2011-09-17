#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    test_util:init_code_path(),
    etap:plan(28),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

get_task_prop(Pid, Prop) ->
    From = list_to_binary(pid_to_list(Pid)),
    Element = lists:foldl(
        fun(PropList,Acc) ->
            case couch_util:get_value(pid,PropList) of
                From ->
                    [PropList | Acc];
                _ ->
                    []
            end
        end,
        [], couch_task_status:all()
    ),
    case couch_util:get_value(Prop, hd(Element), nil) of
    nil ->
        etap:bail("Could not get property '" ++ couch_util:to_list(Prop) ++
            "' for task " ++ pid_to_list(Pid));
    Value ->
        Value
    end.

now_ts() ->
    {Mega, Secs, _} = erlang:now(),
    Mega * 1000000 + Secs.

loop() ->
    receive
    {add, Props, From} ->
        Resp = couch_task_status:add_task(Props),
        From ! {ok, self(), Resp},
        loop();
    {update, Props, From} ->
        Resp = couch_task_status:update(Props),
        From ! {ok, self(), Resp},
        loop();
    {update_frequency, Msecs, From} ->
        Resp = couch_task_status:set_update_frequency(Msecs),
        From ! {ok, self(), Resp},
        loop();
    {done, From} ->
        From ! {ok, self(), ok}
    end.

call(Pid, Command) ->
    Pid ! {Command, self()},
    wait(Pid).

call(Pid, Command, Arg) ->
    Pid ! {Command, Arg, self()},
    wait(Pid).

wait(Pid) ->
    receive
        {ok, Pid, Msg} -> Msg
    after 1000 ->
        throw(timeout_error)
    end.

test() ->
    {ok, TaskStatusPid} = couch_task_status:start_link(),

    TaskUpdater = fun() -> loop() end,
    % create three updaters
    Pid1 = spawn(TaskUpdater),
    Pid2 = spawn(TaskUpdater),
    Pid3 = spawn(TaskUpdater),

    ok = call(Pid1, add, [{type, replication}, {progress, 0}]),
    etap:is(
        length(couch_task_status:all()),
        1,
        "Started a task"
    ),
    Task1StartTime = get_task_prop(Pid1, started_on),
    etap:is(
        is_integer(Task1StartTime),
        true,
        "Task start time is defined."
    ),
    etap:is(
        get_task_prop(Pid1, updated_on),
        Task1StartTime,
        "Task's start time is the same as the update time before an update."
    ),

    etap:is(
        call(Pid1, add, [{type, compaction}, {progress, 0}]),
        {add_task_error, already_registered},
        "Unable to register multiple tasks for a single Pid."
    ),

    etap:is(
        get_task_prop(Pid1, type),
        replication,
        "Task type is 'replication'."
    ),
    etap:is(
        get_task_prop(Pid1, progress),
        0,
        "Task progress is 0."
    ),

    ok = timer:sleep(1000),
    call(Pid1, update, [{progress, 25}]),
    etap:is(
        get_task_prop(Pid1, progress),
        25,
        "Task progress is 25."
    ),
    etap:is(
        get_task_prop(Pid1, updated_on) > Task1StartTime,
        true,
        "Task's last update time has increased after an update."
    ),

    call(Pid2, add, [{type, compaction}, {progress, 0}]),
    etap:is(
        length(couch_task_status:all()),
        2,
        "Started a second task."
    ),
    Task2StartTime = get_task_prop(Pid2, started_on),
    etap:is(
        is_integer(Task2StartTime),
        true,
        "Second task's start time is defined."
    ),
    etap:is(
        get_task_prop(Pid2, updated_on),
        Task2StartTime,
        "Second task's start time is the same as the update time before an update."
    ),

    etap:is(
        get_task_prop(Pid2, type),
        compaction,
        "Second task's type is 'compaction'."
    ),
    etap:is(
        get_task_prop(Pid2, progress),
        0,
        "Second task's progress is 0."
    ),

    ok = timer:sleep(1000),
    call(Pid2, update, [{progress, 33}]),
    etap:is(
        get_task_prop(Pid2, progress),
        33,
        "Second task's progress updated to 33."
    ),
    etap:is(
        get_task_prop(Pid2, updated_on) > Task2StartTime,
        true,
        "Second task's last update time has increased after an update."
    ),

    call(Pid3, add, [{type, indexer}, {progress, 0}]),
    etap:is(
        length(couch_task_status:all()),
        3,
        "Registered a third task."
    ),
    Task3StartTime = get_task_prop(Pid3, started_on),
    etap:is(
        is_integer(Task3StartTime),
        true,
        "Third task's start time is defined."
    ),
    etap:is(
        get_task_prop(Pid3, updated_on),
        Task3StartTime,
        "Third task's start time is the same as the update time before an update."
    ),

    etap:is(
        get_task_prop(Pid3, type),
        indexer,
        "Third task's type is 'indexer'."
    ),
    etap:is(
        get_task_prop(Pid3, progress),
        0,
        "Third task's progress is 0."
    ),

    ok = timer:sleep(1000),
    call(Pid3, update, [{progress, 50}]),
    etap:is(
        get_task_prop(Pid3, progress),
        50,
        "Third task's progress updated to 50."
    ),
    etap:is(
        get_task_prop(Pid3, updated_on) > Task3StartTime,
        true,
        "Third task's last update time has increased after an update."
    ),

    call(Pid3, update_frequency, 500),
    call(Pid3, update, [{progress, 66}]),
    etap:is(
        get_task_prop(Pid3, progress),
        66,
        "Third task's progress updated to 66."
    ),

    call(Pid3, update, [{progress, 67}]),
    etap:is(
        get_task_prop(Pid3, progress),
        66,
        "Task update dropped because of frequency limit."
    ),

    call(Pid3, update_frequency, 0),
    call(Pid3, update, [{progress, 77}]),
    etap:is(
        get_task_prop(Pid3, progress),
        77,
        "Task updated after reseting frequency limit."
    ),


    call(Pid1, done),
    etap:is(
        length(couch_task_status:all()),
        2,
        "First task finished."
    ),

    call(Pid2, done),
    etap:is(
        length(couch_task_status:all()),
        1,
        "Second task finished."
    ),

    call(Pid3, done),
    etap:is(
        length(couch_task_status:all()),
        0,
        "Third task finished."
    ),

    erlang:monitor(process, TaskStatusPid),
    couch_task_status:stop(),
    receive
        {'DOWN', _, _, TaskStatusPid, _} ->
            ok
    after
        1000 ->
            throw(timeout_error)
    end,

    ok.
