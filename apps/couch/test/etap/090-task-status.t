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
    etap:plan(16),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

check_status(Pid,ListPropLists) ->
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
        [], ListPropLists
    ),
    couch_util:get_value(status,hd(Element)).

loop() ->
    receive
    {add, From} ->
        Resp = couch_task_status:add_task("type", "task", "init"),
        From ! {ok, self(), Resp},
        loop();
    {update, Status, From} ->
        Resp = couch_task_status:update(Status),
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

    ok = call(Pid1, add),
    etap:is(
        length(couch_task_status:all()),
        1,
        "Started a task"
    ),

    etap:is(
        call(Pid1, add),
        {add_task_error, already_registered},
        "Unable to register multiple tasks for a single Pid."
    ),

    etap:is(
        check_status(Pid1, couch_task_status:all()),
        <<"init">>,
        "Task status was set to 'init'."
    ),

    call(Pid1,update,"running"),
    etap:is(
        check_status(Pid1,couch_task_status:all()),
        <<"running">>,
        "Status updated to 'running'."
    ),


    call(Pid2,add),
    etap:is(
        length(couch_task_status:all()),
        2,
        "Started a second task."
    ),

    etap:is(
        check_status(Pid2, couch_task_status:all()),
        <<"init">>,
        "Second tasks's status was set to 'init'."
    ),

    call(Pid2, update, "running"),
    etap:is(
        check_status(Pid2, couch_task_status:all()),
        <<"running">>,
        "Second task's status updated to 'running'."
    ),


    call(Pid3, add),
    etap:is(
        length(couch_task_status:all()),
        3,
        "Registered a third task."
    ),

    etap:is(
        check_status(Pid3, couch_task_status:all()),
        <<"init">>,
        "Third tasks's status was set to 'init'."
    ),

    call(Pid3, update, "running"),
    etap:is(
        check_status(Pid3, couch_task_status:all()),
        <<"running">>,
        "Third task's status updated to 'running'."
    ),


    call(Pid3, update_frequency, 500),
    call(Pid3, update, "still running"),
    etap:is(
        check_status(Pid3, couch_task_status:all()),
        <<"still running">>,
        "Third task's status updated to 'still running'."
    ),

    call(Pid3, update, "skip this update"),
    etap:is(
        check_status(Pid3, couch_task_status:all()),
        <<"still running">>,
        "Status update dropped because of frequency limit."
    ),

    call(Pid3, update_frequency, 0),
    call(Pid3, update, "don't skip"),
    etap:is(
        check_status(Pid3, couch_task_status:all()),
        <<"don't skip">>,
        "Status updated after reseting frequency limit."
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
