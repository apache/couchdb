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

-module(couch_task_status_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(TIMEOUT, 1000).

setup() ->
    Ctx = test_util:start(?MODULE, [couch_log], [{dont_mock, [config]}]),
    {ok, TaskStatusPid} = couch_task_status:start_link(),
    TaskUpdaterPid = spawn(fun() -> loop() end),
    {TaskStatusPid, TaskUpdaterPid, Ctx}.

teardown({TaskStatusPid, TaskUpdaterPid, Ctx}) ->
    test_util:stop_sync_throw(
        TaskStatusPid,
        fun() ->
            couch_task_status:stop()
        end,
        timeout_error,
        ?TIMEOUT
    ),
    catch exit(TaskUpdaterPid, kill),
    test_util:stop(Ctx).

couch_task_status_test_() ->
    {
        "CouchDB task status updates",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(should_register_task),
                ?TDEF_FE(should_set_task_startup_time),
                ?TDEF_FE(should_have_update_time_as_startup_before_any_progress),
                ?TDEF_FE(should_set_task_type),
                ?TDEF_FE(should_not_register_multiple_tasks_for_same_pid),
                ?TDEF_FE(should_set_task_progress),
                ?TDEF_FE(should_update_task_progress),
                ?TDEF_FE(should_update_time_changes_on_task_progress),
                %% ?TDEF_FE(should_control_update_frequency/1,
                ?TDEF_FE(should_reset_control_update_frequency),
                ?TDEF_FE(should_track_multiple_tasks),
                ?TDEF_FE(should_finish_task)
            ]
        }
    }.

should_register_task({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    ?assertEqual(1, length(all_tasks_sync())).

should_set_task_startup_time({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    ?assert(is_integer(get_task_prop(Pid, started_on))).

should_have_update_time_as_startup_before_any_progress({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    StartTime = get_task_prop(Pid, started_on),
    ?assertEqual(StartTime, get_task_prop(Pid, updated_on)).

should_set_task_type({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    ?assertEqual(replication, get_task_prop(Pid, type)).

should_not_register_multiple_tasks_for_same_pid({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    ?assertEqual(
        {add_task_error, already_registered},
        call(Pid, add, [{type, compaction}, {progress, 0}])
    ).

should_set_task_progress({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    ?assertEqual(0, get_task_prop(Pid, progress)).

should_update_task_progress({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    call(Pid, update, [{progress, 25}]),
    ?assertEqual(25, get_task_prop(Pid, progress)).

should_update_time_changes_on_task_progress({_, Pid, _Ctx}) ->
    ?assert(
        begin
            ok = call(Pid, add, [{type, replication}, {progress, 0}]),
            % sleep awhile to customize update time
            ok = timer:sleep(1000),
            call(Pid, update, [{progress, 25}]),
            get_task_prop(Pid, updated_on) > get_task_prop(Pid, started_on)
        end
    ).

%%should_control_update_frequency({_, Pid, _Ctx}) ->
%%    ?assertEqual(66,
%%        begin
%%            ok = call(Pid, add, [{type, replication}, {progress, 0}]),
%%            call(Pid, update, [{progress, 50}]),
%%            call(Pid, update_frequency, 500),
%%            call(Pid, update, [{progress, 66}]),
%%            call(Pid, update, [{progress, 77}]),
%%            get_task_prop(Pid, progress)
%%        end).

should_reset_control_update_frequency({_, Pid, _Ctx}) ->
    ?assertEqual(
        87,
        begin
            ok = call(Pid, add, [{type, replication}, {progress, 0}]),
            call(Pid, update, [{progress, 50}]),
            call(Pid, update_frequency, 500),
            call(Pid, update, [{progress, 66}]),
            call(Pid, update, [{progress, 77}]),
            call(Pid, update_frequency, 0),
            call(Pid, update, [{progress, 87}]),
            get_task_prop(Pid, progress)
        end
    ).

should_finish_task({_, Pid, _Ctx}) ->
    ok = call(Pid, add, [{type, replication}, {progress, 0}]),
    ?assertEqual(1, length(all_tasks_sync())),
    ok = call(Pid, done),
    ?assertEqual(0, length(all_tasks_sync())).

should_track_multiple_tasks(_) ->
    Pid1 = spawn(fun() -> loop() end),
    Pid2 = spawn(fun() -> loop() end),
    Pid3 = spawn(fun() -> loop() end),
    call(Pid1, add, [{type, replication}, {progress, 0}]),
    call(Pid2, add, [{type, compaction}, {progress, 0}]),
    call(Pid3, add, [{type, indexer}, {progress, 0}]),

    ?assertEqual(3, length(all_tasks_sync())),
    ?assertEqual(replication, get_task_prop(Pid1, type)),
    ?assertEqual(compaction, get_task_prop(Pid2, type)),
    ?assertEqual(indexer, get_task_prop(Pid3, type)),

    call(Pid2, update, [{progress, 33}]),
    call(Pid3, update, [{progress, 42}]),
    call(Pid1, update, [{progress, 11}]),
    ?assertEqual(42, get_task_prop(Pid3, progress)),
    call(Pid1, update, [{progress, 72}]),
    ?assertEqual(72, get_task_prop(Pid1, progress)),
    ?assertEqual(33, get_task_prop(Pid2, progress)),

    call(Pid1, done),
    ?assertEqual(2, length(all_tasks_sync())),
    call(Pid3, done),
    ?assertEqual(1, length(all_tasks_sync())),
    call(Pid2, done),
    ?assertEqual(0, length(all_tasks_sync())).

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

call(Pid, done) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {done, self()},
    Res = wait(Pid),
    receive
        {'DOWN', Ref, _Type, Pid, _Info} ->
            Res
    after ?TIMEOUT ->
        throw(timeout_error)
    end;
call(Pid, Command) ->
    Pid ! {Command, self()},
    wait(Pid).

call(Pid, Command, Arg) ->
    Pid ! {Command, Arg, self()},
    wait(Pid).

wait(Pid) ->
    receive
        {ok, Pid, Msg} ->
            Msg
    after ?TIMEOUT ->
        throw(timeout_error)
    end.

get_task_prop(Pid, Prop) ->
    From = list_to_binary(pid_to_list(Pid)),
    Element = lists:foldl(
        fun(PropList, Acc) ->
            case couch_util:get_value(pid, PropList) of
                From ->
                    [PropList | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        all_tasks_sync()
    ),
    case couch_util:get_value(Prop, hd(Element), nil) of
        nil ->
            erlang:error(
                {assertion_failed, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {reason,
                        "Could not get property '" ++
                            couch_util:to_list(Prop) ++
                            "' for task " ++
                            pid_to_list(Pid)}
                ]}
            );
        Value ->
            Value
    end.

all_tasks_sync() ->
    % When we remove the `all` call, switch to test_util:wait/1
    gen_server:call(couch_task_status, all).
