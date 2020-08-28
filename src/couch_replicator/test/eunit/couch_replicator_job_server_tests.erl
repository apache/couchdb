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

-module(couch_replicator_job_server_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


-define(SHUTDOWN_TIMEOUT, 1000).
-define(JOB_SERVER, couch_replicator_job_server).


job_server_test_() ->
    {
        "Test job server",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_start_up),
                    ?TDEF_FE(reschedule_resets_timer),
                    ?TDEF_FE(reschedule_reads_config),
                    ?TDEF_FE(acceptors_spawned_if_pending),
                    ?TDEF_FE(acceptors_not_spawned_if_no_pending),
                    ?TDEF_FE(acceptors_not_spawned_if_no_max_churn),
                    ?TDEF_FE(acceptors_not_spawned_if_no_churn_budget),
                    ?TDEF_FE(acceptors_spawned_on_acceptor_exit),
                    ?TDEF_FE(acceptor_turns_into_worker),
                    ?TDEF_FE(acceptors_spawned_on_worker_exit),
                    ?TDEF_FE(excess_acceptors_spawned),
                    ?TDEF_FE(excess_workers_trimmed_on_reschedule),
                    ?TDEF_FE(recent_workers_are_not_stopped)
                ]
            }
        }
    }.


setup_all() ->
    Ctx = test_util:start_couch(),
    meck:new(couch_replicator_job_server, [passthrough]),
    mock_pending(0),
    meck:expect(couch_replicator_jobs, set_timeout, 0, ok),
    meck:expect(couch_replicator_jobs, fold_jobs, 3, ok),
    meck:expect(couch_replicator_job, start_link, fun() ->
        {ok, spawn_link(fun() -> start_job() end)}
    end),
    Ctx.


teardown_all(Ctx) ->
    meck:unload(),
    config_delete("interval_sec"),
    config_delete("max_acceptors"),
    config_delete("max_jobs"),
    config_delete("max_churn"),
    config_delete("min_run_time_sec"),
    config_delete("transient_job_max_age_sec"),
    test_util:stop_couch(Ctx).


setup() ->
    config_set("interval_sec", "99999"),
    config_set("max_acceptors", "0"),
    config_set("max_jobs", "0"),
    config_set("max_churn", "1"),
    config_set("min_run_time_sec", "0"),
    config_set("transient_job_max_age_sec", "99999"),

    mock_pending(0),

    {ok, SPid} = ?JOB_SERVER:start_link(?SHUTDOWN_TIMEOUT),
    SPid.


teardown(SPid) when is_pid(SPid) ->
    unlink(SPid),
    Ref = monitor(process, SPid),
    exit(SPid, kill),
    receive {'DOWN', Ref, _, _, _} -> ok end,

    meck:reset(couch_replicator_jobs),
    meck:reset(couch_replicator_job),
    meck:reset(couch_replicator_job_server),

    config_delete("interval_sec"),
    config_delete("max_acceptors"),
    config_delete("max_jobs"),
    config_delete("max_churn"),
    config_delete("min_run_time_sec"),
    config_delete("transient_job_max_age_sec").


should_start_up(SPid) ->
    ?assert(is_process_alive(SPid)),
    ?assertEqual(SPid, whereis(?JOB_SERVER)),
    State = sys:get_state(?JOB_SERVER),
    #{
        acceptors := #{},
        workers := #{},
        churn := 0,
        config := Config,
        timer := Timer,
        timeout := ?SHUTDOWN_TIMEOUT
    } = State,

    % Make sure it read the config
    ?assertMatch(#{
        max_acceptors := 0,
        interval_sec := 99999,
        max_jobs := 0,
        max_churn := 1,
        min_run_time_sec := 0,
        transient_job_max_age_sec := 99999
    }, Config),

    % Timer was set up
    ?assert(is_reference(Timer)),
    ?assert(is_integer(erlang:read_timer(Timer))).


reschedule_resets_timer(_) ->
    #{timer := OldTimer} = sys:get_state(?JOB_SERVER),

    ?assertEqual(ok, ?JOB_SERVER:reschedule()),

    #{timer := Timer} = sys:get_state(?JOB_SERVER),
    ?assert(is_reference(Timer)),
    ?assert(Timer =/= OldTimer).


reschedule_reads_config(_) ->
    config_set("interval_sec", "99998"),

    ?JOB_SERVER:reschedule(),

    #{config := Config} = sys:get_state(?JOB_SERVER),
    ?assertMatch(#{interval_sec := 99998}, Config).


acceptors_spawned_if_pending(_) ->
    config_set("max_acceptors", "1"),
    mock_pending(1),

    ?JOB_SERVER:reschedule(),

    ?assertMatch([Pid] when is_pid(Pid), acceptors()).


acceptors_not_spawned_if_no_pending(_) ->
    config_set("max_acceptors", "1"),
    mock_pending(0),

    ?JOB_SERVER:reschedule(),

    ?assertEqual([], acceptors()).


acceptors_not_spawned_if_no_max_churn(_) ->
    config_set("max_churn", "0"),
    config_set("max_acceptors", "1"),
    mock_pending(1),

    ?JOB_SERVER:reschedule(),

    ?assertEqual([], acceptors()).


acceptors_not_spawned_if_no_churn_budget(_) ->
    config_set("max_churn", "1"),
    config_set("max_acceptors", "1"),
    mock_pending(0),

    % To read the config
    ?JOB_SERVER:reschedule(),

    ?assertEqual([], acceptors()),

    mock_pending(1),

    % Exhaust churn budget
    sys:replace_state(couch_replicator_job_server, fun(#{} = St) ->
        St#{churn := 1}
    end),

    ?JOB_SERVER:reschedule(),

    ?assertEqual([], acceptors()).


acceptors_spawned_on_acceptor_exit(_) ->
    config_set("max_acceptors", "3"),
    config_set("max_jobs", "4"),
    mock_pending(1),

    ?JOB_SERVER:reschedule(),

    [A1] = acceptors(),

    exit(A1, kill),
    meck:wait(?JOB_SERVER, handle_info, [{'EXIT', A1, killed}, '_'], 2000),

    ?assertEqual(3, length(acceptors())).


acceptor_turns_into_worker(_) ->
    config_set("max_acceptors", "3"),
    config_set("max_jobs", "4"),
    mock_pending(1),

    ?JOB_SERVER:reschedule(),

    [A1] = acceptors(),
    accept_job(A1, true),
    ?assertEqual(3, length(acceptors())),
    #{workers := Workers} = sys:get_state(?JOB_SERVER),
    ?assertMatch([{A1, {true, _}}], maps:to_list(Workers)).


acceptors_spawned_on_worker_exit(_) ->
    config_set("max_acceptors", "1"),
    config_set("max_jobs", "1"),
    mock_pending(1),

    ?JOB_SERVER:reschedule(),

    [A1] = acceptors(),
    accept_job(A1, true),

    % Since max_jobs = 1 no more acceptors are spawned
    ?assertEqual(0, length(acceptors())),

    % Same acceptor process is now a worker
    ?assertEqual([A1], workers()),

    exit(A1, shutdown),
    meck:wait(?JOB_SERVER, handle_info, [{'EXIT', A1, shutdown}, '_'], 2000),

    % New acceptor process started
    ?assertEqual(1, length(acceptors())),
    ?assertEqual(0, length(workers())).


excess_acceptors_spawned(_) ->
    config_set("max_acceptors", "2"),
    config_set("max_churn", "3"),
    config_set("max_jobs", "4"),
    mock_pending(100),

    ?JOB_SERVER:reschedule(),

    ?assertEqual(3, length(acceptors())),

    accept_all(),

    ?assertEqual(3, length(workers())),
    ?assertEqual(1, length(acceptors())),
    % Check that the churn budget was consumed
    ?assertMatch(#{churn := 3}, sys:get_state(?JOB_SERVER)),

    accept_all(),

    % No more acceptors spawned after reaching max_jobs
    ?assertEqual(0, length(acceptors())),
    ?assertEqual(4, length(workers())),

    ?JOB_SERVER:reschedule(),

    % Since all churn budget was consumed, no new acceptors should have beens
    % spawned this cycle but churn budget should have been reset
    ?assertEqual(0, length(acceptors())),
    ?assertEqual(4, length(workers())),
    ?assertMatch(#{churn := 0}, sys:get_state(?JOB_SERVER)),

    ?JOB_SERVER:reschedule(),

    % Should have spawned 3 excess acceptors
    ?assertEqual(3, length(acceptors())),
    ?assertEqual(4, length(workers())),

    accept_all(),

    % Running with an excess number of workers
    ?assertEqual(0, length(acceptors())),
    ?assertEqual(7, length(workers())).


excess_workers_trimmed_on_reschedule(_) ->
    config_set("max_acceptors", "2"),
    config_set("max_churn", "3"),
    config_set("max_jobs", "4"),
    mock_pending(100),

    ?JOB_SERVER:reschedule(),

    [A1, A2, A3] = acceptors(),
    accept_job(A1, true),
    accept_job(A2, false),
    accept_job(A3, false),
    [A4] = acceptors(),
    accept_job(A4, true),

    ?JOB_SERVER:reschedule(),

    % First reschedule was to reset the churn budget, this next one is to spawn
    % an excess number of acceptors.
    ?JOB_SERVER:reschedule(),

    [A5, A6, A7] = acceptors(),
    accept_job(A5, true),
    accept_job(A6, false),
    accept_job(A7, false),

    ?assertEqual(7, length(workers())),

    % Running with an excess number of workers. These should be trimmed on the
    % during the next cycle
    ?JOB_SERVER:reschedule(),

    Workers = workers(),
    ?assertEqual(4, length(Workers)),
    ?assertEqual(0, length(acceptors())),

    % Check that A1 and A4 were skipped since they are not continuous
    ?assertEqual(Workers, Workers -- [A2, A3, A6]).


recent_workers_are_not_stopped(_) ->
    config_set("max_acceptors", "2"),
    config_set("max_churn", "3"),
    config_set("max_jobs", "4"),
    mock_pending(100),

    ?JOB_SERVER:reschedule(),

    [A1, A2, A3] = acceptors(),
    accept_job(A1, true),
    accept_job(A2, false),
    accept_job(A3, false),
    [A4] = acceptors(),
    accept_job(A4, true),

    ?JOB_SERVER:reschedule(),

    % First reschedule was to reset the churn budget, this next one is to spawn
    % an excess number of acceptors.
    ?JOB_SERVER:reschedule(),

    [A5, A6, A7] = acceptors(),
    accept_job(A5, true),
    accept_job(A6, false),
    accept_job(A7, false),

    ?assertEqual(7, length(workers())),

    % Running with an excess number of workers. But they won't be stopped on
    % reschedule if they ran for a period less than min_run_time_sec during the
    % next cycle
    config_set("min_run_time_sec", "9999"),

    % don't want to start new acceptors anymore
    mock_pending(0),
    config_set("max_acceptors", "0"),

    ?JOB_SERVER:reschedule(),

    ?assertEqual(7, length(workers())),
    ?assertEqual(0, length(acceptors())),

    config_set("min_run_time_sec", "0"),
   
    ?JOB_SERVER:reschedule(),

    ?assertEqual(4, length(workers())),
    ?assertEqual(0, length(acceptors())).


config_set(K, V) ->
    config:set("replicator", K, V, _Persist = false).


config_delete(K) ->
    config:delete("replicator", K, _Persist = false).


mock_pending(N) ->
    meck:expect(couch_replicator_jobs, pending_count, 2, N).


acceptors() ->
    #{acceptors := Acceptors} = sys:get_state(?JOB_SERVER),
    maps:keys(Acceptors).


workers() ->
    #{workers := Workers} = sys:get_state(?JOB_SERVER),
    maps:keys(Workers).


accept_job(APid, Normal) ->
    APid ! {accept_job, Normal, self()},
    receive
        {job_accepted, APid} -> ok
    after
        5000 ->
            error(test_job_accept_timeout)
    end.


accept_all() ->
    [accept_job(APid, true) || APid <- acceptors()].


start_job() ->
    receive
        {accept_job, Normal, From} ->
            ok = ?JOB_SERVER:accepted(self(), Normal),
            From ! {job_accepted, self()},
            start_job();
        {exit_job, ExitSig} ->
            exit(ExitSig)
    end.
