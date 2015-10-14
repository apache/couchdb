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

-module(couchdb_os_daemons_tests).

-include_lib("couch/include/couch_eunit.hrl").

%% keep in sync with couchdb/couch_os_daemons.erl
-record(daemon, {
    port,
    name,
    cmd,
    kill,
    status=running,
    cfg_patterns=[],
    errors=[],
    buf=[]
}).

-define(DAEMON_CONFIGER, "os_daemon_configer.escript").
-define(DAEMON_LOOPER, "os_daemon_looper.escript").
-define(DAEMON_BAD_PERM, "os_daemon_bad_perm.sh").
-define(DAEMON_CAN_REBOOT, "os_daemon_can_reboot.sh").
-define(DAEMON_DIE_ON_BOOT, "os_daemon_die_on_boot.sh").
-define(DAEMON_DIE_QUICKLY, "os_daemon_die_quickly.sh").
-define(TRIES, 20).
-define(TRY_DELAY_MS, 100).
-define(TIMEOUT, 1000).


setup(DName) ->
    Ctx = test_util:start(?MODULE, [couch_log], [{dont_mock, [config]}]),
    {ok, OsDPid} = couch_os_daemons:start_link(),
    config:set("os_daemons", DName,
                     filename:join([?FIXTURESDIR, DName]), false),
    % Set configuration option to be used by configuration_reader_test_
    % This will be used in os_daemon_configer.escript:test_get_cfg2
    config:set("uuids", "algorithm","sequential", false),
    ensure_n_daemons_are_alive(1),
    {Ctx, OsDPid}.

teardown(_, {Ctx, OsDPid}) ->
    test_util:stop_sync_throw(OsDPid, fun() ->
        exit(OsDPid, shutdown)
    end, {timeout, os_daemon_stop}, ?TIMEOUT),
    test_util:stop(Ctx).


os_daemons_test_() ->
    {
        "OS Daemons tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{?DAEMON_LOOPER, Fun} || Fun <- [
                fun should_check_daemon/2,
                fun should_check_daemon_table_form/2,
                fun should_clean_tables_on_daemon_remove/2,
                fun should_spawn_multiple_daemons/2,
                fun should_keep_alive_one_daemon_on_killing_other/2
            ]]
        }
    }.

configuration_reader_test_() ->
    {
        "OS Daemon requests CouchDB configuration",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{?DAEMON_CONFIGER,
              fun should_read_write_config_settings_by_daemon/2}]

        }
    }.

error_test_() ->
    {
        "OS Daemon process error tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{?DAEMON_BAD_PERM, fun should_fail_due_to_lack_of_permissions/2},
             {?DAEMON_DIE_ON_BOOT, fun should_die_on_boot/2},
             {?DAEMON_DIE_QUICKLY, fun should_die_quickly/2},
             {?DAEMON_CAN_REBOOT, fun should_not_being_halted/2}]
        }
    }.


should_check_daemon(DName, _) ->
    ?_test(begin
        {ok, [D]} = couch_os_daemons:info([table]),
        check_daemon(D, DName)
    end).

should_check_daemon_table_form(DName, _) ->
    ?_test(begin
        {ok, Tab} = couch_os_daemons:info(),
        [D] = ets:tab2list(Tab),
        check_daemon(D, DName)
    end).

should_clean_tables_on_daemon_remove(DName, _) ->
    ?_test(begin
        config:delete("os_daemons", DName, false),
        {ok, Tab2} = couch_os_daemons:info(),
        ?_assertEqual([], ets:tab2list(Tab2))
    end).

should_spawn_multiple_daemons(DName, _) ->
    ?_test(begin
        config:set("os_daemons", "bar",
                         filename:join([?FIXTURESDIR, DName]), false),
        config:set("os_daemons", "baz",
                         filename:join([?FIXTURESDIR, DName]), false),
        ensure_n_daemons_are_alive(3), % DName, "bar" and "baz"
        {ok, Daemons} = couch_os_daemons:info([table]),
        lists:foreach(fun(D) ->
            check_daemon(D)
        end, Daemons),
        {ok, Tab} = couch_os_daemons:info(),
        lists:foreach(fun(D) ->
            check_daemon(D)
        end, ets:tab2list(Tab))
    end).

should_keep_alive_one_daemon_on_killing_other(DName, _) ->
    ?_test(begin
        config:set("os_daemons", "bar",
                         filename:join([?FIXTURESDIR, DName]), false),
        ensure_n_daemons_are_alive(2), % DName and "bar"
        {ok, Daemons} = couch_os_daemons:info([table]),
        lists:foreach(fun(D) ->
            check_daemon(D)
        end, Daemons),

        config:delete("os_daemons", "bar", false),
        ensure_n_daemons_are_alive(1), % Dname only, "bar" should be dead
        {ok, [D2]} = couch_os_daemons:info([table]),
        check_daemon(D2, DName),

        {ok, Tab} = couch_os_daemons:info(),
        [T] = ets:tab2list(Tab),
        check_daemon(T, DName)
    end).

should_read_write_config_settings_by_daemon(DName, _) ->
    ?_test(begin
        % have to wait till daemon run all his tests
        % see daemon's script for more info
        timer:sleep(?TIMEOUT),
        {ok, [D]} = couch_os_daemons:info([table]),
        check_daemon(D, DName)
    end).

should_fail_due_to_lack_of_permissions(DName, _) ->
    ?_test(should_halts(DName, 1000)).

should_die_on_boot(DName, _) ->
    ?_test(should_halts(DName, 1000)).

should_die_quickly(DName, _) ->
    ?_test(should_halts(DName, 4000)).

should_not_being_halted(DName, _) ->
    ?_test(begin
        timer:sleep(1000),
        {ok, [D1]} = couch_os_daemons:info([table]),
        check_daemon(D1, DName, 0),

        % Should reboot every two seconds. We're at 1s, so wait
        % until 3s to be in the middle of the next invocation's
        % life span.

        timer:sleep(2000),
        {ok, [D2]} = couch_os_daemons:info([table]),
        check_daemon(D2, DName, 1),

        % If the kill command changed, that means we rebooted the process.
        ?assertNotEqual(D1#daemon.kill, D2#daemon.kill)
    end).

should_halts(DName, Time) ->
    timer:sleep(Time),
    {ok, [D]} = couch_os_daemons:info([table]),
    check_dead(D, DName),
    config:delete("os_daemons", DName, false).

check_daemon(D) ->
    check_daemon(D, D#daemon.name).

check_daemon(D, Name) ->
    check_daemon(D, Name, 0).

check_daemon(D, Name, Errs) ->
    ?assert(is_port(D#daemon.port)),
    ?assertEqual(Name, D#daemon.name),
    ?assertNotEqual(undefined, D#daemon.kill),
    ?assertEqual(running, D#daemon.status),
    ?assertEqual(Errs, length(D#daemon.errors)),
    ?assertEqual([], D#daemon.buf).

check_dead(D, Name) ->
    ?assert(is_port(D#daemon.port)),
    ?assertEqual(Name, D#daemon.name),
    ?assertNotEqual(undefined, D#daemon.kill),
    ?assertEqual(halted, D#daemon.status),
    ?assertEqual(nil, D#daemon.errors),
    ?assertEqual(nil, D#daemon.buf).

daemons() ->
    {ok, Daemons} = couch_os_daemons:info([table]),
    Daemons.

ensure_n_daemons_are_alive(NumDaemons) ->
    retry(fun() -> length(daemons()) == NumDaemons end, "spawning"),
    retry(fun() ->
                  lists:all(fun(D) -> D#daemon.kill =/= undefined end, daemons())
          end, "waiting for kill flag").

retry(Pred, FailReason) ->
    retry(Pred, ?TRIES, FailReason).

retry(_Pred, 0, FailReason) ->
    erlang:error({assertion_failed,[{module, ?MODULE}, {line, ?LINE},
                                    {reason, "Timed out: " ++ FailReason}]});
retry(Pred, N, FailReason) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(?TRY_DELAY_MS),
            retry(Pred, N - 1, FailReason)
    end.
