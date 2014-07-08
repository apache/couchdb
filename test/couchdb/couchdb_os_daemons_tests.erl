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

-include("couch_eunit.hrl").

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
-define(DAEMON_CFGREG, "test_cfg_register").
-define(DELAY, 100).
-define(FIXTURES_BUILDDIR,
        filename:join([?BUILDDIR, "test", "couchdb", "fixtures"])).
-define(TIMEOUT, 1000).


setup(DName) ->
    {ok, CfgPid} = couch_config:start_link(?CONFIG_CHAIN),
    {ok, OsDPid} = couch_os_daemons:start_link(),
    Path = case DName of
        ?DAEMON_CFGREG ->
            filename:join([?FIXTURES_BUILDDIR, DName]);
        ?DAEMON_CONFIGER ->
            filename:join([?FIXTURES_BUILDDIR, DName]);
        _ ->
            filename:join([?FIXTURESDIR, DName])
    end,
    couch_config:set("os_daemons", DName, Path, false),
    timer:sleep(?DELAY),  % sleep a bit to let daemon set kill flag
    {CfgPid, OsDPid}.

teardown(_, {CfgPid, OsDPid}) ->
    erlang:monitor(process, CfgPid),
    couch_config:stop(),
    receive
        {'DOWN', _, _, CfgPid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, config_stop})
    end,

    erlang:monitor(process, OsDPid),
    exit(OsDPid, normal),
    receive
        {'DOWN', _, _, OsDPid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, os_daemon_stop})
    end.


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

configuration_register_test_() ->
    {
        "OS daemon subscribed to config changes",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{?DAEMON_CFGREG, Fun} || Fun <- [
                fun should_start_daemon/2,
                fun should_restart_daemon_on_section_change/2,
                fun should_not_restart_daemon_on_changing_ignored_section_key/2,
                fun should_restart_daemon_on_section_key_change/2
            ]]
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
        couch_config:delete("os_daemons", DName, false),
        {ok, Tab2} = couch_os_daemons:info(),
        ?_assertEqual([], ets:tab2list(Tab2))
    end).

should_spawn_multiple_daemons(DName, _) ->
    ?_test(begin
        couch_config:set("os_daemons", "bar",
                         filename:join([?FIXTURESDIR, DName]), false),
        couch_config:set("os_daemons", "baz",
                         filename:join([?FIXTURESDIR, DName]), false),
        timer:sleep(?DELAY),
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
        couch_config:set("os_daemons", "bar",
                         filename:join([?FIXTURESDIR, DName]), false),
        timer:sleep(?DELAY),
        {ok, Daemons} = couch_os_daemons:info([table]),
        lists:foreach(fun(D) ->
            check_daemon(D)
        end, Daemons),

        couch_config:delete("os_daemons", "bar", false),
        timer:sleep(?DELAY),
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
        check_daemon(D1, DName, running, 0),

        % Should reboot every two seconds. We're at 1s, so wait
        % until 3s to be in the middle of the next invocation's
        % life span.

        timer:sleep(2000),
        {ok, [D2]} = couch_os_daemons:info([table]),
        check_daemon(D2, DName, running, 1),

        % If the kill command changed, that means we rebooted the process.
        ?assertNotEqual(D1#daemon.kill, D2#daemon.kill)
    end).

should_halts(DName, Time) ->
    timer:sleep(Time),
    {ok, [D]} = couch_os_daemons:info([table]),
    check_dead(D, DName),
    couch_config:delete("os_daemons", DName, false).

should_start_daemon(DName, _) ->
    ?_test(begin
        wait_for_start(10),
        {ok, [D]} = couch_os_daemons:info([table]),
        check_daemon(D, DName, running, 0, [{"s1"}, {"s2", "k"}])
    end).

should_restart_daemon_on_section_change(DName, _) ->
    ?_test(begin
        wait_for_start(10),
        {ok, [D1]} = couch_os_daemons:info([table]),
        couch_config:set("s1", "k", "foo", false),
        wait_for_restart(10),
        {ok, [D2]} = couch_os_daemons:info([table]),
        check_daemon(D2, DName, running, 0, [{"s1"}, {"s2", "k"}]),
        ?assertNotEqual(D1, D2)
    end).

should_not_restart_daemon_on_changing_ignored_section_key(_, _) ->
    ?_test(begin
        wait_for_start(10),
        {ok, [D1]} = couch_os_daemons:info([table]),
        couch_config:set("s2", "k2", "baz", false),
        timer:sleep(?DELAY),
        {ok, [D2]} = couch_os_daemons:info([table]),
        ?assertEqual(D1, D2)
    end).

should_restart_daemon_on_section_key_change(DName, _) ->
    ?_test(begin
        wait_for_start(10),
        {ok, [D1]} = couch_os_daemons:info([table]),
        couch_config:set("s2", "k", "bingo", false),
        wait_for_restart(10),
        {ok, [D2]} = couch_os_daemons:info([table]),
        check_daemon(D2, DName, running, 0, [{"s1"}, {"s2", "k"}]),
        ?assertNotEqual(D1, D2)
    end).


wait_for_start(0) ->
    erlang:error({assertion_failed,
                  [{module, ?MODULE},
                   {line, ?LINE},
                   {reason, "Timeout on waiting daemon for start"}]});
wait_for_start(N) ->
    case couch_os_daemons:info([table]) of
        {ok, []} ->
            timer:sleep(?DELAY),
            wait_for_start(N - 1);
        _ ->
            timer:sleep(?TIMEOUT)
    end.

wait_for_restart(0) ->
    erlang:error({assertion_failed,
                  [{module, ?MODULE},
                   {line, ?LINE},
                   {reason, "Timeout on waiting daemon for restart"}]});
wait_for_restart(N) ->
    {ok, [D]} = couch_os_daemons:info([table]),
    case D#daemon.status of
        restarting ->
            timer:sleep(?DELAY),
            wait_for_restart(N - 1);
        _ ->
            timer:sleep(?TIMEOUT)
    end.

check_daemon(D) ->
    check_daemon(D, D#daemon.name).

check_daemon(D, Name) ->
    check_daemon(D, Name, running).

check_daemon(D, Name, Status) ->
    check_daemon(D, Name, Status, 0).

check_daemon(D, Name, Status, Errs) ->
    check_daemon(D, Name, Status, Errs, []).

check_daemon(D, Name, Status, Errs, CfgPatterns) ->
    ?assert(is_port(D#daemon.port)),
    ?assertEqual(Name, D#daemon.name),
    ?assertNotEqual(undefined, D#daemon.kill),
    ?assertEqual(Status, D#daemon.status),
    ?assertEqual(CfgPatterns, D#daemon.cfg_patterns),
    ?assertEqual(Errs, length(D#daemon.errors)),
    ?assertEqual([], D#daemon.buf).

check_dead(D, Name) ->
    ?assert(is_port(D#daemon.port)),
    ?assertEqual(Name, D#daemon.name),
    ?assertNotEqual(undefined, D#daemon.kill),
    ?assertEqual(halted, D#daemon.status),
    ?assertEqual(nil, D#daemon.errors),
    ?assertEqual(nil, D#daemon.buf).
