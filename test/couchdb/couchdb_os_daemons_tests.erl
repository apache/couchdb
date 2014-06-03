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
-define(DELAY, 100).
-define(FIXTURES_BUILDDIR,
        filename:join([?BUILDDIR, "test", "couchdb", "fixtures"])).
-define(TIMEOUT, 1000).


setup(DName) ->
    {ok, CfgPid} = couch_config:start_link(?CONFIG_CHAIN),
    {ok, OsDPid} = couch_os_daemons:start_link(),
    Path = case DName of
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


check_daemon(D) ->
    check_daemon(D, D#daemon.name).

check_daemon(D, Name) ->
    ?assert(is_port(D#daemon.port)),
    ?assertEqual(Name, D#daemon.name),
    ?assertNotEqual(undefined, D#daemon.kill),
    ?assertEqual([], D#daemon.errors),
    ?assertEqual([], D#daemon.buf).
