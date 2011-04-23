#!/usr/bin/env escript
%% -*- erlang -*-

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

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

daemon_name() ->
    "wheee".

daemon_cmd() ->
    test_util:build_file("test/etap/test_cfg_register").

main(_) ->
    test_util:init_code_path(),

    etap:plan(27),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_config:start_link(test_util:config_files()),
    couch_os_daemons:start_link(),
    
    DaemonCmd = daemon_cmd() ++ " 2> /dev/null",
    
    etap:diag("Booting the daemon"),
    couch_config:set("os_daemons", daemon_name(), DaemonCmd, false),
    wait_for_start(10),
    {ok, [D1]} = couch_os_daemons:info([table]),
    check_daemon(D1, running),
    
    etap:diag("Daemon restarts when section changes."),
    couch_config:set("s1", "k", "foo", false),
    wait_for_restart(10),
    {ok, [D2]} = couch_os_daemons:info([table]),
    check_daemon(D2, running),
    etap:isnt(D2#daemon.kill, D1#daemon.kill, "Kill command shows restart."),

    etap:diag("Daemon doesn't restart for ignored section key."),
    couch_config:set("s2", "k2", "baz", false),
    timer:sleep(1000), % Message travel time.
    {ok, [D3]} = couch_os_daemons:info([table]),
    etap:is(D3, D2, "Same daemon info after ignored config change."),
    
    etap:diag("Daemon restarts for specific section/key pairs."),
    couch_config:set("s2", "k", "bingo", false),
    wait_for_restart(10),
    {ok, [D4]} = couch_os_daemons:info([table]),
    check_daemon(D4, running),
    etap:isnt(D4#daemon.kill, D3#daemon.kill, "Kill command changed again."),
    
    ok.

wait_for_start(0) ->
    throw({error, wait_for_start});
wait_for_start(N) ->
    case couch_os_daemons:info([table]) of
        {ok, []} ->
            timer:sleep(200),
            wait_for_start(N-1);
        _ ->
            timer:sleep(1000)
    end.

wait_for_restart(0) ->
    throw({error, wait_for_restart});
wait_for_restart(N) ->
    {ok, [D]} = couch_os_daemons:info([table]),
    case D#daemon.status of
        restarting ->
            timer:sleep(200),
            wait_for_restart(N-1);
        _ ->
            timer:sleep(1000)
    end.

check_daemon(D, Status) ->
    BaseName = filename:basename(daemon_cmd()) ++ " 2> /dev/null",
    BaseLen = length(BaseName),
    CmdLen = length(D#daemon.cmd),
    CmdName = lists:sublist(D#daemon.cmd, CmdLen-BaseLen+1, BaseLen),

    etap:is(is_port(D#daemon.port), true, "Daemon port is a port."),
    etap:is(D#daemon.name, daemon_name(), "Daemon name was set correctly."),
    etap:is(CmdName, BaseName, "Command name was set correctly."),
    etap:isnt(D#daemon.kill, undefined, "Kill command was set."),
    etap:is(D#daemon.status, Status, "Daemon status is correct."),
    etap:is(D#daemon.cfg_patterns, [{"s1"}, {"s2", "k"}], "Cfg patterns set"),
    etap:is(D#daemon.errors, [], "No errors have occurred."),
    etap:isnt(D#daemon.buf, nil, "Buffer is active.").
