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

config_files() ->
    lists:map(fun test_util:build_file/1, [
        "etc/couchdb/default_dev.ini"
    ]).

bad_perms() ->
    test_util:source_file("test/etap/172-os-daemon-errors.1.sh").

die_on_boot() ->
    test_util:source_file("test/etap/172-os-daemon-errors.2.sh").

die_quickly() ->
    test_util:source_file("test/etap/172-os-daemon-errors.3.sh").

can_reboot() ->
    test_util:source_file("test/etap/172-os-daemon-errors.4.sh").

main(_) ->
    test_util:init_code_path(),

    etap:plan(36),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_config:start_link(config_files()),
    couch_os_daemons:start_link(),

    etap:diag("Daemon not executable."),
    test_halts("foo", bad_perms(), 1000),

    etap:diag("Daemon dies on boot."),
    test_halts("bar", die_on_boot(), 1000),

    etap:diag("Daemon dies quickly after boot."),
    test_halts("baz", die_quickly(), 4000),
    
    etap:diag("Daemon dies, but not quickly enough to be halted."),
    test_runs("bam", can_reboot()),
    
    ok.

test_halts(Name, Cmd, Time) ->
    couch_config:set("os_daemons", Name, Cmd ++ " 2> /dev/null", false),
    timer:sleep(Time),
    {ok, [D]} = couch_os_daemons:info([table]),
    check_dead(D, Name, Cmd),
    couch_config:delete("os_daemons", Name, false).

test_runs(Name, Cmd) ->
    couch_config:set("os_daemons", Name, Cmd, false),

    timer:sleep(1000),
    {ok, [D1]} = couch_os_daemons:info([table]),
    check_daemon(D1, Name, Cmd, 0),
    
    % Should reboot every two seconds. We're at 1s, so wait
    % utnil 3s to be in the middle of the next invocation's
    % life span.
    timer:sleep(2000),
    {ok, [D2]} = couch_os_daemons:info([table]),
    check_daemon(D2, Name, Cmd, 1),
    
    % If the kill command changed, that means we rebooted the process.
    etap:isnt(D1#daemon.kill, D2#daemon.kill, "Kill command changed.").

check_dead(D, Name, Cmd) ->
    BaseName = filename:basename(Cmd) ++ " 2> /dev/null",
    BaseLen = length(BaseName),
    CmdLen = length(D#daemon.cmd),
    CmdName = lists:sublist(D#daemon.cmd, CmdLen-BaseLen+1, BaseLen),

    etap:is(is_port(D#daemon.port), true, "Daemon port is a port."),
    etap:is(D#daemon.name, Name, "Daemon name was set correctly."),
    etap:is(CmdName, BaseName, "Command name was set correctly."),
    etap:isnt(D#daemon.kill, undefined, "Kill command was set."),
    etap:is(D#daemon.status, halted, "Daemon has been halted."),
    etap:is(D#daemon.errors, nil, "Errors have been disabled."),
    etap:is(D#daemon.buf, nil, "Buffer has been switched off.").

check_daemon(D, Name, Cmd, Errs) ->
    BaseName = filename:basename(Cmd),
    BaseLen = length(BaseName),
    CmdLen = length(D#daemon.cmd),
    CmdName = lists:sublist(D#daemon.cmd, CmdLen-BaseLen+1, BaseLen),

    etap:is(is_port(D#daemon.port), true, "Daemon port is a port."),
    etap:is(D#daemon.name, Name, "Daemon name was set correctly."),
    etap:is(CmdName, BaseName, "Command name was set correctly."),
    etap:isnt(D#daemon.kill, undefined, "Kill command was set."),
    etap:is(D#daemon.status, running, "Daemon still running."),
    etap:is(length(D#daemon.errors), Errs, "Found expected number of errors."),
    etap:is(D#daemon.buf, [], "No extra data left in the buffer.").

