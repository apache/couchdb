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

daemon_cmd() ->
    test_util:source_file("test/etap/171-os-daemons-config.es").

main(_) ->
    test_util:init_code_path(),

    etap:plan(6),
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
    couch_config:set("log", "level", "debug", false),
    couch_log:start_link(),
    couch_os_daemons:start_link(),

    % "foo" is a required name by this test.
    couch_config:set("os_daemons", "foo", daemon_cmd(), false),
    timer:sleep(1000),
    
    {ok, [D1]} = couch_os_daemons:info([table]),
    check_daemon(D1, "foo"),
    
    ok.

check_daemon(D, Name) ->
    BaseName = "171-os-daemons-config.es",
    BaseLen = length(BaseName),
    CmdLen = length(D#daemon.cmd),
    CmdName = lists:sublist(D#daemon.cmd, CmdLen-BaseLen+1, BaseLen),

    etap:is(is_port(D#daemon.port), true, "Daemon port is a port."),
    etap:is(D#daemon.name, Name, "Daemon name was set correctly."),
    etap:is(CmdName, BaseName, "Command name was set correctly."),
    etap:isnt(D#daemon.kill, undefined, "Kill command was set."),
    etap:is(D#daemon.errors, [], "No errors occurred while booting."),
    etap:is(D#daemon.buf, [], "No extra data left in the buffer.").
