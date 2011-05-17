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
    test_util:source_file("test/etap/170-os-daemons.es").

main(_) ->
    test_util:init_code_path(),

    etap:plan(49),
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

    etap:diag("Daemons boot after configuration added."),
    couch_config:set("os_daemons", "foo", daemon_cmd(), false),
    timer:sleep(1000),
    
    {ok, [D1]} = couch_os_daemons:info([table]),
    check_daemon(D1, "foo"),

    % Check table form
    {ok, Tab1} = couch_os_daemons:info(),
    [T1] = ets:tab2list(Tab1),
    check_daemon(T1, "foo"),

    etap:diag("Daemons stop after configuration removed."),
    couch_config:delete("os_daemons", "foo", false),
    timer:sleep(500),
    
    {ok, []} = couch_os_daemons:info([table]),
    {ok, Tab2} = couch_os_daemons:info(),
    etap:is(ets:tab2list(Tab2), [], "As table returns empty table."),
    
    etap:diag("Adding multiple daemons causes both to boot."),
    couch_config:set("os_daemons", "bar", daemon_cmd(), false),
    couch_config:set("os_daemons", "baz", daemon_cmd(), false),
    timer:sleep(500),
    {ok, Daemons} = couch_os_daemons:info([table]),
    lists:foreach(fun(D) ->
        check_daemon(D)
    end, Daemons),

    {ok, Tab3} = couch_os_daemons:info(),
    lists:foreach(fun(D) ->
        check_daemon(D)
    end, ets:tab2list(Tab3)),
    
    etap:diag("Removing one daemon leaves the other alive."),
    couch_config:delete("os_daemons", "bar", false),
    timer:sleep(500),
    
    {ok, [D2]} = couch_os_daemons:info([table]),
    check_daemon(D2, "baz"),
    
    % Check table version
    {ok, Tab4} = couch_os_daemons:info(),
    [T4] = ets:tab2list(Tab4),
    check_daemon(T4, "baz"),
    
    ok.

check_daemon(D) ->
    check_daemon(D, D#daemon.name).

check_daemon(D, Name) ->
    BaseName = "170-os-daemons.es",
    BaseLen = length(BaseName),
    CmdLen = length(D#daemon.cmd),
    CmdName = lists:sublist(D#daemon.cmd, CmdLen-BaseLen+1, BaseLen),

    etap:is(is_port(D#daemon.port), true, "Daemon port is a port."),
    etap:is(D#daemon.name, Name, "Daemon name was set correctly."),
    etap:is(CmdName, BaseName, "Command name was set correctly."),
    etap:isnt(D#daemon.kill, undefined, "Kill command was set."),
    etap:is(D#daemon.errors, [], "No errors occurred while booting."),
    etap:is(D#daemon.buf, [], "No extra data left in the buffer.").
