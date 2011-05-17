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

default_config() ->
    test_util:build_file("etc/couchdb/default_dev.ini").

local_config_1() ->
    test_util:source_file("test/etap/081-config-override.1.ini").

local_config_2() ->
    test_util:source_file("test/etap/081-config-override.2.ini").

local_config_write() ->
    test_util:build_file("test/etap/temp.081").

% Run tests and wait for the config gen_server to shutdown.
run_tests(IniFiles, Tests) ->
    {ok, Pid} = couch_config:start_link(IniFiles),
    erlang:monitor(process, Pid),
    Tests(),
    couch_config:stop(),
    receive
        {'DOWN', _, _, Pid, _} -> ok;
        _Other -> etap:diag("OTHER: ~p~n", [_Other])
    after
        1000 -> throw({timeout_error, config_stop})
    end.

main(_) ->
    test_util:init_code_path(),
    etap:plan(17),

    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->

    CheckStartStop = fun() -> ok end,
    run_tests([default_config()], CheckStartStop),

    CheckDefaults = fun() ->
        etap:is(
            couch_config:get("couchdb", "max_dbs_open"),
            "100",
            "{couchdb, max_dbs_open} is 100 by defualt."
        ),

        etap:is(
            couch_config:get("httpd","port"),
            "5984",
            "{httpd, port} is 5984 by default"
        ),

        etap:is(
            couch_config:get("fizbang", "unicode"),
            undefined,
            "{fizbang, unicode} is undefined by default"
        )
    end,

    run_tests([default_config()], CheckDefaults),


    % Check that subsequent files override values appropriately

    CheckOverride = fun() ->
        etap:is(
            couch_config:get("couchdb", "max_dbs_open"),
            "10",
            "{couchdb, max_dbs_open} was overriden with the value 10"
        ),

        etap:is(
            couch_config:get("httpd", "port"),
            "4895",
            "{httpd, port} was overriden with the value 4895"
        )
    end,

    run_tests([default_config(), local_config_1()], CheckOverride),


    % Check that overrides can create new sections

    CheckOverride2 = fun() ->
        etap:is(
            couch_config:get("httpd", "port"),
            "80",
            "{httpd, port} is overriden with the value 80"
        ),

        etap:is(
            couch_config:get("fizbang", "unicode"),
            "normalized",
            "{fizbang, unicode} was created by override INI file"
        )
    end,

    run_tests([default_config(), local_config_2()], CheckOverride2),


    % Check that values can be overriden multiple times

    CheckOverride3 = fun() ->
        etap:is(
            couch_config:get("httpd", "port"),
            "80",
            "{httpd, port} value was taken from the last specified INI file."
        )
    end,

    run_tests(
        [default_config(), local_config_1(), local_config_2()],
        CheckOverride3
    ),

    % Check persistence to last file.

    % Empty the file in case it exists.
    {ok, Fd} = file:open(local_config_write(), write),
    ok = file:truncate(Fd),
    ok = file:close(Fd),

    % Open and write a value
    CheckCanWrite = fun() ->
        etap:is(
            couch_config:get("httpd", "port"),
            "5984",
            "{httpd, port} is still 5984 by default"
        ),

        etap:is(
            couch_config:set("httpd", "port", "8080"),
            ok,
            "Writing {httpd, port} is kosher."
        ),

        etap:is(
            couch_config:get("httpd", "port"),
            "8080",
            "{httpd, port} was updated to 8080 successfully."
        ),

        etap:is(
            couch_config:delete("httpd", "bind_address"),
            ok,
            "Deleting {httpd, bind_address} succeeds"
        ),

        etap:is(
            couch_config:get("httpd", "bind_address"),
            undefined,
            "{httpd, bind_address} was actually deleted."
        )
    end,

    run_tests([default_config(), local_config_write()], CheckCanWrite),

    % Open and check where we don't expect persistence.

    CheckDidntWrite = fun() ->
        etap:is(
            couch_config:get("httpd", "port"),
            "5984",
            "{httpd, port} was not persisted to the primary INI file."
        ),

        etap:is(
            couch_config:get("httpd", "bind_address"),
            "127.0.0.1",
            "{httpd, bind_address} was not deleted form the primary INI file."
        )
    end,

    run_tests([default_config()], CheckDidntWrite),

    % Open and check we have only the persistence we expect.
    CheckDidWrite = fun() ->
        etap:is(
            couch_config:get("httpd", "port"),
            "8080",
            "{httpd, port} is still 8080 after reopening the config."
        ),

        etap:is(
            couch_config:get("httpd", "bind_address"),
            undefined,
            "{httpd, bind_address} is still \"\" after reopening."
        )
    end,

    run_tests([local_config_write()], CheckDidWrite),

    ok.
