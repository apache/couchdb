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

-mode(compile).
-export([handle_config_change/5]).

default_config() ->
    test_util:build_file("etc/couchdb/default_dev.ini").

main(_) ->
    test_util:init_code_path(),
    etap:plan(5),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    application:set_env(config, ini_files, ["etc/couchdb/default_dev.ini"]),
    ok = application:start(config),

    etap:is(
        config:get("httpd", "port"),
        "5984",
        "{httpd, port} is 5984 by default."
    ),

    ok = config:set("httpd", "port", "4895", false),

    etap:is(
        config:get("httpd", "port"),
        "4895",
        "{httpd, port} changed to 4895"
    ),

    config:listen_for_changes(?MODULE, self()),

    ok = config:set("httpd", "port", "8080", false),

    receive
        {"httpd", "port", Value, false} ->
            etap:is(Value, "8080", "Registered function got notification.")
    after
        1000 ->
            etap:fail("notification failed")
    end,

    % Implicitly checking that we *don't* call the function
    etap:is(
        config:get("httpd", "bind_address"),
        "127.0.0.1",
        "{httpd, bind_address} is not '0.0.0.0'"
    ),

    Msg = receive M -> M after 500 -> nil end,
    etap:is(Msg, nil, "yay, no notification for get"),

    ok.

handle_config_change(Sec, Key, Val, Persist, Pid) ->
    Pid ! {Sec, Key, Val, Persist},
    {ok, Pid}.

