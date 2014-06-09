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

server() ->
    lists:concat([
        "http://127.0.0.1:",
        mochiweb_socket_server:get(couch_httpd, port),
        "/_utils/"
    ]).


main(_) ->
    test_util:init_code_path(),

    etap:plan(3),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    %% launch couchdb
    couch_server_sup:start_link(test_util:config_files()),

    % CSP is disabled by default
    test_no_csp_headers_server(),

    % Now enable CSP
    ok = couch_config:set("csp", "enable", "true", false),

    test_default_header_value(),

    ok = couch_config:set("csp", "header_value", "default-src 'http://example.com';", false),
    test_custom_header_value(),

    % Disabled on all other values than true
    ok = couch_config:set("csp", "enable", "blerg", false),
    test_all_other_values_for_enable(),

    timer:sleep(3000),
    couch_server_sup:stop(),
    ok.

test_no_csp_headers_server() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, Resp, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Content-Security-Policy", Resp),
            undefined, "No CSP Headers when disabled").

test_default_header_value() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, Resp, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Content-Security-Policy", Resp),
            "default-src 'self'; img-src 'self'; font-src 'self'; "
            "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';",
            "Default CSP Headers when enabled").

test_custom_header_value() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, Resp, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Content-Security-Policy", Resp),
            "default-src 'http://example.com';",
            "Custom CSP Headers possible").

test_all_other_values_for_enable() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, Resp, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Content-Security-Policy", Resp),
            undefined, "No CSP Headers when wrong value given").
