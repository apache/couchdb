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

-module(chttpd_csp_tests).

-include_lib("couch/include/couch_eunit.hrl").


setup() ->
    ok = config:set("csp", "enable", "true", false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/_utils/"]).

teardown(_) ->
    ok.



csp_test_() ->
    {
        "Content Security Policy tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_not_return_any_csp_headers_when_disabled/1,
                    fun should_apply_default_policy/1,
                    fun should_return_custom_policy/1,
                    fun should_only_enable_csp_when_true/1
                ]
            }
        }
    }.


should_not_return_any_csp_headers_when_disabled(Url) ->
    ?_assertEqual(undefined,
        begin
            ok = config:set("csp", "enable", "false", false),
            {ok, _, Headers, _} = test_request:get(Url),
            proplists:get_value("Content-Security-Policy", Headers)
        end).

should_apply_default_policy(Url) ->
    ?_assertEqual(
        "child-src 'self' data: blob:; default-src 'self'; img-src 'self' data:; font-src 'self'; "
        "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';",
        begin
            {ok, _, Headers, _} = test_request:get(Url),
            proplists:get_value("Content-Security-Policy", Headers)
        end).

should_return_custom_policy(Url) ->
    ?_assertEqual("default-src 'http://example.com';",
        begin
            ok = config:set("csp", "header_value",
                                  "default-src 'http://example.com';", false),
            {ok, _, Headers, _} = test_request:get(Url),
            proplists:get_value("Content-Security-Policy", Headers)
        end).

should_only_enable_csp_when_true(Url) ->
    ?_assertEqual(undefined,
        begin
            ok = config:set("csp", "enable", "tru", false),
            {ok, _, Headers, _} = test_request:get(Url),
            proplists:get_value("Content-Security-Policy", Headers)
        end).
