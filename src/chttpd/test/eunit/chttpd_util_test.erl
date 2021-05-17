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

-module(chttpd_util_test).


-include_lib("couch/include/couch_eunit.hrl").
-include("chttpd_test.hrl").


setup() ->
    ok = config:set("httpd", "both_exist", "get_in_httpd", _Persist = false),
    ok = config:set("chttpd", "both_exist", "get_in_chttpd", _Persist = false),
    ok = config:set("httpd", "httpd_only", "true", _Persist = false),
    ok = config:set("chttpd", "chttpd_only", "1", _Persist = false).


teardown(_) ->
    ok = config:delete("httpd", "both_exist", _Persist = false),
    ok = config:delete("chttpd", "both_exist", _Persist = false),
    ok = config:delete("httpd", "httpd_only", _Persist = false),
    ok = config:delete("chttpd", "chttpd_only", _Persist = false).


chttpd_util_config_test_() ->
    {
        "chttpd util config tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(test_behavior),
                    ?TDEF_FE(test_with_undefined_option),
                    ?TDEF_FE(test_with_httpd_option),
                    ?TDEF_FE(test_with_chttpd_option),
                    ?TDEF_FE(test_with_chttpd_option_which_moved_from_httpd),
                    ?TDEF_FE(test_get_chttpd_config_integer),
                    ?TDEF_FE(test_get_chttpd_config_boolean)
                ]
            }
        }
    }.


test_behavior(_) ->
    ?assertEqual("get_in_chttpd", chttpd_util:get_chttpd_config("both_exist")),
    ?assertEqual(1, chttpd_util:get_chttpd_config_integer("chttpd_only", 0)),
    ?assert(chttpd_util:get_chttpd_config_boolean("httpd_only", false)).


test_with_undefined_option(_) ->
    ?assertEqual(undefined, chttpd_util:get_chttpd_config("undefined_option")),
    ?assertEqual(abc, chttpd_util:get_chttpd_config("undefined_option", abc)),
    ?assertEqual(123, chttpd_util:get_chttpd_config("undefined_option", 123)),
    ?assertEqual(0.2, chttpd_util:get_chttpd_config("undefined_option", 0.2)),
    ?assertEqual("a", chttpd_util:get_chttpd_config("undefined_option", "a")),
    ?assertEqual("", chttpd_util:get_chttpd_config("undefined_option", "")),
    ?assert(chttpd_util:get_chttpd_config("undefined_option", true)),
    ?assertNot(chttpd_util:get_chttpd_config("undefined_option", false)).


test_with_httpd_option(_) ->
    ?assertEqual("{couch_httpd_auth, cookie_authentication_handler}, " ++
            "{couch_httpd_auth, default_authentication_handler}",
                    chttpd_util:get_chttpd_config("authentication_handlers")).


test_with_chttpd_option(_) ->
    ?assertEqual("512", chttpd_util:get_chttpd_config("backlog")),
    ?assertEqual("512", chttpd_util:get_chttpd_config("backlog", 123)),
    ?assertEqual(512, chttpd_util:get_chttpd_config_integer("backlog", 123)),
    ?assertEqual("false",
        chttpd_util:get_chttpd_config("require_valid_user")),
    ?assertEqual("false",
        chttpd_util:get_chttpd_config("require_valid_user", "true")),
    ?assertEqual(false,
        chttpd_util:get_chttpd_config_boolean("require_valid_user", true)).


test_with_chttpd_option_which_moved_from_httpd(_) ->
    ?assertEqual(undefined, chttpd_util:get_chttpd_config("max_uri_length")),
    ?assertEqual(8000, chttpd_util:get_chttpd_config("max_uri_length", 8000)),
    ?assertEqual(undefined, chttpd_util:get_chttpd_config("WWW-Authenticate")),
    ?assert(chttpd_util:get_chttpd_config("enable_cors", true)).


test_get_chttpd_config_integer(_) ->
    ?assertEqual(123,
        chttpd_util:get_chttpd_config_integer("max_http_request_size", 123)).


test_get_chttpd_config_boolean(_) ->
    ?assert(chttpd_util:get_chttpd_config_boolean("allow_jsonp", true)).
