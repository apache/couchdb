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
    ok = lists:foreach(
        fun(Section) ->
            ok = config_delete_all_keys(Section)
        end,
        ["httpd", "chttpd", "couch_httpd_auth", "chttpd_auth"]
    ),

    ok = config:set(
        "httpd",
        "authentication_handlers",
        "{couch_httpd_auth, cookie_authentication_handler}, "
        "{couch_httpd_auth, default_authentication_handler}",
        _Persist = false
    ),
    ok = config:set("httpd", "backlog", "512", _Persist = false),
    ok = config:set("chttpd", "require_valid_user", "false", _Persist = false),
    ok = config:set("httpd", "both_exist", "get_in_httpd", _Persist = false),
    ok = config:set("chttpd", "both_exist", "get_in_chttpd", _Persist = false),
    ok = config:set("httpd", "httpd_only", "true", _Persist = false),
    ok = config:set("chttpd", "chttpd_only", "1", _Persist = false),
    ok = config:set("couch_httpd_auth", "both_exist", "cha", _Persist = false),
    ok = config:set("chttpd_auth", "both_exist", "ca", _Persist = false),
    ok = config:set("couch_httpd_auth", "cha_only", "true", _Persist = false),
    ok = config:set("chttpd_auth", "ca_only", "1", _Persist = false).

teardown(_) ->
    ok = config:delete("httpd", "authentication_handlers", _Persist = false),
    ok = config:delete("httpd", "backlog", _Persist = false),
    ok = config:delete("chttpd", "require_valid_user", _Persist = false),
    ok = config:delete("httpd", "both_exist", _Persist = false),
    ok = config:delete("chttpd", "both_exist", _Persist = false),
    ok = config:delete("httpd", "httpd_only", _Persist = false),
    ok = config:delete("chttpd", "chttpd_only", _Persist = false),
    ok = config:delete("couch_httpd_auth", "both_exist", _Persist = false),
    ok = config:delete("chttpd_auth", "both_exist", _Persist = false),
    ok = config:delete("couch_httpd_auth", "cha_only", _Persist = false),
    ok = config:delete("chttpd_auth", "ca_only", _Persist = false).

config_delete_all_keys(Section) ->
    lists:foreach(
        fun({Key, _Val}) ->
            ok = config:delete(Section, Key, _Persist = false)
        end,
        config:get(Section)
    ).

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
                    ?TDEF_FE(test_chttpd_behavior),
                    ?TDEF_FE(test_with_undefined_option),
                    ?TDEF_FE(test_auth_behavior),
                    ?TDEF_FE(test_auth_with_undefined_option)
                ]
            }
        }
    }.

test_chttpd_behavior(_) ->
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

test_auth_behavior(_) ->
    ?assertEqual("ca", chttpd_util:get_chttpd_auth_config("both_exist")),
    ?assertEqual(1, chttpd_util:get_chttpd_auth_config_integer("ca_only", 0)),
    ?assert(chttpd_util:get_chttpd_auth_config_boolean("cha_only", false)).

test_auth_with_undefined_option(_) ->
    ?assertEqual(undefined, chttpd_util:get_chttpd_auth_config("undefine")),
    ?assertEqual(abc, chttpd_util:get_chttpd_auth_config("undefine", abc)),
    ?assertEqual(123, chttpd_util:get_chttpd_auth_config("undefine", 123)),
    ?assertEqual(0.2, chttpd_util:get_chttpd_auth_config("undefine", 0.2)),
    ?assertEqual("a", chttpd_util:get_chttpd_auth_config("undefine", "a")),
    ?assertEqual("", chttpd_util:get_chttpd_auth_config("undefine", "")),
    ?assert(chttpd_util:get_chttpd_auth_config("undefine", true)),
    ?assertNot(chttpd_util:get_chttpd_auth_config("undefine", false)).
