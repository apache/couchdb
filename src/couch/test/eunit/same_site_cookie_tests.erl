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

-module(same_site_cookie_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "jan").
-define(PASS, "apple").

-define(PERSIST, false).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), ?PERSIST),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

teardown(_BaseUri) ->
    ok = config:delete("couch_httpd_auth", "same_site", ?PERSIST),
    ok = config:delete("admins", ?USER, ?PERSIST).

get_cookie(BaseUri, User, Password) ->
    {ok, _, Headers, _} = test_request:post(
        BaseUri ++ "/_session",
        [{"Content-Type", "application/json"}],
        couch_util:json_encode(#{username => ?l2b(User), password => ?l2b(Password)})
    ),
    proplists:get_value("Set-Cookie", Headers).

same_site_cookie_test_() ->
    {
        "same-site cookie tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(same_site_none),
                    ?TDEF_FE(same_site_missing),
                    ?TDEF_FE(same_site_strict),
                    ?TDEF_FE(same_site_lax),
                    ?TDEF_FE(same_site_invalid)
                ]
            }
        }
    }.

same_site_none(BaseUri) ->
    ok = config:set("couch_httpd_auth", "same_site", "None", ?PERSIST),
    Cookie = get_cookie(BaseUri, ?USER, ?PASS),
    ?assertNotEqual(nomatch, string:find(Cookie, "; SameSite=None")).

same_site_missing(BaseUri) ->
    ok = config:set("couch_httpd_auth", "same_site", "", ?PERSIST),
    Cookie = get_cookie(BaseUri, ?USER, ?PASS),
    ?assertNotEqual(undefined, Cookie),
    ?assertEqual(nomatch, string:find(Cookie, "; SameSite=")).

same_site_strict(BaseUri) ->
    ok = config:set("couch_httpd_auth", "same_site", "Strict", ?PERSIST),
    Cookie = get_cookie(BaseUri, ?USER, ?PASS),
    ?assertNotEqual(nomatch, string:find(Cookie, "; SameSite=Strict")).

same_site_lax(BaseUri) ->
    ok = config:set("couch_httpd_auth", "same_site", "Lax", ?PERSIST),
    Cookie = get_cookie(BaseUri, ?USER, ?PASS),
    ?assertNotEqual(nomatch, string:find(Cookie, "; SameSite=Lax")).

same_site_invalid(BaseUri) ->
    ok = config:set("couch_httpd_auth", "same_site", "Invalid", ?PERSIST),
    Cookie = get_cookie(BaseUri, ?USER, ?PASS),
    ?assertEqual(nomatch, string:find(Cookie, "; SameSite=")).
