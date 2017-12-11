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

-module(couchdb_cookie_domain_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "cookie_domain_test_admin").
-define(PASS, "pass").

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:flatten(io_lib:format("http://~s:~b/_session", [Addr, Port])).

teardown(_) ->
    ok = config:delete("admins", ?USER, _Persist=false).

cookie_test_() ->
    {
        "Cookie domain tests",
        {
            setup,
            fun() -> test_util:start_couch([chttpd]) end,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_set_cookie_domain/1,
                    fun should_not_set_cookie_domain/1
                ]

            }
        }
    }.

should_set_cookie_domain(Url) ->
    ?_test(begin
        ok = config:set("couch_httpd_auth", "cookie_domain",
            "example.com", false),
        {ok, Code, Headers, _} = test_request:post(Url,
            [{"Content-Type", "application/json"}],
            "{\"name\":\"" ++ ?USER ++ "\", \"password\": \"" ++ ?PASS ++ "\"}"
        ),
        ?assertEqual(200, Code),
        Cookie = proplists:get_value("Set-Cookie", Headers),
        ?assert(string:str(Cookie, "; Domain=example.com") > 0)
    end).

should_not_set_cookie_domain(Url) ->
    ?_test(begin
        ok = config:set("couch_httpd_auth", "cookie_domain", "", false),
        {ok, Code, Headers, _} = test_request:post(Url,
            [{"Content-Type", "application/json"}],
            "{\"name\":\"" ++ ?USER ++ "\", \"password\": \"" ++ ?PASS ++ "\"}"
        ),
        ?assertEqual(200, Code),
        Cookie = proplists:get_value("Set-Cookie", Headers),
        ?assertEqual(0, string:str(Cookie, "; Domain="))
    end).
