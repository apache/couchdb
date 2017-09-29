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

setup(PortType) ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    lists:concat(["http://", Addr, ":", port(PortType), "/_session"]).

teardown(_,_) ->
    ok = config:delete("admins", ?USER, _Persist=false).

cookie_test_() ->
    Tests = [
        fun should_set_cookie_domain/2,
        fun should_not_set_cookie_domain/2
    ],
    {
        "Cookie domain tests",
        {
            setup,
            fun() -> test_util:start_couch([chttpd]) end, fun test_util:stop_couch/1,
            [
                make_test_case(clustered, Tests)
            ]
        }
    }.

make_test_case(Mod, Funs) ->
{
    lists:flatten(io_lib:format("~s", [Mod])),
    {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
}.

should_set_cookie_domain(_PortType, Url) ->
    ?_assertEqual(true,
        begin
            ok = config:set("couch_httpd_auth", "cookie_domain", "example.com", false),
            {ok, Code, Headers, _} = test_request:post(Url, [{"Content-Type", "application/json"}],
                "{\"name\":\"" ++ ?USER ++ "\", \"password\": \"" ++ ?PASS ++ "\"}"),
            ?_assert(Code =:= 200),
            Cookie = proplists:get_value("Set-Cookie", Headers),
            string:str(Cookie, "; Domain=example.com") > 0
        end).

should_not_set_cookie_domain(_PortType, Url) ->
    ?_assertEqual(0,
        begin
            ok = config:set("couch_httpd_auth", "cookie_domain", "", false),
            {ok, Code, Headers, _} = test_request:post(Url, [{"Content-Type", "application/json"}],
                "{\"name\":\"" ++ ?USER ++ "\", \"password\": \"" ++ ?PASS ++ "\"}"),
            ?_assert(Code =:= 200),
            Cookie = proplists:get_value("Set-Cookie", Headers),
            string:str(Cookie, "; Domain=")
        end).

port(clustered) ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port));
port(backdoor) ->
    integer_to_list(mochiweb_socket_server:get(couch_httpd, port)).
