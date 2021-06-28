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

-module(chttpd_session_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include("chttpd_test.hrl").

-define(USER, "chttpd_test_admin").
-define(PASS, "pass").

setup() ->
    ok = config:delete("chttpd_auth", "authentication_db", _Persist = false),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, binary_to_list(Hashed), _Persist = false),
    root_url() ++ "/_session".

cleanup(_) ->
    ok = config:delete("chttpd_auth", "authentication_db", _Persist = false),
    ok = config:delete("admins", ?USER, _Persist = false).

session_test_() ->
    {
        "Session tests",
        {
            setup,
            fun() -> test_util:start_couch([fabric, chttpd]) end,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(session_authentication_db_absent),
                    ?TDEF_FE(session_authentication_db_present),
                    ?TDEF_FE(session_authentication_gzip_request)
                ]
            }
        }
    }.

session_authentication_db_absent(Url) ->
    ok = config:delete("chttpd_auth", "authentication_db", _Persist = false),
    ?assertThrow({not_found, _}, session_authentication_db(Url)).

session_authentication_db_present(Url) ->
    Name = "_users",
    ok = config:set("chttpd_auth", "authentication_db", Name, false),
    ?assertEqual(list_to_binary(Name), session_authentication_db(Url)).

session_authentication_gzip_request(Url) ->
    {ok, 200, _, Body} = test_request:request(
        post,
        Url,
        [{"Content-Type", "application/json"}, {"Content-Encoding", "gzip"}],
        zlib:gzip(
            jiffy:encode({[{username, list_to_binary(?USER)}, {password, list_to_binary(?PASS)}]})
        )
    ),
    {BodyJson} = jiffy:decode(Body),
    ?assert(lists:member({<<"name">>, list_to_binary(?USER)}, BodyJson)).

session_authentication_db(Url) ->
    {ok, 200, _, Body} = test_request:get(Url, [{basic_auth, {?USER, ?PASS}}]),
    couch_util:get_nested_json_value(
        jiffy:decode(Body), [<<"info">>, <<"authentication_db">>]
    ).

root_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port]).
