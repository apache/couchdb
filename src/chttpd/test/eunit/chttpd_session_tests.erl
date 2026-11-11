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

-define(USER, "chttpd_test_admin").
-define(PASS, "pass").

setup() ->
    Persist = false,
    ok = config:delete("chttpd_auth", "authentication_db", Persist),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, binary_to_list(Hashed), Persist),
    root_url() ++ "/_session".

cleanup(_) ->
    Persist = false,
    ok = config:delete("chttpd_auth", "authentication_db", Persist),
    ok = config:delete("admins", ?USER, Persist).

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
                    ?TDEF_FE(session_authentication_gzip_request),
                    ?TDEF_FE(session_cache_control)
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

session_cache_control(Url) ->
    CT = {"Content-Type", "application/json"},
    Auth = {basic_auth, {?USER, ?PASS}},
    BUser = list_to_binary(?USER),
    BPass = list_to_binary(?PASS),
    Good = jiffy:encode({[{name, BUser}, {password, BPass}]}),
    Bad = jiffy:encode({[{name, BUser}, {password, ~"badpass"}]}),
    Cases = [
        {"GET anon", 200, test_request:get(Url)},
        {"GET auth", 200, test_request:get(Url, [Auth])},
        {"POST succ 200", 200, test_request:post(Url, [CT], Good)},
        {"POST succ 302", 302, test_request:post(Url ++ "?next=/_up", [CT], Good)},
        {"POST fail 401", 401, test_request:post(Url, [CT], Bad)},
        {"POST fail 302", 302, test_request:post(Url ++ "?fail=/_up", [CT], Bad)},
        {"DELETE 200", 200, test_request:delete(Url)},
        {"DELETE 302", 302, test_request:delete(Url ++ "?next=/_up")}
    ],
    lists:foreach(
        fun({Desc, Expected, {ok, Code, Headers, _}}) ->
            ?assertEqual({Desc, Expected}, {Desc, Code}),
            ?assertEqual({Desc, "no-store"}, {Desc, cache_control(Headers)})
        end,
        Cases
    ).

% Headers can be a single or a list if duplicated
cache_control(Headers) ->
    case [V || {K, V} <- Headers, K =:= ~"cache-control"] of
        [Value] -> binary_to_list(Value);
        Values -> [binary_to_list(V) || V <- Values]
    end.

session_authentication_db(Url) ->
    {ok, 200, _, Body} = test_request:get(Url, [{basic_auth, {?USER, ?PASS}}]),
    couch_util:get_nested_json_value(
        jiffy:decode(Body), [<<"info">>, <<"authentication_db">>]
    ).

root_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port]).
