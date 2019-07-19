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

-module(couchdb_access_tests).

-include_lib("couch/include/couch_eunit.hrl").

make_url(User, Addr, PortType) ->
    lists:concat(["http://", User, ":", User, "@", Addr, ":", port(PortType)]).

setup(PortType) ->
    Hashed = couch_passwords:hash_admin_password("a"),
    ok = config:set("admins", "a", binary_to_list(Hashed), _Persist=false),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Url = lists:concat(["http://", Addr, ":", port(PortType)]),

    AdminUrl = make_url("a", Addr, PortType),
    XUrl = make_url("x", Addr, PortType),
    YUrl = make_url("y", Addr, PortType),

    % create users
    UserDbUrl = AdminUrl ++ "/_users",
    {ok, 201, _, _} = test_request:put(UserDbUrl, ""),

    UserXUrl = AdminUrl ++ "/_users/x",
    UserXBody = "{ \"name\":\"x\", \"roles\": [], \"password\":\"x\", \"type\": \"user\" }",
    {ok, 201, _, _} = test_request:put(UserXUrl, UserXBody),

    UserYUrl = AdminUrl ++ "/_users/y",
    UserYBody = "{ \"name\":\"y\", \"roles\": [], \"password\":\"y\", \"type\": \"user\" }",
    {ok, 201, _, _} = test_request:put(UserYUrl, UserYBody),

    {ok, _, _, _} = test_request:delete(AdminUrl ++ "/db"),
    {ok, _, _, _} = test_request:put(AdminUrl ++ "/db?access=true", ""),

    {AdminUrl, XUrl, YUrl}.

teardown(_, _) ->
    ok.


access_test_() ->
    Tests = [
        fun should_let_admin_create_doc_with_access/2
    ],
    {
        "Auth tests",
        {
            setup,
            fun() -> test_util:start_couch([chttpd]) end, fun test_util:stop_couch/1,
            [
                make_test_cases(clustered, Tests)
            ]
        }
    }.

make_test_cases(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun setup/1, fun teardown/2, [{Mod, Fun} || Fun <- Funs]}
    }.

should_let_admin_create_doc_with_access(_PortType, {AdminUrl, XUrl, YUrl}) ->
    {ok, Code, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    ?_assertEqual(201, Code).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

port(clustered) ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port));
port(backdoor) ->
    integer_to_list(mochiweb_socket_server:get(couch_httpd, port)).
