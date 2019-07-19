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

make_url(User, Addr) ->
    lists:concat(["http://", User, ":", User, "@", Addr, ":", port()]).

setup(_) ->
    Hashed = couch_passwords:hash_admin_password("a"),
    ok = config:set("admins", "a", binary_to_list(Hashed), _Persist=false),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Url = lists:concat(["http://", Addr, ":", port()]),

    AdminUrl = make_url("a", Addr),
    XUrl = make_url("x", Addr),
    YUrl = make_url("y", Addr),
    ?debugFmt("~nA: ~p, X:~p, Y:~p~n", [AdminUrl, XUrl, YUrl]),
    % cleanup and setup
    {ok, _, _, _} = test_request:delete(AdminUrl ++ "/db"),
    {ok, _, _, _} = test_request:put(AdminUrl ++ "/db?q=1&n=1&access=true", ""),

    % create users
    UserDbUrl = AdminUrl ++ "/_users",
    {ok, 201, _, _} = test_request:put(UserDbUrl, ""),

    UserXUrl = AdminUrl ++ "/_users/x",
    UserXBody = "{ \"name\":\"x\", \"roles\": [], \"password\":\"x\", \"type\": \"user\" }",
    {ok, 201, _, _} = test_request:put(UserXUrl, UserXBody),

    UserYUrl = AdminUrl ++ "/_users/y",
    UserYBody = "{ \"name\":\"y\", \"roles\": [], \"password\":\"y\", \"type\": \"user\" }",
    {ok, 201, _, _} = test_request:put(UserYUrl, UserYBody),

    ?debugFmt("~n~p", [test_request:get(UserXUrl)]),
    ?debugFmt("~n~p", [test_request:get(UserYUrl)]),

    timer:sleep(10000),
    {AdminUrl, XUrl, YUrl}.

teardown(_, {AdminUrl, _, _}) ->
    {ok, _, _, _} = test_request:delete(AdminUrl ++ "/db"),
    ok.


access_test_() ->
    Tests = [
        % fun should_let_admin_create_doc_with_access/2,
        fun should_let_user_create_doc_for_themselves/2
        % fun should_not_let_user_create_doc_for_someone_else/2,
       %  fun should_let_admin_read_doc_with_access/2,
       %  fun user_with_access_can_read_doc/2,
       %  fun user_without_access_can_not_read_doc/2,
       %  fun should_let_admin_delete_doc_with_access/2,
       %  fun should_let_user_delete_doc_for_themselves/2,
       %  fun should_not_let_user_delete_doc_for_someone_else/2,
       %  fun should_let_admin_fetch_all_docs/2,
       %  fun should_let_user_fetch_their_own_all_docs/2,
       %  fun should_let_admin_fetch_changes/2,
       %  fun should_let_user_fetch_their_own_changes/2
    ],
    {
        "Access tests",
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

% Doc creation
should_let_admin_create_doc_with_access(_PortType, {AdminUrl, _XUrl, Y_Url}) ->
    {ok, Code, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    ?_assertEqual(201, Code).

should_let_user_create_doc_for_themselves(_PortType, {_AdminUrl, XUrl, _YUrl}) ->
    {ok, Code, _, _} = test_request:put(XUrl ++ "/db/b", "{\"a\":1,\"_access\":[\"x\"]}"),
    ?_assertEqual(201, Code).

should_not_let_user_create_doc_for_someone_else(_PortType, {_AdminUrl, _XUrl, YUrl}) ->
    {ok, Code, _, _} = test_request:put(YUrl ++ "/db/c", "{\"a\":1,\"_access\":[\"x\"]}"),
    ?_assertEqual(401, Code).

% Doc reads
should_let_admin_read_doc_with_access(_PortType, {AdminUrl, XUrl, _YUrl}) ->
    {ok, 201, _, _} = test_request:put(XUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, Code, _, _} = test_request:get(AdminUrl ++ "/db/a"),
    ?_assertEqual(200, Code).

user_with_access_can_read_doc(_PortType, {AdminUrl, XUrl, _YUrl}) ->
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, Code, _, _} = test_request:get(XUrl ++ "/db/a"),
    ?_assertEqual(200, Code).

user_without_access_can_not_read_doc(_PortType, {AdminUrl, _XUrl, YUrl}) ->
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, Code, _, _} = test_request:get(YUrl ++ "/db/a"),
    ?_assertEqual(401, Code).

% Doc deletes
should_let_admin_delete_doc_with_access(_PortType, {AdminUrl, XUrl, _YUrl}) ->
    {ok, 201, _, _} = test_request:put(XUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, Code, _, _} = test_request:delete(AdminUrl ++ "/db/a?rev=1-967a00dff5e02add41819138abb3284d"),
    ?_assertEqual(200, Code).

should_let_user_delete_doc_for_themselves(_PortType, {AdminUrl, XUrl, _YUrl}) ->
    {ok, 201, _, _} = test_request:put(XUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, Code, _, _} = test_request:delete(XUrl ++ "/db/a?rev=1-967a00dff5e02add41819138abb3284d"),
    ?_assertEqual(200, Code).

should_not_let_user_delete_doc_for_someone_else(_PortType, {_AdminUrl, XUrl, YUrl}) ->
    {ok, 201, _, _} = test_request:put(XUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, Code, _, _} = test_request:delete(YUrl ++ "/db/a?rev=1-967a00dff5e02add41819138abb3284d"),
    ?_assertEqual(401, Code).

% _all_docs with include_docs
should_let_admin_fetch_all_docs(_PortType, {AdminUrl, _XUrl, _YUrl}) ->
    Admin_Fetch_Docs_Body = <<"{\"total_rows\":4,\"offset\":0,\"rows\":[\r\n{\"id\":\"a\",\"key\":\"a\",\"value\":{\"rev\":\"1-967a00dff5e02add41819138abb3284d\"},\"doc\":{\"_id\":\"a\",\"_rev\":\"1-967a00dff5e02add41819138abb3284d\",\"_access\":{}}},\r\n{\"id\":\"b\",\"key\":\"b\",\"value\":{\"rev\":\"1-967a00dff5e02add41819138abb3284d\"},\"doc\":{\"_id\":\"b\",\"_rev\":\"1-967a00dff5e02add41819138abb3284d\",\"_access\":{}}},\r\n{\"id\":\"c\",\"key\":\"c\",\"value\":{\"rev\":\"1-967a00dff5e02add41819138abb3284d\"},\"doc\":{\"_id\":\"c\",\"_rev\":\"1-967a00dff5e02add41819138abb3284d\",\"_access\":{}}},\r\n{\"id\":\"d\",\"key\":\"d\",\"value\":{\"rev\":\"1-967a00dff5e02add41819138abb3284d\"},\"doc\":{\"_id\":\"d\",\"_rev\":\"1-967a00dff5e02add41819138abb3284d\",\"_access\":{}}}\r\n]}\n">>,
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/b", "{\"b\":2,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/c", "{\"c\":3,\"_access\":[\"y\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/d", "{\"d\":4,\"_access\":[\"y\"]}"),
    {ok, 200, _, Body} = test_request:get(AdminUrl ++ "/db/_all_docs?include_docs=true"),
    ?_assertEqual(Admin_Fetch_Docs_Body, Body).

should_let_user_fetch_their_own_all_docs(_PortType, {AdminUrl, XUrl, YUrl}) ->
    Admin_Fetch_Docs_Body = <<"{\"total_rows\":2,\"offset\":0,\"rows\":[\r\n{\"id\":\"a\",\"key\":\"a\",\"value\":{\"rev\":\"1-967a00dff5e02add41819138abb3284d\"},\"doc\":{\"_id\":\"a\",\"_rev\":\"1-967a00dff5e02add41819138abb3284d\",\"_access\":{}}},\r\n{\"id\":\"b\",\"key\":\"b\",\"value\":{\"rev\":\"1-967a00dff5e02add41819138abb3284d\"},\"doc\":{\"_id\":\"b\",\"_rev\":\"1-967a00dff5e02add41819138abb3284d\",\"_access\":{}}}]}\n">>,
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(XUrl ++ "/db/b", "{\"b\":2,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/c", "{\"c\":3,\"_access\":[\"y\"]}"),
    {ok, 201, _, _} = test_request:put(YUrl ++ "/db/d", "{\"d\":4,\"_access\":[\"y\"]}"),
    {ok, 200, _, Body} = test_request:get(XUrl ++ "/db/_all_docs?include_docs=true"),
    ?_assertEqual(Admin_Fetch_Docs_Body, Body).
% _changes
should_let_admin_fetch_changes(_PortType, {AdminUrl, _XUrl, _YUrl}) ->
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/b", "{\"b\":2,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/c", "{\"c\":3,\"_access\":[\"y\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/d", "{\"d\":4,\"_access\":[\"y\"]}"),
    {ok, 200, _, Body} = test_request:get(AdminUrl ++ "/db/_changes"),
    {Json} = jiffy:decode(Body),
    AmountOfDocs = length(proplists:get_value(<<"results">>, Json)),
    ?_assertEqual(4, AmountOfDocs).

should_let_user_fetch_their_own_changes(_PortType, {AdminUrl, XUrl, _YUrl}) ->
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/b", "{\"b\":2,\"_access\":[\"x\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/c", "{\"c\":3,\"_access\":[\"y\"]}"),
    {ok, 201, _, _} = test_request:put(AdminUrl ++ "/db/d", "{\"d\":4,\"_access\":[\"y\"]}"),
    {ok, 200, _, Body} = test_request:get(XUrl ++ "/db/_changes"),
    {Json} = jiffy:decode(Body),
    AmountOfDocs = length(proplists:get_value(<<"results">>, Json)),
    ?_assertEqual(2, AmountOfDocs).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

port() ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port)).
