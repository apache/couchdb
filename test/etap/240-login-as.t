#!/usr/bin/env escript
%% -*- erlang -*-

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

server() ->
    lists:concat([
        "http://127.0.0.1:", mochiweb_socket_server:get(couch_httpd, port), "/"
    ]).

admin_server() ->
    lists:concat([
        "http://", admin_user(), ":", admin_pass(), "@127.0.0.1:", mochiweb_socket_server:get(couch_httpd, port), "/"
    ]).


dbname() -> "etap-test-db".
admin_user() -> "admin".
admin_pass() -> "asd".

user() -> "foo".
pass() -> "bar".

admin_user_ctx() -> {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

main(_) ->
    test_util:init_code_path(),

    etap:plan(4),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server_sup:start_link(test_util:config_files()),
    ibrowse:start(),
    crypto:start(),

    timer:sleep(1000),
    couch_server:delete(list_to_binary(dbname()), [admin_user_ctx()]),
    {ok, Db} = couch_db:create(list_to_binary(dbname()), [admin_user_ctx()]),

    test_admin_party(),

    create_admin_user(),
    create_user(),

    test_non_existent_user(),
    test_existing_user(),

    %% restart boilerplate
    couch_db:close(Db),
    ok = couch_server:delete(couch_db:name(Db), [admin_user_ctx()]),
    ok = couch_server:delete("_users", [admin_user_ctx()]),
    timer:sleep(3000),
    couch_server_sup:stop(),

    ok.

create_admin_user() ->
    ok = couch_config:set("admins", admin_user(), admin_pass(), false).

create_user() ->
    UserDoc = {[
        {<<"_id">>, list_to_binary("org.couchdb.user:" ++ user())},
        {<<"type">>, <<"user">>},
        {<<"name">>, list_to_binary(user())},
        {<<"password">>, list_to_binary(pass())},
        {<<"roles">>, []}
    ]},
    case ibrowse:send_req(server() ++ "_users/org.couchdb.user:" ++ user(), [{"Content-Type", "application/json"}], put, ejson:encode(UserDoc)) of
        {ok, _, _, _} -> ok;
        _Else ->
            etap:bail("http PUT /_users/org.couchdb.user:" ++ user() ++ " request failed")
    end.

test_non_existent_user() ->
    case ibrowse:send_req(admin_server() ++ "_login_as/baz", [], post, []) of
        {ok, Code, _Headers, _Body} ->
            etap:is(Code, "404", "User not found");
        _Else ->
            etap:bail("http POST /_login_as/bar request failed")
    end.

test_existing_user() ->
    case ibrowse:send_req(admin_server() ++ "_login_as/foo", [], post, []) of
        {ok, Code, _Headers, Body} ->
            etap:is(Code, "200", "User found"),
            {[{Key, _}]} = ejson:decode(Body),
            etap:is(Key, <<"auth_token">>, "has auth_token");
        _Else ->
            etap:bail("http POST /_login_as/foo request failed")
    end.

test_admin_party() ->
    case ibrowse:send_req(server() ++ "_login_as/foo", [], post, []) of
        {ok, Code, _Headers, _Body} ->
            etap:is(Code, "404", "User not found");
        _Else ->
            etap:bail("http POST /_login_as request failed")
    end.
