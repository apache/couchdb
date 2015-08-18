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

-module(couchdb_peruser_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(ADMIN_USERNAME, "admin").
-define(ADMIN_PASSWORD, "secret").

start_couch() ->
    test_util:start_couch([chttpd]).

stop_couch(TestCtx) ->
    test_util:stop_couch(TestCtx).

setup() ->
    TestAuthDb = ?tempdb(),
    config:set("admins", ?ADMIN_USERNAME, ?ADMIN_PASSWORD),
    do_request(put, get_base_url() ++ "/" ++ ?b2l(TestAuthDb)),
    set_config("couch_httpd_auth", "authentication_db", ?b2l(TestAuthDb)),
    set_config("couchdb_peruser", "enable", "true"),
    TestAuthDb.

teardown(TestAuthDb) ->
    set_config("couch_httpd_auth", "authentication_db", "_users"),
    set_config("couchdb_peruser", "enable", "false"),
    set_config("couchdb_peruser", "delete_dbs", "false"),
    do_request(delete, get_base_url() ++ "/" ++ ?b2l(TestAuthDb)),
    lists:foreach(fun (DbName) ->
        delete_db(DbName)
    end, all_dbs()),
    config:delete("admins", ?ADMIN_USERNAME).

set_config(Section, Key, Value) ->
    Url = lists:concat([
        get_base_url(), "/_config/", Section, "/", Key]),
    do_request(put, Url, "\"" ++ Value ++ "\"").

do_request(Method, Url) ->
    Headers = [{basic_auth, {?ADMIN_USERNAME, ?ADMIN_PASSWORD}}],
    {ok, _, _, _} = test_request:request(Method, Url, Headers).

do_request(Method, Url, Body) ->
    Headers = [
        {basic_auth, {?ADMIN_USERNAME, ?ADMIN_PASSWORD}},
        {"Content-Type", "application/json"}],
    {ok, _, _, _} = test_request:request(Method, Url, Headers, Body).

create_db(DbName) ->
    {ok, _, _, _} = do_request(put, get_cluster_base_url() ++ "/" ++ ?b2l(DbName)).

delete_db(DbName) ->
    {ok, _, _, _} = do_request(delete, get_cluster_base_url() ++ "/" ++ ?b2l(DbName)).

create_user(AuthDb, Name) ->
    Body = "{\"name\":\"" ++ Name ++
        "\",\"type\":\"user\",\"roles\":[],\"password\":\"secret\"}",
    Url = lists:concat([
        get_base_url(), "/", ?b2l(AuthDb), "/org.couchdb.user:", Name]),
    {ok, 201, _, _} = do_request(put, Url, Body),
    % let's proceed after giving couchdb_peruser some time to create the user db
    timer:sleep(1000).

delete_user(AuthDb, Name) ->
    Url = lists:concat([get_base_url(), "/", ?b2l(AuthDb),
        "/org.couchdb.user:", Name]),
    {ok, 200, _, Body} = do_request(get, Url),
    {DocProps} = jiffy:decode(Body),
    Rev = proplists:get_value(<<"_rev">>, DocProps),
    {ok, 200, _, _} = do_request(delete, Url ++ "?rev=" ++ ?b2l(Rev)),
    % let's proceed after giving couchdb_peruser some time to delete the user db
    timer:sleep(1000).

get_security(DbName) ->
    Url = lists:concat([
        get_cluster_base_url(), "/", ?b2l(DbName), "/_security"]),
    {ok, 200, _, Body} = do_request(get, Url),
    {SecurityProperties} = jiffy:decode(Body),
    SecurityProperties.

set_security(DbName, SecurityProperties) ->
    Url = lists:concat([
        get_cluster_base_url(), "/", ?b2l(DbName), "/_security"]),
    Body = jiffy:encode({SecurityProperties}),
    {ok, 200, _, _} = do_request(put, Url, Body).

all_dbs() ->
    {ok, 200, _, Body} = do_request(get, get_cluster_base_url() ++ "/_all_dbs"),
    jiffy:decode(Body).

get_base_url() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

get_cluster_base_url() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

should_create_user_db(TestAuthDb) ->
    create_user(TestAuthDb, "foo"),
    ?_assert(lists:member(<<"userdb-666f6f">>, all_dbs())).

should_not_delete_user_db(TestAuthDb) ->
    User = "foo",
    UserDbName = <<"userdb-666f6f">>,
    create_user(TestAuthDb, User),
    ?assert(lists:member(UserDbName, all_dbs())),
    delete_user(TestAuthDb, User),
    ?_assert(lists:member(UserDbName, all_dbs())).

should_delete_user_db(TestAuthDb) ->
    User = "bar",
    UserDbName = <<"userdb-626172">>,
    set_config("couchdb_peruser", "delete_dbs", "true"),
    create_user(TestAuthDb, User),
    ?assert(lists:member(UserDbName, all_dbs())),
    delete_user(TestAuthDb, User),
    ?_assert(not lists:member(UserDbName, all_dbs())).

should_reflect_config_changes(TestAuthDb) ->
    User = "baz",
    UserDbName = <<"userdb-62617a">>,
    set_config("couchdb_peruser", "delete_dbs", "true"),
    create_user(TestAuthDb, User),
    ?assert(lists:member(UserDbName, all_dbs())),
    delete_user(TestAuthDb, User),
    ?assert(not lists:member(UserDbName, all_dbs())),
    create_user(TestAuthDb, User),
    ?assert(lists:member(UserDbName, all_dbs())),
    set_config("couchdb_peruser", "delete_dbs", "false"),
    delete_user(TestAuthDb, User),
    ?assert(lists:member(UserDbName, all_dbs())),
    create_user(TestAuthDb, User),
    set_config("couchdb_peruser", "delete_dbs", "true"),
    delete_user(TestAuthDb, User),
    ?assert(not lists:member(UserDbName, all_dbs())),
    set_config("couchdb_peruser", "enable", "false"),
    create_user(TestAuthDb, User),
    ?_assert(not lists:member(UserDbName, all_dbs())).

should_assign_user_to_db_admins(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    create_user(TestAuthDb, User),
    ?_assertEqual(
        {[{<<"names">>,[<<"qux">>]}]},
        proplists:get_value(<<"admins">>, get_security(UserDbName))).

should_assign_user_to_db_members(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    create_user(TestAuthDb, User),
    ?_assertEqual(
        {[{<<"names">>,[<<"qux">>]}]},
        proplists:get_value(<<"members">>, get_security(UserDbName))).

should_not_remove_existing_db_admins(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    SecurityProperties = [
        {<<"admins">>,{[{<<"names">>,[<<"foo">>,<<"bar">>]}]}},
        {<<"members">>,{[{<<"names">>,[<<"baz">>,<<"pow">>]}]}}
    ],
    create_db(UserDbName),
    set_security(UserDbName, SecurityProperties),
    create_user(TestAuthDb, User),
    {AdminProperties} = proplists:get_value(<<"admins">>,
        get_security(UserDbName)),
    AdminNames = proplists:get_value(<<"names">>, AdminProperties),
    ?_assert(lists:member(<<"foo">>, AdminNames)),
    ?_assert(lists:member(<<"bar">>, AdminNames)),
    ?_assert(lists:member(<<"qux">>, AdminNames)).

should_not_remove_existing_db_members(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    SecurityProperties = [
        {<<"admins">>,{[{<<"names">>,[<<"pow">>,<<"wow">>]}]}},
        {<<"members">>,{[{<<"names">>,[<<"pow">>,<<"wow">>]}]}}
    ],
    create_db(UserDbName),
    set_security(UserDbName, SecurityProperties),
    create_user(TestAuthDb, User),
    {MemberProperties} = proplists:get_value(<<"members">>,
        get_security(UserDbName)),
    MemberNames = proplists:get_value(<<"names">>, MemberProperties),
    ?_assert(lists:member(<<"pow">>, MemberNames)),
    ?_assert(lists:member(<<"wow">>, MemberNames)),
    ?_assert(lists:member(<<"qux">>, MemberNames)).

should_remove_user_from_db_admins(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    SecurityProperties = [
        {<<"admins">>,{[{<<"names">>,[<<"foo">>,<<"bar">>]}]}},
        {<<"members">>,{[{<<"names">>,[<<"baz">>,<<"pow">>]}]}}
    ],
    create_db(UserDbName),
    set_security(UserDbName, SecurityProperties),
    create_user(TestAuthDb, User),
    {AdminProperties} = proplists:get_value(<<"admins">>,
        get_security(UserDbName)),
    AdminNames = proplists:get_value(<<"names">>, AdminProperties),
    ?assert(lists:member(<<"foo">>, AdminNames)),
    ?assert(lists:member(<<"bar">>, AdminNames)),
    ?assert(lists:member(<<"qux">>, AdminNames)),
    delete_user(TestAuthDb, User),
    {NewAdminProperties} = proplists:get_value(<<"admins">>,
        get_security(UserDbName)),
    NewAdminNames = proplists:get_value(<<"names">>, NewAdminProperties),
    ?_assert(lists:member(<<"foo">>, NewAdminNames)),
    ?_assert(lists:member(<<"bar">>, NewAdminNames)),
    ?_assert(not lists:member(<<"qux">>, NewAdminNames)).

should_remove_user_from_db_members(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    SecurityProperties = [
        {<<"admins">>,{[{<<"names">>,[<<"pow">>,<<"wow">>]}]}},
        {<<"members">>,{[{<<"names">>,[<<"pow">>,<<"wow">>]}]}}
    ],
    create_db(UserDbName),
    set_security(UserDbName, SecurityProperties),
    create_user(TestAuthDb, User),
    {MemberProperties} = proplists:get_value(<<"members">>,
        get_security(UserDbName)),
    MemberNames = proplists:get_value(<<"names">>, MemberProperties),
    ?assert(lists:member(<<"pow">>, MemberNames)),
    ?assert(lists:member(<<"wow">>, MemberNames)),
    ?assert(lists:member(<<"qux">>, MemberNames)),
    delete_user(TestAuthDb, User),
    {NewMemberProperties} = proplists:get_value(<<"members">>,
        get_security(UserDbName)),
    NewMemberNames = proplists:get_value(<<"names">>, NewMemberProperties),
    ?_assert(lists:member(<<"foo">>, NewMemberNames)),
    ?_assert(lists:member(<<"bar">>, NewMemberNames)),
    ?_assert(not lists:member(<<"qux">>, NewMemberNames)).

couchdb_peruser_test_() ->
    {
        "couchdb_peruser test",
        {
            setup,
            fun start_couch/0, fun stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_create_user_db/1,
                    fun should_not_delete_user_db/1,
                    fun should_delete_user_db/1,
                    fun should_reflect_config_changes/1,
                    fun should_assign_user_to_db_admins/1,
                    fun should_assign_user_to_db_members/1,
                    fun should_not_remove_existing_db_admins/1,
                    fun should_not_remove_existing_db_members/1,
                    fun should_remove_user_from_db_admins/1,
                    fun should_remove_user_from_db_members/1
                ]
            }
        }
    }.
