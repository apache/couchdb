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

-module(couch_peruser_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(ADMIN_USERNAME, "admin").
-define(ADMIN_PASSWORD, "secret").

-define(WAIT_FOR_DB_TIMEOUT, 1000).
-define(WAIT_FOR_USER_DELETE_TIMEOUT, 3000).

setup_all() ->
    TestCtx = test_util:start_couch([chttpd]),
    ok = application:start(couch_peruser),
    Hashed = couch_passwords:hash_admin_password(?ADMIN_PASSWORD),
    ok = config:set("admins", ?ADMIN_USERNAME, ?b2l(Hashed), _Persist=false),
    TestCtx.

teardown_all(TestCtx) ->
    config:delete("admins", ?ADMIN_USERNAME),
    ok = application:stop(couch_peruser),
    test_util:stop_couch(TestCtx).

setup() ->
    TestAuthDb = ?tempdb(),
    do_request(put, get_base_url() ++ "/" ++ ?b2l(TestAuthDb)),
    do_request(put, get_cluster_base_url() ++ "/" ++ ?b2l(TestAuthDb)),
    set_config("couch_httpd_auth", "authentication_db", ?b2l(TestAuthDb)),
    set_config("couch_peruser", "cluster_quiet_period", "1"),
    set_config("couch_peruser", "cluster_start_period", "1"),
    set_config("couch_peruser", "enable", "true"),
    set_config("cluster", "n", "1"),
    TestAuthDb.

teardown(TestAuthDb) ->
    set_config("couch_peruser", "enable", "false"),
    set_config("couch_peruser", "delete_dbs", "false"),
    set_config("couch_httpd_auth", "authentication_db", "_users"),
    set_config("couch_peruser", "cluster_quiet_period", "60"),
    set_config("couch_peruser", "cluster_start_period", "5"),
    set_config("cluster", "n", "3"),
    do_request(delete, get_cluster_base_url() ++ "/" ++ ?b2l(TestAuthDb)),
    do_request(delete, get_base_url() ++ "/" ++ ?b2l(TestAuthDb)),
    lists:foreach(fun(DbName) ->
        case binary:part(DbName, 0, 7) of
            <<"userdb-">> -> delete_db(DbName);
            _ -> ok
        end
    end, all_dbs()).

set_config(Section, Key, Value) ->
    ok = config:set(Section, Key, Value, _Persist=false).

delete_config(Section, Key) ->
    ok = config:delete(Section, Key, _Persist=false).

do_request(Method, Url) ->
    Headers = [{basic_auth, {?ADMIN_USERNAME, ?ADMIN_PASSWORD}}],
    {ok, _, _, _} = test_request:request(Method, Url, Headers).

do_request(Method, Url, Body) ->
    Headers = [
        {basic_auth, {?ADMIN_USERNAME, ?ADMIN_PASSWORD}},
        {"Content-Type", "application/json"}],
    {ok, _, _, _} = test_request:request(Method, Url, Headers, Body).

do_anon_request(Method, Url, Body) ->
    Headers = [
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
        get_cluster_base_url(), "/", ?b2l(AuthDb), "/org.couchdb.user:", Name]),
    {ok, 201, _, _} = do_request(put, Url, Body).

create_anon_user(AuthDb, Name) ->
    Body = "{\"name\":\"" ++ Name ++
        "\",\"type\":\"user\",\"roles\":[],\"password\":\"secret\"}",
    Url = lists:concat([
        get_cluster_base_url(), "/", ?b2l(AuthDb), "/org.couchdb.user:", Name]),
    {ok, 201, _, _} = do_anon_request(put, Url, Body).

delete_user(AuthDb, Name) ->
    Url = lists:concat([get_cluster_base_url(), "/", ?b2l(AuthDb),
        "/org.couchdb.user:", Name]),
    {ok, 200, _, Body} = do_request(get, Url),
    {DocProps} = jiffy:decode(Body),
    Rev = proplists:get_value(<<"_rev">>, DocProps),
    {ok, 200, _, _} = do_request(delete, Url ++ "?rev=" ++ ?b2l(Rev)).

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

all_dbs_with_errors() ->
    {Result, StatusCode, _Headers, Body} = do_request(get, get_cluster_base_url() ++ "/_all_dbs"),
    {Result, StatusCode, _Headers, jiffy:decode(Body)}.

get_base_url() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

get_cluster_base_url() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.


should_create_user_db_with_default(TestAuthDb) ->
    create_user(TestAuthDb, "foo"),
    wait_for_db_create(<<"userdb-666f6f">>),
    {ok, DbInfo} = fabric:get_db_info(<<"userdb-666f6f">>),
    {ClusterInfo} = couch_util:get_value(cluster, DbInfo),
    [
        ?_assert(lists:member(<<"userdb-666f6f">>, all_dbs())),
        ?_assertEqual(1, couch_util:get_value(q, ClusterInfo))
    ].

should_create_user_db_with_custom_prefix(TestAuthDb) ->
    set_config("couch_peruser", "database_prefix", "newuserdb-"),
    create_user(TestAuthDb, "fooo"),
    wait_for_db_create(<<"newuserdb-666f6f6f">>),
    delete_config("couch_peruser", "database_prefix"),
    ?_assert(lists:member(<<"newuserdb-666f6f6f">>, all_dbs())).

should_create_user_db_with_custom_special_prefix(TestAuthDb) ->
    set_config("couch_peruser", "database_prefix", "userdb_$()+--/"),
    create_user(TestAuthDb, "fooo"),
    wait_for_db_create(<<"userdb_$()+--/666f6f6f">>),
    delete_config("couch_peruser", "database_prefix"),
    ?_assert(lists:member(<<"userdb_$()+--/666f6f6f">>, all_dbs())).

should_create_anon_user_db_with_default(TestAuthDb) ->
    create_anon_user(TestAuthDb, "fooo"),
    wait_for_db_create(<<"userdb-666f6f6f">>),
    {ok, DbInfo} = fabric:get_db_info(<<"userdb-666f6f6f">>),
    {ClusterInfo} = couch_util:get_value(cluster, DbInfo),
    [
        ?_assert(lists:member(<<"userdb-666f6f6f">>, all_dbs())),
        ?_assertEqual(1, couch_util:get_value(q, ClusterInfo))
    ].

should_create_anon_user_db_with_custom_prefix(TestAuthDb) ->
    set_config("couch_peruser", "database_prefix", "newuserdb-"),
    create_anon_user(TestAuthDb, "fooo"),
    wait_for_db_create(<<"newuserdb-666f6f6f">>),
    delete_config("couch_peruser", "database_prefix"),
    ?_assert(lists:member(<<"newuserdb-666f6f6f">>, all_dbs())).

should_create_anon_user_db_with_custom_special_prefix(TestAuthDb) ->
    set_config("couch_peruser", "database_prefix", "userdb_$()+--/"),
    create_anon_user(TestAuthDb, "fooo"),
    wait_for_db_create(<<"userdb_$()+--/666f6f6f">>),
    delete_config("couch_peruser", "database_prefix"),
    ?_assert(lists:member(<<"userdb_$()+--/666f6f6f">>, all_dbs())).

should_create_user_db_with_q4(TestAuthDb) ->
    set_config("couch_peruser", "q", "4"),
    create_user(TestAuthDb, "foo"),
    wait_for_db_create(<<"userdb-666f6f">>),
    {ok, DbInfo} = fabric:get_db_info(<<"userdb-666f6f">>),
    {ClusterInfo} = couch_util:get_value(cluster, DbInfo),
    delete_config("couch_peruser", "q"),

    [
        ?_assert(lists:member(<<"userdb-666f6f">>, all_dbs())),
        ?_assertEqual(4, couch_util:get_value(q, ClusterInfo))
    ].

should_create_anon_user_db_with_q4(TestAuthDb) ->
    set_config("couch_peruser", "q", "4"),
    create_anon_user(TestAuthDb, "fooo"),
    wait_for_db_create(<<"userdb-666f6f6f">>),
    {ok, TargetInfo} = fabric:get_db_info(<<"userdb-666f6f6f">>),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    delete_config("couch_peruser", "q"),
    [
        ?_assert(lists:member(<<"userdb-666f6f6f">>, all_dbs())),
        ?_assertEqual(4, couch_util:get_value(q, ClusterInfo))
    ].

should_not_delete_user_db(TestAuthDb) ->
    User = "foo",
    UserDbName = <<"userdb-666f6f">>,
    create_user(TestAuthDb, User),
    wait_for_db_create(<<"userdb-666f6f">>),
    AfterCreate = lists:member(UserDbName, all_dbs()),
    delete_user(TestAuthDb, User),
    timer:sleep(?WAIT_FOR_USER_DELETE_TIMEOUT),
    AfterDelete = lists:member(UserDbName, all_dbs()),
    [?_assert(AfterCreate), ?_assert(AfterDelete)].

should_delete_user_db(TestAuthDb) ->
    User = "bar",
    UserDbName = <<"userdb-626172">>,
    set_config("couch_peruser", "delete_dbs", "true"),
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
    AfterCreate = lists:member(UserDbName, all_dbs()),
    delete_user(TestAuthDb, User),
    wait_for_db_delete(UserDbName),
    AfterDelete = lists:member(UserDbName, all_dbs()),
    [?_assert(AfterCreate), ?_assertNot(AfterDelete)].

should_delete_user_db_with_custom_prefix(TestAuthDb) ->
    User = "bar",
    UserDbName = <<"newuserdb-626172">>,
    set_config("couch_peruser", "delete_dbs", "true"),
    set_config("couch_peruser", "database_prefix", "newuserdb-"),
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
    AfterCreate = lists:member(UserDbName, all_dbs()),
    delete_user(TestAuthDb, User),
    wait_for_db_delete(UserDbName),
    delete_config("couch_peruser", "database_prefix"),
    AfterDelete = lists:member(UserDbName, all_dbs()),
    [
        ?_assert(AfterCreate),
        ?_assertNot(AfterDelete)
    ].

should_delete_user_db_with_custom_special_prefix(TestAuthDb) ->
    User = "bar",
    UserDbName = <<"userdb_$()+--/626172">>,
    set_config("couch_peruser", "delete_dbs", "true"),
    set_config("couch_peruser", "database_prefix", "userdb_$()+--/"),
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
    AfterCreate = lists:member(UserDbName, all_dbs()),
    delete_user(TestAuthDb, User),
    wait_for_db_delete(UserDbName),
    delete_config("couch_peruser", "database_prefix"),
    AfterDelete = lists:member(UserDbName, all_dbs()),
    [
        ?_assert(AfterCreate),
        ?_assertNot(AfterDelete)
    ].

should_reflect_config_changes(TestAuthDb) ->
    User = "baz",
    UserDbName = <<"userdb-62617a">>,
    set_config("couch_peruser", "delete_dbs", "true"),
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
    AfterCreate1 = lists:member(UserDbName, all_dbs()),
    delete_user(TestAuthDb, User),
    timer:sleep(?WAIT_FOR_USER_DELETE_TIMEOUT),
    wait_for_db_delete(UserDbName),
    AfterDelete1 = lists:member(UserDbName, all_dbs()),
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
    AfterCreate2 = lists:member(UserDbName, all_dbs()),
    set_config("couch_peruser", "delete_dbs", "false"),
    delete_user(TestAuthDb, User),
    timer:sleep(?WAIT_FOR_USER_DELETE_TIMEOUT),
    AfterDelete2 = lists:member(UserDbName, all_dbs()),
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
    set_config("couch_peruser", "delete_dbs", "true"),
    delete_user(TestAuthDb, User),
    wait_for_db_delete(UserDbName),
    AfterDelete3 = lists:member(UserDbName, all_dbs()),
    set_config("couch_peruser", "enable", "false"),
    create_user(TestAuthDb, User),
    timer:sleep(?WAIT_FOR_USER_DELETE_TIMEOUT),
    AfterCreate3 = lists:member(UserDbName, all_dbs()),
    [
        ?_assert(AfterCreate1),
        ?_assertNot(AfterDelete1),
        ?_assert(AfterCreate2),
        ?_assert(AfterDelete2),
        ?_assertNot(AfterDelete3),
        ?_assertNot(AfterCreate3)
    ].


should_add_user_to_db_admins(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
    ?_assertEqual(
        {[{<<"names">>,[<<"qux">>]}]},
        proplists:get_value(<<"admins">>, get_security(UserDbName))).

should_add_user_to_db_members(TestAuthDb) ->
    User = "qux",
    UserDbName = <<"userdb-717578">>,
    create_user(TestAuthDb, User),
    wait_for_db_create(UserDbName),
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
    wait_for_security_create(<<"admins">>, User, UserDbName),
    {AdminProperties} = proplists:get_value(<<"admins">>,
        get_security(UserDbName)),
    AdminNames = proplists:get_value(<<"names">>, AdminProperties),
    [
      ?_assert(lists:member(<<"foo">>, AdminNames)),
      ?_assert(lists:member(<<"bar">>, AdminNames)),
      ?_assert(lists:member(<<"qux">>, AdminNames))
    ].

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
    wait_for_security_create(<<"members">>, User, UserDbName),
    {MemberProperties} = proplists:get_value(<<"members">>,
        get_security(UserDbName)),
    MemberNames = proplists:get_value(<<"names">>, MemberProperties),
    [
      ?_assert(lists:member(<<"pow">>, MemberNames)),
      ?_assert(lists:member(<<"wow">>, MemberNames)),
      ?_assert(lists:member(<<"qux">>, MemberNames))
    ].

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
    wait_for_security_create(<<"admins">>, User, UserDbName),
    {AdminProperties} = proplists:get_value(<<"admins">>,
        get_security(UserDbName)),
    AdminNames = proplists:get_value(<<"names">>, AdminProperties),
    FooBefore = lists:member(<<"foo">>, AdminNames),
    BarBefore = lists:member(<<"bar">>, AdminNames),
    QuxBefore = lists:member(<<"qux">>, AdminNames),
    delete_user(TestAuthDb, User),
    wait_for_security_delete(<<"admins">>, User, UserDbName),
    {NewAdminProperties} = proplists:get_value(<<"admins">>,
        get_security(UserDbName)),
    NewAdminNames = proplists:get_value(<<"names">>, NewAdminProperties),
    FooAfter = lists:member(<<"foo">>, NewAdminNames),
    BarAfter = lists:member(<<"bar">>, NewAdminNames),
    QuxAfter = lists:member(<<"qux">>, NewAdminNames),
    [
      ?_assert(FooBefore),
      ?_assert(BarBefore),
      ?_assert(QuxBefore),
      ?_assert(FooAfter),
      ?_assert(BarAfter),
      ?_assertNot(QuxAfter)
    ].

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
    wait_for_security_create(<<"members">>, User, UserDbName),
    {MemberProperties} = proplists:get_value(<<"members">>,
        get_security(UserDbName)),
    MemberNames = proplists:get_value(<<"names">>, MemberProperties),
    PowBefore = lists:member(<<"pow">>, MemberNames),
    WowBefore = lists:member(<<"wow">>, MemberNames),
    QuxBefore = lists:member(<<"qux">>, MemberNames),
    delete_user(TestAuthDb, User),
    wait_for_security_delete(<<"members">>, User, UserDbName),
    {NewMemberProperties} = proplists:get_value(<<"members">>,
        get_security(UserDbName)),
    NewMemberNames = proplists:get_value(<<"names">>, NewMemberProperties),
    PowAfter = lists:member(<<"pow">>, NewMemberNames),
    WowAfter = lists:member(<<"wow">>, NewMemberNames),
    QuxAfter = lists:member(<<"qux">>, NewMemberNames),
    [
      ?_assert(PowBefore),
      ?_assert(WowBefore),
      ?_assert(QuxBefore),
      ?_assert(PowAfter),
      ?_assert(WowAfter),
      ?_assertNot(QuxAfter)
    ].

% infinite loop waiting for a db to be created, either this returns true
% or we get a test timeout error
wait_for_db_create(UserDbName) ->
    case all_dbs_with_errors() of
        {error, _, _ , _} ->
            timer:sleep(?WAIT_FOR_DB_TIMEOUT),
            wait_for_db_create(UserDbName);
        {ok, _, _, AllDbs} ->
            case lists:member(UserDbName, AllDbs) of
                true -> true;
                _Else ->
                    timer:sleep(?WAIT_FOR_DB_TIMEOUT),
                    wait_for_db_create(UserDbName)
            end
    end.

% infinite loop waiting for a db to be deleted, either this returns true
% or we get a test timeout error
wait_for_db_delete(UserDbName) ->
    case all_dbs_with_errors() of
        {ok, 500, _ , _} ->
            timer:sleep(?WAIT_FOR_DB_TIMEOUT),
            wait_for_db_delete(UserDbName);
        {ok, _, _, AllDbs} ->
            case not lists:member(UserDbName, AllDbs) of
                true -> true;
                _Else ->
                    timer:sleep(?WAIT_FOR_DB_TIMEOUT),
                    wait_for_db_delete(UserDbName)
            end
    end.

wait_for_security_create(Type, User, UserDbName) ->
    {MemberProperties} = proplists:get_value(Type,
        get_security(UserDbName)),
    Names = proplists:get_value(<<"names">>, MemberProperties),

    case lists:member(?l2b(User), Names) of
        true -> true;
        _Else ->
            timer:sleep(?WAIT_FOR_DB_TIMEOUT),
            wait_for_security_create(Type, User, UserDbName)
    end.

wait_for_security_delete(Type, User, UserDbName) ->
    {MemberProperties} = proplists:get_value(Type,
        get_security(UserDbName)),
    Names = proplists:get_value(<<"names">>, MemberProperties),

    case not lists:member(?l2b(User), Names) of
        true -> true;
        _Else ->
            timer:sleep(?WAIT_FOR_DB_TIMEOUT),
            wait_for_security_delete(Type, User, UserDbName)
    end.

couch_peruser_test_() ->
    {
        "couch_peruser test",
        {
            setup,
            fun setup_all/0, fun teardown_all/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_create_anon_user_db_with_default/1,
                    fun should_create_anon_user_db_with_custom_prefix/1,
                    fun should_create_anon_user_db_with_custom_special_prefix/1,
                    fun should_create_user_db_with_default/1,
                    fun should_create_user_db_with_custom_prefix/1,
                    fun should_create_user_db_with_custom_special_prefix/1,
                    fun should_create_user_db_with_q4/1,
                    fun should_create_anon_user_db_with_q4/1,
                    fun should_not_delete_user_db/1,
                    fun should_delete_user_db/1,
                    fun should_delete_user_db_with_custom_prefix/1,
                    fun should_delete_user_db_with_custom_special_prefix/1,
                    fun should_reflect_config_changes/1,
                    fun should_add_user_to_db_admins/1,
                    fun should_add_user_to_db_members/1,
                    fun should_not_remove_existing_db_admins/1,
                    fun should_not_remove_existing_db_members/1,
                    fun should_remove_user_from_db_admins/1,
                    fun should_remove_user_from_db_members/1
                ]
            }
        }
    }.
