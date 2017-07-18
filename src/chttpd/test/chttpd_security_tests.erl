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

-module(chttpd_security_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).

-define(TEST_MEMBER, "test_member").
-define(TEST_MEMBER_PASS, "test_member_pass").
-define(TEST_MEMBER_AUTH, {basic_auth, {?TEST_MEMBER, ?TEST_MEMBER_PASS}}).

-define(TEST_ADMIN, "test_admin").
-define(TEST_ADMIN_PASS, "test_admin_pass").
-define(TEST_ADMIN_AUTH, {basic_auth, {?TEST_ADMIN, ?TEST_ADMIN_PASS}}).



-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(FIXTURE_TXT, ?ABS_PATH(?FILE)).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    UserDb = <<"_users">>,
    TmpDb = ?tempdb(),
    ok = config:set("couch_httpd_auth", "authentication_db", ?b2l(UserDb)),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    BaseUrl = lists:concat(["http://", Addr, ":", Port, "/"]),
    Url = lists:concat([BaseUrl, ?b2l(TmpDb)]),
    UsersUrl = lists:concat([BaseUrl, ?b2l(UserDb)]),
    create_db(UsersUrl),
    create_db(Url),
    create_design_doc(Url),
    create_user(UsersUrl,?TEST_MEMBER,?TEST_MEMBER_PASS,[<<?TEST_MEMBER>>]),
    create_user(UsersUrl,?TEST_ADMIN,?TEST_ADMIN_PASS,[<<?TEST_ADMIN>>]),
    set_security(Url),
    [Url, UsersUrl].

teardown([Url,UsersUrl]) ->
    delete_db(Url),
    delete_db(UsersUrl),
    ok = config:delete("admins", ?USER, _Persist=false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

create_design_doc(Url) ->
    {ok, Status, _, _} = test_request:put(lists:concat([Url, '/_design/test']), [?CONTENT_JSON, ?AUTH],
            "{\"id\":\"_design/test\"}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

set_security(Url) ->
    SecurityUrl = lists:concat([Url, "/_security"]),
    SecurityProperties = [
        {<<"admins">>,{[{<<"roles">>,[<<?TEST_ADMIN>>]}]}},
        {<<"members">>,{[{<<"roles">>,[<<?TEST_MEMBER>>]}]}}
    ],

    Body = jiffy:encode({SecurityProperties}),
    {ok, Status, _, _} = test_request:put(SecurityUrl, [?CONTENT_JSON, ?AUTH], Body),
    ?assert(Status =:= 200).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

create_user(UsersUrl, Name, Password, Roles) ->
   
    Body = "{\"name\":\"" ++ Name ++
        "\",\"type\":\"user\",\"roles\":" ++ erlang:binary_to_list(jiffy:encode(Roles)) ++ ",\"password\":\"" ++ Password ++"\"}",
    
    Url = lists:concat([
        UsersUrl, "/org.couchdb.user:", Name]),
    {ok, 201, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], Body).


all_test_() ->
    {
        "chttpd security tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_allow_admin_db_compaction/1,
                    fun should_disallow_anonymous_db_compaction/1,
                    fun should_disallow_db_member_db_compaction/1,
                    fun should_allow_db_admin_db_compaction/1,
                    fun should_allow_admin_view_compaction/1,
                    fun should_disallow_anonymous_view_compaction/1,
                    fun should_allow_admin_db_view_cleanup/1,
                    fun should_disallow_anonymous_db_view_cleanup/1
                ]
            }
        }
    }.

should_allow_admin_db_compaction([Url,_UsersUrl]) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact",
                [?CONTENT_JSON, ?AUTH], ""),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = ResultJson,
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_disallow_anonymous_db_compaction([Url,_UsersUrl]) ->
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact",
        [?CONTENT_JSON], ""),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson} = ResultJson,
    ErrType = couch_util:get_value(<<"error">>, InnerJson),
    ?_assertEqual(<<"unauthorized">>,ErrType).

should_disallow_db_member_db_compaction([Url,_UsersUrl]) ->
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact",
        [?CONTENT_JSON, ?TEST_MEMBER_AUTH], ""),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson} = ResultJson,
    ErrType = couch_util:get_value(<<"error">>, InnerJson),
    ?_assertEqual(<<"unauthorized">>,ErrType).

should_allow_db_admin_db_compaction([Url,_UsersUrl]) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact",
                [?CONTENT_JSON, ?TEST_ADMIN_AUTH], ""),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = ResultJson,
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_allow_admin_view_compaction([Url,_UsersUrl]) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact/test",
                [?CONTENT_JSON, ?AUTH], ""),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = ResultJson,
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_disallow_anonymous_view_compaction([Url,_UsersUrl]) ->
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact/test",
        [?CONTENT_JSON], ""),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson} = ResultJson,
    ErrType = couch_util:get_value(<<"error">>, InnerJson),
    ?_assertEqual(<<"unauthorized">>,ErrType).

should_allow_admin_db_view_cleanup([Url,_UsersUrl]) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_view_cleanup",
                [?CONTENT_JSON, ?AUTH], ""),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = ResultJson,
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_disallow_anonymous_db_view_cleanup([Url,_UsersUrl]) ->
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_view_cleanup",
        [?CONTENT_JSON], ""),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson} = ResultJson,
    ErrType = couch_util:get_value(<<"error">>, InnerJson),
    ?_assertEqual(<<"unauthorized">>, ErrType).
