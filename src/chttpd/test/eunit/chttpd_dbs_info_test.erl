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

-module(chttpd_dbs_info_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    Db1Url = lists:concat([Url, "db1"]),
    create_db(Db1Url),
    Db2Url = lists:concat([Url, "db2"]),
    create_db(Db2Url),
    mock(fabric_util),
    mock(chttpd_util),
    Url.

teardown(Url) ->
    meck:unload(),
    Db1Url = lists:concat([Url, "db1"]),
    Db2Url = lists:concat([Url, "db2"]),
    delete_db(Db1Url),
    delete_db(Db2Url),
    ok = config:delete("admins", ?USER, _Persist = false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

mock(Module) ->
    meck:new(Module, [passthrough]).

mock_timeout() ->
    meck:expect(fabric_util, request_timeout, fun() -> 0 end).

mock_db_not_exist() ->
    meck:expect(chttpd_util, get_db_info,
        fun(_) -> {error, database_does_not_exist} end).

dbs_info_test_() ->
    {
        "chttpd dbs info tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun get_db_info_should_return_db_info/1,
                    fun get_db_info_should_return_error_when_db_not_exist/1,
                    fun get_db_info_should_return_error_when_time_out/1,
                    fun should_return_error_for_put_dbs_info/1,
                    fun should_return_dbs_info_for_get_dbs_info/1,
                    fun should_return_nothing_when_db_not_exist_for_get_dbs_info/1,
                    fun should_return_500_time_out_when_time_is_not_enough_for_get_dbs_info/1,
                    fun should_return_db2_for_get_dbs_info_with_descending/1,
                    fun should_return_db1_for_get_dbs_info_with_limit_1/1,
                    fun should_return_db2_for_get_dbs_info_with_skip_1/1,
                    fun should_return_dbs_info_with_correct_start_end_key/1,
                    fun should_return_empty_list_with_wrong_start_end_key/1,
                    fun should_return_dbs_info_for_single_db/1,
                    fun should_return_dbs_info_for_multiple_dbs/1,
                    fun should_return_error_for_exceeded_keys/1,
                    fun should_return_error_for_missing_keys/1,
                    fun should_return_dbs_info_for_dbs_with_mixed_state/1
                ]
            }
        }
    }.


get_db_info_should_return_db_info(_) ->
    DbInfo = fabric:get_db_info("db1"),
    ?_assertEqual(DbInfo, chttpd_util:get_db_info("db1")).


get_db_info_should_return_error_when_db_not_exist(_) ->
    ?_assertEqual({error, database_does_not_exist},
        chttpd_util:get_db_info("db_not_exist")).


get_db_info_should_return_error_when_time_out(_) ->
    ?_test(
        begin
            mock_timeout(),
            ?assertEqual({error, timeout}, chttpd_util:get_db_info("db1"))
        end).


should_return_error_for_put_dbs_info(Url) ->
    ?_test(
        begin
            {ok, Code, _, ResultBody} = test_request:put(Url
            ++ "_dbs_info", [?CONTENT_JSON, ?AUTH], ""),
            {Body} = jiffy:decode(ResultBody),
            ?assertEqual(405, Code),
            ?assertEqual(<<"method_not_allowed">>,
                couch_util:get_value(<<"error">>, Body))
        end).


should_return_dbs_info_for_get_dbs_info(Url) ->
    ?_test(
        begin
            {ok, _, _, ResultBody} = test_request:get(Url
            ++ "_dbs_info", [?CONTENT_JSON, ?AUTH]),
            BodyJson = jiffy:decode(ResultBody),
            {Db1Data} = lists:nth(1, BodyJson),
            {Db2Data} = lists:nth(2, BodyJson),
            ?assertEqual(2, length(BodyJson)),
            ?assertEqual(<<"db1">>, couch_util:get_value(<<"key">>, Db1Data)),
            ?assertEqual(<<"db2">>, couch_util:get_value(<<"key">>, Db2Data))
        end).


should_return_nothing_when_db_not_exist_for_get_dbs_info(Url) ->
    ?_test(
        begin
            mock_db_not_exist(),
            {ok, Code, _, ResultBody} = test_request:get(Url
            ++ "_dbs_info", [?CONTENT_JSON, ?AUTH]),
            BodyJson = jiffy:decode(ResultBody),
            ?assertEqual(200, Code),
            ?assertEqual([], BodyJson)
        end).


should_return_500_time_out_when_time_is_not_enough_for_get_dbs_info(Url) ->
    ?_test(
        begin
            mock_timeout(),
            {ok, Code, _, ResultBody} = test_request:get(Url ++ "_dbs_info"
                ++ "?buffer_response=true", [?CONTENT_JSON, ?AUTH]),
            {Body} = jiffy:decode(ResultBody),
            ?assertEqual(500, Code),
            ?assertEqual(<<"timeout">>, couch_util:get_value(<<"error">>, Body))
        end).


should_return_db2_for_get_dbs_info_with_descending(Url) ->
    ?_test(
        begin
            {ok, _, _, ResultBody} = test_request:get(Url ++ "_dbs_info"
                ++ "?descending=true", [?CONTENT_JSON, ?AUTH]),
            BodyJson = jiffy:decode(ResultBody),
            {Db1Data} = lists:nth(1, BodyJson),
            {Db2Data} = lists:nth(2, BodyJson),
            ?assertEqual(2, length(BodyJson)),
            ?assertEqual(<<"db2">>, couch_util:get_value(<<"key">>, Db1Data)),
            ?assertEqual(<<"db1">>, couch_util:get_value(<<"key">>, Db2Data))
        end).


should_return_db1_for_get_dbs_info_with_limit_1(Url) ->
    ?_test(
        begin
            {ok, _, _, ResultBody} = test_request:get(Url ++ "_dbs_info"
                ++ "?limit=1", [?CONTENT_JSON, ?AUTH]),
            BodyJson = jiffy:decode(ResultBody),
            {DbData} = lists:nth(1, BodyJson),
            ?assertEqual(1, length(BodyJson)),
            ?assertEqual(<<"db1">>, couch_util:get_value(<<"key">>, DbData))
        end).


should_return_db2_for_get_dbs_info_with_skip_1(Url) ->
    ?_test(
        begin
            {ok, _, _, ResultBody} = test_request:get(Url ++ "_dbs_info"
                ++ "?skip=1", [?CONTENT_JSON, ?AUTH]),
            BodyJson = jiffy:decode(ResultBody),
            {DbData} = lists:nth(1, BodyJson),
            ?assertEqual(1, length(BodyJson)),
            ?assertEqual(<<"db2">>, couch_util:get_value(<<"key">>, DbData))
        end).


should_return_dbs_info_with_correct_start_end_key(Url) ->
    ?_test(
        begin
            {ok, _, _, ResultBody} = test_request:get(Url ++ "_dbs_info"
                ++ "?startkey=\"db1\"&endkey=\"db2\"", [?CONTENT_JSON, ?AUTH]),
            BodyJson = jiffy:decode(ResultBody),
            {DbData} = lists:nth(1, BodyJson),
            ?assertEqual(2, length(BodyJson)),
            ?assertEqual(<<"db1">>, couch_util:get_value(<<"key">>, DbData))
        end).


should_return_empty_list_with_wrong_start_end_key(Url) ->
    ?_test(
        begin
            {ok, _, _, ResultBody} = test_request:get(Url ++ "_dbs_info"
                ++ "?startkey=\"db3\"&endkey=\"db4\"", [?CONTENT_JSON, ?AUTH]),
            ?assertEqual([], jiffy:decode(ResultBody))
        end).

should_return_dbs_info_for_single_db(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\"]}",
        {ok, _, _, ResultBody} = test_request:post(
            Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),
        BodyJson = jiffy:decode(ResultBody),
        {Db1Data} = lists:nth(1, BodyJson),
        [
            ?assertEqual(
                <<"db1">>,
                couch_util:get_value(<<"key">>, Db1Data)
            ),
            ?assertNotEqual(
                undefined,
                couch_util:get_value(<<"info">>, Db1Data)
            )
        ]
    end).

should_return_dbs_info_for_multiple_dbs(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\", \"db2\"]}",
        {ok, _, _, ResultBody} = test_request:post(
            Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),
        BodyJson = jiffy:decode(ResultBody),
        {Db1Data} = lists:nth(1, BodyJson),
        {Db2Data} = lists:nth(2, BodyJson),
        [
            ?assertEqual(
                <<"db1">>,
                couch_util:get_value(<<"key">>, Db1Data)
            ),
            ?assertNotEqual(
                undefined,
                couch_util:get_value(<<"info">>, Db1Data)
            ),
            ?assertEqual(
                <<"db2">>,
                couch_util:get_value(<<"key">>, Db2Data)
            ),
            ?assertNotEqual(
                undefined,
                couch_util:get_value(<<"info">>, Db2Data)
            )
        ]
    end).

should_return_error_for_exceeded_keys(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\", \"db2\"]}",
        ok = config:set("chttpd", "max_db_number_for_dbs_info_req", "1"),
        {ok, Code, _, ResultBody} = test_request:post(
            Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),
        {Body} = jiffy:decode(ResultBody),
        ok = config:delete("chttpd", "max_db_number_for_dbs_info_req"),
        [
            ?assertEqual(
                <<"bad_request">>,
                couch_util:get_value(<<"error">>, Body)
            ),
            ?assertEqual(400, Code)
        ]
    end).

should_return_error_for_missing_keys(Url) ->
    ?_test(begin
        NewDoc = "{\"missingkeys\": [\"db1\", \"db2\"]}",
        {ok, Code, _, ResultBody} = test_request:post(
            Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),
        {Body} = jiffy:decode(ResultBody),
        [
            ?assertEqual(
                <<"bad_request">>,
                couch_util:get_value(<<"error">>, Body)
            ),
            ?assertEqual(400, Code)
        ]
    end).

should_return_dbs_info_for_dbs_with_mixed_state(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\", \"noexisteddb\"]}",
        {ok, _, _, ResultBody} = test_request:post(
            Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),
        Json = jiffy:decode(ResultBody),
        {Db1Data} = lists:nth(1, Json),
        {Db2Data} = lists:nth(2, Json),
        [
            ?assertEqual(
                <<"db1">>, couch_util:get_value(<<"key">>, Db1Data)
            ),
            ?assertNotEqual(
                undefined,
                couch_util:get_value(<<"info">>, Db1Data)
            ),
            ?assertEqual(
                <<"noexisteddb">>, couch_util:get_value(<<"key">>, Db2Data)
            ),
            ?assertEqual(undefined, couch_util:get_value(<<"info">>, Db2Data))
        ]
    end).
