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
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    Db1Url = lists:concat([Url, "db1"]),
    create_db(Db1Url),
    Db2Url = lists:concat([Url, "db2"]),
    create_db(Db2Url),
    Url.

teardown(Url) ->
    Db1Url = lists:concat([Url, "db1"]),
    Db2Url = lists:concat([Url, "db2"]),
    delete_db(Db1Url),
    delete_db(Db2Url),
    ok = config:delete("admins", ?USER, _Persist=false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

dbs_info_test_() ->
    {
        "chttpd dbs info tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_return_for_get_db_info/1,
                    fun should_return_dbs_info_for_single_db/1,
                    fun should_return_dbs_info_for_multiple_dbs/1,
                    fun should_return_error_for_exceeded_keys/1,
                    fun should_return_error_for_missing_keys/1,
                    fun should_return_dbs_info_for_dbs_with_mixed_state/1
                ]
            }
        }
    }.


should_return_for_get_db_info(Url) ->
    ?_test(begin
        {ok, Code, _, ResultBody} = test_request:get(Url ++ "/_dbs_info?"
            ++ "start_key=\"db1\"&end_key=\"db1\"", [?CONTENT_JSON, ?AUTH]),
        Body = jiffy:decode(ResultBody, [return_maps]),
        [
            ?assertEqual(200, Code),
            ?assertMatch([#{<<"db_name">> := <<"db1">>}], Body)
        ]
    end).


should_return_dbs_info_for_single_db(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\"]}",
        {ok, _, _, ResultBody} = test_request:post(Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH], NewDoc),
        BodyJson = jiffy:decode(ResultBody),
        {Db1Data} = lists:nth(1, BodyJson),
        [
            ?assertEqual(<<"db1">>,
                couch_util:get_value(<<"key">>, Db1Data)),
            ?assertNotEqual(undefined,
                couch_util:get_value(<<"info">>, Db1Data))
        ]
    end).


should_return_dbs_info_for_multiple_dbs(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\", \"db2\"]}",
        {ok, _, _, ResultBody} = test_request:post(Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH], NewDoc),
        BodyJson = jiffy:decode(ResultBody),
        {Db1Data} = lists:nth(1, BodyJson),
        {Db2Data} = lists:nth(2, BodyJson),
        [
            ?assertEqual(<<"db1">>,
                couch_util:get_value(<<"key">>, Db1Data)),
            ?assertNotEqual(undefined,
                couch_util:get_value(<<"info">>, Db1Data)),
            ?assertEqual(<<"db2">>,
                couch_util:get_value(<<"key">>, Db2Data)),
            ?assertNotEqual(undefined,
                couch_util:get_value(<<"info">>, Db2Data))
        ]
    end).


should_return_error_for_exceeded_keys(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\", \"db2\"]}",
        ok = config:set("chttpd", "max_db_number_for_dbs_info_req", "1"),
        {ok, Code, _, ResultBody} = test_request:post(Url ++ "/_dbs_info/",
                   [?CONTENT_JSON, ?AUTH], NewDoc),
        {Body} = jiffy:decode(ResultBody),
        ok = config:delete("chttpd", "max_db_number_for_dbs_info_req"),
        [
            ?assertEqual(<<"bad_request">>,
                couch_util:get_value(<<"error">>, Body)),
            ?assertEqual(400, Code)
        ]
    end).


should_return_error_for_missing_keys(Url) ->
    ?_test(begin
        NewDoc = "{\"missingkeys\": [\"db1\", \"db2\"]}",
        {ok, Code, _, ResultBody} = test_request:post(Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH], NewDoc),
        {Body} = jiffy:decode(ResultBody),
        [
            ?assertEqual(<<"bad_request">>,
                couch_util:get_value(<<"error">>, Body)),
            ?assertEqual(400, Code)
        ]
    end).


should_return_dbs_info_for_dbs_with_mixed_state(Url) ->
    ?_test(begin
        NewDoc = "{\"keys\": [\"db1\", \"noexisteddb\"]}",
        {ok, _, _, ResultBody} = test_request:post(Url ++ "/_dbs_info/",
            [?CONTENT_JSON, ?AUTH], NewDoc),
        Json = jiffy:decode(ResultBody),
        {Db1Data} = lists:nth(1, Json),
        {Db2Data} = lists:nth(2, Json),
        [
            ?assertEqual(
                <<"db1">>, couch_util:get_value(<<"key">>, Db1Data)),
            ?assertNotEqual(undefined,
                couch_util:get_value(<<"info">>, Db1Data)),
            ?assertEqual(
                <<"noexisteddb">>, couch_util:get_value(<<"key">>, Db2Data)),
            ?assertEqual(undefined, couch_util:get_value(<<"info">>, Db2Data))
        ]
    end).
