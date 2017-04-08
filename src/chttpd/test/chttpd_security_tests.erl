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
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(FIXTURE_TXT, ?ABS_PATH(?FILE)).

setup() ->
    ok = config:set("admins", ?USER, ?PASS, _Persist=false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    create_design_doc(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist=false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

create_design_doc(Url) ->
    {ok, Status, _, _} = test_request:put(lists:concat([Url, '/_design/test']), [?CONTENT_JSON, ?AUTH], 
            "{\"id\":\"_design/test\"}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

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
                    fun should_allow_admin_view_compaction/1,
                    fun should_disallow_anonymous_view_compaction/1,
                    fun should_allow_admin_db_view_cleanup/1,
                    fun should_disallow_anonymous_db_view_cleanup/1
                ]
            }
        }
    }.

should_allow_admin_db_compaction(Url) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact",
                [?CONTENT_JSON, ?AUTH], ""),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = ResultJson,
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_disallow_anonymous_db_compaction(Url) ->
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact",
        [?CONTENT_JSON], ""),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson} = ResultJson,
    ErrType = couch_util:get_value(<<"error">>, InnerJson),
    ?_assertEqual(<<"unauthorized">>,ErrType).

should_allow_admin_view_compaction(Url) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact/test",
                [?CONTENT_JSON, ?AUTH], ""),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = ResultJson,
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_disallow_anonymous_view_compaction(Url) ->
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_compact/test",
        [?CONTENT_JSON], ""),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson} = ResultJson,
    ErrType = couch_util:get_value(<<"error">>, InnerJson),
    ?_assertEqual(<<"unauthorized">>,ErrType).

should_allow_admin_db_view_cleanup(Url) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_view_cleanup",
                [?CONTENT_JSON, ?AUTH], ""),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = ResultJson,
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_disallow_anonymous_db_view_cleanup(Url) ->
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_view_cleanup",
        [?CONTENT_JSON], ""),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson} = ResultJson,
    ErrType = couch_util:get_value(<<"error">>, InnerJson),
    ?_assertEqual(<<"unauthorized">>, ErrType).
