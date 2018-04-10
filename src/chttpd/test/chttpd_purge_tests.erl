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

-module(chttpd_purge_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).


setup() ->
    ok = config:set("admins", ?USER, ?PASS, _Persist=false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.


teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist=false).


create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


create_doc(Url, Id) ->
    test_request:put(Url ++ "/" ++ Id,
        [?CONTENT_JSON, ?AUTH], "{\"mr\": \"rockoartischocko\"}").


delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).


purge_test_() ->
    {
        "chttpd db tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun test_empty_purge_request/1,
                    fun test_ok_purge_request/1,
                    fun should_error_set_purged_docs_limit_to0/1
                ]
            }
        }
    }.


test_empty_purge_request(Url) ->
    ?_test(begin
        IdsRevs = "{}",
        {ok, Status, _, ResultBody} = test_request:post(Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH], IdsRevs),
        ResultJson = ?JSON_DECODE(ResultBody),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ?assertEqual({[{<<"purged">>,{[]}}]}, ResultJson)
    end).


test_ok_purge_request(Url) ->
    ?_test(begin
        {ok, _, _, Body} = create_doc(Url, "doc1"),
        {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json, undefined),
        {ok, _, _, Body2} = create_doc(Url, "doc2"),
        {Json2} = ?JSON_DECODE(Body2),
        Rev2 = couch_util:get_value(<<"rev">>, Json2, undefined),
        {ok, _, _, Body3} = create_doc(Url, "doc3"),
        {Json3} = ?JSON_DECODE(Body3),
        Rev3 = couch_util:get_value(<<"rev">>, Json3, undefined),
        IdsRevs = "{\"doc1\": [\"" ++ ?b2l(Rev1) ++ "\"], \"doc2\": [\"" ++
            ?b2l(Rev2) ++ "\"], \"doc3\": [\"" ++ ?b2l(Rev3) ++ "\"] }",

        {ok, Status, _, ResultBody} = test_request:post(Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH], IdsRevs),
        ResultJson = ?JSON_DECODE(ResultBody),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ?assertEqual(
            {[{<<"purged">>, {[
                {<<"doc1">>, {[
                    {<<"purged">>,[Rev1]},
                    {<<"ok">>,true}
                ]}},
                {<<"doc2">>, {[
                    {<<"purged">>,[Rev2]},
                    {<<"ok">>,true}
                ]}},
                {<<"doc3">>, {[
                    {<<"purged">>,[Rev3]},
                    {<<"ok">>,true}
                ]}}
            ]}}]},
            ResultJson
        )
    end).


should_error_set_purged_docs_limit_to0(Url) ->
    ?_test(begin
        {ok, Status, _, _} = test_request:put(Url ++ "/_purged_docs_limit/",
            [?CONTENT_JSON, ?AUTH], "0"),
        ?assert(Status =:= 400)
    end).