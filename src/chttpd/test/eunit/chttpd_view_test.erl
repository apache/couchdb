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

-module(chttpd_view_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_view_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(DDOC,
    "{\"_id\": \"_design/bar\", \"views\": {\"baz\":\n"
    "               {\"map\": \"function(doc) {emit(doc._id, doc._id);}\"}}}"
).

-define(FIXTURE_TXT, ?ABS_PATH(?FILE)).
-define(i2l(I), integer_to_list(I)).
% seconds
-define(TIMEOUT, 60).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist = false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

create_doc(Url, Id) ->
    test_request:put(
        Url ++ "/" ++ Id,
        [?CONTENT_JSON, ?AUTH],
        "{\"mr\": \"rockoartischocko\"}"
    ).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

all_view_test_() ->
    {
        "chttpd view tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_succeed_on_view_with_queries_keys/1,
                    fun should_succeed_on_view_with_queries_limit_skip/1,
                    fun should_succeed_on_view_with_multiple_queries/1
                ]
            }
        }
    }.

should_succeed_on_view_with_queries_keys(Url) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
            {ok, _, _, _} = test_request:put(
                Url ++ "/_design/bar",
                [?CONTENT_JSON, ?AUTH],
                ?DDOC
            ),
            QueryDoc =
                "{\"queries\": [{\"keys\": [ \"testdoc3\",\n"
                "            \"testdoc8\"]}]}",
            {ok, _, _, RespBody} = test_request:post(
                Url ++ "/_design/bar/" ++
                    "_view/baz/queries/",
                [?CONTENT_JSON, ?AUTH],
                QueryDoc
            ),
            {ResultJson} = ?JSON_DECODE(RespBody),
            ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
            {InnerJson} = lists:nth(1, ResultJsonBody),
            ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson)))
        end)}.

should_succeed_on_view_with_queries_limit_skip(Url) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
            {ok, _, _, _} = test_request:put(
                Url ++ "/_design/bar",
                [?CONTENT_JSON, ?AUTH],
                ?DDOC
            ),
            QueryDoc = "{\"queries\": [{\"limit\": 5, \"skip\": 2}]}",
            {ok, RC, _, RespBody} = test_request:post(
                Url ++ "/_design/bar/" ++
                    "_view/baz/queries/",
                [?CONTENT_JSON, ?AUTH],
                QueryDoc
            ),
            ?assertEqual(200, RC),
            {ResultJson} = ?JSON_DECODE(RespBody),
            ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
            {InnerJson} = lists:nth(1, ResultJsonBody),
            ?assertEqual(2, couch_util:get_value(<<"offset">>, InnerJson)),
            ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson)))
        end)}.

should_succeed_on_view_with_multiple_queries(Url) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
            {ok, _, _, _} = test_request:put(
                Url ++ "/_design/bar",
                [?CONTENT_JSON, ?AUTH],
                ?DDOC
            ),
            QueryDoc =
                "{\"queries\": [{\"keys\": [ \"testdoc3\",\n"
                "            \"testdoc8\"]}, {\"limit\": 5, \"skip\": 2}]}",
            {ok, RC, _, RespBody} = test_request:post(
                Url ++ "/_design/bar/" ++
                    "_view/baz/queries/",
                [?CONTENT_JSON, ?AUTH],
                QueryDoc
            ),
            ?assertEqual(200, RC),
            {ResultJson} = ?JSON_DECODE(RespBody),
            ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
            {InnerJson1} = lists:nth(1, ResultJsonBody),
            ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson1))),
            {InnerJson2} = lists:nth(2, ResultJsonBody),
            ?assertEqual(2, couch_util:get_value(<<"offset">>, InnerJson2)),
            ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson2)))
        end)}.
