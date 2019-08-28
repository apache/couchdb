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

-module(chttpd_db_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(DESTHEADER1, {"Destination", "foo%E5%95%8Abar"}).
-define(DESTHEADER2, {"Destination", "foo%2Fbar%23baz%3Fpow%3Afiz"}).
-define(FIXTURE_TXT, ?ABS_PATH(?FILE)).
-define(i2l(I), integer_to_list(I)).
-define(TIMEOUT, 60). % seconds

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
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

all_test_() ->
    {
        "chttpd db tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_return_ok_true_on_bulk_update/1,
                    fun should_return_ok_true_on_ensure_full_commit/1,
                    fun should_return_404_for_ensure_full_commit_on_no_db/1,
                    fun should_accept_live_as_an_alias_for_continuous/1,
                    fun should_return_404_for_delete_att_on_notadoc/1,
                    fun should_return_409_for_del_att_without_rev/1,
                    fun should_return_200_for_del_att_with_rev/1,
                    fun should_return_409_for_put_att_nonexistent_rev/1,
                    fun should_return_update_seq_when_set_on_all_docs/1,
                    fun should_not_return_update_seq_when_unset_on_all_docs/1,
                    fun should_return_correct_id_on_doc_copy/1,
                    fun should_return_400_for_bad_engine/1,
                    fun should_succeed_on_all_docs_with_queries_keys/1,
                    fun should_succeed_on_all_docs_with_queries_limit_skip/1,
                    fun should_succeed_on_all_docs_with_multiple_queries/1,
                    fun should_succeed_on_design_docs_with_queries_keys/1,
                    fun should_succeed_on_design_docs_with_queries_limit_skip/1,
                    fun should_succeed_on_design_docs_with_multiple_queries/1,
                    fun should_succeed_on_local_docs_with_queries_keys/1,
                    fun should_succeed_on_local_docs_with_queries_limit_skip/1,
                    fun should_succeed_on_local_docs_with_multiple_queries/1
                ]
            }
        }
    }.


should_return_ok_true_on_bulk_update(Url) ->
    {timeout, ?TIMEOUT, ?_assertEqual(true,
        begin
            {ok, _, _, Body} = create_doc(Url, "testdoc"),
            {Json} = ?JSON_DECODE(Body),
            Ref = couch_util:get_value(<<"rev">>, Json, undefined),
            NewDoc = "{\"docs\": [{\"_rev\": \"" ++ ?b2l(Ref) ++ "\", \"_id\": \"testdoc\"}]}",
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_bulk_docs/",
                [?CONTENT_JSON, ?AUTH], NewDoc),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = lists:nth(1, ResultJson),
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end)}.


should_return_ok_true_on_ensure_full_commit(Url0) ->
    {timeout, ?TIMEOUT, ?_test(begin
        Url = Url0 ++ "/_ensure_full_commit",
        {ok, RC, _, Body} = test_request:post(Url, [?CONTENT_JSON, ?AUTH], []),
        {Json} = ?JSON_DECODE(Body),
        ?assertEqual(201, RC),
        ?assert(couch_util:get_value(<<"ok">>, Json))
    end)}.


should_return_404_for_ensure_full_commit_on_no_db(Url0) ->
    {timeout, ?TIMEOUT, ?_test(begin
        Url = Url0 ++ "-missing-db" ++ "/_ensure_full_commit",
        {ok, RC, _, Body} = test_request:post(Url, [?CONTENT_JSON, ?AUTH], []),
        {Json} = ?JSON_DECODE(Body),
        ?assertEqual(404, RC),
        ?assertEqual(<<"not_found">>, couch_util:get_value(<<"error">>, Json))
    end)}.


should_accept_live_as_an_alias_for_continuous(Url) ->
    GetLastSeq = fun(Bin) ->
        Parts = binary:split(Bin, <<"\n">>, [global]),
        Filtered = [P || P <- Parts, size(P) > 0],
        LastSeqBin = lists:last(Filtered),
        {Result} = try ?JSON_DECODE(LastSeqBin) of
            Data -> Data
        catch
            _:_ ->
                ?assert(false) % should not happen, abort
        end,
        couch_util:get_value(<<"last_seq">>, Result, undefined)
    end,
    {timeout, ?TIMEOUT, ?_test(begin
        {ok, _, _, ResultBody1} =
            test_request:get(Url ++ "/_changes?feed=live&timeout=1", [?AUTH]),
        LastSeq1 = GetLastSeq(ResultBody1),

        {ok, _, _, _} = create_doc(Url, "testdoc2"),
        {ok, _, _, ResultBody2} =
            test_request:get(Url ++ "/_changes?feed=live&timeout=1", [?AUTH]),
        LastSeq2 = GetLastSeq(ResultBody2),

        ?assertNotEqual(LastSeq1, LastSeq2)
    end)}.


should_return_404_for_delete_att_on_notadoc(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        {ok, RC, _, RespBody} = test_request:delete(
            Url ++ "/notadoc/att.pdf",
            [?CONTENT_JSON, ?AUTH],
            []
        ),
        ?assertEqual(404, RC),
        ?assertEqual(
          {[{<<"error">>,<<"not_found">>},
            {<<"reason">>,<<"missing">>}]},
          jiffy:decode(RespBody)
        ),
        {ok, RC1, _, _} = test_request:get(
            Url ++ "/notadoc",
            [?CONTENT_JSON, ?AUTH],
            []
        ),
        ?assertEqual(404, RC1)
    end)}.


should_return_409_for_del_att_without_rev(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        {ok, RC, _, _} = test_request:put(
            Url ++ "/testdoc3",
            [?CONTENT_JSON, ?AUTH],
            jiffy:encode(attachment_doc())
        ),
        ?assertEqual(201, RC),

        {ok, RC1, _, _} = test_request:delete(
            Url ++ "/testdoc3/file.erl",
            [?CONTENT_JSON, ?AUTH],
            []
        ),
        ?assertEqual(409, RC1)
    end)}.


should_return_200_for_del_att_with_rev(Url) ->
  {timeout, ?TIMEOUT, ?_test(begin
      {ok, RC, _Headers, RespBody} = test_request:put(
          Url ++ "/testdoc4",
          [?CONTENT_JSON, ?AUTH],
          jiffy:encode(attachment_doc())
      ),
      ?assertEqual(201, RC),

      {ResultJson} = ?JSON_DECODE(RespBody),
      Rev = couch_util:get_value(<<"rev">>, ResultJson, undefined),

      {ok, RC1, _, _} = test_request:delete(
          Url ++ "/testdoc4/file.erl?rev=" ++ Rev,
          [?CONTENT_JSON, ?AUTH],
          []
      ),
      ?assertEqual(200, RC1)
    end)}.


should_return_409_for_put_att_nonexistent_rev(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        {ok, RC, _Headers, RespBody} = test_request:put(
            Url ++ "/should_return_404/file.erl?rev=1-000",
            [?CONTENT_JSON, ?AUTH],
            jiffy:encode(attachment_doc())
        ),
        ?assertEqual(409, RC),
        ?assertMatch({[
            {<<"error">>,<<"not_found">>},
            {<<"reason">>,<<"missing_rev">>}]},
            ?JSON_DECODE(RespBody))
    end)}.


should_return_update_seq_when_set_on_all_docs(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 3)],
        {ok, RC, _, RespBody} = test_request:get(Url ++ "/_all_docs/"
            ++ "?update_seq=true&keys=[\"testdoc1\"]",[?CONTENT_JSON, ?AUTH]),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ?assertNotEqual(undefined,
            couch_util:get_value(<<"update_seq">>, ResultJson)),
        ?assertNotEqual(undefined,
            couch_util:get_value(<<"offset">>, ResultJson))
    end)}.


should_not_return_update_seq_when_unset_on_all_docs(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 3)],
        {ok, RC, _, RespBody} = test_request:get(Url ++ "/_all_docs/"
            ++ "?update_seq=false&keys=[\"testdoc1\"]",[?CONTENT_JSON, ?AUTH]),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ?assertEqual(undefined,
            couch_util:get_value(<<"update_seq">>, ResultJson)),
        ?assertNotEqual(undefined,
            couch_util:get_value(<<"offset">>, ResultJson))
    end)}.


should_return_correct_id_on_doc_copy(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        {ok, _, _, _} = create_doc(Url, "testdoc"),
        {_, _, _, ResultBody1} = test_request:copy(Url ++ "/testdoc/",
            [?CONTENT_JSON, ?AUTH, ?DESTHEADER1]),
        {ResultJson1} = ?JSON_DECODE(ResultBody1),
        Id1 = couch_util:get_value(<<"id">>, ResultJson1),
        
        {_, _, _, ResultBody2} = test_request:copy(Url ++ "/testdoc/",
            [?CONTENT_JSON, ?AUTH, ?DESTHEADER2]),
        {ResultJson2} = ?JSON_DECODE(ResultBody2),
        Id2 = couch_util:get_value(<<"id">>, ResultJson2),
        [
            ?assertEqual(<<102,111,111,229,149,138,98,97,114>>, Id1),
            ?assertEqual(<<"foo/bar#baz?pow:fiz">>, Id2)
        ]
    end)}.


attachment_doc() ->
    {ok, Data} = file:read_file(?FIXTURE_TXT),
    {[
        {<<"_attachments">>, {[
            {<<"file.erl">>, {[
                {<<"content_type">>, <<"text/plain">>},
                {<<"data">>, base64:encode(Data)}
            ]}
        }]}}
    ]}.


should_return_400_for_bad_engine(_) ->
    {timeout, ?TIMEOUT, ?_test(begin
        TmpDb = ?tempdb(),
        Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
        Port = mochiweb_socket_server:get(chttpd, port),
        BaseUrl = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
        Url = BaseUrl ++ "?engine=cowabunga",
        {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
        ?assertEqual(400, Status)
    end)}.


should_succeed_on_all_docs_with_queries_keys(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"keys\": [ \"testdoc3\", \"testdoc8\"]}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++ "/_all_docs/queries/",
            [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson)))
    end)}.


should_succeed_on_all_docs_with_queries_limit_skip(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"limit\": 5, \"skip\": 2}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++ "/_all_docs/queries/",
            [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, couch_util:get_value(<<"offset">>, InnerJson)),
        ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson)))
    end)}.


should_succeed_on_all_docs_with_multiple_queries(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"keys\": [ \"testdoc3\", \"testdoc8\"]},
            {\"limit\": 5, \"skip\": 2}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++ "/_all_docs/queries/",
            [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson1} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson1))),
        {InnerJson2} = lists:nth(2, ResultJsonBody),
        ?assertEqual(2, couch_util:get_value(<<"offset">>, InnerJson2)),
        ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson2)))
    end)}.


should_succeed_on_design_docs_with_queries_keys(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "_design/ddoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"keys\": [ \"_design/ddoc3\",
            \"_design/ddoc8\"]}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++
            "/_design_docs/queries/", [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson)))
    end)}.


should_succeed_on_design_docs_with_queries_limit_skip(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "_design/ddoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"limit\": 5, \"skip\": 2}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++
            "/_design_docs/queries/", [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, couch_util:get_value(<<"offset">>, InnerJson)),
        ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson)))
    end)}.


should_succeed_on_design_docs_with_multiple_queries(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "_design/ddoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"keys\": [ \"_design/ddoc3\",
            \"_design/ddoc8\"]}, {\"limit\": 5, \"skip\": 2}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++
            "/_design_docs/queries/", [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson1} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson1))),
        {InnerJson2} = lists:nth(2, ResultJsonBody),
        ?assertEqual(2, couch_util:get_value(<<"offset">>, InnerJson2)),
        ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson2)))
    end)}.


should_succeed_on_local_docs_with_queries_keys(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "_local/doc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"keys\":
            [ \"_local/doc3\", \"_local/doc8\"]}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++ "/_local_docs/queries/",
            [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson)))
    end)}.


should_succeed_on_local_docs_with_queries_limit_skip(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "_local/doc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"limit\": 5, \"skip\": 2}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++
            "/_local_docs/queries/", [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson} = lists:nth(1, ResultJsonBody),
        ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson)))
    end)}.


should_succeed_on_local_docs_with_multiple_queries(Url) ->
    {timeout, ?TIMEOUT, ?_test(begin
        [create_doc(Url, "_local/doc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
        QueryDoc = "{\"queries\": [{\"keys\": [ \"_local/doc3\",
            \"_local/doc8\"]}, {\"limit\": 5, \"skip\": 2}]}",
        {ok, RC, _, RespBody} = test_request:post(Url ++
            "/_local_docs/queries/", [?CONTENT_JSON, ?AUTH], QueryDoc),
        ?assertEqual(200, RC),
        {ResultJson} = ?JSON_DECODE(RespBody),
        ResultJsonBody = couch_util:get_value(<<"results">>, ResultJson),
        {InnerJson1} = lists:nth(1, ResultJsonBody),
        ?assertEqual(2, length(couch_util:get_value(<<"rows">>, InnerJson1))),
        {InnerJson2} = lists:nth(2, ResultJsonBody),
        ?assertEqual(5, length(couch_util:get_value(<<"rows">>, InnerJson2)))
    end)}.
