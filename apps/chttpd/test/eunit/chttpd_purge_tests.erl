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

create_doc(Url, Id, Content) ->
    test_request:put(
        Url ++ "/" ++ Id,
        [?CONTENT_JSON, ?AUTH],
        "{\"mr\": \"" ++ Content ++ "\"}"
    ).

create_docs(Url, Docs) ->
    test_request:post(
        Url ++ "/_bulk_docs",
        [?CONTENT_JSON, ?AUTH],
        ?JSON_ENCODE({[{docs, Docs}]})
    ).

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
                    fun test_ok_purge_request_with_101_docid/1,
                    fun test_accepted_purge_request/1,
                    fun test_partial_purge_request/1,
                    fun test_mixed_purge_request/1,
                    fun test_overmany_ids_or_revs_purge_request/1,
                    fun test_exceed_limits_on_purge_infos/1,
                    fun should_error_set_purged_docs_limit_to0/1,
                    fun test_timeout_set_purged_infos_limit/1
                ]
            }
        }
    }.

test_empty_purge_request(Url) ->
    ?_test(begin
        IdsRevs = "{}",
        {ok, Status, _, ResultBody} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),
        ResultJson = ?JSON_DECODE(ResultBody),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ?assertEqual(
            {[
                {<<"purge_seq">>, null},
                {<<"purged">>, {[]}}
            ]},
            ResultJson
        )
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

        IdsRevsEJson =
            {[
                {<<"doc1">>, [Rev1]},
                {<<"doc2">>, [Rev2]},
                {<<"doc3">>, [Rev3]}
            ]},
        IdsRevs = binary_to_list(?JSON_ENCODE(IdsRevsEJson)),

        {ok, Status, _, ResultBody} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),
        ResultJson = ?JSON_DECODE(ResultBody),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ?assertEqual(
            {[
                {<<"purge_seq">>, null},
                {<<"purged">>,
                    {[
                        {<<"doc1">>, [Rev1]},
                        {<<"doc2">>, [Rev2]},
                        {<<"doc3">>, [Rev3]}
                    ]}}
            ]},
            ResultJson
        )
    end).

test_ok_purge_request_with_101_docid(Url) ->
    ?_test(begin
        PurgedDocsNum = 101,
        Docs = lists:foldl(
            fun(I, Acc) ->
                Id = list_to_binary(integer_to_list(I)),
                Doc = {[{<<"_id">>, Id}, {value, I}]},
                [Doc | Acc]
            end,
            [],
            lists:seq(1, PurgedDocsNum)
        ),

        {ok, _, _, Body} = create_docs(Url, Docs),
        BodyJson = ?JSON_DECODE(Body),

        PurgeBody = lists:map(
            fun({DocResp}) ->
                Id = couch_util:get_value(<<"id">>, DocResp, undefined),
                Rev = couch_util:get_value(<<"rev">>, DocResp, undefined),
                {Id, [Rev]}
            end,
            BodyJson
        ),

        ok = config:set("purge", "max_document_id_number", "101"),
        try
            {ok, Status, _, _} = test_request:post(
                Url ++ "/_purge/",
                [?CONTENT_JSON, ?AUTH],
                ?JSON_ENCODE({PurgeBody})
            ),
            ?assert(Status =:= 201 orelse Status =:= 202)
        after
            ok = config:delete("purge", "max_document_id_number")
        end
    end).

test_accepted_purge_request(Url) ->
    ?_test(begin
        {ok, _, _, Body} = create_doc(Url, "doc1"),
        {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json, undefined),
        IdsRevsEJson =
            {[
                {<<"doc1">>, [Rev1]}
            ]},
        IdsRevs = binary_to_list(?JSON_ENCODE(IdsRevsEJson)),
        meck:new(fabric, [passthrough]),
        meck:expect(
            fabric,
            purge_docs,
            fun(_, _, _) ->
                {accepted, [
                    {accepted, [
                        {1,
                            <<57, 27, 64, 134, 152, 18, 73, 243, 40, 1, 141, 214, 135, 104, 79,
                                188>>}
                    ]}
                ]}
            end
        ),
        {ok, Status, _, ResultBody} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),
        ResultJson = ?JSON_DECODE(ResultBody),
        meck:unload(fabric),
        ?assert(Status =:= 202),
        ?assertEqual(
            {[
                {<<"purge_seq">>, null},
                {<<"purged">>,
                    {[
                        {<<"doc1">>, [Rev1]}
                    ]}}
            ]},
            ResultJson
        )
    end).

test_partial_purge_request(Url) ->
    ?_test(begin
        {ok, _, _, Body} = create_doc(Url, "doc1"),
        {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json, undefined),

        NewDoc =
            "{\"new_edits\": false, \"docs\": [{\"_id\": \"doc1\",\n"
            "            \"_revisions\": {\"start\": 1, \"ids\": [\"12345\", \"67890\"]},\n"
            "            \"content\": \"updated\", \"_rev\": \"" ++ ?b2l(Rev1) ++ "\"}]}",
        {ok, _, _, _} = test_request:post(
            Url ++ "/_bulk_docs/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),

        IdsRevsEJson = {[{<<"doc1">>, [Rev1]}]},
        IdsRevs = binary_to_list(?JSON_ENCODE(IdsRevsEJson)),
        {ok, Status, _, ResultBody} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),
        ResultJson = ?JSON_DECODE(ResultBody),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ?assertEqual(
            {[
                {<<"purge_seq">>, null},
                {<<"purged">>,
                    {[
                        {<<"doc1">>, [Rev1]}
                    ]}}
            ]},
            ResultJson
        ),
        {ok, Status2, _, ResultBody2} = test_request:get(
            Url ++
                "/doc1/",
            [?AUTH]
        ),
        {Json2} = ?JSON_DECODE(ResultBody2),
        Content = couch_util:get_value(<<"content">>, Json2, undefined),
        ?assertEqual(<<"updated">>, Content),
        ?assert(Status2 =:= 200)
    end).

test_mixed_purge_request(Url) ->
    ?_test(begin
        {ok, _, _, Body} = create_doc(Url, "doc1"),
        {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json, undefined),

        NewDoc =
            "{\"new_edits\": false, \"docs\": [{\"_id\": \"doc1\",\n"
            "            \"_revisions\": {\"start\": 1, \"ids\": [\"12345\", \"67890\"]},\n"
            "            \"content\": \"updated\", \"_rev\": \"" ++ ?b2l(Rev1) ++ "\"}]}",
        {ok, _, _, _} = test_request:post(
            Url ++ "/_bulk_docs/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),

        {ok, _, _, _Body2} = create_doc(Url, "doc2", "content2"),
        {ok, _, _, Body3} = create_doc(Url, "doc3", "content3"),
        {Json3} = ?JSON_DECODE(Body3),
        Rev3 = couch_util:get_value(<<"rev">>, Json3, undefined),

        IdsRevsEJson =
            {[
                % partial purge
                {<<"doc1">>, [Rev1]},
                % correct format, but invalid rev
                {<<"doc2">>, [Rev3, Rev1]},
                % correct format and rev
                {<<"doc3">>, [Rev3]}
            ]},
        IdsRevs = binary_to_list(?JSON_ENCODE(IdsRevsEJson)),
        {ok, Status, _, Body4} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),
        ResultJson = ?JSON_DECODE(Body4),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ?assertEqual(
            {[
                {<<"purge_seq">>, null},
                {<<"purged">>,
                    {[
                        {<<"doc1">>, [Rev1]},
                        {<<"doc2">>, []},
                        {<<"doc3">>, [Rev3]}
                    ]}}
            ]},
            ResultJson
        ),
        {ok, Status2, _, Body5} = test_request:get(
            Url ++
                "/doc1/",
            [?AUTH]
        ),
        {Json5} = ?JSON_DECODE(Body5),
        Content = couch_util:get_value(<<"content">>, Json5, undefined),
        ?assertEqual(<<"updated">>, Content),
        ?assert(Status2 =:= 200)
    end).

test_overmany_ids_or_revs_purge_request(Url) ->
    ?_test(begin
        {ok, _, _, Body} = create_doc(Url, "doc1"),
        {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json, undefined),

        NewDoc =
            "{\"new_edits\": false, \"docs\": [{\"_id\": \"doc1\",\n"
            "            \"_revisions\": {\"start\": 1, \"ids\": [\"12345\", \"67890\"]},\n"
            "            \"content\": \"updated\", \"_rev\": \"" ++ ?b2l(Rev1) ++ "\"}]}",
        {ok, _, _, _} = test_request:post(
            Url ++ "/_bulk_docs/",
            [?CONTENT_JSON, ?AUTH],
            NewDoc
        ),

        {ok, _, _, _Body2} = create_doc(Url, "doc2", "content2"),
        {ok, _, _, Body3} = create_doc(Url, "doc3", "content3"),
        {Json3} = ?JSON_DECODE(Body3),
        Rev3 = couch_util:get_value(<<"rev">>, Json3, undefined),

        IdsRevsEJson =
            {[
                % partial purge
                {<<"doc1">>, [Rev1]},
                % correct format, but invalid rev
                {<<"doc2">>, [Rev3, Rev1]},
                % correct format and rev
                {<<"doc3">>, [Rev3]}
            ]},
        IdsRevs = binary_to_list(?JSON_ENCODE(IdsRevsEJson)),

        % Ids larger than expected
        config:set("purge", "max_document_id_number", "1"),
        {ok, Status, _, Body4} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),
        config:delete("purge", "max_document_id_number"),
        ResultJson = ?JSON_DECODE(Body4),
        ?assertEqual(400, Status),
        ?assertMatch(
            {[
                {<<"error">>, <<"bad_request">>},
                {<<"reason">>, <<"Exceeded maximum number of documents.">>}
            ]},
            ResultJson
        ),

        % Revs larger than expected
        config:set("purge", "max_revisions_number", "1"),
        {ok, Status2, _, Body5} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),
        config:delete("purge", "max_revisions_number"),
        ResultJson2 = ?JSON_DECODE(Body5),
        ?assertEqual(400, Status2),
        ?assertMatch(
            {[
                {<<"error">>, <<"bad_request">>},
                {<<"reason">>, <<"Exceeded maximum number of revisions.">>}
            ]},
            ResultJson2
        )
    end).

test_exceed_limits_on_purge_infos(Url) ->
    ?_test(begin
        {ok, Status1, _, _} = test_request:put(
            Url ++ "/_purged_infos_limit/",
            [?CONTENT_JSON, ?AUTH],
            "2"
        ),
        ?assert(Status1 =:= 200),

        {ok, _, _, Body} = create_doc(Url, "doc1"),
        {Json} = ?JSON_DECODE(Body),
        Rev1 = couch_util:get_value(<<"rev">>, Json, undefined),
        {ok, _, _, Body2} = create_doc(Url, "doc2"),
        {Json2} = ?JSON_DECODE(Body2),
        Rev2 = couch_util:get_value(<<"rev">>, Json2, undefined),
        {ok, _, _, Body3} = create_doc(Url, "doc3"),
        {Json3} = ?JSON_DECODE(Body3),
        Rev3 = couch_util:get_value(<<"rev">>, Json3, undefined),

        IdsRevsEJson =
            {[
                {<<"doc1">>, [Rev1]},
                {<<"doc2">>, [Rev2]},
                {<<"doc3">>, [Rev3]}
            ]},
        IdsRevs = binary_to_list(?JSON_ENCODE(IdsRevsEJson)),

        {ok, Status2, _, ResultBody} = test_request:post(
            Url ++ "/_purge/",
            [?CONTENT_JSON, ?AUTH],
            IdsRevs
        ),

        ResultJson = ?JSON_DECODE(ResultBody),
        ?assert(Status2 =:= 201 orelse Status2 =:= 202),
        ?assertEqual(
            {[
                {<<"purge_seq">>, null},
                {<<"purged">>,
                    {[
                        {<<"doc1">>, [Rev1]},
                        {<<"doc2">>, [Rev2]},
                        {<<"doc3">>, [Rev3]}
                    ]}}
            ]},
            ResultJson
        )
    end).

should_error_set_purged_docs_limit_to0(Url) ->
    ?_test(begin
        {ok, Status, _, _} = test_request:put(
            Url ++ "/_purged_infos_limit/",
            [?CONTENT_JSON, ?AUTH],
            "0"
        ),
        ?assert(Status =:= 400)
    end).

test_timeout_set_purged_infos_limit(Url) ->
    ?_test(begin
        meck:new(fabric, [passthrough]),
        meck:expect(fabric, set_purge_infos_limit, fun(_, _, _) ->
            {error, timeout}
        end),
        {ok, Status, _, ResultBody} = test_request:put(
            Url ++
                "/_purged_infos_limit/",
            [?CONTENT_JSON, ?AUTH],
            "2"
        ),
        meck:unload(fabric),
        ResultJson = ?JSON_DECODE(ResultBody),
        ?assert(Status =:= 500),
        ?assertMatch(
            {[
                {<<"error">>, <<"error">>},
                {<<"reason">>, <<"timeout">>}
            ]},
            ResultJson
        )
    end).
