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
% seconds
-define(TIMEOUT, 60).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Db = ?tempdb(),
    create_db(Db),
    Db.

teardown(Db) ->
    delete_db(Db),
    ok = config:delete("admins", ?USER, _Persist = false).

all_test_() ->
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
                    ?TDEF_FE(t_return_ok_true_on_bulk_update, ?TIMEOUT),
                    ?TDEF_FE(t_return_201_new_edits_false_with_revs_on_bulk_update, ?TIMEOUT),
                    ?TDEF_FE(t_return_400_new_edits_false_no_revs_on_bulk_update, ?TIMEOUT),
                    ?TDEF_FE(t_return_201_new_edits_false_with_rev_on_doc_update, ?TIMEOUT),
                    ?TDEF_FE(t_return_201_new_edits_false_with_revisions_on_doc_update, ?TIMEOUT),
                    ?TDEF_FE(t_return_400_new_edits_false_no_revs_on_doc_update, ?TIMEOUT),
                    ?TDEF_FE(t_return_ok_true_on_ensure_full_commit, ?TIMEOUT),
                    ?TDEF_FE(t_return_404_for_ensure_full_commit_on_no_db, ?TIMEOUT),
                    ?TDEF_FE(t_accept_live_as_an_alias_for_continuous, ?TIMEOUT),
                    ?TDEF_FE(t_return_headers_after_starting_continuous, ?TIMEOUT),
                    ?TDEF_FE(t_return_404_for_delete_att_on_notadoc, ?TIMEOUT),
                    ?TDEF_FE(t_return_409_for_del_att_without_rev, ?TIMEOUT),
                    ?TDEF_FE(t_return_200_for_del_att_with_rev, ?TIMEOUT),
                    ?TDEF_FE(t_return_409_for_put_att_nonexistent_rev, ?TIMEOUT),
                    ?TDEF_FE(t_return_update_seq_when_set_on_all_docs, ?TIMEOUT),
                    ?TDEF_FE(t_not_return_update_seq_when_unset_on_all_docs, ?TIMEOUT),
                    ?TDEF_FE(t_return_correct_id_on_doc_copy, ?TIMEOUT),
                    ?TDEF_FE(t_return_only_one_ok_on_doc_copy, ?TIMEOUT),
                    ?TDEF_FE(t_return_400_for_bad_engine, ?TIMEOUT),
                    ?TDEF_FE(t_not_change_db_proper_after_rewriting_shardmap, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_all_docs_with_queries_keys, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_all_docs_with_queries_limit_skip, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_all_docs_with_multiple_queries, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_design_docs_with_queries_keys, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_design_docs_with_queries_limit_skip, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_design_docs_with_multiple_queries, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_local_docs_with_queries_keys, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_local_docs_with_queries_limit_skip, ?TIMEOUT),
                    ?TDEF_FE(t_succeed_on_local_docs_with_multiple_queries, ?TIMEOUT)
                ]
            }
        }
    }.

t_return_ok_true_on_bulk_update(Db) ->
    {_, #{<<"rev">> := Rev}} = create_doc(Db, "testdoc"),
    Body = #{<<"docs">> => [#{<<"_id">> => <<"testdoc">>, <<"_rev">> => Rev}]},
    {_, [#{<<"ok">> := Res}]} = req(post, url(Db, "_bulk_docs"), Body),
    ?assert(Res).

t_return_201_new_edits_false_with_revs_on_bulk_update(Db) ->
    {_, #{<<"rev">> := Rev}} = create_doc(Db, "dochasrev"),
    Body = #{
        <<"docs">> => [#{<<"_id">> => <<"dochasrev">>, <<"_rev">> => Rev}],
        <<"new_edits">> => false
    },
    {Status, Response} = req(post, url(Db, "_bulk_docs"), Body),
    ?assertEqual(201, Status),
    ?assertEqual([], Response).

t_return_400_new_edits_false_no_revs_on_bulk_update(Db) ->
    create_doc(Db, "docnorev"),
    NewDoc = #{<<"docs">> => [#{<<"_id">> => <<"docnorev">>}], <<"new_edits">> => false},
    {Status, Response} = req(post, url(Db, "_bulk_docs"), NewDoc),
    ?assertEqual(400, Status),
    ?assertMatch(#{<<"error">> := <<"bad_request">>}, Response).

t_return_201_new_edits_false_with_rev_on_doc_update(Db) ->
    Body = #{<<"foo">> => <<"bar">>, <<"_rev">> => <<"1-abc">>},
    {Status, #{<<"id">> := Id, <<"rev">> := Rev}} =
        req(put, url(Db, "docrev2?new_edits=false"), Body),
    ?assertEqual(201, Status),
    ?assertEqual(<<"docrev2">>, Id),
    ?assertEqual(<<"1-abc">>, Rev).

t_return_201_new_edits_false_with_revisions_on_doc_update(Db) ->
    Body = #{
        <<"foo">> => <<"bar">>,
        <<"_revisions">> => #{<<"ids">> => [<<"abc">>, <<"def">>], <<"start">> => 2}
    },
    {Status, #{<<"id">> := Id, <<"rev">> := Rev}} =
        req(put, url(Db, "docrev3?new_edits=false"), Body),
    ?assertEqual(201, Status),
    ?assertEqual(<<"docrev3">>, Id),
    ?assertEqual(<<"2-abc">>, Rev).

t_return_400_new_edits_false_no_revs_on_doc_update(Db) ->
    Body = #{<<"foo">> => <<"bar">>},
    {Status, Response} = req(put, url(Db, "docrev3?new_edits=false"), Body),
    ?assertEqual(400, Status),
    ?assertMatch(#{<<"error">> := <<"bad_request">>}, Response).

t_return_ok_true_on_ensure_full_commit(Db) ->
    {Status, #{<<"ok">> := Res}} = req(post, url(Db, "_ensure_full_commit")),
    ?assertEqual(201, Status),
    ?assert(Res).

t_return_404_for_ensure_full_commit_on_no_db(Db) ->
    {Status, Response} = req(post, url(Db) ++ "-missing-db" ++ "/_ensure_full_commit"),
    ?assertEqual(404, Status),
    ?assertMatch(#{<<"error">> := <<"not_found">>}, Response).

t_accept_live_as_an_alias_for_continuous(Db) ->
    GetLastSeq =
        fun(Chunks) ->
            LastSeqBin = lists:last(Chunks),
            {Result} =
                try ?JSON_DECODE(LastSeqBin) of
                    Data -> Data
                catch
                    _:_ ->
                        % should not happen, abort
                        ?assert(false)
                end,
            couch_util:get_value(<<"last_seq">>, Result, undefined)
        end,
    LastSeq1 = GetLastSeq(wait_non_empty_chunk(Db)),
    create_doc(Db, "testdoc2"),
    LastSeq2 = GetLastSeq(wait_non_empty_chunk(Db)),
    ?assertNotEqual(LastSeq1, LastSeq2).

t_return_headers_after_starting_continuous(Db) ->
    {ok, _, _, Bin} = test_request:get(url(Db, "_changes?feed=live&timeout=1"), [?AUTH]),
    Parts = binary:split(Bin, <<"\n">>, [global]),
    %% we should receive at least one part even when timeout=1
    ?assertNotEqual([], Parts).

t_return_404_for_delete_att_on_notadoc(Db) ->
    {Status, Response} = req(delete, url(Db, "notadoc/att.pdf")),
    ?assertEqual(404, Status),
    ?assertMatch(#{<<"error">> := <<"not_found">>, <<"reason">> := <<"missing">>}, Response),
    {Status1, Response} = req(get, url(Db, "notadoc")),
    ?assertEqual(404, Status1).

t_return_409_for_del_att_without_rev(Db) ->
    {Status, _} = req(put, url(Db, "testdoc3"), attachment_doc()),
    ?assertEqual(201, Status),
    {Status1, _} = req(delete, url(Db, "testdoc3/file.erl")),
    ?assertEqual(409, Status1).

t_return_200_for_del_att_with_rev(Db) ->
    {Status, #{<<"rev">> := Rev}} = req(put, url(Db, "testdoc4"), attachment_doc()),
    ?assertEqual(201, Status),
    {Status1, _} = req(delete, url(Db, "testdoc4/file.erl?rev=" ++ Rev)),
    ?assertEqual(200, Status1).

t_return_409_for_put_att_nonexistent_rev(Db) ->
    {Status, Response} = req(put, url(Db, "t_return_404/file.erl?rev=1-000"), attachment_doc()),
    ?assertEqual(409, Status),
    ?assertMatch(#{<<"error">> := <<"not_found">>, <<"reason">> := <<"missing_rev">>}, Response).

t_return_update_seq_when_set_on_all_docs(Db) ->
    [create_doc(Db, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 3)],
    {Status, #{<<"update_seq">> := UpdateSeq, <<"offset">> := Offset}} =
        req(get, url(Db, "_all_docs?update_seq=true&keys=[\"testdoc1\"]")),
    ?assertEqual(200, Status),
    ?assertNotEqual(undefined, UpdateSeq),
    ?assertNotEqual(undefined, Offset).

t_not_return_update_seq_when_unset_on_all_docs(Db) ->
    [create_doc(Db, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 3)],
    {Status, #{<<"offset">> := Offset} = Response} =
        req(get, url(Db, "_all_docs?update_seq=false&keys=[\"testdoc1\"]")),
    ?assertEqual(200, Status),
    ?assertNot(maps:is_key(<<"update_seq">>, Response)),
    ?assertNotEqual(undefined, Offset).

t_return_correct_id_on_doc_copy(Db) ->
    create_doc(Db, "testdoc"),
    {_, #{<<"id">> := Id1}} = req(copy, url(Db, "testdoc"), [?CONTENT_JSON, ?AUTH, ?DESTHEADER1]),
    {_, #{<<"id">> := Id2}} = req(copy, url(Db, "testdoc"), [?CONTENT_JSON, ?AUTH, ?DESTHEADER2]),
    ?assertEqual(<<102, 111, 111, 229, 149, 138, 98, 97, 114>>, Id1),
    ?assertEqual(<<"foo/bar#baz?pow:fiz">>, Id2).

t_return_only_one_ok_on_doc_copy(Db) ->
    create_doc(Db, "testdoc"),
    {_, #{<<"ok">> := Res}} = req(copy, url(Db, "testdoc"), [?CONTENT_JSON, ?AUTH, ?DESTHEADER1]),
    ?assert(Res).

t_return_400_for_bad_engine(Db) ->
    {Status, _} = req(put, url(Db) ++ "?engine=cowabunga"),
    ?assertEqual(400, Status).

t_not_change_db_proper_after_rewriting_shardmap(Db) ->
    delete_db(Db),
    {201, _} = req(put, url(Db) ++ "?partitioned=true&q=1"),
    ShardDbName = ?l2b(config:get("mem3", "shards_db", "_dbs")),
    {ok, ShardDb} = mem3_util:ensure_exists(ShardDbName),
    {ok, #doc{body = {Props}}} = couch_db:open_doc(
        ShardDb, Db, [ejson_body]
    ),
    Shards = mem3_util:build_shards(Db, Props),
    {Prop2} = ?JSON_DECODE(?JSON_ENCODE({Props})),
    Shards2 = mem3_util:build_shards(Db, Prop2),
    ?assertEqual(Shards2, Shards).

t_succeed_on_all_docs_with_queries_keys(Db) ->
    helper_queries_with_keys(Db, "_all_docs", 3).

t_succeed_on_design_docs_with_queries_keys(Db) ->
    helper_queries_with_keys(Db, "_design_docs", 1).

t_succeed_on_local_docs_with_queries_keys(Db) ->
    helper_queries_with_keys(Db, "_local_docs", 1).

helper_queries_with_keys(Db, Path, ExpectedRows) ->
    [create_doc(Db, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 5)],
    [create_doc(Db, "_design/ddoc" ++ ?i2l(I)) || I <- lists:seq(1, 5)],
    [create_doc(Db, "_local/doc" ++ ?i2l(I)) || I <- lists:seq(1, 5)],
    QueryDoc = #{
        <<"queries">> => [
            #{
                <<"keys">> =>
                    [<<"testdoc1">>, <<"_design/ddoc3">>, <<"_local/doc5">>]
            }
        ]
    },
    {Status, #{<<"results">> := [#{<<"rows">> := Rows}]}} =
        req(post, url(Db, Path ++ "/queries"), QueryDoc),
    ?assertEqual(200, Status),
    ?assertEqual(ExpectedRows, length(Rows)).

t_succeed_on_all_docs_with_queries_limit_skip(Db) ->
    helper_queries_with_limit_skip(Db, "testdoc", "_all_docs", 2, 5).

t_succeed_on_design_docs_with_queries_limit_skip(Db) ->
    helper_queries_with_limit_skip(Db, "_design/ddoc", "_design_docs", 2, 5).

t_succeed_on_local_docs_with_queries_limit_skip(Db) ->
    helper_queries_with_limit_skip(Db, "_local/doc", "_local_docs", null, 5).

helper_queries_with_limit_skip(Db, Id, Path, ExpectedOffset, ExpectedRows) ->
    [create_doc(Db, Id ++ ?i2l(I)) || I <- lists:seq(1, 10)],
    QueryDoc = #{<<"queries">> => [#{<<"limit">> => 5, <<"skip">> => 2}]},
    {Status, #{<<"results">> := [#{<<"offset">> := Offset, <<"rows">> := Rows}]}} =
        req(post, url(Db, Path ++ "/queries"), QueryDoc),
    ?assertEqual(200, Status),
    ?assertEqual(ExpectedOffset, Offset),
    ?assertEqual(ExpectedRows, length(Rows)).

t_succeed_on_all_docs_with_multiple_queries(Db) ->
    helper_queries_with_keys_limit_skip(Db, "_all_docs", 2, 3, 5).

t_succeed_on_design_docs_with_multiple_queries(Db) ->
    helper_queries_with_keys_limit_skip(Db, "_design_docs", 2, 1, 5).

t_succeed_on_local_docs_with_multiple_queries(Db) ->
    helper_queries_with_keys_limit_skip(Db, "_local_docs", null, 1, 5).

helper_queries_with_keys_limit_skip(Db, Path, ExpectedOffset, ExpectedRows1, ExpectedRows2) ->
    [create_doc(Db, "testdoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
    [create_doc(Db, "_design/ddoc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
    [create_doc(Db, "_local/doc" ++ ?i2l(I)) || I <- lists:seq(1, 10)],
    QueryDoc = #{
        <<"queries">> => [
            #{
                <<"keys">> =>
                    [<<"testdoc1">>, <<"_design/ddoc3">>, <<"_local/doc5">>]
            },
            #{<<"limit">> => 5, <<"skip">> => 2}
        ]
    },
    {Status, #{
        <<"results">> := [
            #{<<"rows">> := Rows1},
            #{<<"offset">> := Offset2, <<"rows">> := Rows2}
        ]
    }} = req(post, url(Db, Path ++ "/queries"), QueryDoc),
    ?assertEqual(200, Status),
    ?assertEqual(ExpectedRows1, length(Rows1)),
    ?assertEqual(ExpectedOffset, Offset2),
    ?assertEqual(ExpectedRows2, length(Rows2)).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
url(Db) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/", ?b2l(Db)]).

url(Db, Path) ->
    url(Db) ++ "/" ++ Path.

create_db(Db) ->
    case req(put, url(Db)) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_test_db, Db, Error})
    end.

delete_db(Db) ->
    case req(delete, url(Db)) of
        {200, #{}} -> ok;
        Error -> error({failed_to_delete_test_db, Db, Error})
    end.

create_doc(Db, Id) ->
    req(put, url(Db, Id), #{<<"mr">> => <<"rockoartischocko">>}).

req(Method, Url) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.

req(copy, Url, Headers) ->
    {ok, Code, _, Res} = test_request:request(copy, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])};
req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.

wait_non_empty_chunk(Db) ->
    test_util:wait(
        fun() ->
            {ok, _, _, Bin} = test_request:get(url(Db, "_changes?feed=live&timeout=1"), [?AUTH]),
            Parts = binary:split(Bin, <<"\n">>, [global]),
            case [P || P <- Parts, size(P) > 0] of
                [] -> wait;
                Chunks -> Chunks
            end
        end
    ).

attachment_doc() ->
    {ok, Data} = file:read_file(?FIXTURE_TXT),
    #{
        <<"_attachments">> => #{
            <<"file.erl">> => #{
                <<"content_type">> => <<"text/plain">>,
                <<"data">> => base64:encode(Data)
            }
        }
    }.
