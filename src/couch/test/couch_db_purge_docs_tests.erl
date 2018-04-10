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

-module(couch_db_purge_docs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    DbName = ?tempdb(),
    {ok, _Db} = create_db(DbName),
    DbName.

teardown(DbName) ->
    delete_db(DbName),
    ok.

couch_db_purge_docs_test_() ->
    {
        "Couch_db purge_docs",
        [
            {
                setup,
                fun test_util:start_couch/0, fun test_util:stop_couch/1,
                [couch_db_purge_docs()]
            },
            purge_with_replication()
        ]

    }.


couch_db_purge_docs() ->
    {
       foreach,
            fun setup/0, fun teardown/1,
            [
                fun purge_simple/1,
                fun add_delete_purge/1,
                fun add_two_purge_one/1,
                fun purge_id_not_exist/1,
                fun purge_non_leaf_rev/1,
                fun purge_conflicts/1,
                fun purge_deep_tree/1
            ]
    }.


purge_simple(DbName) ->
    ?_test(
        begin
            {ok, Db} = couch_db:open_int(DbName, []),
            Doc1 = {[{<<"_id">>, <<"foo1">>}, {<<"vsn">>, 1.1}]},
            Doc2 = {[{<<"_id">>, <<"foo2">>}, {<<"vsn">>, 1.2}]},
            {ok, Rev} = save_doc(Db, Doc1),
            {ok, Rev2} = save_doc(Db, Doc2),
            couch_db:ensure_full_commit(Db),

            {ok, Db2} = couch_db:reopen(Db),
            ?assertEqual(2, couch_db_engine:get_doc_count(Db2)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db2)),
            ?assertEqual(2, couch_db_engine:get_update_seq(Db2)),
            ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),

            UUID = couch_uuids:new(), UUID2 = couch_uuids:new(),
            {ok, [{ok, PRevs}, {ok, PRevs2}]} = couch_db:purge_docs(
                Db2, [{UUID, <<"foo1">>, [Rev]}, {UUID2, <<"foo2">>, [Rev2]}]
            ),

            ?assertEqual([Rev], PRevs),
            ?assertEqual([Rev2], PRevs2),

            {ok, Db3} = couch_db:reopen(Db2),
            {ok, PIdsRevs} = couch_db:fold_purged_docs(
                Db3, 0, fun fold_fun/2, [], []),
            ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
            ?assertEqual(3, couch_db_engine:get_update_seq(Db3)),
            ?assertEqual(2, couch_db_engine:get_purge_seq(Db3)),
            ?assertEqual([{<<"foo2">>, [Rev2]}, {<<"foo1">>, [Rev]}], PIdsRevs)
        end).


add_delete_purge(DbName) ->
    ?_test(
        begin
            {ok, Db0} = couch_db:open_int(DbName, []),
            Doc0 = {[{<<"_id">>,<<"foo">>}, {<<"vsn">>, 1}]},
            {ok, Rev} = save_doc(Db0, Doc0),
            couch_db:ensure_full_commit(Db0),
            {ok, Db1} = couch_db:reopen(Db0),

            Doc1 = {[
                {<<"_id">>, <<"foo">>}, {<<"vsn">>, 2},
                {<<"_rev">>, couch_doc:rev_to_str(Rev)},
                {<<"_deleted">>, true}]
            },
            {ok, Rev2} = save_doc(Db1, Doc1),
            couch_db:ensure_full_commit(Db1),

            {ok, Db2} = couch_db:reopen(Db1),
            {ok, PIdsRevs1} = couch_db:fold_purged_docs(
                Db2, 0, fun fold_fun/2, [], []),
            ?assertEqual(0, couch_db_engine:get_doc_count(Db2)),
            ?assertEqual(1, couch_db_engine:get_del_doc_count(Db2)),
            ?assertEqual(2, couch_db_engine:get_update_seq(Db2)),
            ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),
            ?assertEqual([], PIdsRevs1),

            UUID = couch_uuids:new(),
            {ok, [{ok, PRevs}]} = couch_db:purge_docs(
                Db2, [{UUID, <<"foo">>, [Rev2]}]),
            ?assertEqual([Rev2], PRevs),

            {ok, Db3} = couch_db:reopen(Db2),
            {ok, PIdsRevs2} = couch_db:fold_purged_docs(
                Db3, 0, fun fold_fun/2, [], []),
            ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
            ?assertEqual(3, couch_db_engine:get_update_seq(Db3)),
            ?assertEqual(1, couch_db_engine:get_purge_seq(Db3)),
            ?assertEqual([{<<"foo">>, [Rev2]}], PIdsRevs2)
        end).


add_two_purge_one(DbName) ->
    ?_test(
        begin
            {ok, Db} = couch_db:open_int(DbName, []),
            Doc1 = {[{<<"_id">>, <<"foo1">>}, {<<"vsn">>, 1}]},
            Doc2 = {[{<<"_id">>, <<"foo2">>}, {<<"vsn">>, 2}]},
            {ok, Rev} = save_doc(Db, Doc1),
            {ok, _Rev2} = save_doc(Db, Doc2),
            couch_db:ensure_full_commit(Db),

            {ok, Db2} = couch_db:reopen(Db),
            ?assertEqual(2, couch_db_engine:get_doc_count(Db2)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db2)),
            ?assertEqual(2, couch_db_engine:get_update_seq(Db2)),
            ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),

            UUID = couch_uuids:new(),
            {ok, [{ok, PRevs}]} = couch_db:purge_docs(Db2,
                [{UUID, <<"foo1">>, [Rev]}]),
            ?assertEqual([Rev], PRevs),

            {ok, Db3} = couch_db:reopen(Db2),
            {ok, PIdsRevs} = couch_db:fold_purged_docs(
                Db3, 0, fun fold_fun/2, [], []),
            ?assertEqual(1, couch_db_engine:get_doc_count(Db3)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
            ?assertEqual(3, couch_db_engine:get_update_seq(Db3)),
            ?assertEqual(1, couch_db_engine:get_purge_seq(Db3)),
            ?assertEqual([{<<"foo1">>, [Rev]}], PIdsRevs)
        end).


purge_id_not_exist(DbName) ->
    ?_test(
        begin
            {ok, Db} = couch_db:open_int(DbName, []),
            UUID = couch_uuids:new(),
            {ok, [{ok, PRevs}]} = couch_db:purge_docs(Db,
                [{UUID, <<"foo">>, [{0, <<0>>}]}]),
            ?assertEqual([], PRevs),

            {ok, Db2} = couch_db:reopen(Db),
            {ok, PIdsRevs} = couch_db:fold_purged_docs(
                Db2, 0, fun fold_fun/2, [], []),
            ?assertEqual(0, couch_db_engine:get_doc_count(Db2)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db2)),
            ?assertEqual(0, couch_db_engine:get_update_seq(Db2)),
            ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),
            ?assertEqual([], PIdsRevs)
        end).


purge_non_leaf_rev(DbName) ->
    ?_test(
        begin
            {ok, Db} = couch_db:open_int(DbName, []),
            Doc0 = {[{<<"_id">>, <<"foo">>}, {<<"vsn">>, 1}]},
            {ok, Rev} = save_doc(Db, Doc0),
            couch_db:ensure_full_commit(Db),
            {ok, Db2} = couch_db:reopen(Db),

            Doc1 = {[
                {<<"_id">>, <<"foo">>}, {<<"vsn">>, 2},
                {<<"_rev">>, couch_doc:rev_to_str(Rev)}
            ]},
            {ok, _Rev2} = save_doc(Db2, Doc1),
            couch_db:ensure_full_commit(Db2),
            {ok, Db3} = couch_db:reopen(Db2),

            UUID = couch_uuids:new(),
            {ok, [{ok, PRevs}]} = couch_db:purge_docs(Db3,
                [{UUID, <<"foo">>, [Rev]}]),
            ?assertEqual([], PRevs),

            {ok, Db4} = couch_db:reopen(Db3),
            {ok, PIdsRevs} = couch_db:fold_purged_docs(Db4, 0, fun fold_fun/2, [], []),
            ?assertEqual(1, couch_db_engine:get_doc_count(Db4)),
            ?assertEqual(2, couch_db_engine:get_update_seq(Db4)),
            ?assertEqual(0, couch_db_engine:get_purge_seq(Db4)),
            ?assertEqual([], PIdsRevs)
        end).


purge_conflicts(DbName) ->
    ?_test(
        begin
            {ok, Db} = couch_db:open_int(DbName, []),
            Doc = {[{<<"_id">>, <<"foo">>}, {<<"vsn">>, <<"v1.1">>}]},
            {ok, Rev} = save_doc(Db, Doc),
            couch_db:ensure_full_commit(Db),
            {ok, Db2} = couch_db:reopen(Db),

            % create a conflict
            DocConflict = #doc{
                id = <<"foo">>,
                revs = {1, [crypto:hash(md5, <<"v1.2">>)]},
                body = {[ {<<"vsn">>,  <<"v1.2">>}]}
            },
            {ok, _} = couch_db:update_doc(Db2, DocConflict, [], replicated_changes),
            couch_db:ensure_full_commit(Db2),
            {ok, Db3} = couch_db:reopen(Db2),

            UUID = couch_uuids:new(),
            {ok, [{ok, PRevs}]} = couch_db:purge_docs(Db3,
                [{UUID, <<"foo">>, [Rev]}]),
            ?assertEqual([Rev], PRevs),

            {ok, Db4} = couch_db:reopen(Db3),
            {ok, PIdsRevs} = couch_db:fold_purged_docs(
                Db4, 0, fun fold_fun/2, [], []),
            % still has one doc
            ?assertEqual(1, couch_db_engine:get_doc_count(Db4)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db4)),
            ?assertEqual(3, couch_db_engine:get_update_seq(Db4)),
            ?assertEqual(1, couch_db_engine:get_purge_seq(Db4)),
            ?assertEqual([{<<"foo">>, [Rev]}], PIdsRevs)
        end).


purge_deep_tree(DbName) ->
    ?_test(
        begin
            NRevs = 100,
            {ok, Db0} = couch_db:open_int(DbName, []),
            Doc0 = {[{<<"_id">>, <<"bar">>}, {<<"vsn">>, 0}]},
            {ok, InitRev} = save_doc(Db0, Doc0),
            ok = couch_db:close(Db0),
            LastRev = lists:foldl(fun(V, PrevRev) ->
                {ok, Db} = couch_db:open_int(DbName, []),
                {ok, Rev} = save_doc(Db,
                    {[{<<"_id">>, <<"bar">>},
                    {<<"vsn">>, V},
                    {<<"_rev">>, couch_doc:rev_to_str(PrevRev)}]}
                ),
                ok = couch_db:close(Db),
                Rev
            end, InitRev, lists:seq(2, NRevs)),
            {ok, Db1} = couch_db:open_int(DbName, []),

            % purge doc
            UUID = couch_uuids:new(),
            {ok, [{ok, PRevs}]} = couch_db:purge_docs(Db1,
                [{UUID, <<"bar">>, [LastRev]}]),
            ?assertEqual([LastRev], PRevs),

            {ok, Db2} = couch_db:reopen(Db1),
            % no docs left
            ?assertEqual(0, couch_db_engine:get_doc_count(Db2)),
            ?assertEqual(0, couch_db_engine:get_del_doc_count(Db2)),
            ?assertEqual(1, couch_db_engine:get_purge_seq(Db2)),
            ?assertEqual(NRevs + 1 , couch_db_engine:get_update_seq(Db2))
        end).


purge_with_replication() ->
    ?_test(
        begin
            Ctx = test_util:start_couch([couch_replicator]),
            Source = ?tempdb(),
            {ok, SourceDb} = create_db(Source),
            Target = ?tempdb(),
            {ok, _Db} = create_db(Target),

            % create Doc and do replication to Target
            {ok, Rev} = save_doc(SourceDb,
                {[{<<"_id">>, <<"foo">>}, {<<"vsn">>, 1}]}),
            couch_db:ensure_full_commit(SourceDb),
            {ok, SourceDb2} = couch_db:reopen(SourceDb),
            RepObject = {[
                {<<"source">>, Source},
                {<<"target">>, Target}
            ]},
            {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
            {ok, TargetDb} = couch_db:open_int(Target, []),
            {ok, Doc} = couch_db:get_doc_info(TargetDb, <<"foo">>),

            % purge Doc on Source and do replication to Target
            % assert purges don't get replicated to Target
            UUID = couch_uuids:new(),
            {ok, _} = couch_db:purge_docs(SourceDb2, [{UUID, <<"foo">>, [Rev]}]),
            {ok, SourceDb3} = couch_db:reopen(SourceDb2),
            {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
            {ok, TargetDb2} = couch_db:open_int(Target, []),
            {ok, Doc2} = couch_db:get_doc_info(TargetDb2, <<"foo">>),
            [Rev2] = Doc2#doc_info.revs,
            ?assertEqual(Rev, Rev2#rev_info.rev),
            ?assertEqual(Doc, Doc2),
            ?assertEqual(0, couch_db_engine:get_doc_count(SourceDb3)),
            ?assertEqual(1, couch_db_engine:get_purge_seq(SourceDb3)),
            ?assertEqual(1, couch_db_engine:get_doc_count(TargetDb2)),
            ?assertEqual(0, couch_db_engine:get_purge_seq(TargetDb2)),

            % replicate from Target to Source
            % assert that Doc reappears on Source
            RepObject2 = {[
                {<<"source">>, Target},
                {<<"target">>, Source}
            ]},
            {ok, _} = couch_replicator:replicate(RepObject2, ?ADMIN_USER),
            {ok, SourceDb4} = couch_db:reopen(SourceDb3),
            {ok, Doc3} = couch_db:get_doc_info(SourceDb4, <<"foo">>),
            [Rev3] = Doc3#doc_info.revs,
            ?assertEqual(Rev, Rev3#rev_info.rev),
            ?assertEqual(1, couch_db_engine:get_doc_count(SourceDb4)),
            ?assertEqual(1, couch_db_engine:get_purge_seq(SourceDb4)),

            delete_db(Source),
            delete_db(Target),
            ok = application:stop(couch_replicator),
            ok = test_util:stop_couch(Ctx)
        end).


create_db(DbName) ->
    couch_db:create(DbName, [?ADMIN_CTX, overwrite]).

delete_db(DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]).

save_doc(Db, Json) ->
    Doc = couch_doc:from_json_obj(Json),
    couch_db:update_doc(Db, Doc, []).

fold_fun({_PSeq, _UUID, Id, Revs}, Acc) ->
    [{Id, Revs} | Acc].