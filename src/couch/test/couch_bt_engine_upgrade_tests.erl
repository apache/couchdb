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

-module(couch_bt_engine_upgrade_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    Ctx = test_util:start_couch(),
    DbDir = config:get("couchdb", "database_dir"),
    DbFileNames = [
        "db_without_purge_req.couch",
        "db_with_1_purge_req.couch",
        "db_with_2_purge_req.couch",
        "db_with_1_purge_req_for_2_docs.couch"
    ],
    NewPaths = lists:map(fun(DbFileName) ->
        OldDbFilePath = filename:join([?FIXTURESDIR, DbFileName]),
        NewDbFilePath = filename:join([DbDir, DbFileName]),
        ok = filelib:ensure_dir(NewDbFilePath),
        file:delete(NewDbFilePath),
        {ok, _} = file:copy(OldDbFilePath, NewDbFilePath),
        NewDbFilePath
    end, DbFileNames),
    {Ctx, NewPaths}.


teardown({Ctx, Paths}) ->
    test_util:stop_couch(Ctx),
    lists:foreach(fun(Path) ->
        file:delete(Path)
    end, Paths).


upgrade_test_() ->
    {
        "Couch Bt Engine Upgrade tests",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                t_upgrade_without_purge_req(),
                t_upgrade_with_1_purge_req(),
                t_upgrade_with_N_purge_req(),
                t_upgrade_with_1_purge_req_for_2_docs()
            ]
        }
    }.


t_upgrade_without_purge_req() ->
    ?_test(begin
        % There are three documents in the fixture
        % db with zero purge entries
        DbName = <<"db_without_purge_req">>,

        {ok, UpgradedPurged} = couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual(0, couch_db:get_purge_seq(Db)),
            couch_db:fold_purge_infos(Db, 0, fun fold_fun/2, [])
        end),
        ?assertEqual([], UpgradedPurged),

        {ok, Rev} = save_doc(
            DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}
        ),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 5}, couch_db:get_doc_count(Db)),
            ?assertEqual(0, couch_db:get_purge_seq(Db))
        end),

        PurgeReqs = [
            {couch_uuids:random(), <<"doc4">>, [Rev]}
        ],

        {ok, [{ok, PRevs}]} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:purge_docs(Db, PurgeReqs)
        end),
        ?assertEqual(PRevs, [Rev]),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 4}, couch_db:get_doc_count(Db)),
            ?assertEqual(1, couch_db:get_purge_seq(Db))
        end)
    end).


t_upgrade_with_1_purge_req() ->
    ?_test(begin
        % There are two documents in the fixture database
        % with a single purge entry
        DbName = <<"db_with_1_purge_req">>,

        {ok, UpgradedPurged} = couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual(1, couch_db:get_purge_seq(Db)),
            couch_db:fold_purge_infos(Db, 0, fun fold_fun/2, [])
        end),
        ?assertEqual([{1, <<"doc1">>}], UpgradedPurged),

        {ok, Rev} = save_doc(
            DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}
        ),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 4}, couch_db:get_doc_count(Db)),
            ?assertEqual(1, couch_db:get_purge_seq(Db))
        end),

        PurgeReqs = [
            {couch_uuids:random(), <<"doc4">>, [Rev]}
        ],

        {ok, [{ok, PRevs}]} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:purge_docs(Db, PurgeReqs)
        end),
        ?assertEqual(PRevs, [Rev]),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 3}, couch_db:get_doc_count(Db)),
            ?assertEqual(2, couch_db:get_purge_seq(Db))
        end)
    end).


t_upgrade_with_N_purge_req() ->
    ?_test(begin
        % There is one document in the fixture database
        % with two docs that have been purged
        DbName = <<"db_with_2_purge_req">>,

        {ok, UpgradedPurged} = couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual(2, couch_db:get_purge_seq(Db)),
            couch_db:fold_purge_infos(Db, 1, fun fold_fun/2, [])
        end),
        ?assertEqual([{2, <<"doc2">>}], UpgradedPurged),

        {ok, Rev} = save_doc(DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 3}, couch_db:get_doc_count(Db)),
            ?assertEqual(2, couch_db:get_purge_seq(Db))
        end),

        PurgeReqs = [
            {couch_uuids:random(), <<"doc4">>, [Rev]}
        ],

        {ok, [{ok, PRevs}]} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:purge_docs(Db, PurgeReqs)
        end),
        ?assertEqual(PRevs, [Rev]),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 2}, couch_db:get_doc_count(Db)),
            ?assertEqual(3, couch_db:get_purge_seq(Db))
        end)
    end).


t_upgrade_with_1_purge_req_for_2_docs() ->
    ?_test(begin
        % There are two documents (Doc4 and Doc5) in the fixture database
        % with three docs (Doc1, Doc2 and Doc3) that have been purged, and
        % with one purge req for Doc1 and another purge req for Doc 2 and Doc3
        DbName = <<"db_with_1_purge_req_for_2_docs">>,

        {ok, UpgradedPurged} = couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual(3, couch_db:get_purge_seq(Db)),
            couch_db:fold_purge_infos(Db, 1, fun fold_fun/2, [])
        end),
        ?assertEqual([{3,<<"doc2">>},{2,<<"doc3">>}], UpgradedPurged),

        {ok, Rev} = save_doc(DbName, {[{<<"_id">>, <<"doc6">>}, {<<"v">>, 1}]}),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc7">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 4}, couch_db:get_doc_count(Db)),
            ?assertEqual(3, couch_db:get_purge_seq(Db))
        end),

        PurgeReqs = [
            {couch_uuids:random(), <<"doc6">>, [Rev]}
        ],

        {ok, [{ok, PRevs}]} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:purge_docs(Db, PurgeReqs)
        end),
        ?assertEqual(PRevs, [Rev]),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 3}, couch_db:get_doc_count(Db)),
            ?assertEqual(4, couch_db:get_purge_seq(Db))
        end)
    end).


save_doc(DbName, Json) ->
    Doc = couch_doc:from_json_obj(Json),
    couch_util:with_db(DbName, fun(Db) ->
        couch_db:update_doc(Db, Doc, [])
    end).


fold_fun({PSeq, _UUID, Id, _Revs}, Acc) ->
    {ok, [{PSeq, Id} | Acc]}.
