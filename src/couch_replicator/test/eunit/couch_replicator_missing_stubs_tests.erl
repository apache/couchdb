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

-module(couch_replicator_missing_stubs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


-define(REVS_LIMIT, 3).


missing_stubs_test_() ->
    {
        "Replicate docs with missing stubs (COUCHDB-1365)",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_replicate_docs_with_missed_att_stubs, 60)
                ]
            }
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    populate_db(Source),
    Target = couch_replicator_test_helper:create_db(),
    {Source, Target}.


teardown({Source, Target}) ->
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


should_replicate_docs_with_missed_att_stubs({Source, Target}) ->
    {ok, TargetDb} = fabric2_db:open(Target, [?ADMIN_CTX]),
    ?assertEqual(ok, fabric2_db:set_revs_limit(TargetDb, ?REVS_LIMIT)),

    ?assertMatch({ok, _},
        couch_replicator_test_helper:replicate(Source, Target)),
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target)),

    ok = update_db_docs(Source, ?REVS_LIMIT * 2),

    ?assertMatch({ok, _},
        couch_replicator_test_helper:replicate(Source, Target)),
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target)).


populate_db(DbName) ->
    AttData = crypto:strong_rand_bytes(6000),
    Doc = #doc{
        id = <<"doc1">>,
        atts = [
            couch_att:new([
                {name, <<"doc1_att1">>},
                {type, <<"application/foobar">>},
                {att_len, byte_size(AttData)},
                {data, AttData}
            ])
        ]
    },
    couch_replicator_test_helper:create_docs(DbName, [Doc]).


update_db_docs(DbName, Times) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    FoldFun = fun
        ({meta, _Meta}, Acc) ->
            {ok, Acc};
        (complete, Acc) ->
            {ok, Acc};
        ({row, Row}, Acc) ->
            {_, DocId} = lists:keyfind(id, 1, Row),
            ok = update_doc(DbName, DocId, Times),
            {ok, Acc}
    end,
    Opts = [{restart_tx, true}],
    {ok, _} = fabric2_db:fold_docs(Db, FoldFun, ok, Opts),
    ok.


update_doc(_DbName, _DocId, 0) ->
    ok;

update_doc(DbName, DocId, Times) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    {ok, Doc} = fabric2_db:open_doc(Db, DocId, []),
    #doc{revs = {Pos, [Rev | _]}} = Doc,
    Val = base64:encode(crypto:strong_rand_bytes(100)),
    Doc1 = Doc#doc{
        revs = {Pos, [Rev]},
        body = {[{<<"value">>, Val}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc1),
    update_doc(DbName, DocId, Times - 1).
