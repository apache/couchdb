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

-module(couch_views_indexer_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


-define(I_HEART_EUNIT(Tests), [{with, [T]} || T <- Tests]).


indexer_test_() ->
    {
        "Test view indexing",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                ?I_HEART_EUNIT([
                    fun indexed_empty_db/1,
                    fun indexed_single_doc/1,
                    fun updated_docs_are_reindexed/1,
                    fun updated_docs_without_changes_are_reindexed/1,
                    fun deleted_docs_not_indexed/1,
                    fun deleted_docs_are_unindexed/1,
                    fun multipe_docs_with_same_key/1,
                    fun multipe_keys_from_same_doc/1,
                    fun multipe_identical_keys_from_same_doc/1
                ])
            }
        }
    }.


setup() ->
    Ctx = test_util:start_couch([
            fabric,
            couch_jobs,
            couch_js,
            couch_views
        ]),
    Ctx.


cleanup(Ctx) ->
    test_util:stop_couch(Ctx).


foreach_setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    Db.


foreach_teardown(Db) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


indexed_empty_db(Db) ->
    DDoc = create_ddoc(),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, Out} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([], Out).


indexed_single_doc(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc1, []),

    {ok, Out} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([{row, [
            {id, <<"0">>},
            {key, 0},
            {value, 0}
        ]}], Out).


updated_docs_are_reindexed(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),

    {ok, Out1} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([{row, [
            {id, <<"0">>},
            {key, 0},
            {value, 0}
        ]}], Out1),

    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        body = {[{<<"val">>, 1}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),

    {ok, Out2} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([{row, [
            {id, <<"0">>},
            {key, 1},
            {value, 1}
        ]}], Out2),

    % Check that our id index is updated properly
    % as well.
    DbName = fabric2_db:name(Db),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    Sig = Mrst#mrst.sig,
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        ?assertMatch(
                [{0, 1, _, [1]}, {1, 0, 0, []}],
                couch_views_fdb:get_view_keys(TxDb, Sig, <<"0">>)
            )
    end).


updated_docs_without_changes_are_reindexed(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),

    {ok, Out1} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([{row, [
            {id, <<"0">>},
            {key, 0},
            {value, 0}
        ]}], Out1),

    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        body = {[{<<"val">>, 0}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),

    {ok, Out2} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([{row, [
            {id, <<"0">>},
            {key, 0},
            {value, 0}
        ]}], Out2),

    % Check fdb directly to make sure we've also
    % removed the id idx keys properly.
    DbName = fabric2_db:name(Db),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    Sig = Mrst#mrst.sig,
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        ?assertMatch(
                [{0, 1, _, [0]}, {1, 0, 0, []}],
                couch_views_fdb:get_view_keys(TxDb, Sig, <<"0">>)
            )
    end).


deleted_docs_not_indexed(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),
    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        deleted = true,
        body = {[{<<"val">>, 1}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),

    {ok, Out} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([], Out).


deleted_docs_are_unindexed(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),

    {ok, Out1} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([{row, [
            {id, <<"0">>},
            {key, 0},
            {value, 0}
        ]}], Out1),

    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        deleted = true,
        body = {[{<<"val">>, 1}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),

    {ok, Out2} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([], Out2),

    % Check fdb directly to make sure we've also
    % removed the id idx keys properly.
    DbName = fabric2_db:name(Db),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    Sig = Mrst#mrst.sig,
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        ?assertEqual([], couch_views_fdb:get_view_keys(TxDb, Sig, <<"0">>))
    end).


multipe_docs_with_same_key(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0, 1),
    Doc2 = doc(1, 1),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_docs(Db, [Doc1, Doc2], []),

    {ok, Out} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([
            {row, [
                {id, <<"0">>},
                {key, 1},
                {value, 1}
            ]},
            {row, [
                {id, <<"1">>},
                {key, 1},
                {value, 1}
            ]}
        ], Out).


multipe_keys_from_same_doc(Db) ->
    DDoc = create_ddoc(multi_emit_different),
    Doc = doc(0, 1),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc, []),

    {ok, Out} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([
            {row, [
                {id, <<"0">>},
                {key, 1},
                {value, 1}
            ]},
            {row, [
                {id, <<"0">>},
                {key, <<"0">>},
                {value, <<"0">>}
            ]}
        ], Out).


multipe_identical_keys_from_same_doc(Db) ->
    DDoc = create_ddoc(multi_emit_same),
    Doc = doc(0, 1),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc, []),

    {ok, Out} = couch_views:query(
            Db,
            DDoc,
            <<"map_fun1">>,
            fun fold_fun/2,
            [],
            #mrargs{}
        ),

    ?assertEqual([
            {row, [
                {id, <<"0">>},
                {key, 1},
                {value, 1}
            ]},
            {row, [
                {id, <<"0">>},
                {key, 1},
                {value, 2}
            ]}
        ], Out).


fold_fun({meta, _Meta}, Acc) ->
    {ok, Acc};
fold_fun({row, _} = Row, Acc) ->
    {ok, [Row | Acc]};
fold_fun(complete, Acc) ->
    {ok, lists:reverse(Acc)}.


create_ddoc() ->
    create_ddoc(simple).


create_ddoc(simple) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"map_fun1">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}},
            {<<"map_fun2">>, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}}
        ]}}
    ]});

create_ddoc(multi_emit_different) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"map_fun1">>, {[
                {<<"map">>, <<"function(doc) { "
                    "emit(doc._id, doc._id); "
                    "emit(doc.val, doc.val); "
                "}">>}
            ]}},
            {<<"map_fun2">>, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}}
        ]}}
    ]});

create_ddoc(multi_emit_same) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"map_fun1">>, {[
                {<<"map">>, <<"function(doc) { "
                    "emit(doc.val, doc.val * 2); "
                    "emit(doc.val, doc.val); "
                "}">>}
            ]}},
            {<<"map_fun2">>, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}}
        ]}}
    ]}).


doc(Id) ->
    doc(Id, Id).


doc(Id, Val) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"val">>, Val}
    ]}).
