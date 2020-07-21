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

-module(couch_views_reduce_update_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/test/fabric2_test.hrl").
-include_lib("couch_views/include/couch_views.hrl").

-define(IDX1, <<"idx1">>).
-define(IDX2, <<"idx2">>).


indexer_test_() ->
    {
        "Test indexing",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                [
                    ?TDEF_FE(index_docs),
                    ?TDEF_FE(update_doc),
                    ?TDEF_FE(update_doc_key_changes),
                    ?TDEF_FE(delete_doc)
                ]
            }
        }
    }.


setup() ->
    Ctx = test_util:start_couch([
        fabric,
        couch_jobs,
        couch_rate,
        couch_js,
        couch_eval,
        couch_views
    ]),
    Ctx.


cleanup(Ctx) ->
    test_util:stop_couch(Ctx).


foreach_setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    Db.


foreach_teardown(Db) ->
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


index_docs(Db) ->
    Doc1 = doc(1, [1, 1], 1),
    Doc2 = doc(2, [1, 2], 2),
    DDoc = create_ddoc(simple),

    fabric2_db:update_docs(Db, [Doc1, Doc2, DDoc]),

    Res = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 1},
        {key, [1, 2]}, {value, 2}
    ], Res),

    Doc3 = doc(3, [1, 1], 1),
    fabric2_db:update_docs(Db, [Doc3]),

    Res2 = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 2},
        {key, [1, 2]}, {value, 2}
    ], Res2).


update_doc(Db) ->
    Doc1 = doc(1, [1, 1], 1),
    Doc2 = doc(2, [1, 1], 1),

    {ok, RevInfo} =fabric2_db:update_doc(Db, Doc1),

    DDoc = create_ddoc(simple),
    fabric2_db:update_docs(Db, [DDoc, Doc2]),

    Res = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 2}
    ], Res),

    NewDoc1 = update_doc(Doc1, RevInfo, [1, 1], 3),
    {ok, _} = fabric2_db:update_doc(Db, NewDoc1, []),

    Res2 = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 4}
    ], Res2),

    Res3 = run_query(Db, DDoc, ?IDX2),
    ?assertEqual([
        {key, [1, 1]}, {value, 2}
    ], Res3).


update_doc_key_changes(Db) ->
    Doc1 = doc(1, [1, 1], 2),
    Doc2 = doc(2, [1, 1], 2),

    {ok, RevInfo} =fabric2_db:update_doc(Db, Doc1),

    DDoc = create_ddoc(complex),
    fabric2_db:update_docs(Db, [DDoc, Doc2]),

    Res = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 4},
        {key, [3, 4]}, {value, 40}
    ], Res),

    NewDoc1 = update_doc(Doc1, RevInfo, [1, 1], 3),
    {ok, _} = fabric2_db:update_doc(Db, NewDoc1, []),

    Res2 = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 5},
        {key, [3, 4]}, {value, 20},
        {key, [4, 4]}, {value, 8}
    ], Res2).


delete_doc(Db) ->
    Doc1 = doc(1, [1, 1], 2),
    Doc2 = doc(2, [1, 1], 2),

    {ok, _} =fabric2_db:update_doc(Db, Doc1),

    DDoc = create_ddoc(complex),
    fabric2_db:update_docs(Db, [DDoc, Doc2]),

    Res = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 4},
        {key, [3, 4]}, {value, 40}
    ], Res),

    {ok, Doc} = fabric2_db:open_doc(Db, <<"1">>),
    JsonDoc = couch_doc:to_json_obj(Doc, []),
    JsonDoc2 = couch_util:json_apply_field({<<"_deleted">>, true}, JsonDoc),
    NewDoc1 = couch_doc:from_json_obj(JsonDoc2),

    {ok, _} = fabric2_db:update_doc(Db, NewDoc1, []),

    Res2 = run_query(Db, DDoc, ?IDX1),
    ?assertEqual([
        {key, [1, 1]}, {value, 2},
        {key, [3, 4]}, {value, 20}
    ], Res2).


run_query(Db, DDoc, Idx) ->
    Args = #mrargs{
        view_type = reduce,
        reduce = true,
        group = true,
        group_level = undefined
    },
    CB = fun query_cb/2,
    {ok, Acc} = couch_views:query(Db, DDoc, Idx, CB, [], Args),
    Acc.


create_ddoc(simple) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/ddoc1">>},
        {<<"views">>, {[
            {?IDX1, {[
                {<<"map">>, <<"function(doc) {emit(doc.key, doc.value);}">>},
                {<<"reduce">>, <<"_sum">>}
            ]}},
            {?IDX2, {[
                {<<"map">>, <<"function(doc) {emit(doc.key, doc.value);}">>},
                {<<"reduce">>, <<"_count">>}
            ]}}
        ]}}
    ]});


create_ddoc(complex) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/ddoc1">>},
        {<<"views">>, {[
            {?IDX1, {[
                {<<"map">>, <<"function(doc) {"
                    "emit(doc.key, doc.value);"
                    "if (doc.value % 2 === 0) {"
                    "   emit([doc.value + 1, doc.value + 2], doc.value * 10);"
                    "} else {"
                    "   emit([doc.value + 1, doc.value + 1], doc.value + 5);"
                    "}"
                    "}">>},
                {<<"reduce">>, <<"_sum">>}
            ]}},
            {?IDX2, {[
                {<<"map">>, <<"function(doc) {emit(doc.key, doc.value);}">>},
                {<<"reduce">>, <<"_count">>}
            ]}}
        ]}}
    ]}).


doc(Id, Key, Val) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"key">>, Key},
        {<<"value">>, Val}
    ]}).


update_doc(OldDoc, {Pos, Rev}, Key, Val) ->
    OldDoc#doc{
        revs = {Pos, [Rev]},
        body = {[
            {<<"key">>, Key},
            {<<"value">>, Val}
        ]}
    }.


query_cb({row, Props},  Acc) ->
    {ok, Acc ++ Props};

query_cb(_, Acc) ->
    {ok, Acc}.

