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

-module(couch_views_size_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("couch_views/include/couch_views.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


-define(MAP_FUN1, <<"map_fun1">>).
-define(MAP_FUN2, <<"map_fun2">>).


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
                [
                    ?TDEF_FE(empty_view),
                    ?TDEF_FE(single_doc),
                    ?TDEF_FE(multiple_docs),
                    ?TDEF_FE(update_no_size_change),
                    ?TDEF_FE(update_increases_size),
                    ?TDEF_FE(update_decreases_size),
                    ?TDEF_FE(deleting_docs_decreases_size),
                    ?TDEF_FE(multi_identical_keys_count_twice),
                    ?TDEF_FE(multiple_design_docs),
                    ?TDEF_FE(multiple_identical_design_docs)
                ]
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
    config:set("couch_views", "view_btree_node_size", "4", false),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    Db.


foreach_teardown(Db) ->
    meck:unload(),
    config:delete("couch_views", "change_limit"),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


empty_view(Db) ->
    DDoc = create_ddoc(),
    ?assertEqual(0, view_size(Db)),
    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),
    ?assertEqual(0, view_size(Db)).


single_doc(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc1, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    % Row: key: 0, row: 0, docid: "0"
    % Bytes: key: 1, row: 1, docid: 3
    % Total: 1 + 1 + 3 = 5
    ?assertEqual(5, view_size(Db)).


multiple_docs(Db) ->
    DDoc = create_ddoc(),
    Docs = [doc(I) || I <- lists:seq(0, 49)],

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_docs(Db, Docs, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    % Rows 0-9: 1 + 1 + 3 = 5
    % Rows 10->49: 2 + 2 + 4 = 8
    % 10 * 5 + 40 * 8 = 370
    ?assertEqual(370, view_size(Db)).


update_no_size_change(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    ?assertEqual(5, view_size(Db)),

    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        body = {[{<<"val">>, 1}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    % Row became: key: 1, val: 1, docid: "0"
    % 1 + 1 + 3 = 5 so samesies
    ?assertEqual(5, view_size(Db)).


update_increases_size(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    ?assertEqual(5, view_size(Db)),

    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        body = {[{<<"val">>, 10}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    % Row became: key: 10, val: 10, docid: "0"
    % 2 + 2 + 3 = 7
    ?assertEqual(7, view_size(Db)).


update_decreases_size(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(10),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    % Row: key: 10, val: 10, docid: "10"
    % 2 + 2 + 4 = 8
    ?assertEqual(8, view_size(Db)),

    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        body = {[{<<"val">>, 0}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    % Row became: key: 0, val: 0, docid: "10"
    % 1 + 1 + 4 = 6
    ?assertEqual(6, view_size(Db)).


deleting_docs_decreases_size(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc1, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    ?assertEqual(5, view_size(Db)),

    Doc2 = Doc1#doc{
        revs = {Pos, [Rev]},
        deleted = true,
        body = {[{<<"val">>, 1}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, []),
    {ok, []} = run_query(Db, DDoc, ?MAP_FUN1),

    ?assertEqual(0, view_size(Db)).


multi_identical_keys_count_twice(Db) ->
    DDoc = create_ddoc(multi_emit_same),
    Doc = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc, []),
    {ok, _} = run_query(Db, DDoc, ?MAP_FUN1),

    % Two rows that are the same
    ?assertEqual(10, view_size(Db)).


multiple_design_docs(Db) ->
    Cleanup = fun() ->
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            DDocs = fabric2_db:get_design_docs(Db),
            ok = couch_views:cleanup_indices(TxDb, DDocs)
        end)
    end,

    DDoc1 = create_ddoc(simple, <<"_design/bar1">>),
    DDoc2 = create_ddoc(multi_emit_same, <<"_design/bar2">>),

    % Simple test as before
    {ok, _} = fabric2_db:update_doc(Db, doc(0), []),
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, DDoc1, []),
    {ok, _} = run_query(Db, DDoc1, ?MAP_FUN1),
    ?assertEqual(5, view_size(Db)),

    % Adding a second ddoc increases the size
    {ok, {Pos2, Rev2}} = fabric2_db:update_doc(Db, DDoc2, []),
    {ok, _} = run_query(Db, DDoc2, ?MAP_FUN1),
    ?assertEqual(15, view_size(Db)),

    % Removing the first ddoc decreases the size
    DDoc1Del = DDoc1#doc{revs = {Pos1, [Rev1]}, deleted = true},
    {ok, _} = fabric2_db:update_doc(Db, DDoc1Del, []),
    Cleanup(),
    ?assertEqual(10, view_size(Db)),

    % Removing the second ddoc drops the size
    DDoc2Del = DDoc2#doc{revs = {Pos2, [Rev2]}, deleted = true},
    {ok, _} = fabric2_db:update_doc(Db, DDoc2Del, []),
    Cleanup(),
    ?assertEqual(0, view_size(Db)).


multiple_identical_design_docs(Db) ->
    Cleanup = fun() ->
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            DDocs = fabric2_db:get_design_docs(Db),
            ok = couch_views:cleanup_indices(TxDb, DDocs)
        end)
    end,

    DDoc1 = create_ddoc(simple, <<"_design/bar1">>),
    DDoc2 = create_ddoc(simple, <<"_design/bar2">>),

    % Simple test as before
    {ok, _} = fabric2_db:update_doc(Db, doc(0), []),
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, DDoc1, []),
    {ok, _} = run_query(Db, DDoc1, ?MAP_FUN1),
    ?assertEqual(5, view_size(Db)),

    % Adding a second ddoc with the same sig does not double the size
    {ok, {Pos2, Rev2}} = fabric2_db:update_doc(Db, DDoc2, []),
    {ok, _} = run_query(Db, DDoc2, ?MAP_FUN1),
    ?assertEqual(5, view_size(Db)),

    % Removing the first ddoc does not decrease the size
    DDoc1Del = DDoc1#doc{revs = {Pos1, [Rev1]}, deleted = true},
    {ok, _} = fabric2_db:update_doc(Db, DDoc1Del, []),
    Cleanup(),
    ?assertEqual(5, view_size(Db)),

    % Removing the second ddoc drops the size
    DDoc2Del = DDoc2#doc{revs = {Pos2, [Rev2]}, deleted = true},
    {ok, _} = fabric2_db:update_doc(Db, DDoc2Del, []),
    Cleanup(),
    ?assertEqual(0, view_size(Db)).


view_size(Db) ->
    {ok, Info} = fabric2_db:get_db_info(Db),
    {sizes, {Sizes}} = lists:keyfind(sizes, 1, Info),
    {<<"views">>, ViewSize} = lists:keyfind(<<"views">>, 1, Sizes),
    ViewSize.


create_ddoc() ->
    create_ddoc(simple).


create_ddoc(Type) ->
    create_ddoc(Type, <<"_design/bar">>).


create_ddoc(simple, DocId) when is_binary(DocId) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"views">>, {[
            {?MAP_FUN1, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}},
            {?MAP_FUN2, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}}
        ]}}
    ]});

create_ddoc(multi_emit_same, DocId) when is_binary(DocId) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"views">>, {[
            {?MAP_FUN1, {[
                {<<"map">>, <<"function(doc) { "
                    "emit(doc.val, doc.val * 2); "
                    "emit(doc.val, doc.val); "
                    "if(doc.extra) {"
                    "  emit(doc.val, doc.extra);"
                    "}"
                "}">>}
            ]}},
            {?MAP_FUN2, {[
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


run_query(#{} = Db, DDoc, <<_/binary>> = View) ->
    couch_views:query(Db, DDoc, View, fun fold_fun/2, [], #mrargs{}).


fold_fun({meta, _Meta}, Acc) ->
    {ok, Acc};
fold_fun({row, _} = Row, Acc) ->
    {ok, [Row | Acc]};
fold_fun(complete, Acc) ->
    {ok, lists:reverse(Acc)}.