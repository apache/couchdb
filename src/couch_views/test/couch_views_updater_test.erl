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

-module(couch_views_updater_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/test/fabric2_test.hrl").
-include_lib("mango/src/mango_idx.hrl").
-include_lib("couch_views/include/couch_views.hrl").


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
                    ?TDEF_FE(delete_doc),
                    ?TDEF_FE(includes_design_docs),
                    ?TDEF_FE(handle_erlfdb_errors, 15)
                ]
            }
        }
    }.


setup() ->
    Ctx = test_util:start_couch([
        fabric,
        couch_jobs,
        couch_js,
        couch_views,
        mango
    ]),
    Ctx.


cleanup(Ctx) ->
    test_util:stop_couch(Ctx).


foreach_setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),

    DDoc = create_idx_ddoc(),
    fabric2_db:update_docs(Db, [DDoc]),
    % make sure the index is built for the first time so the background
    % indexer doesn't build the index
    wait_while_ddoc_builds(Db),

    Docs = make_docs(3),
    fabric2_db:update_docs(Db, Docs),
    meck:new(couch_views_fdb, [passthrough]),
    {Db, DDoc}.


foreach_teardown({Db, _}) ->
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


index_docs({Db, DDoc}) ->
    Docs = run_query(Db, DDoc),
    ?assertEqual([
        [{id, <<"1">>}, {value, 1}],
        [{id, <<"2">>}, {value, 2}],
        [{id, <<"3">>}, {value, 3}]
    ], Docs).


update_doc({Db, DDoc}) ->
    {ok, Doc} = fabric2_db:open_doc(Db, <<"2">>),
    JsonDoc = couch_doc:to_json_obj(Doc, []),
    JsonDoc2 = couch_util:json_apply_field({<<"value">>, 4}, JsonDoc),
    Doc2 = couch_doc:from_json_obj(JsonDoc2),
    fabric2_db:update_doc(Db, Doc2),

    Docs = run_query(Db, DDoc),
    ?assertEqual([
        [{id, <<"1">>}, {value, 1}],
        [{id, <<"3">>}, {value, 3}],
        [{id, <<"2">>}, {value, 4}]
    ], Docs).


delete_doc({Db, DDoc}) ->
    {ok, Doc} = fabric2_db:open_doc(Db, <<"2">>),
    JsonDoc = couch_doc:to_json_obj(Doc, []),
    JsonDoc2 = couch_util:json_apply_field({<<"_deleted">>, true}, JsonDoc),
    Doc2 = couch_doc:from_json_obj(JsonDoc2),
    fabric2_db:update_doc(Db, Doc2),

    Docs = run_query(Db, DDoc),
    ?assertEqual([
        [{id, <<"1">>}, {value, 1}],
        [{id, <<"3">>}, {value, 3}]
    ], Docs).


includes_design_docs({Db, _}) ->
    DDoc = create_idx_include_ddocs(),
    fabric2_db:update_docs(Db, [DDoc]),

    IndexDDoc0 = create_idx_ddoc(),
    IndexDDoc = IndexDDoc0#doc{
        id = <<"_design/to_be_indexed">>
    },

    fabric2_db:update_docs(Db, [IndexDDoc]),

    Docs = run_query(Db, DDoc),
    ?assertEqual([
        [{id, <<"_design/ddoc_that_indexes_ddocs">>}, {value, 1}],
        [{id, <<"_design/to_be_indexed">>}, {value, 1}]
    ], Docs).


handle_erlfdb_errors({Db, _}) ->
    meck:expect(couch_views_fdb, write_doc, fun(_, _, _) ->
        error({erlfdb_error, 1009})
    end),
    ?assertError({erlfdb_error, 1009}, fabric2_db:update_docs(Db, [doc(4)])).


run_query(Db, DDoc) ->
    Args = #mrargs{
        view_type = map,
        reduce = false,
        include_docs = true,
        update = false
    },
    CB = fun query_cb/2,
    {ok, Acc} = couch_views:query(Db, DDoc, <<"idx_01">>, CB, [], Args),
    lists:map(fun ({Props}) ->
        [
            {id, couch_util:get_value(<<"_id">>, Props)},
            {value, couch_util:get_value(<<"value">>, Props, 1)}
        ]

    end, Acc).


create_idx_ddoc() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/ddoc1">>},
        {<<"language">>, <<"query">>},
        {<<"views">>, {[
            {<<"idx_01">>, {[
                {<<"map">>, {[
                    {<<"fields">>, {[{<<"value">>, <<"asc">>}]}}
                ]}},
                {<<"reduce">>, <<"_count">>},
                {<<"options">>, {[
                        {<<"def">>,
                        {[{<<"fields">>,
                            {[{<<"value">>, <<"asc">>}]}}]}}
                    ]}}
            ]}}
        ]}
        },
        {<<"autoupdate">>, false},
        {<<"options">>, {[{<<"interactive">>, true}]}}
    ]}).


create_idx_include_ddocs() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/ddoc_that_indexes_ddocs">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"idx_01">>, {[
                {<<"map">>, <<
                    "function(doc) {"
                        "if (doc.language) {"
                            "emit(doc.language, 1);"
                        "}"
                    "}">>}
            ]}}
        ]}},
        {<<"autoupdate">>, false},
        {<<"options">>, {[
            {<<"include_design">>, true},
            {<<"interactive">>, true}
        ]}}
    ]}).


wait_while_ddoc_builds(Db) ->
    Fun = fun () ->
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            Ready = lists:filter(fun (Idx) ->
                Idx#idx.build_status == ?INDEX_READY
            end, mango_idx:list(TxDb)),

            if length(Ready) > 1 -> ok; true ->
                wait
            end
        end)
    end,
    test_util:wait(Fun).



make_docs(Count) ->
    [doc(I) || I <- lists:seq(1, Count)].


doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"value">>, Id}
    ]}).


query_cb({row, Props},  Acc) ->
    Doc = couch_util:get_value(doc, Props),
    {ok, Acc ++ [Doc]};

query_cb(_, Acc) ->
    {ok, Acc}.

