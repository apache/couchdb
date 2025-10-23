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

-module(fabric_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

cleanup_index_files_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_cleanup_index_files),
            ?TDEF_FE(t_cleanup_index_files_with_existing_db),
            ?TDEF_FE(t_cleanup_index_files_with_view_data),
            ?TDEF_FE(t_cleanup_index_files_with_deleted_db),
            ?TDEF_FE(t_cleanup_index_file_after_ddoc_update),
            ?TDEF_FE(t_cleanup_index_file_after_ddoc_delete)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    fabric:create_db(DbName, [{q, 1}]),
    create_ddoc(DbName, <<"_design/foo">>, <<"bar">>),
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar">>),
    create_ddoc(DbName, <<"_design/boo">>, <<"baz">>),
    {ok, _} = fabric:query_view(DbName, <<"boo">>, <<"baz">>),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    meck:unload(),
    fabric:delete_db(DbName),
    test_util:stop_couch(Ctx).

t_cleanup_index_files(_) ->
    ?assertEqual(ok, fabric:cleanup_index_files_this_node()),
    ?assertEqual(ok, fabric:cleanup_index_files_all_nodes()).

t_cleanup_index_files_with_existing_db({_, DbName}) ->
    ?assertEqual(ok, fabric:cleanup_index_files_this_node(DbName)),
    ?assertEqual(ok, fabric:cleanup_index_files_all_nodes(DbName)),
    ?assertEqual(ok, fabric:cleanup_index_files_this_node(<<"non_existent">>)),
    ?assertEqual(ok, fabric:cleanup_index_files_all_nodes(<<"non_existent">>)).

t_cleanup_index_files_with_view_data({_, DbName}) ->
    Sigs = sigs(DbName),
    Indices = indices(DbName),
    Purges = purges(DbName),
    ok = fabric:cleanup_index_files_all_nodes(DbName),
    % We haven't inadvertently removed any active index bits
    ?assertEqual(Sigs, sigs(DbName)),
    ?assertEqual(Indices, indices(DbName)),
    ?assertEqual(Purges, purges(DbName)).

t_cleanup_index_files_with_deleted_db(_) ->
    SomeDb = ?tempdb(),
    ?assertEqual(ok, fabric:cleanup_index_files_all_nodes(SomeDb)).

t_cleanup_index_file_after_ddoc_update({_, DbName}) ->
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view",
            "da817c3d3f7413c1a610f25635a0c521.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>,
            <<"_local/purge-mrview-da817c3d3f7413c1a610f25635a0c521">>
        ],
        purges(DbName)
    ),

    update_ddoc(DbName, <<"_design/foo">>, <<"bar1">>),
    ok = fabric:cleanup_index_files_all_nodes(DbName),
    {ok, _} = fabric:query_view(DbName, <<"foo">>, <<"bar1">>),

    % One 4bc stays, da8 should  gone and 9e3 is added
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view",
            "9e355b0fee411b4257036b8fca56f263.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>,
            <<"_local/purge-mrview-9e355b0fee411b4257036b8fca56f263">>
        ],
        purges(DbName)
    ).

t_cleanup_index_file_after_ddoc_delete({_, DbName}) ->
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view",
            "da817c3d3f7413c1a610f25635a0c521.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>,
            <<"_local/purge-mrview-da817c3d3f7413c1a610f25635a0c521">>
        ],
        purges(DbName)
    ),

    delete_ddoc(DbName, <<"_design/foo">>),
    ok = fabric:cleanup_index_files_all_nodes(DbName),

    % 4bc stays the same, da8 should be gone
    ?assertEqual(
        [
            "4bcdf852098ff6b0578ddf472c320e9c.view"
        ],
        indices(DbName)
    ),
    ?assertEqual(
        [
            <<"_local/purge-mrview-4bcdf852098ff6b0578ddf472c320e9c">>
        ],
        purges(DbName)
    ),

    delete_ddoc(DbName, <<"_design/boo">>),
    ok = fabric:cleanup_index_files_all_nodes(DbName),

    ?assertEqual([], indices(DbName)),
    ?assertEqual([], purges(DbName)),

    % cleaning a db with all deleted indices should still work
    ok = fabric:cleanup_index_files_all_nodes(DbName),

    ?assertEqual([], indices(DbName)),
    ?assertEqual([], purges(DbName)).

shard_names(DbName) ->
    [mem3:name(S) || S <- mem3:local_shards(DbName)].

% Sorted list of sigs
%
sigs(DbName) ->
    case shard_names(DbName) of
        [] ->
            [];
        [SomeDb | _] ->
            Sigs = couch_mrview_util:get_signatures(SomeDb),
            lists:sort(maps:keys(Sigs))
    end.

% Sorted list of index files
%
indices(DbName) ->
    case shard_names(DbName) of
        [] ->
            [];
        [_ | _] = Dbs ->
            AllIndices = lists:map(fun couch_mrview_util:get_index_files/1, Dbs),
            AsList = lists:sort(
                lists:foldl(
                    fun(Indices, Acc) ->
                        maps:values(Indices) ++ Acc
                    end,
                    [],
                    AllIndices
                )
            ),
            % Keep only file names and extensions. Since we use q=1, we shouldn't
            % have any duplicates
            [filename:basename(F) || F <- AsList]
    end.

% Sorted list of purge checkpoint doc ids
%
purges(DbName) ->
    case shard_names(DbName) of
        [] ->
            [];
        [_ | _] = Dbs ->
            AllPurges = lists:map(fun couch_mrview_util:get_purge_checkpoints/1, Dbs),
            lists:sort(
                lists:foldl(
                    fun(Purges, Acc) ->
                        maps:values(Purges) ++ Acc
                    end,
                    [],
                    AllPurges
                )
            )
    end.

create_ddoc(DbName, DDocId, ViewName) ->
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"javascript">>},
            {<<"views">>,
                {[
                    {ViewName,
                        {[
                            {<<"map">>, <<"function(doc) { emit(doc.value, null); }">>}
                        ]}}
                ]}}
        ]}
    ),
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

update_ddoc(DbName, DDocId, ViewName) ->
    {ok, DDoc0} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    DDoc = DDoc0#doc{
        body =
            {[
                {<<"language">>, <<"javascript">>},
                {<<"views">>,
                    {[
                        {ViewName,
                            {[
                                {<<"map">>, <<"function(doc) { emit(doc.value, 1); }">>}
                            ]}}
                    ]}}
            ]}
    },
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

delete_ddoc(DbName, DDocId) ->
    {ok, DDoc0} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    DDoc = DDoc0#doc{deleted = true, body = {[]}},
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

design_docs_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_design_docs_configuration),
            ?TDEF_FE(t_design_docs_configuration_io_priority)
        ]
    }.

t_design_docs_configuration(_) ->
    DbName = <<"db">>,
    AdminCtx = [?ADMIN_CTX],
    QueryArgs =
        #mrargs{
            include_docs = true,
            extra = [{namespace, <<"_design">>}, {view_row_map, true}]
        },
    meck:expect(
        fabric, all_docs, [DbName, AdminCtx, '_', [], QueryArgs], meck:val(all_docs_result)
    ),
    ?assertEqual(all_docs_result, fabric:design_docs(DbName)).

t_design_docs_configuration_io_priority(_) ->
    DbName = <<"db">>,
    AdminCtx = [?ADMIN_CTX],
    QueryArgs =
        #mrargs{
            include_docs = true,
            extra = [{namespace, <<"_design">>}, {io_priority, io_priority}, {view_row_map, true}]
        },
    meck:expect(
        fabric, all_docs, [DbName, AdminCtx, '_', [], QueryArgs], meck:val(all_docs_result)
    ),
    put(io_priority, io_priority),
    ?assertEqual(all_docs_result, fabric:design_docs(DbName)).

query_view_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_query_view_configuration),
            ?TDEF_FE(t_query_all_docs)
        ]
    }.

t_query_view_configuration({_Ctx, DbName}) ->
    DDocName = <<"foo">>,
    ViewName = <<"bar">>,
    QueryArgs =
        #mrargs{
            view_type = map,
            start_key_docid = <<>>,
            end_key_docid = <<255>>,
            extra = [{validated, true}, {view_row_map, true}]
        },
    Options = [],
    Accumulator = [],
    Parameters = [DbName, Options, '_', ViewName, QueryArgs, '_', Accumulator, '_'],
    meck:expect(fabric_view_map, go, Parameters, meck:val(fabric_view_map_results)),
    ?assertEqual(fabric_view_map_results, fabric:query_view(DbName, DDocName, ViewName)).

t_query_all_docs({_Ctx, DbName}) ->
    Cbk = fun
        ({meta, _}, Acc) -> {ok, Acc};
        ({row, Row}, Acc) -> {ok, [Row | Acc]};
        (complete, Acc) -> {ok, Acc}
    end,
    {ok, Rows} = fabric:all_docs(binary_to_list(DbName), Cbk, [], [{limit, 2}]),
    ?assertMatch(
        [
            [{id, <<"_design/foo">>} | _],
            [{id, <<"_design/boo">>} | _]
        ],
        Rows
    ).

fabric_all_dbs_test_() ->
    {
        foreach,
        fun setup_fabric/0,
        fun teardown_fabric/1,
        [
            ?TDEF_FE(t_get_all_dbs),
            ?TDEF_FE(t_prefix_works)
        ]
    }.

t_get_all_dbs(_) ->
    DbList = [<<"aaa">>, <<"a+b">>, <<"a$c">>, <<"aaa/bbb">>],
    ?assertEqual(ok, create_dbs(DbList)),
    ExpectList = lists:sort(DbList),
    ?assertEqual({ok, ExpectList}, fabric:all_dbs()),
    ?assertEqual(ok, delete_dbs(DbList)).

t_prefix_works(_) ->
    DbList = [
        "aaa0",
        "aaa+",
        "aaa(",
        "aa",
        "a",
        "aaaa",
        "aaa/y",
        "aaa/x",
        "aaa/x$",
        "aaa/x/z",
        "aaa$"
    ],
    ?assertEqual(ok, create_dbs(DbList)),
    AllExpect = lists:sort([list_to_binary(Db) || Db <- DbList]),
    ?assertEqual({ok, AllExpect}, fabric:all_dbs()),
    ?assertEqual({ok, AllExpect}, fabric:all_dbs(<<>>)),
    ?assertEqual({ok, []}, fabric:all_dbs(<<"$">>)),
    ?assertEqual({ok, []}, fabric:all_dbs(<<"b">>)),
    ?assertEqual({ok, AllExpect}, fabric:all_dbs(<<"a">>)),
    ?assertEqual({ok, AllExpect -- [<<"a">>, <<"aa">>]}, fabric:all_dbs(<<"aaa">>)),
    ?assertEqual({ok, [<<"aaa0">>]}, fabric:all_dbs(<<"aaa0">>)),
    ?assertEqual({ok, [<<"aaa+">>]}, fabric:all_dbs(<<"aaa+">>)),
    ?assertEqual({ok, [<<"aaa(">>]}, fabric:all_dbs(<<"aaa(">>)),
    ?assertEqual({ok, [<<"aaaa">>]}, fabric:all_dbs(<<"aaaa">>)),
    ?assertEqual({ok, [<<"aaa/x$">>]}, fabric:all_dbs(<<"aaa/x$">>)),
    ?assertEqual({ok, [<<"aaa/x">>, <<"aaa/x$">>, <<"aaa/x/z">>]}, fabric:all_dbs(<<"aaa/x">>)),
    ?assertEqual({ok, [<<"aaa/x/z">>]}, fabric:all_dbs(<<"aaa/x/z">>)),
    ?assertEqual({ok, [<<"aaa/x/z">>]}, fabric:all_dbs(<<"aaa/x/">>)),
    TripleASlash = lists:sort([<<"aaa/x">>, <<"aaa/y">>, <<"aaa/x$">>, <<"aaa/x/z">>]),
    ?assertEqual({ok, TripleASlash}, fabric:all_dbs(<<"aaa/">>)),
    ?assertEqual(ok, delete_dbs(DbList)).

create_dbs(DbList) ->
    Fun = fun(DbName) -> ok = fabric:create_db(DbName) end,
    lists:foreach(Fun, DbList).

delete_dbs(DbList) ->
    Fun = fun(DbName) -> ok = fabric:delete_db(DbName) end,
    lists:foreach(Fun, DbList).

setup_fabric() ->
    Ctx = test_util:start_couch([fabric]),
    ok = clear_shards_db(),
    Ctx.

teardown_fabric(Ctx) ->
    ok = clear_shards_db(),
    test_util:stop_couch(Ctx).

clear_shards_db() ->
    ShardsDb = ?l2b(config:get("mem3", "shards_db", "_dbs")),
    couch_server:delete(ShardsDb, [?ADMIN_CTX]).
