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
-include_lib("mem3/include/mem3.hrl").

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
            ?TDEF_FE(t_cleanup_index_file_after_ddoc_delete),
            ?TDEF_FE(t_cleanup_empty_view_checkpoints),
            ?TDEF_FE(t_cleanup_disallowed_language_checkpoints),
            ?TDEF_FE(t_cleanup_index_files_with_node_down),
            ?TDEF_FE(t_cleanup_search_indexes_with_node_down)
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

t_cleanup_empty_view_checkpoints({_, DbName}) ->
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

    update_empty_ddoc(DbName, <<"_design/foo">>),
    {ok, _} = fabric:get_view_group_info(DbName, <<"foo">>),
    ok = fabric:cleanup_index_files_all_nodes(DbName),

    % One 4bc stays, da8 should gone. If it weren't for the check to
    % empty views it would have used signature "3e823c2a4383ac0c18d4e574135a5b08"
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
    ).

t_cleanup_disallowed_language_checkpoints({_, DbName}) ->
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

    update_invalid_language(DbName, <<"_design/foo">>, <<"bar">>),
    {ok, _} = fabric:get_view_group_info(DbName, <<"foo">>),
    ok = fabric:cleanup_index_files_all_nodes(DbName),

    % One 4bc stays, the new view uses an invalid language which can't index with
    % so we don't create a checkpoint for it
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
    ).

t_cleanup_index_files_with_node_down({_, DbName}) ->
    FakeNode = 'bogus@nohost',
    mock_node_down(DbName, FakeNode),
    meck:new(mem3_util, [passthrough]),
    meck:expect(mem3_util, live_nodes, fun() -> meck:passthrough([]) ++ [FakeNode] end),
    ?assertEqual(ok, fabric:cleanup_index_files_all_nodes(DbName)),
    ErpcError = {error, {erpc, noconnection}},
    lists:foreach(
        fun({M, F}) ->
            Label = {FakeNode, M, F},
            Args = [fabric_index_cleanup, DbName, Label, ErpcError],
            ?assert(meck:called(couch_log, error, ['_', Args]))
        end,
        [
            {couch_mrview_cleanup, cleanup},
            {dreyfus_fabric_cleanup, go_local},
            {nouveau_fabric_cleanup, go_local}
        ]
    ).

t_cleanup_search_indexes_with_node_down({_, DbName}) ->
    FakeNode = 'bogus@nohost',
    mock_node_down(DbName, FakeNode),
    ErpcError = {error, {erpc, noconnection}},
    ?assertEqual(ok, dreyfus_fabric_cleanup:go(DbName)),
    Args1 = [dreyfus_fabric_cleanup, DbName, FakeNode, ErpcError],
    ?assert(meck:called(couch_log, error, ['_', Args1])),
    ?assertEqual(ok, nouveau_fabric_cleanup:go(DbName)),
    Args2 = [nouveau_fabric_cleanup, DbName, FakeNode, ErpcError],
    ?assert(meck:called(couch_log, error, ['_', Args2])).

% Jump through hoops to pretend we have a node down. We mock DbName's shard map
% to return an extra shard copy on a bogus down node.
%
mock_node_down(DbName, FakeNode) ->
    DDocRes = fabric_util:get_design_doc_records(DbName),
    meck:new(fabric_util, [passthrough]),
    meck:expect(fabric_util, get_design_doc_records, fun(Db) ->
        case Db =:= DbName of
            true -> DDocRes;
            false -> meck:passthrough([Db])
        end
    end),
    meck:new(mem3, [passthrough]),
    meck:expect(mem3, shards, fun(Db) ->
        Shards = meck:passthrough([Db]),
        case Db =:= DbName of
            true -> Shards ++ [(hd(Shards))#shard{node = FakeNode}];
            false -> Shards
        end
    end),
    meck:new(couch_log, [passthrough]),
    meck:expect(couch_log, error, 2, ok).

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

update_empty_ddoc(DbName, DDocId) ->
    {ok, DDoc0} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    DDoc = DDoc0#doc{body = {[]}},
    fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]).

update_invalid_language(DbName, DDocId, ViewName) ->
    {ok, DDoc0} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    DDoc = DDoc0#doc{
        body =
            {[
                {<<"language">>, <<"cobol">>},
                {<<"views">>,
                    {[
                        {ViewName,
                            {[
                                {<<"map">>, <<"MAIN-PROCEDURE. PERFORM MAP">>}
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
    ok =
        case clear_shards_db() of
            ok -> ok;
            not_found -> ok
        end,
    Ctx.

teardown_fabric(Ctx) ->
    ok = clear_shards_db(),
    test_util:stop_couch(Ctx).

clear_shards_db() ->
    ShardsDb = ?l2b(config:get("mem3", "shards_db", "_dbs")),
    couch_server:delete(ShardsDb, [?ADMIN_CTX]).

index_cleanup_recv_test_() ->
    {
        foreach,
        fun() -> meck:expect(couch_log, error, 2, ok) end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_recv_no_requests),
            ?TDEF_FE(t_recv_ok_responses),
            ?TDEF_FE(t_recv_logs_non_ok_responses),
            ?TDEF_FE(t_recv_handles_noconnection),
            ?TDEF_FE(t_recv_handles_remote_exceptions),
            ?TDEF_FE(t_recv_handles_timeout)
        ]
    }.

t_recv_no_requests(_) ->
    ?assertEqual(ok, recv(erpc:reqids_new(), 5000)),
    ?assertEqual(0, meck:num_calls(couch_log, error, 2)).

t_recv_ok_responses(_) ->
    Reqs0 = erpc:reqids_new(),
    Reqs1 = send_fun(node(), req1, fun() -> ok end, Reqs0),
    Reqs2 = send_fun(node(), req2, fun() -> ok end, Reqs1),
    ?assertEqual(ok, recv(Reqs2, 5000)),
    ?assertEqual(0, meck:num_calls(couch_log, error, 2)).

t_recv_logs_non_ok_responses(_) ->
    Reqs1 = send_fun(node(), req1, fun() -> {error, potato} end, erpc:reqids_new()),
    ?assertEqual(ok, recv(Reqs1, 5000)),
    ?assert(meck:called(couch_log, error, ['_', ['_', '_', req1, {error, potato}]])).

t_recv_handles_noconnection(_) ->
    Self = self(),
    Reqs0 = erpc:reqids_new(),
    Reqs1 = erpc:send_request('bogus@totallybogus', erlang, node, [], down_node, Reqs0),
    Reqs2 = send_fun(
        node(),
        req2,
        fun() ->
            Self ! req2_ran,
            ok
        end,
        Reqs1
    ),
    ?assertEqual(ok, recv(Reqs2, 5000)),
    % Log the bogus one but keep going otherwise
    NoConn = {error, {erpc, noconnection}},
    ?assert(meck:called(couch_log, error, ['_', ['_', '_', down_node, NoConn]])),
    ?assertEqual(1, meck:num_calls(couch_log, error, 2)),
    receive
        req2_ran -> ok
    end.

t_recv_handles_remote_exceptions(_) ->
    % A variety of failures on the other side: exits, errors and throws
    Reqs0 = erpc:reqids_new(),
    Reqs1 = send_fun(node(), err_req, fun() -> error(potato) end, Reqs0),
    Reqs2 = send_fun(node(), throw_req, fun() -> throw(potato) end, Reqs1),
    Reqs3 = send_fun(node(), exit_req, fun() -> exit(potato) end, Reqs2),
    Reqs4 = send_fun(node(), ok_req, fun() -> ok end, Reqs3),
    ?assertEqual(ok, recv(Reqs4, 5000)),
    ?assert(
        meck:called(couch_log, error, ['_', ['_', '_', err_req, {error, {exception, potato, '_'}}]])
    ),
    ?assert(meck:called(couch_log, error, ['_', ['_', '_', throw_req, {throw, potato}]])),
    ?assert(
        meck:called(couch_log, error, ['_', ['_', '_', exit_req, {exit, {exception, potato}}]])
    ),
    ?assertEqual(3, meck:num_calls(couch_log, error, 2)).

t_recv_handles_timeout(_) ->
    Reqs1 = send_fun(node(), slow_req, fun() -> timer:sleep(10000) end, erpc:reqids_new()),
    ?assertEqual(ok, recv(Reqs1, 100)),
    % Test the global timeout
    ?assert(meck:called(couch_log, error, ['_', ['_', '_', [slow_req], timeout]])).

recv(Reqs, TimeoutMSec) ->
    % Note: this is a standard erpc format see https://www.erlang.org/doc/apps/kernel/erpc.html
    Timeout = {abs, erlang:monotonic_time(millisecond) + TimeoutMSec},
    fabric_index_cleanup:recv(?MODULE, <<"db">>, Reqs, Timeout).

send_fun(Node, Label, Fun, Reqs) ->
    erpc:send_request(Node, erlang, apply, [Fun, []], Label, Reqs).
