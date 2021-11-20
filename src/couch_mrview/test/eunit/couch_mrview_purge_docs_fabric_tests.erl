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

-module(couch_mrview_purge_docs_fabric_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

% seconds
-define(TIMEOUT, 60).

setup_all() ->
    Ctx = test_util:start_couch([fabric, mem3]),
    meck:new(couch_mrview_index, [passthrough]),
    Ctx.

teardown_all(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

setup() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX, {q, 1}]),
    meck:reset([couch_mrview_index]),
    meck:expect(couch_mrview_index, ensure_local_purge_docs, fun(A, B) ->
        meck:passthrough([A, B])
    end),
    DbName.

teardown(DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).

view_purge_fabric_test_() ->
    {
        "Map views",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun test_purge_verify_index/1,
                    fun test_purge_hook_before_compaction/1
                ]
            }
        }
    }.

test_purge_verify_index(DbName) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            Docs1 = couch_mrview_test_util:make_docs(normal, 5),
            {ok, _} = fabric:update_docs(DbName, Docs1, [?ADMIN_CTX]),
            {ok, _} = fabric:update_doc(
                DbName,
                couch_mrview_test_util:ddoc(map),
                [?ADMIN_CTX]
            ),

            Result1 = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
            Expect1 =
                {ok, [
                    {meta, [{total, 5}, {offset, 0}]},
                    {row, [{id, <<"1">>}, {key, 1}, {value, 1}]},
                    {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
                    {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
                    {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
                    {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
                ]},
            ?assertEqual(Expect1, Result1),

            {ok, #doc{body = {Props1}}} = get_local_purge_doc(DbName),
            ?assertEqual(0, couch_util:get_value(<<"purge_seq">>, Props1)),
            ShardNames = [Sh || #shard{name = Sh} <- mem3:local_shards(DbName)],
            [ShardDbName | _Rest] = ShardNames,
            ?assertEqual(
                true,
                couch_mrview_index:verify_index_exists(
                    ShardDbName, Props1
                )
            ),

            purge_docs(DbName, [<<"1">>]),

            Result2 = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
            Expect2 =
                {ok, [
                    {meta, [{total, 4}, {offset, 0}]},
                    {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
                    {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
                    {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
                    {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
                ]},
            ?assertEqual(Expect2, Result2),

            {ok, #doc{body = {Props2}}} = get_local_purge_doc(DbName),
            ?assertEqual(1, couch_util:get_value(<<"purge_seq">>, Props2)),
            ?assertEqual(
                true,
                couch_mrview_index:verify_index_exists(
                    ShardDbName, Props2
                )
            )
        end)}.

test_purge_hook_before_compaction(DbName) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            Docs1 = couch_mrview_test_util:make_docs(normal, 5),
            {ok, _} = fabric:update_docs(DbName, Docs1, [?ADMIN_CTX]),
            {ok, _} = fabric:update_doc(
                DbName,
                couch_mrview_test_util:ddoc(map),
                [?ADMIN_CTX]
            ),

            Result1 = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
            Expect1 =
                {ok, [
                    {meta, [{total, 5}, {offset, 0}]},
                    {row, [{id, <<"1">>}, {key, 1}, {value, 1}]},
                    {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
                    {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
                    {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
                    {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
                ]},
            ?assertEqual(Expect1, Result1),

            purge_docs(DbName, [<<"1">>]),

            Result2 = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
            Expect2 =
                {ok, [
                    {meta, [{total, 4}, {offset, 0}]},
                    {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
                    {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
                    {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
                    {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
                ]},
            ?assertEqual(Expect2, Result2),

            {ok, #doc{body = {Props1}}} = get_local_purge_doc(DbName),
            ?assertEqual(1, couch_util:get_value(<<"purge_seq">>, Props1)),

            [ShardName | _] = local_shards(DbName),
            couch_util:with_db(ShardName, fun(Db) ->
                {ok, _} = couch_db:start_compact(Db)
            end),
            wait_compaction(ShardName, ?LINE),

            ?assertEqual(
                ok,
                meck:wait(
                    1,
                    couch_mrview_index,
                    ensure_local_purge_docs,
                    '_',
                    5000
                )
            ),

            % Make sure compaction didn't change the update seq
            {ok, #doc{body = {Props1}}} = get_local_purge_doc(DbName),
            ?assertEqual(1, couch_util:get_value(<<"purge_seq">>, Props1)),

            purge_docs(DbName, [<<"2">>]),

            couch_util:with_db(ShardName, fun(Db) ->
                {ok, _} = couch_db:start_compact(Db)
            end),
            wait_compaction(ShardName, ?LINE),

            ?assertEqual(
                ok,
                meck:wait(
                    2,
                    couch_mrview_index,
                    ensure_local_purge_docs,
                    '_',
                    5000
                )
            ),

            % Make sure compaction after a purge didn't overwrite
            % the local purge doc for the index
            {ok, #doc{body = {Props2}}} = get_local_purge_doc(DbName),
            ?assertEqual(1, couch_util:get_value(<<"purge_seq">>, Props2)),

            % Force another update to ensure that we update
            % the local doc appropriate after compaction
            Result3 = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
            Expect3 =
                {ok, [
                    {meta, [{total, 3}, {offset, 0}]},
                    {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
                    {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
                    {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
                ]},
            ?assertEqual(Expect3, Result3),

            {ok, #doc{body = {Props3}}} = get_local_purge_doc(DbName),
            ?assertEqual(2, couch_util:get_value(<<"purge_seq">>, Props3)),

            % Check that if the local doc doesn't exist that one
            % is created for the index on compaction
            delete_local_purge_doc(DbName),
            ?assertMatch({not_found, _}, get_local_purge_doc(DbName)),

            couch_util:with_db(ShardName, fun(Db) ->
                {ok, _} = couch_db:start_compact(Db)
            end),
            wait_compaction(ShardName, ?LINE),

            ?assertEqual(
                ok,
                meck:wait(
                    3,
                    couch_mrview_index,
                    ensure_local_purge_docs,
                    '_',
                    5000
                )
            ),

            {ok, #doc{body = {Props4}}} = get_local_purge_doc(DbName),
            ?assertEqual(2, couch_util:get_value(<<"purge_seq">>, Props4))
        end)}.

get_local_purge_doc(DbName) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/bar">>, []),
    {ok, IdxState} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
    Sig = IdxState#mrst.sig,
    HexSig = list_to_binary(couch_index_util:hexsig(Sig)),
    DocId = couch_mrview_util:get_local_purge_doc_id(HexSig),
    [ShardName | _] = local_shards(DbName),
    couch_util:with_db(ShardName, fun(Db) ->
        couch_db:open_doc(Db, DocId, [])
    end).

delete_local_purge_doc(DbName) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/bar">>, []),
    {ok, IdxState} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
    Sig = IdxState#mrst.sig,
    HexSig = list_to_binary(couch_index_util:hexsig(Sig)),
    DocId = couch_mrview_util:get_local_purge_doc_id(HexSig),
    NewDoc = #doc{id = DocId, deleted = true},
    [ShardName | _] = local_shards(DbName),
    couch_util:with_db(ShardName, fun(Db) ->
        {ok, _} = couch_db:update_doc(Db, NewDoc, [])
    end).

get_rev(#full_doc_info{} = FDI) ->
    #doc_info{
        revs = [#rev_info{} = PrevRev | _]
    } = couch_doc:to_doc_info(FDI),
    PrevRev#rev_info.rev.

purge_docs(DbName, DocIds) ->
    lists:foreach(
        fun(DocId) ->
            FDI = fabric:get_full_doc_info(DbName, DocId, []),
            Rev = get_rev(FDI),
            {ok, [{ok, _}]} = fabric:purge_docs(DbName, [{DocId, [Rev]}], [])
        end,
        DocIds
    ).

wait_compaction(DbName, Line) ->
    WaitFun = fun() ->
        case is_compaction_running(DbName) of
            true -> wait;
            false -> ok
        end
    end,
    case test_util:wait(WaitFun, 10000) of
        timeout ->
            erlang:error(
                {assertion_failed, [
                    {module, ?MODULE},
                    {line, Line},
                    {reason, "Timeout waiting for database compaction"}
                ]}
            );
        _ ->
            ok
    end.

is_compaction_running(DbName) ->
    {ok, DbInfo} = couch_util:with_db(DbName, fun(Db) ->
        couch_db:get_db_info(Db)
    end),
    couch_util:get_value(compact_running, DbInfo).

local_shards(DbName) ->
    try
        [ShardName || #shard{name = ShardName} <- mem3:local_shards(DbName)]
    catch
        error:database_does_not_exist ->
            []
    end.
