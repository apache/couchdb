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

-module(couch_views_upgrade_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_views/include/couch_views.hrl").
-include_lib("fabric/include/fabric2.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

-define(MAP_FUN1, <<"map_fun1">>).
-define(MAP_FUN2, <<"map_fun2">>).

upgrade_test_() ->
    {
        "Test view upgrades",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                [
                    ?TDEF_FE(empty_state),
                    ?TDEF_FE(indexed_state),
                    ?TDEF_FE(upgrade_non_interactive),
                    ?TDEF_FE(upgrade_unbuilt_interactive),
                    ?TDEF_FE(upgrade_partially_built_interactive),
                    ?TDEF_FE(upgrade_built_interactive)
                ]
            }
        }
    }.

setup() ->
    Ctx = test_util:start_couch([
        fabric,
        couch_jobs,
        js_engine,
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
    config:delete("couch_views", "change_limit"),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).

empty_state(Db) ->
    DDoc = create_ddoc(),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(fabric2_db:name(Db), DDoc),
    State = fabric2_fdb:transactional(Db, fun(TxDb) ->
        couch_views_fdb:get_view_state(TxDb, Mrst)
    end),

    Expect = #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => <<>>,
        view_vs => not_found,
        build_status => not_found
    },
    ?assertEqual(Expect, State),
    assert_fdb_state(Db, Mrst, Expect).

indexed_state(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc1, []),

    {ok, Out} = run_query(Db, DDoc, ?MAP_FUN1),
    ?assertEqual([row(<<"0">>, 0, 0)], Out),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => fabric2_db:get_update_seq(Db),
        view_vs => not_found,
        build_status => not_found
    }).

upgrade_non_interactive(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_docs(Db, [DDoc, Doc1], []),
    DbSeq = fabric2_db:get_update_seq(Db),

    init_fdb_state(Db, DDoc, #{view_seq => DbSeq}),

    {ok, Out} = run_query(Db, DDoc, ?MAP_FUN1),
    ?assertEqual([row(<<"0">>, 0, 0)], Out),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => DbSeq,
        view_vs => not_found,
        build_status => not_found
    }).

upgrade_unbuilt_interactive(Db) ->
    DDoc = create_ddoc(),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(fabric2_db:name(Db), DDoc),
    Doc1 = doc(0),

    {ok, _} = fabric2_db:update_docs(Db, [DDoc, Doc1], []),
    DbSeq = fabric2_db:get_update_seq(Db),

    init_fdb_state(Db, DDoc, #{
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_BUILDING
    }),

    % Trigger an upgrade
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        couch_views_fdb:get_view_state(TxDb, Mrst)
    end),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => <<>>,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_BUILDING
    }),

    % Build the view
    {ok, Out} = run_query(Db, DDoc, ?MAP_FUN1),
    ?assertEqual([row(<<"0">>, 0, 0)], Out),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => DbSeq,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_READY
    }).

upgrade_partially_built_interactive(Db) ->
    DDoc = create_ddoc(),
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(fabric2_db:name(Db), DDoc),
    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),

    MidSeq = fabric2_db:get_update_seq(Db),

    Doc1 = doc(0),
    {ok, _} = fabric2_db:update_doc(Db, Doc1, []),

    DbSeq = fabric2_db:get_update_seq(Db),

    init_fdb_state(Db, DDoc, #{
        view_seq => MidSeq,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_BUILDING
    }),

    % Trigger an upgrade
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        couch_views_fdb:get_view_state(TxDb, Mrst)
    end),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => <<>>,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_BUILDING
    }),

    % Build the view
    {ok, Out} = run_query(Db, DDoc, ?MAP_FUN1),
    ?assertEqual([row(<<"0">>, 0, 0)], Out),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => DbSeq,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_READY
    }).

upgrade_built_interactive(Db) ->
    DDoc = create_ddoc(),
    Doc1 = doc(0),

    {ok, Mrst} = couch_views_util:ddoc_to_mrst(fabric2_db:name(Db), DDoc),
    {ok, _} = fabric2_db:update_doc(Db, DDoc, []),
    {ok, _} = fabric2_db:update_doc(Db, Doc1, []),

    DbSeq = fabric2_db:get_update_seq(Db),

    init_fdb_state(Db, DDoc, #{
        view_seq => DbSeq,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_READY
    }),

    % Trigger an upgrade
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        couch_views_fdb:get_view_state(TxDb, Mrst)
    end),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => <<>>,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_BUILDING
    }),

    % Build the view
    {ok, Out} = run_query(Db, DDoc, ?MAP_FUN1),
    ?assertEqual([row(<<"0">>, 0, 0)], Out),

    assert_fdb_state(Db, DDoc, #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => DbSeq,
        view_vs => fabric2_fdb:seq_to_vs(DbSeq),
        build_status => ?INDEX_READY
    }).

init_fdb_state(Db, #doc{} = DDoc, Values) ->
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(fabric2_db:name(Db), DDoc),
    init_fdb_state(Db, Mrst, Values);
init_fdb_state(Db, #mrst{sig = Sig}, Values) ->
    init_fdb_state(Db, Sig, Values);
init_fdb_state(Db, Sig, Values) ->
    VersionRow =
        case maps:get(version, Values, undefined) of
            undefined -> [];
            Version -> [{pack(Db, key(version, Sig)), pack({Version})}]
        end,

    SeqRow =
        case maps:get(view_seq, Values, undefined) of
            undefined -> [];
            Seq -> [{pack(Db, key(seq, Sig)), Seq}]
        end,

    VSRow =
        case maps:get(view_vs, Values, undefined) of
            undefined -> [];
            VS -> [{pack(Db, key(vs, Sig)), pack({VS})}]
        end,

    BSRow =
        case maps:get(build_status, Values, undefined) of
            undefined -> [];
            BS -> [{pack(Db, key(bs, Sig)), BS}]
        end,

    Rows = VersionRow ++ SeqRow ++ VSRow ++ BSRow,

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx
        } = TxDb,
        lists:foreach(
            fun({K, V}) ->
                erlfdb:set(Tx, K, V)
            end,
            Rows
        )
    end).

assert_fdb_state(Db, #doc{} = DDoc, Expect) ->
    {ok, Mrst} = couch_views_util:ddoc_to_mrst(fabric2_db:name(Db), DDoc),
    assert_fdb_state(Db, Mrst, Expect);
assert_fdb_state(Db, #mrst{sig = Sig}, Expect) ->
    assert_fdb_state(Db, Sig, Expect);
assert_fdb_state(Db, Sig, Expect) ->
    #{
        version := Version,
        view_seq := ViewSeq,
        view_vs := ViewVS,
        build_status := BuildStatus
    } = Expect,

    VersionRow =
        case Version of
            not_found -> [];
            _ -> [{pack(Db, key(version, Sig)), pack({Version})}]
        end,

    SeqRow =
        case ViewSeq of
            <<>> -> [];
            _ -> [{pack(Db, key(seq, Sig)), ViewSeq}]
        end,

    VSRow =
        case ViewVS of
            not_found -> [];
            _ -> [{pack(Db, key(vs, Sig)), pack({ViewVS})}]
        end,

    BSRow =
        case BuildStatus of
            not_found -> [];
            _ -> [{pack(Db, key(bs, Sig)), BuildStatus}]
        end,

    ExpectRows = lists:sort(VersionRow ++ SeqRow ++ VSRow ++ BSRow),

    RawExistingRows = fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx,
            db_prefix := DbPrefix
        } = TxDb,
        RangePrefix = erlfdb_tuple:pack({?DB_VIEWS, ?VIEW_INFO}, DbPrefix),
        erlfdb:wait(erlfdb:get_range_startswith(Tx, RangePrefix))
    end),

    % Ignore the KV size key in the view info rows
    KVSizeKey = pack(Db, key(kv_size, Sig)),
    ExistingRows = lists:keydelete(KVSizeKey, 1, RawExistingRows),

    ?assertEqual(ExpectRows, ExistingRows).

key(version, Sig) -> {?DB_VIEWS, ?VIEW_INFO, ?VIEW_IMPL_VERSION, Sig};
key(seq, Sig) -> {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Sig};
key(kv_size, Sig) -> {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Sig};
key(vs, Sig) -> {?DB_VIEWS, ?VIEW_INFO, ?VIEW_CREATION_VS, Sig};
key(bs, Sig) -> {?DB_VIEWS, ?VIEW_INFO, ?VIEW_BUILD_STATUS, Sig}.

pack(Db, Key) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    erlfdb_tuple:pack(Key, DbPrefix).

pack(Value) ->
    erlfdb_tuple:pack(Value).

row(Id, Key, Value) ->
    {row, [
        {id, Id},
        {key, Key},
        {value, Value}
    ]}.

fold_fun({meta, _Meta}, Acc) ->
    {ok, Acc};
fold_fun({row, _} = Row, Acc) ->
    {ok, [Row | Acc]};
fold_fun(complete, Acc) ->
    {ok, lists:reverse(Acc)}.

create_ddoc() ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/bar">>},
            {<<"views">>,
                {[
                    {?MAP_FUN1,
                        {[
                            {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
                        ]}},
                    {?MAP_FUN2,
                        {[
                            {<<"map">>, <<"function(doc) {}">>}
                        ]}}
                ]}}
        ]}
    ).

doc(Id) ->
    doc(Id, Id).

doc(Id, Val) ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, list_to_binary(integer_to_list(Id))},
            {<<"val">>, Val}
        ]}
    ).

run_query(#{} = Db, DDoc, <<_/binary>> = View) ->
    couch_views:query(Db, DDoc, View, fun fold_fun/2, [], #mrargs{}).
