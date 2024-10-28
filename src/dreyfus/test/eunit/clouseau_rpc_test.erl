% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(clouseau_rpc_test).

-ifdef(WITH_CLOUSEAU).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(ADM_AUTH, {basic_auth, {"adm", "pass"}}).
-define(JSON, {"Content-Type", "application/json"}).

-define(HOST, "127.0.0.1").
-define(PORT, "15984").
-define(NODE, list_to_atom("node1@" ++ ?HOST)).
-define(TEST_NODE, list_to_atom("test@" ++ ?HOST)).

-define(DOCS_NUM, 4).
-define(SEARCH, #{
    <<"_id">> => <<"_design/ddoc">>,
    <<"indexes">> => #{
        <<"val_idx">> => #{
            <<"index">> =>
                <<"function(doc) { if(doc.val) {index(\"val\", doc.val, {\"store\": true});}}">>
        }
    }
}).

-define(TIMEOUT_IN_MS, 3000).

-record(arg, {
    % cleanup/1
    dbName = undefined,
    cleanup1 = undefined,
    % cleanup/2
    activeSigs = undefined,
    cleanup2 = undefined,
    % rename/1
    rename = undefined,

    % open_index/3
    peer = undefined,
    path = undefined,
    analyzer = undefined,
    pid = undefined,
    % get_update_seq/1
    ref = undefined,
    get_update_seq = undefined,
    % set_purge_seq/2
    seq = undefined,
    set_purge_seq = undefined,
    % get_purge_seq/1
    get_purge_seq = undefined,
    % update/3
    id = undefined,
    fields = undefined,
    update = undefined,
    % delete/2
    delete = undefined,
    % commit/2
    commit = undefined,

    % info/1
    info = undefined,
    % search/2
    args = undefined,
    search = undefined,
    % group1/7
    query = undefined,
    groupBy = undefined,
    refresh = undefined,
    sort = undefined,
    offset = undefined,
    limit = undefined,
    group1 = undefined,
    % group2/2
    group2 = undefined,

    called = false
}).

clouseau_rpc_simple_test_() ->
    {"clouseau rpc simeple tests",
        {setup, fun start/0, fun stop/1,
            with([
                ?TDEF(t_analyze),
                ?TDEF(t_get_root_dir),
                ?TDEF(t_version)
            ])}}.

clouseau_rpc_gen_server_cast_test_() ->
    {"clouseau rpc gen_server:cast tests",
        {setup, fun start/0, fun stop/1,
            with([
                ?TDEF(t_cleanup1),
                ?TDEF(t_cleanup2),
                ?TDEF(t_rename)
            ])}}.

clouseau_rpc_without_docs_test_() ->
    {"clouseau rpc tests without docs",
        {setup, fun start/0, fun stop/1,
            with([
                ?TDEF(t_disk_size),

                ?TDEF(t_open_index),
                ?TDEF(t_get_update_seq),
                ?TDEF(t_set_purge_seq),
                ?TDEF(t_get_purge_seq),
                ?TDEF(t_update),
                ?TDEF(t_delete),
                ?TDEF(t_commit),

                ?TDEF(t_info),
                ?TDEF(t_search),
                ?TDEF(t_group)
            ])}}.

clouseau_rpc_with_docs_test_() ->
    {"clouseau rpc tests with docs",
        {foreach, fun start_with_docs/0, fun stop/1, [
            ?TDEF_FE(t_disk_size_with_docs),

            ?TDEF_FE(t_get_update_seq_with_docs),
            ?TDEF_FE(t_set_purge_seq_with_docs),
            ?TDEF_FE(t_get_purge_seq_with_docs),
            ?TDEF_FE(t_update_with_docs),
            ?TDEF_FE(t_delete_with_docs),
            ?TDEF_FE(t_commit_with_docs),

            ?TDEF_FE(t_info_with_docs),
            ?TDEF_FE(t_search_with_docs),
            ?TDEF_FE(t_group_with_docs)
        ]}}.

t_analyze(_) ->
    Text1 = <<"ablanks">>,
    Text2 = <<"renovations.com">>,
    Text = <<Text1/binary, "@", Text2/binary>>,
    TokensStandard = rpc:call(?NODE, clouseau_rpc, analyze, [<<"standard">>, Text]),
    ?assertEqual({ok, [Text1, Text2]}, TokensStandard),
    TokensKeyword = rpc:call(?NODE, clouseau_rpc, analyze, [<<"keyword">>, Text]),
    ?assertEqual({ok, [Text]}, TokensKeyword).

t_get_root_dir(_) ->
    {ok, RootDir} = rpc:call(?NODE, clouseau_rpc, get_root_dir, []),
    ?assertNotEqual(nomatch, binary:match(RootDir, <<"clouseau">>)).

t_version(_) ->
    Version = rpc:call(?NODE, clouseau_rpc, version, []),
    ?assertMatch({ok, _}, Version).

t_cleanup1(_) ->
    {Tid, Db} = setup_with_ddoc(cleanup, 1),
    Args = get_args(Tid),
    ?assertNotEqual(nomatch, binary:match(Args#arg.dbName, ?l2b(Db))),
    ?assertEqual(ok, Args#arg.cleanup1),
    ?assertEqual(undefined, Args#arg.cleanup2),
    teardown(Tid, Db).

t_cleanup2(_) ->
    {Tid, Db} = setup_with_ddoc(cleanup, 2),
    Res = req(post, url(Db, "_search_cleanup")),
    Args = get_args(Tid),
    ?assertMatch({202, #{<<"ok">> := true}}, Res),
    ?assertNotEqual(nomatch, binary:match(Args#arg.dbName, ?l2b(Db))),
    ?assertEqual(ok, Args#arg.cleanup2),
    ?assertEqual(undefined, Args#arg.cleanup1),
    teardown(Tid, Db).

t_rename(_) ->
    {Tid, Db} = setup_with_ddoc(rename, 1),
    Res = rpc:call(?NODE, clouseau_rpc, rename, [Db]),
    ?assertEqual(ok, Res),
    teardown(Tid, Db).

t_disk_size(_) ->
    {Tid, Db} = setup_with_ddoc(open_index, 3),
    t_disk_size_with_docs({Tid, Db}).

t_disk_size_with_docs({Tid, Db}) ->
    Args = get_args(Tid),
    Res1 = rpc:call(?NODE, clouseau_rpc, disk_size, [Args#arg.path]),
    Res2 = rpc:call(?NODE, clouseau_rpc, disk_size, [?l2b(Db)]),
    ?assert(Res1 == Res2),
    ?assertEqual({ok, [{disk_size, 0}]}, Res1),
    teardown(Tid, Db).

t_open_index(_) ->
    {Tid, Db} = setup_with_ddoc(open_index, 3),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.peer)),
    ?assertNotEqual(nomatch, binary:match(Args#arg.path, ?l2b(Db))),
    ?assertEqual(<<"standard">>, Args#arg.analyzer),
    ?assert(is_pid(Args#arg.pid)),
    teardown(Tid, Db).

t_get_update_seq(_) ->
    {Tid, Db} = setup_with_ddoc(get_update_seq, 1),
    Args = get_args(Tid),
    ?assertEqual({ok, 0}, Args#arg.get_update_seq),
    teardown(Tid, Db).

t_get_update_seq_with_docs({Tid, Db}) ->
    set_tracer(Tid, get_update_seq, 1),
    Args = get_args(Tid),
    ?assertEqual({ok, 0}, Args#arg.get_update_seq),
    teardown(Tid, Db).

t_set_purge_seq(_) ->
    {Tid, Db} = setup_with_ddoc(set_purge_seq, 2),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(0, Args#arg.seq),
    ?assertEqual(ok, Args#arg.set_purge_seq),
    teardown(Tid, Db).

t_set_purge_seq_with_docs({Tid, Db}) ->
    set_tracer(Tid, set_purge_seq, 2),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(0, Args#arg.seq),
    ?assertEqual(ok, Args#arg.set_purge_seq),
    teardown(Tid, Db).

t_get_purge_seq(_) ->
    {Tid, Db} = setup_with_ddoc(get_purge_seq, 1),
    t_get_purge_seq_with_docs({Tid, Db}).

t_get_purge_seq_with_docs({Tid, Db}) ->
    set_tracer(Tid, get_purge_seq, 1),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual({ok, 0}, Args#arg.get_purge_seq),
    teardown(Tid, Db).

t_update(_) ->
    {Tid, Db} = setup_with_ddoc(update, 3),
    ?assertEqual(no_trace, get_args(Tid, 100)),
    teardown(Tid, Db).

t_update_with_docs({Tid, Db}) ->
    set_tracer(Tid, update, 3),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assert(binary_to_integer(Args#arg.id) rem 2 == 1),
    ?assertEqual([{<<"val">>, 1, {[{<<"store">>, true}]}}], Args#arg.fields),
    ?assertEqual(ok, Args#arg.update),
    teardown(Tid, Db).

t_delete(_) ->
    {Tid, Db} = setup_with_ddoc(delete, 2),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(<<"_design/ddoc">>, Args#arg.id),
    ?assertEqual(ok, Args#arg.delete),
    teardown(Tid, Db).

t_delete_with_docs({Tid, Db}) ->
    set_tracer(Tid, delete, 2),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assert(lists:member(Args#arg.id, [<<"_design/ddoc">>, <<"2">>, <<"4">>])),
    ?assertEqual(ok, Args#arg.delete),
    teardown(Tid, Db).

t_commit(_) ->
    {Tid, Db} = setup_with_ddoc(commit, 2),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(1, Args#arg.seq),
    ?assertEqual(ok, Args#arg.commit),
    teardown(Tid, Db).

t_commit_with_docs({Tid, Db}) ->
    set_tracer(Tid, commit, 2),
    Args = get_args(Tid),
    ?assert(is_pid(Args#arg.ref)),
    ?assert(lists:member(Args#arg.seq, [2, 3])),
    ?assertEqual(ok, Args#arg.commit),
    teardown(Tid, Db).

t_info(_) ->
    {Tid, Db} = setup_with_ddoc(info, 1),
    wait_indexing(),
    Res = req(get, url(Db, "_design/ddoc/_search_info/val_idx")),
    Args = get_args(Tid),
    ?assertEqual(
        {200, #{
            <<"name">> => <<"_design/ddoc/val_idx">>,
            <<"search_index">> =>
                #{
                    <<"committed_seq">> => 0,
                    <<"disk_size">> => 0,
                    <<"doc_count">> => 0,
                    <<"doc_del_count">> => 0,
                    <<"pending_seq">> => 1,
                    <<"signature">> => <<"d46ff3d065da8e84d4e7fec9d63b99f9">>
                }
        }},
        Res
    ),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(
        {ok, [
            {disk_size, 0},
            {doc_count, 0},
            {doc_del_count, 0},
            {pending_seq, 1},
            {committed_seq, 0},
            {purge_seq, 0}
        ]},
        Args#arg.info
    ),
    teardown(Tid, Db).

t_info_with_docs({Tid, Db}) ->
    wait_indexing(),
    set_tracer(Tid, info, 1),
    Res = req(get, url(Db, "_design/ddoc/_search_info/val_idx")),
    Args = get_args(Tid),
    ?assertEqual(
        {200, #{
            <<"name">> => <<"_design/ddoc/val_idx">>,
            <<"search_index">> =>
                #{
                    <<"committed_seq">> => 0,
                    <<"disk_size">> => 2456,
                    <<"doc_count">> => 2,
                    <<"doc_del_count">> => 0,
                    <<"pending_seq">> => 5,
                    <<"signature">> => <<"d46ff3d065da8e84d4e7fec9d63b99f9">>
                }
        }},
        Res
    ),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(
        {ok, [
            {disk_size, 1228},
            {doc_count, 1},
            {doc_del_count, 0},
            {pending_seq, 2},
            {committed_seq, 0},
            {purge_seq, 0}
        ]},
        Args#arg.info
    ),
    teardown(Tid, Db).

t_search(_) ->
    {Tid, Db} = setup_with_ddoc(search, 2),
    Res = req(get, url(Db, "_design/ddoc/_search/val_idx", "query=*%3A*")),
    Args = get_args(Tid),
    ?assertEqual(
        {200, #{
            <<"bookmark">> => <<"g2o">>,
            <<"rows">> => [],
            <<"total_rows">> => 0
        }},
        Res
    ),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(
        [
            {query, <<"*:*">>},
            {partition, nil},
            {limit, 25},
            {refresh, true},
            {'after', nil},
            {sort, relevance},
            {include_fields, nil},
            {counts, nil},
            {ranges, nil},
            {drilldown, []},
            {highlight_fields, nil},
            {highlight_pre_tag, <<"<em>">>},
            {highlight_post_tag, <<"</em>">>},
            {highlight_number, 1},
            {highlight_size, 0}
        ],
        Args#arg.args
    ),
    ?assertEqual({ok, {top_docs, 0, 0, [], undefined, undefined}}, Args#arg.search),
    teardown(Tid, Db).

t_search_with_docs({Tid, Db}) ->
    set_tracer(Tid, search, 2),
    Res = req(get, url(Db, "_design/ddoc/_search/val_idx", "query=*%3A*")),
    Args = get_args(Tid),
    ?assertEqual(
        {200, #{
            <<"bookmark">> =>
                <<"g1AAAABpeJzLYWBgYMpgTmHgz8tPSTV0MDQy1zMAQsMckEQiQ1L9____szKY3Ow_MIBBIgNO1XksIAUNQOo_hqYsADX3Gfw">>,
            <<"rows">> =>
                [
                    #{
                        <<"fields">> => #{<<"val">> => 1.0},
                        <<"id">> => <<"3">>,
                        <<"order">> => [1.0, 0]
                    },
                    #{
                        <<"fields">> => #{<<"val">> => 1.0},
                        <<"id">> => <<"1">>,
                        <<"order">> => [1.0, 0]
                    }
                ],
            <<"total_rows">> => 2
        }},
        Res
    ),
    ?assert(is_pid(Args#arg.ref)),
    ?assertEqual(
        [
            {query, <<"*:*">>},
            {partition, nil},
            {limit, 25},
            {refresh, true},
            {'after', nil},
            {sort, relevance},
            {include_fields, nil},
            {counts, nil},
            {ranges, nil},
            {drilldown, []},
            {highlight_fields, nil},
            {highlight_pre_tag, <<"<em>">>},
            {highlight_post_tag, <<"</em>">>},
            {highlight_number, 1},
            {highlight_size, 0}
        ],
        Args#arg.args
    ),
    ?assertMatch(
        {ok,
            {top_docs, 0, 1, [{hit, [1.0, 0], [{<<"_id">>, _}, {<<"val">>, 1.0}]}], undefined,
                undefined}},
        Args#arg.search
    ),
    teardown(Tid, Db).

t_group(_) ->
    {Tid, Db} = setup_with_ddoc(group, 7),
    Res = req(get, url(Db, "_design/ddoc/_search/val_idx", "query=*%3A*&group_field=val")),
    Args1 = get_args(Tid),
    ?assertEqual({200, #{<<"groups">> => [], <<"total_rows">> => 0}}, Res),
    ?assert(is_pid(Args1#arg.ref)),
    ?assertEqual(<<"*:*">>, Args1#arg.query),
    ?assertEqual(<<"val">>, Args1#arg.groupBy),
    ?assert(Args1#arg.refresh),
    ?assertEqual(relevance, Args1#arg.sort),
    ?assertEqual(0, Args1#arg.offset),
    ?assertEqual(10, Args1#arg.limit),
    ?assertEqual({ok, []}, Args1#arg.group1),

    set_tracer(Tid, group, 2),
    req(get, url(Db, "_design/ddoc/_search/val_idx", "query=*%3A*&group_field=val")),
    ?assertEqual(no_trace, get_args(Tid, 100)),
    teardown(Tid, Db).

t_group_with_docs({Tid, Db}) ->
    set_tracer(Tid, group, 7),
    Res = req(get, url(Db, "_design/ddoc/_search/val_idx", "query=*%3A*&group_field=val")),
    Args1 = get_args(Tid),
    ?assertEqual(
        {200, #{
            <<"groups">> =>
                [
                    #{
                        <<"by">> => <<36, 11, 127, 64, 0, 0, 0, 0, 0, 0>>,
                        <<"rows">> =>
                            [
                                #{
                                    <<"fields">> => #{<<"val">> => 1.0},
                                    <<"id">> => <<"3">>,
                                    <<"order">> => [1.0, 0]
                                },
                                #{
                                    <<"fields">> => #{<<"val">> => 1.0},
                                    <<"id">> => <<"1">>,
                                    <<"order">> => [1.0, 0]
                                }
                            ],
                        <<"total_rows">> => 2
                    }
                ],
            <<"total_rows">> => 2
        }},
        Res
    ),
    ?assert(is_pid(Args1#arg.ref)),
    ?assertEqual(<<"*:*">>, Args1#arg.query),
    ?assertEqual(<<"val">>, Args1#arg.groupBy),
    ?assert(Args1#arg.refresh),
    ?assertEqual(relevance, Args1#arg.sort),
    ?assertEqual(0, Args1#arg.offset),
    ?assertEqual(10, Args1#arg.limit),
    ?assertEqual({ok, [{<<36, 11, 127, 64, 0, 0, 0, 0, 0, 0>>, [1.0]}]}, Args1#arg.group1),

    set_tracer(Tid, group, 2),
    req(get, url(Db, "_design/ddoc/_search/val_idx", "query=*%3A*&group_field=val")),
    Args2 = get_args(Tid),
    ?assert(is_pid(Args2#arg.ref)),
    ?assertEqual(
        [
            {query, <<"*:*">>},
            {field, <<"val">>},
            {refresh, true},
            {groups, [{<<36, 11, 127, 64, 0, 0, 0, 0, 0, 0>>, [1.0]}]},
            {group_sort, relevance},
            {sort, relevance},
            {limit, 25},
            {include_fields, nil},
            {highlight_fields, nil},
            {highlight_pre_tag, <<"<em>">>},
            {highlight_post_tag, <<"</em>">>},
            {highlight_number, 1},
            {highlight_size, 0}
        ],
        Args2#arg.args
    ),
    ?assertEqual(
        {ok, 1, 1, [
            {<<36, 11, 127, 64, 0, 0, 0, 0, 0, 0>>, 1, [
                {hit, [1.0, 0], [{<<"_id">>, <<"1">>}, {<<"val">>, 1.0}]}
            ]}
        ]},
        Args2#arg.group2
    ),
    teardown(Tid, Db).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
start() ->
    application:start(ibrowse),
    test_util:wait_value(
        fun() ->
            net_kernel:start([?TEST_NODE, longnames]),
            net_kernel:nodename()
        end,
        ?TEST_NODE
    ),
    pong = net_adm:ping(?NODE),
    true = rpc:call(?NODE, clouseau_rpc, connected, []).

start_with_docs() ->
    start(),
    {Tid, Db} = setup_without_ddoc(open_index, 3),
    create_docs(Db, docs(?DOCS_NUM)),
    create_ddoc(Db, ?SEARCH),
    {Tid, Db}.

stop(_) ->
    {ok, Dbs} = rpc:call(?NODE, fabric, all_dbs, []),
    [rpc:call(?NODE, fabric, delete_db, [Db]) || Db <- Dbs],
    ibrowse:stop(),
    net_kernel:stop().

setup_with_ddoc(Func, Arity) ->
    setup(Func, Arity, true).

setup_without_ddoc(Func, Arity) ->
    setup(Func, Arity, false).

setup(Func, Arity, SetupDdoc) ->
    Tid = list_to_atom(?docid()),
    ets:new(Tid, [named_table, public]),
    ets:insert(Tid, #arg{}),
    set_tracer(Tid, Func, Arity),
    Db = ?b2l(?tempdb()),
    create_db(Db),
    case SetupDdoc of
        true -> create_ddoc(Db, ?SEARCH);
        false -> ok
    end,
    {Tid, Db}.

teardown(Tid, Db) ->
    delete_db(Db),
    % Clear trace pattern and all flags
    dbg:ctp(),
    dbg:p(all, clean),
    dbg:stop(),
    ets:delete(Tid).

set_tracer(Tid, Func, Arity) ->
    dbg:stop(),
    dbg:start(),
    dbg:tracer(process, custom_tracer(Tid, Func)),
    dbg:n(?NODE),
    dbg:cn(?TEST_NODE),
    dbg:tp(clouseau_rpc, literal_fun(Arity)),
    dbg:p(all, c).

custom_tracer(Tid, cleanup) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, cleanup, [DbName]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.dbName, DbName}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, cleanup, 1}, Cleanup1}, N) ->
                ets:update_element(Tid, arg, [{#arg.cleanup1, Cleanup1}, {#arg.called, true}]),
                N + 1;
            ({trace, _, call, {clouseau_rpc, cleanup, [DbName, Sig]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.dbName, DbName}, {#arg.activeSigs, Sig}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, cleanup, 2}, Cleanup2}, N) ->
                ets:update_element(Tid, arg, [{#arg.cleanup2, Cleanup2}, {#arg.called, true}]),
                N + 1
        end,
        0
    };
custom_tracer(Tid, rename) ->
    {
        fun
            ({trace, _, return_from, {clouseau_rpc, rename, 1}, Rename}, N) ->
                ets:update_element(Tid, arg, [{#arg.rename, Rename}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, open_index) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, open_index, [Peer, Path, Analyzer]}}, N) ->
                ets:update_element(Tid, arg, [
                    {#arg.peer, Peer}, {#arg.path, Path}, {#arg.analyzer, Analyzer}
                ]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, open_index, 3}, {ok, Pid}}, N) ->
                ets:update_element(Tid, arg, [{#arg.pid, Pid}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, get_update_seq) ->
    {
        fun
            ({trace, _, return_from, {clouseau_rpc, get_update_seq, 1}, USeq}, N) ->
                ets:update_element(Tid, arg, [{#arg.get_update_seq, USeq}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, set_purge_seq) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, set_purge_seq, [Ref, Seq]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.ref, Ref}, {#arg.seq, Seq}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, set_purge_seq, 2}, PSeq}, N) ->
                ets:update_element(Tid, arg, [{#arg.set_purge_seq, PSeq}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, get_purge_seq) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, get_purge_seq, [Ref]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.ref, Ref}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, get_purge_seq, 1}, PSeq}, N) ->
                ets:update_element(Tid, arg, [{#arg.get_purge_seq, PSeq}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, update) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, update, [Ref, Id, Fields]}}, N) ->
                ets:update_element(Tid, arg, [
                    {#arg.ref, Ref}, {#arg.id, Id}, {#arg.fields, Fields}
                ]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, update, 3}, Update}, N) ->
                ets:update_element(Tid, arg, [{#arg.update, Update}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, delete) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, delete, [Ref, Id]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.ref, Ref}, {#arg.id, Id}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, delete, 2}, Delete}, N) ->
                ets:update_element(Tid, arg, [{#arg.delete, Delete}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, commit) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, commit, [Ref, NewCommitSeq]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.ref, Ref}, {#arg.seq, NewCommitSeq}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, commit, 2}, Commit}, N) ->
                ets:update_element(Tid, arg, [{#arg.commit, Commit}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, info) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, info, [Ref]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.ref, Ref}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, info, 1}, Info}, N) ->
                ets:update_element(Tid, arg, [{#arg.info, Info}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, search) ->
    {
        fun
            ({trace, _, call, {clouseau_rpc, search, [Ref, Args]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.ref, Ref}, {#arg.args, Args}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, search, 2}, Search}, N) ->
                ets:update_element(Tid, arg, [{#arg.search, Search}, {#arg.called, true}]),
                N + 1;
            (_Trace, N) ->
                N + 1
        end,
        0
    };
custom_tracer(Tid, group) ->
    {
        fun
            (
                {trace, _, call,
                    {clouseau_rpc, group1, [
                        Ref, Query, GroupBy, Refresh, Sort, Offset, Limit
                    ]}},
                N
            ) ->
                ets:update_element(Tid, arg, [
                    {#arg.ref, Ref},
                    {#arg.query, Query},
                    {#arg.groupBy, GroupBy},
                    {#arg.refresh, Refresh},
                    {#arg.sort, Sort},
                    {#arg.offset, Offset},
                    {#arg.limit, Limit}
                ]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, group1, 7}, Group1}, N) ->
                ets:update_element(Tid, arg, [{#arg.group1, Group1}, {#arg.called, true}]),
                N + 1;
            ({trace, _, call, {clouseau_rpc, group2, [Ref, Args]}}, N) ->
                ets:update_element(Tid, arg, [{#arg.ref, Ref}, {#arg.args, Args}]),
                N + 1;
            ({trace, _, return_from, {clouseau_rpc, group2, 2}, Group2}, N) ->
                ets:update_element(Tid, arg, [{#arg.group2, Group2}, {#arg.called, true}]),
                N + 1
        end,
        0
    }.

literal_fun(Arity) ->
    case Arity of
        1 -> dbg:fun2ms(fun([_]) -> return_trace() end);
        2 -> dbg:fun2ms(fun([_, _]) -> return_trace() end);
        3 -> dbg:fun2ms(fun([_, _, _]) -> return_trace() end);
        7 -> dbg:fun2ms(fun([_, _, _, _, _, _, _]) -> return_trace() end)
    end.

get_args(Tid) ->
    get_args(Tid, ?TIMEOUT_IN_MS).

get_args(Tid, Timeout) ->
    WaitFun =
        fun() ->
            case ets:lookup(Tid, arg) of
                [Args] when Args#arg.called ->
                    ets:update_element(Tid, arg, [{#arg.called, false}]),
                    Args;
                _ ->
                    wait
            end
        end,
    case test_util:wait(WaitFun, Timeout) of
        timeout -> no_trace;
        Args -> Args
    end.

wait_indexing() ->
    timer:sleep(?TIMEOUT_IN_MS).

base_url() ->
    lists:concat(["http://", ?HOST, ":", ?PORT]).

url(Db) ->
    base_url() ++ "/" ++ Db.

url(Db, Path) ->
    url(Db) ++ "/" ++ Path.

url(Db, Path, Params) ->
    url(Db) ++ "/" ++ Path ++ "?" ++ Params.

delete_db(Db) ->
    req(delete, url(Db)).

create_db(Db) ->
    {201, #{}} = req(put, url(Db)).

create_ddoc(Db, Ddoc) ->
    {201, _} = req(post, url(Db), Ddoc).

create_docs(Db, Docs) ->
    {201, _} = req(post, url(Db, "_bulk_docs"), #{<<"docs">> => Docs}).

docs(Counter) ->
    lists:foldl(
        fun(I, Acc) ->
            Id = ?l2b(integer_to_list(I)),
            Doc = #{<<"_id">> => <<Id/binary>>, <<"val">> => I band 1},
            [Doc | Acc]
        end,
        [],
        lists:seq(1, Counter)
    ).

req(Method, Url) ->
    req(Method, Url, #{}).

req(Method, Url, Body) ->
    req(Method, Url, [], Body).

req(Method, Url, Headers, #{} = Body) ->
    {ok, Code0, _RespHeaders, RespBody0} =
        ibrowse:send_req(Url, Headers ++ [?ADM_AUTH, ?JSON], Method, jiffy:encode(Body)),
    Code = list_to_integer(Code0),
    RespBody = iolist_to_binary(RespBody0),
    {Code, jiffy:decode(RespBody, [return_maps])}.

-endif.
