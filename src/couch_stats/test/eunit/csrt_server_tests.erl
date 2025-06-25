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

-module(csrt_server_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("../../src/couch_stats_resource_tracker.hrl").

-define(DOCS_COUNT, 100).
-define(DDOCS_COUNT, 1).
-define(DB_Q, 8).

-define(DEBUG_ENABLED, false).

csrt_context_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_context_setting)
        ])
    }.

test_funs() ->
    [
        ?TDEF_FE(t_all_docs_include_false),
        ?TDEF_FE(t_all_docs_include_true),
        ?TDEF_FE(t_all_docs_limit_zero),
        ?TDEF_FE(t_get_doc),
        ?TDEF_FE(t_put_doc),
        ?TDEF_FE(t_delete_doc),
        ?TDEF_FE(t_update_docs),
        ?TDEF_FE(t_changes),
        ?TDEF_FE(t_changes_limit_zero),
        ?TDEF_FE(t_changes_filtered),
        ?TDEF_FE(t_updated_at),
        ?TDEF_FE(t_view_query),
        ?TDEF_FE(t_view_query_include_docs)
    ].

ddoc_test_funs() ->
    [
        ?TDEF_FE(t_changes_js_filtered)
        | test_funs()
    ].

csrt_fabric_no_ddoc_test_() ->
    {
        "CSRT fabric tests with no DDoc present",
        foreach,
        fun setup/0,
        fun teardown/1,
        test_funs()
    }.

csrt_fabric_test_() ->
    {
        "CSRT fabric tests with a DDoc present",
        foreach,
        fun() -> setup_ddoc(<<"_design/foo">>, <<"bar">>) end,
        fun teardown/1,
        ddoc_test_funs()
    }.

make_docs(Count) ->
    lists:map(
        fun(I) ->
            #doc{
                id = ?l2b("foo_" ++ integer_to_list(I)),
                body = {[{<<"value">>, I}]}
            }
        end,
        lists:seq(1, Count)
    ).

setup() ->
    Ctx = test_util:start_couch([fabric, couch_stats]),
    config:set_boolean(?CSRT, "randomize_testing", false, false),
    ok = meck:new(ioq, [passthrough]),
    ok = meck:expect(ioq, bypass, fun(_, _) -> false end),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, ?DB_Q}, {n, 1}]),
    Docs = make_docs(?DOCS_COUNT),
    Opts = [],
    {ok, _} = fabric:update_docs(DbName, Docs, Opts),
    {Ctx, DbName, undefined}.

teardown({Ctx, DbName, _View}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    ok = meck:unload(ioq),
    test_util:stop_couch(Ctx).

setup_ddoc(DDocId, ViewName) ->
    {Ctx, DbName, undefined} = setup(),
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"javascript">>},
            {
                <<"views">>,
                {[
                    {
                        ViewName,
                        {[
                            {<<"map">>, <<"function(doc) { emit(doc.value, null); }">>}
                        ]}
                    }
                ]}
            },
            {
                <<"filters">>,
                {[
                    {
                        <<"even">>,
                        <<"function(doc) { return (doc.value % 2 == 0); }">>
                    }
                ]}
            }
        ]}
    ),
    {ok, _Rev} = fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]),
    {Ctx, DbName, {DDocId, ViewName}}.

t_context_setting({_Ctx, _DbName, _View}) ->
    false.

t_all_docs_limit_zero({_Ctx, DbName, _View}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_all_docs"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = false, limit = 0},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => 0,
        docs_read => 0,
        docs_written => 0,
        ioq_calls => assert_gt(),
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_all_docs_include_false({_Ctx, DbName, View}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_all_docs"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => docs_count(View),
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_all_docs_include_true({_Ctx, DbName, View}) ->
    pdebug(dbname, DbName),
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_all_docs"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = true},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => docs_count(View),
        docs_read => docs_count(View),
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_update_docs({_Ctx, DbName, View}) ->
    pdebug(dbname, DbName),
    Context = #{
        method => 'POST',
        path => "/" ++ ?b2l(DbName)
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    Docs = [#doc{id = ?l2b("bar_" ++ integer_to_list(I))} || I <- lists:seq(1, ?DOCS_COUNT)],
    _Res = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]),
    Rctx = load_rctx(PidRef),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => 0,
        docs_read => 0,
        docs_written => ?DOCS_COUNT,
        pid_ref => PidRef
    }),
    ok = ddoc_dependent_local_io_assert(Rctx, View),
    ok = assert_teardown(PidRef).

t_get_doc({_Ctx, DbName, _View}) ->
    pdebug(dbname, DbName),
    DocId = "foo_17",
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/" ++ DocId
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:open_doc(DbName, DocId, [?ADMIN_CTX]),
    Rctx = load_rctx(PidRef),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => 1,
        rows_read => 0,
        docs_read => 1,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx, io_sum),
    ok = assert_teardown(PidRef).

t_updated_at({_Ctx, DbName, _View}) ->
    %% Same test as t_get_doc but with a timer sleep and updated_at assertion
    TimeDelay = 1234,
    pdebug(dbname, DbName),
    DocId = "foo_17",
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/" ++ DocId
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    timer:sleep(TimeDelay),
    _Res = fabric:open_doc(DbName, DocId, [?ADMIN_CTX]),
    Rctx = load_rctx(PidRef),
    %% Get RawRctx to have pre-json-converted timestamps
    RawRctx = csrt:get_resource(PidRef),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => 1,
        rows_read => 0,
        docs_read => 1,
        docs_written => 0,
        pid_ref => PidRef
    }),
    Started = csrt_entry:value(started_at, RawRctx),
    Updated = csrt_entry:value(updated_at, RawRctx),
    ?assert(
        csrt_util:make_dt(Started, Updated, millisecond) > TimeDelay,
        "updated_at gets updated with an expected TimeDelay"
    ),
    ?assert(
        csrt_util:make_dt(Started, Updated, millisecond) < 2 * TimeDelay,
        "updated_at gets updated in a reasonable time frame"
    ),
    ok = nonzero_local_io_assert(Rctx, io_sum),
    ok = assert_teardown(PidRef).

t_put_doc({_Ctx, DbName, View}) ->
    pdebug(dbname, DbName),
    DocId = "bar_put_1919",
    Context = #{
        method => 'PUT',
        path => "/" ++ ?b2l(DbName) ++ "/" ++ DocId
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    Doc = #doc{id = ?l2b(DocId)},
    _Res = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]),
    Rctx = load_rctx(PidRef),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => 1,
        rows_read => 0,
        docs_read => 0,
        docs_written => 1,
        pid_ref => PidRef
    }),
    ok = ddoc_dependent_local_io_assert(Rctx, View),
    ok = assert_teardown(PidRef).

t_delete_doc({_Ctx, DbName, View}) ->
    pdebug(dbname, DbName),
    DocId = "foo_17",
    {ok, Doc0} = fabric:open_doc(DbName, DocId, [?ADMIN_CTX]),
    Doc = Doc0#doc{body = {[{<<"_deleted">>, true}]}},
    Context = #{
        method => 'DELETE',
        path => "/" ++ ?b2l(DbName) ++ "/" ++ DocId
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]),
    Rctx = load_rctx(PidRef),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => 1,
        rows_read => 0,
        docs_read => 0,
        docs_written => 1,
        pid_ref => PidRef
    }),
    ok = ddoc_dependent_local_io_assert(Rctx, View),
    ok = assert_teardown(PidRef).

t_changes({_Ctx, DbName, View}) ->
    pdebug(dbname, DbName),
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_changes"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:changes(DbName, fun changes_cb/2, [], #changes_args{}),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => docs_count(View),
        changes_returned => docs_count(View),
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_changes_limit_zero({_Ctx, DbName, _View}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_changes"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:changes(DbName, fun changes_cb/2, [], #changes_args{limit = 0}),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => assert_gte(?DB_Q),
        changes_returned => assert_gte(?DB_Q),
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

%% TODO: stub in non JS filter with selector
t_changes_filtered({_Ctx, _DbName, _View}) ->
    false.

t_changes_js_filtered({_Ctx, DbName, {DDocId, _ViewName} = View}) ->
    pdebug(dbname, DbName),
    Method = 'GET',
    Path = "/" ++ ?b2l(DbName) ++ "/_changes",
    Context = #{
        method => Method,
        path => Path
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Req = {json_req, null},
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    Filter = configure_filter(DbName, DDocId, Req),
    Args = #changes_args{filter_fun = Filter},
    _Res = fabric:changes(DbName, fun changes_cb/2, [], Args),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => assert_gte(?DB_Q),
        rows_read => assert_gte(docs_count(View)),
        changes_returned => round(?DOCS_COUNT / 2),
        docs_read => assert_gte(docs_count(View)),
        docs_written => 0,
        pid_ref => PidRef,
        js_filter => docs_count(View),
        js_filtered_docs => docs_count(View)
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_view_query({_Ctx, DbName, View}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_design/foo/_view/bar"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => docs_count(View),
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_view_query_include_docs({_Ctx, DbName, View}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_design/foo/_view/bar"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = load_rctx(PidRef),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = true},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => docs_count(View),
        docs_read => docs_count(View),
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

assert_teardown(PidRef) ->
    ?assertEqual(ok, csrt:destroy_context(PidRef)),
    ?assertEqual(undefined, csrt:get_resource()),
    %% Normally the tracker is responsible for destroying the resource
    ?assertEqual(true, csrt_server:destroy_resource(PidRef)),
    ?assertEqual(undefined, csrt:get_resource(PidRef)),
    ok.

view_cb({row, Row}, Acc) ->
    {ok, [Row | Acc]};
view_cb(_Msg, Acc) ->
    {ok, Acc}.

changes_cb({change, {Change}}, Acc) ->
    {ok, [Change | Acc]};
changes_cb(_Msg, Acc) ->
    {ok, Acc}.

pdebug(dbname, DbName) ->
    case ?DEBUG_ENABLED =:= true of
        true ->
            ?debugFmt("DBNAME[~p]: ~p", [DbName, fabric:get_db_info(DbName)]);
        false ->
            ok
    end;
pdebug(rctx, Rctx) ->
    ?DEBUG_ENABLED andalso ?debugFmt("GOT RCTX: ~p~n", [Rctx]).

pdbg(Str, Args) ->
    ?DEBUG_ENABLED andalso ?debugFmt(Str, Args).

convert_pidref({_, _} = PidRef) ->
    csrt_util:convert_pidref(PidRef);
convert_pidref(PidRef) when is_binary(PidRef) ->
    PidRef;
convert_pidref(false) ->
    false.

rctx_assert(Rctx, Asserts0) ->
    DefaultAsserts = #{
        changes_returned => 0,
        js_filter => 0,
        js_filtered_docs => 0,
        nonce => null,
        db_open => 0,
        rows_read => 0,
        docs_read => 0,
        docs_written => 0,
        pid_ref => null
    },
    Updates = #{
        pid_ref => fun convert_pidref/1,
        nonce => fun csrt_util:convert_string/1
    },
    Asserts = maps:merge(
        DefaultAsserts,
        maps:fold(fun maps:update_with/3, Asserts0, Updates)
    ),
    ok = maps:foreach(
        fun
            (_K, false) ->
                ok;
            (K, Fun) when is_function(Fun) ->
                Fun(K, maps:get(K, Rctx));
            (K, V) ->
                case maps:get(K, Rctx) of
                    false ->
                        ok;
                    RV ->
                        pdbg("?assertEqual(~p, ~p, ~p)", [V, RV, K]),
                        ?assertEqual(V, RV, K)
                end
        end,
        Asserts
    ),
    ok.

%% Doc updates and others don't perform local IO, they funnel to another pid
zero_local_io_assert(Rctx) ->
    ?assertEqual(0, maps:get(ioq_calls, Rctx)),
    ?assertEqual(0, maps:get(get_kp_node, Rctx)),
    ?assertEqual(0, maps:get(get_kv_node, Rctx)),
    ok.

nonzero_local_io_assert(Rctx) ->
    nonzero_local_io_assert(Rctx, io_separate).

%% io_sum for when get_kp_node=0
nonzero_local_io_assert(Rctx, io_sum) ->
    ?assert(maps:get(ioq_calls, Rctx) > 0),
    #{
        get_kp_node := KPNodes,
        get_kv_node := KVNodes
    } = Rctx,
    ?assert((KPNodes + KVNodes) > 0),
    ok;
nonzero_local_io_assert(Rctx, io_separate) ->
    ?assert(maps:get(ioq_calls, Rctx) > 0),
    ?assert(maps:get(get_kp_node, Rctx) > 0),
    ?assert(maps:get(get_kv_node, Rctx) > 0),
    ok.

ddoc_dependent_local_io_assert(Rctx, undefined) ->
    zero_local_io_assert(Rctx);
ddoc_dependent_local_io_assert(Rctx, {_DDoc, _ViewName}) ->
    nonzero_local_io_assert(Rctx, io_sum).

coordinator_context(#{method := Method, path := Path}) ->
    Nonce = couch_util:to_hex(crypto:strong_rand_bytes(5)),
    Req = #httpd{method = Method, nonce = Nonce},
    {_, _} = PidRef = csrt:create_coordinator_context(Req, Path),
    {PidRef, Nonce}.

fresh_rctx_assert(Rctx, PidRef, Nonce) ->
    pdebug(rctx, Rctx),
    FreshAsserts = #{
        nonce => Nonce,
        db_open => 0,
        rows_read => 0,
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    },
    rctx_assert(Rctx, FreshAsserts).

assert_gt() ->
    assert_gt(0).

assert_gt(N) ->
    fun(K, RV) -> ?assert(RV > N, {K, RV, N}) end.

assert_gte(N) ->
    fun(K, RV) -> ?assert(RV >= N, {K, RV, N}) end.

docs_count(undefined) ->
    ?DOCS_COUNT;
docs_count({_, _}) ->
    ?DOCS_COUNT + ?DDOCS_COUNT.

configure_filter(DbName, DDocId, Req) ->
    configure_filter(DbName, DDocId, Req, <<"even">>).

configure_filter(DbName, DDocId, Req, FName) ->
    {ok, DDoc} = ddoc_cache:open_doc(DbName, DDocId),
    DIR = fabric_util:doc_id_and_rev(DDoc),
    Style = main_only,
    {fetch, custom, Style, Req, DIR, FName}.

load_rctx(PidRef) ->
    %% Add slight delay to accumulate RPC response deltas
    timer:sleep(50),
    csrt_util:to_json(csrt:get_resource(PidRef)).
