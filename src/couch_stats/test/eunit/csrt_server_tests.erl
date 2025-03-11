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

-define(DOCS_COUNT, 100).
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

csrt_fabric_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_all_docs_include_false),
            ?TDEF(t_all_docs_include_true),
            ?TDEF(t_all_docs_limit_zero),
            ?TDEF(t_get_doc),
            ?TDEF(t_put_doc),
            ?TDEF(t_delete_doc),
            ?TDEF(t_update_docs),
            ?TDEF(t_changes),
            ?TDEF(t_changes_limit_zero),
            ?TDEF(t_changes_filtered),
            ?TDEF(t_changes_js_filtered),
            ?TDEF(t_view_query)
        ])
    }.

setup() ->
    Ctx = test_util:start_couch([fabric, couch_stats]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, ?DB_Q}, {n, 1}]),
    Docs = [#doc{id = ?l2b("foo_" ++ integer_to_list(I))} || I <- lists:seq(1, ?DOCS_COUNT)],
    Opts = [],
    {ok, _} = fabric:update_docs(DbName, Docs, Opts),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx).

t_context_setting({_Ctx, _DbName}) ->
    false.

t_all_docs_limit_zero({_Ctx, DbName}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_all_docs"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = false, limit = 0},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    ?debugFmt("RCTX: ~p~n", [Rctx]),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => 0,
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ?assert(maps:get(ioq_calls, Rctx) > 0),
    ?assert(maps:get(get_kp_node, Rctx) > 0),
    ?assert(maps:get(get_kv_node, Rctx) > 0),
    ok = assert_teardown(PidRef).


t_all_docs_include_false({_Ctx, DbName}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_all_docs"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => ?DOCS_COUNT,
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ?assert(maps:get(ioq_calls, Rctx) > 0),
    ?assert(maps:get(get_kp_node, Rctx) > 0),
    ?assert(maps:get(get_kv_node, Rctx) > 0),
    ok = assert_teardown(PidRef).

t_all_docs_include_true({_Ctx, DbName}) ->
    pdebug(dbname, DbName),
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_all_docs"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    MArgs = #mrargs{include_docs = true},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => ?DOCS_COUNT,
        docs_read => ?DOCS_COUNT,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ok = nonzero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_update_docs({_Ctx, DbName}) ->
    pdebug(dbname, DbName),
    Context = #{
        method => 'POST',
        path => "/" ++ ?b2l(DbName)
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    Docs = [#doc{id = ?l2b("bar_" ++ integer_to_list(I))} || I <- lists:seq(1, ?DOCS_COUNT)],
    _Res = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => 0,
        docs_read => 0,
        docs_written => ?DOCS_COUNT,
        pid_ref => PidRef
    }),
    ok = zero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_get_doc({_Ctx, DbName}) ->
    pdebug(dbname, DbName),
    DocId = "foo_17",
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/" ++ DocId
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:open_doc(DbName, DocId, [?ADMIN_CTX]),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
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


t_put_doc({_Ctx, DbName}) ->
    pdebug(dbname, DbName),
    DocId = "bar_put_1919",
    Context = #{
        method => 'PUT',
        path => "/" ++ ?b2l(DbName) ++ "/" ++ DocId
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    Doc = #doc{id = ?l2b(DocId)},
    _Res = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => 1,
        rows_read => 0,
        docs_read => 0,
        docs_written => 1,
        pid_ref => PidRef
    }),
    ok = zero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_delete_doc({_Ctx, DbName}) ->
    pdebug(dbname, DbName),
    DocId = "foo_17",
    {ok, Doc0} = fabric:open_doc(DbName, DocId, [?ADMIN_CTX]),
    Doc = Doc0#doc{body = {[{<<"_deleted">>, true}]}},
    Context = #{
        method => 'DELETE',
        path => "/" ++ ?b2l(DbName) ++ "/" ++ DocId
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    pdebug(rctx, Rctx),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => 1,
        rows_read => 0,
        docs_read => 0,
        docs_written => 1,
        pid_ref => PidRef
    }),
    ok = zero_local_io_assert(Rctx),
    ok = assert_teardown(PidRef).

t_changes({_Ctx, DbName}) ->
    pdebug(dbname, DbName),
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_changes"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:changes(DbName, fun changes_cb/2, [], #changes_args{limit=0}),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => false,
        changes_returned => false,
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    %% at least one rows_read and changes_returned per shard that has at least
    %% one document in it
    ?assert(maps:get(rows_read, Rctx) >= ?DB_Q, rows_read),
    ?assert(maps:get(changes_returned, Rctx) >= ?DB_Q, changes_returned),
    ok = nonzero_local_io_assert(Rctx),
    ?assert(maps:get(ioq_calls, Rctx) > 0),
    ?assert(maps:get(get_kp_node, Rctx) > 0),
    ?assert(maps:get(get_kv_node, Rctx) > 0),
    ok = assert_teardown(PidRef).


t_changes_limit_zero({_Ctx, DbName}) ->
    Context = #{
        method => 'GET',
        path => "/" ++ ?b2l(DbName) ++ "/_changes"
    },
    {PidRef, Nonce} = coordinator_context(Context),
    Rctx0 = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = fresh_rctx_assert(Rctx0, PidRef, Nonce),
    _Res = fabric:changes(DbName, fun changes_cb/2, [], #changes_args{limit=0}),
    Rctx = csrt_util:to_json(csrt:get_resource(PidRef)),
    ok = rctx_assert(Rctx, #{
        nonce => Nonce,
        db_open => ?DB_Q,
        rows_read => false,
        changes_returned => false,
        docs_read => 0,
        docs_written => 0,
        pid_ref => PidRef
    }),
    ?assert(maps:get(rows_read, Rctx) >= ?DB_Q, rows),
    ?assert(maps:get(changes_returned, Rctx) >= ?DB_Q, rows),
    ok = nonzero_local_io_assert(Rctx),
    ?assert(maps:get(ioq_calls, Rctx) > 0),
    ?assert(maps:get(get_kp_node, Rctx) > 0),
    ?assert(maps:get(get_kv_node, Rctx) > 0),
    ok = assert_teardown(PidRef).

t_changes_filtered({_Ctx, DbName}) ->
    ?assert(false, DbName).

t_changes_js_filtered({_Ctx, DbName}) ->
    ?assert(false, DbName).

t_view_query({_Ctx, DbName}) ->
    ?assert(false, DbName).

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

convert_pidref({_, _}=PidRef) ->
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
        write_kp_node => 0,
        write_kv_node => 0,
        nonce  => undefined,
        db_open  => 0,
        rows_read  => 0,
        docs_read  => 0,
        docs_written  => 0,
        pid_ref  => undefined
    },
    Asserts = maps:merge(
        DefaultAsserts,
        maps:update_with(pid_ref, fun convert_pidref/1, Asserts0)
    ),
    ok = maps:foreach(
        fun
            (_K, false) ->
                ok;
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

coordinator_context(#{method := Method, path := Path}) ->
    Nonce = couch_util:to_hex(crypto:strong_rand_bytes(5)),
    Req = #httpd{method=Method, nonce=Nonce},
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
