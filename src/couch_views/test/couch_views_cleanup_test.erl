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

-module(couch_views_cleanup_test).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("couch_views/include/couch_views.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


clean_old_indices_test_() ->
    {
        "Test cleanup of stale indices",
        {
            setup,
            fun setup_all/0,
            fun cleanup_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(empty_db),
                    ?TDEF_FE(db_with_no_ddocs),
                    ?TDEF_FE(db_with_ddoc),
                    ?TDEF_FE(db_with_many_ddocs),
                    ?TDEF_FE(after_ddoc_deletion),
                    ?TDEF_FE(all_ddocs_deleted),
                    ?TDEF_FE(after_ddoc_recreated),
                    ?TDEF_FE(refcounted_sigs),
                    ?TDEF_FE(removes_old_jobs),
                    ?TDEF_FE(after_job_accepted_initial_build),
                    ?TDEF_FE(after_job_accepted_rebuild),
                    ?TDEF_FE(during_index_initial_build),
                    ?TDEF_FE(during_index_rebuild)
                ]
            }
        }
    }.


setup_all() ->
    test_util:start_couch([
        fabric,
        couch_jobs,
        couch_js,
        couch_views
    ]).


cleanup_all(Ctx) ->
    test_util:stop_couch(Ctx).


setup() ->
    Opts = [{user_ctx, ?ADMIN_USER}],
    {ok, Db} = fabric2_db:create(?tempdb(), Opts),
    Db.


cleanup(Db) ->
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


empty_db(Db) ->
    ?assertEqual(ok, fabric2_index:cleanup(Db)).


db_with_no_ddocs(Db) ->
    create_docs(Db, 10),
    ?assertEqual(ok, fabric2_index:cleanup(Db)).


db_with_ddoc(Db) ->
    create_docs(Db, 10),
    DDoc = create_ddoc(Db, <<"foo">>),
    ?assertEqual(10, length(run_query(Db, DDoc))),
    ?assertEqual(ok, fabric2_index:cleanup(Db)),
    ?assertEqual(10, length(run_query(Db, DDoc))).


db_with_many_ddocs(Db) ->
    create_docs(Db, 10),
    DDocs = create_ddocs(Db, 5),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, DDocs),
    ?assertEqual(ok, fabric2_index:cleanup(Db)).


after_ddoc_deletion(Db) ->
    create_docs(Db, 10),
    DDocs = create_ddocs(Db, 2),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, DDocs),
    [ToDel | RestDDocs] = DDocs,
    delete_doc(Db, ToDel),
    % Not yet cleaned up
    ?assertEqual(true, view_has_data(Db, ToDel)),
    ?assertEqual(ok, fabric2_index:cleanup(Db)),
    ?assertError({ddoc_deleted, _}, run_query(Db, ToDel)),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, RestDDocs).


all_ddocs_deleted(Db) ->
    create_docs(Db, 10),
    DDocs = create_ddocs(Db, 5),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, DDocs),
    lists:foreach(fun(DDoc) ->
        delete_doc(Db, DDoc)
    end, DDocs),
    % Not yet cleaned up
    lists:foreach(fun(DDoc) ->
        ?assertEqual(true, view_has_data(Db, DDoc))
    end, DDocs),
    ?assertEqual(ok, fabric2_index:cleanup(Db)),
    lists:foreach(fun(DDoc) ->
        ?assertError({ddoc_deleted, _}, run_query(Db, DDoc))
    end, DDocs).


after_ddoc_recreated(Db) ->
    create_docs(Db, 10),
    DDocs = create_ddocs(Db, 3),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, DDocs),
    [ToDel | RestDDocs] = DDocs,
    Deleted = delete_doc(Db, ToDel),
    % Not yet cleaned up
    ?assertEqual(true, view_has_data(Db, ToDel)),
    ?assertEqual(ok, fabric2_index:cleanup(Db)),
    ?assertError({ddoc_deleted, _}, run_query(Db, ToDel)),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, RestDDocs),
    recreate_doc(Db, Deleted),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, DDocs),
    ?assertEqual(ok, fabric2_index:cleanup(Db)),
    lists:foreach(fun(DDoc) ->
        ?assertEqual(10, length(run_query(Db, DDoc)))
    end, DDocs).


refcounted_sigs(Db) ->
    create_docs(Db, 10),
    DDoc1 = create_ddoc(Db, <<"1">>),
    DDoc2 = create_doc(Db, <<"_design/2">>, DDoc1#doc.body),
    ?assertEqual(10, length(run_query(Db, DDoc1))),
    ?assertEqual(10, length(run_query(Db, DDoc2))),

    ?assertEqual(true, view_has_data(Db, DDoc1)),
    ?assertEqual(true, view_has_data(Db, DDoc2)),

    delete_doc(Db, DDoc1),
    ok = fabric2_index:cleanup(Db),

    ?assertEqual(true, view_has_data(Db, DDoc1)),
    ?assertEqual(true, view_has_data(Db, DDoc2)),

    delete_doc(Db, DDoc2),
    ok = fabric2_index:cleanup(Db),

    ?assertEqual(false, view_has_data(Db, DDoc1)),
    ?assertEqual(false, view_has_data(Db, DDoc2)).


removes_old_jobs(Db) ->
    create_docs(Db, 10),
    DDoc = create_ddoc(Db, <<"foo">>),

    ?assertEqual(10, length(run_query(Db, DDoc))),
    ?assertEqual(true, view_has_data(Db, DDoc)),
    ?assertEqual(true, job_exists(Db, DDoc)),

    delete_doc(Db, DDoc),
    ?assertEqual(ok, fabric2_index:cleanup(Db)),

    ?assertEqual(false, view_has_data(Db, DDoc)),
    ?assertEqual(false, job_exists(Db, DDoc)).


after_job_accepted_initial_build(Db) ->
    cleanup_during_initial_build(Db, fun meck_intercept_job_accept/2).


after_job_accepted_rebuild(Db) ->
    cleanup_during_rebuild(Db, fun meck_intercept_job_accept/2).


during_index_initial_build(Db) ->
    cleanup_during_initial_build(Db, fun meck_intercept_job_update/2).


during_index_rebuild(Db) ->
    cleanup_during_rebuild(Db, fun meck_intercept_job_update/2).


cleanup_during_initial_build(Db, InterruptFun) ->
    InterruptFun(fabric2_db:name(Db), self()),

    create_docs(Db, 10),
    DDoc = create_ddoc(Db, <<"foo">>),

    {_, Ref1} = spawn_monitor(fun() -> run_query(Db, DDoc) end),

    receive {JobPid, triggered} -> ok end,
    delete_doc(Db, DDoc),
    ok = fabric2_index:cleanup(Db),
    JobPid ! continue,

    receive {'DOWN', Ref1, _, _, _} -> ok end,

    ok = fabric2_index:cleanup(Db),
    ?assertError({ddoc_deleted, _}, run_query(Db, DDoc)),

    ?assertEqual(false, view_has_data(Db, DDoc)),
    ?assertEqual(false, job_exists(Db, DDoc)).


cleanup_during_rebuild(Db, InterruptFun) ->
    create_docs(Db, 10),
    DDoc = create_ddoc(Db, <<"foo">>),
    ?assertEqual(10, length(run_query(Db, DDoc))),

    InterruptFun(fabric2_db:name(Db), self()),

    create_docs(Db, 10, 10),

    {_, Ref1} = spawn_monitor(fun() -> run_query(Db, DDoc) end),

    receive {JobPid, triggered} -> ok end,
    delete_doc(Db, DDoc),
    ok = fabric2_index:cleanup(Db),
    JobPid ! continue,

    receive {'DOWN', Ref1, _, _, _} -> ok end,

    ok = fabric2_index:cleanup(Db),
    ?assertError({ddoc_deleted, _}, run_query(Db, DDoc)),

    ?assertEqual(false, view_has_data(Db, DDoc)),
    ?assertEqual(false, job_exists(Db, DDoc)).



run_query(Db, DDocId) when is_binary(DDocId) ->
    {ok, DDoc} = fabric2_db:open_doc(Db, <<"_design/", DDocId/binary>>),
    run_query(Db, DDoc);

run_query(Db, DDoc) ->
    Fun = fun default_cb/2,
    {ok, Result} = couch_views:query(Db, DDoc, <<"bar">>, Fun, [], #{}),
    Result.


default_cb(complete, Acc) ->
    {ok, lists:reverse(Acc)};
default_cb({final, Info}, []) ->
    {ok, [Info]};
default_cb({final, _}, Acc) ->
    {ok, Acc};
default_cb({meta, _}, Acc) ->
    {ok, Acc};
default_cb(ok, ddoc_updated) ->
    {ok, ddoc_updated};
default_cb(Row, Acc) ->
    {ok, [Row | Acc]}.


view_has_data(Db, DDoc) ->
    DbName = fabric2_db:name(Db),
    {ok, #mrst{sig = Sig}} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx,
            db_prefix := DbPrefix
        } = TxDb,
        SigKeyTuple = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Sig},
        SigKey = erlfdb_tuple:pack(SigKeyTuple, DbPrefix),
        SigVal = erlfdb:wait(erlfdb:get(Tx, SigKey)),

        RangeKeyTuple = {?DB_VIEWS, ?VIEW_TREES, Sig},
        RangeKey = erlfdb_tuple:pack(RangeKeyTuple, DbPrefix),
        Range = erlfdb:wait(erlfdb:get_range_startswith(Tx, RangeKey)),

        SigVal /= not_found andalso Range /= []
    end).


meck_intercept_job_accept(TgtDbName, ParentPid) ->
    meck:new(fabric2_db, [passthrough]),
    meck:expect(fabric2_db, open, fun
        (DbName, Opts) when DbName == TgtDbName ->
            Result = meck:passthrough([DbName, Opts]),
            ParentPid ! {self(), triggered},
            receive continue -> ok end,
            meck:unload(),
            Result;
        (DbName, Opts) ->
            meck:passthrough([DbName, Opts])
    end).


meck_intercept_job_update(_DbName, ParentPid) ->
    meck:new(couch_jobs, [passthrough]),
    meck:expect(couch_jobs, finish, fun(Tx, Job, Data) ->
        ParentPid ! {self(), triggered},
        receive continue -> ok end,
        Result = meck:passthrough([Tx, Job, Data]),
        meck:unload(),
        Result
    end).


create_ddoc(Db, Id) ->
    MapFunFmt = "function(doc) {var f = \"~s\"; emit(doc.val, f)}",
    MapFun = io_lib:format(MapFunFmt, [Id]),
    Body = {[
        {<<"views">>, {[
            {<<"bar">>, {[{<<"map">>, iolist_to_binary(MapFun)}]}}
        ]}}
    ]},
    create_doc(Db, <<"_design/", Id/binary>>, Body).


recreate_doc(Db, #doc{deleted = true} = Doc) ->
    #doc{
        id = DDocId,
        body = Body
    } = Doc,
    create_doc(Db, DDocId, Body).


create_ddocs(Db, Count) when is_integer(Count), Count > 1 ->
    lists:map(fun(Seq) ->
        Id = io_lib:format("~6..0b", [Seq]),
        create_ddoc(Db, iolist_to_binary(Id))
    end, lists:seq(1, Count)).


create_doc(Db, Id) ->
    create_doc(Db, Id, {[{<<"value">>, Id}]}).


create_doc(Db, Id, Body) ->
    Doc = #doc{
        id = Id,
        body = Body
    },
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc),
    Doc#doc{revs = {Pos, [Rev]}}.


create_docs(Db, Count) ->
    create_docs(Db, Count, 0).


create_docs(Db, Count, Offset) ->
    lists:map(fun(Seq) ->
        Id = io_lib:format("~6..0b", [Seq]),
        create_doc(Db, iolist_to_binary(Id))
    end, lists:seq(Offset + 1, Offset + Count)).


delete_doc(Db, DDoc) ->
    #doc{
        revs = {_, [_ | _] = Revs}
    } = DDoc,
    {ok, {NewPos, Rev}} = fabric2_db:update_doc(Db, DDoc#doc{deleted = true}),
    DDoc#doc{
        revs = {NewPos, [Rev | Revs]},
        deleted = true
    }.


job_exists(Db, DDoc) ->
    JobId = job_id(Db, DDoc),
    case couch_jobs:get_job_data(Db, ?INDEX_JOB_TYPE, JobId) of
        {ok, _} -> true;
        {error, not_found} -> false
    end.


job_id(Db, DDoc) ->
    DbName = fabric2_db:name(Db),
    {ok, #mrst{sig = Sig}} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    HexSig = fabric2_util:to_hex(Sig),
    <<HexSig/binary, "-", DbName/binary>>.
