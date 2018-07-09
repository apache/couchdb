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

-module(couchdb_views_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(DELAY, 100).
-define(TIMEOUT, 1000).
-define(WAIT_DELAY_COUNT, 40).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    FooRev = create_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
    query_view(DbName, "foo", "bar"),
    BooRev = create_design_doc(DbName, <<"_design/boo">>, <<"baz">>),
    query_view(DbName, "boo", "baz"),
    {DbName, {FooRev, BooRev}}.

setup_with_docs() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    create_docs(DbName),
    create_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
    DbName.

setup_legacy() ->
    DbName = <<"test">>,
    DbFileName = "test.couch",
    OldDbFilePath = filename:join([?FIXTURESDIR, DbFileName]),
    OldViewName = "3b835456c235b1827e012e25666152f3.view",
    FixtureViewFilePath = filename:join([?FIXTURESDIR, OldViewName]),
    NewViewName = "6cf2c2f766f87b618edf6630b00f8736.view",

    DbDir = config:get("couchdb", "database_dir"),
    ViewDir = config:get("couchdb", "view_index_dir"),
    OldViewFilePath = filename:join([ViewDir, ".test_design", OldViewName]),
    NewViewFilePath = filename:join([ViewDir, ".test_design", "mrview",
                                     NewViewName]),

    NewDbFilePath = filename:join([DbDir, DbFileName]),

    Files = [NewDbFilePath, OldViewFilePath, NewViewFilePath],

    %% make sure there is no left over
    lists:foreach(fun(File) -> file:delete(File) end, Files),

    % copy old db file into db dir
    {ok, _} = file:copy(OldDbFilePath, NewDbFilePath),

    % copy old view file into view dir
    ok = filelib:ensure_dir(OldViewFilePath),

    {ok, _} = file:copy(FixtureViewFilePath, OldViewFilePath),

    {DbName, Files}.

teardown({DbName, _}) ->
    teardown(DbName);
teardown(DbName) when is_binary(DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

teardown_legacy({_DbName, Files}) ->
    lists:foreach(fun(File) -> file:delete(File) end, Files).

view_indexes_cleanup_test_() ->
    {
        "View indexes cleanup",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_have_two_indexes_alive_before_deletion/1,
                    fun should_cleanup_index_file_after_ddoc_deletion/1,
                    fun should_cleanup_all_index_files/1
                ]
            }
        }
    }.

view_group_db_leaks_test_() ->
    {
        "View group db leaks",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup_with_docs/0, fun teardown/1,
                [
                    fun couchdb_1138/1,
                    fun couchdb_1309/1
                ]
            }
        }
    }.

view_group_shutdown_test_() ->
    {
        "View group shutdown",
        {
            setup,
            fun() ->
                meck:new(couch_mrview_index, [passthrough]),
                test_util:start_couch()
            end,
            fun(Ctx) ->
                test_util:stop_couch(Ctx),
                meck:unload()
            end,
            [couchdb_1283()]
        }
    }.

backup_restore_test_() ->
    {
        "Upgrade and bugs related tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup_with_docs/0, fun teardown/1,
                [
                    fun should_not_remember_docs_in_index_after_backup_restore/1
                ]
            }
        }
    }.


upgrade_test_() ->
    {
        "Upgrade tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup_legacy/0, fun teardown_legacy/1,
                [
                    fun should_upgrade_legacy_view_files/1
                ]
            }
        }
    }.

should_not_remember_docs_in_index_after_backup_restore(DbName) ->
    ?_test(begin
        %% COUCHDB-640

        ok = backup_db_file(DbName),
        create_doc(DbName, "doc666"),

        Rows0 = query_view(DbName, "foo", "bar"),
        ?assert(has_doc("doc1", Rows0)),
        ?assert(has_doc("doc2", Rows0)),
        ?assert(has_doc("doc3", Rows0)),
        ?assert(has_doc("doc666", Rows0)),

        ?assertEqual(ok, restore_backup_db_file(DbName)),

        Rows1 = query_view(DbName, "foo", "bar"),
        ?assert(has_doc("doc1", Rows1)),
        ?assert(has_doc("doc2", Rows1)),
        ?assert(has_doc("doc3", Rows1)),
        ?assertNot(has_doc("doc666", Rows1))
      end).

should_upgrade_legacy_view_files({DbName, Files}) ->
    ?_test(begin
        [_NewDbFilePath, OldViewFilePath, NewViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),

        % ensure old header
        OldHeader = read_header(OldViewFilePath),
        ?assertMatch(#index_header{}, OldHeader),

        % query view for expected results
        Rows0 = query_view(DbName, "test", "test"),
        ?assertEqual(2, length(Rows0)),

        % ensure old file gone
        ?assertNot(filelib:is_regular(OldViewFilePath)),

        % add doc to trigger update
        DocUrl = db_url(DbName) ++ "/boo",
        {ok, _, _, _} = test_request:put(
            DocUrl, [{"Content-Type", "application/json"}], <<"{\"a\":3}">>),

        % query view for expected results
        Rows1 = query_view(DbName, "test", "test"),
        ?assertEqual(3, length(Rows1)),

        % ensure new header
        timer:sleep(2000),  % have to wait for awhile to upgrade the index
        NewHeader = read_header(NewViewFilePath),
        ?assertMatch(#mrheader{}, NewHeader)
    end).


should_have_two_indexes_alive_before_deletion({DbName, _}) ->
    view_cleanup(DbName),
    ?_assertEqual(2, count_index_files(DbName)).

should_cleanup_index_file_after_ddoc_deletion({DbName, {FooRev, _}}) ->
    delete_design_doc(DbName, <<"_design/foo">>, FooRev),
    view_cleanup(DbName),
    ?_assertEqual(1, count_index_files(DbName)).

should_cleanup_all_index_files({DbName, {FooRev, BooRev}})->
    delete_design_doc(DbName, <<"_design/foo">>, FooRev),
    delete_design_doc(DbName, <<"_design/boo">>, BooRev),
    view_cleanup(DbName),
    ?_assertEqual(0, count_index_files(DbName)).

couchdb_1138(DbName) ->
    ?_test(begin
        {ok, IndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>),
        ?assert(is_pid(IndexerPid)),
        ?assert(is_process_alive(IndexerPid)),
        ?assertEqual(2, count_users(DbName)),

        wait_indexer(IndexerPid),

        Rows0 = query_view(DbName, "foo", "bar"),
        ?assertEqual(3, length(Rows0)),
        ?assertEqual(2, count_users(DbName)),
        ?assert(is_process_alive(IndexerPid)),

        create_doc(DbName, "doc1000"),
        Rows1 = query_view(DbName, "foo", "bar"),
        ?assertEqual(4, length(Rows1)),
        ?assertEqual(2, count_users(DbName)),

        ?assert(is_process_alive(IndexerPid)),

        compact_db(DbName),
        ?assert(is_process_alive(IndexerPid)),

        compact_view_group(DbName, "foo"),
        ?assertEqual(2, count_users(DbName)),

        ?assert(is_process_alive(IndexerPid)),

        create_doc(DbName, "doc1001"),
        Rows2 = query_view(DbName, "foo", "bar"),
        ?assertEqual(5, length(Rows2)),
        ?assertEqual(2, count_users(DbName)),

        ?assert(is_process_alive(IndexerPid))
    end).

couchdb_1309(DbName) ->
    ?_test(begin
        {ok, IndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>),
        ?assert(is_pid(IndexerPid)),
        ?assert(is_process_alive(IndexerPid)),
        ?assertEqual(2, count_users(DbName)),

        wait_indexer(IndexerPid),

        create_doc(DbName, "doc1001"),
        Rows0 = query_view(DbName, "foo", "bar"),
        check_rows_value(Rows0, null),
        ?assertEqual(4, length(Rows0)),
        ?assertEqual(2, count_users(DbName)),

        ?assert(is_process_alive(IndexerPid)),

        update_design_doc(DbName,  <<"_design/foo">>, <<"bar">>),
        {ok, NewIndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>),
        ?assert(is_pid(NewIndexerPid)),
        ?assert(is_process_alive(NewIndexerPid)),
        ?assertNotEqual(IndexerPid, NewIndexerPid),
        UserCnt = case count_users(DbName) of
                      N when N > 2 ->
                          timer:sleep(1000),
                          count_users(DbName);
                      N -> N
                  end,
        ?assertEqual(2, UserCnt),

        Rows1 = query_view(DbName, "foo", "bar", ok),
        ?assertEqual(0, length(Rows1)),
        Rows2 = query_view(DbName, "foo", "bar"),
        check_rows_value(Rows2, 1),
        ?assertEqual(4, length(Rows2)),

        ok = stop_indexer( %% FIXME we need to grab monitor earlier
               fun() -> ok end,
               IndexerPid, ?LINE,
               "old view group is not dead after ddoc update"),

        ok = stop_indexer(
               fun() -> couch_server:delete(DbName, [?ADMIN_USER]) end,
               NewIndexerPid, ?LINE,
               "new view group did not die after DB deletion")
    end).

couchdb_1283() ->
    ?_test(begin
        ok = config:set("couchdb", "max_dbs_open", "3", false),
        ok = config:set("couchdb", "delayed_commits", "false", false),

        {ok, MDb1} = couch_db:create(?tempdb(), [?ADMIN_CTX]),
        DDoc = couch_doc:from_json_obj({[
            {<<"_id">>, <<"_design/foo">>},
            {<<"language">>, <<"javascript">>},
            {<<"views">>, {[
                {<<"foo">>, {[
                    {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                ]}},
                {<<"foo2">>, {[
                    {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                ]}},
                {<<"foo3">>, {[
                    {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                ]}},
                {<<"foo4">>, {[
                    {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                ]}},
                {<<"foo5">>, {[
                    {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                ]}}
            ]}}
        ]}),
        {ok, _} = couch_db:update_doc(MDb1, DDoc, []),
        ok = populate_db(MDb1, 100, 100),
        query_view(couch_db:name(MDb1), "foo", "foo"),
        ok = couch_db:close(MDb1),

        {ok, Pid} = couch_index_server:get_index(
            couch_mrview_index, couch_db:name(MDb1), <<"_design/foo">>),

        % Start and pause compacton
        WaitRef = erlang:make_ref(),
        meck:expect(couch_mrview_index, compact, fun(Db, State, Opts) ->
            receive {WaitRef, From, init} -> ok end,
            From ! {WaitRef, inited},
            receive {WaitRef, go} -> ok end,
            meck:passthrough([Db, State, Opts])
        end),

        {ok, CPid} = gen_server:call(Pid, compact),
        CRef = erlang:monitor(process, CPid),
        ?assert(is_process_alive(CPid)),

        % Make sure that our compactor is waiting for us
        % before we continue our assertions
        CPid ! {WaitRef, self(), init},
        receive {WaitRef, inited} -> ok end,

        % Make sure that a compaction process takes a monitor
        % on the database's main_pid
        ?assertEqual(true, lists:member(CPid, couch_db:monitored_by(MDb1))),

        % Finish compaction to and make sure the monitor
        % disappears
        CPid ! {WaitRef, go},
        wait_for_process_shutdown(CRef, normal,
          {reason, "Failure compacting view group"}),

        % Make sure that the monitor was removed
        ?assertEqual(false, lists:member(CPid, couch_db:monitored_by(MDb1)))
    end).

wait_for_process_shutdown(Pid, ExpectedReason, Error) ->
  receive
      {'DOWN', Pid, process, _, Reason} ->
          ?assertEqual(ExpectedReason, Reason)
  after ?TIMEOUT ->
      erlang:error(
          {assertion_failed,
           [{module, ?MODULE}, {line, ?LINE}, Error]})
  end.


create_doc(DbName, DocId) when is_list(DocId) ->
    create_doc(DbName, ?l2b(DocId));
create_doc(DbName, DocId) when is_binary(DocId) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc666 = couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"value">>, 999}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc666]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc1 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc1">>},
        {<<"value">>, 1}

    ]}),
    Doc2 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc2">>},
        {<<"value">>, 2}

    ]}),
    Doc3 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc3">>},
        {<<"value">>, 3}

    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc1, Doc2, Doc3]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

populate_db(Db, BatchSize, N) when N > 0 ->
    Docs = lists:map(
        fun(_) ->
            couch_doc:from_json_obj({[
                {<<"_id">>, couch_uuids:new()},
                {<<"value">>, base64:encode(crypto:strong_rand_bytes(1000))}
            ]})
        end,
        lists:seq(1, BatchSize)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    populate_db(Db, BatchSize, N - length(Docs));
populate_db(_Db, _, _) ->
    ok.

create_design_doc(DbName, DDName, ViewName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {ViewName, {[
                {<<"map">>, <<"function(doc) { emit(doc.value, null); }">>}
            ]}}
        ]}}
    ]}),
    {ok, Rev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db),
    Rev.

update_design_doc(DbName, DDName, ViewName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    {ok, Doc} = couch_db:open_doc(Db, DDName, [?ADMIN_CTX]),
    {Props} = couch_doc:to_json_obj(Doc, []),
    Rev = couch_util:get_value(<<"_rev">>, Props),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"_rev">>, Rev},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {ViewName, {[
                {<<"map">>, <<"function(doc) { emit(doc.value, 1); }">>}
            ]}}
        ]}}
    ]}),
    {ok, NewRev} = couch_db:update_doc(Db, DDoc, [?ADMIN_CTX]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db),
    NewRev.

delete_design_doc(DbName, DDName, Rev) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"_rev">>, couch_doc:rev_to_str(Rev)},
        {<<"_deleted">>, true}
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, [Rev]),
    couch_db:close(Db).

db_url(DbName) ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(DbName).

query_view(DbName, DDoc, View) ->
    query_view(DbName, DDoc, View, false).

query_view(DbName, DDoc, View, Stale) ->
    {ok, Code, _Headers, Body} = test_request:get(
        db_url(DbName) ++ "/_design/" ++ DDoc ++ "/_view/" ++ View
        ++ case Stale of
               false -> [];
               _ -> "?stale=" ++ atom_to_list(Stale)
           end),
    ?assertEqual(200, Code),
    {Props} = jiffy:decode(Body),
    couch_util:get_value(<<"rows">>, Props, []).

check_rows_value(Rows, Value) ->
    lists:foreach(
        fun({Row}) ->
            ?assertEqual(Value, couch_util:get_value(<<"value">>, Row))
        end, Rows).

view_cleanup(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    couch_mrview:cleanup(Db),
    couch_db:close(Db).

count_users(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    DbPid = couch_db:get_pid(Db),
    {monitored_by, Monitors} = process_info(DbPid, monitored_by),
    CouchFiles = [P || P <- Monitors, couch_file:process_info(P) =/= undefined],
    ok = couch_db:close(Db),
    length(lists:usort(Monitors) -- [self() | CouchFiles]).

count_index_files(DbName) ->
    % call server to fetch the index files
    RootDir = config:get("couchdb", "view_index_dir"),
    length(filelib:wildcard(RootDir ++ "/." ++
        binary_to_list(DbName) ++ "_design"++"/mrview/*")).

has_doc(DocId1, Rows) ->
    DocId = iolist_to_binary(DocId1),
    lists:any(fun({R}) -> lists:member({<<"id">>, DocId}, R) end, Rows).

backup_db_file(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        SrcPath = couch_db:get_filepath(Db),
        Src = if
            is_list(SrcPath) -> SrcPath;
            true -> binary_to_list(SrcPath)
        end,
        ok = copy_tree(Src, Src ++ ".backup")
    after
        couch_db:close(Db)
    end.

restore_backup_db_file(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    Src = couch_db:get_filepath(Db),
    ok = couch_db:close(Db),
    DbPid = couch_db:get_pid(Db),
    exit(DbPid, shutdown),
    ok = copy_tree(Src ++ ".backup", Src),

    test_util:wait(fun() ->
        case couch_server:open(DbName, [{timeout, ?TIMEOUT}]) of
            {ok, WaitDb} ->
                case couch_db:get_pid(WaitDb) == DbPid of
                    true -> wait;
                    false -> ok
                end;
            Else ->
                Else
        end
    end, ?TIMEOUT, ?DELAY).

compact_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = couch_db:start_compact(Db),
    ok = couch_db:close(Db),
    wait_db_compact_done(DbName, ?WAIT_DELAY_COUNT).

wait_db_compact_done(_DbName, 0) ->
    erlang:error({assertion_failed,
                  [{module, ?MODULE},
                   {line, ?LINE},
                   {reason, "DB compaction failed to finish"}]});
wait_db_compact_done(DbName, N) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ok = couch_db:close(Db),
    CompactorPid = couch_db:get_compactor_pid(Db),
    case is_pid(CompactorPid) of
    false ->
        ok;
    true ->
        ok = timer:sleep(?DELAY),
        wait_db_compact_done(DbName, N - 1)
    end.

compact_view_group(DbName, DDocId) when is_list(DDocId) ->
    compact_view_group(DbName, ?l2b("_design/" ++ DDocId));
compact_view_group(DbName, DDocId) when is_binary(DDocId) ->
    ok = couch_mrview:compact(DbName, DDocId),
    wait_view_compact_done(DbName, DDocId, 10).

wait_view_compact_done(_DbName, _DDocId, 0) ->
    erlang:error({assertion_failed,
                  [{module, ?MODULE},
                   {line, ?LINE},
                   {reason, "DB compaction failed to finish"}]});
wait_view_compact_done(DbName, DDocId, N) ->
    {ok, Code, _Headers, Body} = test_request:get(
        db_url(DbName) ++ "/" ++ ?b2l(DDocId) ++ "/_info"),
    ?assertEqual(200, Code),
    {Info} = jiffy:decode(Body),
    {IndexInfo} = couch_util:get_value(<<"view_index">>, Info),
    CompactRunning = couch_util:get_value(<<"compact_running">>, IndexInfo),
    case CompactRunning of
        false ->
            ok;
        true ->
            ok = timer:sleep(?DELAY),
            wait_view_compact_done(DbName, DDocId, N - 1)
    end.

read_header(File) ->
    {ok, Fd} = couch_file:open(File),
    {ok, {_Sig, Header}} = couch_file:read_header(Fd),
    couch_file:close(Fd),
    Header.

stop_indexer(StopFun, Pid, Line, Reason) ->
    case test_util:stop_sync(Pid, StopFun) of
    timeout ->
        erlang:error(
            {assertion_failed,
             [{module, ?MODULE}, {line, Line},
              {reason, Reason}]});
    ok ->
        ok
    end.

wait_indexer(IndexerPid) ->
    test_util:wait(fun() ->
        {ok, Info} = couch_index:get_info(IndexerPid),
        case couch_util:get_value(compact_running, Info) of
            true ->
                wait;
            false ->
                ok
        end
    end).

copy_tree(Src, Dst) ->
    case filelib:is_dir(Src) of
        true ->
            {ok, Files} = file:list_dir(Src),
            copy_tree(Files, Src, Dst);
        false ->
            ok = filelib:ensure_dir(Dst),
            {ok, _} = file:copy(Src, Dst),
            ok
    end.

copy_tree([], _Src, _Dst) ->
    ok;
copy_tree([File | Rest], Src, Dst) ->
    FullSrc = filename:join(Src, File),
    FullDst = filename:join(Dst, File),
    ok = copy_tree(FullSrc, FullDst),
    copy_tree(Rest, Src, Dst).
