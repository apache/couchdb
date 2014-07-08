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

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(DELAY, 100).
-define(TIMEOUT, 1000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    ok = couch_db:close(Db),
    FooRev = create_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
    query_view(DbName, "foo", "bar"),
    BooRev = create_design_doc(DbName, <<"_design/boo">>, <<"baz">>),
    query_view(DbName, "boo", "baz"),
    {DbName, {FooRev, BooRev}}.

setup_with_docs() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    ok = couch_db:close(Db),
    create_docs(DbName),
    create_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
    DbName.

teardown({DbName, _}) ->
    teardown(DbName);
teardown(DbName) when is_binary(DbName) ->
    couch_server:delete(DbName, [?ADMIN_USER]),
    ok.


view_indexes_cleanup_test_() ->
    {
        "View indexes cleanup",
        {
            setup,
            fun start/0, fun stop/1,
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
            fun start/0, fun stop/1,
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
            fun start/0, fun stop/1,
            [couchdb_1283()]
        }
    }.


should_not_remember_docs_in_index_after_backup_restore_test() ->
    %% COUCHDB-640
    start(),
    DbName = setup_with_docs(),

    ok = backup_db_file(DbName),
    create_doc(DbName, "doc666"),

    Rows0 = query_view(DbName, "foo", "bar"),
    ?assert(has_doc("doc1", Rows0)),
    ?assert(has_doc("doc2", Rows0)),
    ?assert(has_doc("doc3", Rows0)),
    ?assert(has_doc("doc666", Rows0)),

    restore_backup_db_file(DbName),

    Rows1 = query_view(DbName, "foo", "bar"),
    ?assert(has_doc("doc1", Rows1)),
    ?assert(has_doc("doc2", Rows1)),
    ?assert(has_doc("doc3", Rows1)),
    ?assertNot(has_doc("doc666", Rows1)),

    teardown(DbName),
    stop(whereis(couch_server_sup)).


should_upgrade_legacy_view_files_test() ->
    start(),

    ok = couch_config:set("query_server_config", "commit_freq", "0", false),

    DbName = <<"test">>,
    DbFileName = "test.couch",
    DbFilePath = filename:join([?FIXTURESDIR, DbFileName]),
    OldViewName = "3b835456c235b1827e012e25666152f3.view",
    FixtureViewFilePath = filename:join([?FIXTURESDIR, OldViewName]),
    NewViewName = "a1c5929f912aca32f13446122cc6ce50.view",

    DbDir = couch_config:get("couchdb", "database_dir"),
    ViewDir = couch_config:get("couchdb", "view_index_dir"),
    OldViewFilePath = filename:join([ViewDir, ".test_design", OldViewName]),
    NewViewFilePath = filename:join([ViewDir, ".test_design", "mrview",
                                     NewViewName]),

    % cleanup
    Files = [
        filename:join([DbDir, DbFileName]),
        OldViewFilePath,
        NewViewFilePath
    ],
    lists:foreach(fun(File) -> file:delete(File) end, Files),

    % copy old db file into db dir
    {ok, _} = file:copy(DbFilePath, filename:join([DbDir, DbFileName])),

    % copy old view file into view dir
    ok = filelib:ensure_dir(filename:join([ViewDir, ".test_design"]) ++ "/"),
    {ok, _} = file:copy(FixtureViewFilePath, OldViewFilePath),

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
    ?assertMatch(#mrheader{}, NewHeader),

    teardown(DbName),
    stop(whereis(couch_server_sup)).


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
        ?assertEqual(2, count_db_refs(DbName)),

        Rows0 = query_view(DbName, "foo", "bar"),
        ?assertEqual(3, length(Rows0)),
        ?assertEqual(2, count_db_refs(DbName)),
        ?assert(is_process_alive(IndexerPid)),

        create_doc(DbName, "doc1000"),
        Rows1 = query_view(DbName, "foo", "bar"),
        ?assertEqual(4, length(Rows1)),
        ?assertEqual(2, count_db_refs(DbName)),
        ?assert(is_process_alive(IndexerPid)),

        Ref1 = get_db_ref_counter(DbName),
        compact_db(DbName),
        Ref2 = get_db_ref_counter(DbName),
        ?assertEqual(2, couch_ref_counter:count(Ref2)),
        ?assertNotEqual(Ref2, Ref1),
        ?assertNot(is_process_alive(Ref1)),
        ?assert(is_process_alive(IndexerPid)),

        compact_view_group(DbName, "foo"),
        ?assertEqual(2, count_db_refs(DbName)),
        Ref3 = get_db_ref_counter(DbName),
        ?assertEqual(Ref3, Ref2),
        ?assert(is_process_alive(IndexerPid)),

        create_doc(DbName, "doc1001"),
        Rows2 = query_view(DbName, "foo", "bar"),
        ?assertEqual(5, length(Rows2)),
        ?assertEqual(2, count_db_refs(DbName)),
        ?assert(is_process_alive(IndexerPid))
    end).

couchdb_1309(DbName) ->
    ?_test(begin
        {ok, IndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>),
        ?assert(is_pid(IndexerPid)),
        ?assert(is_process_alive(IndexerPid)),
        ?assertEqual(2, count_db_refs(DbName)),

        create_doc(DbName, "doc1001"),
        Rows0 = query_view(DbName, "foo", "bar"),
        check_rows_value(Rows0, null),
        ?assertEqual(4, length(Rows0)),
        ?assertEqual(2, count_db_refs(DbName)),
        ?assert(is_process_alive(IndexerPid)),

        update_design_doc(DbName,  <<"_design/foo">>, <<"bar">>),
        {ok, NewIndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>),
        ?assert(is_pid(NewIndexerPid)),
        ?assert(is_process_alive(NewIndexerPid)),
        ?assertNotEqual(IndexerPid, NewIndexerPid),
        ?assertEqual(2, count_db_refs(DbName)),

        Rows1 = query_view(DbName, "foo", "bar", ok),
        ?assertEqual(0, length(Rows1)),
        Rows2 = query_view(DbName, "foo", "bar"),
        check_rows_value(Rows2, 1),
        ?assertEqual(4, length(Rows2)),

        MonRef0 = erlang:monitor(process, IndexerPid),
        receive
            {'DOWN', MonRef0, _, _, _} ->
                ok
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason, "old view group is not dead after ddoc update"}]})
        end,

        MonRef1 = erlang:monitor(process, NewIndexerPid),
        ok = couch_server:delete(DbName, [?ADMIN_USER]),
        receive
            {'DOWN', MonRef1, _, _, _} ->
                ok
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason, "new view group did not die after DB deletion"}]})
        end
    end).

couchdb_1283() ->
    ?_test(begin
        ok = couch_config:set("couchdb", "max_dbs_open", "3", false),
        ok = couch_config:set("couchdb", "delayed_commits", "false", false),

        {ok, MDb1} = couch_db:create(?tempdb(), [?ADMIN_USER]),
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
        query_view(MDb1#db.name, "foo", "foo"),
        ok = couch_db:close(MDb1),

        {ok, Db1} = couch_db:create(?tempdb(), [?ADMIN_USER]),
        ok = couch_db:close(Db1),
        {ok, Db2} = couch_db:create(?tempdb(), [?ADMIN_USER]),
        ok = couch_db:close(Db2),
        {ok, Db3} = couch_db:create(?tempdb(), [?ADMIN_USER]),
        ok = couch_db:close(Db3),

        Writer1 = spawn_writer(Db1#db.name),
        Writer2 = spawn_writer(Db2#db.name),

        ?assert(is_process_alive(Writer1)),
        ?assert(is_process_alive(Writer2)),

        ?assertEqual(ok, get_writer_status(Writer1)),
        ?assertEqual(ok, get_writer_status(Writer2)),

        {ok, MonRef} = couch_mrview:compact(MDb1#db.name, <<"_design/foo">>,
                                            [monitor]),

        Writer3 = spawn_writer(Db3#db.name),
        ?assert(is_process_alive(Writer3)),
        ?assertEqual({error, all_dbs_active}, get_writer_status(Writer3)),

        ?assert(is_process_alive(Writer1)),
        ?assert(is_process_alive(Writer2)),
        ?assert(is_process_alive(Writer3)),

        receive
            {'DOWN', MonRef, process, _, Reason} ->
                ?assertEqual(normal, Reason)
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason, "Failure compacting view group"}]})
        end,

        ?assertEqual(ok, writer_try_again(Writer3)),
        ?assertEqual(ok, get_writer_status(Writer3)),

        ?assert(is_process_alive(Writer1)),
        ?assert(is_process_alive(Writer2)),
        ?assert(is_process_alive(Writer3)),

        ?assertEqual(ok, stop_writer(Writer1)),
        ?assertEqual(ok, stop_writer(Writer2)),
        ?assertEqual(ok, stop_writer(Writer3))
    end).

create_doc(DbName, DocId) when is_list(DocId) ->
    create_doc(DbName, ?l2b(DocId));
create_doc(DbName, DocId) when is_binary(DocId) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    Doc666 = couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"value">>, 999}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc666]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
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
                {<<"value">>, base64:encode(crypto:rand_bytes(1000))}
            ]})
        end,
        lists:seq(1, BatchSize)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    populate_db(Db, BatchSize, N - length(Docs));
populate_db(_Db, _, _) ->
    ok.

create_design_doc(DbName, DDName, ViewName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
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
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    {ok, Doc} = couch_db:open_doc(Db, DDName, [?ADMIN_USER]),
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
    {ok, NewRev} = couch_db:update_doc(Db, DDoc, [?ADMIN_USER]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db),
    NewRev.

delete_design_doc(DbName, DDName, Rev) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"_rev">>, couch_doc:rev_to_str(Rev)},
        {<<"_deleted">>, true}
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, [Rev]),
    couch_db:close(Db).

db_url(DbName) ->
    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
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
    {Props} = ejson:decode(Body),
    couch_util:get_value(<<"rows">>, Props, []).

check_rows_value(Rows, Value) ->
    lists:foreach(
        fun({Row}) ->
            ?assertEqual(Value, couch_util:get_value(<<"value">>, Row))
        end, Rows).

view_cleanup(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    couch_mrview:cleanup(Db),
    couch_db:close(Db).

get_db_ref_counter(DbName) ->
    {ok, #db{fd_ref_counter = Ref} = Db} = couch_db:open_int(DbName, []),
    ok = couch_db:close(Db),
    Ref.

count_db_refs(DbName) ->
    Ref = get_db_ref_counter(DbName),
    % have to sleep a bit to let couchdb cleanup all refs and leave only
    % active ones. otherwise the related tests will randomly fail due to
    % count number mismatch
    timer:sleep(200),
    couch_ref_counter:count(Ref).

count_index_files(DbName) ->
    % call server to fetch the index files
    RootDir = couch_config:get("couchdb", "view_index_dir"),
    length(filelib:wildcard(RootDir ++ "/." ++
        binary_to_list(DbName) ++ "_design"++"/mrview/*")).

has_doc(DocId1, Rows) ->
    DocId = iolist_to_binary(DocId1),
    lists:any(fun({R}) -> lists:member({<<"id">>, DocId}, R) end, Rows).

backup_db_file(DbName) ->
    DbDir = couch_config:get("couchdb", "database_dir"),
    DbFile = filename:join([DbDir, ?b2l(DbName) ++ ".couch"]),
    {ok, _} = file:copy(DbFile, DbFile ++ ".backup"),
    ok.

restore_backup_db_file(DbName) ->
    DbDir = couch_config:get("couchdb", "database_dir"),
    stop(whereis(couch_server_sup)),
    DbFile = filename:join([DbDir, ?b2l(DbName) ++ ".couch"]),
    ok = file:delete(DbFile),
    ok = file:rename(DbFile ++ ".backup", DbFile),
    start(),
    ok.

compact_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = couch_db:start_compact(Db),
    ok = couch_db:close(Db),
    wait_db_compact_done(DbName, 10).

wait_db_compact_done(_DbName, 0) ->
    erlang:error({assertion_failed,
                  [{module, ?MODULE},
                   {line, ?LINE},
                   {reason, "DB compaction failed to finish"}]});
wait_db_compact_done(DbName, N) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ok = couch_db:close(Db),
    case is_pid(Db#db.compactor_pid) of
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
    {Info} = ejson:decode(Body),
    {IndexInfo} = couch_util:get_value(<<"view_index">>, Info),
    CompactRunning = couch_util:get_value(<<"compact_running">>, IndexInfo),
    case CompactRunning of
        false ->
            ok;
        true ->
            ok = timer:sleep(?DELAY),
            wait_view_compact_done(DbName, DDocId, N - 1)
    end.

spawn_writer(DbName) ->
    Parent = self(),
    spawn(fun() ->
        process_flag(priority, high),
        writer_loop(DbName, Parent)
    end).

get_writer_status(Writer) ->
    Ref = make_ref(),
    Writer ! {get_status, Ref},
    receive
        {db_open, Ref} ->
            ok;
        {db_open_error, Error, Ref} ->
            Error
    after ?TIMEOUT ->
        timeout
    end.

writer_try_again(Writer) ->
    Ref = make_ref(),
    Writer ! {try_again, Ref},
    receive
        {ok, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end.

stop_writer(Writer) ->
    Ref = make_ref(),
    Writer ! {stop, Ref},
    receive
        {ok, Ref} ->
            ok
    after ?TIMEOUT ->
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Timeout on stopping process"}]})
    end.

writer_loop(DbName, Parent) ->
    case couch_db:open_int(DbName, []) of
        {ok, Db} ->
            writer_loop_1(Db, Parent);
        Error ->
            writer_loop_2(DbName, Parent, Error)
    end.

writer_loop_1(Db, Parent) ->
    receive
        {get_status, Ref} ->
            Parent ! {db_open, Ref},
            writer_loop_1(Db, Parent);
        {stop, Ref} ->
            ok = couch_db:close(Db),
            Parent ! {ok, Ref}
    end.

writer_loop_2(DbName, Parent, Error) ->
    receive
        {get_status, Ref} ->
            Parent ! {db_open_error, Error, Ref},
            writer_loop_2(DbName, Parent, Error);
        {try_again, Ref} ->
            Parent ! {ok, Ref},
            writer_loop(DbName, Parent)
    end.

read_header(File) ->
    {ok, Fd} = couch_file:open(File),
    {ok, {_Sig, Header}} = couch_file:read_header(Fd),
    couch_file:close(Fd),
    Header.
