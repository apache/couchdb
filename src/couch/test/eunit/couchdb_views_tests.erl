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
-define(OLD_COLLATOR_VERSION, [1, 1, 1, 1]).
-define(HEADER_WRITE_WAIT_TIMEOUT, 4500).

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

% See src/couch/test/eunit/fixtures for fixture files
%
setup_legacy_2x() ->
    % see src/couch/test/eunit/fixtures folder
    DbName = "test",
    OldViewName = "6cf2c2f766f87b618edf6630b00f8736.view",
    NewViewName = "a1c5929f912aca32f13446122cc6ce50.view",
    setup_legacy(DbName, OldViewName, NewViewName).

setup_legacy_3_2_1() ->
    DbName = "db321",
    ViewName = "15a5cb17365a99cd9ddc7327c82bbd0d.view",
    % View signature stays the same
    setup_legacy(DbName, ViewName, ViewName).

setup_collator_test1() ->
    DbName = "colltest1",
    ViewName = "1f2c24bc334d701c2048f85e7438eef1.view",
    % View signature stays the same
    setup_legacy(DbName, ViewName, ViewName).

setup_legacy(DbName, OldViewName, NewViewName) when
    is_list(DbName), is_list(OldViewName), is_list(NewViewName)
->
    DbFileName = DbName ++ ".couch",
    OldDbFilePath = filename:join([?FIXTURESDIR, DbFileName]),
    FixtureViewFilePath = filename:join([?FIXTURESDIR, OldViewName]),

    DbDir = config:get("couchdb", "database_dir"),
    ViewDir = config:get("couchdb", "view_index_dir"),
    OldViewFilePath = filename:join([
        ViewDir,
        "." ++ DbName ++ "_design",
        "mrview",
        OldViewName
    ]),
    NewViewFilePath = filename:join([
        ViewDir,
        "." ++ DbName ++ "_design",
        "mrview",
        NewViewName
    ]),

    NewDbFilePath = filename:join([DbDir, DbFileName]),

    Files = [NewDbFilePath, OldViewFilePath, NewViewFilePath],

    %% make sure there is no left over
    lists:foreach(fun(File) -> file:delete(File) end, Files),

    % copy old db file into db dir
    {ok, _} = file:copy(OldDbFilePath, NewDbFilePath),

    % copy old view file into view dir
    ok = filelib:ensure_dir(OldViewFilePath),

    {ok, _} = file:copy(FixtureViewFilePath, OldViewFilePath),

    {?l2b(DbName), Files}.

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
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
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
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup_with_docs/0,
                fun teardown/1,
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
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup_with_docs/0,
                fun teardown/1,
                [
                    fun should_not_remember_docs_in_index_after_backup_restore/1
                ]
            }
        }
    }.

upgrade_2x_test_() ->
    {
        "Upgrade 2x tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup_legacy_2x/0,
                fun teardown_legacy/1,
                [
                    fun should_upgrade_legacy_2x_view_files/1
                ]
            }
        }
    }.

upgrade_3_2_1_test_() ->
    {
        "Upgrade 3.2.1 tests",
        {
            foreach,
            fun() ->
                Ctx = test_util:start_couch(),
                DbFiles = setup_legacy_3_2_1(),
                {Ctx, DbFiles}
            end,
            fun({Ctx, DbFiles}) ->
                teardown_legacy(DbFiles),
                test_util:stop_couch(Ctx)
            end,
            [
                fun should_upgrade_legacy_3_2_1_view_files/1,
                fun can_disable_auto_commit_on_view_upgrade/1
            ]
        }
    }.

multiple_view_collators_test_() ->
    {
        "Test views with multiple collators",
        {
            foreach,
            fun() ->
                Ctx = test_util:start_couch(),
                DbFiles = setup_collator_test1(),
                {Ctx, DbFiles}
            end,
            fun({Ctx, DbFiles}) ->
                teardown_legacy(DbFiles),
                test_util:stop_couch(Ctx)
            end,
            [
                fun can_read_views_with_old_collators/1,
                fun can_update_views_with_old_collators/1
            ]
        }
    }.

autocompact_view_to_upgrade_collators_test_() ->
    {
        "Auto compactions triggered to update collators",
        {
            foreach,
            fun() ->
                Ctx = test_util:start_couch([smoosh]),
                DbFiles = setup_collator_test1(),
                {Ctx, DbFiles}
            end,
            fun({Ctx, DbFiles}) ->
                teardown_legacy(DbFiles),
                test_util:stop_couch(Ctx)
            end,
            [
                fun view_collator_auto_upgrade_on_open/1,
                fun view_collator_auto_upgrade_on_update/1,
                fun view_collator_auto_upgrade_can_be_disabled/1
            ]
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

should_upgrade_legacy_2x_view_files({DbName, Files}) ->
    ?_test(begin
        [_NewDbFilePath, OldViewFilePath, NewViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),

        % ensure old header
        OldHeader = read_header(OldViewFilePath),
        ?assertEqual(6, tuple_size(OldHeader)),
        ?assertMatch(mrheader, element(1, OldHeader)),

        % query view for expected results
        Rows0 = query_view(DbName, "test", "test"),
        ?assertEqual(3, length(Rows0)),

        % ensure old file gone
        ?assertNot(filelib:is_regular(OldViewFilePath)),

        % add doc to trigger update
        DocUrl = db_url(DbName) ++ "/bar",
        {ok, _, _, _} = test_request:put(
            DocUrl, [{"Content-Type", "application/json"}], <<"{\"a\":4}">>
        ),

        % query view for expected results
        Rows1 = query_view(DbName, "test", "test"),
        ?assertEqual(4, length(Rows1)),

        % ensure new header

        % have to wait for awhile to upgrade the index
        wait_mrheader_record(NewViewFilePath),
        NewHeader = read_header(NewViewFilePath),
        ?assertMatch(#mrheader{}, NewHeader),

        % assert that 2.x header was upgraded with a view_info map
        ViewInfo = NewHeader#mrheader.view_info,
        ?assert(is_map(ViewInfo)),
        Ver = tuple_to_list(couch_ejson_compare:get_collator_version()),
        ?assertMatch(#{ucol_vs := [Ver]}, ViewInfo),

        NewViewStatus = hd(NewHeader#mrheader.view_states),
        ?assertEqual(5, tuple_size(NewViewStatus))
    end).

should_upgrade_legacy_3_2_1_view_files({_, {DbName, Files}}) ->
    ?_test(begin
        [_NewDbFilePath, OldViewFilePath, NewViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),

        % preliminary assert that we expect view signature and view files names
        % to stay exactly the same
        ?assertEqual(OldViewFilePath, NewViewFilePath),

        % ensure old header
        OldHeader = read_header(OldViewFilePath),
        ?assertEqual(5, tuple_size(OldHeader)),
        ?assertMatch(mrheader, element(1, OldHeader)),

        % query view for expected results
        Rows0 = query_view(DbName, "ddoc321", "view321"),
        ?assertEqual(2, length(Rows0)),

        % have to wait for a while to write to the index
        % with [view_upgrade] commit_on_header_upgrade should happen after open
        wait_mrheader_record(NewViewFilePath),
        NewHeader = read_header(NewViewFilePath),
        ?assertMatch(#mrheader{}, NewHeader),

        % assert that 3.2.1 header was upgraded with a view_info map
        ViewInfo = NewHeader#mrheader.view_info,
        ?assert(is_map(ViewInfo)),
        Ver = tuple_to_list(couch_ejson_compare:get_collator_version()),
        ?assertMatch(#{ucol_vs := [Ver]}, ViewInfo),

        NewViewStatus = hd(NewHeader#mrheader.view_states),
        ?assertEqual(5, tuple_size(NewViewStatus)),

        NewSig = get_signature(DbName, "ddoc321"),
        OldSig = filename:basename(OldViewFilePath, ".view"),
        ?assertEqual(OldSig, ?b2l(NewSig))
    end).

can_disable_auto_commit_on_view_upgrade({_, {DbName, Files}}) ->
    ?_test(begin
        [_NewDbFilePath, OldViewFilePath, NewViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),
        ok = config:set(
            "view_upgrade",
            "commit_on_header_upgrade",
            "false",
            false
        ),

        % preliminary assert that we expect view signature and view files names
        % to stay exactly the same
        ?assertEqual(OldViewFilePath, NewViewFilePath),

        % ensure old header
        OldHeader = read_header(OldViewFilePath),
        ?assertEqual(5, tuple_size(OldHeader)),
        ?assertMatch(mrheader, element(1, OldHeader)),

        % query view for expected results
        Rows0 = query_view(DbName, "ddoc321", "view321"),
        ?assertEqual(2, length(Rows0)),

        % ensure old header is still there after a query as we intend not to
        % auto-commit after header open
        AfterQueryHeader = read_header(NewViewFilePath),
        ?assertEqual(5, tuple_size(AfterQueryHeader)),
        ?assertMatch(mrheader, element(1, AfterQueryHeader)),

        % add 3 new documents
        create_docs(DbName),

        % query view for expected results
        Rows1 = query_view(DbName, "ddoc321", "view321"),
        ?assertEqual(5, length(Rows1)),

        % ensure old file is still there
        ?assert(filelib:is_regular(OldViewFilePath)),

        % ensure new header

        % have to wait for awhile to write to the index
        wait_mrheader_record(NewViewFilePath),
        NewHeader = read_header(NewViewFilePath),
        ?assertMatch(#mrheader{}, NewHeader),

        % assert that 3.2.1 header was upgraded with a view_info map
        ViewInfo = NewHeader#mrheader.view_info,
        ?assert(is_map(ViewInfo)),
        Ver = tuple_to_list(couch_ejson_compare:get_collator_version()),
        ?assertMatch(#{ucol_vs := [Ver]}, ViewInfo),

        NewViewStatus = hd(NewHeader#mrheader.view_states),
        ?assertEqual(5, tuple_size(NewViewStatus)),

        NewSig = get_signature(DbName, "ddoc321"),
        OldSig = filename:basename(OldViewFilePath, ".view"),
        ?assertEqual(OldSig, ?b2l(NewSig))
    end).

can_read_views_with_old_collators({_, {DbName, Files}}) ->
    ?_test(begin
        [_NewDbFilePath, ViewFilePath, ViewFilePath] = Files,

        % check that there is an old (bogus) collator version
        Header1 = read_header(ViewFilePath),
        ViewInfo1 = Header1#mrheader.view_info,
        ?assert(is_map(ViewInfo1)),
        ?assertMatch(#{ucol_vs := [?OLD_COLLATOR_VERSION]}, ViewInfo1),

        % view query works with the old collator version
        Rows0 = query_view(DbName, "colltest1ddoc", "colltest1view"),
        ?assertEqual(2, length(Rows0))
    end).

can_update_views_with_old_collators({_, {DbName, Files}}) ->
    ?_test(begin
        [_NewDbFilePath, ViewFilePath, ViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),

        % check that there is an old (bogus) collator version
        Header1 = read_header(ViewFilePath),
        ViewInfo1 = Header1#mrheader.view_info,
        ?assert(is_map(ViewInfo1)),
        ?assertMatch(#{ucol_vs := [?OLD_COLLATOR_VERSION]}, ViewInfo1),

        create_docs(DbName),
        Rows1 = query_view(DbName, "colltest1ddoc", "colltest1view"),
        ?assertEqual(5, length(Rows1)),

        % ensure old view file is still there
        ?assert(filelib:is_regular(ViewFilePath)),

        % should have two collator versions
        CurVer = tuple_to_list(couch_ejson_compare:get_collator_version()),
        ExpVersions = [?OLD_COLLATOR_VERSION, CurVer],
        ok = wait_collator_versions(ExpVersions, ViewFilePath),
        Header2 = read_header(ViewFilePath),
        ViewInfo2 = Header2#mrheader.view_info,
        ?assertMatch(#{ucol_vs := ExpVersions}, ViewInfo2)
    end).

view_collator_auto_upgrade_on_open({_, {DbName, Files}}) ->
    ?_test(begin
        [_NewDbFilePath, ViewFilePath, ViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),

        % quick sanity check the test setup
        Header1 = read_header(ViewFilePath),
        ViewInfo1 = Header1#mrheader.view_info,
        ?assertMatch(#{ucol_vs := [?OLD_COLLATOR_VERSION]}, ViewInfo1),

        % make sure smoosh is active
        smoosh:resume(),

        % query the view
        Rows = query_view(DbName, "colltest1ddoc", "colltest1view"),
        ?assertEqual(2, length(Rows)),

        CurVer = tuple_to_list(couch_ejson_compare:get_collator_version()),
        wait_collator_versions([CurVer], ViewFilePath),
        Header2 = read_header(ViewFilePath),
        ViewInfo2 = Header2#mrheader.view_info,
        ?assertMatch(#{ucol_vs := [CurVer]}, ViewInfo2),

        % query the view again
        ?assertEqual(Rows, query_view(DbName, "colltest1ddoc", "colltest1view"))
    end).

view_collator_auto_upgrade_on_update({_, {DbName, Files}}) ->
    ?_test(begin
        [_NewDbFilePath, ViewFilePath, ViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),

        % quick sanity check the test setup
        Header1 = read_header(ViewFilePath),
        ViewInfo1 = Header1#mrheader.view_info,
        ?assertMatch(#{ucol_vs := [?OLD_COLLATOR_VERSION]}, ViewInfo1),

        % stop smoosh so the open/read trigger doesn't fire
        application:stop(smoosh),

        % open the view so after smoosh starts it won't trigger
        % the open auto-update event
        Rows0 = query_view(DbName, "colltest1ddoc", "colltest1view"),
        ?assertEqual(2, length(Rows0)),

        % update the db
        create_docs(DbName),

        % start smoosh
        application:start(smoosh),
        smoosh:resume(),

        % query the view to trigger an index commit event
        Rows1 = query_view(DbName, "colltest1ddoc", "colltest1view"),
        ?assertEqual(5, length(Rows1)),

        CurVer = tuple_to_list(couch_ejson_compare:get_collator_version()),
        wait_collator_versions([CurVer], ViewFilePath),
        Header2 = read_header(ViewFilePath),
        ViewInfo2 = Header2#mrheader.view_info,
        ?assertMatch(#{ucol_vs := [CurVer]}, ViewInfo2)
    end).

view_collator_auto_upgrade_can_be_disabled({_, {DbName, Files}}) ->
    ?_test(begin
        [_NewDbFilePath, ViewFilePath, ViewFilePath] = Files,
        ok = config:set("query_server_config", "commit_freq", "0", false),
        ok = config:set(
            "view_upgrade",
            "compact_on_collator_upgrade",
            "false",
            false
        ),

        % quick sanity check the test setup
        Header1 = read_header(ViewFilePath),
        ViewInfo1 = Header1#mrheader.view_info,
        ?assertMatch(#{ucol_vs := [?OLD_COLLATOR_VERSION]}, ViewInfo1),

        % activate smoosh
        smoosh:resume(),

        % query the view
        Rows0 = query_view(DbName, "colltest1ddoc", "colltest1view"),
        ?assertEqual(2, length(Rows0)),

        % update the db and query again to trigger an index commit
        create_docs(DbName),
        Rows1 = query_view(DbName, "colltest1ddoc", "colltest1view"),
        ?assertEqual(5, length(Rows1)),

        % View header doesn't change
        CurVer = tuple_to_list(couch_ejson_compare:get_collator_version()),
        ExpVersions = [?OLD_COLLATOR_VERSION, CurVer],
        wait_collator_versions(ExpVersions, ViewFilePath),
        Header2 = read_header(ViewFilePath),
        ViewInfo2 = Header2#mrheader.view_info,
        ?assertMatch(#{ucol_vs := ExpVersions}, ViewInfo2)
    end).

should_have_two_indexes_alive_before_deletion({DbName, _}) ->
    view_cleanup(DbName),
    ?_assertEqual(2, count_index_files(DbName)).

should_cleanup_index_file_after_ddoc_deletion({DbName, {FooRev, _}}) ->
    delete_design_doc(DbName, <<"_design/foo">>, FooRev),
    view_cleanup(DbName),
    ?_assertEqual(1, count_index_files(DbName)).

should_cleanup_all_index_files({DbName, {FooRev, BooRev}}) ->
    delete_design_doc(DbName, <<"_design/foo">>, FooRev),
    delete_design_doc(DbName, <<"_design/boo">>, BooRev),
    view_cleanup(DbName),
    ?_assertEqual(0, count_index_files(DbName)).

couchdb_1138(DbName) ->
    ?_test(begin
        {ok, IndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>
        ),
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
            couch_mrview_index, DbName, <<"_design/foo">>
        ),
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

        update_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
        {ok, NewIndexerPid} = couch_index_server:get_index(
            couch_mrview_index, DbName, <<"_design/foo">>
        ),
        ?assert(is_pid(NewIndexerPid)),
        ?assert(is_process_alive(NewIndexerPid)),
        ?assertNotEqual(IndexerPid, NewIndexerPid),
        UserCnt =
            case count_users(DbName) of
                N when N > 2 ->
                    timer:sleep(1000),
                    count_users(DbName);
                N ->
                    N
            end,
        ?assertEqual(2, UserCnt),

        Rows1 = query_view(DbName, "foo", "bar", ok),
        ?assertEqual(0, length(Rows1)),
        Rows2 = query_view(DbName, "foo", "bar"),
        check_rows_value(Rows2, 1),
        ?assertEqual(4, length(Rows2)),

        %% FIXME we need to grab monitor earlier
        ok = stop_indexer(
            fun() -> ok end,
            IndexerPid,
            ?LINE,
            "old view group is not dead after ddoc update"
        ),

        ok = stop_indexer(
            fun() -> couch_server:delete(DbName, [?ADMIN_USER]) end,
            NewIndexerPid,
            ?LINE,
            "new view group did not die after DB deletion"
        )
    end).

couchdb_1283() ->
    ?_test(begin
        ok = config:set("couchdb", "max_dbs_open", "3", false),

        {ok, MDb1} = couch_db:create(?tempdb(), [?ADMIN_CTX]),
        DDoc = couch_doc:from_json_obj(
            {[
                {<<"_id">>, <<"_design/foo">>},
                {<<"language">>, <<"javascript">>},
                {<<"views">>,
                    {[
                        {<<"foo">>,
                            {[
                                {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                            ]}},
                        {<<"foo2">>,
                            {[
                                {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                            ]}},
                        {<<"foo3">>,
                            {[
                                {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                            ]}},
                        {<<"foo4">>,
                            {[
                                {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                            ]}},
                        {<<"foo5">>,
                            {[
                                {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
                            ]}}
                    ]}}
            ]}
        ),
        {ok, _} = couch_db:update_doc(MDb1, DDoc, []),
        ok = populate_db(MDb1, 100, 100),
        query_view(couch_db:name(MDb1), "foo", "foo"),
        ok = couch_db:close(MDb1),

        {ok, Pid} = couch_index_server:get_index(
            couch_mrview_index, couch_db:name(MDb1), <<"_design/foo">>
        ),

        % Start and pause compacton
        WaitRef = erlang:make_ref(),
        meck:expect(couch_mrview_index, compact, fun(Db, State, Opts) ->
            receive
                {WaitRef, From, init} -> ok
            end,
            From ! {WaitRef, inited},
            receive
                {WaitRef, go} -> ok
            end,
            meck:passthrough([Db, State, Opts])
        end),

        {ok, CPid} = gen_server:call(Pid, compact),
        CRef = erlang:monitor(process, CPid),
        ?assert(is_process_alive(CPid)),

        % Make sure that our compactor is waiting for us
        % before we continue our assertions
        CPid ! {WaitRef, self(), init},
        receive
            {WaitRef, inited} -> ok
        end,

        % Make sure that a compaction process takes a monitor
        % on the database's main_pid
        ?assertEqual(true, lists:member(CPid, couch_db:monitored_by(MDb1))),

        % Finish compaction to and make sure the monitor
        % disappears
        CPid ! {WaitRef, go},
        wait_for_process_shutdown(
            CRef,
            normal,
            {reason, "Failure compacting view group"}
        ),

        % Make sure that the monitor was removed
        ?assertEqual(false, lists:member(CPid, couch_db:monitored_by(MDb1)))
    end).

wait_for_process_shutdown(Pid, ExpectedReason, Error) ->
    receive
        {'DOWN', Pid, process, _, Reason} ->
            ?assertEqual(ExpectedReason, Reason)
    after ?TIMEOUT ->
        erlang:error(
            {assertion_failed, [{module, ?MODULE}, {line, ?LINE}, Error]}
        )
    end.

create_doc(DbName, DocId) when is_list(DocId) ->
    create_doc(DbName, ?l2b(DocId));
create_doc(DbName, DocId) when is_binary(DocId) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc666 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DocId},
            {<<"value">>, 999}
        ]}
    ),
    {ok, _} = couch_db:update_docs(Db, [Doc666]),
    couch_db:close(Db).

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    Doc1 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc1">>},
            {<<"value">>, 1}
        ]}
    ),
    Doc2 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc2">>},
            {<<"value">>, 2}
        ]}
    ),
    Doc3 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc3">>},
            {<<"value">>, 3}
        ]}
    ),
    {ok, _} = couch_db:update_docs(Db, [Doc1, Doc2, Doc3]),
    couch_db:close(Db).

populate_db(Db, BatchSize, N) when N > 0 ->
    Docs = lists:map(
        fun(_) ->
            couch_doc:from_json_obj(
                {[
                    {<<"_id">>, couch_uuids:new()},
                    {<<"value">>, base64:encode(crypto:strong_rand_bytes(1000))}
                ]}
            )
        end,
        lists:seq(1, BatchSize)
    ),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    populate_db(Db, BatchSize, N - length(Docs));
populate_db(_Db, _, _) ->
    ok.

create_design_doc(DbName, DDName, ViewName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDName},
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
    {ok, Rev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:close(Db),
    Rev.

update_design_doc(DbName, DDName, ViewName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    {ok, Doc} = couch_db:open_doc(Db, DDName, [?ADMIN_CTX]),
    {Props} = couch_doc:to_json_obj(Doc, []),
    Rev = couch_util:get_value(<<"_rev">>, Props),
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDName},
            {<<"_rev">>, Rev},
            {<<"language">>, <<"javascript">>},
            {<<"views">>,
                {[
                    {ViewName,
                        {[
                            {<<"map">>, <<"function(doc) { emit(doc.value, 1); }">>}
                        ]}}
                ]}}
        ]}
    ),
    {ok, NewRev} = couch_db:update_doc(Db, DDoc, [?ADMIN_CTX]),
    couch_db:close(Db),
    NewRev.

delete_design_doc(DbName, DDName, Rev) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDName},
            {<<"_rev">>, couch_doc:rev_to_str(Rev)},
            {<<"_deleted">>, true}
        ]}
    ),
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
        db_url(DbName) ++ "/_design/" ++ DDoc ++ "/_view/" ++ View ++
            case Stale of
                false -> [];
                _ -> "?stale=" ++ atom_to_list(Stale)
            end
    ),
    ?assertEqual(200, Code),
    {Props} = jiffy:decode(Body),
    couch_util:get_value(<<"rows">>, Props, []).

get_signature(DbName, DDoc) ->
    Url = db_url(DbName) ++ "/_design/" ++ DDoc ++ "/_info",
    {ok, Code, _Headers, Body} = test_request:get(Url),
    ?assertEqual(200, Code),
    MapBody = jiffy:decode(Body, [return_maps]),
    #{<<"view_index">> := #{<<"signature">> := Sig}} = MapBody,
    Sig.

check_rows_value(Rows, Value) ->
    lists:foreach(
        fun({Row}) ->
            ?assertEqual(Value, couch_util:get_value(<<"value">>, Row))
        end,
        Rows
    ).

view_cleanup(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    couch_mrview:cleanup(Db),
    couch_db:close(Db).

count_users(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    DbPid = couch_db:get_pid(Db),
    {monitored_by, Monitors0} = process_info(DbPid, monitored_by),
    Monitors = lists:filter(fun is_pid/1, Monitors0),
    CouchFiles = [P || P <- Monitors, couch_file:process_info(P) =/= undefined],
    ok = couch_db:close(Db),
    length(lists:usort(Monitors) -- [self() | CouchFiles]).

count_index_files(DbName) ->
    % call server to fetch the index files
    RootDir = config:get("couchdb", "view_index_dir"),
    length(
        filelib:wildcard(
            RootDir ++ "/." ++
                binary_to_list(DbName) ++ "_design" ++ "/mrview/*"
        )
    ).

has_doc(DocId1, Rows) ->
    DocId = iolist_to_binary(DocId1),
    lists:any(fun({R}) -> lists:member({<<"id">>, DocId}, R) end, Rows).

backup_db_file(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        SrcPath = couch_db:get_filepath(Db),
        Src =
            if
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

    test_util:wait(
        fun() ->
            case couch_server:open(DbName, [{timeout, ?TIMEOUT}]) of
                {ok, WaitDb} ->
                    case couch_db:get_pid(WaitDb) == DbPid of
                        true -> wait;
                        false -> ok
                    end;
                Else ->
                    Else
            end
        end,
        ?TIMEOUT,
        ?DELAY
    ).

compact_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = couch_db:start_compact(Db),
    ok = couch_db:close(Db),
    wait_db_compact_done(DbName, ?WAIT_DELAY_COUNT).

wait_db_compact_done(_DbName, 0) ->
    erlang:error(
        {assertion_failed, [
            {module, ?MODULE},
            {line, ?LINE},
            {reason, "DB compaction failed to finish"}
        ]}
    );
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
    erlang:error(
        {assertion_failed, [
            {module, ?MODULE},
            {line, ?LINE},
            {reason, "DB compaction failed to finish"}
        ]}
    );
wait_view_compact_done(DbName, DDocId, N) ->
    {ok, Code, _Headers, Body} = test_request:get(
        db_url(DbName) ++ "/" ++ ?b2l(DDocId) ++ "/_info"
    ),
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
                {assertion_failed, [
                    {module, ?MODULE},
                    {line, Line},
                    {reason, Reason}
                ]}
            );
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

wait_mrheader_record(File) ->
    wait_mrheader_record(File, ?HEADER_WRITE_WAIT_TIMEOUT).

wait_mrheader_record(File, TimeoutMSec) ->
    WaitFun = fun() ->
        try read_header(File) of
            #mrheader{} -> ok;
            _Other -> wait
        catch
            _:_ -> wait
        end
    end,
    test_util:wait(WaitFun, TimeoutMSec, 200).

wait_collator_versions(Vers, File) ->
    wait_collator_versions(Vers, File, ?HEADER_WRITE_WAIT_TIMEOUT).

wait_collator_versions(Vers, File, TimeoutMSec) ->
    WaitFun = fun() ->
        try read_header(File) of
            #mrheader{view_info = #{ucol_vs := Vers}} ->
                ok;
            _Other ->
                wait
        catch
            _:_ ->
                wait
        end
    end,
    test_util:wait(WaitFun, TimeoutMSec, 200).
