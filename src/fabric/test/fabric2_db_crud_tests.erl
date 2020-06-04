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

-module(fabric2_db_crud_tests).


-include_lib("fabric/include/fabric2.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


-define(PDICT_RAISE_IN_ERLFDB_WAIT, '$pdict_raise_in_erlfdb_wait').


crud_test_() ->
    {
        "Test database CRUD operations",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(create_db),
                    ?TDEF_FE(open_db),
                    ?TDEF_FE(delete_db),
                    ?TDEF_FE(recreate_db),
                    ?TDEF_FE(undelete_db),
                    ?TDEF_FE(remove_deleted_db),
                    ?TDEF_FE(scheduled_remove_deleted_db, 15),
                    ?TDEF_FE(scheduled_remove_deleted_dbs, 15),
                    ?TDEF_FE(old_db_handle),
                    ?TDEF_FE(list_dbs),
                    ?TDEF_FE(list_dbs_user_fun),
                    ?TDEF_FE(list_dbs_user_fun_partial),
                    ?TDEF_FE(list_dbs_info),
                    ?TDEF_FE(list_dbs_info_partial),
                    ?TDEF_FE(list_dbs_tx_too_old),
                    ?TDEF_FE(list_dbs_info_tx_too_old, 15),
                    ?TDEF_FE(list_deleted_dbs_info),
                    ?TDEF_FE(list_deleted_dbs_info_user_fun),
                    ?TDEF_FE(list_deleted_dbs_info_user_fun_partial),
                    ?TDEF_FE(list_deleted_dbs_info_with_timestamps),
                    ?TDEF_FE(get_info_wait_retry_on_tx_too_old),
                    ?TDEF_FE(get_info_wait_retry_on_tx_abort)
                ]
            }
        }
    }.


scheduled_db_remove_error_test_() ->
    {
        "Test scheduled database remove operations",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(scheduled_remove_deleted_dbs_with_error)
                ]
            }
        }
    }.


setup_all() ->
    meck:new(config, [passthrough]),
    meck:expect(config, get_integer, fun
        ("couchdb", "db_expiration_schedule_sec", _) -> 2;
        ("couchdb", "db_expiration_retention_sec", _) -> 0;
        (_, _, Default) -> Default
    end),
    Ctx = test_util:start_couch([fabric, couch_jobs]),
    meck:new(erlfdb, [passthrough]),
    meck:new(fabric2_db_expiration, [passthrough]),
    Ctx.


teardown_all(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).


setup() ->
    fabric2_test_util:tx_too_old_mock_erlfdb().


cleanup(_) ->
    ok = config:set("couchdb", "db_expiration_enabled", "false", false),
    ok = config:set("couchdb", "enable_database_recovery", "false", false),
    fabric2_test_util:tx_too_old_reset_errors(),
    reset_fail_erfdb_wait(),
    meck:reset([fabric2_db_expiration]),
    meck:reset([config]),
    meck:reset([erlfdb]).


create_db(_) ->
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),
    ?assertEqual({error, file_exists}, fabric2_db:create(DbName, [])).


open_db(_) ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:open(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    % Opening the cached version
    ?assertMatch({ok, _}, fabric2_db:open(DbName, [])),

    % Remove from cache and re-open
    true = ets:delete(fabric2_server, DbName),
    ?assertMatch({ok, _}, fabric2_db:open(DbName, [])).


delete_db(_) ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertEqual(false, ets:member(fabric2_server, DbName)),

    ?assertError(database_does_not_exist, fabric2_db:open(DbName, [])).


recreate_db(_) ->
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),

    {ok, Db1} = fabric2_db:open(DbName, []),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),

    ?assertError(database_does_not_exist, fabric2_db:get_db_info(Db1)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),

    {ok, Db2} = fabric2_db:open(DbName, []),

    CurOpts = [{uuid, fabric2_db:get_uuid(Db2)}],
    ?assertMatch({ok, #{}}, fabric2_db:open(DbName, CurOpts)),

    % Remove from cache to force it to open through fabric2_fdb:open
    fabric2_server:remove(DbName),
    ?assertMatch({ok, #{}}, fabric2_db:open(DbName, CurOpts)),

    BadOpts = [{uuid, fabric2_util:uuid()}],
    ?assertError(database_does_not_exist, fabric2_db:open(DbName, BadOpts)),

    % Remove from cache to force it to open through fabric2_fdb:open
    fabric2_server:remove(DbName),
    ?assertError(database_does_not_exist, fabric2_db:open(DbName, BadOpts)).


undelete_db(_) ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertEqual(false, ets:member(fabric2_server, DbName)),


    {ok, Infos} = fabric2_db:list_deleted_dbs_info(),
    [DeletedDbInfo] = [Info || Info <- Infos,
        DbName == proplists:get_value(db_name, Info)
    ],
    Timestamp = proplists:get_value(timestamp, DeletedDbInfo),

    OldTS = <<"2020-01-01T12:00:00Z">>,
    ?assertEqual(not_found, fabric2_db:undelete(DbName, DbName, OldTS, [])),
    BadDbName = <<"bad_dbname">>,
    ?assertEqual(not_found,
        fabric2_db:undelete(BadDbName, BadDbName, Timestamp, [])),

    ok = fabric2_db:undelete(DbName, DbName, Timestamp, []),
    {ok, AllDbInfos} = fabric2_db:list_dbs_info(),
    ?assert(is_db_info_member(DbName, AllDbInfos)).


remove_deleted_db(_) ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertEqual(false, ets:member(fabric2_server, DbName)),

    {ok, Infos} = fabric2_db:list_deleted_dbs_info(),
    [DeletedDbInfo] = [Info || Info <- Infos,
        DbName == proplists:get_value(db_name, Info)
    ],
    Timestamp = proplists:get_value(timestamp, DeletedDbInfo),
    OldTS = <<"2020-01-01T12:00:00Z">>,
    ?assertEqual(not_found,
        fabric2_db:delete(DbName, [{deleted_at, OldTS}])),
    BadDbName = <<"bad_dbname">>,
    ?assertEqual(not_found,
        fabric2_db:delete(BadDbName, [{deleted_at, Timestamp}])),

    ok = fabric2_db:delete(DbName, [{deleted_at, Timestamp}]),
    {ok, Infos2} = fabric2_db:list_deleted_dbs_info(),
    DeletedDbs = [proplists:get_value(db_name, Info) || Info <- Infos2],
    ?assert(not lists:member(DbName, DeletedDbs)).


scheduled_remove_deleted_db(_) ->
    ok = config:set("couchdb", "db_expiration_enabled", "true", false),
    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertEqual(false, ets:member(fabric2_server, DbName)),

    meck:reset(fabric2_db_expiration),
    meck:wait(fabric2_db_expiration, process_expirations, '_', 7000),

    ?assertEqual(ok, test_util:wait(fun() ->
        {ok, Infos} = fabric2_db:list_deleted_dbs_info(),
        DeletedDbs = [proplists:get_value(db_name, Info) || Info <- Infos],
        case lists:member(DbName, DeletedDbs) of
            true -> wait;
            false -> ok
        end
    end)).


scheduled_remove_deleted_dbs(_) ->
    ok = config:set("couchdb", "db_expiration_enabled", "true", false),
    ok = config:set("couchdb", "db_expiration_batch", "2", false),
    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    DbNameList = [create_and_delete_db() || _I <- lists:seq(1, 5)],
    meck:reset(fabric2_db_expiration),
    meck:wait(fabric2_db_expiration, process_expirations, '_', 7000),

    {ok, Infos} = fabric2_db:list_deleted_dbs_info(),
    DeletedDbs = [proplists:get_value(db_name, Info) || Info <- Infos],
    lists:map(fun(DbName) ->
        ?assert(not lists:member(DbName, DeletedDbs))
    end, DbNameList).


scheduled_remove_deleted_dbs_with_error(_) ->
    meck:expect(fabric2_db_expiration, process_expirations, fun(_, _) ->
        throw(process_expirations_error)
    end),

    {Pid, Ref} = spawn_monitor(fun() ->
        fabric2_db_expiration:cleanup(true)
    end),
    receive
        {'DOWN', Ref, process, Pid, Error} ->
            ?assertMatch({job_error, process_expirations_error, _}, Error)
    end,
    JobType = <<"db_expiration">>,
    JobId = <<"db_expiration_job">>,
    FQJobId = <<JobId/binary, "-", 1:16/integer>>,

    ?assertMatch({ok, _}, couch_jobs:get_job_data(undefined, JobType, FQJobId)),
    {ok, JobState} = couch_jobs:get_job_state(undefined, JobType, FQJobId),
    ?assert(lists:member(JobState, [pending, running])).


old_db_handle(_) ->
    % db hard deleted
    DbName1 = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName1, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName1, [])),
    {ok, Db1} = fabric2_db:open(DbName1, []),
    ?assertMatch({ok, _}, fabric2_db:get_db_info(Db1)),
    ?assertEqual(ok, fabric2_db:delete(DbName1, [])),
    ?assertError(database_does_not_exist, fabric2_db:get_db_info(Db1)),

    % db soft deleted
    DbName2 = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName2, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName2, [])),
    {ok, Db2} = fabric2_db:open(DbName2, []),
    ?assertMatch({ok, _}, fabric2_db:get_db_info(Db2)),
    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    ?assertEqual(ok, fabric2_db:delete(DbName2, [])),
    ?assertError(database_does_not_exist, fabric2_db:get_db_info(Db2)),

    % db soft deleted and re-created
    DbName3 = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName3, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName3, [])),
    {ok, Db3} = fabric2_db:open(DbName3, []),
    ?assertMatch({ok, _}, fabric2_db:get_db_info(Db3)),
    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    ?assertEqual(ok, fabric2_db:delete(DbName3, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName3, [])),
    ?assertError(database_does_not_exist, fabric2_db:get_db_info(Db3)),

    % db soft deleted and undeleted
    DbName4 = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName4, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName4, [])),
    {ok, Db4} = fabric2_db:open(DbName4, []),
    ?assertMatch({ok, _}, fabric2_db:get_db_info(Db4)),
    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    ?assertEqual(ok, fabric2_db:delete(DbName4, [])),
    {ok, Infos} = fabric2_db:list_deleted_dbs_info(),
    [DeletedDbInfo] = [Info || Info <- Infos,
        DbName4 == proplists:get_value(db_name, Info)
    ],
    Timestamp = proplists:get_value(timestamp, DeletedDbInfo),
    ok = fabric2_db:undelete(DbName4, DbName4, Timestamp, []),
    ?assertMatch({ok, _}, fabric2_db:get_db_info(Db4)),

    % db hard deleted and re-created
    DbName5 = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName5, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName5, [])),
    {ok, Db5} = fabric2_db:open(DbName5, []),
    ?assertMatch({ok, _}, fabric2_db:get_db_info(Db5)),
    ok = config:set("couchdb", "enable_database_recovery", "false", false),
    ?assertEqual(ok, fabric2_db:delete(DbName5, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName5, [])),
    ?assertError(database_does_not_exist, fabric2_db:get_db_info(Db5)).


list_dbs(_) ->
    DbName = ?tempdb(),
    AllDbs1 = fabric2_db:list_dbs(),

    ?assert(is_list(AllDbs1)),
    ?assert(not lists:member(DbName, AllDbs1)),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    AllDbs2 = fabric2_db:list_dbs(),
    ?assert(lists:member(DbName, AllDbs2)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    AllDbs3 = fabric2_db:list_dbs(),
    ?assert(not lists:member(DbName, AllDbs3)).


list_dbs_user_fun(_) ->
    ?assertMatch({ok, _}, fabric2_db:create(?tempdb(), [])),

    UserFun = fun(Row, Acc) -> {ok, [Row | Acc]} end,
    {ok, UserAcc} = fabric2_db:list_dbs(UserFun, [], []),

    Base = lists:foldl(fun(DbName, Acc) ->
        [{row, [{id, DbName}]} | Acc]
    end, [{meta, []}], fabric2_db:list_dbs()),
    Expect = lists:reverse(Base, [complete]),

    ?assertEqual(Expect, lists:reverse(UserAcc)).


list_dbs_user_fun_partial(_) ->
    UserFun = fun(Row, Acc) -> {stop, [Row | Acc]} end,
    {ok, UserAcc} = fabric2_db:list_dbs(UserFun, [], []),
    ?assertEqual([{meta, []}], UserAcc).


list_dbs_info(_) ->
    DbName = ?tempdb(),
    {ok, AllDbInfos1} = fabric2_db:list_dbs_info(),

    ?assert(is_list(AllDbInfos1)),
    ?assert(not is_db_info_member(DbName, AllDbInfos1)),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    {ok, AllDbInfos2} = fabric2_db:list_dbs_info(),
    ?assert(is_db_info_member(DbName, AllDbInfos2)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    {ok, AllDbInfos3} = fabric2_db:list_dbs_info(),
    ?assert(not is_db_info_member(DbName, AllDbInfos3)).


list_dbs_info_partial(_) ->
    UserFun = fun(Row, Acc) -> {stop, [Row | Acc]} end,
    {ok, UserAcc} = fabric2_db:list_dbs_info(UserFun, [], []),
    ?assertEqual([{meta, []}], UserAcc).


list_dbs_tx_too_old(_) ->
    DbName1 = ?tempdb(),
    DbName2 = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName1, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName2, [])),

    UserFun = fun(Row, Acc) ->
        fabric2_test_util:tx_too_old_raise_in_user_fun(),
        {ok, [Row | Acc]}
    end,

    % Get get expected output without any transactions timing out
    Dbs = fabric2_db:list_dbs(UserFun, [], []),

    % Blow up in fold range
    fabric2_test_util:tx_too_old_setup_errors(0, 1),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in fold_range after emitting one row
    fabric2_test_util:tx_too_old_setup_errors(0, {1, 1}),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in user fun
    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in user fun after emitting one row
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, 0),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in in user fun and fold range
    fabric2_test_util:tx_too_old_setup_errors(1, {1, 1}),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    ok = fabric2_db:delete(DbName1, []),
    ok = fabric2_db:delete(DbName2, []).


list_dbs_info_tx_too_old(_) ->
    % list_dbs_info uses a queue of 100 futures to fetch db infos in parallel
    % so create more than 100 dbs so make sure we have 100+ dbs in our test

    DbCount = 101,
    DbNames = fabric2_util:pmap(fun(_) ->
        DbName = ?tempdb(),
        ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
        DbName
    end, lists:seq(1, DbCount)),

    UserFun = fun(Row, Acc) ->
        fabric2_test_util:tx_too_old_raise_in_user_fun(),
        {ok, [Row | Acc]}
    end,

    % This is the expected return with no tx timeouts
    {ok, DbInfos} = fabric2_db:list_dbs_info(UserFun, [], []),

    % Blow up in fold range on the first call
    fabric2_test_util:tx_too_old_setup_errors(0, 1),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in fold_range after emitting one row
    fabric2_test_util:tx_too_old_setup_errors(0, {1, 1}),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in fold_range after emitting 99 rows
    fabric2_test_util:tx_too_old_setup_errors(0, {DbCount - 2, 1}),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in fold_range after emitting 100 rows
    fabric2_test_util:tx_too_old_setup_errors(0, {DbCount - 1, 1}),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in user fun
    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in user fun after emitting one row
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, 0),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in user fun after emitting 99 rows
    fabric2_test_util:tx_too_old_setup_errors({DbCount - 2, 1}, 0),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in user fun after emitting 100 rows
    fabric2_test_util:tx_too_old_setup_errors({DbCount - 1, 1}, 0),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in in user fun and fold range
    fabric2_test_util:tx_too_old_setup_errors(1, {1, 1}),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    fabric2_util:pmap(fun(DbName) ->
        ?assertEqual(ok, fabric2_db:delete(DbName, []))
    end, DbNames).


list_deleted_dbs_info(_) ->
    DbName = ?tempdb(),
    AllDbs1 = fabric2_db:list_dbs(),

    ?assert(is_list(AllDbs1)),
    ?assert(not lists:member(DbName, AllDbs1)),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    AllDbs2 = fabric2_db:list_dbs(),
    ?assert(lists:member(DbName, AllDbs2)),

    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    ?assertEqual(ok, fabric2_db:delete(DbName, [])),

    AllDbs3 = fabric2_db:list_dbs(),
    ?assert(not lists:member(DbName, AllDbs3)),
    {ok, DeletedDbsInfo} = fabric2_db:list_deleted_dbs_info(),
    DeletedDbs4 = get_deleted_dbs(DeletedDbsInfo),
    ?assert(lists:member(DbName, DeletedDbs4)).


list_deleted_dbs_info_user_fun(_) ->
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(ok, fabric2_db:delete(DbName, [])),

    UserFun = fun(Row, Acc) -> {ok, [Row | Acc]} end,
    {ok, UserAcc} = fabric2_db:list_deleted_dbs_info(UserFun, [], []),
    {ok, DeletedDbsInfo} = fabric2_db:list_deleted_dbs_info(),

    Base = lists:foldl(fun(DbInfo, Acc) ->
        [{row, DbInfo} | Acc]
    end, [{meta, []}], DeletedDbsInfo),
    Expect = lists:reverse(Base, [complete]),

    ?assertEqual(Expect, lists:reverse(UserAcc)).


list_deleted_dbs_info_user_fun_partial(_) ->
    UserFun = fun(Row, Acc) -> {stop, [Row | Acc]} end,
    {ok, UserAcc} = fabric2_db:list_deleted_dbs_info(UserFun, [], []),
    ?assertEqual([{meta, []}], UserAcc).


list_deleted_dbs_info_with_timestamps(_) ->
    ok = config:set("couchdb", "enable_database_recovery", "true", false),

    % Cycle our database three times to get multiple entries
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    timer:sleep(1100),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    timer:sleep(1100),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(ok, fabric2_db:delete(DbName, [])),

    UserFun = fun(Row, Acc) ->
        case Row of
            {row, Info} -> {ok, [Info | Acc]};
            _ -> {ok, Acc}
        end
    end,

    Options1 = [{start_key, DbName}, {end_key, <<DbName/binary, 255>>}],
    {ok, Infos1} = fabric2_db:list_deleted_dbs_info(UserFun, [], Options1),
    TimeStamps1 = [fabric2_util:get_value(timestamp, Info) || Info <- Infos1],
    ?assertEqual(3, length(TimeStamps1)),

    [FirstTS, MiddleTS, LastTS] = lists:sort(TimeStamps1),

    % Check we can skip over the FirstTS
    Options2 = [{start_key, [DbName, MiddleTS]}, {end_key, [DbName, LastTS]}],
    {ok, Infos2} = fabric2_db:list_deleted_dbs_info(UserFun, [], Options2),
    TimeStamps2 = [fabric2_util:get_value(timestamp, Info) || Info <- Infos2],
    ?assertEqual(2, length(TimeStamps2)),
    ?assertEqual([LastTS, MiddleTS], TimeStamps2), % because foldl reverses

    % Check we an end before LastTS
    Options3 = [{start_key, DbName}, {end_key, [DbName, MiddleTS]}],
    {ok, Infos3} = fabric2_db:list_deleted_dbs_info(UserFun, [], Options3),
    TimeStamps3 = [fabric2_util:get_value(timestamp, Info) || Info <- Infos3],
    ?assertEqual([MiddleTS, FirstTS], TimeStamps3),

    % Check that {dir, rev} works without timestamps
    Options4 = [{start_key, DbName}, {end_key, DbName}, {dir, rev}],
    {ok, Infos4} = fabric2_db:list_deleted_dbs_info(UserFun, [], Options4),
    TimeStamps4 = [fabric2_util:get_value(timestamp, Info) || Info <- Infos4],
    ?assertEqual([FirstTS, MiddleTS, LastTS], TimeStamps4),

    % Check that reverse with keys returns correctly
    Options5 = [
        {start_key, [DbName, MiddleTS]},
        {end_key, [DbName, FirstTS]},
        {dir, rev}
    ],
    {ok, Infos5} = fabric2_db:list_deleted_dbs_info(UserFun, [], Options5),
    TimeStamps5 = [fabric2_util:get_value(timestamp, Info) || Info <- Infos5],
    ?assertEqual([FirstTS, MiddleTS], TimeStamps5).


get_info_wait_retry_on_tx_too_old(_) ->
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),

    {ok, Db} = fabric2_db:open(DbName, []),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx,
            db_prefix := DbPrefix
        } = TxDb,

        % Simulate being in a list_dbs_info callback
        ok = erlfdb:set_option(Tx, disallow_writes),

        InfoF = fabric2_fdb:get_info_future(Tx, DbPrefix),
        {info_future, _, _, ChangesF, _, _, _} = InfoF,

        raise_in_erlfdb_wait(ChangesF, {erlfdb_error, 1007}, 3),
        ?assertError({erlfdb_error, 1007}, fabric2_fdb:get_info_wait(InfoF)),

        raise_in_erlfdb_wait(ChangesF, {erlfdb_error, 1007}, 2),
        ?assertMatch([{_, _} | _], fabric2_fdb:get_info_wait(InfoF)),

        ?assertEqual(ok, fabric2_db:delete(DbName, []))
    end).


get_info_wait_retry_on_tx_abort(_)->
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),

    {ok, Db} = fabric2_db:open(DbName, []),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx,
            db_prefix := DbPrefix
        } = TxDb,

        % Simulate being in a list_dbs_info callback
        ok = erlfdb:set_option(Tx, disallow_writes),

        InfoF = fabric2_fdb:get_info_future(Tx, DbPrefix),
        {info_future, _, _, ChangesF, _, _, _} = InfoF,

        raise_in_erlfdb_wait(ChangesF, {erlfdb_error, 1025}, 3),
        ?assertError({erlfdb_error, 1025}, fabric2_fdb:get_info_wait(InfoF)),

        raise_in_erlfdb_wait(ChangesF, {erlfdb_error, 1025}, 2),
        ?assertMatch([{_, _} | _], fabric2_fdb:get_info_wait(InfoF)),

        ?assertEqual(ok, fabric2_db:delete(DbName, []))
    end).


reset_fail_erfdb_wait() ->
    erase(?PDICT_RAISE_IN_ERLFDB_WAIT),
    meck:expect(erlfdb, wait, fun(F) -> meck:passthrough([F]) end).


raise_in_erlfdb_wait(Future, Error, Count) ->
    put(?PDICT_RAISE_IN_ERLFDB_WAIT, Count),
    meck:expect(erlfdb, wait, fun
        (F) when F =:= Future ->
            case get(?PDICT_RAISE_IN_ERLFDB_WAIT) of
                N when is_integer(N), N > 0 ->
                    put(?PDICT_RAISE_IN_ERLFDB_WAIT, N - 1),
                    error(Error);
                _ ->
                    meck:passthrough([F])
            end;
        (F) ->
            meck:passthrough([F])
    end).


is_db_info_member(_, []) ->
    false;

is_db_info_member(DbName, [DbInfo | RestInfos]) ->
    case lists:keyfind(db_name, 1, DbInfo) of
        {db_name, DbName} ->
            true;
        _E ->
            is_db_info_member(DbName, RestInfos)
    end.

get_deleted_dbs(DeletedDbInfos)  ->
    lists:foldl(fun(DbInfo, Acc) ->
        DbName = fabric2_util:get_value(db_name, DbInfo),
        [DbName | Acc]
    end, [], DeletedDbInfos).


create_and_delete_db() ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertEqual(false, ets:member(fabric2_server, DbName)),
    DbName.
