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
                    ?TDEF_FE(list_dbs),
                    ?TDEF_FE(list_dbs_user_fun),
                    ?TDEF_FE(list_dbs_user_fun_partial),
                    ?TDEF_FE(list_dbs_info),
                    ?TDEF_FE(list_dbs_info_partial),
                    ?TDEF_FE(list_dbs_tx_too_old),
                    ?TDEF_FE(list_dbs_info_tx_too_old),
                    ?TDEF_FE(get_info_wait_retry_on_tx_too_old),
                    ?TDEF_FE(get_info_wait_retry_on_tx_abort)
                ]
            }
        }
    }.


setup_all() ->
    Ctx = test_util:start_couch([fabric]),
    meck:new(erlfdb, [passthrough]),
    Ctx.


teardown_all(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).


setup() ->
    fabric2_test_util:tx_too_old_mock_erlfdb().


cleanup(_) ->
    fabric2_test_util:tx_too_old_reset_errors(),
    reset_fail_erfdb_wait(),
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
        {info_future, _, _, ChangesF, _, _} = InfoF,

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
        {info_future, _, _, ChangesF, _, _} = InfoF,

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
