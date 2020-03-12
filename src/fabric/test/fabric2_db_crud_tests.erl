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


-define(PDICT_ERROR_IN_FOLD_RANGE, '$fabric2_error_in_fold_range').
-define(PDICT_ERROR_IN_USER_FUN, '$fabric2_error_throw_in_user_fun').


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
                    ?TDEF_FE(list_dbs_tx_too_long),
                    ?TDEF_FE(list_dbs_info_tx_too_long)
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
    meck:expect(erlfdb, fold_range, fun(Tx, Start, End, Callback, Acc, Opts) ->
        maybe_tx_too_long(?PDICT_ERROR_IN_FOLD_RANGE),
        meck:passthrough([Tx, Start, End, Callback, Acc, Opts])
    end),
    ok.


cleanup(_) ->
    reset_error_counts().


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


list_dbs_tx_too_long(_) ->
    DbName1 = ?tempdb(),
    DbName2 = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName1, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName2, [])),

    UserFun = fun(Row, Acc) ->
        maybe_tx_too_long(?PDICT_ERROR_IN_USER_FUN),
        {ok, [Row, Acc]}
    end,

    % Get get expected output without any transactions timing out
    Dbs = fabric2_db:list_dbs(UserFun, [], []),

    % Blow up in fold range
    tx_too_long_errors(0, 1),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in fold_range after emitting one row
    tx_too_long_errors(0, {1, 1}),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in user fun
    tx_too_long_errors(1, 0),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in user fun after emitting one row
    tx_too_long_errors({1, 1}, 0),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    % Blow up in in user fun and fold range
    tx_too_long_errors(1, {1, 1}),
    ?assertEqual(Dbs, fabric2_db:list_dbs(UserFun, [], [])),

    ok = fabric2_db:delete(DbName1, []),
    ok = fabric2_db:delete(DbName2, []).


list_dbs_info_tx_too_long(_) ->
    DbName1 = ?tempdb(),
    DbName2 = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName1, [])),
    ?assertMatch({ok, _}, fabric2_db:create(DbName2, [])),

    UserFun = fun(Row, Acc) ->
        maybe_tx_too_long(?PDICT_ERROR_IN_USER_FUN),
        {ok, [Row, Acc]}
    end,

    {ok, DbInfos} = fabric2_db:list_dbs_info(UserFun, [], []),

    % Blow up in fold range
    tx_too_long_errors(0, 1),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in fold_range after emitting one row
    tx_too_long_errors(0, {1, 1}),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in user fun
    tx_too_long_errors(1, 0),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in user fun after emitting one row
    tx_too_long_errors({1, 1}, 0),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    % Blow up in in user fun and fold range
    tx_too_long_errors(1, {1, 1}),
    ?assertEqual({ok, DbInfos}, fabric2_db:list_dbs_info(UserFun, [], [])),

    ok = fabric2_db:delete(DbName1, []),
    ok = fabric2_db:delete(DbName2, []).


is_db_info_member(_, []) ->
    false;

is_db_info_member(DbName, [DbInfo | RestInfos]) ->
    case lists:keyfind(db_name, 1, DbInfo) of
        {db_name, DbName} ->
            true;
        _E ->
            is_db_info_member(DbName, RestInfos)
    end.


tx_too_long_errors(UserFunCount, FoldErrors) when is_integer(UserFunCount) ->
    tx_too_long_errors({0, UserFunCount}, FoldErrors);

tx_too_long_errors(UserFunErrors, FoldCount) when is_integer(FoldCount) ->
    tx_too_long_errors(UserFunErrors, {0, FoldCount});

tx_too_long_errors({UserFunSkip, UserFunCount}, {FoldSkip, FoldCount}) ->
    reset_error_counts(),
    put(?PDICT_ERROR_IN_USER_FUN, {UserFunSkip, UserFunCount}),
    put(?PDICT_ERROR_IN_FOLD_RANGE, {FoldSkip, FoldCount}).


reset_error_counts() ->
    erase(?PDICT_ERROR_IN_FOLD_RANGE),
    erase(?PDICT_ERROR_IN_USER_FUN).


maybe_tx_too_long(Key) ->
    case get(Key) of
        {Skip, Count} when is_integer(Skip), Skip > 0 ->
            put(Key, {Skip - 1, Count});
        {0, Count} when is_integer(Count), Count > 0 ->
            put(Key, {0, Count - 1}),
            error({erlfdb_error, 1007});
        {0, 0} ->
            ok;
        undefined ->
            ok
    end.
