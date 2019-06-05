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

-module(fabric2_fdb_tx_retry_tests).


-include_lib("eunit/include/eunit.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


meck_setup() ->
    meck:new(erlfdb),
    meck:new(fabric2_txids),
    EnvSt = case application:get_env(fabric, db) of
        {ok, Db} -> {ok, Db};
        undefined -> undefined
    end,
    application:set_env(fabric, db, not_a_real_db),
    EnvSt.


meck_cleanup(EnvSt) ->
    case EnvSt of
        {ok, Db} -> application:set_env(fabric, db, Db);
        undefined -> application:unset_env(fabric, db)
    end,
    meck:unload().


retry_test_() ->
    {
        foreach,
        fun meck_setup/0,
        fun meck_cleanup/1,
        [
            ?TDEF(read_only_no_retry),
            ?TDEF(read_only_commit_unknown_result),
            ?TDEF(run_on_first_try),
            ?TDEF(retry_when_commit_conflict),
            ?TDEF(retry_when_txid_not_found),
            ?TDEF(no_retry_when_txid_found)
        ]
    }.


read_only_no_retry() ->
    meck:expect(erlfdb, transactional, fun(_Db, UserFun) ->
        UserFun(not_a_real_transaction)
    end),
    meck:expect(erlfdb, get_last_error, fun() -> 0 end),
    meck:expect(erlfdb, get, fun(_, _) -> foo end),
    meck:expect(erlfdb, is_read_only, fun(_) -> true end),
    meck:expect(fabric2_txids, remove, fun(undefined) -> ok end),

    Result = fabric2_fdb:transactional(fun(Tx) ->
        ?assertEqual(foo, erlfdb:get(Tx, bar)),
        did_run
    end),

    ?assertEqual(did_run, Result),
    ?assert(meck:validate([erlfdb, fabric2_txids])).


read_only_commit_unknown_result() ->
    % Not 100% certain that this would ever actually
    % happen in the wild but might as well test that
    % we don't blow up if it does.
    meck:expect(erlfdb, transactional, fun(_Db, UserFun) ->
        UserFun(not_a_real_transaction)
    end),
    meck:expect(erlfdb, get_last_error, fun() -> 1021 end),
    meck:expect(erlfdb, get, fun(_, _) -> foo end),
    meck:expect(erlfdb, is_read_only, fun(_) -> true end),
    meck:expect(fabric2_txids, remove, fun(undefined) -> ok end),

    Result = fabric2_fdb:transactional(fun(Tx) ->
        ?assertEqual(foo, erlfdb:get(Tx, bar)),
        did_run
    end),

    ?assertEqual(did_run, Result),
    ?assert(meck:validate([erlfdb, fabric2_txids])).


run_on_first_try() ->
    meck:expect(erlfdb, transactional, fun(_Db, UserFun) ->
        UserFun(not_a_real_transaction)
    end),
    meck:expect(erlfdb, get_last_error, fun() -> undefined end),
    meck:expect(erlfdb, clear, fun(_, _) -> ok end),
    meck:expect(erlfdb, is_read_only, fun(_) -> false end),
    meck:expect(fabric2_txids, create, fun(_, _) -> <<"a txid">> end),
    meck:expect(erlfdb, set, fun(_, <<"a txid">>, <<>>) -> ok end),
    meck:expect(fabric2_txids, remove, fun(<<"a txid">>) -> ok end),

    Result = fabric2_fdb:transactional(fun(Tx) ->
        ?assertEqual(ok, erlfdb:clear(Tx, bang)),
        did_run
    end),

    ?assertEqual(did_run, Result),
    ?assert(meck:validate([erlfdb, fabric2_txids])).


retry_when_commit_conflict() ->
    meck:expect(erlfdb, transactional, fun(_Db, UserFun) ->
        UserFun(not_a_real_transaction)
    end),
    meck:expect(erlfdb, get_last_error, fun() -> 1020 end),
    meck:expect(erlfdb, clear, fun(_, _) -> ok end),
    meck:expect(erlfdb, is_read_only, fun(_) -> false end),
    meck:expect(fabric2_txids, create, fun(_, _) -> <<"a txid">> end),
    meck:expect(erlfdb, set, fun(_, <<"a txid">>, <<>>) -> ok end),
    meck:expect(fabric2_txids, remove, fun(<<"a txid">>) -> ok end),

    Result = fabric2_fdb:transactional(fun(Tx) ->
        ?assertEqual(ok, erlfdb:clear(Tx, <<"foo">>)),
        did_run
    end),

    ?assertEqual(did_run, Result),
    ?assert(meck:validate([erlfdb, fabric2_txids])).


retry_when_txid_not_found() ->
    meck:expect(erlfdb, transactional, fun(_Db, UserFun) ->
        UserFun(not_a_real_transaction)
    end),
    meck:expect(erlfdb, get_last_error, fun() -> 1021 end),
    meck:expect(erlfdb, get, fun(_, <<"a txid">>) -> future end),
    meck:expect(erlfdb, wait, fun(future) -> not_found end),
    meck:expect(erlfdb, clear, fun(_, _) -> ok end),
    meck:expect(erlfdb, is_read_only, fun(_) -> false end),
    meck:expect(erlfdb, set, fun(_, <<"a txid">>, <<>>) -> ok end),
    meck:expect(fabric2_txids, remove, fun(<<"a txid">>) -> ok end),

    put('$fabric_tx_id', <<"a txid">>),
    put('$fabric_tx_result', not_the_correct_result),

    Result = fabric2_fdb:transactional(fun(Tx) ->
        ?assertEqual(ok, erlfdb:clear(Tx, <<"foo">>)),
        yay_not_skipped
    end),

    ?assertEqual(yay_not_skipped, Result),
    ?assert(meck:validate([erlfdb, fabric2_txids])).


no_retry_when_txid_found() ->
    meck:expect(erlfdb, transactional, fun(_Db, UserFun) ->
        UserFun(not_a_real_transaction)
    end),
    meck:expect(erlfdb, get_last_error, fun() -> 1021 end),
    meck:expect(erlfdb, get, fun(_, <<"a txid">>) -> future end),
    meck:expect(erlfdb, wait, fun(future) -> <<>> end),
    meck:expect(fabric2_txids, remove, fun(<<"a txid">>) -> ok end),

    put('$fabric_tx_id', <<"a txid">>),
    put('$fabric_tx_result', did_not_run),

    Result = fabric2_fdb:transactional(fun(_Tx) ->
        ?assert(false),
        did_run
    end),

    ?assertEqual(did_not_run, Result),
    ?assert(meck:validate([erlfdb, fabric2_txids])).