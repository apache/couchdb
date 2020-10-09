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

-module(fabric2_test_util).


-export([
    tx_too_old_mock_erlfdb/0,
    tx_too_old_setup_errors/2,
    tx_too_old_reset_errors/0,
    tx_too_old_raise_in_user_fun/0
]).


-define(PDICT_ERROR_IN_FOLD_RANGE, '$fabric2_error_in_fold_range').
-define(PDICT_ERROR_IN_USER_FUN, '$fabric2_error_throw_in_user_fun').


% Set of function to test scenarios where the FDB throws transaction_too_long
% (1007) errors. The general pattern is to call tx_too_old_mock_erlfdb() in
% setup. Then, before tests call tx_too_old_setup_errors(UserErrs, FoldErrs)
% which will set how and when the error will be thrown.

tx_too_old_mock_erlfdb() ->
    meck:expect(erlfdb, fold_range, fun(Tx, Start, End, Callback, Acc, Opts) ->
        MockFun = fun(Row, InnerAcc) ->
            maybe_tx_too_old(?PDICT_ERROR_IN_FOLD_RANGE),
            Callback(Row, InnerAcc)
        end,
        meck:passthrough([Tx, Start, End, MockFun, Acc, Opts])
    end).


tx_too_old_setup_errors(UserCnt, FoldErrs) when is_integer(UserCnt) ->
    tx_too_old_setup_errors({0, UserCnt}, FoldErrs);

tx_too_old_setup_errors(UserErrs, FoldCnt) when is_integer(FoldCnt) ->
    tx_too_old_setup_errors(UserErrs, {0, FoldCnt});

tx_too_old_setup_errors({UserSkip, UserCnt}, {FoldSkip, FoldCnt}) ->
    put(?PDICT_ERROR_IN_USER_FUN, {UserSkip, UserCnt}),
    put(?PDICT_ERROR_IN_FOLD_RANGE, {FoldSkip, FoldCnt}).


tx_too_old_reset_errors() ->
    erase(?PDICT_ERROR_IN_FOLD_RANGE),
    erase(?PDICT_ERROR_IN_USER_FUN).


tx_too_old_raise_in_user_fun() ->
    maybe_tx_too_old(?PDICT_ERROR_IN_USER_FUN).


% Private functions

maybe_tx_too_old(Key) ->
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
