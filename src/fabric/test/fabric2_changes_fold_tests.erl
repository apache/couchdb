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

-module(fabric2_changes_fold_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


-define(DOC_COUNT, 25).


changes_fold_test_() ->
    {
        "Test changes fold operations",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(fold_changes_basic),
                    ?TDEF_FE(fold_changes_since_now),
                    ?TDEF_FE(fold_changes_since_seq),
                    ?TDEF_FE(fold_changes_basic_rev),
                    ?TDEF_FE(fold_changes_since_now_rev),
                    ?TDEF_FE(fold_changes_since_seq_rev),
                    ?TDEF_FE(fold_changes_basic_tx_too_old),
                    ?TDEF_FE(fold_changes_reverse_tx_too_old),
                    ?TDEF_FE(fold_changes_tx_too_old_with_single_row_emits),
                    ?TDEF_FE(fold_changes_since_seq_tx_too_old),
                    ?TDEF_FE(fold_changes_not_progressing)
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
    fabric2_test_util:tx_too_old_mock_erlfdb(),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    Rows = lists:map(fun(Val) ->
        DocId = fabric2_util:uuid(),
        Doc = #doc{
            id = DocId,
            body = {[{<<"value">>, Val}]}
        },
        {ok, RevId} = fabric2_db:update_doc(Db, Doc, []),
        UpdateSeq = fabric2_db:get_update_seq(Db),
        #{
            id => DocId,
            sequence => UpdateSeq,
            deleted => false,
            rev_id => RevId
        }
    end, lists:seq(1, ?DOC_COUNT)),
    {Db, Rows}.


cleanup({Db, _DocIdRevs}) ->
    fabric2_test_util:tx_too_old_reset_errors(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


fold_changes_basic({Db, DocRows}) ->
    ?assertEqual(lists:reverse(DocRows), changes(Db)).


fold_changes_since_now({Db, _}) ->
    ?assertEqual([], changes(Db, now, [])).


fold_changes_since_seq({_, []}) ->
    ok;

fold_changes_since_seq({Db, [Row | RestRows]}) ->
    #{sequence := Since} = Row,
    ?assertEqual(lists:reverse(RestRows), changes(Db, Since, [])),
    fold_changes_since_seq({Db, RestRows}).


fold_changes_basic_rev({Db, _}) ->
    ?assertEqual([], changes(Db, 0, [{dir, rev}])).


fold_changes_since_now_rev({Db, DocRows}) ->
    ?assertEqual(DocRows, changes(Db, now, [{dir, rev}])).


fold_changes_since_seq_rev({_, []}) ->
    ok;

fold_changes_since_seq_rev({Db, DocRows}) ->
    #{sequence := Since} = lists:last(DocRows),
    Opts = [{dir, rev}],
    ?assertEqual(DocRows, changes(Db, Since, Opts)),
    RestRows = lists:sublist(DocRows, length(DocRows) - 1),
    fold_changes_since_seq_rev({Db, RestRows}).


fold_changes_basic_tx_too_old({Db, DocRows0}) ->
    DocRows = lists:reverse(DocRows0),

    fabric2_test_util:tx_too_old_setup_errors(0, 1),
    ?assertEqual(DocRows, changes(Db)),

    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual(DocRows, changes(Db)),

    % Blow up in user fun but after emitting one row successfully.
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, 0),
    ?assertEqual(DocRows, changes(Db)),

    % Blow up before last document
    fabric2_test_util:tx_too_old_setup_errors({?DOC_COUNT - 1, 1}, 0),
    ?assertEqual(DocRows, changes(Db)),

    % Emit one value, then blow up in user function and then blow up twice in
    % fold_range. But it is not enough to stop the iteration.
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, {1, 2}),
    ?assertEqual(DocRows, changes(Db)).


fold_changes_reverse_tx_too_old({Db, DocRows}) ->
    Opts = [{dir, rev}],

    fabric2_test_util:tx_too_old_setup_errors(0, 1),
    ?assertEqual([], changes(Db, 0, Opts)),

    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual([], changes(Db, 0, Opts)),

    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual(DocRows, changes(Db, now, Opts)),

    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual(DocRows, changes(Db, now, Opts)),

    % Blow up in user fun but after emitting one row successfully.
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, 0),
    ?assertEqual(DocRows, changes(Db, now, Opts)),

    % Blow up before last document
    fabric2_test_util:tx_too_old_setup_errors({?DOC_COUNT - 1, 1}, 0),
    ?assertEqual(DocRows, changes(Db, now, Opts)),

    % Emit value, blow up in user function, and twice in fold_range
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, {1, 2}),
    ?assertEqual(DocRows, changes(Db, now, Opts)).


fold_changes_tx_too_old_with_single_row_emits({Db, DocRows0}) ->
    % This test does a few basic operations while forcing erlfdb range fold to
    % emit a single row at a time, thus forcing it to use continuations while
    % also inducing tx errors
    Opts = [{target_bytes, 1}],
    DocRows = lists:reverse(DocRows0),

    fabric2_test_util:tx_too_old_setup_errors(0, 1),
    ?assertEqual(DocRows, changes(Db, 0, Opts)),

    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual(DocRows, changes(Db, 0, Opts)),

    % Blow up in user fun but after emitting one row successfully.
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, 0),
    ?assertEqual(DocRows, changes(Db, 0, Opts)),

    % Blow up before last document
    fabric2_test_util:tx_too_old_setup_errors({?DOC_COUNT - 1, 1}, 0),
    ?assertEqual(DocRows, changes(Db, 0, Opts)).


fold_changes_since_seq_tx_too_old({Db, Rows}) ->
    % Blow up after after a successful emit, then twice
    % in range fold call. Also re-use already existing basic
    % fold_changes_since_seq test function.
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, {1, 2}),
    fold_changes_since_seq({Db, Rows}).


fold_changes_not_progressing({Db, _}) ->
    % Fail in first fold range call.
    fabric2_test_util:tx_too_old_setup_errors(5, 0),
    ?assertError(fold_range_not_progressing, changes(Db)),

    % Fail in first user fun call.
    fabric2_test_util:tx_too_old_setup_errors(0, 5),
    ?assertError(fold_range_not_progressing, changes(Db)),

    % Blow up in last user fun call
    fabric2_test_util:tx_too_old_setup_errors({?DOC_COUNT - 1, 5}, 0),
    ?assertError(fold_range_not_progressing, changes(Db)),

    % Blow up in user function after one success.
    fabric2_test_util:tx_too_old_setup_errors({1, 5}, 0),
    ?assertError(fold_range_not_progressing, changes(Db)),

    % Emit value, blow up in user function, then keep blowing up in fold_range.
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, {1, 4}),
    ?assertError(fold_range_not_progressing, changes(Db)).


fold_fun(#{} = Change, Acc) ->
    fabric2_test_util:tx_too_old_raise_in_user_fun(),
    {ok, [Change | Acc]}.


changes(Db) ->
    changes(Db, 0, []).


changes(Db, Since, Opts) ->
    {ok, Rows} = fabric2_db:fold_changes(Db, Since, fun fold_fun/2, [], Opts),
    Rows.
