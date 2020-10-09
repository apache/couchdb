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

-module(fabric2_doc_fold_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


-define(DOC_COUNT, 50).


doc_fold_test_() ->
    {
        "Test document fold operations",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(fold_docs_basic),
                    ?TDEF_FE(fold_docs_rev),
                    ?TDEF_FE(fold_docs_with_start_key),
                    ?TDEF_FE(fold_docs_with_end_key),
                    ?TDEF_FE(fold_docs_with_both_keys_the_same),
                    ?TDEF_FE(fold_docs_with_different_keys, 10000),
                    ?TDEF_FE(fold_docs_with_limit),
                    ?TDEF_FE(fold_docs_with_skip),
                    ?TDEF_FE(fold_docs_with_skip_and_limit),
                    ?TDEF_FE(fold_docs_tx_too_old),
                    ?TDEF_FE(fold_docs_db_recreated)
                ]
            }
        }
    }.


setup_all() ->
    test_util:start_couch([fabric]).


teardown_all(Ctx) ->
    test_util:stop_couch(Ctx).


setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    DocIdRevs = lists:map(fun(Val) ->
        DocId = fabric2_util:uuid(),
        Doc = #doc{
            id = DocId,
            body = {[{<<"value">>, Val}]}
        },
        {ok, Rev} = fabric2_db:update_doc(Db, Doc, []),
        {DocId, {[{rev, couch_doc:rev_to_str(Rev)}]}}
    end, lists:seq(1, ?DOC_COUNT)),
    meck:new(erlfdb, [passthrough]),
    fabric2_test_util:tx_too_old_mock_erlfdb(),
    {Db, lists:sort(DocIdRevs)}.


cleanup({Db, _DocIdRevs}) ->
    fabric2_test_util:tx_too_old_reset_errors(),
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


fold_docs_basic({Db, DocIdRevs}) ->
    {ok, {?DOC_COUNT, Rows}} = fabric2_db:fold_docs(Db, fun fold_fun/2, []),
    ?assertEqual(DocIdRevs, lists:reverse(Rows)).


fold_docs_rev({Db, DocIdRevs}) ->
    Opts = [{dir, rev}],
    {ok, {?DOC_COUNT, Rows}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(DocIdRevs, Rows).


fold_docs_with_start_key({Db, DocIdRevs}) ->
    {StartKey, _} = hd(DocIdRevs),
    Opts = [{start_key, StartKey}],
    {ok, {?DOC_COUNT, Rows}}
            = fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(DocIdRevs, lists:reverse(Rows)),
    if length(DocIdRevs) == 1 -> ok; true ->
        fold_docs_with_start_key({Db, tl(DocIdRevs)})
    end.


fold_docs_with_end_key({Db, DocIdRevs}) ->
    RevDocIdRevs = lists:reverse(DocIdRevs),
    {EndKey, _} = hd(RevDocIdRevs),
    Opts = [{end_key, EndKey}],
    {ok, {?DOC_COUNT, Rows}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(RevDocIdRevs, Rows),
    if length(DocIdRevs) == 1 -> ok; true ->
        fold_docs_with_end_key({Db, lists:reverse(tl(RevDocIdRevs))})
    end.


fold_docs_with_both_keys_the_same({Db, DocIdRevs}) ->
    lists:foreach(fun({DocId, _} = Row) ->
        check_all_combos(Db, DocId, DocId, [Row])
    end, DocIdRevs).


fold_docs_with_different_keys({Db, DocIdRevs}) ->
    lists:foreach(fun(_) ->
        {StartKey, EndKey, Rows} = pick_range(DocIdRevs),
        check_all_combos(Db, StartKey, EndKey, Rows)
    end, lists:seq(1, 500)).


fold_docs_with_limit({Db, DocIdRevs}) ->
    lists:foreach(fun(Limit) ->
        Opts1 = [{limit, Limit}],
        {ok, {?DOC_COUNT, Rows1}} =
                fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts1),
        ?assertEqual(lists:sublist(DocIdRevs, Limit), lists:reverse(Rows1)),

        Opts2 = [{dir, rev} | Opts1],
        {ok, {?DOC_COUNT, Rows2}} =
                fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts2),
        ?assertEqual(
                lists:sublist(lists:reverse(DocIdRevs), Limit),
                lists:reverse(Rows2)
            )
    end, lists:seq(0, 51)).


fold_docs_with_skip({Db, DocIdRevs}) ->
    lists:foreach(fun(Skip) ->
        Opts1 = [{skip, Skip}],
        {ok, {?DOC_COUNT, Rows1}} =
                fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts1),
        Expect1 = case Skip > length(DocIdRevs) of
            true -> [];
            false -> lists:nthtail(Skip, DocIdRevs)
        end,
        ?assertEqual(Expect1, lists:reverse(Rows1)),

        Opts2 = [{dir, rev} | Opts1],
        {ok, {?DOC_COUNT, Rows2}} =
                fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts2),
        Expect2 = case Skip > length(DocIdRevs) of
            true -> [];
            false -> lists:nthtail(Skip, lists:reverse(DocIdRevs))
        end,
        ?assertEqual(Expect2, lists:reverse(Rows2))
    end, lists:seq(0, 51)).


fold_docs_with_skip_and_limit({Db, DocIdRevs}) ->
    lists:foreach(fun(_) ->
        check_skip_and_limit(Db, [], DocIdRevs),
        check_skip_and_limit(Db, [{dir, rev}], lists:reverse(DocIdRevs))
    end, lists:seq(1, 100)).


fold_docs_tx_too_old({Db, _DocIdRevs}) ->
    {ok, Expected} = fabric2_db:fold_docs(Db, fun fold_fun/2, []),

    FoldDocsFun = fun() ->
        fabric2_db:fold_docs(Db, fun fold_fun/2, [], [{restart_tx, true}])
    end,

    % Blow up in fold range on the first call
    fabric2_test_util:tx_too_old_setup_errors(0, 1),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in fold_range after emitting one row
    fabric2_test_util:tx_too_old_setup_errors(0, {1, 1}),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in fold_range after emitting 48 rows
    fabric2_test_util:tx_too_old_setup_errors(0, {?DOC_COUNT - 2, 1}),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in fold_range after emitting 49 rows
    fabric2_test_util:tx_too_old_setup_errors(0, {?DOC_COUNT - 1, 1}),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in user fun
    fabric2_test_util:tx_too_old_setup_errors(1, 0),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in user fun after emitting one row
    fabric2_test_util:tx_too_old_setup_errors({1, 1}, 0),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in user fun after emitting 48 rows
    fabric2_test_util:tx_too_old_setup_errors({?DOC_COUNT - 2, 1}, 0),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in user fun after emitting 49 rows
    fabric2_test_util:tx_too_old_setup_errors({?DOC_COUNT - 1, 1}, 0),
    ?assertEqual({ok, Expected}, FoldDocsFun()),

    % Blow up in in user fun and fold range
    fabric2_test_util:tx_too_old_setup_errors(1, {1, 1}),
    ?assertEqual({ok, Expected}, FoldDocsFun()).


fold_docs_db_recreated({Db, _DocIdRevs}) ->
    DbName = fabric2_db:name(Db),

    RecreateDb = fun() ->
        ok = fabric2_db:delete(DbName, []),
        {ok, _} = fabric2_db:create(DbName, [])
    end,

    FoldFun = fun
        ({meta, _}, Acc) ->
            {ok, Acc};
        ({row, Row}, Acc) ->
            fabric2_test_util:tx_too_old_raise_in_user_fun(),
            % After meta and one row emitted, recreate the db
            case length(Acc) =:= 1 of
                true -> RecreateDb();
                false -> ok
            end,
            {ok, [Row | Acc]};
        (complete, Acc) ->
            {ok, Acc}
    end,
    % Blow up in user fun after emitting two rows
    fabric2_test_util:tx_too_old_setup_errors({2, 1}, 0),
    ?assertError(database_does_not_exist, fabric2_db:fold_docs(Db, FoldFun,
        [], [{restart_tx, true}])).


check_all_combos(Db, StartKey, EndKey, Rows) ->
    Opts1 = make_opts(fwd, StartKey, EndKey, true),
    {ok, {?DOC_COUNT, Rows1}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts1),
    ?assertEqual(lists:reverse(Rows), Rows1),
    check_skip_and_limit(Db, Opts1, Rows),

    Opts2 = make_opts(fwd, StartKey, EndKey, false),
    {ok, {?DOC_COUNT, Rows2}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts2),
    Expect2 = if EndKey == undefined -> lists:reverse(Rows); true ->
        lists:reverse(all_but_last(Rows))
    end,
    ?assertEqual(Expect2, Rows2),
    check_skip_and_limit(Db, Opts2, lists:reverse(Expect2)),

    Opts3 = make_opts(rev, StartKey, EndKey, true),
    {ok, {?DOC_COUNT, Rows3}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts3),
    ?assertEqual(Rows, Rows3),
    check_skip_and_limit(Db, Opts3, lists:reverse(Rows)),

    Opts4 = make_opts(rev, StartKey, EndKey, false),
    {ok, {?DOC_COUNT, Rows4}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts4),
    Expect4 = if StartKey == undefined -> Rows; true ->
        tl(Rows)
    end,
    ?assertEqual(Expect4, Rows4),
    check_skip_and_limit(Db, Opts4, lists:reverse(Expect4)).


check_skip_and_limit(Db, Opts, []) ->
    Skip = rand:uniform(?DOC_COUNT + 1) - 1,
    Limit = rand:uniform(?DOC_COUNT + 1) - 1,
    NewOpts = [{skip, Skip}, {limit, Limit} | Opts],
    {ok, {?DOC_COUNT, OutRows}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], NewOpts),
    ?assertEqual([], OutRows);

check_skip_and_limit(Db, Opts, Rows) ->
    Skip = rand:uniform(length(Rows) + 1) - 1,
    Limit = rand:uniform(?DOC_COUNT + 1 - Skip) - 1,

    ExpectRows = case Skip >= length(Rows) of
        true ->
            [];
        false ->
            lists:sublist(lists:nthtail(Skip, Rows), Limit)
    end,

    SkipLimitOpts = [{skip, Skip}, {limit, Limit} | Opts],
    {ok, {?DOC_COUNT, RevRows}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], SkipLimitOpts),
    OutRows = lists:reverse(RevRows),
    ?assertEqual(ExpectRows, OutRows).


make_opts(fwd, StartKey, EndKey, InclusiveEnd) ->
    DirOpts = case rand:uniform() =< 0.50 of
        true -> [{dir, fwd}];
        false -> []
    end,
    StartOpts = case StartKey of
        undefined -> [];
        <<_/binary>> -> [{start_key, StartKey}]
    end,
    EndOpts = case EndKey of
        undefined -> [];
        <<_/binary>> when InclusiveEnd -> [{end_key, EndKey}];
        <<_/binary>> -> [{end_key_gt, EndKey}]
    end,
    DirOpts ++ StartOpts ++ EndOpts;
make_opts(rev, StartKey, EndKey, InclusiveEnd) ->
    BaseOpts = make_opts(fwd, EndKey, StartKey, InclusiveEnd),
    [{dir, rev}] ++ BaseOpts -- [{dir, fwd}].


all_but_last([]) ->
    [];
all_but_last([_]) ->
    [];
all_but_last(Rows) ->
    lists:sublist(Rows, length(Rows) - 1).


pick_range(DocIdRevs) ->
    {StartKey, StartRow, RestRows} = pick_start_key(DocIdRevs),
    {EndKey, EndRow, RowsBetween} = pick_end_key(RestRows),
    {StartKey, EndKey, StartRow ++ RowsBetween ++ EndRow}.


pick_start_key(Rows) ->
    case rand:uniform() =< 0.1 of
        true ->
            {undefined, [], Rows};
        false ->
            Idx = rand:uniform(length(Rows)),
            {DocId, _} = Row = lists:nth(Idx, Rows),
            {DocId, [Row], lists:nthtail(Idx, Rows)}
    end.


pick_end_key([]) ->
    {undefined, [], []};

pick_end_key(Rows) ->
    case rand:uniform() =< 0.1 of
        true ->
            {undefined, [], Rows};
        false ->
            Idx = rand:uniform(length(Rows)),
            {DocId, _} = Row = lists:nth(Idx, Rows),
            Tail = lists:nthtail(Idx, Rows),
            {DocId, [Row], Rows -- [Row | Tail]}
    end.


fold_fun({meta, Meta}, _Acc) ->
    Total = fabric2_util:get_value(total, Meta),
    {ok, {Total, []}};
fold_fun({row, Row}, {Total, Rows}) ->
    fabric2_test_util:tx_too_old_raise_in_user_fun(),
    RowId = fabric2_util:get_value(id, Row),
    RowId = fabric2_util:get_value(key, Row),
    RowRev = fabric2_util:get_value(value, Row),
    {ok, {Total, [{RowId, RowRev} | Rows]}};
fold_fun(complete, Acc) ->
    {ok, Acc}.
