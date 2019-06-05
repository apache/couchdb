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


-define(DOC_COUNT, 50).


doc_fold_test_() ->
    {
        "Test document fold operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun fold_docs_basic/1,
                fun fold_docs_rev/1,
                fun fold_docs_with_start_key/1,
                fun fold_docs_with_end_key/1,
                fun fold_docs_with_both_keys_the_same/1,
                fun fold_docs_with_different_keys/1
            ]}
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    DocIdRevs = lists:map(fun(Val) ->
        DocId = fabric2_util:uuid(),
        Doc = #doc{
            id = DocId,
            body = {[{<<"value">>, Val}]}
        },
        {ok, Rev} = fabric2_db:update_doc(Db, Doc, []),
        {DocId, couch_doc:rev_to_str(Rev)}
    end, lists:seq(1, ?DOC_COUNT)),
    {Db, lists:sort(DocIdRevs), Ctx}.


cleanup({Db, _DocIdRevs, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


fold_docs_basic({Db, DocIdRevs, _}) ->
    {ok, {?DOC_COUNT, Rows}} = fabric2_db:fold_docs(Db, fun fold_fun/2, []),
    ?assertEqual(DocIdRevs, lists:reverse(Rows)).


fold_docs_rev({Db, DocIdRevs, _}) ->
    Opts = [{dir, rev}],
    {ok, {?DOC_COUNT, Rows}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(DocIdRevs, Rows).


fold_docs_with_start_key({Db, DocIdRevs, _}) ->
    {StartKey, _} = hd(DocIdRevs),
    Opts = [{start_key, StartKey}],
    {ok, {?DOC_COUNT, Rows}}
            = fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(DocIdRevs, lists:reverse(Rows)),
    if length(DocIdRevs) == 1 -> ok; true ->
        fold_docs_with_start_key({Db, tl(DocIdRevs), nil})
    end.


fold_docs_with_end_key({Db, DocIdRevs, _}) ->
    RevDocIdRevs = lists:reverse(DocIdRevs),
    {EndKey, _} = hd(RevDocIdRevs),
    Opts = [{end_key, EndKey}],
    {ok, {?DOC_COUNT, Rows}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts),
    ?assertEqual(RevDocIdRevs, Rows),
    if length(DocIdRevs) == 1 -> ok; true ->
        fold_docs_with_end_key({Db, lists:reverse(tl(RevDocIdRevs)), nil})
    end.


fold_docs_with_both_keys_the_same({Db, DocIdRevs, _}) ->
    lists:foreach(fun({DocId, _} = Row) ->
        check_all_combos(Db, DocId, DocId, [Row])
    end, DocIdRevs).


fold_docs_with_different_keys({Db, DocIdRevs, _}) ->
    lists:foreach(fun(_) ->
        {StartKey, EndKey, Rows} = pick_range(DocIdRevs),
        check_all_combos(Db, StartKey, EndKey, Rows)
    end, lists:seq(1, 500)).


check_all_combos(Db, StartKey, EndKey, Rows) ->
    Opts1 = make_opts(fwd, StartKey, EndKey, true),
    {ok, {?DOC_COUNT, Rows1}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts1),
    ?assertEqual(lists:reverse(Rows), Rows1),

    Opts2 = make_opts(fwd, StartKey, EndKey, false),
    {ok, {?DOC_COUNT, Rows2}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts2),
    Expect2 = if EndKey == undefined -> lists:reverse(Rows); true ->
        lists:reverse(all_but_last(Rows))
    end,
    ?assertEqual(Expect2, Rows2),

    Opts3 = make_opts(rev, StartKey, EndKey, true),
    {ok, {?DOC_COUNT, Rows3}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts3),
    ?assertEqual(Rows, Rows3),

    Opts4 = make_opts(rev, StartKey, EndKey, false),
    {ok, {?DOC_COUNT, Rows4}} =
            fabric2_db:fold_docs(Db, fun fold_fun/2, [], Opts4),
    Expect4 = if StartKey == undefined -> Rows; true ->
        tl(Rows)
    end,
    ?assertEqual(Expect4, Rows4).



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
    RowId = fabric2_util:get_value(id, Row),
    RowId = fabric2_util:get_value(key, Row),
    RowRev = fabric2_util:get_value(value, Row),
    {ok, {Total, [{RowId, RowRev} | Rows]}};
fold_fun(complete, Acc) ->
    {ok, Acc}.
