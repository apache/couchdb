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

-module(test_engine_fold_changes).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(NUM_DOCS, 25).


cet_empty_changes() ->
    {ok, Db} = test_engine_util:create_db(),
    ?assertEqual(0, couch_db_engine:count_changes_since(Db, 0)),
    ?assertEqual({ok, []},
            couch_db_engine:fold_changes(Db, 0, fun fold_fun/2, [], [])).


cet_single_change() ->
    {ok, Db1} = test_engine_util:create_db(),
    Actions = [{create, {<<"a">>, {[]}}}],
    {ok, Db2} = test_engine_util:apply_actions(Db1, Actions),

    ?assertEqual(1, couch_db_engine:count_changes_since(Db2, 0)),
    ?assertEqual({ok, [{<<"a">>, 1}]},
            couch_db_engine:fold_changes(Db2, 0, fun fold_fun/2, [], [])).


cet_two_changes() ->
    {ok, Db1} = test_engine_util:create_db(),
    Actions = [
        {create, {<<"a">>, {[]}}},
        {create, {<<"b">>, {[]}}}
    ],
    {ok, Db2} = test_engine_util:apply_actions(Db1, Actions),

    ?assertEqual(2, couch_db_engine:count_changes_since(Db2, 0)),
    {ok, Changes} =
            couch_db_engine:fold_changes(Db2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}], lists:reverse(Changes)).


cet_two_changes_batch() ->
    {ok, Db1} = test_engine_util:create_db(),
    Actions1 = [
        {batch, [
            {create, {<<"a">>, {[]}}},
            {create, {<<"b">>, {[]}}}
        ]}
    ],
    {ok, Db2} = test_engine_util:apply_actions(Db1, Actions1),

    ?assertEqual(2, couch_db_engine:count_changes_since(Db2, 0)),
    {ok, Changes1} =
            couch_db_engine:fold_changes(Db2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}], lists:reverse(Changes1)),

    {ok, Db3} = test_engine_util:create_db(),
    Actions2 = [
        {batch, [
            {create, {<<"b">>, {[]}}},
            {create, {<<"a">>, {[]}}}
        ]}
    ],
    {ok, Db4} = test_engine_util:apply_actions(Db3, Actions2),

    ?assertEqual(2, couch_db_engine:count_changes_since(Db4, 0)),
    {ok, Changes2} =
            couch_db_engine:fold_changes(Db4, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}], lists:reverse(Changes2)).


cet_update_one() ->
    {ok, Db1} = test_engine_util:create_db(),
    Actions = [
        {create, {<<"a">>, {[]}}},
        {update, {<<"a">>, {[]}}}
    ],
    {ok, Db2} = test_engine_util:apply_actions(Db1, Actions),

    ?assertEqual(1, couch_db_engine:count_changes_since(Db2, 0)),
    ?assertEqual({ok, [{<<"a">>, 2}]},
            couch_db_engine:fold_changes(Db2, 0, fun fold_fun/2, [], [])).


cet_update_first_of_two() ->
    {ok, Db1} = test_engine_util:create_db(),
    Actions = [
        {create, {<<"a">>, {[]}}},
        {create, {<<"b">>, {[]}}},
        {update, {<<"a">>, {[]}}}
    ],
    {ok, Db2} = test_engine_util:apply_actions(Db1, Actions),

    ?assertEqual(2, couch_db_engine:count_changes_since(Db2, 0)),
    {ok, Changes} =
            couch_db_engine:fold_changes(Db2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"b">>, 2}, {<<"a">>, 3}], lists:reverse(Changes)).


cet_update_second_of_two() ->
    {ok, Db1} = test_engine_util:create_db(),
    Actions = [
        {create, {<<"a">>, {[]}}},
        {create, {<<"b">>, {[]}}},
        {update, {<<"b">>, {[]}}}
    ],
    {ok, Db2} = test_engine_util:apply_actions(Db1, Actions),

    ?assertEqual(2, couch_db_engine:count_changes_since(Db2, 0)),
    {ok, Changes} =
            couch_db_engine:fold_changes(Db2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 3}], lists:reverse(Changes)).


cet_check_mutation_ordering() ->
    Actions = shuffle(lists:map(fun(Seq) ->
        {create, {docid(Seq), {[]}}}
    end, lists:seq(1, ?NUM_DOCS))),

    DocIdOrder = [DocId || {_, {DocId, _}} <- Actions],
    DocSeqs = lists:zip(DocIdOrder, lists:seq(1, ?NUM_DOCS)),

    {ok, Db1} = test_engine_util:create_db(),
    {ok, Db2} = test_engine_util:apply_actions(Db1, Actions),

    % First lets see that we can get the correct
    % suffix/prefix starting at every update sequence
    lists:foreach(fun(Seq) ->
        {ok, Suffix} =
                couch_db_engine:fold_changes(Db2, Seq, fun fold_fun/2, [], []),
        ?assertEqual(lists:nthtail(Seq, DocSeqs), lists:reverse(Suffix)),

        {ok, Prefix} = couch_db_engine:fold_changes(
                Db2, Seq, fun fold_fun/2, [], [{dir, rev}]),
        ?assertEqual(lists:sublist(DocSeqs, Seq + 1), Prefix)
    end, lists:seq(0, ?NUM_DOCS)),

    ok = do_mutation_ordering(Db2, ?NUM_DOCS + 1, DocSeqs, []).


do_mutation_ordering(Db, _Seq, [], FinalDocSeqs) ->
    {ok, RevOrder} = couch_db_engine:fold_changes(Db, 0, fun fold_fun/2, [], []),
    ?assertEqual(FinalDocSeqs, lists:reverse(RevOrder)),
    ok;

do_mutation_ordering(Db, Seq, [{DocId, _OldSeq} | Rest], DocSeqAcc) ->
    Actions = [{update, {DocId, {[]}}}],
    {ok, NewDb} = test_engine_util:apply_actions(Db, Actions),
    NewAcc = DocSeqAcc ++ [{DocId, Seq}],
    Expected = Rest ++ NewAcc,
    {ok, RevOrder} =
            couch_db_engine:fold_changes(NewDb, 0, fun fold_fun/2, [], []),
    ?assertEqual(Expected, lists:reverse(RevOrder)),
    do_mutation_ordering(NewDb, Seq + 1, Rest, NewAcc).


shuffle(List) ->
    random:seed(os:timestamp()),
    Paired = [{random:uniform(), I} || I <- List],
    Sorted = lists:sort(Paired),
    [I || {_, I} <- Sorted].


remove_random(List) ->
    Pos = random:uniform(length(List)),
    remove_random(Pos, List).


remove_random(1, [Item | Rest]) ->
    {Item, Rest};

remove_random(N, [Skip | Rest]) when N > 1 ->
    {Item, Tail} = remove_random(N - 1, Rest),
    {Item, [Skip | Tail]}.


fold_fun(#full_doc_info{id=Id, update_seq=Seq}, Acc) ->
    {ok, [{Id, Seq} | Acc]}.


docid(I) ->
    Str = io_lib:format("~4..0b", [I]),
    iolist_to_binary(Str).
