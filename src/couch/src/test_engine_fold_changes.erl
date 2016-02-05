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


-define(NUM_DOCS, 100).


cet_empty_changes() ->
    {ok, Engine, St} = test_engine_util:init_engine(),

    ?assertEqual(0, Engine:count_changes_since(St, 0)),
    ?assertEqual({ok, []}, Engine:fold_changes(St, 0, fun fold_fun/2, [], [])).


cet_single_change() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),
    Actions = [{create, {<<"a">>, []}}],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),

    ?assertEqual(1, Engine:count_changes_since(St2, 0)),
    ?assertEqual({ok, [{<<"a">>, 1}]},
            Engine:fold_changes(St2, 0, fun fold_fun/2, [], [])).


cet_two_changes() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),
    Actions = [
        {create, {<<"a">>, []}},
        {create, {<<"b">>, []}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),

    ?assertEqual(2, Engine:count_changes_since(St2, 0)),
    {ok, Changes} = Engine:fold_changes(St2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}], lists:reverse(Changes)).


cet_two_changes_batch() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),
    Actions1 = [
        {batch, [
            {create, {<<"a">>, []}},
            {create, {<<"b">>, []}}
        ]}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),

    ?assertEqual(2, Engine:count_changes_since(St2, 0)),
    {ok, Changes1} = Engine:fold_changes(St2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}], lists:reverse(Changes1)),

    {ok, Engine, St3} = test_engine_util:init_engine(),
    Actions2 = [
        {batch, [
            {create, {<<"b">>, []}},
            {create, {<<"a">>, []}}
        ]}
    ],
    {ok, St4} = test_engine_util:apply_actions(Engine, St3, Actions2),

    ?assertEqual(2, Engine:count_changes_since(St4, 0)),
    {ok, Changes2} = Engine:fold_changes(St4, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"b">>, 1}, {<<"a">>, 2}], lists:reverse(Changes2)).


cet_update_one() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),
    Actions = [
        {create, {<<"a">>, []}},
        {update, {<<"a">>, []}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),

    ?assertEqual(1, Engine:count_changes_since(St2, 0)),
    ?assertEqual({ok, [{<<"a">>, 2}]},
            Engine:fold_changes(St2, 0, fun fold_fun/2, [], [])).


cet_update_first_of_two() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),
    Actions = [
        {create, {<<"a">>, []}},
        {create, {<<"b">>, []}},
        {update, {<<"a">>, []}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),

    ?assertEqual(2, Engine:count_changes_since(St2, 0)),
    {ok, Changes} = Engine:fold_changes(St2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"b">>, 2}, {<<"a">>, 3}], lists:reverse(Changes)).


cet_update_second_of_two() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),
    Actions = [
        {create, {<<"a">>, []}},
        {create, {<<"b">>, []}},
        {update, {<<"b">>, []}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),

    ?assertEqual(2, Engine:count_changes_since(St2, 0)),
    {ok, Changes} = Engine:fold_changes(St2, 0, fun fold_fun/2, [], []),
    ?assertEqual([{<<"a">>, 1}, {<<"b">>, 3}], lists:reverse(Changes)).


cet_check_mutation_ordering() ->
    Actions = shuffle(lists:map(fun(Seq) ->
        {create, {docid(Seq), []}}
    end, lists:seq(1, ?NUM_DOCS))),

    DocIdOrder = [DocId || {_, {DocId, _}} <- Actions],
    DocSeqs = lists:zip(DocIdOrder, lists:seq(1, ?NUM_DOCS)),

    {ok, Engine, St1} = test_engine_util:init_engine(),
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),

    % First lets see that we can get the correct
    % suffix/prefix starting at every update sequence
    lists:foreach(fun(Seq) ->
        {ok, Suffix} = Engine:fold_changes(St2, Seq, fun fold_fun/2, [], []),
        ?assertEqual(lists:nthtail(Seq, DocSeqs), lists:reverse(Suffix)),

        {ok, Prefix} = Engine:fold_changes(St2, Seq, fun fold_fun/2, [], [
                {dir, rev}
            ]),
        ?assertEqual(lists:sublist(DocSeqs, Seq + 1), Prefix)
    end, lists:seq(0, ?NUM_DOCS)),

    ok = do_mutation_ordering(Engine, St2, ?NUM_DOCS + 1, DocSeqs, []).


do_mutation_ordering(Engine, St, _Seq, [], FinalDocSeqs) ->
    {ok, RevOrder} = Engine:fold_changes(St, 0, fun fold_fun/2, [], []),
    ?assertEqual(FinalDocSeqs, lists:reverse(RevOrder)),
    ok;

do_mutation_ordering(Engine, St, Seq, [{DocId, _OldSeq} | Rest], DocSeqAcc) ->
    Actions = [{update, {DocId, []}}],
    {ok, NewSt} = test_engine_util:apply_actions(Engine, St, Actions),
    NewAcc = DocSeqAcc ++ [{DocId, Seq}],
    Expected = Rest ++ NewAcc,
    {ok, RevOrder} = Engine:fold_changes(NewSt, 0, fun fold_fun/2, [], []),
    ?assertEqual(Expected, lists:reverse(RevOrder)),
    do_mutation_ordering(Engine, NewSt, Seq + 1, Rest, NewAcc).


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
