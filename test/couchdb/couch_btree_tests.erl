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

-module(couch_btree_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ROWS, 1000).


setup() ->
    {ok, Fd} = couch_file:open(?tempfile(), [create, overwrite]),
    {ok, Btree} = couch_btree:open(nil, Fd, [{compression, none},
                                             {reduce, fun reduce_fun/2}]),
    {Fd, Btree}.

setup_kvs(_) ->
    setup().

setup_red() ->
    {_, EvenOddKVs} = lists:foldl(
        fun(Idx, {Key, Acc}) ->
            case Key of
                "even" -> {"odd", [{{Key, Idx}, 1} | Acc]};
                _ -> {"even", [{{Key, Idx}, 1} | Acc]}
            end
        end, {"odd", []}, lists:seq(1, ?ROWS)),
    {Fd, Btree} = setup(),
    {ok, Btree1} = couch_btree:add_remove(Btree, EvenOddKVs, []),
    {Fd, Btree1}.
setup_red(_) ->
    setup_red().

teardown(Fd) when is_pid(Fd) ->
    ok = couch_file:close(Fd);
teardown({Fd, _}) ->
    teardown(Fd).
teardown(_, {Fd, _}) ->
    teardown(Fd).


kvs_test_funs() ->
    [
        fun should_set_fd_correctly/2,
        fun should_set_root_correctly/2,
        fun should_create_zero_sized_btree/2,
        fun should_set_reduce_option/2,
        fun should_fold_over_empty_btree/2,
        fun should_add_all_keys/2,
        fun should_continuously_add_new_kv/2,
        fun should_continuously_remove_keys/2,
        fun should_insert_keys_in_reversed_order/2,
        fun should_add_every_odd_key_remove_every_even/2,
        fun should_add_every_even_key_remove_every_old/2
    ].

red_test_funs() ->
    [
        fun should_reduce_whole_range/2,
        fun should_reduce_first_half/2,
        fun should_reduce_second_half/2
    ].


btree_open_test_() ->
    {ok, Fd} = couch_file:open(?tempfile(), [create, overwrite]),
    {ok, Btree} = couch_btree:open(nil, Fd, [{compression, none}]),
    {
        "Ensure that created btree is really a btree record",
        ?_assert(is_record(Btree, btree))
    }.

sorted_kvs_test_() ->
    Funs = kvs_test_funs(),
    Sorted = [{Seq, random:uniform()} || Seq <- lists:seq(1, ?ROWS)],
    {
        "BTree with sorted keys",
        {
            foreachx,
            fun setup_kvs/1, fun teardown/2,
            [{Sorted, Fun} || Fun <- Funs]
        }
    }.

rsorted_kvs_test_() ->
    Sorted = [{Seq, random:uniform()} || Seq <- lists:seq(1, ?ROWS)],
    Funs = kvs_test_funs(),
    Reversed = Sorted,
    {
        "BTree with backward sorted keys",
        {
            foreachx,
            fun setup_kvs/1, fun teardown/2,
            [{Reversed, Fun} || Fun <- Funs]
        }
    }.

shuffled_kvs_test_() ->
    Funs = kvs_test_funs(),
    Sorted = [{Seq, random:uniform()} || Seq <- lists:seq(1, ?ROWS)],
    Shuffled = shuffle(Sorted),
    {
        "BTree with shuffled keys",
        {
            foreachx,
            fun setup_kvs/1, fun teardown/2,
            [{Shuffled, Fun} || Fun <- Funs]
        }
    }.

reductions_test_() ->
    {
        "BTree reductions",
        [
            {
                "Common tests",
                {
                    foreach,
                    fun setup_red/0, fun teardown/1,
                    [
                        fun should_reduce_without_specified_direction/1,
                        fun should_reduce_forward/1,
                        fun should_reduce_backward/1
                    ]
                }
            },
            {
                "Range requests",
                [
                    {
                        "Forward direction",
                        {
                            foreachx,
                            fun setup_red/1, fun teardown/2,
                            [{fwd, F} || F <- red_test_funs()]
                        }
                    },
                    {
                        "Backward direction",
                        {
                            foreachx,
                            fun setup_red/1, fun teardown/2,
                            [{rev, F} || F <- red_test_funs()]
                        }
                    }
                ]
            }
        ]
    }.


should_set_fd_correctly(_, {Fd, Btree}) ->
    ?_assertMatch(Fd, Btree#btree.fd).

should_set_root_correctly(_, {_, Btree}) ->
    ?_assertMatch(nil, Btree#btree.root).

should_create_zero_sized_btree(_, {_, Btree}) ->
    ?_assertMatch(0, couch_btree:size(Btree)).

should_set_reduce_option(_, {_, Btree}) ->
    ReduceFun = fun reduce_fun/2,
    Btree1 = couch_btree:set_options(Btree, [{reduce, ReduceFun}]),
    ?_assertMatch(ReduceFun, Btree1#btree.reduce).

should_fold_over_empty_btree(_, {_, Btree}) ->
    {ok, _, EmptyRes} = couch_btree:foldl(Btree, fun(_, X) -> {ok, X+1} end, 0),
    ?_assertEqual(EmptyRes, 0).

should_add_all_keys(KeyValues, {Fd, Btree}) ->
    {ok, Btree1} = couch_btree:add_remove(Btree, KeyValues, []),
    [
        should_return_complete_btree_on_adding_all_keys(KeyValues, Btree1),
        should_have_non_zero_size(Btree1),
        should_have_lesser_size_than_file(Fd, Btree1),
        should_keep_root_pointer_to_kp_node(Fd, Btree1),
        should_remove_all_keys(KeyValues, Btree1)
    ].

should_return_complete_btree_on_adding_all_keys(KeyValues, Btree) ->
    ?_assert(test_btree(Btree, KeyValues)).

should_have_non_zero_size(Btree) ->
    ?_assert(couch_btree:size(Btree) > 0).

should_have_lesser_size_than_file(Fd, Btree) ->
    ?_assert((couch_btree:size(Btree) =< couch_file:bytes(Fd))).

should_keep_root_pointer_to_kp_node(Fd, Btree) ->
    ?_assertMatch({ok, {kp_node, _}},
                  couch_file:pread_term(Fd, element(1, Btree#btree.root))).

should_remove_all_keys(KeyValues, Btree) ->
    Keys = keys(KeyValues),
    {ok, Btree1} = couch_btree:add_remove(Btree, [], Keys),
    {
        "Should remove all the keys",
        [
            should_produce_valid_btree(Btree1, []),
            should_be_empty(Btree1)
        ]
    }.

should_continuously_add_new_kv(KeyValues, {_, Btree}) ->
    {Btree1, _} = lists:foldl(
        fun(KV, {BtAcc, PrevSize}) ->
            {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [KV], []),
            ?assert(couch_btree:size(BtAcc2) > PrevSize),
            {BtAcc2, couch_btree:size(BtAcc2)}
        end, {Btree, couch_btree:size(Btree)}, KeyValues),
    {
        "Should continuously add key-values to btree",
        [
            should_produce_valid_btree(Btree1, KeyValues),
            should_not_be_empty(Btree1)
        ]
    }.

should_continuously_remove_keys(KeyValues, {_, Btree}) ->
    {ok, Btree1} = couch_btree:add_remove(Btree, KeyValues, []),
    {Btree2, _} = lists:foldl(
        fun({K, _}, {BtAcc, PrevSize}) ->
            {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [], [K]),
            ?assert(couch_btree:size(BtAcc2) < PrevSize),
            {BtAcc2, couch_btree:size(BtAcc2)}
        end, {Btree1, couch_btree:size(Btree1)}, KeyValues),
    {
        "Should continuously remove keys from btree",
        [
            should_produce_valid_btree(Btree2, []),
            should_be_empty(Btree2)
        ]
    }.

should_insert_keys_in_reversed_order(KeyValues, {_, Btree}) ->
    KeyValuesRev = lists:reverse(KeyValues),
    {Btree1, _} = lists:foldl(
        fun(KV, {BtAcc, PrevSize}) ->
            {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [KV], []),
            ?assert(couch_btree:size(BtAcc2) > PrevSize),
            {BtAcc2, couch_btree:size(BtAcc2)}
        end, {Btree, couch_btree:size(Btree)}, KeyValuesRev),
    should_produce_valid_btree(Btree1, KeyValues).

should_add_every_odd_key_remove_every_even(KeyValues, {_, Btree}) ->
    {ok, Btree1} = couch_btree:add_remove(Btree, KeyValues, []),
    {_, Rem2Keys0, Rem2Keys1} = lists:foldl(fun(X, {Count, Left, Right}) ->
        case Count rem 2 == 0 of
            true -> {Count + 1, [X | Left], Right};
            false -> {Count + 1, Left, [X | Right]}
        end
                                            end, {0, [], []}, KeyValues),
    ?_assert(test_add_remove(Btree1, Rem2Keys0, Rem2Keys1)).

should_add_every_even_key_remove_every_old(KeyValues, {_, Btree}) ->
    {ok, Btree1} = couch_btree:add_remove(Btree, KeyValues, []),
    {_, Rem2Keys0, Rem2Keys1} = lists:foldl(fun(X, {Count, Left, Right}) ->
        case Count rem 2 == 0 of
            true -> {Count + 1, [X | Left], Right};
            false -> {Count + 1, Left, [X | Right]}
        end
                                            end, {0, [], []}, KeyValues),
    ?_assert(test_add_remove(Btree1, Rem2Keys1, Rem2Keys0)).


should_reduce_without_specified_direction({_, Btree}) ->
    ?_assertMatch(
        {ok, [{{"odd", _}, ?ROWS div 2}, {{"even", _}, ?ROWS div 2}]},
        fold_reduce(Btree, [])).

should_reduce_forward({_, Btree}) ->
    ?_assertMatch(
        {ok, [{{"odd", _}, ?ROWS div 2}, {{"even", _}, ?ROWS div 2}]},
        fold_reduce(Btree, [{dir, fwd}])).

should_reduce_backward({_, Btree}) ->
    ?_assertMatch(
        {ok, [{{"even", _}, ?ROWS div 2}, {{"odd", _}, ?ROWS div 2}]},
        fold_reduce(Btree, [{dir, rev}])).

should_reduce_whole_range(fwd, {_, Btree}) ->
    {SK, EK} = {{"even", 0}, {"odd", ?ROWS - 1}},
    [
        {
            "include endkey",
            ?_assertMatch(
                {ok, [{{"odd", 1}, ?ROWS div 2},
                      {{"even", 2}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, fwd},
                                    {start_key, SK},
                                    {end_key, EK}]))
        },
        {
            "exclude endkey",
            ?_assertMatch(
                {ok, [{{"odd", 1}, (?ROWS div 2) - 1},
                      {{"even", 2}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, fwd},
                                    {start_key, SK},
                                    {end_key_gt, EK}]))
        }
    ];
should_reduce_whole_range(rev, {_, Btree}) ->
    {SK, EK} = {{"odd", ?ROWS - 1}, {"even", 2}},
    [
        {
            "include endkey",
            ?_assertMatch(
                {ok, [{{"even", ?ROWS}, ?ROWS div 2},
                      {{"odd", ?ROWS - 1}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, rev},
                                    {start_key, SK},
                                    {end_key, EK}]))
        },
        {
            "exclude endkey",
            ?_assertMatch(
                {ok, [{{"even", ?ROWS}, (?ROWS div 2) - 1},
                      {{"odd", ?ROWS - 1}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, rev},
                                    {start_key, SK},
                                    {end_key_gt, EK}]))
        }
    ].

should_reduce_first_half(fwd, {_, Btree}) ->
    {SK, EK} = {{"even", 0}, {"odd", (?ROWS div 2) - 1}},
    [
        {
            "include endkey",
            ?_assertMatch(
                {ok, [{{"odd", 1}, ?ROWS div 4},
                      {{"even", 2}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, fwd},
                                    {start_key, SK}, {end_key, EK}]))
        },
        {
            "exclude endkey",
            ?_assertMatch(
                {ok, [{{"odd", 1}, (?ROWS div 4) - 1},
                      {{"even", 2}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, fwd},
                                    {start_key, SK},
                                    {end_key_gt, EK}]))
        }
    ];
should_reduce_first_half(rev, {_, Btree}) ->
    {SK, EK} = {{"odd", ?ROWS - 1}, {"even", ?ROWS div 2}},
    [
        {
            "include endkey",
            ?_assertMatch(
                {ok, [{{"even", ?ROWS}, (?ROWS div 4) + 1},
                      {{"odd", ?ROWS - 1}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, rev},
                                    {start_key, SK},
                                    {end_key, EK}]))
        },
        {
            "exclude endkey",
            ?_assertMatch(
                {ok, [{{"even", ?ROWS}, ?ROWS div 4},
                      {{"odd", ?ROWS - 1}, ?ROWS div 2}]},
                fold_reduce(Btree, [{dir, rev},
                                    {start_key, SK},
                                    {end_key_gt, EK}]))
        }
    ].

should_reduce_second_half(fwd, {_, Btree}) ->
    {SK, EK} = {{"even", ?ROWS div 2}, {"odd", ?ROWS - 1}},
    [
        {
            "include endkey",
            ?_assertMatch(
                {ok, [{{"odd", 1}, ?ROWS div 2},
                      {{"even", ?ROWS div 2}, (?ROWS div 4) + 1}]},
                fold_reduce(Btree, [{dir, fwd},
                                    {start_key, SK},
                                    {end_key, EK}]))
        },
        {
            "exclude endkey",
            ?_assertMatch(
                {ok, [{{"odd", 1}, (?ROWS div 2) - 1},
                      {{"even", ?ROWS div 2}, (?ROWS div 4) + 1}]},
                fold_reduce(Btree, [{dir, fwd},
                                    {start_key, SK},
                                    {end_key_gt, EK}]))
        }
    ];
should_reduce_second_half(rev, {_, Btree}) ->
    {SK, EK} = {{"odd", (?ROWS div 2) + 1}, {"even", 2}},
    [
        {
            "include endkey",
            ?_assertMatch(
                {ok, [{{"even", ?ROWS}, ?ROWS div 2},
                      {{"odd", (?ROWS div 2) + 1}, (?ROWS div 4) + 1}]},
                fold_reduce(Btree, [{dir, rev},
                                    {start_key, SK},
                                    {end_key, EK}]))
        },
        {
            "exclude endkey",
            ?_assertMatch(
                {ok, [{{"even", ?ROWS}, (?ROWS div 2) - 1},
                      {{"odd", (?ROWS div 2) + 1}, (?ROWS div 4) + 1}]},
                fold_reduce(Btree, [{dir, rev},
                                    {start_key, SK},
                                    {end_key_gt, EK}]))
        }
    ].

should_produce_valid_btree(Btree, KeyValues) ->
    ?_assert(test_btree(Btree, KeyValues)).

should_be_empty(Btree) ->
    ?_assertEqual(couch_btree:size(Btree), 0).

should_not_be_empty(Btree) ->
    ?_assert(couch_btree:size(Btree) > 0).

fold_reduce(Btree, Opts) ->
    GroupFun = fun({K1, _}, {K2, _}) ->
        K1 == K2
    end,
    FoldFun = fun(GroupedKey, Unreduced, Acc) ->
        {ok, [{GroupedKey, couch_btree:final_reduce(Btree, Unreduced)} | Acc]}
    end,
    couch_btree:fold_reduce(Btree, FoldFun, [],
                            [{key_group_fun, GroupFun}] ++ Opts).


keys(KVs) ->
    [K || {K, _} <- KVs].

reduce_fun(reduce, KVs) ->
    length(KVs);
reduce_fun(rereduce, Reds) ->
    lists:sum(Reds).


shuffle(List) ->
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(
        fun(_E, Acc) ->
            randomize(Acc)
        end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) -> {random:uniform(), A} end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

test_btree(Btree, KeyValues) ->
    ok = test_key_access(Btree, KeyValues),
    ok = test_lookup_access(Btree, KeyValues),
    ok = test_final_reductions(Btree, KeyValues),
    ok = test_traversal_callbacks(Btree, KeyValues),
    true.

test_add_remove(Btree, OutKeyValues, RemainingKeyValues) ->
    Btree2 = lists:foldl(
        fun({K, _}, BtAcc) ->
            {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [], [K]),
            BtAcc2
        end, Btree, OutKeyValues),
    true = test_btree(Btree2, RemainingKeyValues),

    Btree3 = lists:foldl(
        fun(KV, BtAcc) ->
            {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [KV], []),
            BtAcc2
        end, Btree2, OutKeyValues),
    true = test_btree(Btree3, OutKeyValues ++ RemainingKeyValues).

test_key_access(Btree, List) ->
    FoldFun = fun(Element, {[HAcc|TAcc], Count}) ->
        case Element == HAcc of
            true -> {ok, {TAcc, Count + 1}};
            _ -> {ok, {TAcc, Count + 1}}
        end
    end,
    Length = length(List),
    Sorted = lists:sort(List),
    {ok, _, {[], Length}} = couch_btree:foldl(Btree, FoldFun, {Sorted, 0}),
    {ok, _, {[], Length}} = couch_btree:fold(Btree, FoldFun,
                                             {Sorted, 0}, [{dir, rev}]),
    ok.

test_lookup_access(Btree, KeyValues) ->
    FoldFun = fun({Key, Value}, {Key, Value}) -> {stop, true} end,
    lists:foreach(
        fun({Key, Value}) ->
            [{ok, {Key, Value}}] = couch_btree:lookup(Btree, [Key]),
            {ok, _, true} = couch_btree:foldl(Btree, FoldFun,
                                              {Key, Value}, [{start_key, Key}])
        end, KeyValues).

test_final_reductions(Btree, KeyValues) ->
    KVLen = length(KeyValues),
    FoldLFun = fun(_X, LeadingReds, Acc) ->
        CountToStart = KVLen div 3 + Acc,
        CountToStart = couch_btree:final_reduce(Btree, LeadingReds),
        {ok, Acc + 1}
    end,
    FoldRFun = fun(_X, LeadingReds, Acc) ->
        CountToEnd = KVLen - KVLen div 3 + Acc,
        CountToEnd = couch_btree:final_reduce(Btree, LeadingReds),
        {ok, Acc + 1}
    end,
    {LStartKey, _} = case KVLen of
        0 -> {nil, nil};
        _ -> lists:nth(KVLen div 3 + 1, lists:sort(KeyValues))
    end,
    {RStartKey, _} = case KVLen of
        0 -> {nil, nil};
        _ -> lists:nth(KVLen div 3, lists:sort(KeyValues))
    end,
    {ok, _, FoldLRed} = couch_btree:foldl(Btree, FoldLFun, 0,
                                          [{start_key, LStartKey}]),
    {ok, _, FoldRRed} = couch_btree:fold(Btree, FoldRFun, 0,
                                         [{dir, rev}, {start_key, RStartKey}]),
    KVLen = FoldLRed + FoldRRed,
    ok.

test_traversal_callbacks(Btree, _KeyValues) ->
    FoldFun = fun
        (visit, _GroupedKey, _Unreduced, Acc) ->
            {ok, Acc andalso false};
        (traverse, _LK, _Red, Acc) ->
            {skip, Acc andalso true}
    end,
    % With 250 items the root is a kp. Always skipping should reduce to true.
    {ok, _, true} = couch_btree:fold(Btree, FoldFun, true, [{dir, fwd}]),
    ok.
