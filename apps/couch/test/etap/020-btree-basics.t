#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/couchdb -sasl errlog_type error -boot start_sasl -noshell

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

filename() -> test_util:build_file("test/etap/temp.020").
rows() -> 250.

-record(btree, {fd, root, extract_kv, assemble_kv, less, reduce}).

main(_) ->
    test_util:init_code_path(),
    etap:plan(48),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

%% @todo Determine if this number should be greater to see if the btree was
%% broken into multiple nodes. AKA "How do we appropiately detect if multiple
%% nodes were created."
test()->
    Sorted = [{Seq, random:uniform()} || Seq <- lists:seq(1, rows())],
    etap:ok(test_kvs(Sorted), "Testing sorted keys"),
    etap:ok(test_kvs(lists:reverse(Sorted)), "Testing reversed sorted keys"),
    etap:ok(test_kvs(shuffle(Sorted)), "Testing shuffled keys."),
    ok.

test_kvs(KeyValues) ->
    ReduceFun = fun
        (reduce, KVs) ->
            length(KVs);
        (rereduce, Reds) ->
            lists:sum(Reds)
    end,

    Keys = [K || {K, _} <- KeyValues],

    {ok, Fd} = couch_file:open(filename(), [create,overwrite]),
    {ok, Btree} = couch_btree:open(nil, Fd),
    etap:ok(is_record(Btree, btree), "Created btree is really a btree record"),
    etap:is(Btree#btree.fd, Fd, "Btree#btree.fd is set correctly."),
    etap:is(Btree#btree.root, nil, "Btree#btree.root is set correctly."),

    Btree1 = couch_btree:set_options(Btree, [{reduce, ReduceFun}]),
    etap:is(Btree1#btree.reduce, ReduceFun, "Reduce function was set"),
    {ok, _, EmptyRes} = couch_btree:foldl(Btree1, fun(_, X) -> {ok, X+1} end, 0),
    etap:is(EmptyRes, 0, "Folding over an empty btree"),

    {ok, Btree2} = couch_btree:add_remove(Btree1, KeyValues, []),
    etap:ok(test_btree(Btree2, KeyValues),
        "Adding all keys at once returns a complete btree."),

    etap:fun_is(
        fun
            ({ok, {kp_node, _}}) -> true;
            (_) -> false
        end,
        couch_file:pread_term(Fd, element(1, Btree2#btree.root)),
        "Btree root pointer is a kp_node."
    ),

    {ok, Btree3} = couch_btree:add_remove(Btree2, [], Keys),
    etap:ok(test_btree(Btree3, []),
        "Removing all keys at once returns an empty btree."),

    Btree4 = lists:foldl(fun(KV, BtAcc) ->
        {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [KV], []),
        BtAcc2
    end, Btree3, KeyValues),
    etap:ok(test_btree(Btree4, KeyValues),
        "Adding all keys one at a time returns a complete btree."),

    Btree5 = lists:foldl(fun({K, _}, BtAcc) ->
        {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [], [K]),
        BtAcc2
    end, Btree4, KeyValues),
    etap:ok(test_btree(Btree5, []),
        "Removing all keys one at a time returns an empty btree."),

    KeyValuesRev = lists:reverse(KeyValues),
    Btree6 = lists:foldl(fun(KV, BtAcc) ->
        {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [KV], []),
        BtAcc2
    end, Btree5, KeyValuesRev),
    etap:ok(test_btree(Btree6, KeyValues),
        "Adding all keys in reverse order returns a complete btree."),

    {_, Rem2Keys0, Rem2Keys1} = lists:foldl(fun(X, {Count, Left, Right}) ->
        case Count rem 2 == 0 of
            true-> {Count+1, [X | Left], Right};
            false -> {Count+1, Left, [X | Right]}
        end
    end, {0, [], []}, KeyValues),

    etap:ok(test_add_remove(Btree6, Rem2Keys0, Rem2Keys1),
        "Add/Remove every other key."),

    etap:ok(test_add_remove(Btree6, Rem2Keys1, Rem2Keys0),
        "Add/Remove opposite every other key."),

    {ok, Btree7} = couch_btree:add_remove(Btree6, [], [K||{K,_}<-Rem2Keys1]),
    {ok, Btree8} = couch_btree:add_remove(Btree7, [], [K||{K,_}<-Rem2Keys0]),
    etap:ok(test_btree(Btree8, []),
        "Removing both halves of every other key returns an empty btree."),

    %% Third chunk (close out)
    etap:is(couch_file:close(Fd), ok, "closing out"),
    true.

test_btree(Btree, KeyValues) ->
    ok = test_key_access(Btree, KeyValues),
    ok = test_lookup_access(Btree, KeyValues),
    ok = test_final_reductions(Btree, KeyValues),
    true.

test_add_remove(Btree, OutKeyValues, RemainingKeyValues) ->
    Btree2 = lists:foldl(fun({K, _}, BtAcc) ->
        {ok, BtAcc2} = couch_btree:add_remove(BtAcc, [], [K]),
        BtAcc2
    end, Btree, OutKeyValues),
    true = test_btree(Btree2, RemainingKeyValues),

    Btree3 = lists:foldl(fun(KV, BtAcc) ->
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
    {ok, _, {[], Length}} = couch_btree:fold(Btree, FoldFun, {Sorted, 0}, [{dir, rev}]),
    ok.

test_lookup_access(Btree, KeyValues) ->
    FoldFun = fun({Key, Value}, {Key, Value}) -> {stop, true} end,
    lists:foreach(fun({Key, Value}) ->
        [{ok, {Key, Value}}] = couch_btree:lookup(Btree, [Key]),
        {ok, _, true} = couch_btree:foldl(Btree, FoldFun, {Key, Value}, [{start_key, Key}])
    end, KeyValues).

test_final_reductions(Btree, KeyValues) ->
    KVLen = length(KeyValues),
    FoldLFun = fun(_X, LeadingReds, Acc) ->
        CountToStart = KVLen div 3 + Acc,
        CountToStart = couch_btree:final_reduce(Btree, LeadingReds),
        {ok, Acc+1}
    end,
    FoldRFun = fun(_X, LeadingReds, Acc) ->
        CountToEnd = KVLen - KVLen div 3 + Acc,
        CountToEnd = couch_btree:final_reduce(Btree, LeadingReds),
        {ok, Acc+1}
    end,
    {LStartKey, _} = case KVLen of
        0 -> {nil, nil};
        _ -> lists:nth(KVLen div 3 + 1, lists:sort(KeyValues))
    end,
    {RStartKey, _} = case KVLen of
        0 -> {nil, nil};
        _ -> lists:nth(KVLen div 3, lists:sort(KeyValues))
    end,
    {ok, _, FoldLRed} = couch_btree:foldl(Btree, FoldLFun, 0, [{start_key, LStartKey}]),
    {ok, _, FoldRRed} = couch_btree:fold(Btree, FoldRFun, 0, [{dir, rev}, {start_key, RStartKey}]),
    KVLen = FoldLRed + FoldRRed,
    ok.

shuffle(List) ->
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
        randomize(Acc)
    end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) ->
        {random:uniform(), A}
    end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.
