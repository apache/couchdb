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

filename() -> "./test/etap/temp.021".
rows() -> 1000.

main(_) ->
    test_util:init_code_path(),
    etap:plan(20),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test()->
    ReduceFun = fun
        (reduce, KVs) -> length(KVs);
        (rereduce, Reds) -> lists:sum(Reds)
    end,

    {ok, Fd} = couch_file:open(filename(), [create,overwrite]),
    {ok, Btree} = couch_btree:open(nil, Fd, [{reduce, ReduceFun}]),

    % Create a list, of {"even", Value} or {"odd", Value} pairs.
    {_, EvenOddKVs} = lists:foldl(fun(Idx, {Key, Acc}) ->
        case Key of
            "even" -> {"odd", [{{Key, Idx}, 1} | Acc]};
            _ -> {"even", [{{Key, Idx}, 1} | Acc]}
        end
    end, {"odd", []}, lists:seq(1, rows())),

    {ok, Btree2} = couch_btree:add_remove(Btree, EvenOddKVs, []),

    GroupFun = fun({K1, _}, {K2, _}) -> K1 == K2 end,
    FoldFun = fun(GroupedKey, Unreduced, Acc) ->
        {ok, [{GroupedKey, couch_btree:final_reduce(Btree2, Unreduced)} | Acc]}
    end,

    {SK1, EK1} = {{"even", -1}, {"even", foo}},
    {SK2, EK2} = {{"odd", -1}, {"odd", foo}},

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}, {{"even", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{key_group_fun, GroupFun}]),
        "Reduction works with no specified direction, startkey, or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}, {{"even", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{key_group_fun, GroupFun}, {dir, fwd}]),
        "Reducing forward works with no startkey or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"even", _}, 500}, {{"odd", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{key_group_fun, GroupFun}, {dir, rev}]),
        "Reducing backwards works with no startkey or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}, {{"even", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{dir, fwd}, {key_group_fun, GroupFun}, {start_key, SK1}, {end_key, EK2}]),
        "Reducing works over the entire range with startkey and endkey set."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"even", _}, 500}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{dir, fwd}, {key_group_fun, GroupFun}, {start_key, SK1}, {end_key, EK1}]),
        "Reducing forward over first half works with a startkey and endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{dir, fwd}, {key_group_fun, GroupFun}, {start_key, SK2}, {end_key, EK2}]),
        "Reducing forward over second half works with second startkey and endkey"
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{dir, rev}, {key_group_fun, GroupFun}, {start_key, EK2}, {end_key, SK2}]),
        "Reducing in reverse works after swapping the startkey and endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"even", _}, 500}, {{"odd", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, FoldFun, [], [{dir, rev}, {key_group_fun, GroupFun}, {start_key, EK2}, {end_key, SK1}]),
        "Reducing in reverse results in reversed accumulator."
    ),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, fwd}, {key_group_fun, GroupFun},
            {start_key, {"even", 0}}, {end_key, {"odd", rows() + 1}}
        ]),
        {ok, [{{"odd", 1}, 500}, {{"even", 2}, 500}]},
        "Right fold reduce value for whole range with inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, fwd}, {key_group_fun, GroupFun},
            {start_key, {"even", 0}}, {end_key_gt, {"odd", 999}}
        ]),
        {ok, [{{"odd", 1}, 499}, {{"even", 2}, 500}]},
        "Right fold reduce value for whole range without inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, rev}, {key_group_fun, GroupFun},
            {start_key, {"odd", 999}}, {end_key, {"even", 2}}
        ]),
        {ok, [{{"even", 1000}, 500}, {{"odd", 999}, 500}]},
        "Right fold reduce value for whole reversed range with inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, rev}, {key_group_fun, GroupFun},
            {start_key, {"odd", 999}}, {end_key_gt, {"even", 2}}
        ]),
        {ok, [{{"even", 1000}, 499}, {{"odd", 999}, 500}]},
        "Right fold reduce value for whole reversed range without inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, fwd}, {key_group_fun, GroupFun},
            {start_key, {"even", 0}}, {end_key, {"odd", 499}}
        ]),
        {ok, [{{"odd", 1}, 250}, {{"even", 2}, 500}]},
        "Right fold reduce value for first half with inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, fwd}, {key_group_fun, GroupFun},
            {start_key, {"even", 0}}, {end_key_gt, {"odd", 499}}
        ]),
        {ok, [{{"odd", 1}, 249}, {{"even", 2}, 500}]},
        "Right fold reduce value for first half without inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, rev}, {key_group_fun, GroupFun},
            {start_key, {"odd", 999}}, {end_key, {"even", 500}}
        ]),
        {ok, [{{"even", 1000}, 251}, {{"odd", 999}, 500}]},
        "Right fold reduce value for first half reversed with inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, rev}, {key_group_fun, GroupFun},
            {start_key, {"odd", 999}}, {end_key_gt, {"even", 500}}
        ]),
        {ok, [{{"even", 1000}, 250}, {{"odd", 999}, 500}]},
        "Right fold reduce value for first half reversed without inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, fwd}, {key_group_fun, GroupFun},
            {start_key, {"even", 500}}, {end_key, {"odd", 999}}
        ]),
        {ok, [{{"odd", 1}, 500}, {{"even", 500}, 251}]},
        "Right fold reduce value for second half with inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, fwd}, {key_group_fun, GroupFun},
            {start_key, {"even", 500}}, {end_key_gt, {"odd", 999}}
        ]),
        {ok, [{{"odd", 1}, 499}, {{"even", 500}, 251}]},
        "Right fold reduce value for second half without inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, rev}, {key_group_fun, GroupFun},
            {start_key, {"odd", 501}}, {end_key, {"even", 2}}
        ]),
        {ok, [{{"even", 1000}, 500}, {{"odd", 501}, 251}]},
        "Right fold reduce value for second half reversed with inclusive end key"),

    etap:is(
        couch_btree:fold_reduce(Btree2, FoldFun, [], [
            {dir, rev}, {key_group_fun, GroupFun},
            {start_key, {"odd", 501}}, {end_key_gt, {"even", 2}}
        ]),
        {ok, [{{"even", 1000}, 499}, {{"odd", 501}, 251}]},
        "Right fold reduce value for second half reversed without inclusive end key"),

    couch_file:close(Fd).
