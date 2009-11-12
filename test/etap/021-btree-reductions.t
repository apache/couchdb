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
    etap:plan(8),
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
        couch_btree:fold_reduce(Btree2, nil, nil, GroupFun, FoldFun, []),
        "Reduction works with no specified direction, startkey, or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}, {{"even", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, fwd, nil, nil, GroupFun, FoldFun, []),
        "Reducing forward works with no startkey or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"even", _}, 500}, {{"odd", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, rev, nil, nil, GroupFun, FoldFun, []),
        "Reducing backwards works with no startkey or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}, {{"even", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, fwd, SK1, EK2, GroupFun, FoldFun, []),
        "Reducing works over the entire range with startkey and endkey set."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"even", _}, 500}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, fwd, SK1, EK1, GroupFun, FoldFun, []),
        "Reducing foward over first half works with a startkey and endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, fwd, SK2, EK2, GroupFun, FoldFun, []),
        "Reducing foward over second half works with second startkey and endkey"
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, 500}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, rev, EK2, SK2, GroupFun, FoldFun, []),
        "Reducing in reverse works after swapping the startkey and endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"even", _}, 500}, {{"odd", _}, 500}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, rev, EK2, SK1, GroupFun, FoldFun, []),
        "Reducing in reverse results in reversed accumulator."
    ),

    couch_file:close(Fd).
