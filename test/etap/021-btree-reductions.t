#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/couchdb -sasl errlog_type error -boot start_sasl -noshell

-define(FILE_NAME, "./t/temp.021").
-define(ROWS, 1000).

main(_) ->
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
    
    {ok, Fd} = couch_file:open(?FILE_NAME, [create,overwrite]),
    {ok, Btree} = couch_btree:open(nil, Fd, [{reduce, ReduceFun}]),
    
    % Create a list, of {"even", Value} or {"odd", Value} pairs.
    {_, EvenOddKVs} = lists:foldl(fun(Idx, {Key, Acc}) ->
        case Key of
            "even" -> {"odd", [{{Key, Idx}, 1} | Acc]};
            _ -> {"even", [{{Key, Idx}, 1} | Acc]}
        end
    end, {"odd", []}, lists:seq(1, ?ROWS)),

    {ok, Btree2} = couch_btree:add_remove(Btree, EvenOddKVs, []),

    GroupFun = fun({K1, _}, {K2, _}) -> K1 == K2 end,
    FoldFun = fun(GroupedKey, Unreduced, Acc) ->
        {ok, [{GroupedKey, couch_btree:final_reduce(Btree2, Unreduced)} | Acc]}
    end,

    {SK1, EK1} = {{"even", -1}, {"even", foo}},
    {SK2, EK2} = {{"odd", -1}, {"odd", foo}},

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, ?ROWS div 2}, {{"even", _}, ?ROWS div 2}]}) ->
                true;
            (_) ->
                false
        end,    
        couch_btree:fold_reduce(Btree2, nil, nil, GroupFun, FoldFun, []),
        "Reduction works with no specified direction, startkey, or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, ?ROWS div 2}, {{"even", _}, ?ROWS div 2}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, fwd, nil, nil, GroupFun, FoldFun, []),
        "Reducing forward works with no startkey or endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"even", _}, ?ROWS div 2}, {{"odd", _}, ?ROWS div 2}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, rev, nil, nil, GroupFun, FoldFun, []),
        "Reducing backwards works with no startkey or endkey."
    ),
    
    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, ?ROWS div 2}, {{"even", _}, ?ROWS div 2}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, fwd, SK1, EK2, GroupFun, FoldFun, []),
        "Reducing works over the entire range with startkey and endkey set."
    ),
    
    etap:fun_is(
        fun
            ({ok, [{{"even", _}, ?ROWS div 2}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, fwd, SK1, EK1, GroupFun, FoldFun, []),
        "Reducing foward over first half works with a startkey and endkey."
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, ?ROWS div 2}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, fwd, SK2, EK2, GroupFun, FoldFun, []),
        "Reducing foward over second half works with second startkey and endkey"
    ),

    etap:fun_is(
        fun
            ({ok, [{{"odd", _}, ?ROWS div 2}]}) -> true;
            (_) -> false
        end,
        couch_btree:fold_reduce(Btree2, rev, EK2, SK2, GroupFun, FoldFun, []),
        "Reducing in reverse works after swapping the startkey and endkey."
    ),
    
    etap:fun_is(
        fun
            ({ok, [{{"even", _}, ?ROWS div 2}, {{"odd", _}, ?ROWS div 2}]}) ->
                true;
            (_) ->
                false
        end,
        couch_btree:fold_reduce(Btree2, rev, EK2, SK1, GroupFun, FoldFun, []),
        "Reducing in reverse results in reversed accumulator."
    ),

    couch_file:close(Fd).
