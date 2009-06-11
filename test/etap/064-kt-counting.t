#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(4),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    EmptyTree = [],
    One = [{0, {"1","foo",[]}}],
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    Stemmed = [{2, {"1bb", "boo", []}}],
    
    etap:is(0, couch_key_tree:count_leafs(EmptyTree),
        "Empty trees have no leaves."),

    etap:is(1, couch_key_tree:count_leafs(One),
        "Single node trees have a single leaf."),

    etap:is(2, couch_key_tree:count_leafs(TwoChildSibs),
        "Two children siblings counted as two leaves."),
    
    etap:is(1, couch_key_tree:count_leafs(Stemmed),
        "Stemming does not affect leaf counting."),
    
    ok.
