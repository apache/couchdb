#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(3),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    TwoChild = [{0, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}}],
    Stemmed1 = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    Stemmed2 = [{2, {"1aa", "bar", []}}],

    etap:is(TwoChild, couch_key_tree:stem(TwoChild, 3),
        "Stemming more levels than what exists does nothing."),

    etap:is(Stemmed1, couch_key_tree:stem(TwoChild, 2),
        "Stemming with a depth of two returns the deepest two nodes."),

    etap:is(Stemmed2, couch_key_tree:stem(TwoChild, 1),
        "Stemming to a depth of one returns the deepest node."),

    ok.
