#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(unknown),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    Stemmed1 = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    Stemmed2 = [{2, {"1aa", "bar", []}}],
    
    etap:is(
        [],
        couch_key_tree:find_missing(TwoChildSibs, [{0,"1"}, {1,"1a"}]),
        "Look for missing keys."
    ),

    etap:is(
        [{0, "10"}, {100, "x"}],
        couch_key_tree:find_missing(
            TwoChildSibs,
            [{0,"1"}, {0, "10"}, {1,"1a"}, {100, "x"}]
        ),
        "Look for missing keys."
    ),

    etap:is(
        [{0, "1"}, {100, "x"}],
        couch_key_tree:find_missing(
            Stemmed1,
            [{0,"1"}, {1,"1a"}, {100, "x"}]
        ),
        "Look for missing keys."
    ),
    etap:is(
        [{0, "1"}, {1,"1a"}, {100, "x"}],
        couch_key_tree:find_missing(
            Stemmed2,
            [{0,"1"}, {1,"1a"}, {100, "x"}]
        ),
        "Look for missing keys."
    ),

    ok.
