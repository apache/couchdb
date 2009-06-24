#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(6),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    OneChild = [{0, {"1","foo",[{"1a", "bar", []}]}}],
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    Stemmed = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],

    etap:is(
        {TwoChildSibs, []},
        couch_key_tree:remove_leafs(TwoChildSibs, []),
        "Removing no leaves has no effect on the tree."
    ),
    
    etap:is(
        {TwoChildSibs, []},
        couch_key_tree:remove_leafs(TwoChildSibs, [{0, "1"}]),
        "Removing a non-existant branch has no effect."
    ),
    
    etap:is(
        {OneChild, [{1, "1b"}]},
        couch_key_tree:remove_leafs(TwoChildSibs, [{1, "1b"}]),
        "Removing a leaf removes the leaf."
    ),
    
    etap:is(
        {[], [{1, "1b"},{1, "1a"}]},
        couch_key_tree:remove_leafs(TwoChildSibs, [{1, "1a"}, {1, "1b"}]),
        "Removing all leaves returns an empty tree."
    ),
    
    etap:is(
        {Stemmed, []},
        couch_key_tree:remove_leafs(Stemmed, [{1, "1a"}]),
        "Removing a non-existant node has no effect."
    ),
    
    etap:is(
        {[], [{2, "1aa"}]},
        couch_key_tree:remove_leafs(Stemmed, [{2, "1aa"}]),
        "Removing the last leaf returns an empty tree."
    ),

    ok.
