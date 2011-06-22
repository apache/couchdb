#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    test_util:init_code_path(),
    etap:plan(16),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    One = {1, {"1","foo",[]}},

    etap:is(
        {[One], no_conflicts},
        couch_key_tree:merge([], One, 10),
        "The empty tree is the identity for merge."
    ),
    etap:is(
        {[One], no_conflicts},
        couch_key_tree:merge([One], One, 10),
        "Merging is reflexive."
    ),

    TwoSibs = [{1, {"1","foo",[]}},
               {1, {"2","foo",[]}}],

    etap:is(
        {TwoSibs, no_conflicts},
        couch_key_tree:merge(TwoSibs, One, 10),
        "Merging a prefix of a tree with the tree yields the tree."
    ),

    Three = {1, {"3","foo",[]}},
    ThreeSibs = [{1, {"1","foo",[]}},
                 {1, {"2","foo",[]}},
                 {1, {"3","foo",[]}}],

    etap:is(
        {ThreeSibs, conflicts},
        couch_key_tree:merge(TwoSibs, Three, 10),
        "Merging a third unrelated branch leads to a conflict."
    ),


    TwoChild = {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}},

    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], TwoChild, 10),
        "Merging two children is still reflexive."
    ),

    TwoChildSibs = {1, {"1","foo", [{"1a", "bar", []},
                                     {"1b", "bar", []}]}},
    etap:is(
        {[TwoChildSibs], no_conflicts},
        couch_key_tree:merge([TwoChildSibs], TwoChildSibs, 10),
        "Merging a tree to itself is itself."),

    TwoChildPlusSibs =
        {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]},
                         {"1b", "bar", []}]}},

    etap:is(
        {[TwoChildPlusSibs], no_conflicts},
        couch_key_tree:merge([TwoChild], TwoChildSibs, 10),
        "Merging tree of uneven length at node 2."),

    Stemmed1b = {2, {"1a", "bar", []}},
    etap:is(
        {[TwoChildSibs], no_conflicts},
        couch_key_tree:merge([TwoChildSibs], Stemmed1b, 10),
        "Merging a tree with a stem."
    ),

    TwoChildSibs2 = {1, {"1","foo", [{"1a", "bar", []},
                                     {"1b", "bar", [{"1bb", "boo", []}]}]}},
    Stemmed1bb = {3, {"1bb", "boo", []}},
    etap:is(
        {[TwoChildSibs2], no_conflicts},
        couch_key_tree:merge([TwoChildSibs2], Stemmed1bb, 10),
        "Merging a stem at a deeper level."
    ),

    StemmedTwoChildSibs2 = [{2,{"1a", "bar", []}},
                            {2,{"1b", "bar", [{"1bb", "boo", []}]}}],

    etap:is(
        {StemmedTwoChildSibs2, no_conflicts},
        couch_key_tree:merge(StemmedTwoChildSibs2, Stemmed1bb, 10),
        "Merging a stem at a deeper level against paths at deeper levels."
    ),

    Stemmed1aa = {3, {"1aa", "bar", []}},
    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], Stemmed1aa, 10),
        "Merging a single tree with a deeper stem."
    ),

    Stemmed1a = {2, {"1a", "bar", [{"1aa", "bar", []}]}},
    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], Stemmed1a, 10),
        "Merging a larger stem."
    ),

    etap:is(
        {[Stemmed1a], no_conflicts},
        couch_key_tree:merge([Stemmed1a], Stemmed1aa, 10),
        "More merging."
    ),

    OneChild = {1, {"1","foo",[{"1a", "bar", []}]}},
    Expect1 = [OneChild, Stemmed1aa],
    etap:is(
        {Expect1, conflicts},
        couch_key_tree:merge([OneChild], Stemmed1aa, 10),
        "Merging should create conflicts."
    ),

    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge(Expect1, TwoChild, 10),
        "Merge should have no conflicts."
    ),

    %% this test is based on couch-902-test-case2.py
    %% foo has conflicts from replication at depth two
    %% foo3 is the current value
    Foo = {1, {"foo",
               "val1",
               [{"foo2","val2",[]},
                {"foo3", "val3", []}
               ]}},
    %% foo now has an attachment added, which leads to foo4 and val4
    %% off foo3
    Bar = {1, {"foo",
               [],
               [{"foo3",
                 [],
                 [{"foo4","val4",[]}
                  ]}]}},
    %% this is what the merge returns
    %% note that it ignore the conflicting branch as there's no match
    FooBar = {1, {"foo",
               "val1",
               [{"foo2","val2",[]},
                {"foo3", "val3", [{"foo4","val4",[]}]}
               ]}},

    etap:is(
      {[FooBar], no_conflicts},
      couch_key_tree:merge([Foo],Bar,10),
      "Merging trees with conflicts ought to behave."
    ),

    ok.
