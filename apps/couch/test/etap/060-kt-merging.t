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
    etap:plan(12),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    One = {0, {"1","foo",[]}},
    TwoSibs = [{0, {"1","foo",[]}},
               {0, {"2","foo",[]}}],
    OneChild = {0, {"1","foo",[{"1a", "bar", []}]}},
    TwoChild = {0, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}},
    TwoChildSibs = {0, {"1","foo", [{"1a", "bar", []},
                                     {"1b", "bar", []}]}},
    TwoChildSibs2 = {0, {"1","foo", [{"1a", "bar", []},
                                     {"1b", "bar", [{"1bb", "boo", []}]}]}},
    Stemmed1b = {1, {"1a", "bar", []}},
    Stemmed1a = {1, {"1a", "bar", [{"1aa", "bar", []}]}},
    Stemmed1aa = {2, {"1aa", "bar", []}},
    Stemmed1bb = {2, {"1bb", "boo", []}},

    etap:is(
        {[One], no_conflicts},
        couch_key_tree:merge([], One, 10),
        "The empty tree is the identity for merge."
    ),

    etap:is(
        {TwoSibs, no_conflicts},
        couch_key_tree:merge(TwoSibs, One, 10),
        "Merging a prefix of a tree with the tree yields the tree."
    ),

    etap:is(
        {[One], no_conflicts},
        couch_key_tree:merge([One], One, 10),
        "Merging is reflexive."
    ),

    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], TwoChild, 10),
        "Merging two children is still reflexive."
    ),

    etap:is(
        {[TwoChildSibs], no_conflicts},
        couch_key_tree:merge([TwoChildSibs], TwoChildSibs, 10),
        "Merging a tree to itself is itself."),

    etap:is(
        {[TwoChildSibs], no_conflicts},
        couch_key_tree:merge([TwoChildSibs], Stemmed1b, 10),
        "Merging a tree with a stem."
    ),

    etap:is(
        {[TwoChildSibs2], no_conflicts},
        couch_key_tree:merge([TwoChildSibs2], Stemmed1bb, 10),
        "Merging a stem at a deeper level."
    ),

    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], Stemmed1aa, 10),
        "Merging a single tree with a deeper stem."
    ),

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

    ok.
