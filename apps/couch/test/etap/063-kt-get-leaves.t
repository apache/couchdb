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
    etap:plan(11),
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
    Stemmed = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],

    etap:is(
        {[{"foo", {0, ["1"]}}],[]},
        couch_key_tree:get(TwoChildSibs, [{0, "1"}]),
        "extract a subtree."
    ),

    etap:is(
        {[{"bar", {1, ["1a", "1"]}}],[]},
        couch_key_tree:get(TwoChildSibs, [{1, "1a"}]),
        "extract a subtree."
    ),

    etap:is(
        {[],[{0,"x"}]},
        couch_key_tree:get_key_leafs(TwoChildSibs, [{0, "x"}]),
        "gather up the leaves."
    ),

    etap:is(
        {[{"bar", {1, ["1a","1"]}}],[]},
        couch_key_tree:get_key_leafs(TwoChildSibs, [{1, "1a"}]),
        "gather up the leaves."
    ),

    etap:is(
        {[{"bar", {1, ["1a","1"]}},{"bar",{1, ["1b","1"]}}],[]},
        couch_key_tree:get_key_leafs(TwoChildSibs, [{0, "1"}]),
        "gather up the leaves."
    ),

    etap:is(
        {[{0,[{"1", "foo"}]}],[]},
        couch_key_tree:get_full_key_paths(TwoChildSibs, [{0, "1"}]),
        "retrieve full key paths."
    ),

    etap:is(
        {[{1,[{"1a", "bar"},{"1", "foo"}]}],[]},
        couch_key_tree:get_full_key_paths(TwoChildSibs, [{1, "1a"}]),
        "retrieve full key paths."
    ),

    etap:is(
        [{2, [{"1aa", "bar"},{"1a", "bar"}]}],
        couch_key_tree:get_all_leafs_full(Stemmed),
        "retrieve all leaves."
    ),

    etap:is(
        [{1, [{"1a", "bar"},{"1", "foo"}]}, {1, [{"1b", "bar"},{"1", "foo"}]}],
        couch_key_tree:get_all_leafs_full(TwoChildSibs),
        "retrieve all the leaves."
    ),

    etap:is(
        [{"bar", {2, ["1aa","1a"]}}],
        couch_key_tree:get_all_leafs(Stemmed),
        "retrieve all leaves."
    ),

    etap:is(
        [{"bar", {1, ["1a", "1"]}}, {"bar", {1, ["1b","1"]}}],
        couch_key_tree:get_all_leafs(TwoChildSibs),
        "retrieve all the leaves."
    ),

    ok.
