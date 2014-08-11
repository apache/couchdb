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

-module(couch_key_tree_tests).

-include("couch_eunit.hrl").

-define(DEPTH, 10).


key_tree_merge_test_()->
    {
        "Key tree merge",
        [
            should_merge_with_empty_tree(),
            should_merge_reflexive(),
            should_merge_prefix_of_a_tree_with_tree(),
            should_produce_conflict_on_merge_with_unrelated_branch(),
            should_merge_reflexive_for_child_nodes(),
            should_merge_tree_to_itself(),
            should_merge_tree_of_odd_length(),
            should_merge_tree_with_stem(),
            should_merge_with_stem_at_deeper_level(),
            should_merge_with_stem_at_deeper_level_with_deeper_paths(),
            should_merge_single_tree_with_deeper_stem(),
            should_merge_tree_with_large_stem(),
            should_merge_stems(),
            should_create_conflicts_on_merge(),
            should_create_no_conflicts_on_merge(),
            should_ignore_conflicting_branch()
        ]
    }.

key_tree_missing_leaves_test_()->
    {
        "Missing tree leaves",
        [
            should_not_find_missing_leaves(),
            should_find_missing_leaves()
        ]
    }.

key_tree_remove_leaves_test_()->
    {
        "Remove tree leaves",
        [
            should_have_no_effect_on_removing_no_leaves(),
            should_have_no_effect_on_removing_non_existant_branch(),
            should_remove_leaf(),
            should_produce_empty_tree_on_removing_all_leaves(),
            should_have_no_effect_on_removing_non_existant_node(),
            should_produce_empty_tree_on_removing_last_leaf()
        ]
    }.

key_tree_get_leaves_test_()->
    {
        "Leaves retrieving",
        [
            should_extract_subtree(),
            should_extract_subsubtree(),
            should_gather_non_existant_leaf(),
            should_gather_leaf(),
            shoul_gather_multiple_leaves(),
            should_retrieve_full_key_path(),
            should_retrieve_full_key_path_for_node(),
            should_retrieve_leaves_with_parent_node(),
            should_retrieve_all_leaves()
        ]
    }.

key_tree_leaf_counting_test_()->
    {
        "Leaf counting",
        [
            should_have_no_leaves_for_empty_tree(),
            should_have_single_leaf_for_tree_with_single_node(),
            should_have_two_leaves_for_tree_with_chindler_siblings(),
            should_not_affect_on_leaf_counting_for_stemmed_tree()
        ]
    }.

key_tree_stemming_test_()->
    {
        "Stemming",
        [
            should_have_no_effect_for_stemming_more_levels_than_exists(),
            should_return_one_deepest_node(),
            should_return_two_deepest_nodes()
        ]
    }.


should_merge_with_empty_tree()->
    One = {1, {"1","foo",[]}},
    ?_assertEqual({[One], no_conflicts},
                  couch_key_tree:merge([], One, ?DEPTH)).

should_merge_reflexive()->
    One = {1, {"1","foo",[]}},
    ?_assertEqual({[One], no_conflicts},
                  couch_key_tree:merge([One], One, ?DEPTH)).

should_merge_prefix_of_a_tree_with_tree()->
    One = {1, {"1","foo",[]}},
    TwoSibs = [{1, {"1","foo",[]}},
               {1, {"2","foo",[]}}],
    ?_assertEqual({TwoSibs, no_conflicts},
                  couch_key_tree:merge(TwoSibs, One, ?DEPTH)).

should_produce_conflict_on_merge_with_unrelated_branch()->
    TwoSibs = [{1, {"1","foo",[]}},
               {1, {"2","foo",[]}}],
    Three = {1, {"3","foo",[]}},
    ThreeSibs = [{1, {"1","foo",[]}},
                 {1, {"2","foo",[]}},
                 {1, {"3","foo",[]}}],
    ?_assertEqual({ThreeSibs, conflicts},
                  couch_key_tree:merge(TwoSibs, Three, ?DEPTH)).

should_merge_reflexive_for_child_nodes()->
    TwoChild = {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}},
    ?_assertEqual({[TwoChild], no_conflicts},
                  couch_key_tree:merge([TwoChild], TwoChild, ?DEPTH)).

should_merge_tree_to_itself()->
    TwoChildSibs = {1, {"1","foo", [{"1a", "bar", []},
                                    {"1b", "bar", []}]}},
    ?_assertEqual({[TwoChildSibs], no_conflicts},
                  couch_key_tree:merge([TwoChildSibs], TwoChildSibs, ?DEPTH)).

should_merge_tree_of_odd_length()->
    TwoChild = {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}},
    TwoChildSibs = {1, {"1","foo", [{"1a", "bar", []},
                                    {"1b", "bar", []}]}},
    TwoChildPlusSibs = {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]},
                                        {"1b", "bar", []}]}},

    ?_assertEqual({[TwoChildPlusSibs], no_conflicts},
                  couch_key_tree:merge([TwoChild], TwoChildSibs, ?DEPTH)).

should_merge_tree_with_stem()->
    Stemmed = {2, {"1a", "bar", []}},
    TwoChildSibs = {1, {"1","foo", [{"1a", "bar", []},
                                    {"1b", "bar", []}]}},

    ?_assertEqual({[TwoChildSibs], no_conflicts},
                  couch_key_tree:merge([TwoChildSibs], Stemmed, ?DEPTH)).

should_merge_with_stem_at_deeper_level()->
    Stemmed = {3, {"1bb", "boo", []}},
    TwoChildSibs = {1, {"1","foo", [{"1a", "bar", []},
                                    {"1b", "bar", [{"1bb", "boo", []}]}]}},
    ?_assertEqual({[TwoChildSibs], no_conflicts},
                  couch_key_tree:merge([TwoChildSibs], Stemmed, ?DEPTH)).

should_merge_with_stem_at_deeper_level_with_deeper_paths()->
    Stemmed = {3, {"1bb", "boo", []}},
    StemmedTwoChildSibs = [{2,{"1a", "bar", []}},
                           {2,{"1b", "bar", [{"1bb", "boo", []}]}}],
    ?_assertEqual({StemmedTwoChildSibs, no_conflicts},
                  couch_key_tree:merge(StemmedTwoChildSibs, Stemmed, ?DEPTH)).

should_merge_single_tree_with_deeper_stem()->
    Stemmed = {3, {"1aa", "bar", []}},
    TwoChild = {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}},
    ?_assertEqual({[TwoChild], no_conflicts},
                  couch_key_tree:merge([TwoChild], Stemmed, ?DEPTH)).

should_merge_tree_with_large_stem()->
    Stemmed = {2, {"1a", "bar", [{"1aa", "bar", []}]}},
    TwoChild = {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}},
    ?_assertEqual({[TwoChild], no_conflicts},
                  couch_key_tree:merge([TwoChild], Stemmed, ?DEPTH)).

should_merge_stems()->
    StemmedA = {2, {"1a", "bar", [{"1aa", "bar", []}]}},
    StemmedB = {3, {"1aa", "bar", []}},
    ?_assertEqual({[StemmedA], no_conflicts},
                  couch_key_tree:merge([StemmedA], StemmedB, ?DEPTH)).

should_create_conflicts_on_merge()->
    OneChild = {1, {"1","foo",[{"1a", "bar", []}]}},
    Stemmed = {3, {"1aa", "bar", []}},
    ?_assertEqual({[OneChild, Stemmed], conflicts},
                  couch_key_tree:merge([OneChild], Stemmed, ?DEPTH)).

should_create_no_conflicts_on_merge()->
    OneChild = {1, {"1","foo",[{"1a", "bar", []}]}},
    Stemmed = {3, {"1aa", "bar", []}},
    TwoChild = {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}},
    ?_assertEqual({[TwoChild], no_conflicts},
                  couch_key_tree:merge([OneChild, Stemmed], TwoChild, ?DEPTH)).

should_ignore_conflicting_branch()->
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
    {
        "COUCHDB-902",
        ?_assertEqual({[FooBar], no_conflicts},
                      couch_key_tree:merge([Foo], Bar, ?DEPTH))
    }.

should_not_find_missing_leaves()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual([],
                  couch_key_tree:find_missing(TwoChildSibs,
                                              [{0,"1"}, {1,"1a"}])).

should_find_missing_leaves()->
    Stemmed1 = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    Stemmed2 = [{2, {"1aa", "bar", []}}],
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    [
        ?_assertEqual(
            [{0, "10"}, {100, "x"}],
            couch_key_tree:find_missing(
                TwoChildSibs,
                [{0,"1"}, {0, "10"}, {1,"1a"}, {100, "x"}])),
        ?_assertEqual(
            [{0, "1"}, {100, "x"}],
            couch_key_tree:find_missing(
                Stemmed1,
                [{0,"1"}, {1,"1a"}, {100, "x"}])),
        ?_assertEqual(
            [{0, "1"}, {1,"1a"}, {100, "x"}],
            couch_key_tree:find_missing(
                Stemmed2,
                [{0,"1"}, {1,"1a"}, {100, "x"}]))
    ].

should_have_no_effect_on_removing_no_leaves()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({TwoChildSibs, []},
                  couch_key_tree:remove_leafs(TwoChildSibs,
                                              [])).

should_have_no_effect_on_removing_non_existant_branch()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({TwoChildSibs, []},
                  couch_key_tree:remove_leafs(TwoChildSibs,
                                              [{0, "1"}])).

should_remove_leaf()->
    OneChild = [{0, {"1","foo",[{"1a", "bar", []}]}}],
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({OneChild, [{1, "1b"}]},
                  couch_key_tree:remove_leafs(TwoChildSibs,
                                              [{1, "1b"}])).

should_produce_empty_tree_on_removing_all_leaves()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[], [{1, "1b"}, {1, "1a"}]},
                  couch_key_tree:remove_leafs(TwoChildSibs,
                                              [{1, "1b"}, {1, "1a"}])).

should_have_no_effect_on_removing_non_existant_node()->
    Stemmed = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    ?_assertEqual({Stemmed, []},
                  couch_key_tree:remove_leafs(Stemmed,
                                              [{1, "1a"}])).

should_produce_empty_tree_on_removing_last_leaf()->
    Stemmed = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    ?_assertEqual({[], [{2, "1aa"}]},
                  couch_key_tree:remove_leafs(Stemmed,
                                              [{2, "1aa"}])).

should_extract_subtree()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[{"foo", {0, ["1"]}}],[]},
                  couch_key_tree:get(TwoChildSibs, [{0, "1"}])).

should_extract_subsubtree()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[{"bar", {1, ["1a", "1"]}}],[]},
                  couch_key_tree:get(TwoChildSibs, [{1, "1a"}])).

should_gather_non_existant_leaf()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[],[{0, "x"}]},
                  couch_key_tree:get_key_leafs(TwoChildSibs, [{0, "x"}])).

should_gather_leaf()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[{"bar", {1, ["1a","1"]}}],[]},
                  couch_key_tree:get_key_leafs(TwoChildSibs, [{1, "1a"}])).

shoul_gather_multiple_leaves()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[{"bar", {1, ["1a","1"]}},{"bar",{1, ["1b","1"]}}],[]},
                  couch_key_tree:get_key_leafs(TwoChildSibs, [{0, "1"}])).

should_retrieve_full_key_path()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[{0,[{"1", "foo"}]}],[]},
                  couch_key_tree:get_full_key_paths(TwoChildSibs, [{0, "1"}])).

should_retrieve_full_key_path_for_node()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual({[{1,[{"1a", "bar"},{"1", "foo"}]}],[]},
                  couch_key_tree:get_full_key_paths(TwoChildSibs, [{1, "1a"}])).

should_retrieve_leaves_with_parent_node()->
    Stemmed = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    [
        ?_assertEqual([{2, [{"1aa", "bar"},{"1a", "bar"}]}],
                      couch_key_tree:get_all_leafs_full(Stemmed)),
        ?_assertEqual([{1, [{"1a", "bar"},{"1", "foo"}]},
                       {1, [{"1b", "bar"},{"1", "foo"}]}],
                      couch_key_tree:get_all_leafs_full(TwoChildSibs))
    ].

should_retrieve_all_leaves()->
    Stemmed = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    [
        ?_assertEqual([{"bar", {2, ["1aa","1a"]}}],
                      couch_key_tree:get_all_leafs(Stemmed)),
        ?_assertEqual([{"bar", {1, ["1a", "1"]}}, {"bar", {1, ["1b","1"]}}],
                      couch_key_tree:get_all_leafs(TwoChildSibs))
    ].

should_have_no_leaves_for_empty_tree()->
    ?_assertEqual(0, couch_key_tree:count_leafs([])).

should_have_single_leaf_for_tree_with_single_node()->
    ?_assertEqual(1, couch_key_tree:count_leafs([{0, {"1","foo",[]}}])).

should_have_two_leaves_for_tree_with_chindler_siblings()->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    ?_assertEqual(2, couch_key_tree:count_leafs(TwoChildSibs)).

should_not_affect_on_leaf_counting_for_stemmed_tree()->
    ?_assertEqual(1, couch_key_tree:count_leafs([{2, {"1bb", "boo", []}}])).

should_have_no_effect_for_stemming_more_levels_than_exists()->
    TwoChild = [{0, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}}],
    ?_assertEqual(TwoChild, couch_key_tree:stem(TwoChild, 3)).

should_return_one_deepest_node()->
    TwoChild = [{0, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}}],
    Stemmed = [{2, {"1aa", "bar", []}}],
    ?_assertEqual(Stemmed, couch_key_tree:stem(TwoChild, 1)).

should_return_two_deepest_nodes()->
    TwoChild = [{0, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}}],
    Stemmed = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    ?_assertEqual(Stemmed, couch_key_tree:stem(TwoChild, 2)).
