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

-module(couch_key_tree_prop_tests).

-ifdef(WITH_PROPER).

-include_lib("couch/include/couch_eunit_proper.hrl").

% How much to reduce size with tree depth.
-define(SIZE_REDUCTION, 3).
% Maximum number of branches.
-define(MAX_BRANCHES, 4).
-define(RAND_SIZE, 1 bsl 64).

property_test_() ->
    ?EUNIT_QUICKCHECK(60).

%
% Properties
%

% Merge random paths from a revtree into itself. Check that no revisions have
% been lost in the process and that result is one of the 3 expected values.
%
prop_revtree_merge_with_subset_of_own_nodes() ->
    ?FORALL(
        Revs,
        g_revs(),
        ?FORALL(
            {RevTree, Branch},
            {g_revtree(Revs), g_revtree(Revs, 1)},
            ?IMPLIES(
                length(Branch) > 0 andalso repeating_revs(levels(RevTree ++ Branch)) == [],
                begin
                    {Merged, Result} = couch_key_tree:merge(RevTree, hd(Branch)),
                    lists:member(Result, [new_leaf, new_branch, internal_node]) andalso
                        same_keys(RevTree ++ Branch, Merged) andalso
                        valid_revtree(Merged)
                end
            )
        )
    ).

% Merge random trees into revtree.
%
prop_revtree_merge_random_nodes() ->
    ?FORALL(
        {RevTree, Branch},
        {g_revtree(), g_revtree([], 1)},
        ?IMPLIES(
            length(Branch) > 0,
            begin
                {Merged, _} = couch_key_tree:merge(RevTree, hd(Branch)),
                valid_revtree(Merged)
            end
        )
    ).

% Merge mix or random and existing revtree paths into revtree
%
prop_revtree_merge_some_existing_some_new() ->
    ?FORALL(
        RevTree,
        g_revtree(),
        ?FORALL(
            Branch,
            begin
                KeyList = keylist(RevTree),
                Half = lists:sublist(KeyList, length(KeyList) div 2),
                g_revtree(Half, 1)
            end,
            ?IMPLIES(
                length(Branch) > 0 andalso repeating_revs(levels(RevTree ++ Branch)) == [],
                begin
                    {Merged, _} = couch_key_tree:merge(RevTree, hd(Branch)),
                    valid_revtree(Merged)
                end
            )
        )
    ).

% Stem deeper than the current max level. Expect no changes to the revtree
%
prop_no_change_stemming_deeper_than_current_depth() ->
    ?FORALL(
        RevTree,
        g_revtree(),
        begin
            StemDepth = depth(RevTree) + 1,
            Stemmed = couch_key_tree:stem(RevTree, StemDepth),
            StemmedKeys = lists:usort(keylist(Stemmed)),
            InputKeys = lists:usort(keylist(RevTree)),
            StemmedKeys == InputKeys
        end
    ).

% Stem at a random small depth, make sure that resulting tree has
% unique revisions and the same number or less revisions than input
%
prop_stemming_results_in_same_or_less_total_revs() ->
    ?FORALL(
        {RevTree, StemDepth},
        {g_revtree(), choose(1, 20)},
        begin
            Stemmed = couch_key_tree:stem(RevTree, StemDepth),
            OldRealDepth = real_depth(RevTree),
            StemmedKeys = keylist(Stemmed),
            UniqueStemmedKeys = lists:usort(StemmedKeys),
            UniqueInputKeys = lists:usort(keylist(RevTree)),
            NewRealDepth = real_depth(Stemmed),
            length(StemmedKeys) == length(UniqueStemmedKeys) andalso
                length(UniqueStemmedKeys) =< length(UniqueInputKeys) andalso
                OldRealDepth >= NewRealDepth
        end
    ).

% Generate a longer path (revtree with no branches) then stem it.
% Always expect it to shrink to stemmed depth.
prop_stem_path_expect_size_to_get_smaller() ->
    ?FORALL(
        {RevTree, StemDepth},
        {
            ?SIZED(Size, g_revtree(Size * 10, [], 1)),
            choose(1, 3)
        },
        ?IMPLIES(
            real_depth(RevTree) > 3,
            begin
                Stemmed = couch_key_tree:stem(RevTree, StemDepth),
                StemmedKeys = lists:usort(keylist(Stemmed)),
                InputKeys = lists:usort(keylist(RevTree)),
                length(InputKeys) > length(StemmedKeys) andalso
                    real_depth(Stemmed) == StemDepth
            end
        )
    ).

% After stemming all leaves are still present
prop_after_stemming_all_leaves_are_present() ->
    ?FORALL(
        {RevTree, StemDepth},
        {g_revtree(), choose(1, 20)},
        begin
            OldRealDepth = real_depth(RevTree),
            OldLeaves = leaves(RevTree),
            Stemmed = couch_key_tree:stem(RevTree, StemDepth),
            NewRealDepth = real_depth(Stemmed),
            NewLeaves = leaves(Stemmed),
            valid_revtree(Stemmed) andalso
                OldRealDepth >= NewRealDepth andalso
                OldLeaves == NewLeaves
        end
    ).

% After stemming paths to root didn't get longer
prop_after_stemming_paths_are_shorter() ->
    ?FORALL(
        {StemDepth, RevTree},
        {choose(2, 10), g_revtree()},
        begin
            OldPaths = paths(RevTree),
            Stemmed = couch_key_tree:stem(RevTree, StemDepth),
            NewPaths = paths(Stemmed),
            GrowingPaths = orddict:fold(
                fun(Rev, Path, Acc) ->
                    OldPath = orddict:fetch(Rev, OldPaths),
                    case length(Path) > length(OldPath) of
                        true ->
                            [{Rev, Path, OldPath} | Acc];
                        false ->
                            Acc
                    end
                end,
                [],
                NewPaths
            ),
            valid_revtree(Stemmed) andalso GrowingPaths == []
        end
    ).

% Check leaf count
prop_leaf_count() ->
    ?FORALL(
        RevTree,
        g_revtree(),
        length(leaves(RevTree)) == couch_key_tree:count_leafs(RevTree)
    ).

% Check get leafs
prop_get_leafs() ->
    ?FORALL(
        RevTree,
        g_revtree(),
        begin
            LeafsFull = couch_key_tree:get_all_leafs(RevTree),
            lists:usort([Rev || {_V, {_D, [Rev | _]}} <- LeafsFull]) == leaves(RevTree)
        end
    ).

%
% Generators
%

% Generate a full rev tree. Most of the forms are just there to set up default
% parameters, _revtree/3 does all heavy lifting.
%

g_revtree() ->
    ?SIZED(Size, g_revtree(Size)).

g_revtree(Size) when is_integer(Size) ->
    g_revtree(Size, [], ?MAX_BRANCHES);
g_revtree(Revs) when is_list(Revs) ->
    ?SIZED(Size, g_revtree(Size, Revs, ?MAX_BRANCHES)).

g_revtree(Size, Revs) when is_integer(Size), is_list(Revs) ->
    g_revtree(Size, Revs, ?MAX_BRANCHES);
g_revtree(Revs, MaxBranches) when is_list(Revs), is_integer(MaxBranches) ->
    ?SIZED(Size, g_revtree(Size, Revs, MaxBranches)).

g_revtree(0, _Revs, _MaxBranches) ->
    [];
g_revtree(Size, ERevs, MaxBranches) ->
    ?LET(
        {Depth, Revs},
        {g_stem_depth(Size), g_revs(Size, ERevs)},
        [{Depth, g_treenode(Size, Revs, MaxBranches)}]
    ).

% Generate a tree node and then recursively generate its children.
%
g_treenode(0, Revs, _) ->
    {elements(Revs), x, []};
g_treenode(Size, Revs, MaxBranches) ->
    ?LAZY(
        ?LET(
            N,
            choose(0, MaxBranches),
            begin
                [Rev | ChildRevs] = Revs,
                {Rev, x, g_nodes(Size div ?SIZE_REDUCTION, N, ChildRevs, MaxBranches)}
            end
        )
    ).

% Generate a list of child nodes. Depending on how many children there are
% the pre-generarated revision list is split into that many sublists.
%
g_nodes(0, _N, _Revs, _MaxBranches) ->
    [];
g_nodes(_Size, 0, _Revs, _MaxBranches) ->
    [];
g_nodes(Size, ChildCount, Revs, MaxBranches) ->
    ?LETSHRINK(
        ChildNodes,
        begin
            ChildRevList = child_revs(ChildCount, Revs, Size, MaxBranches),
            [g_treenode(Size, ChildRevs, MaxBranches) || ChildRevs <- ChildRevList]
        end,
        ordered_nodes(ChildNodes)
    ).

% Generate each subtree's stem depth
%

g_stem_depth(Size) ->
    choose(0, expected_height(Size, ?SIZE_REDUCTION) div 2).

% Uses the shuffle/1 function to shuffle the input list. Unshuffled list is
% used as the shrink value.
%
g_shuffle([]) ->
    [];
g_shuffle(L) when is_list(L) ->
    ?LET(X, elements(L), [X | g_shuffle(lists:delete(X, L))]).

% Wrapper to make a list shuffling generator that doesn't shrink
%
g_shuffle_noshrink(L) when is_list(L) ->
    proper_types:noshrink(g_shuffle(L)).

% Generate shuffled sublists up to N items long from a list.
%
g_shuffled_sublists(L, N) ->
    ?LET(Shuffled, g_shuffle_noshrink(L), lists:sublist(Shuffled, N)).

% Generate revision lists.
%
g_revs() ->
    ?SIZED(Size, g_revs(Size)).

g_revs(Size) when is_integer(Size) ->
    g_revs(Size, []).

g_revs(Size, Existing) when is_integer(Size), is_list(Existing) ->
    Expected = keys_needed(Size, ?SIZE_REDUCTION, ?MAX_BRANCHES),
    Revs = revs(Expected, Existing),
    case length(Revs) > Expected of
        % have extra, try various sublists
        true ->
            g_shuffled_sublists(Revs, Expected);
        false ->
            proper_types:return(Revs)
    end.

%
% Helper functions
%

valid_revtree(RevTree) ->
    repeating_revs(levels(RevTree)) == [] andalso children_sorted(RevTree).

same_keys(RevTree1, RevTree2) ->
    Keys1 = lists:usort(keylist(RevTree1)),
    Keys2 = lists:usort(keylist(RevTree2)),
    Keys1 == Keys2.

all(L) ->
    lists:all(fun(E) -> E end, L).

% Generate list of relateively unique large random numbers
rand_list(N) when N =< 0 ->
    [];
rand_list(N) ->
    [rand:uniform(?RAND_SIZE) || _ <- lists:seq(1, N)].

% Generate a list of revisions to be used as key in revision trees. Expected
% must the number of maximum expected nodes in a revision tree. Existing is an
% optional list revisions which must be included in the result. The output list
% is sorted.
revs(0, _Existing) ->
    [];
revs(Expected, Existing) when is_integer(Expected), is_list(Existing) ->
    Need = Expected - length(Existing),
    lists:usort(lists:append(Existing, rand_list(Need))).

% Get the list of all the keys in a revision tree. The input can also be a
% an individual tree (tagged with the depth to virtual root) or a node.
% Yes, this is not tail recursive but the idea is to keep it simple.
%
keylist({_D, Node}) when is_tuple(Node) ->
    keylist(Node);
keylist({K, _V, Nodes}) ->
    [K | keylist(Nodes)];
keylist(Nodes) ->
    lists:append([keylist(Node) || Node <- Nodes]).

% Get the list of leaves from a revision tree.
leaves([]) ->
    [];
leaves({_D, Node}) when is_tuple(Node) ->
    leaves(Node);
leaves({K, _V, []}) ->
    [K];
leaves({_K, _V, Nodes}) ->
    leaves(Nodes);
leaves(Nodes) ->
    lists:usort(lists:append([leaves(N) || N <- Nodes])).

% Get paths from leaf to root. Result is an orddict of [{LeafRev, [Rev]}]
%
paths([]) ->
    orddict:new();
paths(RevTree) when is_list(RevTree) ->
    paths_merge_dicts([paths(T) || T <- RevTree]);
paths({_Depth, Node}) when is_tuple(Node) ->
    paths(Node);
paths({K, _V, []}) ->
    orddict:store(K, [], orddict:new());
paths({K, _V, Nodes}) ->
    CombinedDict = paths_merge_dicts([paths(N) || N <- Nodes]),
    orddict:map(fun(_LeafKey, Path) -> Path ++ [K] end, CombinedDict).

paths_merge_dicts(Dicts) ->
    lists:foldl(
        fun(D, AccD) ->
            orddict:merge(
                fun(K, V1, V2) ->
                    throw({found_duplicates, K, V1, V2})
                end,
                D,
                AccD
            )
        end,
        orddict:new(),
        Dicts
    ).

% Get lists of all the keys at each depth level. Result is an orddict that
% looks like [{depth, [key]}]. The depth used here is the "virtual" depth as
% indicated by the stemmed depth tag that goes with every top level subtree.
%
levels([]) ->
    orddict:new();
levels(RevTree) when is_list(RevTree) ->
    lists:foldl(fun(T, Dict) -> levels(T, Dict) end, orddict:new(), RevTree).

levels({Depth, Node}, Dict) when is_tuple(Node) ->
    levels(Node, Depth, Dict).

levels({K, _V, Nodes}, Depth, Dict) ->
    Dict1 =
        case orddict:is_key(Depth, Dict) of
            true -> orddict:append(Depth, K, Dict);
            false -> orddict:store(Depth, [K], Dict)
        end,
    levels(Nodes, Depth + 1, Dict1);
levels(Nodes, Depth, Dict) ->
    lists:foldl(
        fun(Node, AccDict) ->
            levels(Node, Depth, AccDict)
        end,
        Dict,
        Nodes
    ).

% Using the output of leaves/1 as input return any repeating revisions if
% there are any at a particular level. Levels which have not revisions are
% not returned.
%
repeating_revs(Dict) ->
    orddict:filter(
        fun(_Depth, Revs) ->
            length(lists:usort(Revs)) =/= length(Revs)
        end,
        Dict
    ).

% Check that children of all nodes are sorted
children_sorted([]) ->
    true;
children_sorted(Nodes) when is_list(Nodes) ->
    all([children_sorted(N) || N <- Nodes]);
children_sorted({_D, Node}) when is_tuple(Node) ->
    children_sorted(Node);
children_sorted({_K, _V, Nodes}) ->
    children_sorted(Nodes).

% Get the maximum depth of a revtree. The depth is "virtual" as it takes into
% account the distance to the now stemmed root node as indicated by the top
% level subtrees.
%
depth([]) ->
    0;
depth(RevTree) when is_list(RevTree) ->
    lists:max([depth(T) || T <- RevTree]);
depth({Depth, Node}) when is_tuple(Node) ->
    depth(Node, Depth - 1).

depth({_K, _V, Nodes}, Depth) ->
    depth(Nodes, Depth + 1);
depth([], Depth) ->
    Depth;
depth(Nodes, Depth) ->
    lists:max([depth(Node, Depth) || Node <- Nodes]).

% Get the "real" tree depth, not the virtual one. As revtrees gets stemmed they
% will keep their virtual depth but the actual number of nodes in the tree
% could be reduced.
%
real_depth([]) ->
    0;
real_depth(RevTree) when is_list(RevTree) ->
    lists:max([real_depth(T) || T <- RevTree]);
real_depth({_Depth, Node}) when is_tuple(Node) ->
    % Note from here on use the depth/3 function
    depth(Node, 0).

% Return an ordered list of revtree nodes. When sorting only immediate keys
% (revisions) are looked at and comparison doesn't descent into the treee.
%
ordered_nodes(Nodes) ->
    lists:sort(fun({K1, _, _}, {K2, _, _}) -> K1 =< K2 end, Nodes).

% Calculate a maximum number of rev tree nodes needed for a tree of a given
% height and branchiness. Height is derived from Size and LevelReductionFactor,
% that is how big the sample should be and quickly the size parameter would
% shrink on each level.
%
keys_needed(0, _, _) ->
    0;
keys_needed(Size, LevelReductionFactor, 1) ->
    expected_height(Size, LevelReductionFactor);
keys_needed(Size, LevelReductionFactor, Branches) ->
    Height = expected_height(Size, LevelReductionFactor),
    trunc(math:pow(Branches, Height + 1)) + 1.

% Calculate expected tree height for a given sample size and branchiness.
% At each step the size is divided by the reduction factor.
expected_height(Size, LevelReductionFactor) ->
    trunc(log(LevelReductionFactor, Size)) + 1.

log(B, X) ->
    math:log(X) / math:log(B).

% Distribute items in a list into roughly equal chunks of a given size.
%
distribute(_ChunkSize, []) ->
    [];
distribute(ChunkSize, L) when ChunkSize >= length(L) ->
    [L];
distribute(ChunkSize, L) ->
    {L1, L2} = lists:split(ChunkSize, L),
    [L1 | distribute(ChunkSize, L2)].

% Split a single (parent) revision list into chunks (sub-lists), one for each
% child. Also, for safety, double check that at this point in the process the
% list of revisions is sufficiently large. If it isn't something went wrong and
% a specific exception is thrown ({not_enough_revisions, Got, Needed}).
%
child_revs(ChildCount, Revs, Size, MaxBranches) ->
    NeedKeys = keys_needed(Size, ?SIZE_REDUCTION, MaxBranches),
    case length(Revs) >= NeedKeys of
        true ->
            ChunkSize = trunc(length(Revs) / ChildCount) + 1,
            distribute(ChunkSize, Revs);
        false ->
            throw({not_enough_revisions, length(Revs), NeedKeys})
    end.

-endif.
