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

%% @doc Data structure used to represent document edit histories.

%% A key tree is used to represent the edit history of a document. Each node of
%% the tree represents a particular version. Relations between nodes represent
%% the order that these edits were applied. For instance, a set of three edits
%% would produce a tree of versions A->B->C indicating that edit C was based on
%% version B which was in turn based on A. In a world without replication (and
%% no ability to disable MVCC checks), all histories would be forced to be
%% linear lists of edits due to constraints imposed by MVCC (ie, new edits must
%% be based on the current version). However, we have replication, so we must
%% deal with not so easy cases, which lead to trees.
%%
%% Consider a document in state A. This doc is replicated to a second node. We
%% then edit the document on each node leaving it in two different states, B
%% and C. We now have two key trees, A->B and A->C. When we go to replicate a
%% second time, the key tree must combine these two trees which gives us
%% A->(B|C). This is how conflicts are introduced. In terms of the key tree, we
%% say that we have two leaves (B and C) that are not deleted. The presense of
%% the multiple leaves indicate conflict. To remove a conflict, one of the
%% edits (B or C) can be deleted, which results in, A->(B|C->D) where D is an
%% edit that is specially marked with the a deleted=true flag.
%%
%% What makes this a bit more complicated is that there is a limit to the
%% number of revisions kept, specified in couch_db.hrl (default is 1000). When
%% this limit is exceeded only the last 1000 are kept. This comes in to play
%% when branches are merged. The comparison has to begin at the same place in
%% the branches. A revision id is of the form N-XXXXXXX where N is the current
%% revision depth. So each path will have a start number, calculated in
%% couch_doc:to_path using the formula N - length(RevIds) + 1 So, .eg. if a doc
%% was edit 1003 times this start number would be 4, indicating that 3
%% revisions were truncated.
%%
%% This comes into play in @see merge_at/3 which recursively walks down one
%% tree or the other until they begin at the same revision.

-module(couch_key_tree).

-export([
count_leafs/1,
find_missing/2,
fold/3,
get/2,
get_all_leafs/1,
get_all_leafs_full/1,
get_full_key_paths/2,
get_key_leafs/2,
map/2,
map_leafs/2,
mapfold/3,
merge/3,
merge/2,
remove_leafs/2,
stem/2
]).

-include_lib("couch/include/couch_db.hrl").
-type treenode() :: {Key::term(), Value::term(), [Node::treenode()]}.
-type tree() :: {Depth::pos_integer(), [treenode()]}.
-type revtree() :: [tree()].


%% @doc Merge a path into the given tree and then stem the result.
%% Although Tree is of type tree(), it must not contain any branches.
-spec merge(revtree(), tree(), pos_integer()) ->
                {revtree(), new_leaf | new_branch | internal_node}.
merge(RevTree, Tree, StemDepth) ->
    {Merged, Result} = merge(RevTree, Tree),
    case config:get("couchdb", "stem_interactive_updates", "true") of
        "true" -> {stem(Merged, StemDepth), Result};
        _ -> {Merged, Result}
    end.


%% @doc Merge a path into a tree.
-spec merge(revtree(), tree()) ->
                {revtree(), new_leaf | new_branch | internal_node}.
merge(RevTree, Tree) ->
    {Merged, Result} = merge_tree(RevTree, Tree, []),
    {lists:sort(Merged), Result}.

%% @private
%% @doc Attempt to merge Tree into each branch of the RevTree.
%% If it can't find a branch that the new tree merges into, add it as a
%% new branch in the RevTree.
-spec merge_tree(revtree(), tree(), revtree()) ->
                {revtree(), new_leaf | new_branch | internal_node}.
merge_tree([], Tree, []) ->
    {[Tree], new_leaf};
merge_tree([], Tree, MergeAcc) ->
    {[Tree|MergeAcc], new_branch};
merge_tree([{Depth, Nodes} | Rest], {IDepth, INodes}=Tree, MergeAcc) ->
    % For the intrepid observer following along at home, notice what we're
    % doing here with (Depth - IDepth). This tells us which of the two
    % branches (Nodes or INodes) we need to seek into. If Depth > IDepth
    % that means we need go into INodes to find where we line up with
    % Nodes. If Depth < IDepth, its obviously the other way. If it turns
    % out that (Depth - IDepth) == 0, then we know that this is where
    % we begin our actual merge operation (ie, looking for key matches).
    % Its helpful to note that this whole moving into sub-branches is due
    % to how we store trees that have been stemmed. When a path is
    % stemmed so that the root node is lost, we wrap it in a tuple with
    % the number keys that have been droped. This number is the depth
    % value that's used throughout this module.
    case merge_at([Nodes], Depth - IDepth, [INodes]) of
        {[Merged], Result} ->
            NewDepth = erlang:min(Depth, IDepth),
            {Rest ++ [{NewDepth, Merged} | MergeAcc], Result};
        fail ->
            merge_tree(Rest, Tree, [{Depth, Nodes} | MergeAcc])
    end.

%% @private
%% @doc Locate the point at which merging can start.
%% Because of stemming we may need to seek into one of the branches
%% before we can start comparing node keys. If one of the branches
%% ends up running out of nodes we know that these two branches can
%% not be merged.
-spec merge_at([node()], integer(), [node()]) ->
                {revtree(), new_leaf | new_branch | internal_node} | fail.
merge_at(_Nodes, _Pos, []) ->
    fail;
merge_at([], _Pos, _INodes) ->
    fail;
merge_at(Nodes, Pos, [{IK, IV, [NextINode]}]) when Pos > 0 ->
    % Depth was bigger than IDepth, so we need to discard from the
    % insert path to find where it might start matching.
    case merge_at(Nodes, Pos - 1, [NextINode]) of
        {Merged, Result} -> {[{IK, IV, Merged}], Result};
        fail -> fail
    end;
merge_at(_Nodes, Pos, [{_IK, _IV, []}]) when Pos > 0 ->
    % We've run out of path on the insert side, there's no way we can
    % merge with this branch
    fail;
merge_at([{K, V, SubTree} | Sibs], Pos, INodes) when Pos < 0 ->
    % When Pos is negative, Depth was less than IDepth, so we
    % need to discard from the revision tree path
    case merge_at(SubTree, Pos + 1, INodes) of
        {Merged, Result} ->
            {[{K, V, Merged} | Sibs], Result};
        fail ->
            % Merging along the subtree failed. We need to also try
            % merging the insert branch against the siblings of this
            % node.
            case merge_at(Sibs, Pos, INodes) of
                {Merged, Result} -> {[{K, V, SubTree} | Merged], Result};
                fail -> fail
            end
    end;
merge_at([{K, V1, Nodes} | Sibs], 0, [{K, V2, INodes}]) ->
    % Keys are equal. At this point we have found a possible starting
    % position for our merge to take place.
    {Merged, Result} = merge_extend(Nodes, INodes),
    {[{K, value_pref(V1, V2), Merged} | Sibs], Result};
merge_at([{K1, _, _} | _], 0, [{K2, _, _}]) when K1 > K2 ->
    % Siblings keys are ordered, no point in continuing
    fail;
merge_at([Tree | Sibs], 0, INodes) ->
    % INodes key comes after this key, so move on to the next sibling.
    case merge_at(Sibs, 0, INodes) of
        {Merged, Result} -> {[Tree | Merged], Result};
        fail -> fail
    end.

-spec merge_extend(tree(), tree()) ->
                {tree(), new_leaf | new_branch | internal_node}.
merge_extend([], B) when B =/= [] ->
    % Most likely the insert branch simply extends this one, so the new
    % branch is exactly B. Its also possible that B is a branch because
    % its key sorts greater than all siblings of an internal node. This
    % condition is checked in the last clause of this function and the
    % new_leaf result is fixed to be new_branch.
    {B, new_leaf};
merge_extend(A, []) ->
    % Insert branch ends an internal node in our original revtree()
    % so the end result is exactly our original revtree.
    {A, internal_node};
merge_extend([{K, V1, SubA} | NextA], [{K, V2, SubB}]) ->
    % Here we're simply extending the path to the next deeper
    % level in the two branches.
    {Merged, Result} = merge_extend(SubA, SubB),
    {[{K, value_pref(V1, V2), Merged} | NextA], Result};
merge_extend([{K1, _, _}=NodeA | Rest], [{K2, _, _}=NodeB]) when K1 > K2 ->
    % Keys are ordered so we know this is where the insert branch needs
    % to be inserted into the tree. We also know that this creates a new
    % branch so we have a new leaf to report.
    {[NodeB, NodeA | Rest], new_branch};
merge_extend([Tree | RestA], NextB) ->
    % Here we're moving on to the next sibling to try and extend our
    % merge even deeper. The length check is due to the fact that the
    % key in NextB might be larger than the largest key in RestA which
    % means we've created a new branch.
    {Merged, Result0} = merge_extend(RestA, NextB),
    Result = case length(Merged) == length(RestA) of
        true -> Result0;
        false -> new_branch
    end,
    {[Tree | Merged], Result}.

find_missing(_Tree, []) ->
    [];
find_missing([], SeachKeys) ->
    SeachKeys;
find_missing([{Start, {Key, Value, SubTree}} | RestTree], SeachKeys) ->
    PossibleKeys = [{KeyPos, KeyValue} || {KeyPos, KeyValue} <- SeachKeys, KeyPos >= Start],
    ImpossibleKeys = [{KeyPos, KeyValue} || {KeyPos, KeyValue} <- SeachKeys, KeyPos < Start],
    Missing = find_missing_simple(Start, [{Key, Value, SubTree}], PossibleKeys),
    find_missing(RestTree, ImpossibleKeys ++ Missing).

find_missing_simple(_Pos, _Tree, []) ->
    [];
find_missing_simple(_Pos, [], SeachKeys) ->
    SeachKeys;
find_missing_simple(Pos, [{Key, _, SubTree} | RestTree], SeachKeys) ->
    PossibleKeys = [{KeyPos, KeyValue} || {KeyPos, KeyValue} <- SeachKeys, KeyPos >= Pos],
    ImpossibleKeys = [{KeyPos, KeyValue} || {KeyPos, KeyValue} <- SeachKeys, KeyPos < Pos],

    SrcKeys2 = PossibleKeys -- [{Pos, Key}],
    SrcKeys3 = find_missing_simple(Pos + 1, SubTree, SrcKeys2),
    ImpossibleKeys ++ find_missing_simple(Pos, RestTree, SrcKeys3).


filter_leafs([], _Keys, FilteredAcc, RemovedKeysAcc) ->
    {FilteredAcc, RemovedKeysAcc};
filter_leafs([{Pos, [{LeafKey, _}|_]} = Path |Rest], Keys, FilteredAcc, RemovedKeysAcc) ->
    FilteredKeys = lists:delete({Pos, LeafKey}, Keys),
    if FilteredKeys == Keys ->
        % this leaf is not a key we are looking to remove
        filter_leafs(Rest, Keys, [Path | FilteredAcc], RemovedKeysAcc);
    true ->
        % this did match a key, remove both the node and the input key
        filter_leafs(Rest, FilteredKeys, FilteredAcc, [{Pos, LeafKey} | RemovedKeysAcc])
    end.

% Removes any branches from the tree whose leaf node(s) are in the Keys
remove_leafs(Trees, Keys) ->
    % flatten each branch in a tree into a tree path
    Paths = get_all_leafs_full(Trees),

    % filter out any that are in the keys list.
    {FilteredPaths, RemovedKeys} = filter_leafs(Paths, Keys, [], []),

    SortedPaths = lists:sort(
        [{Pos + 1 - length(Path), Path} || {Pos, Path} <- FilteredPaths]
    ),

    % convert paths back to trees
    NewTree = lists:foldl(
        fun({StartPos, Path},TreeAcc) ->
            [SingleTree] = lists:foldl(
                fun({K,V},NewTreeAcc) -> [{K,V,NewTreeAcc}] end, [], Path),
            {NewTrees, _} = merge(TreeAcc, {StartPos, SingleTree}),
            NewTrees
        end, [], SortedPaths),
    {NewTree, RemovedKeys}.


% get the leafs in the tree matching the keys. The matching key nodes can be
% leafs or an inner nodes. If an inner node, then the leafs for that node
% are returned.
get_key_leafs(Tree, Keys) ->
    get_key_leafs(Tree, Keys, []).

get_key_leafs(_, [], Acc) ->
    {Acc, []};
get_key_leafs([], Keys, Acc) ->
    {Acc, Keys};
get_key_leafs([{Pos, Tree}|Rest], Keys, Acc) ->
    {Gotten, RemainingKeys} = get_key_leafs_simple(Pos, [Tree], Keys, []),
    get_key_leafs(Rest, RemainingKeys, Gotten ++ Acc).

get_key_leafs_simple(_Pos, _Tree, [], _KeyPathAcc) ->
    {[], []};
get_key_leafs_simple(_Pos, [], KeysToGet, _KeyPathAcc) ->
    {[], KeysToGet};
get_key_leafs_simple(Pos, [{Key, _Value, SubTree}=Tree | RestTree], KeysToGet, KeyPathAcc) ->
    case lists:delete({Pos, Key}, KeysToGet) of
    KeysToGet -> % same list, key not found
        {LeafsFound, KeysToGet2} = get_key_leafs_simple(Pos + 1, SubTree, KeysToGet, [Key | KeyPathAcc]),
        {RestLeafsFound, KeysRemaining} = get_key_leafs_simple(Pos, RestTree, KeysToGet2, KeyPathAcc),
        {LeafsFound ++ RestLeafsFound, KeysRemaining};
    KeysToGet2 ->
        LeafsFound = get_all_leafs_simple(Pos, [Tree], KeyPathAcc),
        LeafKeysFound = [{LeafPos, LeafRev} || {_, {LeafPos, [LeafRev|_]}}
            <- LeafsFound],
        KeysToGet3 = KeysToGet2 -- LeafKeysFound,
        {RestLeafsFound, KeysRemaining} = get_key_leafs_simple(Pos, RestTree, KeysToGet3, KeyPathAcc),
        {LeafsFound ++ RestLeafsFound, KeysRemaining}
    end.

get(Tree, KeysToGet) ->
    {KeyPaths, KeysNotFound} = get_full_key_paths(Tree, KeysToGet),
    FixedResults = [ {Value, {Pos, [Key0 || {Key0, _} <- Path]}} || {Pos, [{_Key, Value}|_]=Path} <- KeyPaths],
    {FixedResults, KeysNotFound}.

get_full_key_paths(Tree, Keys) ->
    get_full_key_paths(Tree, Keys, []).

get_full_key_paths(_, [], Acc) ->
    {Acc, []};
get_full_key_paths([], Keys, Acc) ->
    {Acc, Keys};
get_full_key_paths([{Pos, Tree}|Rest], Keys, Acc) ->
    {Gotten, RemainingKeys} = get_full_key_paths(Pos, [Tree], Keys, []),
    get_full_key_paths(Rest, RemainingKeys, Gotten ++ Acc).


get_full_key_paths(_Pos, _Tree, [], _KeyPathAcc) ->
    {[], []};
get_full_key_paths(_Pos, [], KeysToGet, _KeyPathAcc) ->
    {[], KeysToGet};
get_full_key_paths(Pos, [{KeyId, Value, SubTree} | RestTree], KeysToGet, KeyPathAcc) ->
    KeysToGet2 = KeysToGet -- [{Pos, KeyId}],
    CurrentNodeResult =
    case length(KeysToGet2) =:= length(KeysToGet) of
    true -> % not in the key list.
        [];
    false -> % this node is the key list. return it
        [{Pos, [{KeyId, Value} | KeyPathAcc]}]
    end,
    {KeysGotten, KeysRemaining} = get_full_key_paths(Pos + 1, SubTree, KeysToGet2, [{KeyId, Value} | KeyPathAcc]),
    {KeysGotten2, KeysRemaining2} = get_full_key_paths(Pos, RestTree, KeysRemaining, KeyPathAcc),
    {CurrentNodeResult ++ KeysGotten ++ KeysGotten2, KeysRemaining2}.

get_all_leafs_full(Tree) ->
    get_all_leafs_full(Tree, []).

get_all_leafs_full([], Acc) ->
    Acc;
get_all_leafs_full([{Pos, Tree} | Rest], Acc) ->
    get_all_leafs_full(Rest, get_all_leafs_full_simple(Pos, [Tree], []) ++ Acc).

get_all_leafs_full_simple(_Pos, [], _KeyPathAcc) ->
    [];
get_all_leafs_full_simple(Pos, [{KeyId, Value, []} | RestTree], KeyPathAcc) ->
    [{Pos, [{KeyId, Value} | KeyPathAcc]} | get_all_leafs_full_simple(Pos, RestTree, KeyPathAcc)];
get_all_leafs_full_simple(Pos, [{KeyId, Value, SubTree} | RestTree], KeyPathAcc) ->
    get_all_leafs_full_simple(Pos + 1, SubTree, [{KeyId, Value} | KeyPathAcc]) ++ get_all_leafs_full_simple(Pos, RestTree, KeyPathAcc).

get_all_leafs(Trees) ->
    get_all_leafs(Trees, []).

get_all_leafs([], Acc) ->
    Acc;
get_all_leafs([{Pos, Tree}|Rest], Acc) ->
    get_all_leafs(Rest, get_all_leafs_simple(Pos, [Tree], []) ++ Acc).

get_all_leafs_simple(_Pos, [], _KeyPathAcc) ->
    [];
get_all_leafs_simple(Pos, [{KeyId, Value, []} | RestTree], KeyPathAcc) ->
    [{Value, {Pos, [KeyId | KeyPathAcc]}} | get_all_leafs_simple(Pos, RestTree, KeyPathAcc)];
get_all_leafs_simple(Pos, [{KeyId, _Value, SubTree} | RestTree], KeyPathAcc) ->
    get_all_leafs_simple(Pos + 1, SubTree, [KeyId | KeyPathAcc]) ++ get_all_leafs_simple(Pos, RestTree, KeyPathAcc).


count_leafs([]) ->
    0;
count_leafs([{_Pos,Tree}|Rest]) ->
    count_leafs_simple([Tree]) + count_leafs(Rest).

count_leafs_simple([]) ->
    0;
count_leafs_simple([{_Key, _Value, []} | RestTree]) ->
    1 + count_leafs_simple(RestTree);
count_leafs_simple([{_Key, _Value, SubTree} | RestTree]) ->
    count_leafs_simple(SubTree) + count_leafs_simple(RestTree).


fold(_Fun, Acc, []) ->
    Acc;
fold(Fun, Acc0, [{Pos, Tree}|Rest]) ->
    Acc1 = fold_simple(Fun, Acc0, Pos, [Tree]),
    fold(Fun, Acc1, Rest).

fold_simple(_Fun, Acc, _Pos, []) ->
    Acc;
fold_simple(Fun, Acc0, Pos, [{Key, Value, SubTree} | RestTree]) ->
    Type = if SubTree == [] -> leaf; true -> branch end,
    Acc1 = Fun({Pos, Key}, Value, Type, Acc0),
    Acc2 = fold_simple(Fun, Acc1, Pos+1, SubTree),
    fold_simple(Fun, Acc2, Pos, RestTree).


map(_Fun, []) ->
    [];
map(Fun, [{Pos, Tree}|Rest]) ->
    case erlang:fun_info(Fun, arity) of
    {arity, 2} ->
        [NewTree] = map_simple(fun(A,B,_C) -> Fun(A,B) end, Pos, [Tree]),
        [{Pos, NewTree} | map(Fun, Rest)];
    {arity, 3} ->
        [NewTree] = map_simple(Fun, Pos, [Tree]),
        [{Pos, NewTree} | map(Fun, Rest)]
    end.

map_simple(_Fun, _Pos, []) ->
    [];
map_simple(Fun, Pos, [{Key, Value, SubTree} | RestTree]) ->
    Value2 = Fun({Pos, Key}, Value,
            if SubTree == [] -> leaf; true -> branch end),
    [{Key, Value2, map_simple(Fun, Pos + 1, SubTree)} | map_simple(Fun, Pos, RestTree)].


mapfold(_Fun, Acc, []) ->
    {[], Acc};
mapfold(Fun, Acc, [{Pos, Tree} | Rest]) ->
    {[NewTree], Acc2} = mapfold_simple(Fun, Acc, Pos, [Tree]),
    {Rest2, Acc3} = mapfold(Fun, Acc2, Rest),
    {[{Pos, NewTree} | Rest2], Acc3}.

mapfold_simple(_Fun, Acc, _Pos, []) ->
    {[], Acc};
mapfold_simple(Fun, Acc, Pos, [{Key, Value, SubTree} | RestTree]) ->
    {Value2, Acc2} = Fun({Pos, Key}, Value,
            if SubTree == [] -> leaf; true -> branch end, Acc),
    {SubTree2, Acc3} = mapfold_simple(Fun, Acc2, Pos + 1, SubTree),
    {RestTree2, Acc4} = mapfold_simple(Fun, Acc3, Pos, RestTree),
    {[{Key, Value2, SubTree2} | RestTree2], Acc4}.


map_leafs(_Fun, []) ->
    [];
map_leafs(Fun, [{Pos, Tree}|Rest]) ->
    [NewTree] = map_leafs_simple(Fun, Pos, [Tree]),
    [{Pos, NewTree} | map_leafs(Fun, Rest)].

map_leafs_simple(_Fun, _Pos, []) ->
    [];
map_leafs_simple(Fun, Pos, [{Key, Value, []} | RestTree]) ->
    Value2 = Fun({Pos, Key}, Value),
    [{Key, Value2, []} | map_leafs_simple(Fun, Pos, RestTree)];
map_leafs_simple(Fun, Pos, [{Key, Value, SubTree} | RestTree]) ->
    [{Key, Value, map_leafs_simple(Fun, Pos + 1, SubTree)} | map_leafs_simple(Fun, Pos, RestTree)].


stem(Trees, Limit) ->
    % flatten each branch in a tree into a tree path, sort by starting rev #
    Paths = lists:sort(lists:map(fun({Pos, Path}) ->
        StemmedPath = lists:sublist(Path, Limit),
        {Pos + 1 - length(StemmedPath), StemmedPath}
    end, get_all_leafs_full(Trees))),

    % convert paths back to trees
    lists:foldl(
        fun({StartPos, Path},TreeAcc) ->
            [SingleTree] = lists:foldl(
                fun({K,V},NewTreeAcc) -> [{K,V,NewTreeAcc}] end, [], Path),
            {NewTrees, _} = merge(TreeAcc, {StartPos, SingleTree}),
            NewTrees
        end, [], Paths).


value_pref(Tuple, _) when is_tuple(Tuple),
        (tuple_size(Tuple) == 3 orelse tuple_size(Tuple) == 4) ->
    Tuple;
value_pref(_, Tuple) when is_tuple(Tuple),
        (tuple_size(Tuple) == 3 orelse tuple_size(Tuple) == 4) ->
    Tuple;
value_pref(?REV_MISSING, Other) ->
    Other;
value_pref(Other, ?REV_MISSING) ->
    Other;
value_pref(Last, _) ->
    Last.


% Tests moved to test/etap/06?-*.t

