%   Copyright 2007, 2008 Damien Katz <damien_katz@yahoo.com>
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

-module(couch_key_tree).

-export([merge/2, find_missing/2, get_key_leafs/2, get_full_key_paths/2, get/2]).
-export([map/2, get_all_leafs/1, get_leaf_keys/1, count_leafs/1]).

% a key tree looks like this:
% Tree -> [] or [{Key, Value, ChildTree} | SiblingTree]
% ChildTree -> Tree
% SiblingTree -> [] or [{SiblingKey, Value, Tree} | Tree]
% And each Key < SiblingKey



% key tree functions

% When the same key is found in the trees, the value in tree B is discarded.
merge([], B) ->
    B;
merge(A, []) ->
    A;
merge([ATree | ANextTree], [BTree | BNextTree]) ->
    {AKey, AValue, ASubTree} = ATree,
    {BKey, _BValue, BSubTree} = BTree,
    if
    AKey == BKey ->
        %same key
        MergedSubTree = merge(ASubTree, BSubTree),
        MergedNextTree = merge(ANextTree, BNextTree),
        [{AKey, AValue, MergedSubTree} | MergedNextTree];
    AKey < BKey ->
        [ATree | merge(ANextTree, [BTree | BNextTree])];
    true ->
        [BTree | merge([ATree | ANextTree], BNextTree)]
    end.

find_missing(_Tree, []) ->
    [];
find_missing([], Keys) ->
    Keys;
find_missing([{Key, _, SubTree} | RestTree], Keys) ->
    SrcKeys2 = Keys -- [Key],
    SrcKeys3 = find_missing(SubTree, SrcKeys2),
    find_missing(RestTree, SrcKeys3).
    

% get the leafs in the tree matching the keys. The matching key nodes can be
% leafs or an inner nodes. If an inner node, then the leafs for that node
% are returned.
get_key_leafs(Tree, Keys) ->
    get_key_leafs(Tree, Keys, []).
    
get_key_leafs(_Tree, [], _KeyPathAcc) ->
    {[], []};
get_key_leafs([], KeysToGet, _KeyPathAcc) ->
    {[], KeysToGet};
get_key_leafs([{Key, _Value, SubTree}=Tree | RestTree], KeysToGet, KeyPathAcc) ->
    case KeysToGet -- [Key] of
    KeysToGet -> % same list, key not found    
        {LeafsFound, KeysToGet2} = get_key_leafs(SubTree, KeysToGet, [Key | KeyPathAcc]),
        {RestLeafsFound, KeysRemaining} = get_key_leafs(RestTree, KeysToGet2, KeyPathAcc),
        {LeafsFound ++ RestLeafsFound, KeysRemaining};
    KeysToGet2 ->
        LeafsFound = get_all_leafs([Tree], KeyPathAcc),
        LeafKeysFound = [LeafKeyFound || {LeafKeyFound, _, _} <- LeafsFound],
        KeysToGet2 = KeysToGet2 -- LeafKeysFound,
        {RestLeafsFound, KeysRemaining} = get_key_leafs(RestTree, KeysToGet2, KeyPathAcc),
        {LeafsFound ++ RestLeafsFound, KeysRemaining}
    end.

get(Tree, KeysToGet) ->
    {KeyPaths, KeysNotFound} = get_full_key_paths(Tree, KeysToGet),
    FixedResults = [ {Key, Value, [Key0 || {Key0, _} <- Path]} || [{Key, Value}|_] = Path <- KeyPaths],
    {FixedResults, KeysNotFound}.

get_full_key_paths(Tree, Keys) ->
    get_full_key_paths(Tree, Keys, []).
    
get_full_key_paths(_Tree, [], _KeyPathAcc) ->
    {[], []};
get_full_key_paths([], KeysToGet, _KeyPathAcc) ->
    {[], KeysToGet};
get_full_key_paths([{KeyId, Value, SubTree} | RestTree], KeysToGet, KeyPathAcc) ->
    KeysToGet2 = KeysToGet -- [KeyId],
    CurrentNodeResult =
    case length(KeysToGet2) == length(KeysToGet) of
    true -> % not in the key list.
        [];
    false -> % this node is the key list. return it
        [[{KeyId, Value} | KeyPathAcc]]
    end,
    {KeysGotten, KeysRemaining} = get_full_key_paths(SubTree, KeysToGet2, [{KeyId, Value} | KeyPathAcc]),
    {KeysGotten2, KeysRemaining2} = get_full_key_paths(RestTree, KeysRemaining, KeyPathAcc),
    {CurrentNodeResult ++ KeysGotten ++ KeysGotten2, KeysRemaining2}.

get_all_leafs(Tree) ->
    get_all_leafs(Tree, []).
    
get_all_leafs([], _KeyPathAcc) ->
    [];
get_all_leafs([{KeyId, Value, []} | RestTree], KeyPathAcc) ->
    [{KeyId, Value, [KeyId | KeyPathAcc]} | get_all_leafs(RestTree, KeyPathAcc)];
get_all_leafs([{KeyId, _Value, SubTree} | RestTree], KeyPathAcc) ->
    get_all_leafs(SubTree, [KeyId | KeyPathAcc]) ++ get_all_leafs(RestTree, KeyPathAcc).

get_leaf_keys([]) ->
    [];
get_leaf_keys([{Key, _Value, []} | RestTree]) ->
    [Key | get_leaf_keys(RestTree)];
get_leaf_keys([{_Key, _Value, SubTree} | RestTree]) ->
    get_leaf_keys(SubTree) ++ get_leaf_keys(RestTree).
    
count_leafs([]) ->
    0;
count_leafs([{_Key, _Value, []} | RestTree]) ->
    1 + count_leafs(RestTree);
count_leafs([{_Key, _Value, SubTree} | RestTree]) ->
    count_leafs(SubTree) + count_leafs(RestTree).
    

map(_Fun, []) ->
    [];
map(Fun, [{Key, Value, SubTree} | RestTree]) ->
    Value2 = Fun(Key, Value),
    [{Key, Value2, map(Fun, SubTree)} | map(Fun, RestTree)].

