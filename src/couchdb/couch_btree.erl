% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_btree).

-export([open/2, open/3, query_modify/4, add/2, add_remove/3, foldl/3, foldl/4]).
-export([foldr/3, foldr/4, fold/4, fold/5, row_count/1]).
-export([lookup/2, get_state/1, test/1, test/0]).

-define(CHUNK_THRESHOLD, 16#fff).

-record(btree,
    {fd,
    root,
    extract_kv = fun({Key, Value}) -> {Key, Value} end,
    assemble_kv =  fun(Key, Value) -> {Key, Value} end,
    less = fun(A, B) -> A < B end
    }).

extract(#btree{extract_kv=Extract}, Value) ->
    Extract(Value).

assemble(#btree{assemble_kv=Assemble}, Key, Value) ->
    Assemble(Key, Value).

less(#btree{less=Less}, A, B) ->
    Less(A, B).

% pass in 'nil' for State if a new Btree.
open(State, Fd) ->
    {ok, #btree{root=State, fd=Fd}}.
    
set_options(Bt, []) ->
    Bt;
set_options(Bt, [{split, Extract}|Rest]) ->
    set_options(Bt#btree{extract_kv=Extract}, Rest);
set_options(Bt, [{join, Assemble}|Rest]) ->
    set_options(Bt#btree{assemble_kv=Assemble}, Rest);
set_options(Bt, [{less, Less}|Rest]) ->
    set_options(Bt#btree{less=Less}, Rest).

open(State, Fd, Options) ->
    {ok, set_options(#btree{root=State, fd=Fd}, Options)}.

get_state(#btree{root=Root}) ->
    Root.

row_count(#btree{root=nil}) ->
    0;
row_count(#btree{root={_RootPointer, Count}}) ->
    Count.

foldl(Bt, Fun, Acc) ->
    fold(Bt, fwd, Fun, Acc).

foldl(Bt, Key, Fun, Acc) ->
    fold(Bt, Key, fwd, Fun, Acc).

foldr(Bt, Fun, Acc) ->
    fold(Bt, rev, Fun, Acc).

foldr(Bt, Key, Fun, Acc) ->
    fold(Bt, Key, rev, Fun, Acc).

% wraps a 2 arity function with the proper 3 arity function
convert_fun_arity(Fun) when is_function(Fun, 2) ->
    fun(KV, _Offset, AccIn) -> Fun(KV, AccIn) end;
convert_fun_arity(Fun) when is_function(Fun, 3) ->
    Fun.    % Already arity 3

fold(Bt, Dir, Fun, Acc) ->
    {_ContinueFlag, Acc2} = stream_node(Bt, 0, Bt#btree.root, nil, Dir, convert_fun_arity(Fun), Acc),
    {ok, Acc2}.

fold(Bt, Key, Dir, Fun, Acc) ->
    {_ContinueFlag, Acc2} = stream_node(Bt, 0, Bt#btree.root, Key, Dir, convert_fun_arity(Fun), Acc),
    {ok, Acc2}.

add(Bt, InsertKeyValues) ->
    add_remove(Bt, InsertKeyValues, []).

add_remove(Bt, InsertKeyValues, RemoveKeys) ->
    {ok, [], Bt2} = query_modify(Bt, [], InsertKeyValues, RemoveKeys),
    {ok, Bt2}.

query_modify(Bt, LookupKeys, InsertValues, RemoveKeys) ->
    #btree{root=Root} = Bt,
    InsertActions = lists:map(
        fun(KeyValue) ->
            {Key, Value} = extract(Bt, KeyValue),
            {insert, Key, Value}
        end, InsertValues),
    RemoveActions = [{remove, Key, nil} || Key <- RemoveKeys],
    FetchActions = [{fetch, Key, nil} || Key <- LookupKeys],
    SortFun =
        fun({OpA, A, _}, {OpB, B, _}) ->
            case less(Bt, A, B) of
            true -> true;
            false ->
                case less(Bt, B, A) of
                true -> false;
                false ->
                    % A and B are equal, sort by op.
                    op_order(OpA) < op_order(OpB)
                end
            end
        end,
    Actions = lists:sort(SortFun, lists:append([InsertActions, RemoveActions, FetchActions])),
    {ok, KeyPointers, QueryResults, Bt2} = modify_node(Bt, Root, Actions, []),
    {ok, NewRoot, Bt3} = complete_root(Bt2, KeyPointers),
    {ok, QueryResults, Bt3#btree{root=NewRoot}}.

% for ordering different operatations with the same key.
% fetch < remove < insert
op_order(fetch) -> 1;
op_order(remove) -> 2;
op_order(insert) -> 3.

lookup(#btree{root=Root, less=Less}=Bt, Keys) ->
    SortedKeys = lists:sort(Less, Keys),
    {ok, SortedResults} = lookup(Bt, Root, SortedKeys),
    % We want to return the results in the same order as the keys were input
    % but we may have changed the order when we sorted. So we need to put the
    % order back into the results.
    KeyDict = dict:from_list(SortedResults),
    [dict:fetch(Key, KeyDict) || Key <- Keys].

lookup(_Bt, nil, Keys) ->
    {ok, [{Key, not_found} || Key <- Keys]};
lookup(Bt, {Pointer, _Count}, Keys) ->
    {NodeType, NodeList} = get_node(Bt, Pointer),
    case NodeType of
    kp_node ->
        lookup_kpnode(Bt, NodeList, Keys, []);
    kv_node ->
        lookup_kvnode(Bt, NodeList, Keys, [])
    end.


lookup_kpnode(_Bt, [], Keys, Output) ->
    {ok, lists:reverse(Output, [{Key, not_found} || Key <- Keys])};

lookup_kpnode(_Bt, _KPs, [], Output) ->
    {ok, lists:reverse(Output)};

lookup_kpnode(Bt, [{Key, PointerInfo} | RestKPs], LookupKeys, Output) ->
    % Split the Keys into two lists, queries of values less
    % than equals, and greater than the current key
    SplitFun = fun(LookupKey) -> not less(Bt, Key, LookupKey) end,
    case lists:splitwith(SplitFun, LookupKeys) of
    {[], GreaterQueries} ->
        lookup_kpnode(Bt, RestKPs, GreaterQueries, Output);
    {LessEqQueries, GreaterQueries} ->
        {ok, Results} = lookup(Bt, PointerInfo, LessEqQueries),
        lookup_kpnode(Bt, RestKPs, GreaterQueries, lists:reverse(Results, Output))
    end.



lookup_kvnode(_Bt, _KVs, [], Output) ->
    {ok, lists:reverse(Output)};
lookup_kvnode(_Bt, [], Keys, Output) ->
    % keys not found
    {ok, lists:reverse(Output, [{Key, not_found} || Key <- Keys])};
lookup_kvnode(Bt, [{Key, Value} | RestKVs], [LookupKey | RestLookupKeys], Output) ->
    case less(Bt, LookupKey, Key) of
    true ->
        lookup_kvnode(Bt, [{Key, Value} | RestKVs], RestLookupKeys, [{LookupKey, not_found} | Output]);
    false ->
        case less(Bt, Key, LookupKey) of
        true ->
            % LookupKey is greater than Key
            lookup_kvnode(Bt, RestKVs, [LookupKey | RestLookupKeys], Output);
        false ->
            % LookupKey is equal to Key
            lookup_kvnode(Bt, RestKVs, RestLookupKeys, [{LookupKey, {ok, assemble(Bt, LookupKey, Value)}} | Output])
        end
    end.


complete_root(Bt, []) ->
    {ok, nil, Bt};
complete_root(Bt, [{_Key, PointerInfo}])->
    {ok, PointerInfo, Bt};
complete_root(Bt, KPs) ->
    {ok, ResultKeyPointers, Bt2} = write_node(Bt, kp_node, KPs),
    complete_root(Bt2, ResultKeyPointers).

%%%%%%%%%%%%% The chunkify function sucks! %%%%%%%%%%%%% 
% It is inaccurate as it does not account for compression when blocks are
% written. Plus with the "case size(term_to_binary(InList)) of" code it's
% probably really inefficient.

chunkify(_Bt, []) ->
    [];
chunkify(Bt, InList) ->
    case size(term_to_binary(InList)) of
    Size when Size > ?CHUNK_THRESHOLD ->
        NumberOfChunksLikely = ((Size div ?CHUNK_THRESHOLD) + 1),
        ChunkThreshold = Size div NumberOfChunksLikely,
        chunkify(Bt, InList, ChunkThreshold, [], 0, []);
    _Else ->
        [InList]
    end.

chunkify(_Bt, [], _ChunkThreshold, [], 0, OutputChunks) ->
    lists:reverse(OutputChunks);
chunkify(_Bt, [], _ChunkThreshold, OutList, _OutListSize, OutputChunks) ->
    lists:reverse([lists:reverse(OutList) | OutputChunks]);
chunkify(Bt, [InElement | RestInList], ChunkThreshold, OutList, OutListSize, OutputChunks) ->
    case size(term_to_binary(InElement)) of
    Size when (Size + OutListSize) > ChunkThreshold ->
        chunkify(Bt, RestInList, ChunkThreshold, [], 0, [lists:reverse([InElement | OutList]) | OutputChunks]);
    Size ->
        chunkify(Bt, RestInList, ChunkThreshold, [InElement | OutList], OutListSize + Size, OutputChunks)
    end.

modify_node(Bt, RootPointerInfo, Actions, QueryOutput) ->
    case RootPointerInfo of
    nil ->
        NodeType = kv_node,
        NodeList = [];
    {Pointer, _count} ->
        {NodeType, NodeList} = get_node(Bt, Pointer)
    end,
    case NodeType of
    kp_node ->
        {ok, NewNodeList, QueryOutput2, Bt2} = modify_kpnode(Bt, NodeList, Actions, [], QueryOutput);
    kv_node ->
        {ok, NewNodeList, QueryOutput2, Bt2} = modify_kvnode(Bt, NodeList, Actions, [], QueryOutput)
    end,
    case NewNodeList of
    [] ->  % no nodes remain
        {ok, [], QueryOutput2, Bt2};
    NodeList ->  % nothing changed
        {LastKey, _LastValue} = lists:last(NodeList),
        {ok, [{LastKey, RootPointerInfo}], QueryOutput2, Bt2};
    _Else2 ->
        {ok, ResultList, Bt3} = write_node(Bt2, NodeType, NewNodeList),
        {ok, ResultList, QueryOutput2, Bt3}
    end.


count(kv_node, NodeList) ->
    length(NodeList);
count(kp_node, NodeList) ->
    lists:foldl( fun({_Key, {_Pointer, Count}}, AccCount) ->
            Count + AccCount
        end,
        0, NodeList).


get_node(#btree{fd = Fd}, NodePos) ->
    {ok, {NodeType, NodeList}} = couch_file:pread_term(Fd, NodePos),
    case NodeType of
    kp_node ->
        % Node pointers always point backward on disk.
        % Validating this prevents infinite loops should
        % a disk corruption occur.
        [throw({error, disk_corruption})
            || {_Key, {SubNodePos, _Count}}
            <- NodeList, SubNodePos >= NodePos];
    kv_node ->
        ok
    end,
    {NodeType, NodeList}.

write_node(Bt, NodeType, NodeList) ->
    % split up nodes into smaller sizes
    NodeListList = chunkify(Bt, NodeList),
    % now write out each chunk and return the KeyPointer pairs for those nodes
    ResultList = [
        begin
            {ok, Pointer} = couch_file:append_term(Bt#btree.fd, {NodeType, ANodeList}),
            {LastKey, _} = lists:last(ANodeList),
            {LastKey, {Pointer, count(NodeType, ANodeList)}}
        end
    ||
        ANodeList <- NodeListList
    ],
    {ok, ResultList, Bt}.


modify_kpnode(Bt, KPs, [], ResultNode, QueryOutput) ->
    % processed all queries for the current tree
    {ok, lists:reverse(ResultNode, KPs), QueryOutput, Bt};

modify_kpnode(Bt, [], Actions, [], QueryOutput) ->
    modify_node(Bt, nil, Actions, QueryOutput);

modify_kpnode(Bt, [], Actions, [{_Key, PointerInfo} | ResultNode], QueryOutput) ->
    {ok, ChildKPs, QueryOutput2, Bt2} = modify_node(Bt, PointerInfo, Actions, QueryOutput),
    {ok, lists:reverse(ResultNode, ChildKPs), QueryOutput2, Bt2};

modify_kpnode(Bt, [{Key,PointerInfo} | RestKPs], Actions, ResultNode, QueryOutput) ->
    % Split the actions into two lists, queries of values <= and > than the current key
    SplitFun = fun({_ActionType, ActionKey, _ActionValue}) ->
            not less(Bt, Key, ActionKey)
        end,
    case lists:splitwith(SplitFun, Actions) of
    {[], GreaterQueries} ->
        modify_kpnode(Bt, RestKPs, GreaterQueries, [{Key, PointerInfo} | ResultNode], QueryOutput);
    {LessEqQueries, GreaterQueries} ->
        {ok, ChildKPs, QueryOutput2, Bt2} = modify_node(Bt, PointerInfo, LessEqQueries, QueryOutput),
        modify_kpnode(Bt2, RestKPs, GreaterQueries, lists:reverse(ChildKPs, ResultNode), QueryOutput2)
    end.

modify_kvnode(Bt, KVs, [], ResultNode, QueryOutput) ->
    {ok, lists:reverse(ResultNode, KVs), QueryOutput, Bt};
modify_kvnode(Bt, [], [{ActionType, ActionKey, ActionValue} | RestActions], ResultNode, QueryOutput) ->
    case ActionType of
    insert ->
        modify_kvnode(Bt, [], RestActions, [{ActionKey, ActionValue} | ResultNode], QueryOutput);
    remove ->
        % just drop the action
        modify_kvnode(Bt, [], RestActions, ResultNode, QueryOutput);
    fetch ->
        % the key/value must not exist in the tree
        modify_kvnode(Bt, [], RestActions, ResultNode, [{not_found, {ActionKey, nil}} | QueryOutput])
    end;
modify_kvnode(Bt, [{Key, Value} | RestKVs], [{ActionType, ActionKey, ActionValue} | RestActions], ResultNode, QueryOutput) ->
    case less(Bt, ActionKey, Key) of
    true ->
        case ActionType of
        insert ->
            % ActionKey is less than the Key, so insert
            modify_kvnode(Bt, [{Key, Value} | RestKVs], RestActions, [{ActionKey, ActionValue} | ResultNode], QueryOutput);
        remove ->
            % ActionKey is less than the Key, just drop the action
            modify_kvnode(Bt, [{Key, Value} | RestKVs], RestActions, ResultNode, QueryOutput);
        fetch ->
            % ActionKey is less than the Key, the key/value must not exist in the tree
            modify_kvnode(Bt, [{Key, Value} | RestKVs], RestActions, ResultNode, [{not_found, {ActionKey, nil}} | QueryOutput])
        end;
    false ->
        case less(Bt, Key, ActionKey) of
        true ->
            % ActionKey is greater than Key
            modify_kvnode(Bt, RestKVs, [{ActionType, ActionKey, ActionValue} | RestActions], [{Key, Value} | ResultNode], QueryOutput);
        false ->
            % InsertKey is equal to Key
            case ActionType of
            insert ->
                % ActionKey is less than the Key, so insert
                modify_kvnode(Bt, RestKVs, RestActions, [{ActionKey, ActionValue} | ResultNode], QueryOutput);
            remove ->
                modify_kvnode(Bt, RestKVs, RestActions, ResultNode, QueryOutput);
            fetch ->
                % ActionKey is equal to the Key, insert into the QueryOuput, but re-process the node
                % since an identical action key can follow it.
                modify_kvnode(Bt, [{Key, Value} | RestKVs], RestActions, ResultNode, [{ok, assemble(Bt, Key, Value)} | QueryOutput])
            end
        end
    end.

adjust_dir(fwd, List) ->
    List;
adjust_dir(rev, List) ->
    lists:reverse(List).

stream_node(Bt, Offset, PointerInfo, nil, Dir, Fun, Acc) ->
    stream_node(Bt, Offset, PointerInfo, Dir, Fun, Acc);
stream_node(_Bt, _Offset, nil, _StartKey, _Dir, _Fun, Acc) ->
    {ok, Acc};
stream_node(Bt, Offset, {Pointer, _Count}, StartKey, Dir, Fun, Acc) ->
    {NodeType, NodeList} = get_node(Bt, Pointer),
    case NodeType of
    kp_node ->
        stream_kp_node(Bt, Offset, adjust_dir(Dir, NodeList), StartKey, Dir, Fun, Acc);
    kv_node ->
        stream_kv_node(Bt, Offset, adjust_dir(Dir, NodeList), StartKey, Dir, Fun, Acc)
    end.

stream_node(_Bt, _Offset, nil, _Dir, _Fun, Acc) ->
    {ok, Acc};
stream_node(Bt, Offset, {Pointer, _Count}, Dir, Fun, Acc) ->
    {NodeType, NodeList} = get_node(Bt, Pointer),
    case NodeType of
    kp_node ->
        stream_kp_node(Bt, Offset, adjust_dir(Dir, NodeList), Dir, Fun, Acc);
    kv_node ->
        stream_kv_node(Bt, Offset, adjust_dir(Dir, NodeList), Dir, Fun, Acc)
    end.

stream_kp_node(_Bt, _Offset, [], _Dir, _Fun, Acc) ->
    {ok, Acc};
stream_kp_node(Bt, Offset, [{_Key, {Pointer, Count}} | Rest], Dir, Fun, Acc) ->
    case stream_node(Bt, Offset, {Pointer, Count}, Dir, Fun, Acc) of
    {ok, Acc2} ->
        stream_kp_node(Bt, Offset + Count, Rest, Dir, Fun, Acc2);
    {stop, Acc2} ->
        {stop, Acc2}
    end.

drop_nodes(_Bt, Offset, _StartKey, []) ->
    {Offset, []};
drop_nodes(Bt, Offset, StartKey, [{NodeKey, {Pointer, Count}} | RestKPs]) ->
    case less(Bt, NodeKey, StartKey) of
    true -> drop_nodes(Bt, Offset + Count, StartKey, RestKPs);
    false -> {Offset, [{NodeKey, {Pointer, Count}} | RestKPs]}
    end.

stream_kp_node(Bt, Offset, KPs, StartKey, Dir, Fun, Acc) ->
    {NewOffset, NodesToStream} =
    case Dir of
    fwd ->
        % drop all nodes sorting before the key
        drop_nodes(Bt, Offset, StartKey, KPs);
    rev ->
        % keep all nodes sorting before the key, AND the first node to sort after
        RevKPs = lists:reverse(KPs),
        case lists:splitwith(fun({Key, _Pointer}) -> less(Bt, Key, StartKey) end, RevKPs) of
        {_RevBefore, []} ->
            % everything sorts before it
            {Offset, KPs};
        {RevBefore, [FirstAfter | Drop]} ->
            {Offset + count(kp_node, Drop), [FirstAfter | lists:reverse(RevBefore)]}
        end
    end,
    case NodesToStream of
    [] ->
        {ok, Acc};
    [{_Key, PointerInfo} | Rest] ->
        case stream_node(Bt, NewOffset, PointerInfo, StartKey, Dir, Fun, Acc) of
        {ok, Acc2} ->
            stream_kp_node(Bt, NewOffset, Rest, Dir, Fun, Acc2);
        {stop, Acc2} ->
            {stop, Acc2}
        end
    end.

stream_kv_node(_Bt, _Offset, [], _Dir, _Fun, Acc) ->
    {ok, Acc};
stream_kv_node(Bt, Offset, [{K, V} | RestKVs], Dir, Fun, Acc) ->
    case Fun(assemble(Bt, K, V), Offset, Acc) of
    {ok, Acc2} ->
        stream_kv_node(Bt, Offset + 1, RestKVs, Dir, Fun, Acc2);
    {stop, Acc2} ->
        {stop, Acc2}
    end.

stream_kv_node(Bt, Offset, KVs, StartKey, Dir, Fun, Acc) ->
    DropFun =
    case Dir of
    fwd ->
        fun({Key, _}) -> less(Bt, Key, StartKey) end;
    rev ->
        fun({Key, _}) -> less(Bt, StartKey, Key) end
    end,
    % drop all nodes preceding the key
    GTEKVs = lists:dropwhile(DropFun, KVs),
    LenSkipped = length(KVs) - length(GTEKVs),
    stream_kv_node(Bt, Offset + LenSkipped, GTEKVs, Dir, Fun, Acc).




test()->
    test(1000).

test(N) ->
    KeyValues = [{random:uniform(), random:uniform()} || _Seq <- lists:seq(1, N)],
    test_btree(KeyValues),              % randomly distributed
    Sorted = lists:sort(KeyValues),
    test_btree(Sorted),                 % sorted regular
    test_btree(lists:reverse(Sorted)).  % sorted reverse


test_btree(KeyValues) ->
    {ok, Fd} = couch_file:open("foo", [create,overwrite]),
    {ok, Btree} = open(nil, Fd),

    % first dump in all the values in one go
    {ok, Btree10} = add_remove(Btree, KeyValues, []),

    ok = test_keys(Btree10, KeyValues),

    % remove everything
    {ok, Btree20} = test_remove(Btree10, KeyValues),

    % make sure its empty
    {ok, false} = foldl(Btree20, fun(_X, _Acc) ->
            {ok, true} % change Acc to 'true'
        end,
        false),

    % add everything back one at a time.
    {ok, Btree30} = test_add(Btree20, KeyValues),

    ok = test_keys(Btree30, KeyValues),

    KeyValuesRev = lists:reverse(KeyValues),

    % remove everything, in reverse order
    {ok, Btree40} = test_remove(Btree30, KeyValuesRev),

    % make sure its empty
    {ok, false} = foldl(Btree40, fun(_X, _Acc) ->
            {ok, true} % change Acc to 'true'
        end,
        false),


    {A, B} = every_other(KeyValues),

    % add everything back
    {ok, Btree50} = test_add(Btree40,KeyValues),

    ok = test_keys(Btree50, KeyValues),

    % remove half the values
    {ok, Btree60} = test_remove(Btree50, A),

    % verify the remaining
    ok = test_keys(Btree60, B),

    % add A back
    {ok, Btree70} = test_add(Btree60, A),

    % verify
    ok = test_keys(Btree70, KeyValues),

    % remove B
    {ok, Btree80} = test_remove(Btree70, B),

    % verify the remaining
    ok = test_keys(Btree80, A),

    ok = couch_file:close(Fd).




every_other(List) ->
    every_other(List, [], [], 1).

every_other([], AccA, AccB, _Flag) ->
    {lists:reverse(AccA), lists:reverse(AccB)};
every_other([H|T], AccA, AccB, 1) ->
    every_other(T, [H|AccA], AccB, 0);
every_other([H|T], AccA, AccB, 0) ->
    every_other(T, AccA, [H|AccB], 1).

test_keys(Btree, List) ->
    FoldFun =
    fun(Element, [HAcc|TAcc]) ->
            Element = HAcc, % must match
            {ok, TAcc}
        end,
    Sorted = lists:sort(List),
    {ok, []} = foldl(Btree, FoldFun, Sorted),
    {ok, []} = foldr(Btree, FoldFun, lists:reverse(Sorted)),

    test_lookup(Btree, List).

% Makes sure each key value pair is found in the btree
test_lookup(_Btree, []) ->
    ok;
test_lookup(Btree, [{Key, Value} | Rest]) ->
    [{ok,{Key, Value}}] = lookup(Btree, [Key]),
    {ok, []} = foldl(Btree, Key, fun({KeyIn, ValueIn}, []) ->
            KeyIn = Key,
            ValueIn = Value,
            {stop, []}
        end,
        []),
    {ok, []} = foldr(Btree, Key, fun({KeyIn, ValueIn}, []) ->
            KeyIn = Key,
            ValueIn = Value,
            {stop, []}
        end,
        []),
    test_lookup(Btree, Rest).

% removes each key one at a time from the btree
test_remove(Btree, []) ->
    {ok, Btree};
test_remove(Btree, [{Key, _Value} | Rest]) ->
    {ok, Btree2} = add_remove(Btree,[], [Key]),
    test_remove(Btree2, Rest).

% adds each key one at a time from the btree
test_add(Btree, []) ->
    {ok, Btree};
test_add(Btree, [KeyValue | Rest]) ->
    {ok, Btree2} = add_remove(Btree, [KeyValue], []),
    test_add(Btree2, Rest).
