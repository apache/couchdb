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

-module(couch_emsort).

% This is an implementation of an external N-way merge sort. It's primary
% purpose is to be used during database compaction as an optimization for
% managing the docid btree.
%
% Trunk currently writes the docid btree as its compacting the database but
% this is quite inneficient as its written out of order in the general case
% as writes are ordered by update_seq.
%
% The general design of this module is a very standard merge sort with one
% caveat due to append only files. This is described in more detail in the
% sorting phase.
%
% The basic algorithm is in two halves. The first half stores KV pairs to disk
% which is then followed by the actual sorting phase that streams KV's back
% to the client using a fold-like function. After some basic definitions we'll
% describe both phases.
%
% Key/Value apairs (aka, KV pairs, or KVs) are simply lists of two-tuples with
% a key as the first element and an arbitrary value as the second. The key of
% this pair is what used to determine the sort order based on native Erlang
% term comparison.
%
% Internally, KVs are stored as lists with a max size defined by
% #ems.chain_chunk. These lists are then chained together on disk using disk
% offsets as a poor man's linked list. The basic format of a list looks like
% {KVs, DiskOffset} where DiskOffset is either the atom nil which means "end
% of the list" or an integer that is a file position offset that is the
% location of another {KVs, DiskOffset} term. The head of each list is
% referred to with a single DiskOffset. The set of terms that extend from
% this initial DiskOffset to the last {KVs, nil} term is referred to in the
% code as a chain. Two important facts are that one call to couch_emsort:add/2
% creates a single chain, and that a chain is always sorted on disk (though its
% possible to be sorted in descending order which will be discussed later).
%
% The second major internal structure is the back bone. This is a list of
% chains that has a quite similar structure to chains but contains different
% data types and has no guarantee on ordering. The back bone is merely the
% list of all head DiskOffsets. The structure has the similar structure of
% {DiskOffsets, DiskOffset} that we use for chains, except that DiskOffsets is
% a list of integers that refer to the heads of chains. The maximum size of
% DiskOffsets is defined by #ems.bb_chunk. It is important to note that the
% backbone has no defined ordering. The other thing of note is that the RAM
% bounds are loosely defined as:
%
%     #ems.bb_chunk * #ems.chain_chunk * avg_size(KV).
%
% Build Phase
% -----------
%
% As mentioned, each call to couch_emsort:add/2 creates a chain from the
% list of KVs that are passed in. This list is first sorted and then the
% chain is created by foldr-ing (note: r) across the list to build the
% chain on disk. It is important to note that the final chain is then
% sorted in ascending order on disk.
%
%
% Sort Phase
% ----------
%
% The sort phase is where the merge sort kicks in. This is generally your
% average merge sort with a caveat for append only storage. First the
% general outline.
%
% The general outline for this sort is that it iteratively merges chains
% in the backbone until less than #ems.bb_chunk chains exist. At this
% point it switches to the last merge sort phase where it just streams
% the sorted KVs back to the client using a fold function.
%
% The general chain merging is a pretty standard merge sort. You load up
% the initial KVs from each phase, pick the next one in sort order and
% then when you run out of KVs you're left with a single DiskOffset for
% the head of a single chain that represents the merge. These new
% DiskOffsets are used to build the new back bone.
%
% The one caveat here is that we're using append only storage. This is
% important because once we make a pass we've effectively reversed the
% sort order of each chain. Ie, the first merge results in chains that
% are ordered in descending order. Since, one pass reverses the list
% the trick is that each phase does two passes. The first phase picks
% the smallest KV to write next and the second phase picks the largest.
% In this manner each time we do a back bone merge we end up with chains
% that are always sorted in an ascending order.
%
% The one downfall is that in the interest of simplicity the sorting is
% restricted to Erlang's native term sorting. A possible extension would
% be to allow two comparison functions to be used, but this module is
% currently only used for docid sorting which is hardcoded to be raw
% Erlang ordering.
%
% Diagram
% -------
%
% If it helps, this is a general diagram of the internal structures. A
% couple points to note since this is ASCII art. The BB pointers across
% the top are lists of chains going down. Each BBN item is one of the
% {DiskOffsets, DiskOffset} structures discussed earlier. Going down,
% the CMN nodes are actually representing #ems.bb_chunk chains in parallel
% going off the back bone. It is important and not represented in this
% diagram that within these groups the chains don't have to be the same
% length. That's just a limitiationg of my ASCII artistic abilities.
%
% The BBN* node is marked with a * to denote that it is the only state
% that we store when writing headeres to disk as it has pointers that
% lead us to all data in the tree.
%
%     BB1 <- BB2 <- BB3 <- BBN*
%      |      |      |      |
%      v      v      v      v
%     CA1    CB1    CC1    CD1
%      |             |      |
%      v             v      v
%     CA2           CC2    CD2
%      |                    |
%      v                    v
%     CA3                  CD3
%

-export([open/1, open/2, get_fd/1, get_state/1]).
-export([add/2, merge/1, sort/1, iter/1, next/1]).


-record(ems, {
    fd,
    root,
    bb_chunk = 10,
    chain_chunk = 100
}).


open(Fd) ->
    {ok, #ems{fd=Fd}}.


open(Fd, Options) ->
    {ok, set_options(#ems{fd=Fd}, Options)}.


set_options(Ems, []) ->
    Ems;
set_options(Ems, [{root, Root} | Rest]) ->
    set_options(Ems#ems{root=Root}, Rest);
set_options(Ems, [{chain_chunk, Count} | Rest]) when is_integer(Count) ->
    set_options(Ems#ems{chain_chunk=Count}, Rest);
set_options(Ems, [{back_bone_chunk, Count} | Rest]) when is_integer(Count) ->
    set_options(Ems#ems{bb_chunk=Count}, Rest).


get_fd(#ems{fd=Fd}) ->
    Fd.


get_state(#ems{root=Root}) ->
    Root.


add(Ems, []) ->
    {ok, Ems};
add(Ems, KVs) ->
    Pos = write_kvs(Ems, KVs),
    {ok, add_bb_pos(Ems, Pos)}.


sort(#ems{}=Ems) ->
    {ok, Ems1} = merge(Ems),
    iter(Ems1).


merge(#ems{root=undefined}=Ems) ->
    {ok, Ems};
merge(#ems{}=Ems) ->
    {ok, decimate(Ems)}.


iter(#ems{root=undefined}=Ems) ->
    {ok, {Ems, []}};
iter(#ems{root={BB, nil}}=Ems) ->
    Chains = init_chains(Ems, small, BB),
    {ok, {Ems, Chains}};
iter(#ems{root={_, _}}) ->
    {error, not_merged}.


next({_Ems, []}) ->
    finished;
next({Ems, Chains}) ->
    {KV, RestChains} = choose_kv(small, Ems, Chains),
    {ok, KV, {Ems, RestChains}}.


add_bb_pos(#ems{root=undefined}=Ems, Pos) ->
    Ems#ems{root={[Pos], nil}};
add_bb_pos(#ems{root={BB, Prev}}=Ems, Pos) ->
    {NewBB, NewPrev} = append_item(Ems, {BB, Prev}, Pos, Ems#ems.bb_chunk),
    Ems#ems{root={NewBB, NewPrev}}.


write_kvs(Ems, KVs) ->
    % Write the list of KV's to disk in sorted order in chunks
    % of 100. Also make sure that the order is so that they
    % can be streamed in asscending order.
    {LastKVs, LastPos} =
    lists:foldr(fun(KV, Acc) ->
        append_item(Ems, Acc, KV, Ems#ems.chain_chunk)
    end, {[], nil}, lists:sort(KVs)),
    {ok, Final, _} = couch_file:append_term(Ems#ems.fd, {LastKVs, LastPos},
        [{compression, none}]),
    Final.


decimate(#ems{root={_BB, nil}}=Ems) ->
    % We have less than bb_chunk backbone pointers so we're
    % good to start streaming KV's back to the client.
    Ems;
decimate(#ems{root={BB, NextBB}}=Ems) ->
    % To make sure we have a bounded amount of data in RAM
    % at any given point we first need to decimate the data
    % by performing the first couple iterations of a merge
    % sort writing the intermediate results back to disk.

    % The first pass gives us a sort with pointers linked from
    % largest to smallest.
    {RevBB, RevNextBB} = merge_back_bone(Ems, small, BB, NextBB),

    % We have to run a second pass so that links are pointed
    % back from smallest to largest.
    {FwdBB, FwdNextBB} = merge_back_bone(Ems, big, RevBB, RevNextBB),

    % Continue deicmating until we have an acceptable bound on
    % the number of keys to use.
    decimate(Ems#ems{root={FwdBB, FwdNextBB}}).


merge_back_bone(Ems, Choose, BB, NextBB) ->
    BBPos = merge_chains(Ems, Choose, BB),
    merge_rest_back_bone(Ems, Choose, NextBB, {[BBPos], nil}).


merge_rest_back_bone(_Ems, _Choose, nil, Acc) ->
    Acc;
merge_rest_back_bone(Ems, Choose, BBPos, Acc) ->
    {ok, {BB, NextBB}} = couch_file:pread_term(Ems#ems.fd, BBPos),
    NewPos = merge_chains(Ems, Choose, BB),
    {NewBB, NewPrev} = append_item(Ems, Acc, NewPos, Ems#ems.bb_chunk),
    merge_rest_back_bone(Ems, Choose, NextBB, {NewBB, NewPrev}).


merge_chains(Ems, Choose, BB) ->
    Chains = init_chains(Ems, Choose, BB),
    merge_chains(Ems, Choose, Chains, {[], nil}).


merge_chains(Ems, _Choose, [], ChainAcc) ->
    {ok, CPos, _} = couch_file:append_term(Ems#ems.fd, ChainAcc,
        [{compression, none}]),
    CPos;
merge_chains(#ems{chain_chunk=CC}=Ems, Choose, Chains, Acc) ->
    {KV, RestChains} = choose_kv(Choose, Ems, Chains),
    {NewKVs, NewPrev} = append_item(Ems, Acc, KV, CC),
    merge_chains(Ems, Choose, RestChains, {NewKVs, NewPrev}).


init_chains(Ems, Choose, BB) ->
    Chains = lists:map(fun(CPos) ->
        {ok, {KVs, NextKVs}} = couch_file:pread_term(Ems#ems.fd, CPos),
        {KVs, NextKVs}
    end, BB),
    order_chains(Choose, Chains).


order_chains(small, Chains) -> lists:sort(Chains);
order_chains(big, Chains) -> lists:reverse(lists:sort(Chains)).


choose_kv(_Choose, _Ems, [{[KV], nil} | Rest]) ->
    {KV, Rest};
choose_kv(Choose, Ems, [{[KV], Pos} | RestChains]) ->
    {ok, Chain} = couch_file:pread_term(Ems#ems.fd, Pos),
    case Choose of
        small -> {KV, ins_small_chain(RestChains, Chain, [])};
        big -> {KV, ins_big_chain(RestChains, Chain, [])}
    end;
choose_kv(Choose, _Ems, [{[KV | RestKVs], Prev} | RestChains]) ->
    case Choose of
        small -> {KV, ins_small_chain(RestChains, {RestKVs, Prev}, [])};
        big -> {KV, ins_big_chain(RestChains, {RestKVs, Prev}, [])}
    end.


ins_small_chain([{[{K1,_}|_],_}=C1|Rest], {[{K2,_}|_],_}=C2, Acc) when K1<K2 ->
    ins_small_chain(Rest, C2, [C1 | Acc]);
ins_small_chain(Rest, Chain, Acc) ->
    lists:reverse(Acc, [Chain | Rest]).


ins_big_chain([{[{K1,_}|_],_}=C1|Rest], {[{K2,_}|_],_}=C2, Acc) when K1>K2 ->
    ins_big_chain(Rest, C2, [C1 | Acc]);
ins_big_chain(Rest, Chain, Acc) ->
    lists:reverse(Acc, [Chain | Rest]).


append_item(Ems, {List, Prev}, Pos, Size) when length(List) >= Size ->
    {ok, PrevList, _} = couch_file:append_term(Ems#ems.fd, {List, Prev},
        [{compression, none}]),
    {[Pos], PrevList};
append_item(_Ems, {List, Prev}, Pos, _Size) ->
    {[Pos | List], Prev}.

