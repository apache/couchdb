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

-module(couch_ehamt).

% This file is an implementation of an external hash array
% mapped trie (HAMT). Currently this is used by the first pass
% of compaction to map document id's to `#full_doc_info{}`
% records so that we can recreate the id_tree in the new
% database.
%
% The structure of an HAMT is described in [1] which can
% be found easily on the internet. While it may sound a
% bit complicated its fairly easy conceptually. For each key
% we add to the HAMT, we calculate a hash value. To navigate
% the tree we consume 5 bits to find the child node. When a
% followed path has only one key/value we terminate our recursion
% rather than fill all nodes as one might expect from a b+tree
% (In other words, not all leaf nodes are at the same depth).
%
% This implementation has a slightly different solution for
% hash collisions in that rather than compute new hashes to
% continue recursing we just use a linked list that is probed
% to find the requested key. Given that our hash collision
% rates are on the order of 2^-32 this should provide for fairly
% small linked lists that must be searched when collisions occur.
%
% Beyond those basics the only somewhat complicated bits of math
% are related to the CTPOP implementation. This is copied from
% the original HAMT paper and taken as an article of faith. If
% you just consider it to be "number of 1 bits in this value" it
% will simplify the logic when reading this module.
%
% [1] "Ideal Hash Trees" Phil Bagwell 2000
%      http://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf

-export([
    open/1,
    open/2,

    get_fd/1,
    get_state/1,

    add/2,

    insert/2,
    lookup/2
]).


-record(ehamt, {
    version = 1,
    fd,
    root = {node, 0, {}}
}).


open(Fd) ->
    {ok, #ehamt{fd = Fd}}.


open(Fd, Options) ->
    {ok, set_options(#ehamt{fd = Fd}, Options)}.


set_options(EHamt, []) ->
    EHamt;
set_options(EHamt, [{root, Root} | Rest]) ->
    set_options(EHamt#ehamt{root = Root}, Rest).


get_fd(#ehamt{fd = Fd}) ->
    Fd.


get_state(#ehamt{fd = Fd, root = Root}) ->
    flush_nodes(Fd, Root).


add(EHamt, KVs) ->
    NewSt = lists:foldl(fun(KV, Acc) ->
        insert(Acc, KV)
    end, EHamt, KVs),
    {ok, NewSt}.


insert(EHamt, {K, V}) ->
    #ehamt{
        fd = Fd,
        root = Root
    } = EHamt,
    HId = hash(K),
    {ok, DiskLoc, _} = couch_file:append_term(Fd, {K, V}),
    NewRoot = insert(Fd, read_node(Fd, Root), HId, DiskLoc, 0),
    EHamt#ehamt{
        root = NewRoot
    }.

insert(Fd, NodeDiskLoc, HId, DiskLoc, Depth) when is_integer(NodeDiskLoc) ->
    {ok, Node} = couch_file:pread_term(Fd, NodeDiskLoc),
    insert(Fd, Node, HId, DiskLoc, Depth);

insert(_Fd, {item, HId1, DiskLocs}, HId2, DiskLoc, Depth)
        when HId1 == HId2 orelse Depth >= 5 ->
    {item, HId1, [DiskLoc | DiskLocs]};

insert(Fd, {item, HId1, DiskLocs}, HId2, DiskLoc, Depth) ->
    % Transform this item in a new node and then
    % continue our insertion algorithm. We do this
    % because we're not guaranteed that the hash ids
    % are different at the current depth.
    Map = 1 bsl get_header_index(HId1, Depth),
    Children = {{item, HId1, DiskLocs}},
    Node = {node, Map, Children},
    insert(Fd, Node, HId2, DiskLoc, Depth);

insert(Fd, {node, Map, Children}, HId, DiskLoc, Depth) ->
    Idx = get_header_index(HId, Depth),
    case is_set(Map, Idx) of
        true ->
            Pos = get_header_position(Map, Idx),
            Child = erlang:element(Pos, Children),
            NewChild0 = insert(Fd, Child, HId, DiskLoc, Depth + 1),
            NewChild1 = write_node(Fd, NewChild0, Depth + 1),
            NewChildren = erlang:setelement(Pos, Children, NewChild1),
            {node, Map, NewChildren};
        false ->
            Pos = get_header_position(Map, Idx),
            NewChild0 = {item, HId, [DiskLoc]},
            NewChild1 = write_node(Fd, NewChild0, Depth + 1),
            NewMap = Map bor (1 bsl Idx),
            NewChildren = erlang:insert_element(Pos, Children, NewChild1),
            {node, NewMap, NewChildren}
    end.


lookup(EHamt, Key) ->
    #ehamt{
        fd = Fd,
        root = Root
    } = EHamt,
    HId = hash(Key),
    lookup(Fd, read_node(Fd, Root), HId, Key, 0).


lookup(_Fd, {item, HId1, []}, HId2, _, Depth)
        when HId1 == HId2 orelse Depth >= 5 ->
    not_found;

lookup(Fd, {item, HId1, [DiskLoc | RestLocs]}, HId2, Key, Depth)
        when HId1 == HId2 orelse Depth >= 5 ->
    case couch_file:pread_term(Fd, DiskLoc) of
        {ok, {Key, Value}} ->
            {ok, Value};
        {ok, _} ->
            lookup(Fd, {item, HId1, RestLocs}, HId2, Key, Depth)
    end;

lookup(_Fd, {item, HId1, _}, HId2, _, _) when HId1 /= HId2 ->
    not_found;

lookup(Fd, {node, Map, Children}, HId, Key, Depth) ->
    Idx = get_header_index(HId, Depth),
    case is_set(Map, Idx) of
        true ->
            Pos = get_header_position(Map, Idx),
            Child = read_node(Fd, Pos, Children),
            lookup(Fd, Child, HId, Key, Depth + 1);
        false ->
            not_found
    end.


write_node(_Fd, Child, Depth) when Depth >= 0, Depth =< 2 ->
    Child;

write_node(Fd, Child, Depth) when is_integer(Depth), Depth > 2 ->
    {ok, DiskLoc, _} = couch_file:append_term(Fd, Child),
    DiskLoc.


flush_nodes(_Fd, DiskLoc) when is_integer(DiskLoc) ->
    DiskLoc;

flush_nodes(Fd, {item, _, _} = Node) ->
    {ok, DiskLoc, _} = couch_file:append_term(Fd, Node),
    DiskLoc;

flush_nodes(Fd, {node, Map, Children}) ->
    NewChildren = lists:foldl(fun(Idx, Acc) ->
        case erlang:element(Idx, Acc) of
            DiskLoc when is_integer(DiskLoc) ->
                Acc;
            Child when is_tuple(Child) ->
                NewChild = flush_nodes(Fd, Child),
                erlang:setelement(Idx, Acc, NewChild)
        end
    end, Children, lists:seq(1, size(Children))),
    NewNode = {node, Map, NewChildren},
    {ok, DiskLoc, _} = couch_file:append_term(Fd, NewNode),
    DiskLoc.


read_node(Fd, Pos, Children) ->
    read_node(Fd, erlang:element(Pos, Children)).


read_node(Fd, DiskLoc) when is_integer(DiskLoc) ->
    {ok, Child} = couch_file:pread_term(Fd, DiskLoc),
    Child;

read_node(_Fd, Child) when is_tuple(Child) ->
    Child.


hash(Term) ->
    erlang:phash2(Term, 4294967296).


get_header_index(HashId, Depth) when Depth >= 0, Depth < 5->
    Rotate = 32 - 5 * (Depth + 1),
    (HashId bsr Rotate) band 16#1F.


is_set(Header, Idx) when Idx >= 0, Idx =< 31 ->
    Mask = 1 bsl Idx,
    Header band Mask == Mask.


get_header_position(Header, Idx) ->
    % Adding 1 because tuples are 1 based
    1 + ctpop(Header band ((1 bsl Idx) - 1)).


ctpop(V0) when V0 >= 0, V0 =< 16#FFFFFFFF ->
    SK5 = 16#55555555,
    SK3 = 16#33333333,
    SKF0 = 16#F0F0F0F,

    V1 = V0 - ((V0 bsr 1) band SK5),
    V2 = (V1 band SK3) + ((V1 bsr 2) band SK3),
    V3 = (V2 band SKF0) + ((V2 bsr 4) band SKF0),
    V4 = V3 + (V3 bsr 8),
    (V4 + (V4 bsr 16)) band 16#3F.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bit_count(0) ->
    0;
bit_count(V0) when V0 >= 0, V0 =< 16#FFFFFFFF ->
    (V0 band 1) + bit_count(V0 bsr 1).


ctpop_test() ->
    Cases = [
        {0, 0},
        {1, 1},
        {1, 2},
        {2, 3},
        {1, 4},
        {8, 255},
        {1, 256},
        {32, 16#FFFFFFFF}
    ],
    random:seed(os:timestamp()),
    SmallCases = lists:map(fun(I) ->
        {bit_count(I), I}
    end, lists:seq(1, 16#10000)),
    RandomCases = lists:map(fun(_) ->
        T = random:uniform(16#100000000) - 1,
        {bit_count(T), T}
    end, lists:seq(1, 5000)),
    lists:foreach(fun({E, T}) ->
        ?assertEqual(E, ctpop(T))
    end, Cases ++ SmallCases ++ RandomCases).

-endif.

