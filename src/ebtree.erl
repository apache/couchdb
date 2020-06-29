-module(ebtree).

-export([
     init/3,
     open/2,
     open/3,
     insert/4,
     delete/3,
     lookup/3,
     range/6,
     reverse_range/6,
     reduce/2,
     validate_tree/2
]).

%% built-in reduce functions
-export([
    reduce_sum/2,
    reduce_count/2,
    reduce_stats/2
]).

-record(node, {
    id,
    level = 0,
    prev,
    next,
    members = [] %% [{Key0, Value0} | {Key0, Pointer0, Reduction0}, ...]
}).

-record(tree, {
    prefix,
    min,
    max,
    reduce_fun
}).

-define(META, 0).
-define(META_ORDER, 0).

-define(NODE, 1).
-define(NODE_ROOT_ID, <<0>>).

-define(underflow(Tree, Node), Tree#tree.min > length(Node#node.members)).
-define(at_min(Tree, Node), Tree#tree.min == length(Node#node.members)).
-define(is_full(Tree, Node), Tree#tree.max == length(Node#node.members)).


init(Db, Prefix, Order) when is_binary(Prefix), is_integer(Order), Order > 2, Order rem 2 == 0 ->
    erlfdb:transactional(Db, fun(Tx) ->
        erlfdb:clear_range_startswith(Tx, Prefix),
        set_meta(Tx, Prefix, ?META_ORDER, Order),
        set_node(Tx, to_tree(Prefix, Order, undefined), #node{id = ?NODE_ROOT_ID}),
        ok
    end).


open(Db, Prefix) ->
    open(Db, Prefix, fun reduce_noop/2).

open(Db, Prefix, ReduceFun) when is_function(ReduceFun, 2) ->
     erlfdb:transactional(Db, fun(Tx) ->
        Order = get_meta(Tx, Prefix, ?META_ORDER),
        to_tree(Prefix, Order, ReduceFun)
    end).


%% lookup

lookup(Db, #tree{} = Tree, Key) ->
    erlfdb:transactional(Db, fun(Tx) ->
        lookup(Tx, Tree, get_node_wait(Tx, Tree, ?NODE_ROOT_ID), Key)
    end).

lookup(_Tx, #tree{} = _Tree, #node{level = 0} = Node, Key) ->
     find_value(Node, Key);

lookup(Tx, #tree{} = Tree, #node{} = Node, Key) ->
    ChildId = find_child_id(Node, Key),
    lookup(Tx, Tree, get_node_wait(Tx, Tree, ChildId), Key).

%% reduce lookup

reduce(Db, #tree{} = Tree) ->
    erlfdb:transactional(Db, fun(Tx) ->
        Root = get_node_wait(Tx, Tree, ?NODE_ROOT_ID),
        reduce_node(Tree, Root)
    end).


%% range (inclusive of both ends)

range(Db, #tree{} = Tree, StartKey, EndKey, Fun, Acc0) ->
    erlfdb:transactional(Db, fun(Tx) ->
        range(Tx, Tree, get_node_wait(Tx, Tree, ?NODE_ROOT_ID), StartKey, EndKey, Fun, Acc0)
    end).

range(Tx, #tree{} = Tree, #node{level = 0} = Node, StartKey, EndKey, Fun, Acc0) ->
    InRange = [{K, V} || {K, V} <- Node#node.members, K >= StartKey, K =< EndKey],
    Acc1 = Fun(InRange, Acc0),
    LastKey = last_key(Node),
    case Node#node.next /= undefined andalso EndKey >= LastKey of
        true ->
            range(Tx, Tree, get_node_wait(Tx, Tree, Node#node.next), StartKey, EndKey, Fun, Acc1);
        false ->
            Acc1
    end;

range(Tx, #tree{} = Tree, #node{} = Node, StartKey, EndKey, Fun, Acc) ->
    ChildId = find_child_id(Node, StartKey),
    range(Tx, Tree, get_node_wait(Tx, Tree, ChildId), StartKey, EndKey, Fun, Acc).

%% reverse range (inclusive of both ends)

reverse_range(Db, #tree{} = Tree, StartKey, EndKey, Fun, Acc0) ->
    erlfdb:transactional(Db, fun(Tx) ->
        reverse_range(Tx, Tree, get_node_wait(Tx, Tree, ?NODE_ROOT_ID), StartKey, EndKey, Fun, Acc0)
    end).

reverse_range(Tx, #tree{} = Tree, #node{level = 0} = Node, StartKey, EndKey, Fun, Acc0) ->
    InRange = [{K, V} || {K, V} <- Node#node.members, K >= StartKey, K =< EndKey],
    Acc1 = Fun(lists:reverse(InRange), Acc0),
    {FirstKey, _} = hd(Node#node.members),
    case Node#node.prev /= undefined andalso StartKey =< FirstKey of
        true ->
            reverse_range(Tx, Tree, get_node_wait(Tx, Tree, Node#node.prev), StartKey, EndKey, Fun, Acc1);
        false ->
            Acc1
    end;

reverse_range(Tx, #tree{} = Tree, #node{} = Node, StartKey, EndKey, Fun, Acc) ->
    ChildId = find_child_id(Node, EndKey),
    reverse_range(Tx, Tree, get_node_wait(Tx, Tree, ChildId), StartKey, EndKey, Fun, Acc).


%% insert

insert(Db, #tree{} = Tree, Key, Value) ->
    erlfdb:transactional(Db, fun(Tx) ->
        Root0 = get_node_wait(Tx, Tree, ?NODE_ROOT_ID),
        case ?is_full(Tree, Root0) of
            true ->
                OldRoot = Root0#node{id = new_node_id()},
                LastKey = last_key(OldRoot),
                Root1 = #node{
                    id = ?NODE_ROOT_ID,
                    level = Root0#node.level + 1,
                    members = [{LastKey, OldRoot#node.id}]},
                Root2 = split_child(Tx, Tree, Root1, OldRoot),
                insert_nonfull(Tx, Tree, Root2, Key, Value);
            false ->
                insert_nonfull(Tx, Tree, Root0, Key, Value)
        end
    end),
    Tree.

split_child(Tx, #tree{} = Tree, #node{} = Parent0, #node{} = Child) ->
    {LeftMembers, RightMembers} = lists:split(Tree#tree.min, Child#node.members),

    LeftId = new_node_id(),
    RightId = new_node_id(),

    LeftChild = remove_pointers_if_not_leaf(#node{
        id = LeftId,
        level = Child#node.level,
        prev = Child#node.prev,
        next = RightId,
        members = LeftMembers
    }),

    RightChild = remove_pointers_if_not_leaf(#node{
        id = RightId,
        level = Child#node.level,
        prev = LeftId,
        next = Child#node.next,
        members = RightMembers
    }),

    update_prev_neighbour(Tx, Tree, LeftChild),
    update_next_neighbour(Tx, Tree, RightChild),

    %% adjust parent members
    LastLeftKey = last_key(LeftMembers),
    LastRightKey = last_key(RightMembers),

    %% adjust parent reductions
    LeftReduction = reduce_node(Tree, LeftChild),
    RightReduction = reduce_node(Tree, RightChild),

    Parent1 = Parent0#node{
        members = lists:keymerge(1, [{LastLeftKey, LeftId, LeftReduction}, {LastRightKey, RightId, RightReduction}],
            lists:keydelete(Child#node.id, 2, Parent0#node.members))
    },
    clear_node(Tx, Tree, Child),
    set_nodes(Tx, Tree, [LeftChild, RightChild, Parent1]),
    Parent1.


update_prev_neighbour(_Tx, #tree{} = _Tree, #node{prev = undefined} = _Node) ->
    ok;

update_prev_neighbour(Tx, #tree{} = Tree, #node{} = Node) ->
    Left = get_node_wait(Tx, Tree, Node#node.prev),
    set_node(Tx, Tree, Left#node{next = Node#node.id}).


update_next_neighbour(_Tx, #tree{} = _Tree, #node{next = undefined} = _Node) ->
    ok;

update_next_neighbour(Tx, #tree{} = Tree, #node{} = Node) ->
    Left = get_node_wait(Tx, Tree, Node#node.next),
    set_node(Tx, Tree, Left#node{prev = Node#node.id}).


insert_nonfull(Tx, #tree{} = Tree, #node{level = 0} = Node0, Key, Value) ->
    Node1 = Node0#node{
        members = lists:ukeymerge(1, [{Key, Value}], Node0#node.members)
    },
    set_node(Tx, Tree, Node1),
    reduce_node(Tree, Node1);

insert_nonfull(Tx, #tree{} = Tree, #node{} = Node0, Key, Value) ->
    ChildId0 = find_child_id(Node0, Key),
    Child0 = get_node_wait(Tx, Tree, ChildId0),
    Node1 = case ?is_full(Tree, Child0) of
        true ->
            split_child(Tx, Tree, Node0, Child0);
        false ->
            Node0
    end,
    ChildId1 = find_child_id(Node1, Key),
    Child1 = get_node_wait(Tx, Tree, ChildId1),
    NewReduction = insert_nonfull(Tx, Tree, Child1, Key, Value),
    {CurrentKey, ChildId1, _OldReduction} = lists:keyfind(ChildId1, 2, Node1#node.members),
    Node2 = Node1#node{
        members = lists:keyreplace(ChildId1, 2, Node1#node.members,
            {erlang:max(Key, CurrentKey), ChildId1, NewReduction})
    },
    set_node(Tx, Tree, Node2),
    reduce_node(Tree, Node2).


%% delete

delete(Db, #tree{} = Tree, Key) ->
    erlfdb:transactional(Db, fun(Tx) ->
        Root0 = get_node_wait(Tx, Tree, ?NODE_ROOT_ID),
        case delete(Tx, Tree, Root0, Key) of
            % if only one child, make it the new root.
            #node{level = L, members = [_]} = Root1 when L > 0 ->
                [{_, ChildId, _}] = Root1#node.members,
                Root2 = get_node_wait(Tx, Tree, ChildId),
                clear_node(Tx, Tree, Root2),
                set_node(Tx, Tree, Root2#node{id = ?NODE_ROOT_ID});
            Root1 ->
                set_node(Tx, Tree, Root1)
        end
    end),
    Tree.


delete(_Tx, #tree{} = _Tree, #node{level = 0} = Node, Key) ->
    Node#node{
        members = lists:keydelete(Key, 1, Node#node.members)
    };

delete(Tx, #tree{} = Tree, #node{} = Parent0, Key) ->
    ChildId0 = find_child_id(Parent0, Key),
    Child0 = get_node_wait(Tx, Tree, ChildId0),
    Child1 = delete(Tx, Tree, Child0, Key),
    case ?underflow(Tree, Child1) of
        true ->
            SiblingId = find_sibling_id(Parent0, ChildId0, Key),
            Sibling = get_node_wait(Tx, Tree, SiblingId),
            NewNodes = case ?at_min(Tree, Sibling) of
                true ->
                    Merged = merge(Child1, Sibling),
                    update_prev_neighbour(Tx, Tree, Merged),
                    update_next_neighbour(Tx, Tree, Merged),
                    [Merged];
                false ->
                    {Left, Right} = rebalance(Child1, Sibling),
                    update_prev_neighbour(Tx, Tree, Left),
                    update_next_neighbour(Tx, Tree, Right),
                    [Left, Right]
            end,

            %% remove old members and insert new members
            Members0 = Parent0#node.members,
            Members1 = lists:keydelete(ChildId0, 2, Members0),
            Members2 = lists:keydelete(Sibling#node.id, 2, Members1),
            Members3 = lists:foldl(fun(N, Acc) ->
                lists:merge([{last_key(N), N#node.id, reduce_node(Tree, N)}], Acc)
            end, Members2, NewNodes),

            Parent1 = Parent0#node{
                %% TODO change id
                members = Members3
            },

            clear_nodes(Tx, Tree, [Child0, Sibling]),
            set_nodes(Tx, Tree, NewNodes),
            Parent1;
        false ->
            set_node(Tx, Tree, Child1),
            {ChildKey, ChildId0, _OldReduction} = lists:keyfind(ChildId0, 2, Parent0#node.members),
            Parent0#node{
                members = lists:keyreplace(ChildId0, 1, Parent0#node.members,
                    {ChildKey, Child1#node.id, reduce_node(Tree, Child1)})
            }
    end.


merge(#node{members = RightM} = Right, #node{members = LeftM} = Left) when RightM > LeftM ->
    merge(Left, Right);

merge(#node{level = Level} = Left, #node{level = Level} = Right) ->
    #node{
        id = new_node_id(),
        level = Level,
        prev = Left#node.prev,
        next = Right#node.next,
        members = lists:append(Left#node.members, Right#node.members)
    }.


rebalance(#node{members = RightM} = Right, #node{members = LeftM} = Left) when RightM > LeftM ->
    rebalance(Left, Right);

rebalance(#node{level = Level} = Left0, #node{level = Level} = Right0) ->
    Members = lists:append(Left0#node.members, Right0#node.members),
    {LeftMembers, RightMembers} = lists:split(length(Members) div 2, Members),

    Left1Id = new_node_id(),
    Right1Id = new_node_id(),

    Left1 = Left0#node{
        id = Left1Id,
        next = Right1Id,
        members = LeftMembers
    },
    Right1 = Right0#node{
        id = Right1Id,
        prev = Left1Id,
        members = RightMembers
    },
    {Left1, Right1}.


%% lookup functions

find_value(#node{level = 0} = Node, Key) ->
    lists:keyfind(Key, 1, Node#node.members).


find_child_id(#node{level = L} = Node, Key) when L > 0 ->
    find_child_id_int(Node#node.members, Key).

find_child_id_int([{K, V, _R}], Key) when Key > K ->
    V;

find_child_id_int([{K, V, _R} | _Rest], Key) when Key =< K ->
    V;

find_child_id_int([_ | Rest], Key) ->
    find_child_id_int(Rest, Key).


find_sibling_id(#node{level = L} = Node0, Id, Key) when L > 0 ->
    Node1 = Node0#node{members = lists:keydelete(Id, 2, Node0#node.members)},
    find_child_id(Node1, Key).

%% metadata functions

get_meta(Tx, #tree{} = Tree, MetaKey) ->
    get_meta(Tx, Tree#tree.prefix, MetaKey);

get_meta(Tx, Prefix, MetaKey) when is_binary(Prefix) ->
    decode_value(erlfdb:wait(erlfdb:get(Tx, meta_key(Prefix, MetaKey)))).


set_meta(Tx, Prefix, MetaKey, MetaValue) ->
    erlfdb:set(
        Tx,
        meta_key(Prefix, MetaKey),
        encode_value(MetaValue)
    ).

meta_key(Prefix, MetaKey) when is_binary(Prefix) ->
    erlfdb_tuple:pack({?META, MetaKey}, Prefix).

%% node persistence functions

get_node_wait(Tx, #tree{} = Tree, Id) ->
    get_node(Id, get_node_future(Tx, Tree, Id)).


get_node(Id, Future) ->
    decode_node(Id, erlfdb:wait(Future)).


get_node_future(Tx, #tree{} = Tree, Id) ->
    Key = node_key(Tree#tree.prefix, Id),
    erlfdb:get(Tx, Key).


clear_nodes(Tx, #tree{} = Tree, Nodes) ->
    lists:foreach(fun(Node) ->
        clear_node(Tx, Tree, Node)
    end, Nodes).


clear_node(Tx, #tree{} = Tree, #node{} = Node) ->
     Key = node_key(Tree#tree.prefix, Node#node.id),
     erlfdb:clear(Tx, Key).


set_nodes(Tx, #tree{} = Tree, Nodes) ->
    lists:foreach(fun(Node) ->
        set_node(Tx, Tree, Node)
    end, Nodes).


set_node(Tx, #tree{} = Tree, #node{} = Node) ->
    validate_node(Tree, Node),
    Key = node_key(Tree#tree.prefix, Node#node.id),
    Value = encode_node(Node),
    erlfdb:set(Tx, Key, Value).


node_key(Prefix, Id) when is_binary(Prefix), is_binary(Id) ->
    erlfdb_tuple:pack({?NODE, Id}, Prefix).


validate_tree(Db, #tree{} = Tree) ->
    erlfdb:transactional(Db, fun(Tx) ->
        Root = get_node_wait(Db, Tree, ?NODE_ROOT_ID),
        validate_tree(Tx, Tree, Root)
    end).

validate_tree(_Tx, #tree{} = Tree, #node{level = 0} = Node) ->
    print_node(Node),
    validate_node(Tree, Node);

validate_tree(Tx, #tree{} = Tree, #node{} = Node) ->
    print_node(Node),
    validate_node(Tree, Node),
    validate_tree(Tx, Tree, Node#node.members);

validate_tree(_Tx, #tree{} = _Tree, []) ->
    ok;

validate_tree(Tx, #tree{} = Tree, [NodeTuple | Rest]) ->
    Node = get_node_wait(Tx, Tree, element(2, NodeTuple)),
    validate_tree(Tx, Tree, Node),
    validate_tree(Tx, Tree, Rest).


validate_node(#tree{} = Tree, #node{} = Node) ->
    NumKeys = length(Node#node.members),
    IsRoot = ?NODE_ROOT_ID == Node#node.id,
    if
        Node#node.id == undefined ->
            erlang:error({node_without_id, Node});
        not IsRoot andalso NumKeys < Tree#tree.min ->
            erlang:error({too_few_keys, Node});
        NumKeys > Tree#tree.max ->
            erlang:error({too_many_keys, Node});
        true ->
            ok
    end.


%% data marshalling functions (encodes unnecesary fields as a NIL_REF)

encode_node(#node{prev = undefined} = Node) ->
    encode_node(Node#node{prev = []});

encode_node(#node{next = undefined} = Node) ->
    encode_node(Node#node{next = []});

encode_node(#node{} = Node) ->
    encode_value(Node#node{id = []}).


decode_node(Id, Bin) when is_binary(Bin) ->
    decode_node(Id, decode_value(Bin));

decode_node(Id, #node{prev = []} = Node) ->
    decode_node(Id, Node#node{prev = undefined});

decode_node(Id, #node{next = []} = Node) ->
    decode_node(Id, Node#node{next = undefined});

decode_node(Id, #node{} = Node) ->
    Node#node{id = Id}.


encode_value(Value) ->
    term_to_binary(Value, [compressed, {minor_version, 2}]).


decode_value(Bin) when is_binary(Bin) ->
    binary_to_term(Bin, [safe]).


%% built-in reduce functions.

reduce_noop(_KVs, _Rereduce) ->
    [].


reduce_sum(KVs, false) ->
    {_, Vs} = lists:unzip(KVs),
    lists:sum(Vs);

reduce_sum(Rs, true) ->
    lists:sum(Rs).


reduce_count(KVs, false) ->
    length(KVs);

reduce_count(Rs, true) ->
    lists:sum(Rs).


reduce_stats(KVs, false) ->
    {_, Vs} = lists:unzip(KVs),
    {
        lists:sum(Vs),
        lists:min(Vs),
        lists:max(Vs),
        length(Vs),
        lists:sum([V * V || V <- Vs])
    };

reduce_stats(Rs, true) ->
    lists:foldl(
        fun({Sum, Min, Max, Count, SumSqr},
            {SumAcc, MinAcc, MaxAcc, CountAcc, SumSqrAcc}) ->
        {
            Sum + SumAcc,
            erlang:min(Min, MinAcc),
            erlang:max(Max, MaxAcc),
            Count + CountAcc,
            SumSqr + SumSqrAcc
        } end, hd(Rs), tl(Rs)).


reduce_node(#tree{} = Tree, #node{level = 0} = Node) ->
    #tree{reduce_fun = ReduceFun} = Tree,
    ReduceFun(Node#node.members, false);

reduce_node(#tree{} = Tree, #node{} = Node) ->
    #tree{reduce_fun = ReduceFun} = Tree,
    Rs = [R || {_K, _V, R} <- Node#node.members],
    ReduceFun(Rs, true).


%% private functions

to_tree(Prefix, Order, ReduceFun)
  when is_binary(Prefix), is_integer(Order), Order > 2, Order rem 2 == 0 ->
    #tree{
        prefix = Prefix,
        min = Order div 2,
        max = Order,
        reduce_fun = ReduceFun
    }.


last_key(#node{} = Node) ->
    last_key(Node#node.members);

last_key(Members) when is_list(Members) ->
    element(1, lists:last(Members)).


new_node_id() ->
    crypto:strong_rand_bytes(16).


%% remove prev/next pointers for nonleaf nodes
remove_pointers_if_not_leaf(#node{level = 0} = Node) ->
    Node;

remove_pointers_if_not_leaf(#node{} = Node) ->
    Node#node{prev = undefined, next = undefined}.


print_node(#node{level = 0} = Node) ->
    io:format("#node{id = ~s, level = ~w, prev = ~s, next = ~s, members = ~w}~n~n",
        [b64(Node#node.id), Node#node.level, b64(Node#node.prev), b64(Node#node.next),
        Node#node.members]);

print_node(#node{} = Node) ->
    io:format("#node{id = ~s, level = ~w, prev = ~s, next = ~s, members = ~s}~n~n",
        [base64:encode(Node#node.id), Node#node.level, b64(Node#node.prev), b64(Node#node.next),
        [io_lib:format("{~w, ~s, ~w}, ", [K, b64(V), R]) || {K, V, R} <- Node#node.members]]).


b64(undefined) ->
    undefined;

b64(Bin) ->
    base64:encode(Bin).

%% tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lookup_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    ?MODULE:init(Db, <<1,2,3>>, 4),
    Tree = ?MODULE:open(Db, <<1,2,3>>),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, 100)])],
    lists:foreach(fun(Key) -> ?MODULE:insert(Db, Tree, Key, Key + 1) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, ?MODULE:lookup(Db, Tree, Key)) end, Keys).


delete_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    ?MODULE:init(Db, <<1,2,3>>, 4),
    Tree = ?MODULE:open(Db, <<1,2,3>>),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, 100)])],
    lists:foreach(fun(Key) -> ?MODULE:insert(Db, Tree, Key, Key + 1) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, ?MODULE:lookup(Db, Tree, Key)) end, Keys),
    lists:foreach(fun(Key) -> ?MODULE:delete(Db, Tree, Key) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual(false, ?MODULE:lookup(Db, Tree, Key)) end, Keys).


range_after_delete_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    ?MODULE:init(Db, <<1,2,3>>, 4),
    Tree = ?MODULE:open(Db, <<1,2,3>>),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, 100)])],
    lists:foreach(fun(Key) -> ?MODULE:insert(Db, Tree, Key, Key + 1) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, ?MODULE:lookup(Db, Tree, Key)) end, Keys),
    lists:foreach(fun(Key) -> ?MODULE:delete(Db, Tree, Key) end, lists:seq(1, 100, 2)),
    ?assertEqual(50, ?MODULE:range(Db, Tree, 1, 100, fun(E, A) -> length(E) + A end, 0)),
    ?assertEqual(50, ?MODULE:reverse_range(Db, Tree, 1, 100, fun(E, A) -> length(E) + A end, 0)).


reduce_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    ?MODULE:init(Db, <<1,2,3>>, 4),
    Tree = ?MODULE:open(Db, <<1,2,3>>, fun reduce_sum/2),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    lists:foreach(fun(Key) -> ?MODULE:insert(Db, Tree, Key, Key) end, Keys),
    ?assertEqual(round(Max * ((1 + Max) / 2)), ?MODULE:reduce(Db, Tree)).


reduce_after_delete_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    ?MODULE:init(Db, <<1,2,3>>, 4),
    Tree = ?MODULE:open(Db, <<1,2,3>>, fun reduce_sum/2),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    lists:foreach(fun(Key) -> ?MODULE:insert(Db, Tree, Key, Key) end, Keys),
    ?assertEqual(round(Max * ((1 + Max) / 2)), ?MODULE:reduce(Db, Tree)),
    lists:foreach(fun(Key) -> ?MODULE:delete(Db, Tree, Key) end, Keys),
    ?assertEqual(0, ?MODULE:reduce(Db, Tree)).

intense_lookup_test_() ->
    [
        {timeout, 1000, fun() -> lookup_test_fun(1000, 20) end},
        {timeout, 1000, fun() -> lookup_test_fun(1000, 50) end},
        {timeout, 1000, fun() -> lookup_test_fun(1000, 500) end}
    ].


lookup_test_fun(Max, Order) ->
    Db = erlfdb_util:get_test_db([empty]),
    ?MODULE:init(Db, <<1,2,3>>, Order),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max, 2)])],
    T0 = erlang:monotonic_time(),
    Tree = lists:foldl(fun(Key, T) -> ?MODULE:insert(Db, T, Key, Key + 1) end, ?MODULE:open(Db, <<1,2,3>>), Keys),
    T1 = erlang:monotonic_time(),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, ?MODULE:lookup(Db, Tree, Key)) end, Keys),
    T2 = erlang:monotonic_time(),
    ?debugFmt("~B order. ~B iterations. insert rate: ~.2f/s, lookup rate: ~.2f/s",
        [Order, Max, Max / sec(T1 - T0), Max / sec(T2 - T1)]).


range_test_() ->
    {timeout, 1000, fun() ->
        Db = erlfdb_util:get_test_db([empty]),
        ?MODULE:init(Db, <<1,2,3>>, 10),
        Max = 1000,
        Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
        Tree = lists:foldl(fun(Key, T) -> ?MODULE:insert(Db, T, Key, Key + 1) end, ?MODULE:open(Db, <<1,2,3>>), Keys),
        lists:foreach(
            fun(_) ->
                [StartKey, EndKey] = lists:sort([rand:uniform(Max), rand:uniform(Max)]),
                ?assertEqual(EndKey - StartKey + 1,
                    ?MODULE:range(Db, Tree, StartKey, EndKey, fun(E, A) -> length(E) + A end, 0)
                ) end,
        lists:seq(1, 1000))
    end}.


reverse_range_test_() ->
    {timeout, 1000, fun() ->
        Db = erlfdb_util:get_test_db([empty]),
        ?MODULE:init(Db, <<1,2,3>>, 10),
        Max = 1000,
        Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
        Tree = lists:foldl(fun(Key, T) -> ?MODULE:insert(Db, T, Key, Key + 1) end, ?MODULE:open(Db, <<1,2,3>>), Keys),
        lists:foreach(
            fun(_) ->
                [StartKey, EndKey] = lists:sort([rand:uniform(Max), rand:uniform(Max)]),
                ?assertEqual(EndKey - StartKey + 1,
                    ?MODULE:reverse_range(Db, Tree, StartKey, EndKey, fun(E, A) -> length(E) + A end, 0)
                ) end,
        lists:seq(1, 1000))
    end}.


sec(Native) ->
    erlang:max(1, erlang:convert_time_unit(Native, native, second)).

-endif.
