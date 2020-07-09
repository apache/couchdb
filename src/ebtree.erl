-module(ebtree).

-export([
     open/3,
     open/4,
     min/0,
     max/0,
     insert/4,
     delete/3,
     lookup/3,
     range/6,
     reverse_range/6,
     fold/4,
     reduce/4,
     full_reduce/2,
     group_reduce/7,
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
    members = [] %% [{Key0, Value0} | {FirstKey0, LastKey0, Pointer0, Reduction0}, ...]
}).

-record(tree, {
    prefix,
    min,
    max,
    collate_fun,
    reduce_fun
}).

-define(META, 0).
-define(META_ORDER, 0).
-define(META_NEXT_ID, 1).

-define(NODE, 1).
-define(NODE_ROOT_ID, 0).

-define(underflow(Tree, Node), Tree#tree.min > length(Node#node.members)).
-define(at_min(Tree, Node), Tree#tree.min == length(Node#node.members)).
-define(is_full(Tree, Node), Tree#tree.max == length(Node#node.members)).

%% two special 1-bit bitstrings that cannot appear in valid keys.
-define(MIN, <<0:1>>).
-define(MAX, <<1:1>>).


open(Db, Prefix, Order) ->
    open(Db, Prefix, Order, []).

open(Db, Prefix, Order, Options) when is_binary(Prefix), is_integer(Order), Order > 2, Order rem 2 == 0 ->
    ReduceFun = proplists:get_value(reduce_fun, Options, fun reduce_noop/2),
    CollateFun = proplists:get_value(collate_fun, Options, fun collate_raw/2),

    erlfdb:transactional(Db, fun(Tx) ->
        case get_meta(Tx, Prefix, ?META_ORDER) of
            not_found ->
                erlfdb:clear_range_startswith(Tx, Prefix),
                set_meta(Tx, Prefix, ?META_ORDER, Order),
                set_meta(Tx, Prefix, ?META_NEXT_ID, 1),
                set_node(Tx, init_tree(Prefix, Order), #node{id = ?NODE_ROOT_ID});
            Order ->
                ok;
            Else ->
                erlang:error({order_mismatch, Else})
        end
    end),
    Tree = init_tree(Prefix, Order),
    Tree#tree{
        reduce_fun = ReduceFun,
        collate_fun = CollateFun
    }.


min() ->
    ?MIN.


max() ->
    ?MAX.

%% lookup

lookup(Db, #tree{} = Tree, Key) ->
    Fun = fun
        ({visit, K, V}, _Acc) when K =:= Key ->
            {stop, {K, V}};
        ({visit, K, _V}, Acc) ->
            case greater_than(Tree, K, Key) of
                true ->
                    {stop, Acc};
                false ->
                    {ok, Acc}
            end;
        ({traverse, F, L, _R}, Acc) ->
            case {greater_than(Tree, F, Key), less_than_or_equal(Tree, Key, L)} of
                {true, _} ->
                    {stop, Acc};
                {false, true} ->
                    {ok, Acc};
                {false, false} ->
                    {skip, Acc}
            end
    end,
    fold(Db, Tree, Fun, false).

%% fold

fold(Db, #tree{} = Tree, Fun, Acc) ->
    {_, Reduce} = erlfdb:transactional(Db, fun(Tx) ->
        Root = get_node(Tx, Tree, ?NODE_ROOT_ID),
        fold(Db, Tree, Root, Fun, Acc)
    end),
    Reduce.

fold(Db, #tree{} = Tree, #node{} = Node, Fun, Acc) ->
    fold(Db, #tree{} = Tree, Node#node.members, Fun, Acc);


fold(_Db, #tree{} = _Tree, [], _Fun, Acc) ->
    {ok, Acc};

fold(Db, #tree{} = Tree, [{K, V} | Rest], Fun, Acc0) ->
    case Fun({visit, K, V}, Acc0) of
        {ok, Acc1} ->
            fold(Db, Tree, Rest, Fun, Acc1);
        {stop, Acc1} ->
            {stop, Acc1}
    end;

fold(Db, #tree{} = Tree, [{F, L, P, R} | Rest], Fun, Acc0) ->
    case Fun({traverse, F, L, R}, Acc0) of
        {ok, Acc1} ->
            Node = get_node(Db, Tree, P),
            case fold(Db, Tree, Node, Fun, Acc1) of
                {ok, Acc2} ->
                    fold(Db, Tree, Rest, Fun, Acc2);
                {stop, Acc2} ->
                    {stop, Acc2}
            end;
        {skip, Acc1} ->
            fold(Db, Tree, Rest, Fun, Acc1);
        {stop, Acc1} ->
            {stop, Acc1}
    end.

%% full reduce

full_reduce(Db, #tree{} = Tree) ->
    Fun = fun
        ({visit, K, V}, {MapAcc, ReduceAcc}) ->
            {ok, {[{K, V} | MapAcc], ReduceAcc}};
        ({traverse, _F, _L, R}, {MapAcc, ReduceAcc}) ->
            {skip, {MapAcc, [R | ReduceAcc]}}
    end,
    {MapValues, ReduceValues} = fold(Db, Tree, Fun, {[], []}),
    do_reduce(Tree, MapValues, ReduceValues).


%% reduce

reduce(Db, #tree{} = Tree, StartKey, EndKey) ->
    Fun = fun
        ({visit, Key, Value}, {MapAcc, ReduceAcc}) ->
            AfterEnd = greater_than(Tree, Key, EndKey),
            InRange = greater_than_or_equal(Tree, Key, StartKey) andalso less_than_or_equal(Tree, Key, EndKey),
            if
                AfterEnd ->
                    {stop, {MapAcc, ReduceAcc}};
                InRange ->
                     {ok, {[{Key, Value} | MapAcc], ReduceAcc}};
                true ->
                    {ok, {MapAcc, ReduceAcc}}
            end;
        ({traverse, FirstKey, LastKey, Reduction}, {MapAcc, ReduceAcc}) ->
            BeforeStart = less_than(Tree, LastKey, StartKey),
            AfterEnd = greater_than(Tree, FirstKey, EndKey),
            Whole = greater_than_or_equal(Tree, FirstKey, StartKey) andalso less_than_or_equal(Tree, LastKey, EndKey),
            if
                BeforeStart ->
                    {skip, {MapAcc, ReduceAcc}};
                AfterEnd ->
                    {stop, {MapAcc, ReduceAcc}};
                Whole ->
                    {skip, {MapAcc, [Reduction | ReduceAcc]}};
                true ->
                    {ok, {MapAcc, ReduceAcc}}
            end
    end,
    {MapValues, ReduceValues} = fold(Db, Tree, Fun, {[], []}),
    do_reduce(Tree, MapValues, ReduceValues).


do_reduce(#tree{} = Tree, [], ReduceValues) when is_list(ReduceValues) ->
    reduce_values(Tree, ReduceValues, true);

do_reduce(#tree{} = Tree, MapValues, ReduceValues) when is_list(MapValues), is_list(ReduceValues) ->
    do_reduce(Tree, [], [reduce_values(Tree, MapValues, false) | ReduceValues]).


%% group reduce - produces reductions for contiguous keys in the same group.

group_reduce(Db, #tree{} = Tree, StartKey, EndKey, GroupKeyFun, UserAccFun, UserAcc0) ->
    NoGroupYet = ?MIN,
    Fun = fun
        ({visit, Key, Value}, {CurrentGroup, UserAcc, MapAcc, ReduceAcc}) ->
            AfterEnd = greater_than(Tree, Key, EndKey),
            InRange = in_range(Tree, StartKey, Key, EndKey),
            KeyGroup = GroupKeyFun(Key),
            SameGroup = CurrentGroup =:= KeyGroup,
            if
                AfterEnd ->
                    {stop, {CurrentGroup, UserAcc, MapAcc, ReduceAcc}};
                SameGroup ->
                    {ok, {CurrentGroup, UserAcc, [{Key, Value} | MapAcc], ReduceAcc}};
                InRange andalso CurrentGroup =:= NoGroupYet ->
                    {ok, {KeyGroup, UserAcc, [{Key, Value}], []}};
                InRange ->
                    %% implicit end of current group and start of a new one
                    GroupValue = do_reduce(Tree, MapAcc, ReduceAcc),
                    {ok, {KeyGroup, UserAccFun({CurrentGroup, GroupValue}, UserAcc), [{Key, Value}], []}};
                true ->
                    {ok, {CurrentGroup, UserAcc, MapAcc, ReduceAcc}}
            end;
        ({traverse, FirstKey, LastKey, Reduction}, {CurrentGroup, UserAcc, MapAcc, ReduceAcc}) ->
            BeforeStart = less_than(Tree, LastKey, StartKey),
            AfterEnd = greater_than(Tree, FirstKey, EndKey),
            Whole = CurrentGroup =:= GroupKeyFun(FirstKey) andalso CurrentGroup =:= GroupKeyFun(LastKey),
            FirstInRange = in_range(Tree, StartKey, FirstKey, EndKey),
            LastInRange = in_range(Tree, StartKey, LastKey, EndKey),
            if
                BeforeStart ->
                    {skip, {CurrentGroup, UserAcc, MapAcc, ReduceAcc}};
                AfterEnd ->
                    {stop, {CurrentGroup, UserAcc, MapAcc, ReduceAcc}};
                Whole andalso FirstInRange andalso LastInRange ->
                    {skip, {CurrentGroup, UserAcc, MapAcc, [Reduction | ReduceAcc]}};
                true ->
                    {ok, {CurrentGroup, UserAcc, MapAcc, ReduceAcc}}
            end
    end,
    {CurrentGroup, UserAcc1, MapValues, ReduceValues} = fold(Db, Tree, Fun, {NoGroupYet, UserAcc0, [], []}),
    if
        MapValues /= [] orelse ReduceValues /= [] ->
            FinalGroup = do_reduce(Tree, MapValues, ReduceValues),
            UserAccFun({CurrentGroup, FinalGroup}, UserAcc1);
        true ->
            UserAcc1
    end.


%% range (inclusive of both ends)

range(Db, #tree{} = Tree, StartKey, EndKey, AccFun, Acc0) ->
    erlfdb:transactional(Db, fun(Tx) ->
        range(Tx, Tree, get_node(Tx, Tree, ?NODE_ROOT_ID), StartKey, EndKey, AccFun, Acc0)
    end).

range(Tx, #tree{} = Tree, #node{level = 0} = Node, StartKey, EndKey, AccFun, Acc0) ->
    InRange = [{K, V} || {K, V} <- Node#node.members,
        less_than_or_equal(Tree, StartKey, K), less_than_or_equal(Tree, K, EndKey)],
    Acc1 = AccFun(InRange, Acc0),
    LastKey = last_key(Node),
    case Node#node.next /= undefined andalso less_than_or_equal(Tree, LastKey, EndKey) of
        true ->
            range(Tx, Tree, get_node(Tx, Tree, Node#node.next), StartKey, EndKey, AccFun, Acc1);
        false ->
            Acc1
    end;

range(Tx, #tree{} = Tree, #node{} = Node, StartKey, EndKey, AccFun, Acc) ->
    ChildId = find_child_id(Tree, Node, StartKey),
    range(Tx, Tree, get_node(Tx, Tree, ChildId), StartKey, EndKey, AccFun, Acc).

%% reverse range (inclusive of both ends)

reverse_range(Db, #tree{} = Tree, StartKey, EndKey, AccFun, Acc0) ->
    erlfdb:transactional(Db, fun(Tx) ->
        reverse_range(Tx, Tree, get_node(Tx, Tree, ?NODE_ROOT_ID), StartKey, EndKey, AccFun, Acc0)
    end).

reverse_range(Tx, #tree{} = Tree, #node{level = 0} = Node, StartKey, EndKey, AccFun, Acc0) ->
    InRange = [{K, V} || {K, V} <- Node#node.members,
        less_than_or_equal(Tree, StartKey, K), less_than_or_equal(Tree, K, EndKey)],
    Acc1 = AccFun(lists:reverse(InRange), Acc0),
    FirstKey = first_key(Node),
    case Node#node.prev /= undefined andalso less_than_or_equal(Tree, StartKey, FirstKey) of
        true ->
            reverse_range(Tx, Tree, get_node(Tx, Tree, Node#node.prev), StartKey, EndKey, AccFun, Acc1);
        false ->
            Acc1
    end;

reverse_range(Tx, #tree{} = Tree, #node{} = Node, StartKey, EndKey, AccFun, Acc) ->
    ChildId = find_child_id(Tree, Node, EndKey),
    reverse_range(Tx, Tree, get_node(Tx, Tree, ChildId), StartKey, EndKey, AccFun, Acc).


%% insert

insert(_Db, #tree{} = _Tree, ?MIN, _Value) ->
    erlang:error(min_not_allowed);

insert(_Db, #tree{} = _Tree, ?MAX, _Value) ->
    erlang:error(max_not_allowed);

insert(Db, #tree{} = Tree, Key, Value) ->
    erlfdb:transactional(Db, fun(Tx) ->
        Root0 = get_node(Tx, Tree, ?NODE_ROOT_ID),
        case ?is_full(Tree, Root0) of
            true ->
                OldRoot = Root0#node{id = new_node_id(Tx, Tree)},
                FirstKey = first_key(OldRoot),
                LastKey = last_key(OldRoot),
                Root1 = #node{
                    id = ?NODE_ROOT_ID,
                    level = Root0#node.level + 1,
                    members = [{FirstKey, LastKey, OldRoot#node.id, []}]},
                {Root2, _, _} = split_child(Tx, Tree, Root1, OldRoot),
                insert_nonfull(Tx, Tree, Root2, Key, Value);
            false ->
                insert_nonfull(Tx, Tree, Root0, Key, Value)
        end
    end),
    Tree.

split_child(Tx, #tree{} = Tree, #node{} = Parent0, #node{} = Child) ->
    {LeftMembers, RightMembers} = lists:split(Tree#tree.min, Child#node.members),

    LeftId = new_node_id(Tx, Tree),
    RightId = new_node_id(Tx, Tree),

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
    FirstLeftKey = first_key(LeftMembers),
    LastLeftKey = last_key(LeftMembers),
    FirstRightKey = first_key(RightMembers),
    LastRightKey = last_key(RightMembers),

    %% adjust parent reductions
    LeftReduction = reduce_node(Tree, LeftChild),
    RightReduction = reduce_node(Tree, RightChild),

    Parent1 = Parent0#node{
        members =
            umerge(Tree, [{FirstLeftKey, LastLeftKey, LeftId, LeftReduction}],
                umerge(Tree, [{FirstRightKey, LastRightKey, RightId, RightReduction}],
                    lists:keydelete(Child#node.id, 3, Parent0#node.members)))
    },
    clear_node(Tx, Tree, Child),
    set_nodes(Tx, Tree, [LeftChild, RightChild, Parent1]),
    {Parent1, LeftChild, RightChild}.


update_prev_neighbour(_Tx, #tree{} = _Tree, #node{prev = undefined} = _Node) ->
    ok;

update_prev_neighbour(Tx, #tree{} = Tree, #node{} = Node) ->
    Left = get_node(Tx, Tree, Node#node.prev),
    set_node(Tx, Tree, Left#node{next = Node#node.id}).


update_next_neighbour(_Tx, #tree{} = _Tree, #node{next = undefined} = _Node) ->
    ok;

update_next_neighbour(Tx, #tree{} = Tree, #node{} = Node) ->
    Left = get_node(Tx, Tree, Node#node.next),
    set_node(Tx, Tree, Left#node{prev = Node#node.id}).


insert_nonfull(Tx, #tree{} = Tree, #node{level = 0} = Node0, Key, Value) ->
    Node1 = Node0#node{
        members = umerge(Tree, [{Key, Value}], Node0#node.members)
    },
    set_node(Tx, Tree, Node1),
    reduce_node(Tree, Node1);

insert_nonfull(Tx, #tree{} = Tree, #node{} = Node0, Key, Value) ->
    ChildId0 = find_child_id(Tree, Node0, Key),
    Child0 = get_node(Tx, Tree, ChildId0),
    {Node1, Child1} = case ?is_full(Tree, Child0) of
        true ->
            {Parent, LeftChild, RightChild} = split_child(Tx, Tree, Node0, Child0),
            ChildId = find_child_id(Tree, Parent, Key),
            Child = if
                ChildId =:= LeftChild#node.id ->
                    LeftChild;
                ChildId =:= RightChild#node.id ->
                    RightChild
            end,
            {Parent, Child};
        false ->
            {Node0, Child0}
    end,
    ChildId1 = Child1#node.id,
    NewReduction = insert_nonfull(Tx, Tree, Child1, Key, Value),
    {CurrentFirstKey, CurrentLastKey, ChildId1, _OldReduction} = lists:keyfind(ChildId1, 3, Node1#node.members),
    [NewFirstKey, _] = sort(Tree, [Key, CurrentFirstKey]),
    [_, NewLastKey] = sort(Tree, [Key, CurrentLastKey]),
    Node2 = Node1#node{
        members = lists:keyreplace(ChildId1, 3, Node1#node.members,
            {NewFirstKey, NewLastKey, ChildId1, NewReduction})
    },
    set_node(Tx, Tree, Node2),
    reduce_node(Tree, Node2).


%% delete

delete(Db, #tree{} = Tree, Key) ->
    erlfdb:transactional(Db, fun(Tx) ->
        Root0 = get_node(Tx, Tree, ?NODE_ROOT_ID),
        case delete(Tx, Tree, Root0, Key) of
            % if only one child, make it the new root.
            #node{level = L, members = [_]} = Root1 when L > 0 ->
                [{_, _, ChildId, _}] = Root1#node.members,
                Root2 = get_node(Tx, Tree, ChildId),
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
    ChildId0 = find_child_id(Tree, Parent0, Key),
    Child0 = get_node(Tx, Tree, ChildId0),
    Child1 = delete(Tx, Tree, Child0, Key),
    case ?underflow(Tree, Child1) of
        true ->
            SiblingId = find_sibling_id(Tree, Parent0, ChildId0, Key),
            Sibling = get_node(Tx, Tree, SiblingId),
            NewNodes = case ?at_min(Tree, Sibling) of
                true ->
                    Merged = merge(Tx, Tree, Child1, Sibling),
                    update_prev_neighbour(Tx, Tree, Merged),
                    update_next_neighbour(Tx, Tree, Merged),
                    [Merged];
                false ->
                    {Left, Right} = rebalance(Tx, Tree, Child1, Sibling),
                    update_prev_neighbour(Tx, Tree, Left),
                    update_next_neighbour(Tx, Tree, Right),
                    [Left, Right]
            end,

            %% remove old members and insert new members
            Members0 = Parent0#node.members,
            Members1 = lists:keydelete(ChildId0, 3, Members0),
            Members2 = lists:keydelete(Sibling#node.id, 3, Members1),
            Members3 = lists:foldl(fun(N, Acc) ->
                umerge(Tree, [{first_key(N), last_key(N), N#node.id, reduce_node(Tree, N)}], Acc)
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
            {_OldFirstKey, _OldLastKey, ChildId0, _OldReduction} = lists:keyfind(ChildId0, 3, Parent0#node.members),
            Parent0#node{
                members = lists:keyreplace(ChildId0, 3, Parent0#node.members,
                    {first_key(Child1), last_key(Child1), Child1#node.id, reduce_node(Tree, Child1)})
            }
    end.


merge(Tx, #tree{} = Tree, #node{level = Level} = Node1, #node{level = Level} = Node2) ->
    [Left, Right] = sort(Tree, [Node1, Node2]),

    #node{
        id = new_node_id(Tx, Tree),
        level = Level,
        prev = Left#node.prev,
        next = Right#node.next,
        members = lists:append(Left#node.members, Right#node.members)
    }.


rebalance(Tx, #tree{} = Tree, #node{level = Level} = Node1, #node{level = Level} = Node2) ->
    [Left0, Right0] = sort(Tree, [Node1, Node2]),

    Members = lists:append(Left0#node.members, Right0#node.members),
    {LeftMembers, RightMembers} = lists:split(length(Members) div 2, Members),

    Left1Id = new_node_id(Tx, Tree),
    Right1Id = new_node_id(Tx, Tree),

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

find_child_id(#tree{} = Tree, #node{} = Node, Key) ->
    element(3, find_child(Tree, Node, Key)).


find_sibling_id(#tree{} = Tree, #node{level = L} = Node0, Id, Key) when L > 0 ->
    Node1 = Node0#node{members = lists:keydelete(Id, 3, Node0#node.members)},
    find_child_id(Tree, Node1, Key).


find_child(#tree{} = Tree, #node{level = L} = Node, Key) when L > 0 ->
    find_child_int(Tree, Node#node.members, Key).


find_child_int(#tree{} = _Tree, [Child], _Key) ->
    Child;

find_child_int(#tree{} = Tree, [{_F, L, _P, _R} = Child| Rest], Key) ->
    #tree{collate_fun = CollateFun} = Tree,
    case CollateFun(Key, L) of
        true ->
            Child;
        false ->
            find_child_int(Tree, Rest, Key)
    end.


%% metadata functions

get_meta(Tx, #tree{} = Tree, MetaKey) ->
    get_meta(Tx, Tree#tree.prefix, MetaKey);


get_meta(Tx, Prefix, MetaKey) when is_binary(Prefix) ->
    Future = get_meta_future(Tx, Prefix, MetaKey),
    case erlfdb:wait(Future) of
        not_found ->
            not_found;
        Value ->
            decode_value(Value)
    end.


get_meta_future(Tx, Prefix, MetaKey) ->
    erlfdb:get(Tx, meta_key(Prefix, MetaKey)).


set_meta(Tx, Prefix, MetaKey, MetaValue) ->
    erlfdb:set(
        Tx,
        meta_key(Prefix, MetaKey),
        encode_value(MetaValue)
    ).

meta_key(Prefix, MetaKey) when is_binary(Prefix) ->
    erlfdb_tuple:pack({?META, MetaKey}, Prefix).

%% node persistence functions

get_node(Tx, #tree{} = Tree, Id) ->
    get_node_wait(Id, get_node_future(Tx, Tree, Id)).


get_node_wait(Id, Future) ->
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


node_key(Prefix, Id) when is_binary(Prefix), is_integer(Id) ->
    erlfdb_tuple:pack({?NODE, Id}, Prefix).


validate_tree(Db, #tree{} = Tree) ->
    erlfdb:transactional(Db, fun(Tx) ->
        Root = get_node(Db, Tree, ?NODE_ROOT_ID),
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

validate_tree(Tx, #tree{} = Tree, [{_F, _L, P, _R} | Rest]) ->
    Node = get_node(Tx, Tree, P),
    validate_tree(Tx, Tree, Node),
    validate_tree(Tx, Tree, Rest).


validate_node(#tree{} = Tree, #node{} = Node) ->
    NumKeys = length(Node#node.members),
    IsLeaf = Node#node.level =:= 0,
    IsRoot = ?NODE_ROOT_ID == Node#node.id,
    OutOfOrder = Node#node.members /= sort(Tree, Node#node.members),
    Duplicates = Node#node.members /= usort(Tree, Node#node.members),
    if
        Node#node.id == undefined ->
            erlang:error({node_without_id, Node});
        not IsRoot andalso NumKeys < Tree#tree.min ->
            erlang:error({too_few_keys, Node});
        NumKeys > Tree#tree.max ->
            erlang:error({too_many_keys, Node});
        not IsLeaf andalso Node#node.prev /= undefined ->
            erlang:error({non_leaf_with_prev, Node});
        not IsLeaf andalso Node#node.next /= undefined ->
            erlang:error({non_leaf_with_next, Node});
        OutOfOrder ->
            erlang:error({out_of_order, Node});
        Duplicates ->
            erlang:error({duplicates, Node});
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
    reduce_values(Tree, Node#node.members, false);

reduce_node(#tree{} = Tree, #node{} = Node) ->
    Rs = [R || {_F, _L, _P, R} <- Node#node.members],
    reduce_values(Tree, Rs, true).


reduce_values(#tree{} = Tree, Values, Rereduce) when is_list(Values) ->
    #tree{reduce_fun = ReduceFun} = Tree,
    ReduceFun(Values, Rereduce).


%% collation functions

in_range(#tree{} = Tree, StartOfRange, Key, EndOfRange) ->
    greater_than_or_equal(Tree, Key, StartOfRange) andalso less_than_or_equal(Tree, Key, EndOfRange).


greater_than(#tree{} = Tree, A, B) ->
    not less_than_or_equal(Tree, A, B).


greater_than_or_equal(#tree{} = _Tree, A, A) ->
    true;

greater_than_or_equal(#tree{} = Tree, A, B) ->
    greater_than(Tree, A, B).


less_than(#tree{} = _Tree, A, A) ->
    false;

less_than(#tree{} = Tree, A, B) ->
    less_than_or_equal(Tree, A, B).


less_than_or_equal(#tree{} = _Tree, ?MIN, _B) ->
    true;

less_than_or_equal(#tree{} = _Tree, _A, ?MIN) ->
    false;

less_than_or_equal(#tree{} = _Tree, ?MAX, _B) ->
    false;

less_than_or_equal(#tree{} = _Tree, _A, ?MAX) ->
    true;

less_than_or_equal(#tree{} = Tree, A, B) ->
    #tree{collate_fun = CollateFun} = Tree,
    CollateFun(A, B).


umerge(#tree{} = Tree, List1, List2) ->
    #tree{collate_fun = CollateFun} = Tree,
    lists:umerge(collation_wrapper_fun(CollateFun), List1, List2).


sort(#tree{} = Tree, List) ->
    #tree{collate_fun = CollateFun} = Tree,
    lists:sort(collation_wrapper_fun(CollateFun), List).


usort(#tree{} = Tree, List) ->
    #tree{collate_fun = CollateFun} = Tree,
    lists:usort(collation_wrapper_fun(CollateFun), List).

collation_wrapper_fun(CollateFun) ->
    fun
        (#node{} = N1, #node{} = N2) ->
            CollateFun(first_key(N1), first_key(N2));
        ({K1, _V1}, {K2, _V2}) ->
            CollateFun(K1, K2);
        ({_F1, L1, _V1, _R1}, {_F2, L2, _V2, _R2}) ->
            CollateFun(L1, L2);
        (K1, K2) ->
            CollateFun(K1, K2)
    end.


collate_raw(K1, K2) ->
    K1 =< K2.


%% private functions

init_tree(Prefix, Order)
  when is_binary(Prefix), is_integer(Order), Order > 2, Order rem 2 == 0 ->
    #tree{
        prefix = Prefix,
        min = Order div 2,
        max = Order
    }.


first_key(#node{} = Node) ->
    first_key(Node#node.members);

first_key(Members) when is_list(Members) ->
    element(1, hd(Members)).


last_key(#node{} = Node) ->
    last_key(Node#node.members);

last_key(Members) when is_list(Members) ->
    case lists:last(Members) of
        {K, _V} ->
            K;
        {_F, L, _P, _R} ->
            L
    end.


new_node_id(Tx, Tree) ->
    NextId = get_meta(Tx, Tree, ?META_NEXT_ID),
    set_meta(Tx, Tree#tree.prefix, ?META_NEXT_ID, NextId + 1),
    NextId.


%% remove prev/next pointers for nonleaf nodes
remove_pointers_if_not_leaf(#node{level = 0} = Node) ->
    Node;

remove_pointers_if_not_leaf(#node{} = Node) ->
    Node#node{prev = undefined, next = undefined}.


print_node(#node{} = Node) ->
    io:format("#node{id = ~w, level = ~w, prev = ~w, next = ~w, members = ~w}~n~n",
        [Node#node.id, Node#node.level, Node#node.prev, Node#node.next, Node#node.members]).


%% tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

collation_fun_test_() ->
    Tree = #tree{collate_fun = fun collate_raw/2},
    [
        ?_test(?assert(greater_than(Tree, 4, 3))),
        ?_test(?assertNot(greater_than(Tree, 3, 4))),
        ?_test(?assert(greater_than_or_equal(Tree, 3, 3))),
        ?_test(?assert(greater_than_or_equal(Tree, 3, 3))),
        ?_test(?assert(less_than(Tree, 3, 4))),
        ?_test(?assertNot(less_than(Tree, 3, 3))),
        ?_test(?assertNot(less_than(Tree, 4, 3))),
        ?_test(?assert(less_than_or_equal(Tree, 3, 3))),
        ?_test(?assert(less_than_or_equal(Tree, 3, 4))),
        ?_test(?assertNot(less_than_or_equal(Tree, 4, 3)))
    ].


lookup_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, 100)])],
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key + 1) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, lookup(Db, Tree, Key)) end, Keys),
    ?assertEqual(false, lookup(Db, Tree, 101)).


delete_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, 100)])],
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key + 1) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, lookup(Db, Tree, Key)) end, Keys),
    lists:foreach(fun(Key) -> delete(Db, Tree, Key) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual(false, lookup(Db, Tree, Key)) end, Keys).


range_after_delete_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, 100)])],
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key + 1) end, Keys),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, lookup(Db, Tree, Key)) end, Keys),
    lists:foreach(fun(Key) -> delete(Db, Tree, Key) end, lists:seq(1, 100, 2)),
    ?assertEqual(50, range(Db, Tree, 1, 100, fun(E, A) -> length(E) + A end, 0)),
    ?assertEqual(50, reverse_range(Db, Tree, 1, 100, fun(E, A) -> length(E) + A end, 0)).


full_reduce_test_() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4, [{reduce_fun, fun reduce_sum/2}]),
    TestFun = fun(Max) ->
        Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
        lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key) end, Keys),
        ?assertEqual(round(Max * ((1 + Max) / 2)), full_reduce(Db, Tree))
    end,
    [
        ?_test(TestFun(4)),
        ?_test(TestFun(8))
    ].


full_reduce_after_delete_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4, [{reduce_fun, fun reduce_sum/2}]),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key) end, Keys),
    ?assertEqual(round(Max * ((1 + Max) / 2)), full_reduce(Db, Tree)),
    lists:foreach(fun(Key) -> delete(Db, Tree, Key) end, Keys),
    ?assertEqual(0, full_reduce(Db, Tree)).


count_reduce_test_() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4, [{reduce_fun, fun reduce_count/2}]),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key) end, Keys),
    Expected = fun(S, E) -> E - S + 1 end,
    [
        ?_test(?assertEqual(Expected(1, 5), reduce(Db, Tree, 1, 5))),
        ?_test(?assertEqual(Expected(50, 60), reduce(Db, Tree, 50, 60))),
        ?_test(?assertEqual(Expected(21, 83), reduce(Db, Tree, 21, 83))),
        ?_test(?assertEqual(Expected(1, 1), reduce(Db, Tree, 1, 1))),
        ?_test(?assertEqual(Expected(1, 100), reduce(Db, Tree, 0, 200))),
        ?_test(?assertEqual(Expected(5, 7), reduce(Db, Tree, 5, 7)))
    ].

sum_reduce_test_() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4, [{reduce_fun, fun reduce_sum/2}]),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key) end, Keys),
    Expected = fun(S, E) -> lists:sum(lists:seq(S, E)) end,
    [
        ?_test(?assertEqual(Expected(1, 5), reduce(Db, Tree, 1, 5))),
        ?_test(?assertEqual(Expected(50, 60), reduce(Db, Tree, 50, 60))),
        ?_test(?assertEqual(Expected(21, 83), reduce(Db, Tree, 21, 83))),
        ?_test(?assertEqual(Expected(1, 1), reduce(Db, Tree, 1, 1))),
        ?_test(?assertEqual(Expected(1, 100), reduce(Db, Tree, 0, 200))),
        ?_test(?assertEqual(Expected(5, 7), reduce(Db, Tree, 5, 7)))
    ].


stats_reduce_test_() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4, [{reduce_fun, fun reduce_stats/2}]),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key) end, Keys),
    [
        ?_test(?assertEqual({15,1,5,5,55}, reduce(Db, Tree, 1, 5))),
        ?_test(?assertEqual({605,50,60,11,33385}, reduce(Db, Tree, 50, 60))),
        ?_test(?assertEqual({3276,21,83,63,191184}, reduce(Db, Tree, 21, 83))),
        ?_test(?assertEqual({1,1,1,1,1}, reduce(Db, Tree, 1, 1))),
        ?_test(?assertEqual({5050,1,100,100,338350}, reduce(Db, Tree, 0, 200))),
        ?_test(?assertEqual({18,5,7,3,110}, reduce(Db, Tree, 5, 7)))
    ].


group_reduce_level_test_() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4, [{reduce_fun, fun reduce_sum/2}]),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    GroupKeyFun = fun(Key) -> lists:sublist(Key, 2) end,
    UserAccFun = fun({K,V}, Acc) -> Acc ++ [{K, V}] end,
    lists:foreach(fun(Key) -> insert(Db, Tree, [Key rem 4, Key rem 3, Key], Key) end, Keys),
    [
        ?_test(?assertEqual([{[1, 0], 408}, {[1, 1], 441}, {[1, 2], 376}],
            group_reduce(Db, Tree, [1], [2], GroupKeyFun, UserAccFun, []))),

        ?_test(?assertEqual([{[0,0],432}, {[0,1],468}, {[0,2],400}, {[1,0],408}, {[1,1],441}, {[1,2],376},
            {[2,0],384}, {[2,1],416}, {[2,2],450}, {[3,0],459}, {[3,1],392}, {[3,2],424}],
            group_reduce(Db, Tree, ebtree:min(), ebtree:max(), GroupKeyFun, UserAccFun, [])))
    ].


group_reduce_int_test_() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4, [{reduce_fun, fun reduce_count/2}]),
    Max = 100,
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
    GroupKeyFun = fun(_Key) -> null end,
    UserAccFun = fun({K,V}, Acc) -> Acc ++ [{K, V}] end,
    lists:foreach(fun(Key) -> insert(Db, Tree, Key, Key) end, Keys),
    [
        ?_test(?assertEqual([{null, 100}], group_reduce(Db, Tree,
            ebtree:min(), ebtree:max(), GroupKeyFun, UserAccFun, []))),
        ?_test(?assertEqual([{null, 99}], group_reduce(Db, Tree, 2, ebtree:max(), GroupKeyFun, UserAccFun, []))),
        ?_test(?assertEqual([{null, 96}], group_reduce(Db, Tree, 3, 98, GroupKeyFun, UserAccFun, [])))
    ].


raw_collation_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    Tree = open(Db, <<1,2,3>>, 4),
    insert(Db, Tree, null, null),
    insert(Db, Tree, 1, 1),
    ?assertEqual([{1, 1}, {null, null}], range(Db, Tree, 1, null, fun(E, A) -> A ++ E end, [])).


custom_collation_test() ->
    Db = erlfdb_util:get_test_db([empty]),
    CollateFun = fun(A, B) -> B =< A end,
    Tree = open(Db, <<1,2,3>>, 4, [{collate_fun, CollateFun}]),
    insert(Db, Tree, 1, 1),
    insert(Db, Tree, 2, 2),
    ?assertEqual([{2, 2}, {1, 1}], range(Db, Tree, 3, 0, fun(E, A) -> A ++ E end, [])).


intense_lookup_test_() ->
    [
        {timeout, 1000, fun() -> lookup_test_fun(1000, 20) end},
        {timeout, 1000, fun() -> lookup_test_fun(1000, 50) end},
        {timeout, 1000, fun() -> lookup_test_fun(1000, 500) end}
    ].


lookup_test_fun(Max, Order) ->
    Db = erlfdb_util:get_test_db([empty]),
    Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max, 2)])],
    T0 = erlang:monotonic_time(),
    Tree = lists:foldl(fun(Key, T) -> insert(Db, T, Key, Key + 1) end, open(Db, <<1,2,3>>, Order), Keys),
    T1 = erlang:monotonic_time(),
    lists:foreach(fun(Key) -> ?assertEqual({Key, Key + 1}, lookup(Db, Tree, Key)) end, Keys),
    T2 = erlang:monotonic_time(),
    ?debugFmt("~B order. ~B iterations. insert rate: ~.2f/s, lookup rate: ~.2f/s",
        [Order, Max, Max / sec(T1 - T0), Max / sec(T2 - T1)]).


range_test_() ->
    {timeout, 1000, fun() ->
        Db = erlfdb_util:get_test_db([empty]),
        Max = 1000,
        Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
        Tree = lists:foldl(fun(Key, T) -> insert(Db, T, Key, Key + 1) end, open(Db, <<1,2,3>>, 10), Keys),
        lists:foreach(
            fun(_) ->
                [StartKey, EndKey] = lists:sort([rand:uniform(Max), rand:uniform(Max)]),
                ?assertEqual([{K, K + 1} || K <- lists:seq(StartKey, EndKey)],
                    range(Db, Tree, StartKey, EndKey, fun(E, A) -> A ++ E end, [])
                ) end,
        lists:seq(1, 1000))
    end}.


reverse_range_test_() ->
    {timeout, 1000, fun() ->
        Db = erlfdb_util:get_test_db([empty]),
        Max = 1000,
        Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
        Tree = lists:foldl(fun(Key, T) -> insert(Db, T, Key, Key + 1) end, open(Db, <<1,2,3>>, 10), Keys),
        lists:foreach(
            fun(_) ->
                [StartKey, EndKey] = lists:sort([rand:uniform(Max), rand:uniform(Max)]),
                ?assertEqual([{K, K + 1} || K <- lists:seq(EndKey, StartKey, -1)],
                    reverse_range(Db, Tree, StartKey, EndKey, fun(E, A) -> A ++ E end, [])
                ) end,
        lists:seq(1, 1000))
    end}.


custom_collation_range_test_() ->
    {timeout, 1000, fun() ->
        Db = erlfdb_util:get_test_db([empty]),
        Max = 1000,
        Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
        CollateFun = fun(A, B) -> B =< A end,
        Tree = open(Db, <<1,2,3>>, 10, [{collate_fun, CollateFun}]),
        lists:foldl(fun(Key, T) -> insert(Db, T, Key, Key + 1) end, Tree, Keys),
        lists:foreach(
            fun(_) ->
                [StartKey, EndKey] = sort(Tree, [rand:uniform(Max), rand:uniform(Max)]),
                Seq = if
                    StartKey < EndKey ->
                        lists:seq(StartKey, EndKey);
                    true ->
                        lists:seq(StartKey, EndKey, -1)
                end,
                ?assertEqual([{K, K + 1} || K <- Seq],
                    range(Db, Tree, StartKey, EndKey, fun(E, A) -> A ++ E end, [])
                ) end,
        lists:seq(1, 1000))
    end}.


custom_collation_reverse_range_test_() ->
    {timeout, 1000, fun() ->
        Db = erlfdb_util:get_test_db([empty]),
        Max = 1000,
        Keys = [X || {_, X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1, Max)])],
        CollateFun = fun(A, B) -> B =< A end,
        Tree = open(Db, <<1,2,3>>, 10, [{collate_fun, CollateFun}]),
        lists:foldl(fun(Key, T) -> insert(Db, T, Key, Key + 1) end, Tree, Keys),
        lists:foreach(
            fun(_) ->
                [StartKey, EndKey] = sort(Tree, [rand:uniform(Max), rand:uniform(Max)]),
                Seq = if
                    StartKey < EndKey ->
                        lists:seq(StartKey, EndKey);
                    true ->
                        lists:seq(StartKey, EndKey, -1)
                end,
                ?assertEqual([{K, K + 1} || K <- lists:reverse(Seq)],
                    reverse_range(Db, Tree, StartKey, EndKey, fun(E, A) -> A ++ E end, [])
                ) end,
        lists:seq(1, 1000))
    end}.


sec(Native) ->
    erlang:max(1, erlang:convert_time_unit(Native, native, second)).

-endif.
