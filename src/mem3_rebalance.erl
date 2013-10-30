% Copyright 2013 Cloudant
%
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

-module(mem3_rebalance).

-export([
    contract/0,
    contract/1,
    contract/3,
    expand/0,
    expand/1,
    expand/3,
    fix_zoning/0,
    fix_zoning/1,
    fix_zoning/2,
    print/1
]).

% Exposed for debugging purposes
-export([
    shard_count_by_node/1,
    shard_count_view/0
]).

-include("mem3.hrl").

-record (gacc, {
    node,
    targets,
    moves,
    limit,
    target_level
}).

%% @equiv expand(1000)
-spec expand() -> [{atom(), #shard{}, node()}].
expand() ->
    expand(1000).

%% @doc Expands a cluster without requiring each DB to be optimally balanced.
-spec expand(integer() | global) -> [{atom(), #shard{}, node()}].
expand(global) ->
    global_expand(surviving_nodes(), [], 1000);

%% @doc Expands all databases in the cluster, stopping at Limit operations.
expand(Limit) when is_integer(Limit), Limit > 0 ->
    TargetNodes = surviving_nodes(),
    LocalBalanceFun = fun(Db, Moves) -> expand(Db, TargetNodes, Moves) end,
    LocalBalanceOps = apply_to_cluster(LocalBalanceFun, Limit),
    % Now apply additional operations as needed to achieve global balance.
    global_expand(TargetNodes, LocalBalanceOps, Limit);

expand(DbName) when is_binary(DbName); is_list(DbName) ->
    TargetNodes = surviving_nodes(),
    expand(DbName, TargetNodes, []).

%% @doc Computes a plan to balance the shards across the target nodes.
-spec expand(DbName::iolist(), [node()], [{atom(), #shard{}, node()}]) ->
        [{atom(), #shard{}, node()}].
expand(DbName, Nodes, PrevMoves) ->
    Shards = mem3:shards(DbName),
    Floor = length(Shards) div length(Nodes),
    % Ensure every target node reaches the floor
    {NewShards, Moves0} = rebalance2(Floor, Shards, Nodes, Nodes, PrevMoves),
    % Now look for any nodes with more than floor+1 shards
    {_, Moves} = rebalance2(Floor+1, NewShards, Nodes, Nodes, Moves0),
    Moves.

%% @equiv contract(1000)
-spec contract() -> [{atom(), #shard{}, node()}].
contract() ->
    contract(1000).

%% @doc Computes a plan to remove up to Limit shards from nodes in "decom" zone.
-spec contract(integer()) -> [{atom(), #shard{}, node()}].
contract(Limit) when is_integer(Limit), Limit > 0 ->
    TargetNodes = surviving_nodes(),
    apply_to_cluster(fun(Db, Moves) -> contract(Db, TargetNodes, Moves) end, Limit);

contract(DbName) when is_binary(DbName); is_list(DbName) ->
    TargetNodes = surviving_nodes(),
    contract(DbName, TargetNodes, []).

%% @doc Computes a plan to consolidate shards from a single database onto the
%%      supplied set of nodes.
-spec contract(DbName::iolist(), [node()], [{atom(), #shard{}, node()}]) ->
        [{atom(), #shard{}, node()}].
contract(DbName, TargetNodes, PrevMoves) ->
    {OK, MoveThese} = lists:partition(fun(#shard{node=Node}) ->
        lists:member(Node, TargetNodes)
    end, mem3:shards(DbName)),
    find_homes(MoveThese, shards_by_node(OK, TargetNodes), PrevMoves).

%% @equiv fix_zoning(1000)
-spec fix_zoning() -> [{atom(), #shard{}, node()}].
fix_zoning() ->
    fix_zoning(1000).

%% @doc Computes a plan containg up to Limit operations to repair replica
%%      levels and improper zoning.
-spec fix_zoning(integer()) -> [{atom(), #shard{}, node()}].
fix_zoning(Limit) when is_integer(Limit), Limit > 0 ->
    apply_to_cluster(fun fix_zoning/2, Limit);

fix_zoning(DbName) when is_binary(DbName); is_list(DbName) ->
    fix_zoning(DbName, []).

%% @doc Computes a plan to repair replica levels and improper zoning for a
%%      single database.
-spec fix_zoning(DbName::iolist(), [{atom(), #shard{}, node()}]) ->
        [{atom(), #shard{}, node()}].
fix_zoning(DbName, PrevMoves) ->
    IdealZoning = orddict:from_list(mem3:get_placement([])),
    ByRange = shards_by_range(mem3:shards(DbName)),
    orddict:fold(fun(_Range, Shards, Acc) ->
        compute_moves(IdealZoning, computed_zoning(Shards), Shards, Acc)
    end, PrevMoves, ByRange).

%% Internal functions.

global_expand(TargetNodes0, LocalOps, Limit) ->
    TargetNodes = [couch_util:to_binary(Node) || Node <- TargetNodes0],
    CountByNode = lists:filter(fun({Node, _Count}) ->
        lists:member(Node, TargetNodes)
    end, shard_count_by_node(LocalOps)),
    TotalCount = lists:foldl(fun({_, C}, Sum) -> Sum + C end, 0, CountByNode),
    TargetLevel = (TotalCount div length(TargetNodes)) + 1,
    FoldFun = fun
        (_, Acc) when length(Acc) >= Limit ->
            % We've already accumulated the max number of shard ops.
            Acc;
        ({_Node, Count}, Acc) when Count =< TargetLevel ->
            % This node is not a donor.
            Acc;
        ({Node0, Count}, Acc) ->
            Node = list_to_existing_atom(binary_to_list(Node0)),
            InternalAcc0 = #gacc{
                node = Node,
                targets = TargetNodes0,
                moves = Acc,
                limit = erlang:min(Count - TargetLevel, Limit - length(Acc)),
                target_level = TargetLevel
            },
            try mem3_shards:fold(fun donate_fold/2, InternalAcc0) of
                #gacc{moves = Moves} ->
                    Moves
            catch
                {complete, Moves} ->
                    Moves
            end
    end,
    lists:foldl(FoldFun, LocalOps, CountByNode).

donate_fold(_Shard, #gacc{limit = 0, moves = Moves}) ->
    throw({complete, Moves});
donate_fold(#shard{node = Node} = Shard, #gacc{node = Node} = Acc0) ->
     #gacc{
        targets = Nodes,
        moves = Moves,
        limit = DC,
        target_level = TargetLevel
    } = Acc0,
    Zone = mem3:node_info(Node, <<"zone">>),
    Shards = apply_shard_moves(mem3:shards(Shard#shard.dbname), Moves),
    InZone = filter_map_by_zone(shards_by_node(Shards, Nodes), Zone),
    SortedByCount = lists:sort(smallest_first(Moves), InZone),
    Candidates = lists:dropwhile(fun({_Node, OwnShards}) ->
        lists:keymember(Shard#shard.range, #shard.range, OwnShards)
    end, SortedByCount),
    case {lists:member(Shard, Shards), Candidates} of
        {false, _} ->
            Acc0;
        {true, []} ->
            Acc0;
        {true, [{Node, _} | _]} ->
            Acc0;
        {true, [{Target, _} | _]} ->
            % Execute the move only if the target has fewer shards for this DB
            % than the source. Otherwise we'd generate a local imbalance.
            SourceCount = get_shard_count(Node, SortedByCount),
            TargetCount = get_shard_count(Target, SortedByCount),
            % Execute the move only if the target needs shards.
            NodeKey = couch_util:to_binary(Target),
            Total = couch_util:get_value(NodeKey, shard_count_by_node(Moves)),
            if (TargetCount < SourceCount), (Total < TargetLevel) ->
                print({move, Shard, Target}),
                Acc0#gacc{
                    moves = [{move, Shard, Target} | Moves],
                    limit = DC - 1
                };
            true ->
                Acc0
            end
    end;
donate_fold(_Shard, Acc) ->
    Acc.

get_shard_count(AtomKey, ShardsByNode) when is_atom(AtomKey) ->
    length(couch_util:get_value(AtomKey, ShardsByNode, [])).

compute_moves(IdealZoning, IdealZoning, _Copies, OtherMoves) ->
    OtherMoves;
compute_moves(IdealZoning, ActualZoning, Copies, OtherMoves) ->
    {Donor, Recipient} = find_donor_and_recipient(IdealZoning, ActualZoning),
    pair_up(Donor, Recipient, Copies, OtherMoves).

find_donor_and_recipient(IdealZoning, ActualZoning) ->
    lists:foldl(fun({Zone, IdealCopies}, {D,R}) ->
        case couch_util:get_value(Zone, ActualZoning, 0) of
            Actual when Actual < IdealCopies ->
                {D, Zone};
            Actual when Actual > IdealCopies ->
                {Zone, R};
            _ ->
                {D, R}
        end
    end, {nil, nil}, IdealZoning).

pair_up(_, nil, _Copies, Moves) ->
    Moves;
pair_up(nil, Recipient, Copies, Moves) ->
    % We've got an insufficient replica level -- a recipient but no donor
    Candidate = hd(Copies),
    TargetNode = choose_node_in_target_zone(Candidate, Recipient, Moves),
    print({copy, Candidate, TargetNode}),
    [{copy, Candidate, TargetNode}|Moves];
pair_up(Donor, Recipient, Copies, Moves) ->
    Candidate = hd(lists:filter(fun(#shard{node = Node}) ->
        mem3:node_info(Node, <<"zone">>) =:= Donor
    end, Copies)),
    TargetNode = choose_node_in_target_zone(Candidate, Recipient, Moves),
    print({move, Candidate, TargetNode}),
    [{move, Candidate, TargetNode}|Moves].

choose_node_in_target_zone(#shard{dbname = DbName} = Candidate, Take, Moves) ->
    TargetNodes = allowed_nodes(fun(Zone) -> Zone =:= Take end),
    CurrentShards = apply_shard_moves(mem3:shards(DbName), Moves),
    ByTargetNode = shards_by_node(CurrentShards, TargetNodes),
    InZone = filter_map_by_zone(ByTargetNode, Take),
    {TargetNode, _} = find_home(Candidate, InZone, Moves),
    TargetNode.

-spec find_homes([#shard{}], [{node(), [#shard{}]}], [{atom(), #shard{}, node()}]) ->
        [{atom(), #shard{}, node()}].
find_homes([], _ShardsByTargetNode, Result) ->
    Result;
find_homes([#shard{node = Node0} = Shard | Rest], ShardsByNode, PrevMoves) ->
    InZone = filter_map_by_zone(ShardsByNode, mem3:node_info(Node0, <<"zone">>)),
    {TargetNode, NewMap} = find_home(Shard, InZone, PrevMoves),
    print({move, Shard, TargetNode}),
    MergedMap = orddict:merge(fun(_, V1, _) -> V1 end, NewMap, ShardsByNode),
    find_homes(Rest, MergedMap, [{move, Shard, TargetNode} | PrevMoves]).

find_home(Shard, ShardsByNode, PrevMoves) ->
    SortedByCount = lists:sort(smallest_first(PrevMoves), ShardsByNode),
    % Ensure that the target node is not already an owner of this range
    [{TargetNode, _} | _] = lists:dropwhile(fun({_Node, Shards}) ->
        lists:keymember(Shard#shard.range, #shard.range, Shards)
    end, SortedByCount),
    NewMap = orddict:append(TargetNode, Shard#shard{node=TargetNode}, ShardsByNode),
    {TargetNode, NewMap}.

rebalance2(_TargetLevel, Shards, _Nodes, [], Moves) ->
    {Shards, Moves};
rebalance2(TargetLevel, Shards, Nodes, [Node | Rest], Moves) ->
    ShardsForNode = [S || S <- Shards, S#shard.node =:= Node],
    CurrentLevel = length(ShardsForNode),
    case CurrentLevel < TargetLevel of
        true ->
            case victim(TargetLevel, Shards, Nodes, Node, Moves) of
                {ok, Victim} ->
                    print({move, Victim, Node}),
                    rebalance2(TargetLevel,
                             replace(Victim, Victim#shard{node=Node}, Shards),
                             Nodes, [Node|Rest], [{move, Victim, Node}|Moves]);
                false ->
                    rebalance2(TargetLevel, Shards, Nodes, Rest, Moves)
            end;
        false ->
            rebalance2(TargetLevel, Shards, Nodes, Rest, Moves)
    end.

victim(TargetLevel, Shards, Nodes, TargetNode, Moves) ->
    % Build a map of shards owned by nodes in the target zone.
    TargetZone = mem3:node_info(TargetNode, <<"zone">>),
    ShardsByNode0 = filter_map_by_zone(shards_by_node(Shards, Nodes), TargetZone),
    % Filter nodes that would drop below target level (including TargetNode).
    ShardsByNode1 = [{N, SS} || {N, SS} <- ShardsByNode0, length(SS) > TargetLevel],
    % Prefer to take from a node with more shards than others.
    ShardsByNode2 = lists:sort(largest_first(Moves), ShardsByNode1),
    % Don't take a shard for a range already hosted by the target.
    TargetRanges = [S#shard.range || S <- Shards, S#shard.node =:= TargetNode],
    ShardsByNode3 = lists:map(fun({N, SS}) ->
        {N, [S || S <- SS, not lists:member(S#shard.range, TargetRanges)]}
    end, ShardsByNode2),
    % Find the first node that still owns a candidate shard.
    case lists:dropwhile(fun({_, SS}) -> SS =:= [] end, ShardsByNode3) of
        [] ->
            false;
        [{_SourceNode, [Victim | _OtherShards]} | _] ->
            {ok, Victim}
    end.

largest_first(PrevMoves) ->
    % use the global shard count on each node to break the tie
    Global = shard_count_by_node(PrevMoves),
    fun(A, B) -> sort_by_count(A, B, Global) >= 0 end.

smallest_first(PrevMoves) ->
    % use the global shard count on each node to break the tie
    Global = shard_count_by_node(PrevMoves),
    fun(A, B) -> sort_by_count(A, B, Global) =< 0 end.

sort_by_count({NodeA, SA}, {NodeB, SB}, Global) when length(SA) =:= length(SB) ->
    CountA = couch_util:get_value(couch_util:to_binary(NodeA), Global, 0),
    CountB = couch_util:get_value(couch_util:to_binary(NodeB), Global, 0),
    cmp(CountA, CountB);
sort_by_count({_, A}, {_, B}, _) ->
    cmp(length(A), length(B)).

cmp(A, B) when A < B ->
    -1;
cmp(A, B) when A > B ->
    1;
cmp(_, _) ->
    0.

replace(A, B, List) ->
    replace(A, B, List, []).

replace(_A, _B, [], Acc) ->
    Acc;
replace(A, B, [A | Rest], Acc) ->
    replace(A, B, Rest, [B | Acc]);
replace(A, B, [C | Rest], Acc) ->
    replace(A, B, Rest, [C | Acc]).

%% @doc Takes a list of copy/move operations and applies them to the current
%%      set of shards.  Any moves that reference a shard not in the current set
%%      will be ignored.
apply_shard_moves(Shards, []) ->
    Shards;
apply_shard_moves(Shards, [{move, Shard, Node}| Rest]) ->
    NewShards = replace(Shard, Shard#shard{node = Node}, Shards, []),
    apply_shard_moves(NewShards, Rest);
apply_shard_moves(Shards, [{copy, Shard, Node}| Rest]) ->
    case lists:member(Shard, Shards) of
        true ->
            apply_shard_moves([Shard#shard{node = Node} | Shards], Rest);
        false ->
            apply_shard_moves(Shards, Rest)
    end.

allowed_nodes(Fun) ->
    lists:filter(fun(Node) ->
        Fun(mem3:node_info(Node, <<"zone">>))
    end, mem3:nodes()).

surviving_nodes() ->
    lists:filter(fun(Node) ->
        mem3:node_info(Node, <<"decom">>) =/= true
    end, mem3:nodes()).

shards_by_node(Shards, Nodes) ->
    % Ensure every target node is present in the orddict
    ShardsByNode0 = orddict:from_list([{N,[]} || N <- Nodes]),
    lists:foldl(fun(#shard{node = Node} = Shard, Acc) ->
        orddict:append(Node, Shard, Acc)
    end, ShardsByNode0, Shards).

filter_map_by_zone(ShardsByNode, Zone) ->
    Result = orddict:filter(fun(Node, _Shards) ->
        mem3:node_info(Node, <<"zone">>) =:= Zone
    end, ShardsByNode),
    if Result =:= [] ->
        erlang:error({empty_zone, Zone});
    true ->
        Result
    end.

shards_by_range(Shards) ->
    lists:foldl(fun(#shard{range = Range} = Shard, OD) ->
        orddict:append(Range, Shard, OD)
    end, orddict:new(), Shards).

computed_zoning(Shards) ->
    lists:foldl(fun(#shard{node = Node}, OD) ->
        orddict:update_counter(mem3:node_info(Node, <<"zone">>), 1, OD)
    end, orddict:new(), Shards).

shard_count_by_node(PrevMoves) ->
    Map0 = case erlang:get(shard_count_by_node) of
        undefined ->
            try shard_count_view() catch _:_ -> [] end;
        {T0, Map} ->
            case timer:now_diff(os:timestamp(), T0) div 1000 of
                Delta when Delta < 5000 ->
                    Map;
                _Else ->
                    try shard_count_view() catch _:_ -> [] end
            end
    end,
    % Incorporate the operations we've already scheduled into the total counts
    lists:foldl(fun
        ({copy, _, TargetNode}, OD0) ->
            orddict:update_counter(couch_util:to_binary(TargetNode), 1, OD0);
        ({move, #shard{node = SourceNode}, TargetNode}, OD0) ->
            OD1 = orddict:update_counter(couch_util:to_binary(SourceNode), -1, OD0),
            orddict:update_counter(couch_util:to_binary(TargetNode), 1, OD1)
    end, orddict:from_list(Map0), PrevMoves).

shard_count_view() ->
    %% TODO rewrite CouchDB's internal view API.  Wow!
    {ok, Db} = couch_db:open(<<"dbs">>, []),
    {ok, DDoc} = couch_db:open_doc(Db, <<"_design/rebalance">>, []),
    Group0 = couch_view_group:design_doc_to_view_group(DDoc),
    {ok, Pid} = gen_server:call(couch_view, {get_group_server, <<"dbs">>, Group0}),
    {ok, Group} = couch_view_group:request_group(Pid, 0),
    Lang = couch_view_group:get_language(Group),
    Views = couch_view_group:get_views(Group),
    Ref = erlang:monitor(process, couch_view_group:get_fd(Group)),
    {IRed, View} = fabric_view:extract_view(Pid, <<"count_by_node">>, Views, reduce),
    ReduceView = {reduce, IRed, Lang, View},
    Options = [{key_group_level, exact}],
    Fold = fun(Node, Count, Acc) -> {ok, [{Node, Count} | Acc]} end,
    %% Workaround for problems where we hold onto bad collators in the shell
    erlang:erase(couch_drv_port),
    {ok, Map} = couch_view:fold_reduce(ReduceView, Fold, [], Options),
    erlang:put(shard_count_by_node, {os:timestamp(), Map}),
    erlang:demonitor(Ref),
    Map.

print({Op, Shard, TargetNode} = Operation) ->
    {match, [SourceId, Cluster]} = re:run(
        atom_to_list(Shard#shard.node),
        "dbcore@db(?<node>[0-9]+)\.(?<cluster>[a-z0-9]+)\.cloudant.net",
        [{capture, all_but_first, binary}]
    ),
    {match, [TargetId, Cluster]} = re:run(
        atom_to_list(TargetNode),
        "dbcore@db(?<node>[0-9]+)\.(?<cluster>[a-z0-9]+)\.cloudant.net",
        [{capture, all_but_first, binary}]
    ),
    {match, [Range, Account, DbName]} = re:run(
        Shard#shard.name,
        "shards/(?<range>[0-9a-f\-]+)/(?<account>.+)/(?<dbname>[a-z][a-z0-9\\_\\$()\\+\\-\\/]+)\.[0-9]{8}",
        [{capture, all_but_first, binary}]
    ),
    OpName = case Op of move -> move2; _ -> Op end,
    io:format("clou shard ~s ~s ~s ~s ~s ~s ~s~n", [OpName, Cluster, Account, DbName,
         Range, SourceId, TargetId]),
    Operation;

print(Operations) when is_list(Operations) ->
    [print(Operation) || Operation <- Operations].

apply_to_cluster(UserFun, Limit) ->
    try mem3_shards:fold(cluster_fold_fun(UserFun, Limit), {nil, []}) of
        {_LastDb, Moves} ->
            Moves
    catch
        {complete, Moves} ->
            Moves
    end.

cluster_fold_fun(UserFun, Limit) ->
    fun
        (#shard{dbname = DbName}, {DbName, PrevMoves}) ->
            {DbName, PrevMoves};
        (#shard{dbname = DbName}, {_PrevName, PrevMoves}) ->
            Moves = UserFun(DbName, PrevMoves),
            check_limit(Moves, Limit),
            {DbName, Moves}
    end.

check_limit(Moves, Limit) when length(Moves) >= Limit ->
    throw({complete, Moves});
check_limit(_, _) ->
    ok.
