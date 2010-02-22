-module(mem_utils).

-export([fix_mappings/3, get_remote_fullmap/1, join_type/3, pmap_from_full/1,
         nodeparts_up/1, remove_partition/3, use_persistent/2,
         was_i_nodedown/2]).

-include("../include/common.hrl").

join_type(Node, Fullmap, Options) ->
  case proplists:get_value(replace, Options) of
  undefined ->
      case lists:filter(fun({N,_P,_T}) -> N =:= Node end, Fullmap) of
      [] -> new;
      _ -> rejoin
      end;
  OldNode when is_atom(OldNode) ->
      % not a particularly strong guard, but will have to do
      {replace, OldNode};
  _ -> new
  end.


%% @doc return a {PMap, Fullmap} tuple that has corrections for
%%      down, rejoining, or replacing Node
fix_mappings(nodedown, Node, OldFullmap) ->
  fix_mappings_fold(fun({N,P,T}, AccIn) ->
    case {N,T} of
    {Node, {nodedown, Type}} ->
        % already marked as nodedown, so leave it
        [{N,P, {nodedown, Type}} | AccIn];
    {Node, _} ->
        % mark it as nodedown
        [{N,P, {nodedown, T}} | AccIn];
    _ -> [{N,P,T} | AccIn]
    end
  end, [], OldFullmap);

fix_mappings(rejoin, Node, OldFullmap) ->
  fix_mappings_fold(fun({N,P,{nodedown,T}}, AccIn) when N =:= Node ->
                        [{N,P,T} | AccIn];
                       (NPT, AccIn) -> [NPT | AccIn]
  end, [], OldFullmap);

fix_mappings(replace, {OldNode, NewNode}, OldFullmap) ->
  fix_mappings_fold(fun({N,P,T}, AccIn) ->
    case {N, T} of
      {OldNode, {nodedown,T1}} -> [{NewNode,P,T1} | AccIn];
      {OldNode, _} -> [{NewNode,P,T} | AccIn];
      _ -> [{N,P,T} | AccIn]
    end
  end, [], OldFullmap).


fix_mappings_fold(Fun, Acc0, OldFullmap) ->
  NewFullmap = lists:foldl(Fun, Acc0, OldFullmap),
  NewPMap = pmap_from_full(NewFullmap),
  {NewPMap, NewFullmap}.


%% @doc create a PMap (primary nodes only) from provided Fullmap
%%      If a primary node is down, a partner will be supplied
pmap_from_full(Fullmap) ->
  NodePartList = nodeparts_up(Fullmap),
  lists:keysort(2,lists:foldl(fun({N,P,T}, AccIn) ->
    case T of
      primary -> [{N,P} | AccIn];
      {nodedown, primary} ->
        NewNode = case lists:delete(N,
            membership2:nodes_for_part(P, NodePartList)) of
        [First|_] -> First;
        [] -> N  % wtf, are all partners down too?
        end,
        [{NewNode,P} | AccIn];
      _ -> AccIn
    end
  end, [], Fullmap)).


nodeparts_up(Fullmap) ->
  lists:foldl(fun({_N,_P,{nodedown,_}}, AccIn) -> AccIn;
                 ({N,P,_T}, AccIn) -> [{N,P} | AccIn]
              end, [], Fullmap).



%% @doc if Node is in the Fullmap as {nodedown,_} return true
was_i_nodedown(Node, Fullmap) ->
  lists:member(yes, lists:map(fun({N,_P,{nodedown,_T}}) ->
                                  case N of
                                  Node -> yes;
                                  _ -> no
                                  end;
                                 (_) -> no
                              end, Fullmap)).


remove_partition(FullMap, Node, Partition) ->
  case lists:filter(
         fun({N,P,_Type}) -> N =:= Node andalso P =:= Partition end,
         FullMap) of
  [Elem|_] ->
      lists:delete(Elem, FullMap);
  Other ->
      ?LOG_ERROR("~nNo partition to remove: ~p~n"
                 "Node: ~p~nPartition: ~p~n", [Other, Node, Partition]),
      FullMap
  end.


use_persistent(_PartnersPlus, undefined) ->
  false;

use_persistent(PartnersPlus, _PersistentParts) ->
  % get a fullmap from a partner
  % this may need rework for network partitions, as you could get a bad
  % fullmap from another node that was partitioned w/ this one :\
  RemoteFullmap = get_remote_fullmap(PartnersPlus),
  % return opposite of was_i_nodedown
  not mem_utils:was_i_nodedown(node(), RemoteFullmap).


get_remote_fullmap([]) ->
  []; % no remote fullmap available, so return empty list

get_remote_fullmap([Node|Rest]) ->
  case gen_server:call({membership, Node}, fullmap) of
  {ok, Fullmap} -> Fullmap;
  _ -> get_remote_fullmap(Rest)
  end.
