%%%-------------------------------------------------------------------
%%% File:      partitions.erl
%%% @author    Cliff Moon <cliff@powerset.com> [http://www.powerset.com/]
%%% @copyright 2008 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2008-10-12 by Cliff Moon
%%%-------------------------------------------------------------------
-module(partitions).
-author('cliff@powerset.com').

%% API
-export([partition_range/1, create_partitions/3, map_partitions/2,
         diff/2, pp_diff/1, int_to_partition/2,
         join/3, leave/3, hash/1, hash_to_partition/2, item_to_nodepart/1,
         shard_name/2, hash_to_hex/2]).

-define(RINGTOP, trunc(math:pow(2,160)-1)).  % SHA-1 space

-include("../../couch/src/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

%% -ifdef(TEST).
%% -include("etest/partitions_test.erl").
%% -endif.

%%====================================================================
%% API
%%====================================================================

partition_range(Q) ->
  trunc( ?RINGTOP / math:pow(2,Q) ).  % SHA-1 space / 2^Q

create_partitions(Q, Node, _Nodes) ->
  fresh(trunc(math:pow(2,Q)), Node).
  % map_partitions(Table, Nodes).


%% @spec map_partitions(Table::proplist(),Nodes::list()) -> proplist()
%% @doc maps partitions to nodes.  The resulting list should be Dynomite format,
%%      namely {Node,Part}
%% @end
map_partitions(Table, Nodes) ->
  {_Nodes, Parts} = lists:unzip(Table),
  do_map(Nodes, Parts).


%% @doc in case Hints is undefined, turn it into a list for clauses below.
join(Node, Table, undefined) ->
  join(Node, Table, []);

%% @spec join(node(), proplist(), list()) -> {ok, PartTable::proplist()} |
%%       {error, Error}
%% @doc given a node, current partition table, and hints, this function returns
%%      the new partition table
join(Node, Table, Hints) ->
  {NodeList, Parts} = lists:unzip(Table),
  OtherNodes = lists:delete(Node, NodeList),
  OtherDistinctNodes = lists:usort(OtherNodes),
  %% quick check to see if we have more nodes than partitions
  if
    length(Parts) == length(OtherDistinctNodes) ->
      {error, "Too many nodes vs partitions", Table};
    true ->
      AlreadyPresent = length(NodeList) - length(OtherNodes),
      Nodes = lists:usort(NodeList),
      PartCountToTake = trunc(length(Parts) / (length(Nodes) + 1)),
      %% calcs done, let's steal some partitions
      {HintsTaken, NewTable} = steal_hints(Node, Table, Hints),
      if
        PartCountToTake - AlreadyPresent - HintsTaken > 0 ->
          steal_partitions(Node, OtherDistinctNodes, NewTable,
                           PartCountToTake - AlreadyPresent - HintsTaken);
        true ->
          %% no partitions to take
          {ok, NewTable}
      end
  end.


%% TODO: implement me
leave(_Node, Table, _Hints) ->
  Table.


diff(From, To) when length(From) =/= length(To) ->
  {error, badlength, "Cannot diff partition maps with different length"};

diff(From, To) ->
  diff(sort_for_diff(From), sort_for_diff(To), []).


pp_diff(Diff) ->
  lists:map(
    fun({F,T,Part}) -> {F,T,showroom_utils:int_to_hexstr(Part)} end,
    Diff).


%% @spec hash(term()) -> Digest::binary()
%% @doc Showroom uses SHA-1 as its hash
hash(Item) ->
  crypto:sha(term_to_binary(Item)).


%% @spec hash_to_partition(binary(), integer()) -> integer()
%% @doc given a hashed value and Q, return the partition
hash_to_partition(Hash, Q) ->
  HashInt = hash_int(Hash),
  Size = partition_range(Q),
  Factor = (HashInt div Size),
  Rem = (HashInt rem Size),
  if
    Rem > 0 -> Factor * Size;
    true -> ((Factor-1) * Size)
  end.


hash_to_hex(Hash, Q) ->
  Part = hash_to_partition(Hash, Q),
  showroom_utils:int_to_hexstr(Part).


%% @doc given an int and a list of partitions, get the first part greater
%%      than Int.  Used for a hex part being turned back into an int.
int_to_partition(Int, Parts) ->
  Rem = lists:dropwhile(fun(E) -> E < Int end, lists:sort(Parts)),
  case Rem of
    [] -> 0;  % wrap-around-ring case (back to 0)
    [H|_T] -> H
  end.


%% @spec item_to_nodepart(bin()) -> {Node::node(),Part::integer()}
%% @doc given a raw item, return the node/partition/shard
%%      name based on consistent hashing
item_to_nodepart(Item) when is_binary(Item) ->
  Q = list_to_integer(couch_config:get("cluster","q")),
  Hash = hash(?b2l(Item)),
  Part = hash_to_partition(Hash, Q),
  {ok, Table} = membership2:partitions(),
  lists:keyfind(Part, 2, Table);

item_to_nodepart(Item) ->
  item_to_nodepart(term_to_binary(Item)).


%% @spec shard_name(integer(), binary()) -> binary()
%% @doc create shard name
shard_name(Part, DbName) ->
  PartHex = ?l2b(showroom_utils:int_to_hexstr(Part)),
  <<"x", PartHex/binary, "/", DbName/binary, "_", PartHex/binary>>.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Create a brand new table.  The size and seednode are specified;
%%      initially all partitions are owned by the seednode.  If NumPartitions
%%      is not much larger than the intended eventual number of
%%       participating nodes, then performance will suffer.
%% from http://code.google.com/p/distributerl (trunk revision 4) chash:fresh/2
%% @spec fresh(NumPartitions :: integer(), SeedNode :: node()) -> table()
fresh(NumPartitions, SeedNode) ->
  Increment = ?RINGTOP div NumPartitions,
  [{SeedNode, IndexAsInt} || IndexAsInt <- lists:seq(0,(?RINGTOP-1),Increment)].


%% @spec steal_hints(node(), proplist(), list( integer() )) ->
%%       {integer(), proplist()}
%% @doc move the partitions listed in Hints over to the new owner, Node
steal_hints(Node, Table, Hints) ->
  steal_hints(Node, Table, Hints, 0).


%% @doc recursive workhorse for hints mechanism, Acc is tracking how many
%%      hints/partitions were successfully moved to a new Node.
%% @end
steal_hints(_Node, Table, [], Acc) ->
  {Acc, Table};

steal_hints(Node, Table, [Hint|RestHints], Acc) ->
  {Status, NewTable} = swap_node_for_part(Node, Hint, Table),
  Acc1 = case Status of
           ok -> Acc+1;
           _ -> Acc
         end,
  steal_hints(Node, NewTable, RestHints, Acc1).


%% @doc take a part from one of the other nodes based on most # of parts per
%%      node.
%% @end
%% TODO: This fun does list ops on the Table each time through.  Inefficient?
%%       Hopefully not, due to small Table sizes
steal_partitions(_Node, _OtherNodes, Table, 0) ->
  {ok, Table};
steal_partitions(Node, OtherNodes, Table, Count) ->
  %% first, get a list of OtherNodes and their partition counts
  NPCountFun = fun(N) ->
                   L = proplists:get_all_values(N, Table),
                   {N, length(lists:delete(undefined, L))}
               end,
  NPCounts = lists:reverse(lists:keysort(2,lists:map(NPCountFun, OtherNodes))),
  %% grab the node that has the most partitions
  [{TakeFrom, _PartsCount}|_RestOfTable] = NPCounts,
  %% get the highest # partition of the TakeFrom node
  TakeFromParts = lists:reverse(lists:sort(proplists:get_all_values(TakeFrom,
                                                                    Table))),
  [Part|_RestOfParts] = TakeFromParts,
  {ok, NewTable} = swap_node_for_part(Node, Part, Table),
  steal_partitions(Node, OtherNodes, NewTable, Count-1).


%% @doc Make Node the owner of the partition beginning at Part.
%% from http://code.google.com/p/distributerl (trunk revision 4) chash:update/3
swap_node_for_part(Node, Part, Table) ->
  case lists:keymember(Part, 2, Table) of
    true ->
      GapList = [{N,P} || {N,P} <- Table, P /= Part],
      {A, B} = lists:partition(fun({_,K1}) -> K1 < Part end, GapList),
      {ok, A ++ [{Node, Part}] ++ B};
    false ->
      showroom_log:message(info,
          "'~p' partition was not found in partition table", [Part]),
      {noswap, Table}
  end.


%% @doc get the difference between two FullPMaps
%%      lists need to be sorted by part, then node
diff([], [], Results) ->
  lists:reverse(remove_dupes(Results));

diff([{Node,Part,_}|PartsA], [{Node,Part,_}|PartsB], Results) ->
  diff(PartsA, PartsB, Results);

diff([{NodeA,Part,_}|PartsA], [{NodeB,Part,_}|PartsB], Results) ->
  diff(PartsA, PartsB, [{NodeA,NodeB,Part}|Results]).


%% @doc sorts the full map for diff/3.  This may change to get more accurate
%%      diff w/o dupes
sort_for_diff(FullMap) ->
  lists:keysort(2,lists:sort(FullMap)).


remove_dupes(Diff) ->
  {_,_,AllParts} = lists:unzip3(Diff),
  Parts = lists:usort(AllParts),
  remove_dupes_from_part(Parts, Diff, []).


%% @doc ex: take [{a,b,1},{b,c,1}] diff and make it [{a,c,1}] so we don't go
%%      moving unnecessary shard files.  'Move partition 1 from a to b and
%%      then move partition 1 from b to c' is unnecessary.  Just move it a to c.
remove_dupes_from_part([], _Diff, Acc) ->
  Acc;

remove_dupes_from_part([Part|Rest], Diff, Acc) ->
  PartData = lists:filter(fun({_,_,P}) -> P =:= Part end, Diff),
  NewPartData = process_part_data(Part, PartData, PartData, PartData),
  remove_dupes_from_part(Rest, Diff, lists:concat([NewPartData, Acc])).


%% for one partition of the full diff, remove the dupes
process_part_data(_Part, _PartData, [], Acc) ->
  Acc;

process_part_data(Part, PartData, [{From,To,_Part}|Rest], Acc) ->
  case proplists:lookup(To, PartData) of
  {To, NewTo, _Part} ->

      Remove1 = proplists:delete(To, PartData),
      Remove2 = proplists:delete(From, Remove1),
      NewPartData = [{From, NewTo, Part}|Remove2],
      %?debugFmt("~nFrom : ~p~nTo   : ~p~nNewTo: ~p~n"
      %          "Remove1: ~p~nRemove2: ~p~n"
      %          "NewPartData: ~p~n"
      %          , [From, To, NewTo, Remove1, Remove2, NewPartData]),
      process_part_data(Part, NewPartData, Rest, NewPartData);
  none ->
      process_part_data(Part, PartData, Rest, Acc)
  end.


% %% @doc from dynomite
% diff([], [], Results) ->
%   lists:reverse(Results);

% diff([{Node,Part}|PartsA], [{Node,Part}|PartsB], Results) ->
%   diff(PartsA, PartsB, Results);

% diff([{NodeA,Part}|PartsA], [{NodeB,Part}|PartsB], Results) ->
%   diff(PartsA, PartsB, [{NodeA,NodeB,Part}|Results]).


%% @doc does Node/Partition mapping based on Amazon Dynamo paper,
%%      section 6.2, strategy 3, more or less
%%      http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html
%% @end
do_map([Node|RestNodes], Parts) ->
  Max = length(Parts) / length([Node|RestNodes]),
  do_map(Node, RestNodes, Parts, [], 1, Max).


%% return final mapped list
do_map(_,_,[],Mapped, _, _) ->
  lists:keysort(1, Mapped);

%% finish off last node, Cnt & Max no longer needed
do_map(Node, [], [Part|RestParts], Mapped, _, _) ->
  do_map(Node, [], RestParts, [{Node, Part}|Mapped], 0,0);

%% workhorse clause, iterates through parts, until Cnt > Max, then advances to
%% next node, wash, rinse, repeat
do_map(Node, [NextNode|RestNodes], [Part|RestParts], Mapped, Cnt, Max) ->
  case Cnt > Max of
    true ->
      do_map(NextNode, RestNodes, RestParts, [{Node, Part}|Mapped],
             1, Max);
    false ->
      do_map(Node, [NextNode|RestNodes], RestParts, [{Node, Part}|Mapped],
             Cnt+1, Max)
  end.


%% TODO: other guards
hash_int(Hash) when is_binary(Hash) ->
  <<IndexAsInt:160/integer>> = Hash,
  IndexAsInt;
hash_int(Hash) when is_integer(Hash) ->
  Hash.
