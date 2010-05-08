-module(partitions).
-author('brad@cloudant.com').

%% API
-export([fullmap/3, hash/1]).

-define(RINGTOP, trunc(math:pow(2,160))).  % SHA-1 space

-include("../../couch/src/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc build a full partition map
fullmap(DbName, Nodes, Options) ->
    {N,Q} = db_init_constants(Options),
    NewNodes = ordered_nodes(DbName, Nodes),
    Pmap = pmap(Q, NewNodes),
    int_fullmap(N, Pmap, NewNodes).

%% @spec hash(term()) -> Digest::binary()
%% @doc uses SHA-1 as its hash
hash(Item) ->
  crypto:sha(term_to_binary(Item)).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc get cluster constants from options or config
db_init_constants(Options) ->
    {const(n, Options), const(q, Options)}.

%% @doc get individual constant
const(Const, Options) ->
    ListResult = case couch_util:get_value(Const, Options) of
    undefined -> couch_config:get("cluster", atom_to_list(Const));
    Val -> Val
    end,
    list_to_integer(ListResult).

%% @doc hash the dbname, and return the corresponding node for seeding a ring
seednode(DbName, Nodes) ->
    Hash = hash(DbName),
    HashInt = hash_int(Hash),
    Size = partition_range(length(Nodes)),
    Factor = (HashInt div Size),
    lists:nth(Factor+1, Nodes).

%% @doc take the list of nodes, and rearrange it, starting with the node that
%%      results from hashing the Term
ordered_nodes(Term, Nodes) ->
    SeedNode = seednode(Term, Nodes),
    {A, B} = lists:splitwith(fun(N) -> N /= SeedNode end, Nodes),
    lists:append(B,A).

%% @doc create a partition map [{node(),part{}|_}
pmap(NumPartitions, Nodes) ->
    Increment = ?RINGTOP div NumPartitions + 1,
    Parts = lists:seq(0,(?RINGTOP),Increment),
    make_map(Nodes, Nodes, Parts, []).

%% @doc create a full map, which is a pmap with N-1 replication partner nodes
%%      added per partition
int_fullmap(N, Pmap, Nodes) ->
    Full = lists:foldl(fun({Node,Part}, AccIn) ->
        Partners = partners(N, Node, Nodes, Part),
        lists:append([ [{Node,Part}], Partners, AccIn])
    end, [], Pmap),
    lists:reverse(Full).

partners(N, Node, Nodes, Part) ->
    {A, [Node|B]} = lists:splitwith(fun(Nd) -> Nd /= Node end, Nodes),
    Nodes1 = lists:append(B,A),
    Partners = lists:sublist(Nodes1, N-1), % N-1 replication partner nodes
    lists:map(fun(Partner) -> {Partner, Part} end, Partners).


%% @doc turn hash into an integer
hash_int(Hash) when is_binary(Hash) ->
    <<IndexAsInt:160/integer>> = Hash,
    IndexAsInt;
hash_int(Hash) when is_integer(Hash) ->
    Hash.

%% @doc size of one partition in the ring
partition_range(Q) ->
    trunc( ?RINGTOP / Q ).  % SHA-1 space / Q

%% @doc assign nodes to each of the partitions.  When you run out of nodes,
%%      start at the beginning of the node list again.
%%      The provided node list starts with the seed node (seednode fun)
make_map(_,_,[], Acc) ->
    lists:keysort(2,Acc);
make_map(AllNodes, [], Parts, Acc) ->
    % start back at beginning of node list
    make_map(AllNodes, AllNodes, Parts, Acc);
make_map(AllNodes, [Node|RestNodes], [Part|RestParts], Acc) ->
    % add a node/part combo to the Acc
    make_map(AllNodes, RestNodes, RestParts, [{Node,Part}|Acc]).
