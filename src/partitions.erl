-module(partitions).
-author('brad@cloudant.com').

%% API
-export([fullmap/2, fullmap/3, hash/1, install_fullmap/4]).
-export([for_key/2, all_parts/1]).
-export([shard_name/2]).

-define(RINGTOP, trunc(math:pow(2,160))).  % SHA-1 space

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc build a full partition map
fullmap(DbName, Options) ->
    {ok, Nodes} = mem3:nodes(),
    fullmap(DbName, Nodes, Options).

fullmap(DbName, Nodes, Options) ->
    {N,Q} = db_init_constants(Options),
    NewNodes = ordered_nodes(DbName, Nodes),
    Pmap = pmap(Q, NewNodes),
    int_fullmap(DbName, N, Pmap, NewNodes).

%% @spec hash(term()) -> Digest::binary()
%% @doc uses SHA-1 as its hash
hash(Item) when is_binary(Item) ->
    crypto:sha(Item);
hash(Item) ->
    crypto:sha(term_to_binary(Item)).

install_fullmap(DbName, Fullmap, FullNodes, Options) ->
    {N,Q} = db_init_constants(Options),
    Doc = {[{<<"_id">>,DbName},
            {map, jsonify(map, Fullmap)},
            {nodes, jsonify(nodes, FullNodes)},
            {n,N},
            {q,Q}]},
    write_db_doc(Doc).

for_key(DbName, Key) ->
    <<HashKey:160/integer>> = hash(Key),
    Head = #shard{
        name = '_',
        node = '_',
        dbname = DbName,
        range = ['$1','$2'],
        ref = '_'
    },
    % TODO these conditions assume A < B, which we don't require
    Conditions = [{'<', '$1', HashKey}, {'<', HashKey, '$2'}],
    case ets:select(partitions, [{Head, Conditions, ['$_']}]) of
    [] ->
        erlang:error(database_does_not_exist);
    Shards ->
        Shards
    end.

all_parts(DbName) ->
    case ets:lookup(partitions, DbName) of
    [] ->
        erlang:error(database_does_not_exist);
    Else ->
        Else
    end.

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
    <<HashInt:160/integer>> = hash(DbName),
    Size = partition_range(length(Nodes)),
    Factor = (HashInt div Size),
    lists:nth(Factor+1, Nodes).

%% @doc take the list of nodes, and rearrange it, starting with the node that
%%      results from hashing the Term
ordered_nodes(Term, Nodes) ->
    SeedNode = seednode(Term, Nodes),
    {A, B} = lists:splitwith(fun(N) -> N /= SeedNode end, Nodes),
    lists:append(B,A).

%% @doc create a partition map
pmap(NumPartitions, Nodes) ->
    Increment = ?RINGTOP div NumPartitions,
    Parts = parts(?RINGTOP, Increment, 0, []),
    make_map(Nodes, Nodes, Parts, []).

%% @doc makes a {beg, end} list of partition ranges
%%      last range may have an extra few values, because Increment is created
%%      with Ringtop 'div' NumPartitions above.
parts(Top, _, Beg, Acc) when Beg > Top -> Acc;
parts(Top, Increment, Beg, Acc) ->
    End = case Beg + 2*Increment of
    Over when Over > Top -> Top;
    _ -> Beg + Increment - 1
    end,
    NewAcc = [{Beg, End} | Acc],
    parts(Top, Increment, End+1, NewAcc).

%% @doc create a full map, which is a pmap with N-1 replication partner nodes
%%      added per partition
int_fullmap(DbName, N, Pmap, Nodes) ->
    Full = lists:foldl(fun({Node,{B,E} = Part}, AccIn) ->
        Primary = [#shard{dbname=DbName, node=Node, range=[B,E],
                          name=shard_name(B,DbName)}],
        Partners = partners(DbName, N, Node, Nodes, Part),
        lists:append([Primary, Partners, AccIn])
    end, [], Pmap),
    lists:reverse(Full).

partners(DbName, N, Node, Nodes, {Beg,End}) ->
    {A, [Node|B]} = lists:splitwith(fun(Nd) -> Nd /= Node end, Nodes),
    Nodes1 = lists:append(B,A),
    Partners = lists:sublist(Nodes1, N-1), % N-1 replication partner nodes
    lists:map(fun(Partner) ->
        #shard{dbname=DbName, node=Partner, range=[Beg,End],
               name=shard_name(Beg,DbName)}
    end, Partners).

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

jsonify(map, Map) ->
    lists:map(fun(#shard{node=Node, range=[Beg,End]}) ->
        {[{node, Node}, {b, Beg}, {e, End}]}
    end, Map);
jsonify(nodes, Nodes) ->
    lists:map(fun({Order, Node, Options}) ->
        {[{order, Order}, {node, Node}, {options, Options}]}
    end, Nodes).

write_db_doc(EDoc) ->
    {ok, Db} = couch_db:open(<<"dbs">>, []),
    try
        update_db_doc(Db, couch_doc:from_json_obj(EDoc))
    catch {conflict, _} ->
        ?LOG_ERROR("conflict writing db doc, must be a race", [])
    after
        couch_db:close(Db)
    end.

update_db_doc(Db, #doc{id=Id, body=Body} = Doc) ->
    case couch_db:open_doc(Db, Id, []) of
    {not_found, _} ->
        {ok, _} = couch_db:update_doc(Db, Doc, []);
    {ok, #doc{body=Body}} ->
        ok;
    {ok, OldDoc} ->
        {ok, _} = couch_db:update_doc(Db, OldDoc#doc{body=Body}, [])
    end.

shard_name(Part, DbName) when is_list(DbName) ->
    shard_name(Part, ?l2b(DbName));
shard_name(Part, DbName) ->
  PartHex = ?l2b(showroom_utils:int_to_hexstr(Part)),
  <<"x", PartHex/binary, "/", DbName/binary, "_", PartHex/binary>>.
