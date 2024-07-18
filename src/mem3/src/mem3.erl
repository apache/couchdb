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

-module(mem3).

-export([
    start/0,
    stop/0,
    restart/0,
    nodes/0,
    node_info/2,
    shards/1, shards/2,
    choose_shards/2,
    n/1, n/2,
    dbname/1,
    ushards/1, ushards/2
]).
-export([get_shard/3, local_shards/1, shard_suffix/1, fold_shards/2]).
-export([fold_dbs/2, fold_dbs/3]).
-export([sync_security/0, sync_security/1]).
-export([compare_nodelists/0, compare_shards/1]).
-export([quorum/1, group_by_proximity/1]).
-export([live_shards/2]).
-export([belongs/2, owner/3]).
-export([get_placement/1]).
-export([ping/1, ping/2]).
-export([ping_nodes/0, ping_nodes/1, ping_nodes/2]).
-export([dead_nodes/0, dead_nodes/1]).
-export([db_is_current/1]).
-export([shard_creation_time/1]).
-export([generate_shard_suffix/0]).

%% For mem3 use only.
-export([name/1, node/1, range/1, engine/1]).

-include_lib("mem3/include/mem3.hrl").

-define(PING_TIMEOUT_IN_MS, 60000).

start() ->
    application:start(mem3).

stop() ->
    application:stop(mem3).

restart() ->
    stop(),
    start().

%% @doc Detailed report of cluster-wide membership state.  Queries the state
%%      on all member nodes and builds a dictionary with unique states as the
%%      key and the nodes holding that state as the value.  Also reports member
%%      nodes which fail to respond and nodes which are connected but are not
%%      cluster members.  Useful for debugging.
-spec compare_nodelists() ->
    [
        {
            {cluster_nodes, [node()]}
            | bad_nodes
            | non_member_nodes,
            [node()]
        }
    ].
compare_nodelists() ->
    Nodes = mem3:nodes(),
    AllNodes = erlang:nodes([this, visible]),
    {Replies, BadNodes} = gen_server:multi_call(Nodes, mem3_nodes, get_nodelist),
    Dict = lists:foldl(
        fun({Node, Nodelist}, D) ->
            orddict:append({cluster_nodes, Nodelist}, Node, D)
        end,
        orddict:new(),
        Replies
    ),
    [{non_member_nodes, AllNodes -- Nodes}, {bad_nodes, BadNodes} | Dict].

-spec compare_shards(DbName :: iodata()) -> [{bad_nodes | [#shard{}], [node()]}].
compare_shards(DbName) when is_list(DbName) ->
    compare_shards(list_to_binary(DbName));
compare_shards(DbName) ->
    Nodes = mem3:nodes(),
    {Replies, BadNodes} = rpc:multicall(mem3, shards, [DbName]),
    GoodNodes = [N || N <- Nodes, not lists:member(N, BadNodes)],
    Dict = lists:foldl(
        fun({Shards, Node}, D) ->
            orddict:append(Shards, Node, D)
        end,
        orddict:new(),
        lists:zip(Replies, GoodNodes)
    ),
    [{bad_nodes, BadNodes} | Dict].

-spec n(DbName :: iodata()) -> integer().
n(DbName) ->
    % Use _design to avoid issues with
    % partition validation
    n(DbName, <<"_design/foo">>).

n(DbName, DocId) ->
    length(mem3:shards(DbName, DocId)).

-spec nodes() -> [node()].
nodes() ->
    mem3_nodes:get_nodelist().

node_info(Node, Key) ->
    mem3_nodes:get_node_info(Node, Key).

-spec shards(DbName :: iodata()) -> [#shard{}].
shards(DbName) ->
    shards_int(DbName, []).

shards_int(DbName, Options) when is_list(DbName) ->
    shards_int(list_to_binary(DbName), Options);
shards_int(DbName, Options) ->
    Ordered = lists:member(ordered, Options),
    ShardDbName =
        list_to_binary(config:get("mem3", "shards_db", "_dbs")),
    case DbName of
        ShardDbName when Ordered ->
            %% shard_db is treated as a single sharded db to support calls to db_info
            %% and view_all_docs
            [
                #ordered_shard{
                    node = config:node_name(),
                    name = ShardDbName,
                    dbname = ShardDbName,
                    range = [0, (2 bsl 31) - 1],
                    order = undefined,
                    opts = []
                }
            ];
        ShardDbName ->
            %% shard_db is treated as a single sharded db to support calls to db_info
            %% and view_all_docs
            [
                #shard{
                    node = config:node_name(),
                    name = ShardDbName,
                    dbname = ShardDbName,
                    range = [0, (2 bsl 31) - 1],
                    opts = []
                }
            ];
        _ ->
            mem3_shards:for_db(DbName, Options)
    end.

-spec shards(DbName :: iodata(), DocId :: binary()) -> [#shard{}].
shards(DbName, DocId) ->
    shards_int(DbName, DocId, []).

shards_int(DbName, DocId, Options) when is_list(DbName) ->
    shards_int(list_to_binary(DbName), DocId, Options);
shards_int(DbName, DocId, Options) when is_list(DocId) ->
    shards_int(DbName, list_to_binary(DocId), Options);
shards_int(DbName, DocId, Options) ->
    mem3_shards:for_docid(DbName, DocId, Options).

-spec ushards(DbName :: iodata()) -> [#shard{}].
ushards(DbName) ->
    Nodes = [node() | erlang:nodes()],
    ZoneMap = zone_map(Nodes),
    Shards = ushards(DbName, live_shards(DbName, Nodes, [ordered]), ZoneMap),
    mem3_util:downcast(Shards).

-spec ushards(DbName :: iodata(), DocId :: binary()) -> [#shard{}].
ushards(DbName, DocId) ->
    Shards = shards_int(DbName, DocId, [ordered]),
    Shard = hd(Shards),
    mem3_util:downcast([Shard]).

ushards(DbName, Shards0, ZoneMap) ->
    {L, S, D} = group_by_proximity(Shards0, ZoneMap),
    % Prefer shards in the local zone over shards in a different zone,
    % but sort each zone separately to ensure a consistent choice between
    % nodes in the same zone.
    Shards = choose_ushards(DbName, L ++ S) ++ choose_ushards(DbName, D),
    OverlappedShards = lists:ukeysort(#shard.range, Shards),
    mem3_util:non_overlapping_shards(OverlappedShards).

get_shard(DbName, Node, Range) ->
    mem3_shards:get(DbName, Node, Range).

local_shards(DbName) ->
    mem3_shards:local(DbName).

shard_suffix(DbName0) when is_binary(DbName0) ->
    Shard = hd(shards(DbName0)),
    <<"shards/", _:8/binary, "-", _:8/binary, "/", DbName/binary>> =
        Shard#shard.name,
    filename:extension(binary_to_list(DbName));
shard_suffix(Db) ->
    shard_suffix(couch_db:name(Db)).

shard_creation_time(DbName0) ->
    Shard = hd(shards(DbName0)),
    case Shard#shard.name of
        <<"shards/", _:8/binary, "-", _:8/binary, "/", DbName/binary>> ->
            case filename:extension(DbName) of
                <<".", Time/binary>> ->
                    Time;
                _ ->
                    <<"0">>
            end;
        _ ->
            <<"0">>
    end.

fold_shards(Fun, Acc) ->
    mem3_shards:fold(Fun, Acc).

fold_dbs(Fun, Acc) when is_function(Fun, 2) ->
    mem3_shards:fold_dbs(<<>>, Fun, Acc).

fold_dbs(<<Prefix/binary>>, Fun, Acc) when is_function(Fun, 2) ->
    mem3_shards:fold_dbs(Prefix, Fun, Acc).

sync_security() ->
    mem3_sync_security:go().

sync_security(Db) ->
    mem3_sync_security:go(dbname(Db)).

-spec choose_shards(DbName :: iodata(), Options :: list()) -> [#shard{}].
choose_shards(DbName, Options) when is_list(DbName) ->
    choose_shards(list_to_binary(DbName), Options);
choose_shards(DbName, Options) ->
    try
        shards(DbName)
    catch
        error:E when E == database_does_not_exist; E == badarg ->
            Nodes = allowed_nodes(),
            case get_placement(Options) of
                undefined ->
                    choose_shards(DbName, Nodes, Options);
                Placement ->
                    lists:flatmap(
                        fun({Zone, N}) ->
                            NodesInZone = nodes_in_zone(Nodes, Zone),
                            Options1 = lists:keymerge(1, [{n, N}], Options),
                            choose_shards(DbName, NodesInZone, Options1)
                        end,
                        Placement
                    )
            end
    end.

choose_shards(DbName, Nodes, Options) ->
    NodeCount = length(Nodes),
    Suffix = couch_util:get_value(shard_suffix, Options, ""),
    N = mem3_util:n_val(couch_util:get_value(n, Options), NodeCount),
    if
        N =:= 0 -> erlang:error(no_nodes_in_zone);
        true -> ok
    end,
    Q = mem3_util:q_val(
        couch_util:get_value(
            q,
            Options,
            config:get_integer("cluster", "q", 2)
        )
    ),
    %% rotate to a random entry in the nodelist for even distribution
    RotatedNodes = rotate_rand(Nodes),
    mem3_util:create_partition_map(DbName, N, Q, RotatedNodes, Suffix).

rotate_rand(Nodes) ->
    {A, B} = lists:split(couch_rand:uniform(length(Nodes)), Nodes),
    B ++ A.

get_placement(Options) ->
    case couch_util:get_value(placement, Options) of
        undefined ->
            case config:get("cluster", "placement") of
                undefined ->
                    undefined;
                PlacementStr ->
                    decode_placement_string(PlacementStr)
            end;
        PlacementStr ->
            decode_placement_string(PlacementStr)
    end.

decode_placement_string(PlacementStr) ->
    [
        begin
            [Zone, N] = string:tokens(Rule, ":"),
            {list_to_binary(Zone), list_to_integer(N)}
        end
     || Rule <- string:tokens(PlacementStr, ",")
    ].

-spec dbname(#shard{} | iodata()) -> binary().
dbname(#shard{dbname = DbName}) ->
    DbName;
dbname(<<"shards/", _:8/binary, "-", _:8/binary, "/", DbName/binary>>) ->
    strip_shard_suffix(DbName);
dbname(DbName) when is_list(DbName) ->
    dbname(list_to_binary(DbName));
dbname(DbName) when is_binary(DbName) ->
    DbName;
dbname(_) ->
    erlang:error(badarg).

%% @doc Determine if DocId belongs in shard (identified by record or filename)
belongs(#shard{} = Shard, DocId) when is_binary(DocId) ->
    [Begin, End] = range(Shard),
    belongs(Begin, End, Shard, DocId);
belongs(<<"shards/", _/binary>> = ShardName, DocId) when is_binary(DocId) ->
    [Begin, End] = range(ShardName),
    belongs(Begin, End, ShardName, DocId);
belongs(DbName, DocId) when is_binary(DbName), is_binary(DocId) ->
    true.

belongs(Begin, End, Shard, DocId) ->
    HashKey = mem3_hash:calculate(Shard, DocId),
    Begin =< HashKey andalso HashKey =< End.

range(#shard{range = Range}) ->
    Range;
range(#ordered_shard{range = Range}) ->
    Range;
range(<<"shards/", Start:8/binary, "-", End:8/binary, "/", _/binary>>) ->
    [
        binary_to_integer(Start, 16),
        binary_to_integer(End, 16)
    ].

allowed_nodes() ->
    lists:filter(
        fun(Node) ->
            Decom = mem3:node_info(Node, <<"decom">>),
            (Decom =/= true) andalso (Decom =/= <<"true">>)
        end,
        mem3:nodes()
    ).

nodes_in_zone(Nodes, Zone) ->
    [Node || Node <- Nodes, Zone == mem3:node_info(Node, <<"zone">>)].

live_shards(DbName, Nodes) ->
    live_shards(DbName, Nodes, []).

live_shards(DbName, Nodes, Options) ->
    [S || S <- shards_int(DbName, Options), lists:member(mem3:node(S), Nodes)].

zone_map(Nodes) ->
    [{Node, node_info(Node, <<"zone">>)} || Node <- Nodes].

group_by_proximity(Shards) ->
    Nodes = [mem3:node(S) || S <- lists:ukeysort(#shard.node, Shards)],
    group_by_proximity(Shards, zone_map(Nodes)).

group_by_proximity(Shards, ZoneMap) ->
    {Local, Remote} = lists:partition(
        fun(S) -> mem3:node(S) =:= config:node_name() end,
        Shards
    ),
    LocalZone = proplists:get_value(config:node_name(), ZoneMap),
    Fun = fun(S) -> proplists:get_value(mem3:node(S), ZoneMap) =:= LocalZone end,
    {SameZone, DifferentZone} = lists:partition(Fun, Remote),
    {Local, SameZone, DifferentZone}.

choose_ushards(DbName, Shards) ->
    Groups0 = group_by_range(Shards),
    Groups1 = [
        mem3_util:rotate_list({DbName, R}, order_shards(G))
     || {R, G} <- Groups0
    ],
    [hd(G) || G <- Groups1].

order_shards([#ordered_shard{} | _] = OrderedShards) ->
    lists:keysort(#ordered_shard.order, OrderedShards);
order_shards(UnorderedShards) ->
    UnorderedShards.

group_by_range(Shards) ->
    lists:foldl(
        fun(Shard, Dict) ->
            orddict:append(mem3:range(Shard), Shard, Dict)
        end,
        orddict:new(),
        Shards
    ).

% quorum functions

quorum(DbName) when is_binary(DbName) ->
    n(DbName) div 2 + 1;
quorum(Db) ->
    quorum(couch_db:name(Db)).

node(#shard{node = Node}) ->
    Node;
node(#ordered_shard{node = Node}) ->
    Node.

name(#shard{name = Name}) ->
    Name;
name(#ordered_shard{name = Name}) ->
    Name.

% Direct calculation of node membership. This is the algorithm part. It
% doesn't read the shard map, just picks owner based on a hash.
-spec owner(binary(), binary(), [node()]) -> node().
owner(DbName, DocId, Nodes) ->
    hd(mem3_util:rotate_list({DbName, DocId}, lists:usort(Nodes))).

engine(#shard{opts = Opts}) ->
    engine(Opts);
engine(#ordered_shard{opts = Opts}) ->
    engine(Opts);
engine(Opts) when is_list(Opts) ->
    case couch_util:get_value(engine, Opts) of
        Engine when is_binary(Engine) ->
            [{engine, Engine}];
        _ ->
            []
    end.

%% Check whether a node is up or down
%%  side effect: set up a connection to Node if there not yet is one.

-spec ping(node()) -> pos_integer() | Error :: term().

ping(Node) ->
    [{Node, Res}] = ping_nodes([Node]),
    Res.

-spec ping(node(), Timeout :: pos_integer()) -> pos_integer() | Error :: term().

ping(Node, Timeout) when is_atom(Node) ->
    [{Node, Res}] = ping_nodes([Node], Timeout),
    Res.

-spec ping_nodes() -> [{node(), pos_integer() | Error :: term()}].

ping_nodes() ->
    ping_nodes(live_cluster_nodes(), ?PING_TIMEOUT_IN_MS).

-spec ping_nodes(Timeout :: pos_integer()) -> [{node(), pos_integer() | Error :: term()}].

ping_nodes(Timeout) when is_integer(Timeout), Timeout > 0 ->
    ping_nodes(live_cluster_nodes(), Timeout).

ping_nodes(Nodes, Timeout) ->
    PidRefs = [spawn_monitor(fun() -> exit(do_ping(N, Timeout)) end) || N <- Nodes],
    Refs = maps:from_keys([Ref || {_Pid, Ref} <- PidRefs], true),
    UntilMSec = erlang:monotonic_time(millisecond) + Timeout,
    Results = gather_ping_results(Refs, UntilMSec, #{}),
    Fun = fun(Node, {_Pid, Ref}) -> {Node, map_get(Ref, Results)} end,
    lists:sort(lists:zipwith(Fun, Nodes, PidRefs)).

% Gather ping results but use an absolute time limit to avoid
% waiting up to Timeout's worth of time per individual node.
%
gather_ping_results(Refs, _Until, Results) when map_size(Refs) == 0 ->
    Results;
gather_ping_results(Refs, Until, Results) ->
    Timeout = Until - erlang:monotonic_time(millisecond),
    case Timeout >= 0 of
        true ->
            receive
                {'DOWN', Ref, _, _, Res} when is_map_key(Ref, Refs) ->
                    Refs1 = maps:remove(Ref, Refs),
                    Results1 = Results#{Ref => Res},
                    gather_ping_results(Refs1, Until, Results1)
            after min(100, Timeout) ->
                gather_ping_results(Refs, Until, Results)
            end;
        false ->
            Fun = fun(Ref, true, Acc) ->
                erlang:demonitor(Ref, [flush]),
                Acc#{Ref => timeout}
            end,
            maps:fold(Fun, Results, Refs)
    end.

live_cluster_nodes() ->
    mem3_util:live_nodes() -- [node()].

do_ping(Node, Timeout) ->
    T0 = erlang:monotonic_time(),
    % This is the function called by net_adm ping. One difference is that
    % net_adm ping on a failure will also forcibly disconnect a node
    % which we don't do here:
    %  https://github.com/erlang/otp/blob/master/lib/kernel/src/net_adm.erl#L97
    try gen_server:call({net_kernel, Node}, {is_auth, node()}, Timeout) of
        yes ->
            erlang:convert_time_unit(erlang:monotonic_time() - T0, native, microsecond);
        Error ->
            Error
    catch
        exit:{GenServerErr, _Stack} ->
            GenServerErr;
        Tag:Err ->
            {Tag, Err}
    end.

-spec dead_nodes() -> [node() | Error :: term()].

%% @doc Returns a list of dead nodes from the cluster.
%%
%% "dead" node is a node which appears in the mem3:nodes() list but is not
%% connected. Dead nodes are included in the result if it's considered
%% "dead" by any of the reachable nodes or doesn't respond or timeout when
%% queried by the multicall/2.
%%
%% dead_nodes(TimeoutInMSec) will use the timeout for the rpc:multicall. If any
%% node fails to respond in that time, it will be added to the dead nodes
%% response list as well.
%%
%% The default timeout is 30 seconds
%%
dead_nodes() ->
    dead_nodes(?PING_TIMEOUT_IN_MS).

-spec dead_nodes(Timeout :: pos_integer()) -> [node() | Error :: term()].

dead_nodes(Timeout) when is_integer(Timeout), Timeout > 0 ->
    % Here we are trying to detect overlapping partitions where not all the
    % nodes connect to each other. For example: n1 connects to n2 and n3, but
    % n2 and n3 are not connected.
    DeadFun = fun() ->
        Expected = ordsets:from_list(mem3:nodes()),
        Live = ordsets:from_list(mem3_util:live_nodes()),
        Dead = ordsets:subtract(Expected, Live),
        ordsets:to_list(Dead)
    end,
    {Responses, BadNodes} = multicall(DeadFun, Timeout),
    AccF = lists:foldl(
        fun
            (Dead, Acc) when is_list(Dead) -> ordsets:union(Acc, Dead);
            (Error, Acc) -> ordsets:union(Acc, [Error])
        end,
        ordsets:from_list(BadNodes),
        Responses
    ),
    ordsets:to_list(AccF).

multicall(Fun, Timeout) when is_integer(Timeout), Timeout > 0 ->
    F = fun() -> catch Fun() end,
    rpc:multicall(erlang, apply, [F, []], Timeout).

db_is_current(#shard{name = Name}) ->
    db_is_current(Name);
db_is_current(<<"shards/", _/binary>> = Name) ->
    try
        Shards = mem3:shards(mem3:dbname(Name)),
        lists:keyfind(Name, #shard.name, Shards) =/= false
    catch
        error:database_does_not_exist ->
            false
    end;
db_is_current(Name) when is_binary(Name) ->
    % This accounts for local (non-sharded) dbs, and is mostly
    % for unit tests that either test or use mem3_rep logic
    couch_server:exists(Name).

generate_shard_suffix() ->
    UnixSeconds = os:system_time(second),
    "." ++ integer_to_list(UnixSeconds).

strip_shard_suffix(DbName) when is_binary(DbName) ->
    % length(".1684269710") = 11. On 2286-11-20 the timestamp would flip to 11
    % digits so we'd have to increase length to 12 then.
    case DbName of
        <<Prefix:(byte_size(DbName) - 11)/binary, $., Ts/binary>> ->
            try
                _ = binary_to_integer(Ts),
                Prefix
            catch
                error:badarg ->
                    filename:rootname(DbName)
            end;
        _ ->
            filename:rootname(DbName)
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(ALLOWED_NODE, 'node1@127.0.0.1').

allowed_nodes_test_() ->
    {"allowed_nodes test", [
        {
            setup,
            fun() ->
                Props = [
                    {?ALLOWED_NODE, []},
                    {'node2@127.0.0.1', [{<<"decom">>, <<"true">>}]},
                    {'node3@127.0.0.1', [{<<"decom">>, true}]}
                ],
                ok = meck:expect(
                    mem3_nodes,
                    get_nodelist,
                    fun() -> proplists:get_keys(Props) end
                ),
                ok = meck:expect(
                    mem3_nodes,
                    get_node_info,
                    fun(Node, Key) ->
                        couch_util:get_value(Key, proplists:get_value(Node, Props))
                    end
                )
            end,
            fun(_) -> meck:unload() end,
            [
                ?_assertMatch([?ALLOWED_NODE], allowed_nodes())
            ]
        }
    ]}.

rotate_rand_degenerate_test() ->
    ?assertEqual([1], rotate_rand([1])).

rotate_rand_distribution_test() ->
    Cases = [rotate_rand([1, 2, 3]) || _ <- lists:seq(1, 100)],
    ?assertEqual(3, length(lists:usort(Cases))).

strip_shard_suffix_test_() ->
    Prefix = <<"shards/c0000000-ffffffff/">>,
    [
        {DbName, ?_assertEqual(Res, dbname(<<Prefix/binary, DbName/binary>>))}
     || {Res, DbName} <- [
            {<<"foo">>, <<"foo.1684269710">>},
            {<<"foo">>, <<"foo.168426971z">>},
            {<<"foo/bar">>, <<"foo/bar.1684269710">>},
            {<<"foo">>, <<"foo.1">>},
            {<<"foo">>, <<"foo.abc">>},
            {<<"foo">>, <<"foo.1111111111111111">>},
            {<<"">>, <<"">>},
            {<<"/">>, <<"/">>},
            {<<"//">>, <<"//">>},
            {<<"/.foo">>, <<"/.foo">>},
            {<<".">>, <<"..">>},
            {<<".">>, <<"..foo">>}
        ]
    ].

shard_suffix_test() ->
    % Assert a few basic things about the db suffixes we generate. If we change
    % this scheme make sure to update strip_shard_suffix/1 and other places
    % which assume the suffix is a 10 digit unix timestamp.
    Suffix = generate_shard_suffix(),
    ?assertEqual($., hd(Suffix)),
    ?assertEqual(11, length(Suffix)),
    [$. | Timestamp] = Suffix,
    ?assert(is_integer(list_to_integer(Timestamp))).

-endif.
