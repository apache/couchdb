-module(mem3).

-export([start/0, stop/0, restart/0, state/0, nodes/0, shards/1, shards/2,
    choose_shards/2]).

-include("mem3.hrl").

-define(SERVER, mem3_server).

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
-spec state() -> [{any | bad_nodes | non_member_nodes, [node()]}].
state() ->
    {ok, Nodes} = mem3:nodes(),
    AllNodes = erlang:nodes([this, visible]),
    {Replies, BadNodes} = gen_server:multi_call(Nodes, ?SERVER, state),
    Dict = lists:foldl(fun({Node, {ok,State}}, D) ->
        orddict:append(State, Node, D)
    end, orddict:new(), Replies),
    [{non_member_nodes, AllNodes -- Nodes}, {bad_nodes, BadNodes} | Dict].

-spec nodes() -> [node()].
nodes() ->
    mem3_nodes:get_nodelist().

-spec shards(DbName::binary()) -> [#shard{}].
shards(DbName) ->
    case ets:lookup(partitions, DbName) of
    [] ->
        % TODO fall back to checking dbs.couch directly
        erlang:error(database_does_not_exist);
    Else ->
        Else
    end.

-spec shards(DbName::binary(), DocId::binary()) -> [#shard{}].
shards(DbName, DocId) ->
    HashKey = mem3_util:hash(DocId),
    Head = #shard{
        name = '_',
        node = '_',
        dbname = DbName,
        range = ['$1','$2'],
        ref = '_'
    },
    % TODO these conditions assume A < B, which we don't require
    Conditions = [{'<', '$1', HashKey}, {'=<', HashKey, '$2'}],
    case ets:select(partitions, [{Head, Conditions, ['$_']}]) of
    [] ->
        % TODO fall back to checking dbs.couch directly
        erlang:error(database_does_not_exist);
    Shards ->
        Shards
    end.

choose_shards(DbName, Options) ->
    try shards(DbName)
    catch error:database_does_not_exist ->
        Nodes = mem3:nodes(),
        NodeCount = length(Nodes),
        N = mem3_util:n_val(couch_util:get_value(n, Options), NodeCount),
        Q = mem3_util:to_integer(couch_util:get_value(q, Options,
            couch_config:get("cluster", "q", "8"))),
        % rotate to a random entry in the nodelist for even distribution
        {A, B} = lists:split(crypto:rand_uniform(1,length(Nodes)+1), Nodes),
        RotatedNodes = B ++ A,
        mem3_util:create_partition_map(DbName, N, Q, RotatedNodes)
    end.
