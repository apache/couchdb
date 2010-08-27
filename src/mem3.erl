% Copyright 2010 Cloudant
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

-module(mem3).

-export([start/0, stop/0, restart/0, nodes/0, shards/1, shards/2,
    choose_shards/2]).
-export([compare_nodelists/0, compare_shards/1]).

-include("mem3.hrl").

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
-spec compare_nodelists() -> [{{cluster_nodes, [node()]} | bad_nodes
    | non_member_nodes, [node()]}].
compare_nodelists() ->
    Nodes = mem3:nodes(),
    AllNodes = erlang:nodes([this, visible]),
    {Replies, BadNodes} = gen_server:multi_call(Nodes, mem3_nodes, get_nodelist),
    Dict = lists:foldl(fun({Node, Nodelist}, D) ->
        orddict:append({cluster_nodes, Nodelist}, Node, D)
    end, orddict:new(), Replies),
    [{non_member_nodes, AllNodes -- Nodes}, {bad_nodes, BadNodes} | Dict].

-spec compare_shards(DbName::iodata()) -> [{bad_nodes | [#shard{}], [node()]}].
compare_shards(DbName) when is_list(DbName) ->
    compare_shards(list_to_binary(DbName));
compare_shards(DbName) ->
    Nodes = mem3:nodes(),
    {Replies, BadNodes} = rpc:multicall(mem3, shards, [DbName]),
    GoodNodes = [N || N <- Nodes, not lists:member(N, BadNodes)],
    Dict = lists:foldl(fun({Shards, Node}, D) ->
        orddict:append(Shards, Node, D)
    end, orddict:new(), lists:zip(Replies, GoodNodes)),
    [{bad_nodes, BadNodes} | Dict].

-spec nodes() -> [node()].
nodes() ->
    mem3_nodes:get_nodelist().

-spec shards(DbName::iodata()) -> [#shard{}].
shards(DbName) when is_list(DbName) ->
    shards(list_to_binary(DbName));
shards(DbName) ->
    try ets:lookup(partitions, DbName) of
    [] ->
        mem3_util:load_shards_from_disk(DbName);
    Else ->
        Else
    catch error:badarg ->
        mem3_util:load_shards_from_disk(DbName)
    end.

-spec shards(DbName::iodata(), DocId::binary()) -> [#shard{}].
shards(DbName, DocId) when is_list(DbName) ->
    shards(list_to_binary(DbName), DocId);
shards(DbName, DocId) when is_list(DocId) ->
    shards(DbName, list_to_binary(DocId));
shards(DbName, DocId) ->
    HashKey = mem3_util:hash(DocId),
    Head = #shard{
        name = '_',
        node = '_',
        dbname = DbName,
        range = ['$1','$2'],
        ref = '_'
    },
    Conditions = [{'<', '$1', HashKey}, {'=<', HashKey, '$2'}],
    try ets:select(partitions, [{Head, Conditions, ['$_']}]) of
    [] ->
        mem3_util:load_shards_from_disk(DbName, DocId);
    Shards ->
        Shards
    catch error:badarg ->
        mem3_util:load_shards_from_disk(DbName, DocId)
    end.

-spec choose_shards(DbName::iodata(), Options::list()) -> [#shard{}].
choose_shards(DbName, Options) when is_list(DbName) ->
    choose_shards(list_to_binary(DbName), Options);
choose_shards(DbName, Options) ->
    try shards(DbName)
    catch error:E when E==database_does_not_exist; E==badarg ->
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
