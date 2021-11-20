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

-module(mem3_httpd).

-export([
    handle_membership_req/1,
    handle_shards_req/2,
    handle_sync_req/2
]).

%% includes
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

handle_membership_req(
    #httpd{
        method = 'GET',
        path_parts = [<<"_membership">>]
    } = Req
) ->
    ClusterNodes =
        try
            mem3:nodes()
        catch
            _:_ -> {ok, []}
        end,
    couch_httpd:send_json(
        Req,
        {[
            {all_nodes, lists:sort([node() | nodes()])},
            {cluster_nodes, lists:sort(ClusterNodes)}
        ]}
    );
handle_membership_req(#httpd{path_parts = [<<"_membership">>]} = Req) ->
    chttpd:send_method_not_allowed(Req, "GET").

handle_shards_req(
    #httpd{
        method = 'GET',
        path_parts = [_DbName, <<"_shards">>]
    } = Req,
    Db
) ->
    DbName = mem3:dbname(couch_db:name(Db)),
    Shards = mem3:shards(DbName),
    JsonShards = json_shards(Shards, dict:new()),
    couch_httpd:send_json(
        Req,
        {[
            {shards, JsonShards}
        ]}
    );
handle_shards_req(
    #httpd{
        method = 'GET',
        path_parts = [_DbName, <<"_shards">>, DocId]
    } = Req,
    Db
) ->
    DbName = mem3:dbname(couch_db:name(Db)),
    Shards = mem3:shards(DbName, DocId),
    {[{Shard, Dbs}]} = json_shards(Shards, dict:new()),
    couch_httpd:send_json(
        Req,
        {[
            {range, Shard},
            {nodes, Dbs}
        ]}
    );
handle_shards_req(#httpd{path_parts = [_DbName, <<"_shards">>]} = Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET");
handle_shards_req(#httpd{path_parts = [_DbName, <<"_shards">>, _DocId]} = Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET").

handle_sync_req(
    #httpd{
        method = 'POST',
        path_parts = [_DbName, <<"_sync_shards">>]
    } = Req,
    Db
) ->
    DbName = mem3:dbname(couch_db:name(Db)),
    ShardList = [S#shard.name || S <- mem3:ushards(DbName)],
    [sync_shard(S) || S <- ShardList],
    chttpd:send_json(Req, 202, {[{ok, true}]});
handle_sync_req(Req, _) ->
    chttpd:send_method_not_allowed(Req, "POST").

%%
%% internal
%%

json_shards([], AccIn) ->
    List = dict:to_list(AccIn),
    {lists:sort(List)};
json_shards([#shard{node = Node, range = [B, E]} | Rest], AccIn) ->
    HexBeg = couch_util:to_hex(<<B:32/integer>>),
    HexEnd = couch_util:to_hex(<<E:32/integer>>),
    Range = list_to_binary(HexBeg ++ "-" ++ HexEnd),
    json_shards(Rest, dict:append(Range, Node, AccIn)).

sync_shard(ShardName) ->
    Shards = mem3_shards:for_shard_range(ShardName),
    [
        rpc:call(S1#shard.node, mem3_sync, push, [S1, S2#shard.node])
     || S1 <- Shards, S2 <- Shards, S1 =/= S2
    ],
    ok.
