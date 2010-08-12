-module(mem3_httpd).

-export([handle_membership_req/1]).

%% includes
-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").


handle_membership_req(#httpd{method='GET',
        path_parts=[<<"_membership">>]} = Req) ->
    ClusterNodes = try mem3:nodes()
    catch _:_ -> {ok,[]} end,
    couch_httpd:send_json(Req, {[
        {all_nodes, lists:sort([node()|nodes()])},
        {cluster_nodes, lists:sort(ClusterNodes)}
    ]});
handle_membership_req(#httpd{method='GET',
        path_parts=[<<"_membership">>, <<"parts">>, DbName]} = Req) ->
    ClusterNodes = try mem3:nodes()
    catch _:_ -> {ok,[]} end,
    Shards = mem3:shards(DbName),
    JsonShards = json_shards(Shards, dict:new()),
    couch_httpd:send_json(Req, {[
        {all_nodes, lists:sort([node()|nodes()])},
        {cluster_nodes, lists:sort(ClusterNodes)},
        {partitions, JsonShards}
    ]}).

%%
%% internal
%%

json_shards([], AccIn) ->
    List = dict:to_list(AccIn),
    {lists:sort(List)};
json_shards([#shard{node=Node, range=[B,_E]} | Rest], AccIn) ->
    HexBeg = couch_util:to_hex(<<B:32/integer>>),
    json_shards(Rest, dict:append(HexBeg, Node, AccIn)).
