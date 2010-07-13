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

handle_membership_req(#httpd{method='POST',
                           path_parts=[<<"_membership">>]} = Req) ->
    {JsonProps} = couch_httpd:json_body_obj(Req),
    Method = couch_util:get_value(<<"method">>, JsonProps),
    Params = couch_util:get_value(<<"params">>, JsonProps),
    Id = couch_util:get_value(<<"id">>, JsonProps),
    {Result, Error} = membership_dispatch(Method, Params),
    couch_httpd:send_json(Req, {[
        {result, Result},
        {error, Error},
        {id, Id}
    ]}).

%%
%% internal
%%
membership_dispatch(<<"replace">>, Params) ->
    OldNode = get_oldnode(Params),
    NewNodeOpts = get_value_json(<<"newnode_options">>, Params, []),
    PingNode = get_pingnode(Params),
    send_join(replace, {OldNode, NewNodeOpts}, PingNode);
membership_dispatch(TypeBin, Params) ->
    Type = list_to_atom(?b2l(TypeBin)),
    NodeList = get_value_json(<<"nodes">>, Params, []),
    Nodes = lists:map(fun({List}) -> node_info(List) end, NodeList),
    PingNode = get_pingnode(Params),
    send_join(Type, Nodes, PingNode).

get_pingnode(Params) ->
    PingNodeBin = get_value_json(<<"pingnode">>, Params, <<"nil">>),
    list_to_atom(?b2l(PingNodeBin)).

get_oldnode(Params) ->
    NodeBin = get_value_json(<<"oldnode">>, Params, undefined),
    NodeList = ?b2l(NodeBin),
    list_to_atom(NodeList).

%% @doc send join command to mem module
send_join(Type, Payload, PingNode) ->
    case mem3:join(Type, Payload, PingNode) of
    ok -> {ok, null};
    {error, Error} -> {Type, Error};
    Other ->
        ?LOG_ERROR("membership dispatch error ~p", [Other]),
        {Type, unknown_error}
    end.

node_info(List) ->
    Order = couch_util:get_value(<<"order">>, List),
    Node1 = couch_util:get_value(<<"node">>, List),
    Node2 = list_to_atom(?b2l(Node1)),
    Options = couch_util:get_value(<<"options">>, List),
    {Order, Node2, Options}.

get_value_json(_,[], Default) -> Default;
get_value_json(Key, [JsonProp|Rest], Default) ->
    case JsonProp of
    {[{Key, Value}]} -> Value;
    _ -> get_value_json(Key, Rest, Default)
    end.
