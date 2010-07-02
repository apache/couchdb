-module(fabric_doc_missing_revs).

-export([go/2]).

-include("fabric.hrl").

go(DbName, AllIdsRevs) ->
    Workers = lists:map(fun({#shard{name=Name, node=Node} = Shard, IdsRevs}) ->
        Ref = rexi:cast(Node, {fabric_rpc, get_missing_revs, [Name, IdsRevs]}),
        Shard#shard{ref=Ref}
    end, group_idrevs_by_shard(DbName, AllIdsRevs)),
    ResultDict = dict:from_list([{Id, {nil,Revs}} || {Id, Revs} <- AllIdsRevs]),
    Acc0 = {length(Workers), ResultDict},
    fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0).

handle_message({rexi_DOWN, _, _, _}, _Worker, Acc0) ->
    skip_message(Acc0);
handle_message({rexi_EXIT, _, _, _}, _Worker, Acc0) ->
    skip_message(Acc0);
handle_message({ok, Results}, _Worker, {1, D0}) ->
    D = update_dict(D0, Results),
    {stop, dict:fold(fun force_reply/3, [], D)};
handle_message({ok, Results}, _Worker, {WaitingCount, D0}) ->
    D = update_dict(D0, Results),
    case dict:fold(fun maybe_reply/3, {stop, []}, D) of
    continue ->
        % still haven't heard about some Ids
        {ok, {WaitingCount - 1, D}};
    {stop, FinalReply} ->
        {stop, FinalReply}
    end.

force_reply(Id, {nil,Revs}, Acc) ->
    % never heard about this ID, assume it's missing
    [{Id, Revs} | Acc];
force_reply(_, [], Acc) ->
    Acc;
force_reply(Id, Revs, Acc) ->
    [{Id, Revs} | Acc].

maybe_reply(_, _, continue) ->
    continue;
maybe_reply(_, {nil, _}, _) ->
    continue;
maybe_reply(_, [], {stop, Acc}) ->
    {stop, Acc};
maybe_reply(Id, Revs, {stop, Acc}) ->
    {stop, [{Id, Revs} | Acc]}.

group_idrevs_by_shard(DbName, IdsRevs) ->
    dict:to_list(lists:foldl(fun({Id, Revs}, D0) ->
        lists:foldl(fun(Shard, D1) ->
            dict:append(Shard, {Id, Revs}, D1)
        end, D0, mem3:shards(DbName,Id))
    end, dict:new(), IdsRevs)).

update_dict(D0, KVs) ->
    lists:foldl(fun({K,V,_}, D1) -> dict:store(K, V, D1) end, D0, KVs).

skip_message({1, Dict}) ->
    {stop, dict:fold(fun force_reply/3, [], Dict)};
skip_message({WaitingCount, Dict}) ->
    {ok, {WaitingCount-1, Dict}}.
