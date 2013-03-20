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

-module(fabric_doc_missing_revs).

-export([go/2, go/3]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

go(DbName, AllIdsRevs) ->
    go(DbName, AllIdsRevs, []).

go(DbName, AllIdsRevs, Options) ->
    Workers = lists:map(fun({#shard{name=Name, node=Node} = Shard, IdsRevs}) ->
        Ref = rexi:cast(Node, {fabric_rpc, get_missing_revs, [Name, IdsRevs,
            Options]}),
        Shard#shard{ref=Ref}
    end, group_idrevs_by_shard(DbName, AllIdsRevs)),
    ResultDict = dict:from_list([{Id, {{nil,Revs},[]}} || {Id, Revs} <- AllIdsRevs]),
    RexiMon = fabric_util:create_monitors(Workers),
    Acc0 = {length(Workers), ResultDict, Workers},
    try
        fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0)
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Shard, {_WorkerLen, ResultDict, Workers}) ->
    NewWorkers = [W || #shard{node=Node} = W <- Workers, Node =/= NodeRef],
    skip_message({fabric_dict:size(NewWorkers), ResultDict, NewWorkers});
handle_message({rexi_EXIT, _}, Worker, {W, D, Workers}) ->
    skip_message({W-1,D,lists:delete(Worker, Workers)});
handle_message({ok, Results}, _Worker, {1, D0, _}) ->
    D = update_dict(D0, Results),
    {stop, dict:fold(fun force_reply/3, [], D)};
handle_message({ok, Results}, Worker, {WaitingCount, D0, Workers}) ->
    D = update_dict(D0, Results),
    case dict:fold(fun maybe_reply/3, {stop, []}, D) of
    continue ->
        % still haven't heard about some Ids
        {ok, {WaitingCount - 1, D, lists:delete(Worker,Workers)}};
    {stop, FinalReply} ->
        % finished, stop the rest of the jobs
        fabric_util:cleanup(lists:delete(Worker,Workers)),
        {stop, FinalReply}
    end.

force_reply(Id, {{nil,Revs}, Anc}, Acc) ->
    % never heard about this ID, assume it's missing
    [{Id, Revs, Anc} | Acc];
force_reply(_, {[], _}, Acc) ->
    Acc;
force_reply(Id, {Revs, Anc}, Acc) ->
    [{Id, Revs, Anc} | Acc].

maybe_reply(_, _, continue) ->
    continue;
maybe_reply(_, {{nil, _}, _}, _) ->
    continue;
maybe_reply(_, {[], _}, {stop, Acc}) ->
    {stop, Acc};
maybe_reply(Id, {Revs, Anc}, {stop, Acc}) ->
    {stop, [{Id, Revs, Anc} | Acc]}.

group_idrevs_by_shard(DbName, IdsRevs) ->
    dict:to_list(lists:foldl(fun({Id, Revs}, D0) ->
        lists:foldl(fun(Shard, D1) ->
            dict:append(Shard, {Id, Revs}, D1)
        end, D0, mem3:shards(DbName,Id))
    end, dict:new(), IdsRevs)).

update_dict(D0, KVs) ->
    lists:foldl(fun({K,V,A}, D1) -> dict:store(K, {V,A}, D1) end, D0, KVs).

skip_message({0, Dict, _Workers}) ->
    {stop, dict:fold(fun force_reply/3, [], Dict)};
skip_message(Acc) ->
    {ok, Acc}.
