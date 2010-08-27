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

-module(fabric_util).

-export([submit_jobs/3, cleanup/1, recv/4, receive_loop/4, receive_loop/6,
    get_db/1]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

submit_jobs(Shards, EndPoint, ExtraArgs) ->
    lists:map(fun(#shard{node=Node, name=ShardName} = Shard) ->
        Ref = rexi:cast(Node, {fabric_rpc, EndPoint, [ShardName | ExtraArgs]}),
        Shard#shard{ref = Ref}
    end, Shards).

cleanup(Workers) ->
    [rexi:kill(Node, Ref) || #shard{node=Node, ref=Ref} <- Workers].

recv(Workers, Keypos, Fun, Acc0) ->
    receive_loop(Workers, Keypos, Fun, Acc0).

receive_loop(Workers, Keypos, Fun, Acc0) ->
    case couch_config:get("fabric", "request_timeout", "60000") of
    "infinity" ->
        Timeout = infinity;
    N ->
        Timeout = list_to_integer(N)
    end,
    receive_loop(Workers, Keypos, Fun, Acc0, Timeout, infinity).

%% @doc set up the receive loop with an overall timeout
-spec receive_loop([any()], integer(), function(), any(), timeout(), timeout()) ->
    {ok, any()} | timeout | {error, any()}.
receive_loop(RefPartMap, Keypos, Fun, Acc0, infinity, PerMsgTO) ->
    process_mailbox(RefPartMap, Keypos, Fun, Acc0, nil, PerMsgTO);
receive_loop(RefPartMap, Keypos, Fun, Acc0, GlobalTimeout, PerMsgTO) ->
    TimeoutRef = erlang:make_ref(),
    {ok, TRef} = timer:send_after(GlobalTimeout, {timeout, TimeoutRef}),
    try
        process_mailbox(RefPartMap, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO)
    after
        timer:cancel(TRef)
    end.

process_mailbox(RefList, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO) ->
    case process_message(RefList, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO) of
    {ok, Acc} ->
        process_mailbox(RefList, Keypos, Fun, Acc, TimeoutRef, PerMsgTO);
    {stop, Acc} ->
        {ok, Acc};
    Error ->
        Error
    end.

process_message(RefList, Keypos, Fun, Acc0, TimeoutRef, PerMsgTO) ->
    receive
    {timeout, TimeoutRef} ->
        timeout;
    {Ref, Msg} ->
        case lists:keyfind(Ref, Keypos, RefList) of
        false ->
            % this was some non-matching message which we will ignore
            {ok, Acc0};
        Worker ->
            Fun(Msg, Worker, Acc0)
        end;
    {Ref, From, Msg} ->
        case lists:keyfind(Ref, Keypos, RefList) of
        false ->
            {ok, Acc0};
        Worker ->
            Fun(Msg, {Worker, From}, Acc0)
        end;
    {rexi_DOWN, _RexiMonPid, ServerPid, Reason} = Msg ->
        ?LOG_ERROR("rexi_DOWN ~p ~p", [ServerPid, Reason]),
        Fun(Msg, nil, Acc0)
    after PerMsgTO ->
        timeout
    end.

get_db(DbName) ->
    Shards = mem3:shards(DbName),
    case lists:partition(fun(#shard{node = N}) -> N =:= node() end, Shards) of
    {[#shard{name = ShardName}|_], _} ->
        % prefer node-local DBs
        couch_db:open(ShardName, []);
    {[], [#shard{node = Node, name = ShardName}|_]} ->
        % but don't require them
        rpc:call(Node, couch_db, open, [ShardName, []])
    end.
