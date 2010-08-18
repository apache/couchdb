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

-export([submit_jobs/3, cleanup/1, recv/4, get_db/1]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

submit_jobs(Shards, EndPoint, ExtraArgs) ->
    lists:map(fun(#shard{node=Node, name=ShardName} = Shard) ->
        Ref = rexi:cast(Node, {fabric_rpc, EndPoint, [ShardName | ExtraArgs]}),
        Shard#shard{ref = Ref}
    end, Shards).

cleanup(Workers) ->
    [rexi:kill(Node, Ref) || #shard{node=Node, ref=Ref} <- Workers].

recv(Workers, Keypos, Fun, Acc0) ->
    Timeout = case couch_config:get("fabric", "request_timeout", "60000") of
    "infinity" -> infinity;
    N -> list_to_integer(N)
    end,
    rexi_utils:recv(Workers, Keypos, Fun, Acc0, Timeout, infinity).


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
