% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mem3_sync_event_listener).
-behavior(couch_event_listener).

-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_event/3,
    handle_cast/2,
    handle_info/2
]).

-include_lib("mem3/include/mem3.hrl").

-record(state, {
    nodes,
    shards,
    users
}).

start_link() ->
    couch_event_listener:start_link(?MODULE, [], [all_dbs]).

init(_) ->
    State = #state{
        nodes = mem3_sync:nodes_db(),
        shards = mem3_sync:shards_db(),
        users = mem3_sync:users_db()
    },
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_event(NodesDb, updated, #state{nodes = NodesDb} = St) ->
    Nodes = mem3:nodes(),
    Live = nodes(),
    [mem3_sync:push(NodesDb, N) || N <- Nodes, lists:member(N, Live)],
    {ok, St};
handle_event(ShardsDb, updated, #state{shards = ShardsDb} = St) ->
    mem3_sync:push(ShardsDb, mem3_sync:find_next_node()),
    {ok, St};
handle_event(UsersDb, updated, #state{users = UsersDb} = St) ->
    mem3_sync:push(UsersDb, mem3_sync:find_next_node()),
    {ok, St};
handle_event(<<"shards/", _/binary>> = ShardName, updated, St) ->
    try mem3:shards(mem3:dbname(ShardName)) of
    Shards ->
        Targets = [S || #shard{node=N, name=Name} = S <- Shards,
            N =/= node(), Name =:= ShardName],
        Live = nodes(),
        [mem3_sync:push(ShardName,N) || #shard{node=N} <- Targets,
            lists:member(N, Live)]
    catch error:database_does_not_exist ->
        ok
    end,
    {ok, St};
handle_event(<<"shards/", _:18/binary, _/binary>> = ShardName, deleted, St) ->
    mem3_sync:remove_shard(ShardName),
    {ok, St};
handle_event(_DbName, _Event, St) ->
    {ok, St}.

handle_cast(Msg, St) ->
    couch_log:notice("unexpected cast to mem3_sync_event_listener: ~p", [Msg]),
    {ok, St}.

handle_info(Msg, St) ->
    couch_log:notice("unexpected info to mem3_sync_event_listener: ~p", [Msg]),
    {ok, St}.
