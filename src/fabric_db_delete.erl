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

-module(fabric_db_delete).
-export([go/2]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

%% @doc Options aren't used at all now in couch on delete but are left here
%%      to be consistent with fabric_db_create for possible future use
%% @see couch_server:delete/2
%%
go(DbName, _Options) ->
    Shards = mem3:shards(DbName),
    % delete doc from shard_db
    try delete_shard_db_doc(DbName) of
    {ok, ok} ->
        ok;
    {ok, accepted} ->
        accepted;
    {ok, not_found} ->
        erlang:error(database_does_not_exist, DbName);
    Error ->
        Error
    after
        % delete the shard files
        fabric_util:submit_jobs(Shards, delete_db, [])
    end.

delete_shard_db_doc(Doc) ->
    Shards = [#shard{node=N} || N <- mem3:nodes()],
    RexiMon = fabric_util:create_monitors(Shards),
    Workers = fabric_util:submit_jobs(Shards, delete_shard_db_doc, [Doc]),
    Acc0 = {length(Shards), fabric_dict:init(Workers, nil)},
    try fabric_util:recv(Workers, #shard.ref, fun handle_db_update/3, Acc0) of
    {timeout, {_, WorkersDict}} ->
        DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
        fabric_util:log_timeout(
            DefunctWorkers,
            "delete_shard_db_doc"
        ),
        {error, timeout};
    Else ->
        Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_db_update({rexi_DOWN, _, {_, Node}, _}, _Worker, {W, Counters}) ->
    New = fabric_dict:filter(fun(S, _) -> S#shard.node =/= Node end, Counters),
    maybe_stop(W, New);

handle_db_update({rexi_EXIT, _Reason}, Worker, {W, Counters}) ->
    maybe_stop(W, fabric_dict:erase(Worker, Counters));

handle_db_update(conflict, _, _) ->
    % just fail when we get any conflicts
    {error, conflict};

handle_db_update(Msg, Worker, {W, Counters}) ->
    maybe_stop(W, fabric_dict:store(Worker, Msg, Counters)).

maybe_stop(W, Counters) ->
    case fabric_dict:any(nil, Counters) of
    true ->
        {ok, {W, Counters}};
    false ->
        {Ok,NotFound} = fabric_dict:fold(fun count_replies/3, {0,0}, Counters),
        case {Ok + NotFound, Ok, NotFound} of
        {W, 0, W} ->
            {#shard{dbname=Name}, _} = hd(Counters),
            couch_log:warning("~p not_found ~s", [?MODULE, Name]),
            {stop, not_found};
        {W, _, _} ->
            {stop, ok};
        {N, M, _} when N >= (W div 2 + 1), M > 0 ->
            {stop, accepted};
        _ ->
            {error, internal_server_error}
        end
    end.

count_replies(_, ok, {Ok, NotFound}) ->
    {Ok+1, NotFound};
count_replies(_, not_found, {Ok, NotFound}) ->
    {Ok, NotFound+1};
count_replies(_, _, Acc) ->
    Acc.
