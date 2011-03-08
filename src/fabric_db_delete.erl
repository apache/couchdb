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

-module(fabric_db_delete).
-export([go/2]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

%% @doc Options aren't used at all now in couch on delete but are left here
%%      to be consistent with fabric_db_create for possible future use
%% @see couch_server:delete_db
%%
go(DbName, _Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, delete_db, [DbName]),
    Acc0 = fabric_dict:init(Workers, nil),
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, ok} ->
        ok;
    {ok, not_found} ->
        erlang:error(database_does_not_exist, DbName);
    Error ->
        Error
    end.

handle_message({rexi_EXIT, Reason}, _Worker, _Counters) ->
    {error, Reason};

handle_message(Msg, Shard, Counters) ->
    C1 = fabric_dict:store(Shard, Msg, Counters),
    case fabric_dict:any(nil, C1) of
    true ->
        {ok, C1};
    false ->
        final_answer(C1)
    end.

final_answer(Counters) ->
    Successes = [X || {_, M} = X <- Counters, M == ok orelse M == not_found],
    case fabric_view:is_progress_possible(Successes) of
    true ->
        case lists:keymember(ok, 2, Successes) of
        true ->
            {stop, ok};
        false ->
            {stop, not_found}
        end;
    false ->
        {error, internal_server_error}
    end.
