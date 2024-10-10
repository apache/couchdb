%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(nouveau_rpc).

-export([
    search/3,
    info/2,
    cleanup/2
]).

-include("nouveau.hrl").
-import(nouveau_util, [index_path/1]).

search(DbName, #index{} = Index0, QueryArgs0) ->
    %% Incorporate the shard name into the record.
    Index1 = Index0#index{dbname = DbName},

    %% get minimum seqs for search
    {MinUpdateSeq, MinPurgeSeq} = nouveau_index_updater:get_db_info(Index1),

    %% Incorporate min seqs into the query args.
    QueryArgs1 = QueryArgs0#{
        min_update_seq => MinUpdateSeq,
        min_purge_seq => MinPurgeSeq
    },
    Update = maps:get(update, QueryArgs1, true),

    %% check if index is up to date
    T0 = erlang:monotonic_time(),
    case Update andalso nouveau_index_updater:outdated(Index1) of
        true ->
            case nouveau_index_manager:update_index(Index1) of
                ok ->
                    ok;
                {error, Reason} ->
                    rexi:reply({error, Reason})
            end;
        false ->
            ok;
        {error, Reason} ->
            rexi:reply({error, Reason})
    end,
    T1 = erlang:monotonic_time(),
    UpdateLatency = erlang:convert_time_unit(T1 - T0, native, millisecond),

    %% Run the search
    case nouveau_api:search(Index1, QueryArgs1) of
        {ok, Response} ->
            rexi:reply({ok, Response#{update_latency => UpdateLatency}});
        {error, stale_index} ->
            %% try again.
            search(DbName, Index0, QueryArgs0);
        Else ->
            rexi:reply(Else)
    end.

info(DbName, #index{} = Index0) ->
    %% Incorporate the shard name into the record.
    Index1 = Index0#index{dbname = DbName},
    case nouveau_api:index_info(Index1) of
        {ok, Info0} ->
            Info1 = Info0#{signature => Index0#index.sig},
            rexi:reply({ok, Info1});
        {error, Reason} ->
            rexi:reply({error, Reason})
    end.

cleanup(DbName, Exclusions) ->
    nouveau_api:delete_path(nouveau_util:index_name(DbName), Exclusions),
    rexi:reply(ok).
