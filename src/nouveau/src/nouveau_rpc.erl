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
    info/2
]).

-include("nouveau.hrl").
-import(nouveau_util, [index_path/1]).

search(DbName, #index{} = Index0, #{} = QueryArgs) ->
    %% Incorporate the shard name into the record.
    Index1 = Index0#index{dbname = DbName},

    case maps:get(update, QueryArgs, true) of
        true ->
            %% get minimum seqs for search
            {MinUpdateSeq, MinPurgeSeq} = nouveau_index_updater:get_db_info(Index1),
            %% Incorporate min seqs into the query args.
            fresh_search(Index1, QueryArgs#{
                min_update_seq => MinUpdateSeq,
                min_purge_seq => MinPurgeSeq
            });
        false ->
            case
                nouveau_api:search(Index1, QueryArgs#{
                    min_update_seq => 0,
                    min_purge_seq => 0
                })
            of
                {ok, Response} ->
                    rexi:reply({ok, Response#{update_latency => 0}});
                Else ->
                    rexi:reply(Else)
            end
    end.

fresh_search(#index{} = Index, #{} = QueryArgs) ->
    fresh_search(Index, QueryArgs, 0).

fresh_search(#index{} = Index, #{} = QueryArgs, UpdateLatency) ->
    case nouveau_api:search(Index, QueryArgs) of
        {ok, Response} ->
            rexi:reply({ok, Response#{update_latency => ms(UpdateLatency)}});
        {error, stale_index} ->
            update_and_retry(Index, QueryArgs, UpdateLatency);
        {error, {not_found, _}} ->
            update_and_retry(Index, QueryArgs, UpdateLatency);
        Else ->
            rexi:reply(Else)
    end.

update_and_retry(Index, QueryArgs, UpdateLatency) ->
    T0 = erlang:monotonic_time(),
    case nouveau_index_manager:update_index(Index) of
        ok ->
            T1 = erlang:monotonic_time(),
            fresh_search(Index, QueryArgs, UpdateLatency + T1 - T0);
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

ms(NativeTime) ->
    erlang:convert_time_unit(NativeTime, native, millisecond).
