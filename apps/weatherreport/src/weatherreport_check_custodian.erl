%% -------------------------------------------------------------------
%%
%% weatherreport - automated diagnostic tools for CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% @doc Diagnostic that performs safety and liveness checks on
%% cluster shards. Shard safety is determined by the availability of
%% the nodes that contain copies of that shard. A shard is considered
%% unsafe if one or more nodes containing copies are unavailable.
%% Shard liveness is similar but also requires nodes containing copies
%% to be actively participating in the cluster. If one or more nodes
%% containing copies are in maintenance mode then liveness is impaired.
%% Messages are also returned for any databases where there are
%% conflicting shard maps.

-module(weatherreport_check_custodian).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-include_lib("eunit/include/eunit.hrl").

-spec description() -> string().
description() ->
    "Shard safety/liveness checks".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

n_to_level(2) ->
    warning;
n_to_level(1) ->
    error;
n_to_level(0) ->
    critical;
n_to_level(_) ->
    info.

report_to_message({DbName, ShardRange, {Type, N}}) ->
    {n_to_level(N), {Type, N, DbName, ShardRange}};
report_to_message({DbName, {conflicted, N}}) ->
    {warning, {conflicted, N, DbName}}.

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    case custodian:report() of
        [] ->
            [{info, ok}];
        Report ->
            lists:map(fun(R) -> report_to_message(R) end, Report)
    end.

-spec format(term()) -> {io:format(), [term()]}.
format(ok) ->
    {"All shards available and alive.", []};
format({Type, N, DbName, ShardRange}) ->
    {"~w ~w shards for Db: ~s Range: ~w.", [N, Type, DbName, ShardRange]};
format({conflicted, 1, DbName}) ->
    {"1 conflicted shard map for Db: ~s", [DbName]};
format({conflicted, N, DbName}) ->
    {"~w conflicted shard maps for Db: ~s", [N, DbName]}.
