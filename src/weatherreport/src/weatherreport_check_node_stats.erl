%% -------------------------------------------------------------------
%%
%% weatherreport - automated diagnostic tools for CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic that checks various erlang VM statistics that are
%% useful for diagnostics. A warning message is printed if certain stats
%% rise above pre-determined thresholds, otherwise an info message is sent.
-module(weatherreport_check_node_stats).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-define(SAMPLES, 10).
-define(T_RUN_QUEUE, 40).
-define(T_PROCESS_COUNT, 100000).

-spec description() -> string().
description() ->
    "Check useful erlang statistics for diagnostics".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec sum_absolute_stats({list(), list()}, list()) -> list().
sum_absolute_stats({AbsStats, _}, AbsSum) ->
    [{K, V + proplists:get_value(K, AbsSum, 0)} || {K, V} <- AbsStats].

-spec mean_to_message({atom(), integer()}) -> {atom(), {atom(), integer()}}.
mean_to_message({run_queue, Mean}) when Mean > ?T_RUN_QUEUE ->
    {warning, {run_queue, Mean}};
mean_to_message({process_count, Mean}) when Mean > ?T_PROCESS_COUNT ->
    {warning, {process_count, Mean}};
mean_to_message({Statistic, Mean}) ->
    {info, {Statistic, Mean}}.

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    SumOfStats = recon:node_stats(?SAMPLES, 100, fun sum_absolute_stats/2, []),
    MeanStats = [{K, erlang:round(V / ?SAMPLES)} || {K, V} <- SumOfStats],
    lists:map(fun mean_to_message/1, MeanStats).

-spec format(term()) -> {io:format(), [term()]}.
format({Statistic, Value}) ->
    {"Mean ~w over one second is ~w", [Statistic, Value]}.
