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

%% @doc Diagnostic that checks for processes with high memory usage
%% and sends a warning message if one or more processes exceed the
%% threshold.
-module(weatherreport_check_process_memory).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-define(THRESHOLD, 104857600).

-spec description() -> string().
description() ->
    "Check for processes with high memory usage".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec check(list()) -> [{atom(), term()}].
check(Opts) ->
    weatherreport_util:check_proc_count(
        memory,
        ?THRESHOLD,
        Opts
    ).

-spec format(term()) -> {io:format(), [term()]}.
format({high, {Pid, Memory, Info, Pinfo}}) ->
    {"Process ~w has excessive memory usage of ~w: ~w ~w", [Pid, Memory, Info, Pinfo]};
format({high, {Pid, Memory, Info}}) ->
    {"Process ~w has excessive memory usage of ~w: ~w", [Pid, Memory, Info]};
format({ok, {Pid, Memory, Info}}) ->
    {"Process ~w has memory usage of ~w: ~w", [Pid, Memory, Info]}.
