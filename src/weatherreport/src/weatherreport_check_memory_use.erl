%% -------------------------------------------------------------------
%%
%% derived from riaknostic - automated diagnostic tools for Riak
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
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
%% File renamed from riaknostic_check_memory_use.erl to
%% weatherreport_check_memory_use.erl and modified to work with Apache
%% CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic that checks the current memory usage. If memory
%% usage is high, a warning message will be sent, otherwise only
%% informational messages.
-module(weatherreport_check_memory_use).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-spec description() -> string().
description() ->
    "Measure memory usage".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    Pid = weatherreport_node:pid(),
    Output = weatherreport_util:run_command("ps -o pmem,rss -p " ++ Pid),
    [_, _, Percent, RealSize | _] = string:tokens(Output, "/n \n"),
    Messages = [{info, {process_usage, Percent, RealSize}}],
    case weatherreport_util:binary_to_float(list_to_binary(Percent)) >= 90 of
        false ->
            Messages;
        true ->
            [{critical, {high_memory, Percent}} | Messages]
    end.

-spec format(term()) -> {io:format(), [term()]}.
format({high_memory, Percent}) ->
    {"Memory usage is HIGH: ~s% of available RAM", [Percent]};
format({process_usage, Percent, Real}) ->
    {"Process is using ~s% of available RAM, totalling ~s KB of real memory.", [Percent, Real]}.
