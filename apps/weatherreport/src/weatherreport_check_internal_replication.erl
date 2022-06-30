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

%% @doc Diagnostic that checks the current size of the mem3_sync
%% backlog. The size is printed as an info message if under a defined
%% threshold, or as a warning if above the threshold.
-module(weatherreport_check_internal_replication).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-define(THRESHOLD, 1000000).

-spec description() -> string().
description() ->
    "Check the number of pending internal replication jobs".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec total_to_level(integer()) -> atom().
total_to_level(Total) when Total > ?THRESHOLD ->
    warning;
total_to_level(_Total) ->
    info.

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    Backlog = mem3_sync:get_backlog(),
    [{total_to_level(Backlog), Backlog}].

-spec format(term()) -> {io:format(), [term()]}.
format(Backlog) ->
    {"Total number of pending internal replication jobs: ~w", [Backlog]}.
