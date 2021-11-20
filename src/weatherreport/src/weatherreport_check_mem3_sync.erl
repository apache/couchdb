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

%% @doc Diagnostic that checks for the presence of the mem3_sync
%% registered process. If this is not found a warning message will be
%% sent, otherwise only informational messages.
-module(weatherreport_check_mem3_sync).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-spec description() -> string().
description() ->
    "Check there is a registered mem3_sync process".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    case erlang:whereis(mem3_sync) of
        undefined ->
            [{warning, mem3_sync_not_found}];
        Pid ->
            [{info, {mem3_sync_found, Pid}}]
    end.

-spec format(term()) -> {io:format(), [term()]}.
format(mem3_sync_not_found) ->
    {"No mem3_sync process found on local node.", []};
format({mem3_sync_found, Pid}) ->
    {"mem3_sync process found on local node with pid ~w", [Pid]}.
