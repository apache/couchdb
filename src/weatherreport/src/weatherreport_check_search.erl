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

%% @doc Diagnostic that checks the local clouseau node is responsive.
%% If clouseau is unresponsive then search will not work. An info
%% message is returned if clouseau responds to pings and an error
%% otherwise.
-module(weatherreport_check_search).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-spec description() -> string().
description() ->
    "Check the local search node is responsive".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    SearchNodeStr = config:get("dreyfus", "name"),
    case SearchNodeStr of
        undefined ->
            [{info, clouseau_not_configured}];
        _ ->
            try list_to_existing_atom(SearchNodeStr) of
                SearchNode ->
                    ping_search_node(SearchNode)
            catch
                error:badarg ->
                    [{warning, {clouseau_node, SearchNodeStr}}]
            end
    end.

-spec ping_search_node(atom()) -> [{atom(), term()}].
ping_search_node(SearchNode) ->
    case net_adm:ping(SearchNode) of
        pong ->
            [{info, {clouseau_ok, SearchNode}}];
        Error ->
            % only warning since search is not enabled by default
            [{warning, {clouseau_error, SearchNode, Error}}]
    end.

-spec format(term()) -> {io:format(), [term()]}.
format(clouseau_not_configured) ->
    {"Clouseau service is not configured", []};
format({clouseau_ok, SearchNode}) ->
    {"Local search node at ~w responding ok", [SearchNode]};
format({clouseau_node, SearchNode}) ->
    {"Search node name ~s is not recognised", [SearchNode]};
format({clouseau_error, SearchNode, Error}) ->
    {"Local search node at ~w not responding: ~w", [SearchNode, Error]}.
