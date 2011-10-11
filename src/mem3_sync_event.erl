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

-module(mem3_sync_event).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
    code_change/3]).

init(_) ->
    net_kernel:monitor_nodes(true),
    {ok, nil}.

handle_event({add_node, Node}, State) when Node =/= node() ->
    Db1 = list_to_binary(couch_config:get("mem3", "node_db", "nodes")),
    Db2 = list_to_binary(couch_config:get("mem3", "shard_db", "dbs")),
    Db3 = list_to_binary(couch_config:get("couch_httpd_auth",
                                          "authentication_db", "_users")),
    [mem3_sync:push(Db, Node) || Db <- [Db1, Db2, Db3]],
    {ok, State};

handle_event({remove_node, Node}, State)  ->
    mem3_sync:remove_node(Node),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({nodeup, Node}, State) ->
    case lists:member(Node, mem3:nodes()) of
    true ->
        Db1 = list_to_binary(couch_config:get("mem3", "node_db", "nodes")),
        Db2 = list_to_binary(couch_config:get("mem3", "shard_db", "dbs")),
        Db3 = list_to_binary(couch_config:get("couch_httpd_auth",
                                              "authentication_db", "_users")),
        [mem3_sync:push(Db, Node) || Db <- [Db1, Db2, Db3]],
        mem3_sync:initial_sync([Node]);
    false ->
        ok
    end,
    {ok, State};

handle_info({nodedown, Node}, State) ->
    mem3_sync:remove_node(Node),
    {ok, State};

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
