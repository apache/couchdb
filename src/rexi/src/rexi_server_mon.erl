% Copyright 2010-2013 Cloudant
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

-module(rexi_server_mon).
-behaviour(gen_server).
-behaviour(mem3_cluster).

-export([
    start_link/1,
    status/0,
    aggregate_queue_len/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    cluster_stable/1,
    cluster_unstable/1
]).

-define(CLUSTER_STABILITY_PERIOD_SEC, 15).

start_link(ChildMod) ->
    Name = list_to_atom(lists:concat([ChildMod, "_mon"])),
    gen_server:start_link({local, Name}, ?MODULE, ChildMod, []).

status() ->
    gen_server:call(?MODULE, status).

aggregate_queue_len(rexi_buffer) ->
    % rexi_buffer acts as an explicit message queue. In order to get useful
    % metrics from it we really need to add both its process' message queue and
    % already buffered messages.
    ServerIds = server_ids(rexi_buffer),
    MQLengths = [message_queue_len(ServerId) || ServerId <- ServerIds],
    BufLengths = [rexi_buffer:get_buffered_count(ServerId) || ServerId <- ServerIds],
    lists:sum(MQLengths) + lists:sum(BufLengths);
aggregate_queue_len(ChildMod) ->
    lists:sum([message_queue_len(ServerId) || ServerId <- server_ids(ChildMod)]).

% Mem3 cluster callbacks

cluster_unstable(Server) ->
    gen_server:cast(Server, cluster_unstable),
    Server.

cluster_stable(Server) ->
    gen_server:cast(Server, cluster_stable),
    Server.

% gen_server callbacks

init(ChildMod) ->
    {ok, _Mem3Cluster} = mem3_cluster:start_link(
        ?MODULE,
        self(),
        ?CLUSTER_STABILITY_PERIOD_SEC,
        ?CLUSTER_STABILITY_PERIOD_SEC
    ),
    start_servers(ChildMod),
    couch_log:info("~s : started servers", [ChildMod]),
    {ok, ChildMod}.

handle_call(status, _From, ChildMod) ->
    case missing_servers(ChildMod) of
        [] ->
            {reply, ok, ChildMod};
        Missing ->
            {reply, {waiting, length(Missing)}, ChildMod}
    end;
handle_call(Msg, _From, St) ->
    couch_log:notice("~s ignored_call ~w", [?MODULE, Msg]),
    {reply, ignored, St}.

% If cluster is unstable a node was added or just removed. Check if any nodes
% can be started, but do not immediately stop nodes, defer that till cluster
% stabilized.
handle_cast(cluster_unstable, ChildMod) ->
    couch_log:info("~s : cluster unstable", [ChildMod]),
    start_servers(ChildMod),
    {noreply, ChildMod};
% When cluster is stable, start any servers for new nodes and stop servers for
% the ones that disconnected.
handle_cast(cluster_stable, ChildMod) ->
    couch_log:info("~s : cluster stable", [ChildMod]),
    start_servers(ChildMod),
    stop_servers(ChildMod),
    {noreply, ChildMod};
handle_cast(Msg, St) ->
    couch_log:notice("~s ignored_cast ~w", [?MODULE, Msg]),
    {noreply, St}.

handle_info(Msg, St) ->
    couch_log:notice("~s ignored_info ~w", [?MODULE, Msg]),
    {noreply, St}.

start_servers(ChildMod) ->
    lists:foreach(
        fun(Id) ->
            {ok, _} = start_server(ChildMod, Id)
        end,
        missing_servers(ChildMod)
    ).

stop_servers(ChildMod) ->
    lists:foreach(
        fun(Id) ->
            ok = stop_server(ChildMod, Id)
        end,
        extra_servers(ChildMod)
    ).

server_ids(ChildMod) ->
    Nodes = [node() | nodes()],
    [list_to_atom(lists:concat([ChildMod, "_", Node])) || Node <- Nodes].

running_servers(ChildMod) ->
    [Id || {Id, _, _, _} <- supervisor:which_children(sup_module(ChildMod))].

missing_servers(ChildMod) ->
    server_ids(ChildMod) -- running_servers(ChildMod).

extra_servers(ChildMod) ->
    running_servers(ChildMod) -- server_ids(ChildMod).

start_server(ChildMod, ChildId) ->
    ChildSpec = {
        ChildId,
        {ChildMod, start_link, [ChildId]},
        permanent,
        brutal_kill,
        worker,
        [ChildMod]
    },
    case supervisor:start_child(sup_module(ChildMod), ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        Else ->
            erlang:error(Else)
    end.

stop_server(ChildMod, ChildId) ->
    SupMod = sup_module(ChildMod),
    ok = supervisor:terminate_child(SupMod, ChildId),
    ok = supervisor:delete_child(SupMod, ChildId).

sup_module(ChildMod) ->
    list_to_atom(lists:concat([ChildMod, "_sup"])).

message_queue_len(ServerId) when is_atom(ServerId) ->
    case whereis(ServerId) of
        Pid when is_pid(Pid) ->
            case process_info(Pid, message_queue_len) of
                {message_queue_len, Length} -> Length;
                undefined -> 0
            end;
        undefined ->
            0
    end.
