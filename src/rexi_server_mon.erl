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

-module(rexi_server_mon).
-behaviour(gen_server).


-export([
    start_link/0,
    status/0
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(SUP_MODULE, rexi_server_sup).
-define(CHILD_MODULE, rexi_server).
-define(INTERVAL, 60000).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


status() ->
    gen_server:call(?MODULE, status).


init([]) ->
    net_kernel:monitor_nodes(true),
    erlang:send_after(?INTERVAL, self(), check_nodes),
    {ok, nil}.


terminate(_Reason, _St) ->
    ok.


handle_call(status, _From, St) ->
    case missing_servers() of
        [] ->
            {reply, ok, St};
        Missing ->
            {reply, {waiting, length(Missing)}, St}
    end;

handle_call(Msg, _From, St) ->
    twig:log(notice, "~s ignored_call ~w", [?MODULE, Msg]),
    {reply, ignored, St}.


handle_cast(Msg, St) ->
    twig:log(notice, "~s ignored_cast ~w", [?MODULE, Msg]),
    {noreply, St}.


handle_info({nodeup, _}, St) ->
    start_rexi_servers(),
    {noreply, St};

handle_info({nodedown, _}, St) ->
    {noreply, St};

handle_info(check_nodes, St) ->
    start_rexi_servers(),
    erlang:send_after(?INTERVAL, self(), check_nodes),
    {noreply, St};

handle_info(Msg, St) ->
    twig:log(notice, "~s ignored_info ~w", [?MODULE, Msg]),
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


start_rexi_servers() ->
    lists:foreach(fun(Id) ->
        {ok, _} = start_rexi_server(Id)
    end, missing_servers()).


missing_servers() ->
    ServerIds = [rexi_utils:server_id(Node) || Node <- [node() | nodes()]],
    ChildIds = [Id || {Id, _, _, _} <- supervisor:which_children(?SUP_MODULE)],
    ServerIds -- ChildIds.


start_rexi_server(ChildId) ->
    ChildSpec = {
        ChildId,
        {rexi_server, start_link, [ChildId]},
        permanent,
        brutal_kill,
        worker,
        [?CHILD_MODULE]
    },
    case supervisor:start_child(?SUP_MODULE, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        Else ->
            erlang:error(Else)
    end.
