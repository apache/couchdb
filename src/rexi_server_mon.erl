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
-vsn(1).


-export([
    start_link/1,
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


-define(INTERVAL, 60000).


start_link(ChildMod) ->
    Name = list_to_atom(lists:concat([ChildMod, "_mon"])),
    gen_server:start_link({local, Name}, ?MODULE, ChildMod, []).


status() ->
    gen_server:call(?MODULE, status).


init(ChildMod) ->
    net_kernel:monitor_nodes(true),
    erlang:send(self(), check_nodes),
    {ok, ChildMod}.


terminate(_Reason, _St) ->
    ok.


handle_call(status, _From, ChildMod) ->
    case missing_servers(ChildMod) of
        [] ->
            {reply, ok, ChildMod};
        Missing ->
            {reply, {waiting, length(Missing)}, ChildMod}
    end;

handle_call(Msg, _From, St) ->
    twig:log(notice, "~s ignored_call ~w", [?MODULE, Msg]),
    {reply, ignored, St}.


handle_cast(Msg, St) ->
    twig:log(notice, "~s ignored_cast ~w", [?MODULE, Msg]),
    {noreply, St}.


handle_info({nodeup, _}, ChildMod) ->
    start_servers(ChildMod),
    {noreply, ChildMod};

handle_info({nodedown, _}, St) ->
    {noreply, St};

handle_info(check_nodes, ChildMod) ->
    start_servers(ChildMod),
    erlang:send_after(?INTERVAL, self(), check_nodes),
    {noreply, ChildMod};

handle_info(Msg, St) ->
    twig:log(notice, "~s ignored_info ~w", [?MODULE, Msg]),
    {noreply, St}.


code_change(_OldVsn, nil, _Extra) ->
    {ok, rexi_server};
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


start_servers(ChildMod) ->
    lists:foreach(fun(Id) ->
        {ok, _} = start_server(ChildMod, Id)
    end, missing_servers(ChildMod)).


missing_servers(ChildMod) ->
    ServerIds = [list_to_atom(lists:concat([ChildMod, "_", Node]))
        || Node <- [node() | nodes()]],
    SupModule = sup_module(ChildMod),
    ChildIds = [Id || {Id, _, _, _} <- supervisor:which_children(SupModule)],
    ServerIds -- ChildIds.


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

sup_module(ChildMod) ->
    list_to_atom(lists:concat([ChildMod, "_sup"])).
