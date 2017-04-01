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

-module(mem3_sync_nodes).
-behaviour(gen_server).
-vsn(1).


-export([start_link/0]).
-export([add/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([monitor_sync/1]).


-record(st, {
    tid
}).


-record(job, {
    nodes,
    pid,
    retry
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


add(Nodes) ->
    gen_server:cast(?MODULE, {add, Nodes}).


init([]) ->
    {ok, #st{
        tid = ets:new(?MODULE, [set, protected, {keypos, #job.nodes}])
    }}.


terminate(_Reason, St) ->
    [exit(Pid, kill) || #job{pid=Pid} <- ets:tab2list(St#st.tid)],
    ok.


handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, invalid_call, St}.


handle_cast({add, Nodes}, #st{tid=Tid}=St) ->
    case ets:lookup(Tid, Nodes) of
        [] ->
            Pid = start_sync(Nodes),
            ets:insert(Tid, #job{nodes=Nodes, pid=Pid, retry=false});
        [#job{retry=false}=Job] ->
            ets:insert(Tid, Job#job{retry=true});
        _ ->
            ok
    end,
    {noreply, St};

handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info({'DOWN', _, _, _, {sync_done, Nodes}}, #st{tid=Tid}=St) ->
    case ets:lookup(Tid, Nodes) of
        [#job{retry=true}=Job] ->
            Pid = start_sync(Nodes),
            ets:insert(Tid, Job#job{pid=Pid, retry=false});
        _ ->
            ets:delete(Tid, Nodes)
    end,
    {noreply, St};

handle_info({'DOWN', _, _, _, {sync_error, Nodes}}, #st{tid=Tid}=St) ->
    Pid = start_sync(Nodes),
    ets:insert(Tid, #job{nodes=Nodes, pid=Pid, retry=false}),
    {noreply, St};

handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


start_sync(Nodes) ->
    {Pid, _} = spawn_monitor(?MODULE, monitor_sync, [Nodes]),
    Pid.


monitor_sync(Nodes) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(mem3_sync, initial_sync, [Nodes]),
    receive
        {'EXIT', Pid, normal} ->
            exit({sync_done, Nodes});
        _ ->
            exit({sync_error, Nodes})
    end.

