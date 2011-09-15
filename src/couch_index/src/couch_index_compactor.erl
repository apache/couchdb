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

-module(couch_index_compactor).
-behaviour(gen_server).


%% API
-export([start_link/2, run/2, monitor/1, cancel/1, is_running/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-record(st, {
    idx,
    mod,
    pid
}).


start_link(Index, Module) ->
    gen_server:start_link(?MODULE, {Index, Module}, []).


run(Pid, IdxState) ->
    gen_server:call(Pid, {compact, IdxState}).


monitor(Pid) ->
    case gen_server:call(Pid, get_pid) of
        {ok, undefined} -> {error, compaction_not_running};
        {ok, CPid} -> {ok, erlang:monitor(process, CPid)}
    end.


cancel(Pid) ->
    gen_server:call(Pid, cancel).


is_running(Pid) ->
    gen_server:call(Pid, is_running).


init({Index, Module}) ->
    process_flag(trap_exit, true),
    {ok, #st{idx=Index, mod=Module}}.


terminate(_Reason, State) ->
    couch_util:shutdown_sync(State#st.pid),
    ok.


handle_call({compact, _}, _From, #st{pid=Pid}=State) when is_pid(Pid) ->
    {reply, ok, State};
handle_call({compact, IdxState}, _From, #st{idx=Idx}=State) ->
    Pid = spawn_link(fun() -> compact(Idx, State#st.mod, IdxState) end),
    {reply, ok, State#st{pid=Pid}};
handle_call(get_pid, _From, State) ->
    {reply, {ok, State#st.pid}, State};
handle_call(cancel, _From, #st{pid=undefined}=State) ->
    {reply, ok, State};
handle_call(cancel, _From, #st{pid=Pid}=State) ->
    unlink(Pid),
    exit(Pid, kill),
    {reply, ok, State#st{pid=undefined}};
handle_call(is_running, _From, #st{pid=Pid}=State) when is_pid(Pid) ->
    {reply, true, State};
handle_call(is_running, _From, State) ->
    {reply, false, State}.


handle_cast(_Mesg, State) ->
    {stop, unknown_cast, State}.


handle_info({'EXIT', Pid, normal}, #st{pid=Pid}=State) ->
    {noreply, State#st{pid=undefined}};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, #st{idx=Pid}=State) ->
    {stop, normal, State};
handle_info(_Mesg, State) ->
    {stop, unknown_info, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


compact(Parent, Mod, IdxState) ->
    compact(Parent, Mod, IdxState, []).

compact(Idx, Mod, IdxState, Opts) ->
    {ok, NewIdxState} = Mod:compact(IdxState, Opts),
    case gen_server:call(Idx, {compacted, NewIdxState}) of
        recompact -> compact(Idx, Mod, NewIdxState, [recompact]);
        _ -> ok
    end.
