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
-module(rexi_governor).

-behaviour(gen_server).

%  gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pids = ets:new(pids, [set]),
                spawn_max = 10000,
                spawn_cnt = 0,
                drop_cnt = 0}).

init([PidSpawnMax]) ->
    {ok, #state{spawn_max = PidSpawnMax}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({spawn_and_track, Dest, Msg},
            #state{pids = Pids,
                   spawn_max = SpawnMax,
                   spawn_cnt = SC,
                   drop_cnt = DC} = State) ->
    {NewSC, NewDC} =
    case ets:info(Pids, size) < SpawnMax of
    true ->
        {Pid, Ref} = spawn_monitor(erlang, send, [Dest, Msg]),
        ets:insert(Pids, {Pid, Ref}),
        {SC + 1, DC};
    false ->
        % drop message on floor
        {SC, DC + 1}
    end,
    {noreply, State#state{spawn_cnt = NewSC, drop_cnt = NewDC}};

handle_cast(nodeout, #state{pids = Pids} = State) ->
    % kill all the pids
    ets:foldl(fun({P, _Ref}, Acc) ->
                  exit(P, kill),
                  Acc
              end, [], Pids),
    ets:delete_all_objects(Pids),
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, normal},
            #state{pids = Pids} = State) ->
    ets:delete(Pids, Pid),
    {noreply, State};

handle_info({'DOWN', _, process, _Pid, killed}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
