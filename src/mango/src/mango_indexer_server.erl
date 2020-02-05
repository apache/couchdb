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

-module(mango_indexer_server).


-behaviour(gen_server).


-export([
    start_link/0
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(MAX_WORKERS, 1).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    mango_jobs:set_timeout(),
    St = #{
        workers => #{},
        max_workers => max_workers()
    },
    {ok, spawn_workers(St)}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'EXIT', Pid, Reason}, St) ->
    #{workers := Workers} = St,
    case maps:is_key(Pid, Workers) of
        true ->
            if Reason == normal -> ok; true ->
                LogMsg = "~p : indexer process ~p exited with ~p",
                couch_log:error(LogMsg, [?MODULE, Pid, Reason])
            end,
            NewWorkers = maps:remove(Pid, Workers),
            {noreply, spawn_workers(St#{workers := NewWorkers})};
        false ->
            LogMsg = "~p : unknown process ~p exited with ~p",
            couch_log:error(LogMsg, [?MODULE, Pid, Reason]),
            {stop, {unknown_pid_exit, Pid}, St}
    end;

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


spawn_workers(St) ->
    #{
        workers := Workers,
        max_workers := MaxWorkers
    } = St,
    case maps:size(Workers) < MaxWorkers of
        true ->
            Pid = mango_jobs_indexer:spawn_link(),
            NewSt = St#{workers := Workers#{Pid => true}},
            spawn_workers(NewSt);
        false ->
            St
    end.


max_workers() ->
    config:get_integer("mango", "max_workers", ?MAX_WORKERS).
