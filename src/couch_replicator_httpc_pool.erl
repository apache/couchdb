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

-module(couch_replicator_httpc_pool).
-behaviour(gen_server).
-vsn(1).

% public API
-export([start_link/2, stop/1]).
-export([get_worker/1, release_worker/2]).

% gen_server API
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-include_lib("couch/include/couch_db.hrl").

-import(couch_util, [
    get_value/2
]).

-record(state, {
    url,
    limit,                  % max # of workers allowed
    free = [],              % free workers (connections)
    busy = [],              % busy workers (connections)
    waiting = queue:new(),  % blocked clients waiting for a worker
    callers = []            % clients who've been given a worker
}).


start_link(Url, Options) ->
    gen_server:start_link(?MODULE, {Url, Options}, []).

stop(Pool) ->
    ok = gen_server:call(Pool, stop, infinity).


get_worker(Pool) ->
    {ok, _Worker} = gen_server:call(Pool, get_worker, infinity).


release_worker(Pool, Worker) ->
    ok = gen_server:cast(Pool, {release_worker, Worker}).


init({Url, Options}) ->
    process_flag(trap_exit, true),
    State = #state{
        url = Url,
        limit = get_value(max_connections, Options)
    },
    {ok, State}.


handle_call(get_worker, From, State) ->
    #state{
        waiting = Waiting,
        callers = Callers,
        url = Url,
        limit = Limit,
        busy = Busy,
        free = Free
    } = State,
    case length(Busy) >= Limit of
    true ->
        {noreply, State#state{waiting = queue:in(From, Waiting)}};
    false ->
        case Free of
        [] ->
           {ok, Worker} = ibrowse:spawn_link_worker_process(Url),
           Free2 = Free;
        [Worker | Free2] ->
           ok
        end,
        NewState = State#state{
            free = Free2,
            busy = [Worker | Busy],
            callers = monitor_client(Callers, Worker, From)
        },
        {reply, {ok, Worker}, NewState}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_cast({release_worker, Worker}, State) ->
    #state{waiting = Waiting, callers = Callers} = State,
    NewCallers0 = demonitor_client(Callers, Worker),
    case is_process_alive(Worker) andalso
        lists:member(Worker, State#state.busy) of
    true ->
        case queue:out(Waiting) of
        {empty, Waiting2} ->
            NewCallers1 = NewCallers0,
            Busy2 = State#state.busy -- [Worker],
            Free2 = [Worker | State#state.free];
        {{value, From}, Waiting2} ->
            NewCallers1 = monitor_client(NewCallers0, Worker, From),
            gen_server:reply(From, {ok, Worker}),
            Busy2 = State#state.busy,
            Free2 = State#state.free
        end,
        NewState = State#state{
           busy = Busy2,
           free = Free2,
           waiting = Waiting2,
           callers = NewCallers1
        },
        {noreply, NewState};
   false ->
        {noreply, State#state{callers = NewCallers0}}
   end.

handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{
        url = Url,
        busy = Busy,
        free = Free,
        waiting = Waiting,
        callers = Callers
    } = State,
    NewCallers0 = demonitor_client(Callers, Pid),
    case Free -- [Pid] of
    Free ->
        case Busy -- [Pid] of
        Busy ->
            {noreply, State#state{callers = NewCallers0}};
        Busy2 ->
            case queue:out(Waiting) of
            {empty, _} ->
                {noreply, State#state{busy = Busy2, callers = NewCallers0}};
            {{value, From}, Waiting2} ->
                {ok, Worker} = ibrowse:spawn_link_worker_process(Url),
                NewCallers1 = monitor_client(NewCallers0, Worker, From),
                gen_server:reply(From, {ok, Worker}),
                NewState = State#state{
                    busy = [Worker | Busy2],
                    waiting = Waiting2,
                    callers = NewCallers1
                },
                {noreply, NewState}
            end
        end;
    Free2 ->
        {noreply, State#state{free = Free2, callers = NewCallers0}}
    end;

handle_info({'DOWN', Ref, process, _, _}, #state{callers = Callers} = State) ->
    case lists:keysearch(Ref, 2, Callers) of
        {value, {Worker, Ref}} ->
            handle_cast({release_worker, Worker}, State);
        false ->
            {noreply, State}
    end.

code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.


terminate(_Reason, State) ->
    lists:foreach(fun ibrowse_http_client:stop/1, State#state.free),
    lists:foreach(fun ibrowse_http_client:stop/1, State#state.busy).

monitor_client(Callers, Worker, {ClientPid, _}) ->
    [{Worker, erlang:monitor(process, ClientPid)} | Callers].

demonitor_client(Callers, Worker) ->
    case lists:keysearch(Worker, 1, Callers) of
        {value, {Worker, MonRef}} ->
            erlang:demonitor(MonRef, [flush]),
            lists:keydelete(Worker, 1, Callers);
        false ->
            Callers
    end.
