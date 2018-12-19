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
-export([get_worker/1, release_worker/2, release_worker_sync/2]).

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
    workers = [],
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

release_worker_sync(Pool, Worker) ->
    ok = gen_server:call(Pool, {release_worker_sync, Worker}).

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
        workers = Workers
    } = State,
    case length(Workers) >= Limit of
    true ->
        {noreply, State#state{waiting = queue:in(From, Waiting)}};
    false ->
        % If the call to acquire fails, the worker pool will crash with a
        % badmatch.
        {ok, Worker} = couch_replicator_connection:acquire(Url),
        NewState = State#state{
            workers = [Worker | Workers],
            callers = monitor_client(Callers, Worker, From)
        },
        {reply, {ok, Worker}, NewState}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({release_worker_sync, Worker}, _From, State) ->
    {reply, ok, release_worker_internal(Worker, State)}.

handle_cast({release_worker, Worker}, State) ->
    {noreply, release_worker_internal(Worker, State)}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{
        url = Url,
        workers = Workers,
        waiting = Waiting,
        callers = Callers
    } = State,
    NewCallers0 = demonitor_client(Callers, Pid),
    case Workers -- [Pid] of
        Workers ->
            {noreply, State#state{callers = NewCallers0}};
        Workers2 ->
            case queue:out(Waiting) of
                {empty, _} ->
                    {noreply, State#state{workers = Workers2,
                        callers = NewCallers0}};
                {{value, From}, Waiting2} ->
                    {ok, Worker} = couch_replicator_connection:acquire(Url),
                    NewCallers1 = monitor_client(NewCallers0, Worker, From),
                    gen_server:reply(From, {ok, Worker}),
                    NewState = State#state{
                        workers = [Worker | Workers2],
                        waiting = Waiting2,
                        callers = NewCallers1
                    },
                    {noreply, NewState}
            end
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


terminate(_Reason, _State) ->
    ok.

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

release_worker_internal(Worker, State) ->
    #state{waiting = Waiting, callers = Callers} = State,
    NewCallers0 = demonitor_client(Callers, Worker),
    case is_process_alive(Worker) andalso
        lists:member(Worker, State#state.workers) of
    true ->
        Workers = case queue:out(Waiting) of
        {empty, Waiting2} ->
            NewCallers1 = NewCallers0,
            couch_replicator_connection:release(Worker),
            State#state.workers -- [Worker];
        {{value, From}, Waiting2} ->
            NewCallers1 = monitor_client(NewCallers0, Worker, From),
            gen_server:reply(From, {ok, Worker}),
            State#state.workers
        end,
        NewState = State#state{
           workers = Workers,
           waiting = Waiting2,
           callers = NewCallers1
        },
        NewState;
   false ->
        State#state{callers = NewCallers0}
   end.
