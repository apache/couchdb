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

-module(couch_httpc_pool).
-behaviour(gen_server).

% public API
-export([start_link/2, stop/1]).
-export([get_worker/1, release_worker/2]).

% gen_server API
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-include("couch_db.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).

-record(state, {
    url,
    limit,                  % max # of workers allowed
    free = [],              % free workers (connections)
    busy = [],              % busy workers (connections)
    waiting = queue:new()   % blocked clients waiting for a worker
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


handle_call(get_worker, From, #state{waiting = Waiting} = State) ->
    #state{url = Url, limit = Limit, busy = Busy, free = Free} = State,
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
        NewState = State#state{free = Free2, busy = [Worker | Busy]},
        {reply, {ok, Worker}, NewState}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_cast({release_worker, Worker}, #state{waiting = Waiting} = State) ->
    case is_process_alive(Worker) andalso
        lists:member(Worker, State#state.busy) of
    true ->
        case queue:out(Waiting) of
        {empty, Waiting2} ->
            Busy2 = State#state.busy -- [Worker],
            Free2 = [Worker | State#state.free];
        {{value, From}, Waiting2} ->
            gen_server:reply(From, {ok, Worker}),
            Busy2 = State#state.busy,
            Free2 = State#state.free
        end,
        NewState = State#state{
           busy = Busy2,
           free = Free2,
           waiting = Waiting2
        },
        {noreply, NewState};
   false ->
        {noreply, State}
   end.


handle_info({'EXIT', Pid, _Reason}, #state{busy = Busy, free = Free} = State) ->
    case Free -- [Pid] of
    Free ->
        case Busy -- [Pid] of
        Busy ->
            {noreply, State};
        Busy2 ->
            case queue:out(State#state.waiting) of
            {empty, _} ->
                {noreply, State#state{busy = Busy2}};
            {{value, From}, Waiting2} ->
                {ok, Worker} = ibrowse:spawn_link_worker_process(State#state.url),
                gen_server:reply(From, {ok, Worker}),
                {noreply, State#state{busy = [Worker | Busy2], waiting = Waiting2}}
            end
        end;
    Free2 ->
        {noreply, State#state{free = Free2}}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, State) ->
    lists:foreach(fun ibrowse_http_client:stop/1, State#state.free),
    lists:foreach(fun ibrowse_http_client:stop/1, State#state.busy).

