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

% This module is similar to Ibrowse's ibrowse_lb.erl (load balancer) module.
% The main differences are:
%
% 1) Several HTTP connection pools can be spawned. This is important for
%    replications, as each replication can have its own pool which allows
%    for better error isolation - connections (and their pipelines) are not
%    shared between different replications;
%
% 2) The caller can request both pipelined connections and non-pipelined
%    connections.
%
-module(couch_httpc_pool).
-behaviour(gen_server).

% public API
-export([start_link/2, stop/1]).
-export([get_piped_worker/1]).
-export([get_worker/1, release_worker/2]).

% gen_server API
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-include("couch_db.hrl").
-include("../ibrowse/ibrowse.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3
]).

-record(state, {
    url,
    ssl_options,
    max_piped_workers,
    pipeline_size,
    piped_workers,
    used_piped_workers = 0,
    free_workers = [],  % free workers (connections) without pipeline
    busy_workers = []   % busy workers (connections) without pipeline
}).


start_link(BaseUrl, Options) ->
    gen_server:start_link(?MODULE, {BaseUrl, Options}, []).


stop(Pool) ->
    ok = gen_server:call(Pool, stop, infinity).


get_piped_worker(Pool) ->
    gen_server:call(Pool, get_piped_worker, infinity).


get_worker(Pool) ->
    gen_server:call(Pool, get_worker, infinity).


% Only workers without a pipeline need to be released.
release_worker(Pool, Worker) ->
    ok = gen_server:call(Pool, {release_worker, Worker}, infinity).


init({BaseUrl, Options}) ->
    process_flag(trap_exit, true),
    State = #state{
        url = BaseUrl,
        ssl_options = get_value(ssl_options, Options, []),
        pipeline_size = get_value(pipeline_size, Options),
        max_piped_workers = get_value(max_piped_connections, Options),
        piped_workers = ets:new(httpc_pool, [ordered_set, public])
    },
    {ok, State}.


handle_call(get_piped_worker, _From,
    #state{piped_workers = WorkersEts, max_piped_workers = Max,
        used_piped_workers = Used, url = Url,
        ssl_options = SslOptions} = State) when Used < Max ->
    {ok, Worker} = ibrowse_http_client:start_link({WorkersEts, Url,
        {SslOptions, SslOptions =/= []}}),
    true = ets:insert(WorkersEts, {{1, Worker}, []}),
    {reply, {ok, Worker}, State#state{used_piped_workers = Used + 1}};

handle_call(get_piped_worker, _From,
    #state{piped_workers = WorkersEts, pipeline_size = PipeSize} = State) ->
    case ets:first(WorkersEts) of
	{NumSessions, Worker} when NumSessions < PipeSize ->
	    true = ets:delete(WorkersEts, {NumSessions, Worker}),
	    true = ets:insert(WorkersEts, {{NumSessions + 1, Worker}, []}),
        {reply, {ok, Worker}, State};
	_ ->
	    {reply, retry_later, State}
    end;

handle_call(get_worker, _From, #state{
        free_workers = [], busy_workers = Busy,
        url = #url{host = Host, port = Port}} = State) ->
    {ok, Worker} = ibrowse_http_client:start_link({Host, Port}),
    {reply, {ok, Worker}, State#state{busy_workers = [Worker | Busy]}};

handle_call(get_worker, _From, #state{
        free_workers = [Worker | RestFree], busy_workers = Busy} = State) ->
    {reply, {ok, Worker}, State#state{
        busy_workers = [Worker | Busy], free_workers = RestFree}};

handle_call({release_worker, Worker}, _From, #state{
        free_workers = Free, busy_workers = Busy} = State) ->
    case Busy -- [Worker] of
    Busy ->
        {reply, ok, State};
    Busy2 ->
        {reply, ok, State#state{
            busy_workers = Busy2, free_workers = [Worker | Free]}}
    end;

handle_call(stop, _From, #state{piped_workers = WorkersEts,
        free_workers = Free, busy_workers = Busy} = State) ->
    ets:foldl(
        fun({{_, W}, _}, _) -> ibrowse_http_client:stop(W) end, ok, WorkersEts),
    lists:foreach(fun ibrowse_http_client:stop/1, Free),
    lists:foreach(fun ibrowse_http_client:stop/1, Busy),
    {stop, normal, ok, State}.


handle_cast(Msg, State) ->
    {stop, {unexpected_cast, Msg}, State}.


handle_info({'EXIT', Pid, _Reason}, #state{
        piped_workers = WorkersEts, used_piped_workers = Used,
        busy_workers = Busy, free_workers = Free} = State) ->
    case Free -- [Pid] of
    Free ->
        case Busy -- [Pid] of
        Busy ->
            true = ets:match_delete(WorkersEts, {{'_', Pid}, '_'}),
            {noreply, State#state{used_piped_workers = Used - 1}};
        Busy2 ->
            {noreply, State#state{busy_workers = Busy2}}
        end;
    Free2 ->
        {noreply, State#state{free_workers = Free2}}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.
