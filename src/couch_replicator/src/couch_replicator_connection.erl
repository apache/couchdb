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

-module(couch_replicator_connection).

-behavior(gen_server).
-behavior(config_listener).

-export([
    start_link/0
]).

-export([
   init/1,
   terminate/2,
   handle_call/3,
   handle_info/2,
   handle_cast/2,
   code_change/3
]).

-export([
   acquire/1,
   release/1
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-include_lib("ibrowse/include/ibrowse.hrl").

-define(DEFAULT_CLOSE_INTERVAL, 90000).
-define(RELISTEN_DELAY, 5000).


-record(state, {
    close_interval,
    timer
}).

-record(connection, {
    worker,
    host,
    port,
    mref
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    ?MODULE = ets:new(?MODULE, [named_table, public,
        {keypos, #connection.worker}]),
    ok = config:listen_for_changes(?MODULE, nil),
    Interval = config:get_integer("replicator", "connection_close_interval",
        ?DEFAULT_CLOSE_INTERVAL),
    Timer = erlang:send_after(Interval, self(), close_idle_connections),
    ibrowse:add_config([{inactivity_timeout, Interval}]),
    {ok, #state{close_interval=Interval, timer=Timer}}.


acquire(URL) when is_binary(URL) ->
    acquire(binary_to_list(URL));

acquire(URL0) ->
    URL = couch_util:url_strip_password(URL0),
    case gen_server:call(?MODULE, {acquire, URL}) of
        {ok, Worker} ->
            link(Worker),
            {ok, Worker};
        {error, all_allocated} ->
            {ok, Pid} = ibrowse:spawn_link_worker_process(URL),
            ok = gen_server:call(?MODULE, {create, URL, Pid}),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.


release(Worker) ->
    unlink(Worker),
    gen_server:cast(?MODULE, {release, Worker}).


handle_call({acquire, URL}, From, State) ->
    {Pid, _Ref} = From,
    case ibrowse_lib:parse_url(URL) of
        #url{host=Host, port=Port} ->
            Pat = #connection{host=Host, port=Port, mref=undefined, _='_'},
            case ets:match_object(?MODULE, Pat, 1) of
                '$end_of_table' ->
                    {reply, {error, all_allocated}, State};
                {[Worker], _Cont} ->
                    couch_stats:increment_counter([couch_replicator, connection,
                        acquires]),
                    ets:insert(?MODULE, Worker#connection{mref=monitor(process,
                        Pid)}),
                    {reply, {ok, Worker#connection.worker}, State}
            end;
        {error, invalid_uri} ->
            {reply, {error, invalid_uri}, State}
    end;

handle_call({create, URL, Worker}, From, State) ->
    {Pid, _Ref} = From,
    case ibrowse_lib:parse_url(URL) of
        #url{host=Host, port=Port} ->
            link(Worker),
            couch_stats:increment_counter([couch_replicator, connection,
                creates]),
            true = ets:insert_new(
                ?MODULE,
                #connection{host=Host, port=Port, worker=Worker,
                    mref=monitor(process, Pid)}
            ),
            {reply, ok, State}
    end.


handle_cast({release, WorkerPid}, State) ->
    couch_stats:increment_counter([couch_replicator, connection, releases]),
    case ets:lookup(?MODULE, WorkerPid) of
        [Worker] ->
            case Worker#connection.mref of
                MRef when is_reference(MRef) -> demonitor(MRef, [flush]);
                undefined -> ok
            end,
            ets:insert(?MODULE, Worker#connection{mref=undefined});
        [] ->
            ok
    end,
    {noreply, State};

handle_cast({connection_close_interval, V}, State) ->
    erlang:cancel_timer(State#state.timer),
    NewTimer = erlang:send_after(V, self(), close_idle_connections),
    ibrowse:add_config([{inactivity_timeout, V}]),
    {noreply, State#state{close_interval=V, timer=NewTimer}}.


% owner crashed
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    couch_stats:increment_counter([couch_replicator, connection,
        owner_crashes]),
    ets:match_delete(?MODULE, #connection{mref=Ref, _='_'}),
    {noreply, State};

% worker crashed
handle_info({'EXIT', Pid, Reason}, State) ->
    couch_stats:increment_counter([couch_replicator, connection,
        worker_crashes]),
    case ets:lookup(?MODULE, Pid) of
        [] ->
            ok;
        [Worker] ->
            #connection{host=Host, port=Port} = Worker,
            maybe_log_worker_death(Host, Port, Reason),
            case Worker#connection.mref of
                MRef when is_reference(MRef) -> demonitor(MRef, [flush]);
                undefined -> ok
            end,
            ets:delete(?MODULE, Pid)
    end,
    {noreply, State};

handle_info(close_idle_connections, State) ->
    #state{
        close_interval=Interval,
        timer=Timer
    } = State,
    Conns = ets:match_object(?MODULE, #connection{mref=undefined, _='_'}),
    lists:foreach(fun(Conn) ->
        couch_stats:increment_counter([couch_replicator, connection, closes]),
        delete_worker(Conn)
    end, Conns),
    erlang:cancel_timer(Timer),
    NewTimer = erlang:send_after(Interval, self(), close_idle_connections),
    {noreply, State#state{timer=NewTimer}};

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


maybe_log_worker_death(_Host, _Port, normal) ->
    ok;

maybe_log_worker_death(Host, Port, Reason) ->
    ErrMsg = "Replication connection to: ~p:~p died with reason ~p",
    couch_log:info(ErrMsg, [Host, Port, Reason]).


-spec delete_worker(#connection{}) -> ok.
delete_worker(Worker) ->
    ets:delete(?MODULE, Worker#connection.worker),
    unlink(Worker#connection.worker),
    spawn(fun() -> ibrowse_http_client:stop(Worker#connection.worker) end),
    ok.


handle_config_change("replicator", "connection_close_interval", V, _, S) ->
    ok = gen_server:cast(?MODULE, {connection_close_interval,
        list_to_integer(V)}),
    {ok, S};

handle_config_change(_, _, _, _, S) ->
    {ok, S}.


handle_config_terminate(_, stop, _) ->
    ok;

handle_config_terminate(_, _, _) ->
    Pid = whereis(?MODULE),
    erlang:send_after(?RELISTEN_DELAY, Pid, restart_config_listener).
