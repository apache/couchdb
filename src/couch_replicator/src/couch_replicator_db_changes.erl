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

-module(couch_replicator_db_changes).

-behaviour(gen_server).

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
   notify_cluster_event/2
]).

-record(state, {
   event_listener :: pid(),
   mdb_changes :: pid() | nil
}).


-spec notify_cluster_event(pid(), {cluster, any()}) -> ok.
notify_cluster_event(Server, {cluster, _} = Event) ->
    gen_server:cast(Server, Event).


-spec start_link() ->
    {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    EvtPid = couch_replicator_clustering:link_cluster_event_listener(?MODULE,
        notify_cluster_event, [self()]),
    State = #state{event_listener = EvtPid, mdb_changes = nil},
    case couch_replicator_clustering:is_stable() of
        true ->
            {ok, restart_mdb_changes(State)};
        false ->
            {ok, State}
    end.


terminate(_Reason, _State) ->
    ok.


handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.


handle_cast({cluster, unstable}, State) ->
    {noreply, stop_mdb_changes(State)};

handle_cast({cluster, stable}, State) ->
    {noreply, restart_mdb_changes(State)}.


handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec restart_mdb_changes(#state{}) -> #state{}.
restart_mdb_changes(#state{mdb_changes = nil} = State) ->
    Suffix = <<"_replicator">>,
    CallbackMod = couch_replicator_doc_processor,
    Options = [skip_ddocs],
    {ok, Pid} = couch_multidb_changes:start_link(Suffix, CallbackMod, nil,
        Options),
    couch_stats:increment_counter([couch_replicator, db_scans]),
    couch_log:notice("Started replicator db changes listener ~p", [Pid]),
    State#state{mdb_changes = Pid};

restart_mdb_changes(#state{mdb_changes = _Pid} = State) ->
    restart_mdb_changes(stop_mdb_changes(State)).


-spec stop_mdb_changes(#state{}) -> #state{}.
stop_mdb_changes(#state{mdb_changes = nil} = State) ->
    State;
stop_mdb_changes(#state{mdb_changes = Pid} = State) ->
    couch_log:notice("Stopping replicator db changes listener ~p", [Pid]),
    unlink(Pid),
    exit(Pid, kill),
    State#state{mdb_changes = nil}.
