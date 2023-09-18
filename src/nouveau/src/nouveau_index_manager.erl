%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% index manager ensures only one process is updating a nouveau index at a time.
%% calling update_index will block until at least one attempt has been made to
%% make the index as current as the database at the time update_index was called.

-module(nouveau_index_manager).
-behaviour(gen_server).
-behaviour(config_listener).
-include("nouveau.hrl").

%% public api
-export([
    update_index/1
]).

%% gen_server bits
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

-export([handle_db_event/3]).

-define(BY_DBSIG, nouveau_by_dbsig).
-define(BY_REF, nouveau_by_ref).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_index(#index{} = Index) ->
    case nouveau:enabled() of
        true ->
            gen_server:call(?MODULE, {update, Index}, infinity);
        false ->
            {error, nouveau_not_enabled}
    end.

init(_) ->
    couch_util:set_mqd_off_heap(?MODULE),
    ets:new(?BY_DBSIG, [set, named_table]),
    ets:new(?BY_REF, [set, named_table]),
    couch_event:link_listener(?MODULE, handle_db_event, nil, [all_dbs]),
    configure_ibrowse(nouveau_util:nouveau_url()),
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, nil}.

handle_call({update, #index{} = Index0}, From, State) ->
    DbSig = {Index0#index.dbname, Index0#index.sig},
    case ets:lookup(?BY_DBSIG, DbSig) of
        [] ->
            {_IndexerPid, IndexerRef} = spawn_monitor(nouveau_index_updater, update, [Index0]),
            Queue = queue:in(From, queue:new()),
            true = ets:insert(?BY_DBSIG, {DbSig, Index0, Queue}),
            true = ets:insert(?BY_REF, {IndexerRef, DbSig});
        [{_DbSig, Index1, Queue}] ->
            ets:insert(?BY_DBSIG, {DbSig, Index1, queue:in(From, Queue)})
    end,
    {noreply, State};
handle_call(_Msg, _From, State) ->
    {reply, unexpected_msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', IndexerRef, process, _Pid, Reason}, State) ->
    case ets:lookup(?BY_REF, IndexerRef) of
        [] ->
            % not one of ours, somehow...
            {noreply, State};
        [{_, DbSig}] ->
            true = ets:delete(?BY_REF, IndexerRef),
            [{_, Index, Queue0}] = ets:lookup(?BY_DBSIG, DbSig),
            {{value, From}, Queue1} = queue:out(Queue0),
            case Reason of
                ok ->
                    gen_server:reply(From, ok);
                {error, Msg} ->
                    couch_log:error(
                        "~p: db:~s ddoc:~s index:~s failed with: ~p",
                        [
                            ?MODULE,
                            mem3:dbname(Index#index.dbname),
                            Index#index.ddoc_id,
                            Index#index.name,
                            Msg
                        ]
                    ),
                    gen_server:reply(From, {error, Msg})
            end,
            case queue:is_empty(Queue1) of
                true ->
                    true = ets:delete(?BY_DBSIG, DbSig);
                false ->
                    {_IndexerPid, NewIndexerRef} = spawn_monitor(nouveau_index_updater, update, [
                        Index
                    ]),
                    true = ets:insert(?BY_DBSIG, {DbSig, Index, Queue1}),
                    true = ets:insert(?BY_REF, {NewIndexerRef, DbSig})
            end,
            {noreply, State}
    end;
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_db_event(DbName, deleted, State) ->
    couch_log:notice("Deleting indexes for ~s as database was deleted", [DbName]),
    nouveau_api:delete_path(nouveau_util:index_name(DbName)),
    {ok, State};
handle_db_event(_DbName, _Event, State) ->
    {ok, State}.

handle_config_change("nouveau", "url", URL, _Persist, State) ->
    configure_ibrowse(URL),
    {ok, State};
handle_config_change(_Section, _Key, _Value, _Persist, State) ->
    {ok, State}.

handle_config_terminate(_Server, stop, _State) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(
        5000,
        whereis(?MODULE),
        restart_config_listener
    ).

configure_ibrowse(URL) ->
    #{host := Host, port := Port} = uri_string:parse(URL),
    ibrowse:set_max_sessions(
        Host,
        Port,
        nouveau_util:max_sessions()
    ),
    ibrowse:set_max_pipeline_size(
        Host,
        Port,
        nouveau_util:max_pipeline_size()
    ).
