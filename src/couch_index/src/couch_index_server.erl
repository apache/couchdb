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

-module(couch_index_server).
-behaviour(gen_server).
-behaviour(config_listener).

-vsn(2).

-export([start_link/0, validate/2, get_index/4, get_index/3, get_index/2, close/1]).
-export([set_committing/2, set_compacting/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

% Exported for callbacks
-export([
    handle_config_change/5,
    handle_config_terminate/3,
    handle_db_event/3
]).

-include_lib("couch/include/couch_db.hrl").

-define(BY_SIG, couchdb_indexes_by_sig).
-define(BY_PID, couchdb_indexes_by_pid).
-define(BY_DB, couchdb_indexes_by_db).
-define(RELISTEN_DELAY, 5000).
-define(MAX_INDICES_OPEN, 500).

-record(st, {
    lru=couch_lru:new(fun maybe_close_index/1),
    open=0,
    max_open=?MAX_INDICES_OPEN,
    root_dir
}).

-record(entry, {
    name,
    pid,
    locked=false,
    committing=false,
    compacting=false,
    waiters=undefined
}).

close(Mon) ->
    erlang:demonitor(Mon, [flush]),
    ok.

maybe_close_lru_view(#st{open=Open, max_open=Max}=State) when Open =< Max ->
    {ok, State};
maybe_close_lru_view(State) ->
    #st{lru=Lru, open=Open} = State,
    case couch_lru:close(Lru) of
        false ->
            {ok, State};
        {true, NewLru} ->
            maybe_close_lru_view(State#st{lru=NewLru, open=Open-1})
    end.

is_idle(Pid) ->
    case erlang:process_info(Pid, monitored_by) of
        undefined ->
            true;
        {monitored_by, Pids} ->
            [] =:= Pids -- [whereis(couch_stats_process_tracker)]
    end.

set_compacting(Idx, IsCompacting) ->
    gen_server:call(?MODULE, {compacting, Idx, IsCompacting}, infinity).

set_committing(Pid, IsCommitting) ->
    gen_server:call(?MODULE, {committing, Pid, IsCommitting}, infinity).

maybe_close_index({DbName, DDocId, Sig}) ->
    case ets:update_element(?BY_SIG, {DbName, Sig}, {#entry.locked, true}) of
        true ->
            case ets:lookup(?BY_SIG, {DbName, Sig}) of
                [#entry{pid=Pid, committing=false, compacting=false}] ->
                    case is_idle(Pid) of
                        true ->
                            rem_from_ets(DbName, Sig, DDocId, Pid),
                            couch_index:stop(Pid),
                            {true, true};
                        false ->
                            ets:update_element(?BY_SIG, {DbName, Sig}, {#entry.locked, false}),
                            couch_stats:increment_counter([couchdb, couch_index_server, lru_skip]),
                            {false, false}
                    end;
                _ ->
                    ets:update_element(?BY_SIG, {DbName, Sig}, {#entry.locked, false}),
                    couch_stats:increment_counter([couchdb, couch_index_server, lru_skip]),
                    {false, false}
            end;
        false ->
            {false, true}
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


validate(DbName, DDoc) ->
    LoadModFun = fun
        ({ModNameList, "true"}) ->
            try
                [list_to_existing_atom(ModNameList)]
            catch error:badarg ->
                []
            end;
        ({_ModNameList, _Enabled}) ->
            []
    end,
    ValidateFun = fun
        (ModName) ->
            ModName:validate(DbName, DDoc)
    end,
    EnabledIndexers = lists:flatmap(LoadModFun, config:get("indexers")),
    lists:foreach(ValidateFun, EnabledIndexers).


get_index(Module, #db{name = <<"shards/", _/binary>> = DbName}, DDoc) ->
    case is_record(DDoc, doc) of
        true -> get_index(Module, DbName, DDoc, nil);
        false -> get_index(Module, DbName, DDoc)
    end;
get_index(Module, <<"shards/", _/binary>> = DbName, DDoc) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        exit(fabric:open_doc(mem3:dbname(DbName), DDoc, [ejson_body, ?ADMIN_CTX]))
    end),
    receive {'DOWN', Ref, process, Pid, {ok, Doc}} ->
        get_index(Module, DbName, Doc, nil);
    {'DOWN', Ref, process, Pid, Error} ->
        Error
    after 61000 ->
        erlang:demonitor(Ref, [flush]),
        {error, timeout}
    end;

get_index(Module, DbName, DDoc) ->
    get_index(Module, DbName, DDoc, nil).


get_index(Module, DbName, DDoc, Fun) when is_binary(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        get_index(Module, Db, DDoc, Fun)
    end);
get_index(Module, Db, DDoc, Fun) when is_binary(DDoc) ->
    case couch_db:open_doc(Db, DDoc, [ejson_body, ?ADMIN_CTX]) of
        {ok, Doc} -> get_index(Module, Db, Doc, Fun);
        Error -> Error
    end;
get_index(Module, Db, DDoc, Fun) when is_function(Fun, 1) ->
    {ok, InitState} = Module:init(Db, DDoc),
    {ok, FunResp} = Fun(InitState),
    {ok, Pid, Monitor} = get_index(Module, InitState),
    {ok, Pid, Monitor, FunResp};
get_index(Module, Db, DDoc, _Fun) ->
    {ok, InitState} = Module:init(Db, DDoc),
    get_index(Module, InitState).


get_index(Module, IdxState) ->
    DbName = Module:get(db_name, IdxState),
    Sig = Module:get(signature, IdxState),
    case ets:lookup(?BY_SIG, {DbName, Sig}) of
        [#entry{pid=Pid, locked=false}] when is_pid(Pid) ->
            Monitor = erlang:monitor(process, Pid),
            {ok, Pid, Monitor};
        _ ->
            Args = {Module, IdxState, DbName, Sig},
            case gen_server:call(?MODULE, {get_index, Args}, infinity) of
                {ok, Pid} ->
                    Monitor = erlang:monitor(process, Pid),
                    {ok, Pid, Monitor};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, couch_index_util:root_dir()),
    ets:new(?BY_SIG, [protected, set, named_table, {keypos, #entry.name}]),
    ets:new(?BY_PID, [protected, set, named_table]),
    ets:new(?BY_DB, [protected, bag, named_table]),
    couch_event:link_listener(?MODULE, handle_db_event, nil, [all_dbs]),
    RootDir = couch_index_util:root_dir(),
    MaxIndicesOpen = config:get_integer("couchdb", "max_indices_open", ?MAX_INDICES_OPEN),
    couch_file:init_delete_dir(RootDir),
    {ok, #st{root_dir=RootDir, max_open=MaxIndicesOpen}}.


terminate(_Reason, _State) ->
    Pids = [Pid || {Pid, _} <- ets:tab2list(?BY_PID)],
    lists:map(fun couch_util:shutdown_sync/1, Pids),
    ok.


handle_call({get_index, {_Mod, _IdxState, DbName, Sig}=Args}, From, State) ->
    case ets:lookup(?BY_SIG, {DbName, Sig}) of
        [] ->
            {ok, NewState} = maybe_close_lru_view(State#st{open=(State#st.open)+1}),
            spawn_link(fun() -> new_index(Args) end),
            ets:insert(?BY_SIG, #entry{name={DbName, Sig}, waiters=[From]}),
            {noreply, NewState};
        [#entry{waiters=Waiters}=Entry] when is_list(Waiters) ->
            ets:insert(?BY_SIG, Entry#entry{waiters=[From | Waiters]}),
            {noreply, State};
        [#entry{pid=Pid}] when is_pid(Pid) ->
            {reply, {ok, Pid}, State}
    end;
handle_call({async_open, {DbName, DDocId, Sig}, {ok, Pid}}, _From, State) ->
    [#entry{waiters=Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    NewLru = couch_lru:insert({DbName, DDocId, Sig}, State#st.lru),
    [gen_server:reply(From, {ok, Pid}) || From <- Waiters],
    link(Pid),
    add_to_ets(DbName, Sig, DDocId, Pid),
    {reply, ok, State#st{lru=NewLru}};
handle_call({async_error, {DbName, _DDocId, Sig}, Error}, _From, State) ->
    [#entry{waiters=Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, Error) || From <- Waiters],
    ets:delete(?BY_SIG, {DbName, Sig}),
    {reply, ok, State#st{open=(State#st.open)-1}};
handle_call({compacting, Pid, IsCompacting}, _From, State) ->
    case ets:lookup(?BY_PID, Pid) of
        [{Pid, {DbName, Sig}}] ->
            ets:update_element(?BY_SIG, {DbName, Sig}, {#entry.compacting, IsCompacting});
        [] ->
            ok
    end,
    {reply, ok, State};
handle_call({committing, Pid, IsCommitting}, _From, State) ->
    case ets:lookup(?BY_PID, Pid) of
        [{Pid, {DbName, Sig}}] ->
            ets:update_element(?BY_SIG, {DbName, Sig}, {#entry.committing, IsCommitting});
        [] ->
            ok
    end,
    {reply, ok, State};
handle_call({reset_indexes, DbName}, _From, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {reply, ok, State};
handle_call(get_open_count, _From, State) ->
    {reply, State#st.open, State}.


handle_cast({reset_indexes, DbName}, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {noreply, State};
handle_cast(close_indexes, State) ->
    {ok, NewState} = maybe_close_lru_view(State),
    {noreply, NewState}.

handle_info({'EXIT', Pid, Reason}, State) ->
    NewState = case ets:lookup(?BY_PID, Pid) of
        [{Pid, {DbName, Sig}}] ->
            rem_from_ets(DbName, Sig, Pid),
            State#st{open=(State#st.open)-1};
        [] when Reason /= normal ->
            exit(Reason);
        _Else ->
            State
    end,
    {noreply, NewState};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, couch_index_util:root_dir()),
    {noreply, State};
handle_info(Msg, State) ->
    couch_log:warning("~p did not expect ~p", [?MODULE, Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_config_change("couchdb", "index_dir", RootDir, _, #st{root_dir=RootDir}=State) ->
    {ok, State};
handle_config_change("couchdb", "view_index_dir", RootDir, _, #st{root_dir=RootDir}=State) ->
    {ok, State};
handle_config_change("couchdb", "index_dir", _, _, _) ->
    exit(whereis(couch_index_server), config_change),
    remove_handler;
handle_config_change("couchdb", "view_index_dir", _, _, _) ->
    exit(whereis(couch_index_server), config_change),
    remove_handler;
handle_config_change("couchdb", "max_indices_open", Max, _, State) ->
    {ok, State#st{max_open=list_to_integer(Max)}};
handle_config_change(_, _, _, _, State) ->
    {ok, State}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener),
    {ok, couch_index_util:root_dir()}.

new_index({Mod, IdxState, DbName, Sig}) ->
    DDocId = Mod:get(idx_name, IdxState),
    case couch_index:start_link({Mod, IdxState}) of
        {ok, Pid} ->
            ok = gen_server:call(
                ?MODULE, {async_open, {DbName, DDocId, Sig}, {ok, Pid}}),
            unlink(Pid);
        Error ->
            ok = gen_server:call(
                ?MODULE, {async_error, {DbName, DDocId, Sig}, Error})
    end.


reset_indexes(DbName, Root) ->
    % shutdown all the updaters and clear the files, the db got changed
    Fun = fun({_, {DDocId, Sig}}) ->
        [#entry{pid=Pid}] = ets:lookup(?BY_SIG, {DbName, Sig}),
        MRef = erlang:monitor(process, Pid),
        gen_server:cast(Pid, delete),
        receive {'DOWN', MRef, _, _, _} -> ok end,
        rem_from_ets(DbName, Sig, DDocId, Pid)
    end,
    lists:foreach(Fun, ets:lookup(?BY_DB, DbName)),
    Path = couch_index_util:index_dir("", DbName),
    couch_file:nuke_dir(Root, Path).


add_to_ets(DbName, Sig, DDocId, Pid) ->
    ets:insert(?BY_SIG, #entry{name={DbName, Sig}, pid=Pid}),
    ets:insert(?BY_PID, {Pid, {DbName, Sig}}),
    ets:insert(?BY_DB, {DbName, {DDocId, Sig}}).


rem_from_ets(DbName, Sig, Pid) ->
    [{DbName, {DDocId, Sig}}] =
        ets:match_object(?BY_DB, {DbName, {'$1', Sig}}),
    rem_from_ets(DbName, Sig, DDocId, Pid).


rem_from_ets(DbName, Sig, DDocId, Pid) ->
    ets:delete(?BY_SIG, {DbName, Sig}),
    ets:delete(?BY_PID, Pid),
    ets:delete_object(?BY_DB, {DbName, {DDocId, Sig}}).


handle_db_event(DbName, created, St) ->
    gen_server:cast(?MODULE, {reset_indexes, DbName}),
    {ok, St};
handle_db_event(DbName, deleted, St) ->
    gen_server:cast(?MODULE, {reset_indexes, DbName}),
    {ok, St};
handle_db_event(DbName, {ddoc_updated, DDocId}, St) ->
    lists:foreach(fun({_DbName, {_DDocId, Sig}}) ->
        case ets:lookup(?BY_SIG, {DbName, Sig}) of
            [#entry{pid=IndexPid}] ->
                (catch gen_server:cast(IndexPid, ddoc_updated));
            [] ->
                ok
        end
    end, ets:match_object(?BY_DB, {DbName, {DDocId, '$1'}})),
    {ok, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.
