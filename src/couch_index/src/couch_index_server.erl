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

-export([start_link/0, validate/2, get_index/4, get_index/3, get_index/2]).

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

-record(st, {root_dir}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


validate(Db, DDoc) ->
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
            ModName:validate(Db, DDoc)
    end,
    EnabledIndexers = lists:flatmap(LoadModFun, config:get("indexers")),
    lists:foreach(ValidateFun, EnabledIndexers).


get_index(Module, <<"shards/", _/binary>> = DbName, DDoc)
        when is_record(DDoc, doc) ->
    get_index(Module, DbName, DDoc, nil);
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
get_index(Module, DbName, DDoc) when is_binary(DbName) ->
    get_index(Module, DbName, DDoc, nil);
get_index(Module, Db, DDoc) ->
    get_index(Module, couch_db:name(Db), DDoc).


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
    {ok, Pid} = get_index(Module, InitState),
    {ok, Pid, FunResp};
get_index(Module, Db, DDoc, _Fun) ->
    {ok, InitState} = Module:init(Db, DDoc),
    get_index(Module, InitState).


get_index(Module, IdxState) ->
    DbName = Module:get(db_name, IdxState),
    Sig = Module:get(signature, IdxState),
    case ets:lookup(?BY_SIG, {DbName, Sig}) of
        [{_, Pid}] when is_pid(Pid) ->
            DDocId = Module:get(idx_name, IdxState),
            case ets:match_object(?BY_DB, {DbName, {DDocId, Sig}}) of
                [] ->
                    Args = [Pid, DbName, DDocId, Sig],
                    gen_server:cast(?MODULE, {add_to_ets, Args});
                _ -> ok
            end,
            {ok, Pid};
        _ ->
            Args = {Module, IdxState, DbName, Sig},
            gen_server:call(?MODULE, {get_index, Args}, infinity)
    end.


init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, couch_index_util:root_dir()),
    ets:new(?BY_SIG, [protected, set, named_table]),
    ets:new(?BY_PID, [private, set, named_table]),
    ets:new(?BY_DB, [protected, bag, named_table]),
    couch_event:link_listener(?MODULE, handle_db_event, nil, [all_dbs]),
    RootDir = couch_index_util:root_dir(),
    couch_file:init_delete_dir(RootDir),
    {ok, #st{root_dir=RootDir}}.


terminate(_Reason, _State) ->
    Pids = [Pid || {Pid, _} <- ets:tab2list(?BY_PID)],
    lists:map(fun couch_util:shutdown_sync/1, Pids),
    ok.


handle_call({get_index, {_Mod, _IdxState, DbName, Sig}=Args}, From, State) ->
    case ets:lookup(?BY_SIG, {DbName, Sig}) of
        [] ->
            spawn_link(fun() -> new_index(Args) end),
            ets:insert(?BY_SIG, {{DbName, Sig}, [From]}),
            {noreply, State};
        [{_, Waiters}] when is_list(Waiters) ->
            ets:insert(?BY_SIG, {{DbName, Sig}, [From | Waiters]}),
            {noreply, State};
        [{_, Pid}] when is_pid(Pid) ->
            {reply, {ok, Pid}, State}
    end;
handle_call({async_open, {DbName, DDocId, Sig}, {ok, Pid}}, _From, State) ->
    [{_, Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, {ok, Pid}) || From <- Waiters],
    link(Pid),
    add_to_ets(DbName, Sig, DDocId, Pid),
    {reply, ok, State};
handle_call({async_error, {DbName, _DDocId, Sig}, Error}, _From, State) ->
    [{_, Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, Error) || From <- Waiters],
    ets:delete(?BY_SIG, {DbName, Sig}),
    {reply, ok, State};
handle_call({reset_indexes, DbName}, _From, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {reply, ok, State}.


handle_cast({reset_indexes, DbName}, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {noreply, State};
handle_cast({add_to_ets, [Pid, DbName, DDocId, Sig]}, State) ->
    % check if Pid still exists
    case ets:lookup(?BY_PID, Pid) of
        [{Pid, {DbName, Sig}}] when is_pid(Pid) ->
            ets:insert(?BY_DB, {DbName, {DDocId, Sig}});
        _ -> ok
    end,
    {noreply, State};
handle_cast({rem_from_ets, [DbName, DDocId, Sig]}, State) ->
    ets:delete_object(?BY_DB, {DbName, {DDocId, Sig}}),
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, Server) ->
    case ets:lookup(?BY_PID, Pid) of
        [{Pid, {DbName, Sig}}] ->
            DDocIds = [DDocId || {_, {DDocId, _}}
                <- ets:match_object(?BY_DB, {DbName, {'$1', Sig}})],
            rem_from_ets(DbName, Sig, DDocIds, Pid);
        [] when Reason /= normal ->
            exit(Reason);
        _Else ->
            ok
    end,
    {noreply, Server};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, couch_index_util:root_dir()),
    {noreply, State};
handle_info(Msg, State) ->
    couch_log:warning("~p did not expect ~p", [?MODULE, Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_config_change("couchdb", "index_dir", RootDir, _, RootDir) ->
    {ok, RootDir};
handle_config_change("couchdb", "view_index_dir", RootDir, _, RootDir) ->
    {ok, RootDir};
handle_config_change("couchdb", "index_dir", _, _, _) ->
    exit(whereis(couch_index_server), config_change),
    remove_handler;
handle_config_change("couchdb", "view_index_dir", _, _, _) ->
    exit(whereis(couch_index_server), config_change),
    remove_handler;
handle_config_change(_, _, _, _, RootDir) ->
    {ok, RootDir}.

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
    SigDDocIds = lists:foldl(fun({_, {DDocId, Sig}}, DDict) ->
        dict:append(Sig, DDocId, DDict)
    end, dict:new(), ets:lookup(?BY_DB, DbName)),
    Fun = fun({Sig, DDocIds}) ->
        [{_, Pid}] = ets:lookup(?BY_SIG, {DbName, Sig}),
        unlink(Pid),
        gen_server:cast(Pid, delete),
        receive
            {'EXIT', Pid, _} ->
                ok
        after
            0 ->
                ok
        end,
        rem_from_ets(DbName, Sig, DDocIds, Pid)
    end,
    lists:foreach(Fun, dict:to_list(SigDDocIds)),
    Path = couch_index_util:index_dir("", DbName),
    couch_file:nuke_dir(Root, Path).


add_to_ets(DbName, Sig, DDocId, Pid) ->
    ets:insert(?BY_SIG, {{DbName, Sig}, Pid}),
    ets:insert(?BY_PID, {Pid, {DbName, Sig}}),
    ets:insert(?BY_DB, {DbName, {DDocId, Sig}}).


rem_from_ets(DbName, Sig, DDocIds, Pid) ->
    ets:delete(?BY_SIG, {DbName, Sig}),
    ets:delete(?BY_PID, Pid),
    lists:foreach(fun(DDocId) ->
        ets:delete_object(?BY_DB, {DbName, {DDocId, Sig}})
    end, DDocIds).


handle_db_event(DbName, created, St) ->
    gen_server:cast(?MODULE, {reset_indexes, DbName}),
    {ok, St};
handle_db_event(DbName, deleted, St) ->
    gen_server:cast(?MODULE, {reset_indexes, DbName}),
    {ok, St};
handle_db_event(<<"shards/", _/binary>> = DbName, {ddoc_updated,
        DDocId}, St) ->
    DDocResult = couch_util:with_db(DbName, fun(Db) ->
        couch_db:open_doc(Db, DDocId, [ejson_body, ?ADMIN_CTX])
    end),
    LocalShards = try mem3:local_shards(mem3:dbname(DbName))
        catch error:database_does_not_exist ->
            []
    end,
    DbShards = [mem3:name(Sh) || Sh <- LocalShards],
    lists:foreach(fun(DbShard) ->
        lists:foreach(fun({_DbShard, {_DDocId, Sig}}) ->
            % check if there are other ddocs with the same Sig for the same db
            SigDDocs = ets:match_object(?BY_DB, {DbShard, {'$1', Sig}}),
            if length(SigDDocs) > 1 ->
                % remove records from ?BY_DB for this DDoc
                Args = [DbShard, DDocId, Sig],
                gen_server:cast(?MODULE, {rem_from_ets, Args});
            true ->
                % single DDoc with this Sig - close couch_index processes
                case ets:lookup(?BY_SIG, {DbShard, Sig}) of
                    [{_, IndexPid}] -> (catch
                        gen_server:cast(IndexPid, {ddoc_updated, DDocResult}));
                    [] -> []
                end
            end
        end, ets:match_object(?BY_DB, {DbShard, {DDocId, '$1'}}))
    end, DbShards),
    {ok, St};
handle_db_event(DbName, {ddoc_updated, DDocId}, St) ->
    lists:foreach(fun({_DbName, {_DDocId, Sig}}) ->
        case ets:lookup(?BY_SIG, {DbName, Sig}) of
            [{_, IndexPid}] ->
                (catch gen_server:cast(IndexPid, ddoc_updated));
            [] ->
                ok
        end
    end, ets:match_object(?BY_DB, {DbName, {DDocId, '$1'}})),
    {ok, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.
