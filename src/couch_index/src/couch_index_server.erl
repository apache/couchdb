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

-export([start_link/1, validate/2, get_index/4, get_index/3, get_index/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

% Sharding functions
-export([num_servers/0, server_name/1, by_sig/1, by_pid/1, by_db/1]).
-export([aggregate_queue_len/0, names/0]).

% Exported for callbacks
-export([
    handle_config_change/5,
    handle_config_terminate/3,
    handle_db_event/3
]).

-include_lib("couch/include/couch_db.hrl").

-define(RELISTEN_DELAY, 5000).

-record(st, {
    root_dir,
    server_name,
    by_sig,
    by_pid,
    by_db
}).

start_link(N) ->
    gen_server:start_link({local, server_name(N)}, ?MODULE, [N], []).

validate(Db, DDoc) ->
    LoadModFun = fun
        ({ModNameList, "true"}) ->
            try
                [list_to_existing_atom(ModNameList)]
            catch
                error:badarg ->
                    []
            end;
        ({_ModNameList, _Enabled}) ->
            []
    end,
    ValidateFun = fun(ModName) ->
        ModName:validate(Db, DDoc)
    end,
    EnabledIndexers = lists:flatmap(LoadModFun, config:get("indexers")),
    lists:foreach(ValidateFun, EnabledIndexers).

get_index(Module, <<"shards/", _/binary>> = DbName, DDoc) when
    is_record(DDoc, doc)
->
    get_index(Module, DbName, DDoc, nil);
get_index(Module, <<"shards/", _/binary>> = DbName, DDoc) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        exit(fabric:open_doc(mem3:dbname(DbName), DDoc, [ejson_body, ?ADMIN_CTX]))
    end),
    receive
        {'DOWN', Ref, process, Pid, {ok, Doc}} ->
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
    case ets:lookup(by_sig(DbName), {DbName, Sig}) of
        [{_, Pid}] when is_pid(Pid) ->
            DDocId = Module:get(idx_name, IdxState),
            case ets:match_object(by_db(DbName), {DbName, {DDocId, Sig}}) of
                [] ->
                    Args = [Pid, DbName, DDocId, Sig],
                    gen_server:cast(server_name(DbName), {add_to_ets, Args});
                _ ->
                    ok
            end,
            {ok, Pid};
        _ ->
            Args = {Module, IdxState, DbName, Sig},
            gen_server:call(server_name(DbName), {get_index, Args}, infinity)
    end.

init([N]) ->
    process_flag(trap_exit, true),
    ets:new(by_sig(N), [protected, set, named_table]),
    ets:new(by_pid(N), [private, set, named_table]),
    ets:new(by_db(N), [protected, bag, named_table]),
    RootDir = couch_index_util:root_dir(),
    % We only need one of the index servers to nuke this on startup.
    case N of
        1 -> couch_file:init_delete_dir(RootDir);
        _ -> ok
    end,
    St = #st{
        root_dir = RootDir,
        server_name = server_name(N),
        by_sig = by_sig(N),
        by_pid = by_pid(N),
        by_db = by_db(N)
    },
    ok = config:listen_for_changes(?MODULE, St),
    couch_event:link_listener(?MODULE, handle_db_event, St, [all_dbs]),
    {ok, St}.

terminate(_Reason, State) ->
    Pids = [Pid || {Pid, _} <- ets:tab2list(State#st.by_pid)],
    lists:map(fun couch_util:shutdown_sync/1, Pids),
    ok.

handle_call({get_index, {_Mod, _IdxState, DbName, Sig} = Args}, From, State) ->
    case ets:lookup(State#st.by_sig, {DbName, Sig}) of
        [] ->
            spawn_link(fun() -> new_index(Args) end),
            ets:insert(State#st.by_sig, {{DbName, Sig}, [From]}),
            {noreply, State};
        [{_, Waiters}] when is_list(Waiters) ->
            ets:insert(State#st.by_sig, {{DbName, Sig}, [From | Waiters]}),
            {noreply, State};
        [{_, Pid}] when is_pid(Pid) ->
            {reply, {ok, Pid}, State}
    end;
handle_call({async_open, {DbName, DDocId, Sig}, {ok, Pid}}, _From, State) ->
    [{_, Waiters}] = ets:lookup(State#st.by_sig, {DbName, Sig}),
    [gen_server:reply(From, {ok, Pid}) || From <- Waiters],
    link(Pid),
    add_to_ets(DbName, Sig, DDocId, Pid, State),
    {reply, ok, State};
handle_call({async_error, {DbName, _DDocId, Sig}, Error}, _From, State) ->
    [{_, Waiters}] = ets:lookup(State#st.by_sig, {DbName, Sig}),
    [gen_server:reply(From, Error) || From <- Waiters],
    ets:delete(State#st.by_sig, {DbName, Sig}),
    {reply, ok, State};
handle_call({reset_indexes, DbName}, _From, State) ->
    reset_indexes(DbName, State),
    {reply, ok, State}.

handle_cast({reset_indexes, DbName}, State) ->
    reset_indexes(DbName, State),
    {noreply, State};
handle_cast({add_to_ets, [Pid, DbName, DDocId, Sig]}, State) ->
    % check if Pid still exists
    case ets:lookup(State#st.by_pid, Pid) of
        [{Pid, {DbName, Sig}}] when is_pid(Pid) ->
            ets:insert(State#st.by_db, {DbName, {DDocId, Sig}});
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({rem_from_ets, [DbName, DDocId, Sig]}, State) ->
    ets:delete_object(State#st.by_db, {DbName, {DDocId, Sig}}),
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, Server) ->
    case ets:lookup(Server#st.by_pid, Pid) of
        [{Pid, {DbName, Sig}}] ->
            DDocIds = [
                DDocId
             || {_, {DDocId, _}} <-
                    ets:match_object(Server#st.by_db, {DbName, {'$1', Sig}})
            ],
            rem_from_ets(DbName, Sig, DDocIds, Pid, Server);
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

handle_config_change("couchdb", "index_dir", RootDir, _, #st{root_dir = RootDir} = St) ->
    {ok, St};
handle_config_change("couchdb", "view_index_dir", RootDir, _, #st{root_dir = RootDir} = St) ->
    {ok, St};
handle_config_change("couchdb", "index_dir", _, _, St) ->
    exit(whereis(St#st.server_name), config_change),
    remove_handler;
handle_config_change("couchdb", "view_index_dir", _, _, St) ->
    exit(whereis(St#st.server_name), config_change),
    remove_handler;
handle_config_change(_, _, _, _, St) ->
    {ok, St}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(State#st.server_name), restart_config_listener),
    {ok, State}.

new_index({Mod, IdxState, DbName, Sig}) ->
    DDocId = Mod:get(idx_name, IdxState),
    case couch_index:start_link({Mod, IdxState}) of
        {ok, Pid} ->
            ok = gen_server:call(
                server_name(DbName), {async_open, {DbName, DDocId, Sig}, {ok, Pid}}
            ),
            unlink(Pid);
        Error ->
            ok = gen_server:call(
                server_name(DbName), {async_error, {DbName, DDocId, Sig}, Error}
            )
    end.

reset_indexes(DbName, #st{} = State) ->
    % shutdown all the updaters and clear the files, the db got changed
    SigDDocIds = lists:foldl(
        fun({_, {DDocId, Sig}}, DDict) ->
            dict:append(Sig, DDocId, DDict)
        end,
        dict:new(),
        ets:lookup(State#st.by_db, DbName)
    ),
    Fun = fun({Sig, DDocIds}) ->
        [{_, Pid}] = ets:lookup(State#st.by_sig, {DbName, Sig}),
        unlink(Pid),
        gen_server:cast(Pid, delete),
        receive
            {'EXIT', Pid, _} ->
                ok
        after 0 ->
            ok
        end,
        rem_from_ets(DbName, Sig, DDocIds, Pid, State)
    end,
    lists:foreach(Fun, dict:to_list(SigDDocIds)),
    % We only need one of the index servers to do this.
    case State#st.server_name == server_name(1) of
        true ->
            Path = couch_index_util:index_dir("", DbName),
            couch_file:nuke_dir(State#st.root_dir, Path);
        false ->
            ok
    end.

add_to_ets(DbName, Sig, DDocId, Pid, #st{} = St) ->
    ets:insert(St#st.by_sig, {{DbName, Sig}, Pid}),
    ets:insert(St#st.by_pid, {Pid, {DbName, Sig}}),
    ets:insert(St#st.by_db, {DbName, {DDocId, Sig}}).

rem_from_ets(DbName, Sig, DDocIds, Pid, #st{} = St) ->
    ets:delete(St#st.by_sig, {DbName, Sig}),
    ets:delete(St#st.by_pid, Pid),
    lists:foreach(
        fun(DDocId) ->
            ets:delete_object(St#st.by_db, {DbName, {DDocId, Sig}})
        end,
        DDocIds
    ).

handle_db_event(DbName, created, St) ->
    gen_server:cast(St#st.server_name, {reset_indexes, DbName}),
    {ok, St};
handle_db_event(DbName, deleted, St) ->
    gen_server:cast(St#st.server_name, {reset_indexes, DbName}),
    {ok, St};
handle_db_event(<<"shards/", _/binary>> = DbName, {ddoc_updated, DDocId}, St) ->
    DDocResult = couch_util:with_db(DbName, fun(Db) ->
        couch_db:open_doc(Db, DDocId, [ejson_body, ?ADMIN_CTX])
    end),
    LocalShards =
        try
            mem3:local_shards(mem3:dbname(DbName))
        catch
            error:database_does_not_exist ->
                []
        end,
    DbShards = [mem3:name(Sh) || Sh <- LocalShards],
    lists:foreach(
        fun(DbShard) ->
            lists:foreach(
                fun({_DbShard, {_DDocId, Sig}}) ->
                    % check if there are other ddocs with the same Sig for the same db
                    SigDDocs = ets:match_object(St#st.by_db, {DbShard, {'$1', Sig}}),
                    if
                        length(SigDDocs) > 1 ->
                            % remove records from by_db for this DDoc
                            Args = [DbShard, DDocId, Sig],
                            gen_server:cast(St#st.server_name, {rem_from_ets, Args});
                        true ->
                            % single DDoc with this Sig - close couch_index processes
                            case ets:lookup(St#st.by_sig, {DbShard, Sig}) of
                                [{_, IndexPid}] ->
                                    (catch gen_server:cast(IndexPid, {ddoc_updated, DDocResult}));
                                [] ->
                                    []
                            end
                    end
                end,
                ets:match_object(St#st.by_db, {DbShard, {DDocId, '$1'}})
            )
        end,
        DbShards
    ),
    {ok, St};
handle_db_event(DbName, {ddoc_updated, DDocId}, St) ->
    lists:foreach(
        fun({_DbName, {_DDocId, Sig}}) ->
            case ets:lookup(St#st.by_sig, {DbName, Sig}) of
                [{_, IndexPid}] ->
                    (catch gen_server:cast(IndexPid, ddoc_updated));
                [] ->
                    ok
            end
        end,
        ets:match_object(St#st.by_db, {DbName, {DDocId, '$1'}})
    ),
    {ok, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.

num_servers() ->
    erlang:system_info(schedulers).

server_name(Arg) ->
    name("index_server", Arg).

by_sig(Arg) ->
    name("couchdb_indexes_by_sig", Arg).

by_pid(Arg) ->
    name("couchdb_indexes_by_pid", Arg).

by_db(Arg) ->
    name("couchdb_indexes_by_db", Arg).

name(BaseName, Arg) when is_list(Arg) ->
    name(BaseName, ?l2b(Arg));
name(BaseName, Arg) when is_binary(Arg) ->
    N = 1 + erlang:phash2(Arg, num_servers()),
    name(BaseName, N);
name(BaseName, N) when is_integer(N), N > 0 ->
    list_to_atom(BaseName ++ "_" ++ integer_to_list(N)).

names() ->
    N = num_servers(),
    [server_name(I) || I <- lists:seq(1, N)].

aggregate_queue_len() ->
    N = num_servers(),
    Names = [server_name(I) || I <- lists:seq(1, N)],
    MQs = [
        process_info(whereis(Name), message_queue_len)
     || Name <- Names
    ],
    lists:sum([X || {_, X} <- MQs]).
