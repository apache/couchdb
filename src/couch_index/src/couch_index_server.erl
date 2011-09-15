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

-export([start_link/0, get_index/4, get_index/3, get_index/2]).
-export([config_change/2, update_notify/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("couch_db.hrl").

-define(BY_SIG, couchdb_indexes_by_sig).
-define(BY_PID, couchdb_indexes_by_pid).
-define(BY_DB, couchdb_indexes_by_db).


-record(st, {root_dir}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_index(Module, DbName, DDoc) ->
    get_index(Module, DbName, DDoc, nil).


get_index(Module, DbName, DDoc, Fun) when is_binary(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        get_index(Module, Db, DDoc, Fun)
    end);
get_index(Module, Db, DDoc, Fun) when is_binary(DDoc) ->
    case couch_db:open_doc(Db, DDoc, [ejson_body]) of
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
            {ok, Pid};
        _ ->
            Args = {Module, IdxState, DbName, Sig},
            gen_server:call(?MODULE, {get_index, Args}, infinity)
    end.


init([]) ->
    process_flag(trap_exit, true),
    couch_config:register(fun ?MODULE:config_change/2),
    couch_db_update_notifier:start_link(fun ?MODULE:update_notify/1),
    ets:new(?BY_SIG, [protected, set, named_table]),
    ets:new(?BY_PID, [private, set, named_table]),
    ets:new(?BY_DB, [private, bag, named_table]),
    RootDir = couch_index_util:root_dir(),
    % Deprecation warning if it wasn't index_dir
    case couch_config:get("couchdb", "index_dir") of
        undefined ->
            Msg = "Deprecation warning: 'view_index_dir' is now 'index_dir'",
            ?LOG_ERROR(Msg, []);
        _ -> ok
    end,
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
handle_call({async_open, {DbName, Sig}, {ok, Pid}}, _From, State) ->
    [{_, Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, {ok, Pid}) || From <- Waiters],
    link(Pid),
    add_to_ets(DbName, Sig, Pid),
    {reply, ok, State};
handle_call({async_error, {DbName, Sig}, Error}, _From, State) ->
    [{_, Waiters}] = ets:lookup(?BY_SIG, {DbName, Sig}),
    [gen_server:reply(From, Error) || From <- Waiters],
    ets:delete(?BY_SIG, {DbName, Sig}),
    {reply, ok, State};
handle_call({reset_indexes, DbName}, _From, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {reply, ok, State}.


handle_cast({reset_indexes, DbName}, State) ->
    reset_indexes(DbName, State#st.root_dir),
    {noreply, State}.


handle_info({'EXIT', Pid, Reason}, Server) ->
    case ets:lookup(?BY_PID, Pid) of
        [{Pid, DbName, Sig}] ->
            rem_from_ets(DbName, Sig, Pid);
        [] when Reason /= normal ->
            exit(Reason);
        _Else ->
            ok
    end,
    {noreply, Server}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


new_index({Mod, IdxState, DbName, Sig}) ->
    case couch_index:start_link({Mod, IdxState}) of
        {ok, Pid} ->
            gen_server:call(?MODULE, {async_open, {DbName, Sig}, {ok, Pid}}),
            unlink(Pid);
        Error ->
            gen_server:call(?MODULE, {async_error, {DbName, Sig}, Error})
    end.


reset_indexes(DbName, Root) ->
    % shutdown all the updaters and clear the files, the db got changed
    Fun = fun({_, Sig}) ->
        [{_, Pid}] = ets:lookup(?BY_SIG, {DbName, Sig}),
        couch_util:shutdown_sync(Pid),
        rem_from_ets(DbName, Sig, Pid)
    end,
    lists:foreach(Fun, ets:lookup(?BY_DB, DbName)),
    Path = Root ++ "/." ++ binary_to_list(DbName) ++ "_design",
    couch_file:nuke_dir(Root, Path).


add_to_ets(DbName, Sig, Pid) ->
    ets:insert(?BY_SIG, {{DbName, Sig}, Pid}),
    ets:insert(?BY_PID, {Pid, {DbName, Sig}}),
    ets:insert(?BY_DB, {DbName, Sig}).


rem_from_ets(DbName, Sig, Pid) ->
    ets:delete(?BY_SIG, {DbName, Sig}),
    ets:delete(?BY_PID, Pid),
    ets:delete_object(?BY_DB, {DbName, Sig}).


config_change("couchdb", "view_index_dir") ->
    exit(whereis(?MODULE), config_change);
config_change("couchdb", "index_dir") ->
    exit(whereis(?MODULE), config_change).


update_notify({deleted, DbName}) ->
    gen_server:cast(?MODULE, {reset_indexes, DbName});
update_notify({created, DbName}) ->
    gen_server:cast(?MODULE, {reset_indexes, DbName});
update_notify(_) ->
    ok.

