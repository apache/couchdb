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

-module(couch_server).
-behaviour(gen_server).
-behaviour(config_listener).
-vsn(3).

-export([open/2,create/2,delete/2,get_version/0,get_version/1,get_uuid/0]).
-export([all_databases/0, all_databases/2]).
-export([init/1, handle_call/3,sup_start_link/0]).
-export([handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([dev_start/0,is_admin/2,has_admins/0,get_stats/0]).
-export([close_lru/0]).
-export([close_db_if_idle/1]).
-export([delete_compaction_files/1]).
-export([exists/1]).
-export([get_engine_extensions/0]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_server_int.hrl").

-define(MAX_DBS_OPEN, 500).
-define(RELISTEN_DELAY, 5000).

-record(server,{
    root_dir = [],
    engines = [],
    max_dbs_open=?MAX_DBS_OPEN,
    dbs_open=0,
    start_time="",
    update_lru_on_read=true,
    lru = couch_lru:new()
    }).

dev_start() ->
    couch:stop(),
    up_to_date = make:all([load, debug_info]),
    couch:start().

get_version() ->
    ?COUCHDB_VERSION. %% Defined in rebar.config.script
get_version(short) ->
  %% strip git hash from version string
  [Version|_Rest] = string:tokens(get_version(), "+"),
  Version.


get_uuid() ->
    case config:get("couchdb", "uuid", undefined) of
        undefined ->
            UUID = couch_uuids:random(),
            config:set("couchdb", "uuid", ?b2l(UUID)),
            UUID;
        UUID -> ?l2b(UUID)
    end.

get_stats() ->
    {ok, #server{start_time=Time,dbs_open=Open}} =
            gen_server:call(couch_server, get_server),
    [{start_time, ?l2b(Time)}, {dbs_open, Open}].

sup_start_link() ->
    gen_server:start_link({local, couch_server}, couch_server, [], []).


open(DbName, Options0) ->
    Ctx = couch_util:get_value(user_ctx, Options0, #user_ctx{}),
    case ets:lookup(couch_dbs, DbName) of
    [#entry{db = Db0, lock = Lock} = Entry] when Lock =/= locked ->
        update_lru(DbName, Entry#entry.db_options),
        {ok, Db1} = couch_db:incref(Db0),
        couch_db:set_user_ctx(Db1, Ctx);
    _ ->
        Options = maybe_add_sys_db_callbacks(DbName, Options0),
        Timeout = couch_util:get_value(timeout, Options, infinity),
        Create = couch_util:get_value(create_if_missing, Options, false),
        case gen_server:call(couch_server, {open, DbName, Options}, Timeout) of
        {ok, Db0} ->
            {ok, Db1} = couch_db:incref(Db0),
            couch_db:set_user_ctx(Db1, Ctx);
        {not_found, no_db_file} when Create ->
            couch_log:warning("creating missing database: ~s", [DbName]),
            couch_server:create(DbName, Options);
        Error ->
            Error
        end
    end.

update_lru(DbName, Options) ->
    case lists:member(sys_db, Options) of
        false -> gen_server:cast(couch_server, {update_lru, DbName});
        true -> ok
    end.

close_lru() ->
    gen_server:call(couch_server, close_lru).

create(DbName, Options0) ->
    Options = maybe_add_sys_db_callbacks(DbName, Options0),
    case gen_server:call(couch_server, {create, DbName, Options}, infinity) of
    {ok, Db0} ->
        Ctx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
        {ok, Db1} = couch_db:incref(Db0),
        couch_db:set_user_ctx(Db1, Ctx);
    Error ->
        Error
    end.

delete(DbName, Options) ->
    gen_server:call(couch_server, {delete, DbName, Options}, infinity).


exists(DbName) ->
    RootDir = config:get("couchdb", "database_dir", "."),
    Engines = get_configured_engines(),
    Possible = get_possible_engines(DbName, RootDir, Engines),
    Possible /= [].


delete_compaction_files(DbName) ->
    delete_compaction_files(DbName, []).

delete_compaction_files(DbName, DelOpts) when is_list(DbName) ->
    RootDir = config:get("couchdb", "database_dir", "."),
    lists:foreach(fun({Ext, Engine}) ->
        FPath = make_filepath(RootDir, DbName, Ext),
        couch_db_engine:delete_compaction_files(Engine, RootDir, FPath, DelOpts)
    end, get_configured_engines()),
    ok;
delete_compaction_files(DbName, DelOpts) when is_binary(DbName) ->
    delete_compaction_files(?b2l(DbName), DelOpts).

maybe_add_sys_db_callbacks(DbName, Options) when is_binary(DbName) ->
    maybe_add_sys_db_callbacks(?b2l(DbName), Options);
maybe_add_sys_db_callbacks(DbName, Options) ->
    DbsDbName = config:get("mem3", "shards_db", "_dbs"),
    NodesDbName = config:get("mem3", "nodes_db", "_nodes"),

    IsReplicatorDb = path_ends_with(DbName, "_replicator"),
    UsersDbSuffix = config:get("couchdb", "users_db_suffix", "_users"),
    IsUsersDb = path_ends_with(DbName, "_users")
        orelse path_ends_with(DbName, UsersDbSuffix),
    if
	DbName == DbsDbName ->
	    [sys_db | Options];
	DbName == NodesDbName ->
	    [sys_db | Options];
	IsReplicatorDb ->
	    [{before_doc_update, fun couch_replicator_manager:before_doc_update/2},
	     {after_doc_read, fun couch_replicator_manager:after_doc_read/2},
	     sys_db | Options];
	IsUsersDb ->
	    [{before_doc_update, fun couch_users_db:before_doc_update/2},
	     {after_doc_read, fun couch_users_db:after_doc_read/2},
	     sys_db | Options];
	true ->
	    Options
    end.

path_ends_with(Path, Suffix) when is_binary(Suffix) ->
    Suffix =:= couch_db:dbname_suffix(Path);
path_ends_with(Path, Suffix) when is_list(Suffix) ->
    path_ends_with(Path, ?l2b(Suffix)).

check_dbname(#server{}, DbName) ->
    couch_db:validate_dbname(DbName).

is_admin(User, ClearPwd) ->
    case config:get("admins", User) of
    "-hashed-" ++ HashedPwdAndSalt ->
        [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
        couch_util:to_hex(crypto:hash(sha, ClearPwd ++ Salt)) == HashedPwd;
    _Else ->
        false
    end.

has_admins() ->
    config:get("admins") /= [].

hash_admin_passwords() ->
    hash_admin_passwords(true).

hash_admin_passwords(Persist) ->
    lists:foreach(
        fun({User, ClearPassword}) ->
            HashedPassword = couch_passwords:hash_admin_password(ClearPassword),
            config:set("admins", User, ?b2l(HashedPassword), Persist)
        end, couch_passwords:get_unhashed_admins()).

close_db_if_idle(DbName) ->
    case ets:lookup(couch_dbs, DbName) of
        [#entry{}] ->
            gen_server:cast(couch_server, {close_db_if_idle, DbName});
        [] ->
            ok
    end.


init([]) ->
    % Mark pluggable storage engines as a supported feature
    config:enable_feature('pluggable-storage-engines'),

    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    RootDir = config:get("couchdb", "database_dir", "."),
    Engines = get_configured_engines(),
    MaxDbsOpen = list_to_integer(
            config:get("couchdb", "max_dbs_open", integer_to_list(?MAX_DBS_OPEN))),
    UpdateLruOnRead =
        config:get("couchdb", "update_lru_on_read", "false") =:= "true",
    ok = config:listen_for_changes(?MODULE, nil),
    ok = couch_file:init_delete_dir(RootDir),
    hash_admin_passwords(),
    ets:new(couch_dbs, [set, protected, named_table, {keypos, #entry.name}]),
    ets:new(couch_dbs_pid_to_name, [set, protected, named_table]),
    process_flag(trap_exit, true),
    {ok, #server{root_dir=RootDir,
                engines = Engines,
                max_dbs_open=MaxDbsOpen,
                update_lru_on_read=UpdateLruOnRead,
                start_time=couch_util:rfc1123_date()}}.

terminate(Reason, Srv) ->
    couch_log:error("couch_server terminating with ~p, state ~2048p",
                    [Reason,
                     Srv#server{lru = redacted}]),
    ets:foldl(fun(#entry{db = Db}, _) ->
        couch_util:shutdown_sync(couch_db:get_pid(Db))
    end, nil, couch_dbs),
    ok.

handle_config_change("couchdb", "database_dir", _, _, _) ->
    exit(whereis(couch_server), config_change),
    remove_handler;
handle_config_change("couchdb", "update_lru_on_read", "true", _, _) ->
    {ok, gen_server:call(couch_server,{set_update_lru_on_read,true})};
handle_config_change("couchdb", "update_lru_on_read", _, _, _) ->
    {ok, gen_server:call(couch_server,{set_update_lru_on_read,false})};
handle_config_change("couchdb", "max_dbs_open", Max, _, _) when is_list(Max) ->
    {ok, gen_server:call(couch_server,{set_max_dbs_open,list_to_integer(Max)})};
handle_config_change("couchdb", "max_dbs_open", _, _, _) ->
    {ok, gen_server:call(couch_server,{set_max_dbs_open,?MAX_DBS_OPEN})};
handle_config_change("couchdb_engines", _, _, _, _) ->
    {ok, gen_server:call(couch_server, reload_engines)};
handle_config_change("admins", _, _, Persist, _) ->
    % spawn here so couch event manager doesn't deadlock
    {ok, spawn(fun() -> hash_admin_passwords(Persist) end)};
handle_config_change("httpd", "authentication_handlers", _, _, _) ->
    {ok, couch_httpd:stop()};
handle_config_change("httpd", "bind_address", _, _, _) ->
    {ok, couch_httpd:stop()};
handle_config_change("httpd", "port", _, _, _) ->
    {ok, couch_httpd:stop()};
handle_config_change("httpd", "max_connections", _, _, _) ->
    {ok, couch_httpd:stop()};
handle_config_change("httpd", "default_handler", _, _, _) ->
    {ok, couch_httpd:stop()};
handle_config_change("httpd_global_handlers", _, _, _, _) ->
    {ok, couch_httpd:stop()};
handle_config_change("httpd_db_handlers", _, _, _, _) ->
    {ok, couch_httpd:stop()};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).


all_databases() ->
    {ok, DbList} = all_databases(
        fun(DbName, Acc) -> {ok, [DbName | Acc]} end, []),
    {ok, lists:usort(DbList)}.

all_databases(Fun, Acc0) ->
    {ok, #server{root_dir=Root}} = gen_server:call(couch_server, get_server),
    NormRoot = couch_util:normpath(Root),
    Extensions = get_engine_extensions(),
    ExtRegExp = "(" ++ string:join(Extensions, "|") ++ ")",
    RegExp =
        "^[a-z0-9\\_\\$()\\+\\-]*" % stock CouchDB name regex
        "(\\.[0-9]{10,})?"         % optional shard timestamp
        "\\." ++ ExtRegExp ++ "$", % filename extension
    FinalAcc = try
    couch_util:fold_files(Root,
        RegExp,
        true,
            fun(Filename, AccIn) ->
                NormFilename = couch_util:normpath(Filename),
                case NormFilename -- NormRoot of
                [$/ | RelativeFilename] -> ok;
                RelativeFilename -> ok
                end,
                Ext = filename:extension(RelativeFilename),
                case Fun(?l2b(filename:rootname(RelativeFilename, Ext)), AccIn) of
                {ok, NewAcc} -> NewAcc;
                {stop, NewAcc} -> throw({stop, Fun, NewAcc})
                end
            end, Acc0)
    catch throw:{stop, Fun, Acc1} ->
         Acc1
    end,
    {ok, FinalAcc}.


make_room(Server, Options) ->
    case lists:member(sys_db, Options) of
        false -> maybe_close_lru_db(Server);
        true -> {ok, Server}
    end.

maybe_close_lru_db(#server{dbs_open=NumOpen, max_dbs_open=MaxOpen}=Server)
        when NumOpen < MaxOpen ->
    {ok, Server};
maybe_close_lru_db(#server{lru=Lru}=Server) ->
    case couch_lru:close(Lru) of
        {true, NewLru} ->
            {ok, db_closed(Server#server{lru = NewLru}, [])};
        false ->
            {error, all_dbs_active}
    end.

open_async(Server, From, DbName, {Module, Filepath}, Options) ->
    Parent = self(),
    T0 = os:timestamp(),
    Opener = spawn_link(fun() ->
        Res = couch_db:start_link(Module, DbName, Filepath, Options),
        case {Res, lists:member(create, Options)} of
            {{ok, _Db}, true} ->
                couch_event:notify(DbName, created);
            _ ->
                ok
        end,
        gen_server:call(Parent, {open_result, T0, DbName, Res}, infinity),
        unlink(Parent)
    end),
    ReqType = case lists:member(create, Options) of
        true -> create;
        false -> open
    end,
    true = ets:insert(couch_dbs, #entry{
        name = DbName,
        pid = Opener,
        lock = locked,
        waiters = [From],
        req_type = ReqType,
        db_options = Options
    }),
    true = ets:insert(couch_dbs_pid_to_name, {Opener, DbName}),
    db_opened(Server, Options).

handle_call(close_lru, _From, #server{lru=Lru} = Server) ->
    case couch_lru:close(Lru) of
        {true, NewLru} ->
            {reply, ok, db_closed(Server#server{lru = NewLru}, [])};
        false ->
            {reply, {error, all_dbs_active}, Server}
    end;
handle_call(open_dbs_count, _From, Server) ->
    {reply, Server#server.dbs_open, Server};
handle_call({set_update_lru_on_read, UpdateOnRead}, _From, Server) ->
    {reply, ok, Server#server{update_lru_on_read=UpdateOnRead}};
handle_call({set_max_dbs_open, Max}, _From, Server) ->
    {reply, ok, Server#server{max_dbs_open=Max}};
handle_call(reload_engines, _From, Server) ->
    {reply, ok, Server#server{engines = get_configured_engines()}};
handle_call(get_server, _From, Server) ->
    {reply, {ok, Server}, Server};
handle_call({open_result, T0, DbName, {ok, Db}}, {FromPid, _Tag}, Server) ->
    true = ets:delete(couch_dbs_pid_to_name, FromPid),
    OpenTime = timer:now_diff(os:timestamp(), T0) / 1000,
    couch_stats:update_histogram([couchdb, db_open_time], OpenTime),
    DbPid = couch_db:get_pid(Db),
    case ets:lookup(couch_dbs, DbName) of
        [] ->
            % db was deleted during async open
            exit(DbPid, kill),
            {reply, ok, Server};
        [#entry{req_type = ReqType, waiters = Waiters} = Entry] ->
            link(DbPid),
            [gen_server:reply(Waiter, {ok, Db}) || Waiter <- Waiters],
            % Cancel the creation request if it exists.
            case ReqType of
                {create, DbName, _Engine, _Options, CrFrom} ->
                    gen_server:reply(CrFrom, file_exists);
                _ ->
                    ok
            end,
            true = ets:insert(couch_dbs, #entry{
                name = DbName,
                db = Db,
                pid = DbPid,
                lock = unlocked,
                db_options = Entry#entry.db_options,
                start_time = couch_db:get_instance_start_time(Db)
            }),
            true = ets:insert(couch_dbs_pid_to_name, {DbPid, DbName}),
            Lru = case couch_db:is_system_db(Db) of
                false ->
                    couch_lru:insert(DbName, Server#server.lru);
                true ->
                    Server#server.lru
            end,
            {reply, ok, Server#server{lru = Lru}}
    end;
handle_call({open_result, T0, DbName, {error, eexist}}, From, Server) ->
    handle_call({open_result, T0, DbName, file_exists}, From, Server);
handle_call({open_result, _T0, DbName, Error}, {FromPid, _Tag}, Server) ->
    case ets:lookup(couch_dbs, DbName) of
        [] ->
            % db was deleted during async open
            {reply, ok, Server};
        [#entry{req_type = ReqType, waiters = Waiters} = Entry] ->
            [gen_server:reply(Waiter, Error) || Waiter <- Waiters],
            couch_log:info("open_result error ~p for ~s", [Error, DbName]),
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, FromPid),
            NewServer = case ReqType of
                {create, DbName, Engine, Options, CrFrom} ->
                    open_async(Server, CrFrom, DbName, Engine, Options);
                _ ->
                    Server
            end,
            {reply, ok, db_closed(NewServer, Entry#entry.db_options)}
    end;
handle_call({open, DbName, Options}, From, Server) ->
    case ets:lookup(couch_dbs, DbName) of
    [] ->
        DbNameList = binary_to_list(DbName),
        case check_dbname(Server, DbNameList) of
        ok ->
            case make_room(Server, Options) of
            {ok, Server2} ->
                {ok, Engine} = get_engine(Server2, DbNameList),
                {noreply, open_async(Server2, From, DbName, Engine, Options)};
            CloseError ->
                {reply, CloseError, Server}
            end;
        Error ->
            {reply, Error, Server}
        end;
    [#entry{waiters = Waiters} = Entry] when is_list(Waiters) ->
        true = ets:insert(couch_dbs, Entry#entry{waiters = [From | Waiters]}),
        if length(Waiters) =< 10 -> ok; true ->
            Fmt = "~b clients waiting to open db ~s",
            couch_log:info(Fmt, [length(Waiters), DbName])
        end,
        {noreply, Server};
    [#entry{db = Db}] ->
        {reply, {ok, Db}, Server}
    end;
handle_call({create, DbName, Options}, From, Server) ->
    DbNameList = binary_to_list(DbName),
    case get_engine(Server, DbNameList, Options) of
    {ok, Engine} ->
        case check_dbname(Server, DbNameList) of
        ok ->
            case ets:lookup(couch_dbs, DbName) of
            [] ->
                case make_room(Server, Options) of
                {ok, Server2} ->
                    {noreply, open_async(Server2, From, DbName, Engine,
                            [create | Options])};
                CloseError ->
                    {reply, CloseError, Server}
                end;
            [#entry{req_type = open} = Entry] ->
                % We're trying to create a database while someone is in
                % the middle of trying to open it. We allow one creator
                % to wait while we figure out if it'll succeed.
                CrOptions = [create | Options],
                Req = {create, DbName, Engine, CrOptions, From},
                true = ets:insert(couch_dbs, Entry#entry{req_type = Req}),
                {noreply, Server};
            [_AlreadyRunningDb] ->
                {reply, file_exists, Server}
            end;
        Error ->
            {reply, Error, Server}
        end;
    Error ->
        {reply, Error, Server}
    end;
handle_call({delete, DbName, Options}, _From, Server) ->
    DbNameList = binary_to_list(DbName),
    case check_dbname(Server, DbNameList) of
    ok ->
        Server2 =
        case ets:lookup(couch_dbs, DbName) of
        [] -> Server;
        [#entry{pid = Pid, waiters = Waiters} = Entry] when is_list(Waiters) ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, Pid),
            exit(Pid, kill),
            [gen_server:reply(Waiter, not_found) || Waiter <- Waiters],
            db_closed(Server, Entry#entry.db_options);
        [#entry{pid = Pid} = Entry] ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, Pid),
            exit(Pid, kill),
            db_closed(Server, Entry#entry.db_options)
        end,

        couch_db_plugin:on_delete(DbName, Options),

        DelOpt = [{context, delete} | Options],

        % Make sure and remove all compaction data
        delete_compaction_files(DbNameList, DelOpt),

        {ok, {Engine, FilePath}} = get_engine(Server, DbNameList),
        RootDir = Server#server.root_dir,
        case couch_db_engine:delete(Engine, RootDir, FilePath, DelOpt) of
        ok ->
            couch_event:notify(DbName, deleted),
            {reply, ok, Server2};
        {error, enoent} ->
            {reply, not_found, Server2};
        Else ->
            {reply, Else, Server2}
        end;
    Error ->
        {reply, Error, Server}
    end;
handle_call({db_updated, Db}, _From, Server0) ->
    DbName = couch_db:name(Db),
    StartTime = couch_db:get_instance_start_time(Db),
    Server = try ets:lookup_element(couch_dbs, DbName, #entry.start_time) of
        StartTime ->
            true = ets:update_element(couch_dbs, DbName, {#entry.db, Db}),
            Lru = case couch_db:is_system_db(Db) of
                false -> couch_lru:update(DbName, Server0#server.lru);
                true -> Server0#server.lru
            end,
            Server0#server{lru = Lru};
        _ ->
            Server0
    catch _:_ ->
        Server0
    end,
    {reply, ok, Server}.

handle_cast({update_lru, DbName}, #server{lru = Lru, update_lru_on_read=true} = Server) ->
    {noreply, Server#server{lru = couch_lru:update(DbName, Lru)}};
handle_cast({update_lru, _DbName}, Server) ->
    {noreply, Server};
handle_cast({close_db_if_idle, DbName}, Server) ->
    case ets:update_element(couch_dbs, DbName, {#entry.lock, locked}) of
    true ->
        [#entry{db = Db, db_options = DbOpts}] = ets:lookup(couch_dbs, DbName),
        case couch_db:is_idle(Db) of
        true ->
            DbPid = couch_db:get_pid(Db),
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, DbPid),
            exit(DbPid, kill),
            {noreply, db_closed(Server, DbOpts)};
        false ->
            true = ets:update_element(
                     couch_dbs, DbName, {#entry.lock, unlocked}),
            {noreply, Server}
        end;
    false ->
        {noreply, Server}
    end;

handle_cast(Msg, Server) ->
    {stop, {unknown_cast_message, Msg}, Server}.

code_change(_OldVsn, #server{}=State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', _Pid, config_change}, Server) ->
    {stop, config_change, Server};
handle_info({'EXIT', Pid, Reason}, Server) ->
    case ets:lookup(couch_dbs_pid_to_name, Pid) of
    [{Pid, DbName}] ->
        [#entry{waiters = Waiters} = Entry] = ets:lookup(couch_dbs, DbName),
        if Reason /= snappy_nif_not_loaded -> ok; true ->
            Msg = io_lib:format("To open the database `~s`, Apache CouchDB "
                "must be built with Erlang OTP R13B04 or higher.", [DbName]),
            couch_log:error(Msg, [])
        end,
        couch_log:info("db ~s died with reason ~p", [DbName, Reason]),
        if not is_list(Waiters) -> ok; true ->
            [gen_server:reply(Waiter, Reason) || Waiter <- Waiters]
        end,
        true = ets:delete(couch_dbs, DbName),
        true = ets:delete(couch_dbs_pid_to_name, Pid),
        {noreply, db_closed(Server, Entry#entry.db_options)};
    [] ->
        {noreply, Server}
    end;
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(Info, Server) ->
    {stop, {unknown_message, Info}, Server}.

db_opened(Server, Options) ->
    case lists:member(sys_db, Options) of
        false -> Server#server{dbs_open=Server#server.dbs_open + 1};
        true -> Server
    end.

db_closed(Server, Options) ->
    case lists:member(sys_db, Options) of
        false -> Server#server{dbs_open=Server#server.dbs_open - 1};
        true -> Server
    end.


get_configured_engines() ->
    ConfigEntries = config:get("couchdb_engines"),
    Engines = lists:flatmap(fun({Extension, ModuleStr}) ->
        try
            [{Extension, list_to_atom(ModuleStr)}]
        catch _T:_R ->
            []
        end
    end, ConfigEntries),
    case Engines of
        [] ->
            [{"couch", couch_bt_engine}];
        Else ->
            Else
    end.


get_engine(Server, DbName, Options) ->
    #server{
        root_dir = RootDir,
        engines = Engines
    } = Server,
    case couch_util:get_value(engine, Options) of
        Ext when is_binary(Ext) ->
            ExtStr = binary_to_list(Ext),
            case lists:keyfind(ExtStr, 1, Engines) of
                {ExtStr, Engine} ->
                    Path = make_filepath(RootDir, DbName, ExtStr),
                    {ok, {Engine, Path}};
                false ->
                    {error, {invalid_engine_extension, Ext}}
            end;
        _ ->
            get_engine(Server, DbName)
    end.


get_engine(Server, DbName) ->
    #server{
        root_dir = RootDir,
        engines = Engines
    } = Server,
    Possible = get_possible_engines(DbName, RootDir, Engines),
    case Possible of
        [] ->
            get_default_engine(Server, DbName);
        [Engine] ->
            {ok, Engine};
        _ ->
            erlang:error(engine_conflict)
    end.


get_possible_engines(DbName, RootDir, Engines) ->
    lists:foldl(fun({Extension, Engine}, Acc) ->
        Path = make_filepath(RootDir, DbName, Extension),
        case couch_db_engine:exists(Engine, Path) of
            true ->
                [{Engine, Path} | Acc];
            false ->
                Acc
        end
    end, [], Engines).


get_default_engine(Server, DbName) ->
    #server{
        root_dir = RootDir,
        engines = Engines
    } = Server,
    Default = {couch_bt_engine, make_filepath(RootDir, DbName, "couch")},
    case config:get("couchdb", "default_engine") of
        Extension when is_list(Extension) ->
            case lists:keyfind(Extension, 1, Engines) of
                {Extension, Module} ->
                    {ok, {Module, make_filepath(RootDir, DbName, Extension)}};
                false ->
                    Fmt = "Invalid storage engine extension ~s,"
                            " configured engine extensions are: ~s",
                    Exts = [E || {E, _} <- Engines],
                    Args = [Extension, string:join(Exts, ", ")],
                    couch_log:error(Fmt, Args),
                    {ok, Default}
            end;
        _ ->
            {ok, Default}
    end.


make_filepath(RootDir, DbName, Extension) when is_binary(RootDir) ->
    make_filepath(binary_to_list(RootDir), DbName, Extension);
make_filepath(RootDir, DbName, Extension) when is_binary(DbName) ->
    make_filepath(RootDir, binary_to_list(DbName), Extension);
make_filepath(RootDir, DbName, Extension) when is_binary(Extension) ->
    make_filepath(RootDir, DbName, binary_to_list(Extension));
make_filepath(RootDir, DbName, Extension) ->
    filename:join([RootDir, "./" ++ DbName ++ "." ++ Extension]).


get_engine_extensions() ->
    case config:get("couchdb_engines") of
        [] ->
            ["couch"];
        Entries ->
            [Ext || {Ext, _Mod} <- Entries]
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok = meck:new(config, [passthrough]),
    ok = meck:expect(config, get, fun config_get/3),
    ok.

teardown(_) ->
    (catch meck:unload(config)).

config_get("couchdb", "users_db_suffix", _) -> "users_db";
config_get(_, _, _) -> undefined.

maybe_add_sys_db_callbacks_pass_test_() ->
    SysDbCases = [
        "shards/00000000-3fffffff/foo/users_db.1415960794.couch",
        "shards/00000000-3fffffff/foo/users_db.1415960794",
        "shards/00000000-3fffffff/foo/users_db",
        "shards/00000000-3fffffff/users_db.1415960794.couch",
        "shards/00000000-3fffffff/users_db.1415960794",
        "shards/00000000-3fffffff/users_db",

        "shards/00000000-3fffffff/_users.1415960794.couch",
        "shards/00000000-3fffffff/_users.1415960794",
        "shards/00000000-3fffffff/_users",

        "foo/users_db.couch",
        "foo/users_db",
        "users_db.couch",
        "users_db",
        "foo/_users.couch",
        "foo/_users",
        "_users.couch",
        "_users",

        "shards/00000000-3fffffff/foo/_replicator.1415960794.couch",
        "shards/00000000-3fffffff/foo/_replicator.1415960794",
        "shards/00000000-3fffffff/_replicator",
        "foo/_replicator.couch",
        "foo/_replicator",
        "_replicator.couch",
        "_replicator"
    ],

    NonSysDbCases = [
        "shards/00000000-3fffffff/foo/mydb.1415960794.couch",
        "shards/00000000-3fffffff/foo/mydb.1415960794",
        "shards/00000000-3fffffff/mydb",
        "foo/mydb.couch",
        "foo/mydb",
        "mydb.couch",
        "mydb"
    ],
    {
        foreach, fun setup/0, fun teardown/1,
        [
            [should_add_sys_db_callbacks(C) || C <- SysDbCases]
            ++
            [should_add_sys_db_callbacks(?l2b(C)) || C <- SysDbCases]
            ++
            [should_not_add_sys_db_callbacks(C) || C <- NonSysDbCases]
            ++
            [should_not_add_sys_db_callbacks(?l2b(C)) || C <- NonSysDbCases]
        ]
    }.

should_add_sys_db_callbacks(DbName) ->
    {test_name(DbName), ?_test(begin
        Options = maybe_add_sys_db_callbacks(DbName, [other_options]),
        ?assert(lists:member(sys_db, Options)),
        ok
    end)}.
should_not_add_sys_db_callbacks(DbName) ->
    {test_name(DbName), ?_test(begin
        Options = maybe_add_sys_db_callbacks(DbName, [other_options]),
        ?assertNot(lists:member(sys_db, Options)),
        ok
    end)}.

test_name(DbName) ->
    lists:flatten(io_lib:format("~p", [DbName])).


-endif.
