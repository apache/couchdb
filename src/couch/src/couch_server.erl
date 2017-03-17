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
-export([close_lru/0, maybe_close_db/1]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

-include_lib("couch/include/couch_db.hrl").

-define(MAX_DBS_OPEN, 100).
-define(RELISTEN_DELAY, 5000).

-record(server,{
    root_dir = [],
    max_dbs_open=?MAX_DBS_OPEN,
    dbs_open=0,
    start_time="",
    update_lru_on_read=true,
    lru = couch_lru:new(fun maybe_close_db/1)
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
    [#db{fd=Fd, fd_monitor=Lock} = Db] when Lock =/= locked ->
        update_lru(DbName),
        {ok, Db#db{user_ctx=Ctx, fd_monitor=erlang:monitor(process,Fd)}};
    _ ->
        Options = maybe_add_sys_db_callbacks(DbName, Options0),
        Timeout = couch_util:get_value(timeout, Options, infinity),
        Create = couch_util:get_value(create_if_missing, Options, false),
        case gen_server:call(couch_server, {open, DbName, Options}, Timeout) of
        {ok, #db{fd=Fd} = Db} ->
            {ok, Db#db{user_ctx=Ctx, fd_monitor=erlang:monitor(process,Fd)}};
        {not_found, no_db_file} when Create ->
            couch_log:warning("creating missing database: ~s", [DbName]),
            couch_server:create(DbName, Options);
        Error ->
            Error
        end
    end.

update_lru(DbName) ->
    gen_server:cast(couch_server, {update_lru, DbName}).

close_lru() ->
    gen_server:call(couch_server, close_lru).

maybe_close_db(DbName) ->
    % First element of return indicates if we should stop closing DBs from the
    % LRU and the second element indicates if we closed a DB.
    case ets:update_element(couch_dbs, DbName, {#db.fd_monitor, locked}) of
    true ->
        [#db{main_pid = Pid} = Db] = ets:lookup(couch_dbs, DbName),
        case couch_db:is_idle(Db) of true ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, Pid),
            exit(Pid, kill),
            {true, true};
        false ->
            true = ets:update_element(couch_dbs, DbName, {#db.fd_monitor, nil}),
            couch_stats:increment_counter([couchdb, couch_server, lru_skip]),
            {false, false}
        end;
    false ->
        {false, true}
    end.

create(DbName, Options0) ->
    Options = maybe_add_sys_db_callbacks(DbName, Options0),
    case gen_server:call(couch_server, {create, DbName, Options}, infinity) of
    {ok, #db{fd=Fd} = Db} ->
        Ctx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
        {ok, Db#db{user_ctx=Ctx, fd_monitor=erlang:monitor(process,Fd)}};
    Error ->
        Error
    end.

delete(DbName, Options) ->
    gen_server:call(couch_server, {delete, DbName, Options}, infinity).

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
        couch_util:to_hex(couch_crypto:hash(sha, ClearPwd ++ Salt)) == HashedPwd;
    _Else ->
        false
    end.

has_admins() ->
    config:get("admins") /= [].

get_full_filename(Server, DbName) ->
    filename:join([Server#server.root_dir, "./" ++ DbName ++ ".couch"]).

hash_admin_passwords() ->
    hash_admin_passwords(true).

hash_admin_passwords(Persist) ->
    lists:foreach(
        fun({User, ClearPassword}) ->
            HashedPassword = couch_passwords:hash_admin_password(ClearPassword),
            config:set("admins", User, ?b2l(HashedPassword), Persist)
        end, couch_passwords:get_unhashed_admins()).

init([]) ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    RootDir = config:get("couchdb", "database_dir", "."),
    MaxDbsOpen = list_to_integer(
            config:get("couchdb", "max_dbs_open", integer_to_list(?MAX_DBS_OPEN))),
    UpdateLruOnRead =
        config:get("couchdb", "update_lru_on_read", "true") =:= "true",
    ok = config:listen_for_changes(?MODULE, nil),
    ok = couch_file:init_delete_dir(RootDir),
    hash_admin_passwords(),
    ets:new(couch_dbs, [set, protected, named_table, {keypos, #db.name}]),
    ets:new(couch_dbs_pid_to_name, [set, protected, named_table]),
    process_flag(trap_exit, true),
    {ok, #server{root_dir=RootDir,
                max_dbs_open=MaxDbsOpen,
                update_lru_on_read=UpdateLruOnRead,
                start_time=couch_util:rfc1123_date()}}.

terminate(Reason, Srv) ->
    couch_log:error("couch_server terminating with ~p, state ~2048p",
                    [Reason,
                     Srv#server{lru = redacted}]),
    ets:foldl(fun(#db{main_pid=Pid}, _) -> couch_util:shutdown_sync(Pid) end,
        nil, couch_dbs),
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
    FinalAcc = try
    filelib:fold_files(Root,
        "^[a-z0-9\\_\\$()\\+\\-]*" % stock CouchDB name regex
        "(\\.[0-9]{10,})?"         % optional shard timestamp
        "\\.couch$",               % filename extension
        true,
            fun(Filename, AccIn) ->
                NormFilename = couch_util:normpath(Filename),
                case NormFilename -- NormRoot of
                [$/ | RelativeFilename] -> ok;
                RelativeFilename -> ok
                end,
                case Fun(couch_util:drop_dot_couch_ext(?l2b(RelativeFilename)), AccIn) of
                {ok, NewAcc} -> NewAcc;
                {stop, NewAcc} -> throw({stop, Fun, NewAcc})
                end
            end, Acc0)
    catch throw:{stop, Fun, Acc1} ->
         Acc1
    end,
    {ok, FinalAcc}.


maybe_close_lru_db(#server{dbs_open=NumOpen, max_dbs_open=MaxOpen}=Server)
        when NumOpen < MaxOpen ->
    {ok, Server};
maybe_close_lru_db(#server{lru=Lru}=Server) ->
    case couch_lru:close(Lru) of
        false ->
            {ok, Server};
        {true, NewLru} ->
            maybe_close_lru_db(db_closed(Server#server{lru = NewLru}))
    end.

open_async(Server, From, DbName, Filepath, Options) ->
    Parent = self(),
    T0 = os:timestamp(),
    Opener = spawn_link(fun() ->
        Res = couch_db:start_link(DbName, Filepath, Options),
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
    % icky hack of field values - compactor_pid used to store clients
    % and fd used for opening request info
    true = ets:insert(couch_dbs, #db{
        name = DbName,
        fd = ReqType,
        main_pid = Opener,
        compactor_pid = [From],
        fd_monitor = locked,
        options = Options
    }),
    true = ets:insert(couch_dbs_pid_to_name, {Opener, DbName}),
    db_opened(Server).

handle_call(close_lru, _From, #server{lru=Lru} = Server) ->
    case couch_lru:close(Lru) of
        false ->
            {reply, ok, Server};
        {true, NewLru} ->
            {reply, ok, db_closed(Server#server{lru = NewLru})}
    end;
handle_call(open_dbs_count, _From, Server) ->
    {reply, Server#server.dbs_open, Server};
handle_call({set_update_lru_on_read, UpdateOnRead}, _From, Server) ->
    {reply, ok, Server#server{update_lru_on_read=UpdateOnRead}};
handle_call({set_max_dbs_open, Max}, _From, Server) ->
    {reply, ok, Server#server{max_dbs_open=Max}};
handle_call(get_server, _From, Server) ->
    {reply, {ok, Server}, Server};
handle_call({open_result, T0, DbName, {ok, Db}}, {FromPid, _Tag}, Server) ->
    true = ets:delete(couch_dbs_pid_to_name, FromPid),
    OpenTime = timer:now_diff(os:timestamp(), T0) / 1000,
    couch_stats:update_histogram([couchdb, db_open_time], OpenTime),
    % icky hack of field values - compactor_pid used to store clients
    % and fd used to possibly store a creation request
    case ets:lookup(couch_dbs, DbName) of
        [] ->
            % db was deleted during async open
            exit(Db#db.main_pid, kill),
            {reply, ok, Server};
        [#db{fd=ReqType, compactor_pid=Froms}] ->
            link(Db#db.main_pid),
            [gen_server:reply(From, {ok, Db}) || From <- Froms],
            % Cancel the creation request if it exists.
            case ReqType of
                {create, DbName, _Filepath, _Options, CrFrom} ->
                    gen_server:reply(CrFrom, file_exists);
                _ ->
                    ok
            end,
            true = ets:insert(couch_dbs, Db),
            true = ets:insert(couch_dbs_pid_to_name, {Db#db.main_pid, DbName}),
            Lru = couch_lru:insert(DbName, Server#server.lru),
            {reply, ok, Server#server{lru = Lru}}
    end;
handle_call({open_result, T0, DbName, {error, eexist}}, From, Server) ->
    handle_call({open_result, T0, DbName, file_exists}, From, Server);
handle_call({open_result, _T0, DbName, Error}, {FromPid, _Tag}, Server) ->
    % icky hack of field values - compactor_pid used to store clients
    case ets:lookup(couch_dbs, DbName) of
        [] ->
            % db was deleted during async open
            {reply, ok, Server};
        [#db{fd=ReqType, compactor_pid=Froms}] ->
            [gen_server:reply(From, Error) || From <- Froms],
            couch_log:info("open_result error ~p for ~s", [Error, DbName]),
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, FromPid),
            NewServer = case ReqType of
                {create, DbName, Filepath, Options, CrFrom} ->
                    open_async(Server, CrFrom, DbName, Filepath, Options);
                _ ->
                    Server
            end,
            {reply, ok, db_closed(NewServer)}
    end;
handle_call({open, DbName, Options}, From, Server) ->
    case ets:lookup(couch_dbs, DbName) of
    [] ->
        DbNameList = binary_to_list(DbName),
        case check_dbname(Server, DbNameList) of
        ok ->
            {ok, Server2} = maybe_close_lru_db(Server),
            Filepath = get_full_filename(Server, DbNameList),
            {noreply, open_async(Server2, From, DbName, Filepath, Options)};
        Error ->
            {reply, Error, Server}
        end;
    [#db{compactor_pid = Froms} = Db] when is_list(Froms) ->
        % icky hack of field values - compactor_pid used to store clients
        true = ets:insert(couch_dbs, Db#db{compactor_pid = [From|Froms]}),
        if length(Froms) =< 10 -> ok; true ->
            Fmt = "~b clients waiting to open db ~s",
            couch_log:info(Fmt, [length(Froms), DbName])
        end,
        {noreply, Server};
    [#db{} = Db] ->
        {reply, {ok, Db}, Server}
    end;
handle_call({create, DbName, Options}, From, Server) ->
    DbNameList = binary_to_list(DbName),
    Filepath = get_full_filename(Server, DbNameList),
    case check_dbname(Server, DbNameList) of
    ok ->
        case ets:lookup(couch_dbs, DbName) of
        [] ->
            {ok, Server2} = maybe_close_lru_db(Server),
            {noreply, open_async(Server2, From, DbName, Filepath,
                [create | Options])};
        [#db{fd=open}=Db] ->
            % We're trying to create a database while someone is in
            % the middle of trying to open it. We allow one creator
            % to wait while we figure out if it'll succeed.
            % icky hack of field values - fd used to store create request
            CrOptions = [create | Options],
            NewDb = Db#db{fd={create, DbName, Filepath, CrOptions, From}},
            true = ets:insert(couch_dbs, NewDb),
            {noreply, Server};
        [_AlreadyRunningDb] ->
            {reply, file_exists, Server}
        end;
    Error ->
        {reply, Error, Server}
    end;
handle_call({delete, DbName, Options}, _From, Server) ->
    DbNameList = binary_to_list(DbName),
    case check_dbname(Server, DbNameList) of
    ok ->
        FullFilepath = get_full_filename(Server, DbNameList),
        Server2 =
        case ets:lookup(couch_dbs, DbName) of
        [] -> Server;
        [#db{main_pid=Pid, compactor_pid=Froms}] when is_list(Froms) ->
            % icky hack of field values - compactor_pid used to store clients
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, Pid),
            exit(Pid, kill),
            [gen_server:reply(F, not_found) || F <- Froms],
            db_closed(Server);
        [#db{main_pid=Pid}] ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, Pid),
            exit(Pid, kill),
            db_closed(Server)
        end,

        %% Delete any leftover compaction files. If we don't do this a
        %% subsequent request for this DB will try to open them to use
        %% as a recovery.
        lists:foreach(fun(Ext) ->
            couch_file:delete(Server#server.root_dir, FullFilepath ++ Ext)
        end, [".compact", ".compact.data", ".compact.meta"]),
        couch_file:delete(Server#server.root_dir, FullFilepath ++ ".compact"),

        couch_db_plugin:on_delete(DbName, Options),

        DelOpt = [{context, delete} | Options],
        case couch_file:delete(Server#server.root_dir, FullFilepath, DelOpt) of
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
handle_call({db_updated, #db{}=Db}, _From, Server0) ->
    #db{name = DbName, instance_start_time = StartTime} = Db,
    Server = try ets:lookup_element(couch_dbs, DbName, #db.instance_start_time) of
        StartTime ->
            true = ets:insert(couch_dbs, Db),
            Lru = couch_lru:update(DbName, Server0#server.lru),
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
handle_cast(Msg, Server) ->
    {stop, {unknown_cast_message, Msg}, Server}.

code_change(_OldVsn, #server{}=State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', _Pid, config_change}, Server) ->
    {stop, config_change, Server};
handle_info({'EXIT', Pid, Reason}, Server) ->
    case ets:lookup(couch_dbs_pid_to_name, Pid) of
    [{Pid, DbName}] ->
        [#db{compactor_pid=Froms}] = ets:lookup(couch_dbs, DbName),
        if Reason /= snappy_nif_not_loaded -> ok; true ->
            Msg = io_lib:format("To open the database `~s`, Apache CouchDB "
                "must be built with Erlang OTP R13B04 or higher.", [DbName]),
            couch_log:error(Msg, [])
        end,
        couch_log:info("db ~s died with reason ~p", [DbName, Reason]),
        % icky hack of field values - compactor_pid used to store clients
        if is_list(Froms) ->
            [gen_server:reply(From, Reason) || From <- Froms];
        true ->
            ok
        end,
        true = ets:delete(couch_dbs, DbName),
        true = ets:delete(couch_dbs_pid_to_name, Pid),
        {noreply, db_closed(Server)};
    [] ->
        {noreply, Server}
    end;
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(Info, Server) ->
    {stop, {unknown_message, Info}, Server}.

db_opened(Server) ->
    Server#server{dbs_open=Server#server.dbs_open + 1}.

db_closed(Server) ->
    Server#server{dbs_open=Server#server.dbs_open - 1}.

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
