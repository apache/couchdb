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

-export([open/2,create/2,delete/2,all_databases/0,all_databases/1]).
-export([init/1, handle_call/3,sup_start_link/0]).
-export([handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([dev_start/0,is_admin/2,has_admins/0,get_stats/0,config_change/4]).
-export([close_lru/0]).

-include("couch_db.hrl").

-record(server,{
    root_dir = [],
    dbname_regexp,
    max_dbs_open=100,
    dbs_open=0,
    start_time=""
    }).

dev_start() ->
    couch:stop(),
    up_to_date = make:all([load, debug_info]),
    couch:start().

get_stats() ->
    {ok, #server{start_time=Time,dbs_open=Open}} =
            gen_server:call(couch_server, get_server),
    [{start_time, ?l2b(Time)}, {dbs_open, Open}].

sup_start_link() ->
    gen_server:start_link({local, couch_server}, couch_server, [], []).


open(DbName, Options) ->
    Ctx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
    case ets:lookup(couch_dbs, DbName) of
    [#db{fd=Fd, fd_monitor=Lock} = Db] when Lock =/= locked ->
        ets:insert(couch_lru, {DbName, now()}),
        {ok, Db#db{user_ctx=Ctx, fd_monitor=erlang:monitor(process,Fd)}};
    _ ->
        Timeout = couch_util:get_value(timeout, Options, infinity),
        case gen_server:call(couch_server, {open, DbName, Options}, Timeout) of
        {ok, #db{fd=Fd} = Db} ->
            ets:insert(couch_lru, {DbName, now()}),
            {ok, Db#db{user_ctx=Ctx, fd_monitor=erlang:monitor(process,Fd)}};
        Error ->
            Error
        end
    end.

close_lru() ->
    gen_server:call(couch_server, close_lru).

create(DbName, Options) ->
    case gen_server:call(couch_server, {create, DbName, Options}, infinity) of
    {ok, #db{fd=Fd} = Db} ->
        Ctx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
        {ok, Db#db{user_ctx=Ctx, fd_monitor=erlang:monitor(process,Fd)}};
    Error ->
        Error
    end.

delete(DbName, Options) ->
    gen_server:call(couch_server, {delete, DbName, Options}, infinity).

check_dbname(#server{dbname_regexp=RegExp}, DbName) ->
    case re:run(DbName, RegExp, [{capture, none}]) of
    nomatch ->
        case DbName of
            "_users" -> ok;
            "_replicator" -> ok;
            _Else ->
                {error, illegal_database_name}
            end;
    match ->
        ok
    end.

is_admin(User, ClearPwd) ->
    case couch_config:get("admins", User) of
    "-hashed-" ++ HashedPwdAndSalt ->
        [HashedPwd, Salt] = string:tokens(HashedPwdAndSalt, ","),
        couch_util:to_hex(crypto:sha(ClearPwd ++ Salt)) == HashedPwd;
    _Else ->
        false
    end.

has_admins() ->
    couch_config:get("admins") /= [].

get_full_filename(Server, DbName) ->
    filename:join([Server#server.root_dir, "./" ++ DbName ++ ".couch"]).

hash_admin_passwords() ->
    hash_admin_passwords(true).

hash_admin_passwords(Persist) ->
    lists:foreach(
        fun({_User, "-hashed-" ++ _}) ->
            ok; % already hashed
        ({User, ClearPassword}) ->
            Salt = ?b2l(couch_uuids:random()),
            Hashed = couch_util:to_hex(crypto:sha(ClearPassword ++ Salt)),
            couch_config:set("admins",
                User, "-hashed-" ++ Hashed ++ "," ++ Salt, Persist)
        end, couch_config:get("admins")).

init([]) ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    RootDir = couch_config:get("couchdb", "database_dir", "."),
    MaxDbsOpen = list_to_integer(
            couch_config:get("couchdb", "max_dbs_open")),
    ok = couch_config:register(fun ?MODULE:config_change/4),
    ok = couch_file:init_delete_dir(RootDir),
    hash_admin_passwords(),
    {ok, RegExp} = re:compile(
        "^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*" % use the stock CouchDB regex
        "(\\.[0-9]{10,})?$" % but allow an optional shard timestamp at the end
    ),
    ets:new(couch_dbs, [set, protected, named_table, {keypos, #db.name}]),
    ets:new(couch_lru, [set, public, named_table]),
    process_flag(trap_exit, true),
    {ok, #server{root_dir=RootDir,
                dbname_regexp=RegExp,
                max_dbs_open=MaxDbsOpen,
                start_time=httpd_util:rfc1123_date()}}.

terminate(_Reason, _Srv) ->
    ets:foldl(fun(#db{main_pid=Pid}, _) -> couch_util:shutdown_sync(Pid) end,
        nil, couch_dbs),
    ok.

config_change("couchdb", "database_dir", _, _) ->
    exit(whereis(couch_server), config_change);
config_change("couchdb", "max_dbs_open", Max, _) ->
    gen_server:call(couch_server, {set_max_dbs_open, list_to_integer(Max)});
config_change("admins", _, _, Persist) ->
    % spawn here so couch_config doesn't try to call itself
    spawn(fun() -> hash_admin_passwords(Persist) end).

all_databases() ->
    all_databases("").

all_databases(Prefix) ->
    {ok, #server{root_dir=Root}} = gen_server:call(couch_server, get_server),
    NormRoot = couch_util:normpath(Root),
    Filenames =
    filelib:fold_files(Root++Prefix,
        "^[a-z0-9\\_\\$()\\+\\-]*" % stock CouchDB name regex
        "(\\.[0-9]{10,})?"         % optional shard timestamp
        "\\.couch$",               % filename extenstion
        true,
        fun(Filename, AccIn) ->
            NormFilename = couch_util:normpath(Filename),
            case NormFilename -- NormRoot of
            [$/ | RelativeFilename] -> ok;
            RelativeFilename -> ok
            end,
            [list_to_binary(filename:rootname(RelativeFilename, ".couch")) | AccIn]
        end, []),
    {ok, lists:usort(Filenames)}.


maybe_close_lru_db(#server{dbs_open=NumOpen, max_dbs_open=MaxOpen}=Server)
        when NumOpen < MaxOpen ->
    {ok, Server};
maybe_close_lru_db(#server{dbs_open=NumOpen}=Server) ->
    % must free up the lru db.
    case try_close_lru(now()) of
    ok ->
        {ok, Server#server{dbs_open=NumOpen - 1}};
    Error -> Error
    end.

find_oldest_db({DbName, Lru}, Acc) ->
    erlang:min({Lru, DbName}, Acc).

try_close_lru(StartTime) ->
    case ets:foldl(fun find_oldest_db/2, {StartTime, nil}, couch_lru) of
    {StartTime, nil} ->
        {error, all_dbs_active};
    {_, DbName} ->
        % There may exist an extremely small possibility of a race
        % condition here, if a process could lookup the DB before the lock,
        % but fail to monitor the fd before the is_idle check.
        true = ets:update_element(couch_dbs, DbName, {#db.fd_monitor, locked}),
        [#db{main_pid = Pid} = Db] = ets:lookup(couch_dbs, DbName),
        case couch_db:is_idle(Db) of true ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_lru, DbName),
            exit(Pid, kill),
            ok;
        false ->
            true = ets:update_element(couch_dbs, DbName, {#db.fd_monitor, nil}),
            true = ets:insert(couch_lru, {DbName, now()}),
            try_close_lru(StartTime)
        end
    end.

open_async(Server, From, DbName, Filepath, Options) ->
    Parent = self(),
    Opener = spawn_link(fun() ->
        Res = couch_db:start_link(DbName, Filepath, Options),
        gen_server:call(Parent, {open_result, DbName, Res}, infinity),
        unlink(Parent)
    end),
    % icky hack of field values - compactor_pid used to store clients
    true = ets:insert(couch_dbs, #db{
        name = DbName,
        main_pid = Opener,
        compactor_pid = [From],
        fd_monitor = locked
    }),
    Server#server{dbs_open=Server#server.dbs_open + 1}.

handle_call(close_lru, _From, #server{dbs_open=N} = Server) ->
    case try_close_lru(now()) of
    ok ->
        {reply, ok, Server#server{dbs_open = N-1}};
    Error ->
        {reply, Error, Server}
    end;
handle_call(open_dbs_count, _From, Server) ->
    {reply, Server#server.dbs_open, Server};
handle_call({set_dbname_regexp, RegExp}, _From, Server) ->
    {reply, ok, Server#server{dbname_regexp=RegExp}};
handle_call({set_max_dbs_open, Max}, _From, Server) ->
    {reply, ok, Server#server{max_dbs_open=Max}};
handle_call(get_server, _From, Server) ->
    {reply, {ok, Server}, Server};
handle_call({open_result, DbName, {ok, Db}}, _From, Server) ->
    link(Db#db.main_pid),
    % icky hack of field values - compactor_pid used to store clients
    [#db{compactor_pid=Froms}] = ets:lookup(couch_dbs, DbName),
    [gen_server:reply(From, {ok, Db}) || From <- Froms],
    true = ets:insert(couch_dbs, Db),
    true = ets:insert(couch_lru, {DbName, now()}),
    {reply, ok, Server};
handle_call({open_result, DbName, Error}, _From, Server) ->
    % icky hack of field values - compactor_pid used to store clients
    [#db{compactor_pid=Froms}] = ets:lookup(couch_dbs, DbName),
    [gen_server:reply(From, Error) || From <- Froms],
    true = ets:delete(couch_dbs, DbName),
    {reply, ok, Server#server{dbs_open=Server#server.dbs_open - 1}};
handle_call({open, DbName, Options}, From, Server) ->
    case ets:lookup(couch_dbs, DbName) of
    [] ->
        DbNameList = binary_to_list(DbName),
        case check_dbname(Server, DbNameList) of
        ok ->
            case maybe_close_lru_db(Server) of
            {ok, Server2} ->
                Filepath = get_full_filename(Server, DbNameList),
                {noreply, open_async(Server2, From, DbName, Filepath, Options)};
            CloseError ->
                {reply, CloseError, Server}
            end;
        Error ->
            {reply, Error, Server}
        end;
    [#db{compactor_pid = Froms} = Db] when is_list(Froms) ->
        % icky hack of field values - compactor_pid used to store clients
        true = ets:insert(couch_dbs, Db#db{compactor_pid = [From|Froms]}),
        {noreply, Server};
    [#db{} = Db] ->
        {reply, {ok, Db}, Server}
    end;
handle_call({create, DbName, Options}, From, Server) ->
    DbNameList = binary_to_list(DbName),
    case check_dbname(Server, DbNameList) of
    ok ->
        case ets:lookup(couch_dbs, DbName) of
        [] ->
            case maybe_close_lru_db(Server) of
            {ok, Server2} ->
                Filepath = get_full_filename(Server, DbNameList),
                {noreply, open_async(Server2, From, DbName, Filepath,
                        [create | Options])};
            CloseError ->
                {reply, CloseError, Server}
            end;
        [_AlreadyRunningDb] ->
            {reply, file_exists, Server}
        end;
    Error ->
        {reply, Error, Server}
    end;
handle_call({delete, DbName, _Options}, _From, Server) ->
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
            true = ets:delete(couch_lru, DbName),
            exit(Pid, kill),
            [gen_server:reply(F, not_found) || F <- Froms],
            Server#server{dbs_open=Server#server.dbs_open - 1};
        [#db{main_pid=Pid}] ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_lru, DbName),
            exit(Pid, kill),
            Server#server{dbs_open=Server#server.dbs_open - 1}
        end,

        %% Delete any leftover .compact files.  If we don't do this a subsequent
        %% request for this DB will try to open the .compact file and use it.
        couch_file:delete(Server#server.root_dir, FullFilepath ++ ".compact"),

        case couch_file:delete(Server#server.root_dir, FullFilepath) of
        ok ->
            couch_db_update_notifier:notify({deleted, DbName}),
            {reply, ok, Server2};
        {error, enoent} ->
            {reply, not_found, Server2};
        Else ->
            {reply, Else, Server2}
        end;
    Error ->
        {reply, Error, Server}
    end;
handle_call({db_updated, #db{name = DbName} = Db}, _From, Server) ->
    true = ets:insert(couch_dbs, Db),
    true = ets:insert(couch_lru, {DbName, now()}),
    {reply, ok, Server}.


handle_cast(Msg, Server) ->
    {stop, {unknown_cast_message, Msg}, Server}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', _Pid, config_change}, Server) ->
    {stop, config_change, Server};
handle_info({'EXIT', Pid, Reason}, #server{dbs_open=DbsOpen}=Server) ->
    case ets:match_object(couch_dbs, #db{main_pid=Pid, _='_'}) of
    [#db{name = DbName, compactor_pid=Froms}] ->
        ?LOG_INFO("db ~s died with reason ~p", [DbName, Reason]),
        % icky hack of field values - compactor_pid used to store clients
        if is_list(Froms) ->
            [gen_server:reply(From, Reason) || From <- Froms];
        true ->
            ok
        end,
        true = ets:delete(couch_dbs, DbName),
        true = ets:delete(couch_lru, DbName),
        {noreply, Server#server{dbs_open=DbsOpen - 1}};
    [] ->
        {noreply, Server}
    end;
handle_info(Info, Server) ->
    {stop, {unknown_message, Info}, Server}.
