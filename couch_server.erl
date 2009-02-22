% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_server).
-behaviour(gen_server).
-behaviour(application).

-export([start/0,start/1,start/2,stop/0,stop/1,restart/0]).
-export([open/2,create/2,delete/2,all_databases/0,get_version/0]).
-export([init/1, handle_call/3,sup_start_link/0]).
-export([handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([dev_start/0,is_admin/2,has_admins/0,get_stats/0]).

-include("couch_db.hrl").

-record(server,{
    root_dir = [],
    dbname_regexp,
    max_dbs_open=100,
    dbs_open=0,
    start_time=""
    }).

start() ->
    start(["default.ini"]).

start(IniFiles) ->
    couch_server_sup:start_link(IniFiles).

start(_Type, _Args) ->
    start().

restart() ->
    stop(),
    start().
    
stop() ->
    couch_server_sup:stop().

stop(_Reason) ->
    stop().

dev_start() ->
    stop(),
    up_to_date = make:all([load, debug_info]),
    start().

get_version() ->
    Apps = application:loaded_applications(),
    case lists:keysearch(couch, 1, Apps) of
    {value, {_, _, Vsn}} ->
        Vsn;
    false ->
        "0.0.0"
    end.

get_stats() ->
    {ok, #server{start_time=Time,dbs_open=Open}} =
            gen_server:call(couch_server, get_server),
    [{start_time, ?l2b(Time)}, {dbs_open, Open}].

sup_start_link() ->
    gen_server:start_link({local, couch_server}, couch_server, [], []).

open(DbName, Options) ->
    case gen_server:call(couch_server, {open, DbName, Options}) of
    {ok, MainPid} ->
        Ctx = proplists:get_value(user_ctx, Options, #user_ctx{}),
        couch_db:open_ref_counted(MainPid, Ctx);
    Error ->
        Error
    end.

create(DbName, Options) ->
    case gen_server:call(couch_server, {create, DbName, Options}) of
    {ok, MainPid} ->
        Ctx = proplists:get_value(user_ctx, Options, #user_ctx{}),
        couch_db:open_ref_counted(MainPid, Ctx);
    Error ->
        Error
    end.

delete(DbName, Options) ->
    gen_server:call(couch_server, {delete, DbName, Options}).

check_dbname(#server{dbname_regexp=RegExp}, DbName) ->
    case regexp:match(DbName, RegExp) of
    nomatch ->
        {error, illegal_database_name};
    _Match ->
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
    lists:foreach(
        fun({_User, "-hashed-" ++ _}) ->
            ok; % already hashed
        ({User, ClearPassword}) ->
            Salt = ?b2l(couch_util:new_uuid()),
            Hashed = couch_util:to_hex(crypto:sha(ClearPassword ++ Salt)),
            couch_config:set("admins", User, "-hashed-" ++ Hashed ++ "," ++ Salt)
        end, couch_config:get("admins")).

init([]) ->
    % read config and register for configuration changes
    
    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    RootDir = couch_config:get("couchdb", "database_dir", "."),
    MaxDbsOpen = list_to_integer(
            couch_config:get("couchdb", "max_dbs_open")),
    Self = self(),
    ok = couch_config:register(
        fun("couchdb", "database_dir") ->
            exit(Self, config_change)
        end),
    ok = couch_config:register(
        fun("couchdb", "max_dbs_open", Max) ->
            gen_server:call(couch_server,
                    {set_max_dbs_open, list_to_integer(Max)})
        end),
    ok = couch_config:register(
        fun("admins") ->
            hash_admin_passwords()
        end),
    hash_admin_passwords(),
    {ok, RegExp} = regexp:parse("^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*$"),
    ets:new(couch_dbs_by_name, [set, private, named_table]),
    ets:new(couch_dbs_by_pid, [set, private, named_table]),
    ets:new(couch_dbs_by_lru, [ordered_set, private, named_table]),
    process_flag(trap_exit, true),
    {ok, #server{root_dir=RootDir,
                dbname_regexp=RegExp,
                max_dbs_open=MaxDbsOpen,
                start_time=httpd_util:rfc1123_date()}}.

terminate(_Reason, _Server) ->
    ok.

all_databases() ->
    {ok, #server{root_dir=Root}} = gen_server:call(couch_server, get_server),
    Filenames =
    filelib:fold_files(Root, "^[a-z0-9\\_\\$()\\+\\-]*[\\.]couch$", true,
        fun(Filename, AccIn) ->
            case Filename -- Root of
            [$/ | RelativeFilename] -> ok;
            RelativeFilename -> ok
            end,
            [list_to_binary(filename:rootname(RelativeFilename, ".couch")) | AccIn]
        end, []),
    {ok, Filenames}.


maybe_close_lru_db(#server{dbs_open=NumOpen, max_dbs_open=MaxOpen}=Server)
        when NumOpen < MaxOpen ->
    {ok, Server};
maybe_close_lru_db(#server{dbs_open=NumOpen}=Server) ->
    % must free up the lru db.
    case try_close_lru(now()) of
    ok -> 
        couch_stats_collector:decrement({couchdb, open_databases}),   
        {ok, Server#server{dbs_open=NumOpen - 1}};
    Error -> Error
    end.

try_close_lru(StartTime) ->
    LruTime = ets:first(couch_dbs_by_lru),
    if LruTime > StartTime ->
        % this means we've looped through all our opened dbs and found them
        % all in use.
        {error, all_dbs_active};
    true ->
        [{_, DbName}] = ets:lookup(couch_dbs_by_lru, LruTime),
        [{_, {MainPid, LruTime}}] = ets:lookup(couch_dbs_by_name, DbName),
        case couch_db:is_idle(MainPid) of
        true ->
            exit(MainPid, kill),
            receive {'EXIT', MainPid, _Reason} -> ok end,
            true = ets:delete(couch_dbs_by_lru, LruTime),
            true = ets:delete(couch_dbs_by_name, DbName),
            true = ets:delete(couch_dbs_by_pid, MainPid),
            ok;
        false ->
            % this still has referrers. Go ahead and give it a current lru time
            % and try the next one in the table.
            NewLruTime = now(),
            true = ets:insert(couch_dbs_by_name, {DbName, {MainPid, NewLruTime}}),
            true = ets:insert(couch_dbs_by_pid, {MainPid, DbName}),
            true = ets:delete(couch_dbs_by_lru, LruTime),
            true = ets:insert(couch_dbs_by_lru, {NewLruTime, DbName}),
            try_close_lru(StartTime)
        end
    end.

handle_call({set_max_dbs_open, Max}, _From, Server) ->
    {reply, ok, Server#server{max_dbs_open=Max}};
handle_call(get_server, _From, Server) ->
    {reply, {ok, Server}, Server};
handle_call({open, DbName, Options}, _From, Server) ->
    DbNameList = binary_to_list(DbName),
    case check_dbname(Server, DbNameList) of
    ok ->
        Filepath = get_full_filename(Server, DbNameList),
        LruTime = now(),
        case ets:lookup(couch_dbs_by_name, DbName) of
        [] ->
            case maybe_close_lru_db(Server) of
            {ok, Server2} ->
                case couch_db:start_link(DbName, Filepath, Options) of
                {ok, MainPid} ->
                    true = ets:insert(couch_dbs_by_name, {DbName, {MainPid, LruTime}}),
                    true = ets:insert(couch_dbs_by_pid, {MainPid, DbName}),
                    true = ets:insert(couch_dbs_by_lru, {LruTime, DbName}),
                    DbsOpen = Server2#server.dbs_open + 1,
                    couch_stats_collector:increment({couchdb, open_databases}),
                    {reply, {ok, MainPid},
                            Server2#server{dbs_open=DbsOpen}};
                Error ->
                    {reply, Error, Server2}
                end;
            CloseError ->
                {reply, CloseError, Server}
            end;
        [{_, {MainPid, PrevLruTime}}] ->
            true = ets:insert(couch_dbs_by_name, {DbName, {MainPid, LruTime}}),
            true = ets:delete(couch_dbs_by_lru, PrevLruTime),
            true = ets:insert(couch_dbs_by_lru, {LruTime, DbName}),
            {reply, {ok, MainPid}, Server}
        end;
    Error ->
        {reply, Error, Server}
    end;
handle_call({create, DbName, Options}, _From, Server) ->
    DbNameList = binary_to_list(DbName),
    case check_dbname(Server, DbNameList) of
    ok ->
        Filepath = get_full_filename(Server, DbNameList),

        case ets:lookup(couch_dbs_by_name, DbName) of
        [] ->
            case maybe_close_lru_db(Server) of
            {ok, Server2} ->
                case couch_db:start_link(DbName, Filepath, [create|Options]) of
                {ok, MainPid} ->
                    LruTime = now(),
                    true = ets:insert(couch_dbs_by_name,
                            {DbName, {MainPid, LruTime}}),
                    true = ets:insert(couch_dbs_by_pid, {MainPid, DbName}),
                    true = ets:insert(couch_dbs_by_lru, {LruTime, DbName}),
                    DbsOpen = Server2#server.dbs_open + 1,
                    couch_stats_collector:increment({couchdb, open_databases}),
                    couch_db_update_notifier:notify({created, DbName}),
                    {reply, {ok, MainPid},
                            Server2#server{dbs_open=DbsOpen}};
                Error ->
                    {reply, Error, Server2}
                end;
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
        case ets:lookup(couch_dbs_by_name, DbName) of
        [] -> Server;
        [{_, {Pid, LruTime}}] ->
            exit(Pid, kill),
            receive {'EXIT', Pid, _Reason} -> ok end,
            true = ets:delete(couch_dbs_by_name, DbName),
            true = ets:delete(couch_dbs_by_pid, Pid),
            true = ets:delete(couch_dbs_by_lru, LruTime),
            couch_stats_collector:decrement({couchdb, open_databases}),
            Server#server{dbs_open=Server#server.dbs_open - 1}
        end,
        case file:delete(FullFilepath) of
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
    end.

handle_cast(Msg, _Server) ->
    exit({unknown_cast_message, Msg}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
handle_info({'EXIT', _Pid, config_change}, _Server) ->
    exit(kill);
handle_info({'EXIT', Pid, _Reason}, #server{dbs_open=DbsOpen}=Server) ->
    [{Pid, DbName}] = ets:lookup(couch_dbs_by_pid, Pid),
    [{DbName, {Pid, LruTime}}] = ets:lookup(couch_dbs_by_name, DbName),
    true = ets:delete(couch_dbs_by_pid, Pid),
    true = ets:delete(couch_dbs_by_name, DbName),
    true = ets:delete(couch_dbs_by_lru, LruTime),
    couch_stats_collector:decrement({couchdb, open_databases}),
    {noreply, Server#server{dbs_open=DbsOpen - 1}};
handle_info(Info, _Server) ->
    exit({unknown_message, Info}).
