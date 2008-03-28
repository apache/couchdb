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

-export([start/0,start/1,start/2,stop/0,stop/1]).
-export([open/1,create/2,delete/1,all_databases/0,get_version/0]).
-export([init/1, handle_call/3,sup_start_link/2]).
-export([handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([dev_start/0,remote_restart/0]).

-include("couch_db.hrl").

-record(server,{
    root_dir = [],
    dbname_regexp,
    options=[]
    }).

start() ->
    start("").

start(IniFile) when is_atom(IniFile) ->
    couch_server_sup:start_link(atom_to_list(IniFile) ++ ".ini");
start(IniNum) when is_integer(IniNum) ->
    couch_server_sup:start_link("couch" ++ integer_to_list(IniNum) ++ ".ini");
start(IniFile) ->
    couch_server_sup:start_link(IniFile).

start(_Type, _Args) ->
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

sup_start_link(RootDir, Options) ->
    gen_server:start_link({local, couch_server}, couch_server, {RootDir, Options}, []).

open(Filename) ->
    gen_server:call(couch_server, {open, Filename}).

create(Filename, Options) ->
    gen_server:call(couch_server, {create, Filename, Options}).

delete(Filename) ->
    gen_server:call(couch_server, {delete, Filename}).

remote_restart() ->
    gen_server:call(couch_server, remote_restart).

init({RootDir, Options}) ->
    {ok, RegExp} = regexp:parse("^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*$"),
    {ok, #server{root_dir=RootDir, dbname_regexp=RegExp, options=Options}}.

check_filename(#server{dbname_regexp=RegExp}, Filename) ->
    case regexp:match(Filename, RegExp) of
    nomatch ->
        {error, illegal_database_name};
    _Match ->
        ok
    end.

get_full_filename(Server, Filename) ->
    filename:join([Server#server.root_dir, "./" ++ Filename ++ ".couch"]).


terminate(_Reason, _Server) ->
    ok.

all_databases() ->
    {ok, Root} = gen_server:call(couch_server, get_root),
    Filenames =
    filelib:fold_files(Root, "^[a-z0-9\\_\\$()\\+\\-]*[\\.]couch$", true,
        fun(Filename, AccIn) ->
            case Filename -- Root of
            [$/ | RelativeFilename] -> ok;
            RelativeFilename -> ok
            end,
            [filename:rootname(RelativeFilename, ".couch") | AccIn]
        end, []),
    {ok, Filenames}.


handle_call(get_root, _From, #server{root_dir=Root}=Server) ->
    {reply, {ok, Root}, Server};
handle_call({open, Filename}, From, Server) ->
    case check_filename(Server, Filename) of
    {error, Error} ->
        {reply, {error, Error}, Server};
    ok ->
        Filepath = get_full_filename(Server, Filename),
        Result = supervisor:start_child(couch_server_sup,
            {Filename,
                {couch_db, open, [Filename, Filepath]},
                transient ,
                infinity,
                supervisor,
                [couch_db]}),
        case Result of
        {ok, Db} ->
            {reply, {ok, Db}, Server};
        {error, already_present} ->
            ok = supervisor:delete_child(couch_server_sup, Filename),
            % call self recursively
            handle_call({open, Filename}, From, Server);
        {error, {already_started, Db}} ->
            {reply, {ok, Db}, Server};
        {error, {not_found, _}} ->
            {reply, not_found, Server};
        {error, {Error, _}} ->
            {reply, {error, Error}, Server}
        end
    end;
handle_call({create, Filename, Options}, _From, Server) ->
    case check_filename(Server, Filename) of
    {error, Error} ->
        {reply, {error, Error}, Server};
    ok ->
        Filepath = get_full_filename(Server, Filename),
        ChildSpec = {Filename,
                {couch_db, create, [Filename, Filepath, Options]},
                transient,
                infinity,
                supervisor,
                [couch_db]},
        Result =
        case supervisor:delete_child(couch_server_sup, Filename) of
        ok ->
            sup_start_child(couch_server_sup, ChildSpec);
        {error, not_found} ->
            sup_start_child(couch_server_sup, ChildSpec);
        {error, running} ->
            % a server process for this database already started. Maybe kill it
            case lists:member(overwrite, Options) of
            true ->
                supervisor:terminate_child(couch_server_sup, Filename),
                ok = supervisor:delete_child(couch_server_sup, Filename),
                sup_start_child(couch_server_sup, ChildSpec);
            false ->
                {error, database_already_exists}
            end
        end,
        case Result of
        {ok, _Db} -> couch_db_update_notifier:notify({created, Filename});
        _ -> ok
        end,
        {reply, Result, Server}
    end;
handle_call({delete, Filename}, _From, Server) ->
    FullFilepath = get_full_filename(Server, Filename),
    supervisor:terminate_child(couch_server_sup, Filename),
    supervisor:delete_child(couch_server_sup, Filename),
    case file:delete(FullFilepath) of
    ok ->
        couch_db_update_notifier:notify({deleted, Filename}),
        {reply, ok, Server};
    {error, enoent} ->
        {reply, not_found, Server};
    Else ->
        {reply, Else, Server}
    end;
handle_call(remote_restart, _From, #server{options=Options}=Server) ->
    case proplists:get_value(remote_restart, Options) of
    true ->
        exit(self(), restart);
    _ ->
        ok
    end,
    {reply, ok, Server}.

% this function is just to strip out the child spec error stuff if hit
sup_start_child(couch_server_sup, ChildSpec) ->
    case supervisor:start_child(couch_server_sup, ChildSpec) of
    {error, {Error, _ChildInfo}} ->
        {error, Error};
    Else ->
        Else
    end.

handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
