%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%----------------------------------------------------------------------
%% Purpose: The top supervisor for the http server (httpd) hangs under 
%%          inets_sup.
%%----------------------------------------------------------------------

-module(httpd_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_child/1, stop_child/2]).

%% Supervisor callback
-export([init/1]).


%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(HttpdServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HttpdServices]).

start_child(ConfigFile) ->
    {ok, Spec} = httpd_child_spec(ConfigFile, 15000, []),
    supervisor:start_child(?MODULE, Spec).
    
stop_child(Addr, Port) ->
    Name = {httpd_instance_sup, Addr, Port},
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.
    
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([HttpdServices]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = child_spec(HttpdServices, []),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
%% The format of the httpd service is:
%% httpd_service() -> {httpd,httpd()}
%% httpd()         -> [httpd_config()] | file()
%% httpd_config()  -> {file,file()} |
%%                    {debug,debug()} |
%%                    {accept_timeout,integer()}
%% debug()         -> disable | [debug_options()]
%% debug_options() -> {all_functions,modules()} | 
%%                    {exported_functions,modules()} |
%%                    {disable,modules()}
%% modules()       -> [atom()]
child_spec([], Acc) ->
    Acc;
child_spec([{httpd, HttpdService} | Rest], Acc) ->
    NewHttpdService = mk_tuple_list(HttpdService),
    %%    Acc2 = child_spec2(NewHttpdService,Acc),
    NewAcc=
	case catch child_spec2(NewHttpdService) of
	    {ok,Acc2} ->
		[Acc2|Acc];
	    {error,Reason} ->
		error_msg("failed to create child spec for ~n~p~ndue to: ~p",
			  [HttpdService,Reason]),
%		exit({error,Reason})
		Acc
	end,
    child_spec(Rest,NewAcc).

child_spec2(HttpdService) ->
    Debug = http_util:key1search(HttpdService,debug,[]),
    AcceptTimeout = http_util:key1search(HttpdService,accept_timeout,15000),
    ConfigFile =
    case http_util:key1search(HttpdService,file) of
	undefined -> throw({error,{mandatory_conf_file_missed}});
	File -> File
    end,
    httpd_util:valid_options(Debug,AcceptTimeout,ConfigFile),
    httpd_child_spec(ConfigFile,AcceptTimeout,Debug).


httpd_child_spec(ConfigFile,AcceptTimeout,Debug) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    Port = httpd_util:key1search(ConfigList, port, 80),
	    Addr = httpd_util:key1search(ConfigList, bind_address),
	    {ok, httpd_child_spec(ConfigFile, AcceptTimeout, 
				  Debug, Addr, Port)};
	Error ->
	    Error
    end.

httpd_child_spec(ConfigFile, AcceptTimeout, Debug, Addr, Port) ->
    Name = {httpd_instance_sup, Addr, Port},
    StartFunc = {httpd_instance_sup, start_link,
		 [ConfigFile,AcceptTimeout,Debug]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpd_instance_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.


mk_tuple_list([]) ->
    [];
mk_tuple_list([H={_,_}|T]) ->
    [H|mk_tuple_list(T)];
mk_tuple_list(F) ->
    [{file,F}].

error_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).
