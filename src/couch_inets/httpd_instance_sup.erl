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
%% Purpose: The top supervisor for an instance of the http server. (You may
%%          have several instances running on the same machine.) Hangs under
%%          httpd_sup.
%%----------------------------------------------------------------------

-module(httpd_instance_sup).

-behaviour(supervisor).

-export([init/1]).

%% Internal API
-export([start/1, start_link/1, start_link/3, start2/1, start_link2/1, 
	 stop/1, stop/2, stop2/1]).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([ConfigFile, ConfigList, AcceptTimeout, Debug, Addr, Port]) -> 
    httpd_util:enable_debug(Debug),
    Flags = {one_for_one, 0, 1},
    Children  = [sup_spec(httpd_acceptor_sup, Addr, Port), 
		 sup_spec(httpd_misc_sup, Addr, Port), 
		 worker_spec(httpd_manager, Addr, Port, 
			     ConfigFile, ConfigList,AcceptTimeout)],
    {ok, {Flags, Children}}.


%%%=========================================================================
%%%  ??? functions
%%%=========================================================================

start(ConfigFile) ->
    case start_link(ConfigFile) of
	{ok, Pid} ->
	    unlink(Pid),
	    {ok, Pid};

	Else ->
	    Else
    end.

start_link(Config) ->
    case catch httpd_options(Config) of
	{error,Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason};
	{ConfigFile,AcceptTimeout,Debug} -> 
	    start_link(ConfigFile, AcceptTimeout, Debug)
    end.
start_link(ConfigFile, AcceptTimeout, Debug) ->
    case get_addr_and_port(ConfigFile) of
	{ok, ConfigList, Addr, Port} ->
	    Name    = make_name(Addr, Port),
	    SupName = {local, Name},
	    supervisor:start_link(SupName, ?MODULE, 
				  [ConfigFile, ConfigList ,AcceptTimeout ,
				   Debug, Addr, Port]);	
	{error, Reason} ->
	    error_logger:error_report(Reason),
	    {stop, Reason}
    end.

    
start2(ConfigList) ->
    case start_link2(ConfigList) of
	{ok, Pid} ->
	    unlink(Pid),
	    {ok, Pid};

	Else ->
	    Else
    end.

    
start_link2(ConfigList) ->
    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
    Name    = make_name(Addr, Port),
    SupName = {local, Name},
    Debug = [],
    AcceptTimeout = 15000,
    supervisor:start_link(SupName, ?MODULE, 
			  [undefined, ConfigList, AcceptTimeout, 
			   Debug, Addr, Port]).
    

stop(Pid) when pid(Pid) ->
    do_stop(Pid);
stop(ConfigFile) when list(ConfigFile) ->
    case get_addr_and_port(ConfigFile) of
	{ok, _, Addr, Port} ->
	    stop(Addr, Port);
	    
	Error ->
	    Error
    end;
stop(_StartArgs) ->
    ok.


stop(Addr, Port) when integer(Port) ->
    Name = make_name(Addr, Port), 
    case whereis(Name) of
	Pid when pid(Pid) ->
	    do_stop(Pid),
	    ok;
	_ ->
	    not_started
    end.
    

stop2(ConfigList) when list(ConfigList) ->
    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
    stop(Addr, Port).

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
do_stop(Pid) ->
    exit(Pid, shutdown).

sup_spec(SupModule, Addr, Port) ->
    Name = {SupModule, Addr, Port},
    StartFunc = {SupModule, start_link, [Addr, Port]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [SupModule],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.
    
worker_spec(WorkerModule, Addr, Port, ConfigFile, ConfigList, AcceptTimeout) ->
    Name = {WorkerModule, Addr, Port},
    StartFunc = {WorkerModule, start_link, 
		 [ConfigFile, ConfigList, AcceptTimeout]}, 
    Restart = permanent, 
    Shutdown = 4000,
    Modules = [WorkerModule],
    Type = worker,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.

httpd_options(Config) ->
    OptionList = mk_tuple_list(Config),
    Debug = http_util:key1search(OptionList,debug,[]),
    AcceptTimeout = http_util:key1search(OptionList,accept_timeout,15000),
    ConfigFile =
    case http_util:key1search(OptionList,file) of
	undefined -> throw({error,{mandatory_conf_file_missed}});
	File -> File
    end,
    httpd_util:valid_options(Debug,AcceptTimeout,ConfigFile),
    {ConfigFile, AcceptTimeout, Debug}.

mk_tuple_list([]) -> 
    [];
mk_tuple_list([H={_,_}|T]) -> 
    [H|mk_tuple_list(T)];
mk_tuple_list(F) when list(F) ->
    [{file,F}].

make_name(Addr,Port) ->
    httpd_util:make_name("httpd_instance_sup",Addr,Port).

get_addr_and_port(ConfigFile) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    {ok, Addr, Port} = get_addr_and_port2(ConfigList),
	    {ok, ConfigList, Addr, Port};
	Error ->
	    Error
    end.

get_addr_and_port2(ConfigList) ->
    Port = httpd_util:key1search(ConfigList, port, 80),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    {ok, Addr, Port}.
