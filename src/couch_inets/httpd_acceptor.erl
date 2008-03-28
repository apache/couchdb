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
-module(httpd_acceptor).

-include("httpd.hrl").

%% External API
-export([start_link/5, start_link/6]).

%% Other exports (for spawn's etc.)
-export([acceptor/5, acceptor/6, acceptor/7]).


%%
%% External API
%%

%% start_link

start_link(Manager, SocketType, Addr, Port, ConfigDb) ->
    start_link(Manager, SocketType, Addr, Port, ConfigDb, 15000).

start_link(Manager, SocketType, Addr, Port, ConfigDb,AcceptTimeout) ->
    Args = [self(), Manager, SocketType, Addr, Port, ConfigDb, AcceptTimeout],
    proc_lib:start_link(?MODULE, acceptor, Args).

acceptor(Parent, Manager, SocketType, Addr, Port, ConfigDb) ->
    acceptor(Parent, Manager, SocketType, Addr, Port, ConfigDb, 15000).
acceptor(Parent, Manager, SocketType, Addr, Port, ConfigDb, AcceptTimeout) ->
    case (catch do_init(SocketType, Addr, Port)) of
	{ok, ListenSocket} ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    acceptor(Manager, SocketType, ListenSocket, ConfigDb, AcceptTimeout);
	Error ->
	    proc_lib:init_ack(Parent, Error),
	    error
    end.
   
do_init(SocketType, Addr, Port) ->
    do_socket_start(SocketType),
    ListenSocket = do_socket_listen(SocketType, Addr, Port),
    {ok, ListenSocket}.


do_socket_start(SocketType) ->
    case http_transport:start(SocketType) of
	ok ->
	    ok;
	{error, Reason} ->
	    throw({error, {socket_start_failed, Reason}})
    end.


do_socket_listen(SocketType, Addr, Port) ->
    case http_transport:listen(SocketType, Addr, Port) of
	{ok, ListenSocket} ->
	    ListenSocket;
	{error, Reason} ->
	    throw({error, {listen, Reason}})
    end.


%% acceptor 

acceptor(Manager, SocketType, ListenSocket, ConfigDb, AcceptTimeout) ->
    case (catch http_transport:accept(SocketType, ListenSocket, 50000)) of
	{ok, Socket} ->
	    handle_connection(Manager, ConfigDb, AcceptTimeout, SocketType, Socket),
	    ?MODULE:acceptor(Manager, SocketType, ListenSocket, ConfigDb,AcceptTimeout);
	{error, Reason} ->
	    handle_error(Reason, ConfigDb, SocketType),
	    ?MODULE:acceptor(Manager, SocketType, ListenSocket, ConfigDb, AcceptTimeout);
	{'EXIT', Reason} ->
	    handle_error({'EXIT', Reason}, ConfigDb, SocketType),
	    ?MODULE:acceptor(Manager, SocketType, ListenSocket, ConfigDb, AcceptTimeout)
    end.


handle_connection(Manager, ConfigDb, AcceptTimeout, SocketType, Socket) ->
    {ok, Pid} = httpd_request_handler:start(Manager, ConfigDb, AcceptTimeout),
    http_transport:controlling_process(SocketType, Socket, Pid),
    httpd_request_handler:socket_ownership_transfered(Pid, SocketType, Socket).

handle_error(timeout, _, _) ->
    ok;

handle_error({enfile, _}, _, _) ->
    %% Out of sockets...
    sleep(200);

handle_error(emfile, _, _) ->
    %% Too many open files -> Out of sockets...
    sleep(200);

handle_error(closed, _, _) ->
    error_logger:info_report("The httpd accept socket was closed by" 
			     "a third party. "
			     "This will not have an impact on inets "
			     "that will open a new accept socket and " 
			     "go on as nothing happened. It does however "
			     "indicate that some other software is behaving "
			     "badly."),
    exit(normal);

%% This will only happen when the client is terminated abnormaly
%% and is not a problem for the server, so we want
%% to terminate normal so that we can restart without any 
%% error messages.
handle_error(econnreset,_,_) ->
    exit(normal);

handle_error(econnaborted, _, _) ->
    ok;

handle_error(esslaccept, _, _) ->
    %% The user has selected to cancel the installation of 
    %% the certifikate, This is not a real error, so we do 
    %% not write an error message.
    ok;

handle_error({'EXIT', Reason}, ConfigDb, SocketType) ->
    String = lists:flatten(io_lib:format("Accept exit: ~p", [Reason])),
    accept_failed(SocketType, ConfigDb, String);

handle_error(Reason, ConfigDb, SocketType) ->
    String = lists:flatten(io_lib:format("Accept error: ~p", [Reason])),
    accept_failed(SocketType, ConfigDb, String).


accept_failed(SocketType, ConfigDb, String) ->
    error_logger:error_report(String),
    mod_log:error_log(SocketType, undefined, ConfigDb, 
		      {0, "unknown"}, String),
    mod_disk_log:error_log(SocketType, undefined, ConfigDb, 
			   {0, "unknown"}, String),
    exit({accept_failed, String}).    

sleep(T) -> receive after T -> ok end.


