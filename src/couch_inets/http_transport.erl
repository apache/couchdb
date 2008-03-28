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
%
-module(http_transport).

% Internal application API
-export([start/1, connect/3, connect/4, listen/2, listen/3, 
	 accept/2, accept/3, close/2,
	 send/3, controlling_process/3, setopts/3,
	 peername/2, resolve/0]).

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% start(SocketType) -> ok | {error, Reason}
%%      SocketType = ip_comm | {ssl, _}  
%%                                   
%% Description: Makes sure inet_db or ssl is started. 
%%-------------------------------------------------------------------------
start(ip_comm) ->
    case inet_db:start() of
	{ok, _} ->
	    ok;
	{error, {already_started, _}} ->
	    ok;
	Error ->
	    Error
    end;
start({ssl, _}) ->
    case ssl:start() of
	ok ->
	    ok;
	{ok, _} ->
	    ok;
	{error, {already_started,_}} ->
	    ok;
	Error ->
	    Error
    end.

%%-------------------------------------------------------------------------
%% connect(SocketType, Address, IPV6, Timeout) ->
%%                                            {ok, Socket} | {error, Reason}
%%      SocketType = ip_comm | {ssl, SslConfig}  
%%      Address = {Host, Port}
%%      IPV6 = disabled | enabled
%%      Socket = socket()
%%                                   
%% Description: Connects to the Host and Port specified in HTTPRequest.
%%		uses ipv6 if possible.
%%-------------------------------------------------------------------------
connect(SocketType, Address, IPV6) ->
    connect(SocketType, Address, IPV6, infinity).

connect(ip_comm, {Host, Port}, enabled, Timeout) ->
    {Opts, NewHost} = 
	case inet:getaddr(Host, inet6) of
	    {ok, IPAddr = {0, 0, 0, 0, 0, 16#ffff, _, _}} ->
		case inet:getaddr(Host, inet) of
		    {ok,NewIP} ->
			{[binary, {packet, 0}, {active, false},
			  {reuseaddr,true}], NewIP};
		    _Error ->
			{[binary, {packet, 0}, {active, false},
			  {reuseaddr,true}, inet6], IPAddr}
		end;
	    {ok, IPAddr} ->
		{[binary, {packet, 0}, {active, false},
		  {reuseaddr,true}, inet6], IPAddr};
	    _ ->
		{[binary, {packet, 0}, {active, false},
		  {reuseaddr,true}], Host}
	end,
    gen_tcp:connect(NewHost, Port, Opts, Timeout);

connect(ip_comm, {Host, Port}, disabled, Timeout) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr,true}],
    gen_tcp:connect(Host, Port, Opts, Timeout);

connect({ssl, SslConfig}, {Host, Port}, _, Timeout) ->
    Opts = [binary, {active, false}] ++ SslConfig,
    ssl:connect(Host, Port, Opts, Timeout).

%%-------------------------------------------------------------------------
%% listen(SocketType, Port) -> {ok, Socket} | {error, Reason}
%%      SocketType = ip_comm | {ssl, SSLConfig}  
%%      Port = integer() 
%%      Socket = socket()                            
%%
%% Description: Sets up socket to listen on the port Port on the local
%% host using either gen_tcp or ssl. In the gen_tcp case the port
%% might allready have been initiated by a wrapper-program and is
%% given as an Fd that can be retrieved by init:get_argument. The
%% reason for this to enable a HTTP-server not runnig as root to use
%% port 80.
%%-------------------------------------------------------------------------
listen(SocketType, Port) ->
    listen(SocketType, undefined, Port).

listen(ip_comm, Addr, Port) ->
    FdName = list_to_atom("httpd_" ++ integer_to_list(Port)),
    {NewPort, Opt} =
	case init:get_argument(FdName) of
	    {ok, [[FdStr]]} ->
		Fd = list_to_integer(FdStr),
		{0,
		 sock_opt(ip_comm, Addr, [{backlog, 128}, 
					  {reuseaddr,true}, {fd,Fd}, {nodelay, true}])};
	    error ->
		{Port,
		 sock_opt(ip_comm, Addr, 
			  [{backlog, 128}, {reuseaddr, true}, {nodelay, true}])}
	end,
    gen_tcp:listen(NewPort, Opt);

listen({ssl, SSLConfig} = Ssl, Addr, Port) ->
    Opt = sock_opt(Ssl, Addr, SSLConfig),
    ssl:listen(Port, Opt).

%%-------------------------------------------------------------------------
%% accept(SocketType, ListenSocket) -> {ok, Socket} | {error, Reason}
%% accept(SocketType, ListenSocket, Timeout) -> ok | {error, Reason}
%%   SocketType = ip_comm | {ssl, SSLConfig}  
%%   ListenSocket = socket()    
%%   Timeout = infinity | integer() >= 0
%%   Socket = socket()
%%                                   
%% Description: Accepts an incoming connection request on a listen socket,
%% using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
accept(SocketType, ListenSocket) ->
    accept(SocketType, ListenSocket, infinity).
accept(ip_comm, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);
accept({ssl,_SSLConfig}, ListenSocket, Timeout) ->
    ssl:accept(ListenSocket, Timeout).

%%-------------------------------------------------------------------------
%% controlling_process(SocketType, Socket, NewOwner) -> ok | {error, Reason}
%%   SocketType = ip_comm | {ssl, _}  
%%   Socket = socket()        
%%   NewOwner = pid()
%%                                
%% Description: Assigns a new controlling process to Socket. 
%%-------------------------------------------------------------------------
controlling_process(ip_comm, Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner);
controlling_process({ssl, _}, Socket, NewOwner) ->
    ssl:controlling_process(Socket, NewOwner).

%%-------------------------------------------------------------------------
%% setopts(SocketType, Socket, Options) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     Options = list()                              
%% Description: Sets one or more options for a socket, using either
%% gen_tcp or ssl.
%%-------------------------------------------------------------------------
setopts(ip_comm, Socket, Options) ->
    inet:setopts(Socket,Options);
setopts({ssl, _}, Socket, Options) ->
    ssl:setopts(Socket, Options).

%%-------------------------------------------------------------------------
%% send(RequestOrSocketType, Socket, Message) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     Message = list() | binary()                           
%% Description: Sends a packet on a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
send(ip_comm, Socket, Message) ->
    gen_tcp:send(Socket, Message);
send({ssl, _}, Socket, Message) ->
    ssl:send(Socket, Message).

%%-------------------------------------------------------------------------
%% close(SocketType, Socket) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()  
%%                                   
%% Description: Closes a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
close(ip_comm, Socket) ->
    gen_tcp:close(Socket);
close({ssl, _}, Socket) ->
    ssl:close(Socket).

%%-------------------------------------------------------------------------
%% peername(SocketType, Socket) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket() 
%%                          
%% Description: Returns the address and port for the other end of a
%% connection, usning either gen_tcp or ssl.
%%-------------------------------------------------------------------------
peername(ip_comm, Socket) ->
    case inet:peername(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, PeerName};
	{ok,{{A, B, C, D, E, F, G, H}, Port}} ->
	    PeerName =  http_util:integer_to_hexlist(A) ++ ":"++  
		http_util:integer_to_hexlist(B) ++ ":" ++  
		http_util:integer_to_hexlist(C) ++ ":" ++ 
		http_util:integer_to_hexlist(D) ++ ":" ++  
		http_util:integer_to_hexlist(E) ++ ":" ++  
		http_util:integer_to_hexlist(F) ++ ":" ++  
		http_util:integer_to_hexlist(G) ++":"++  
		http_util:integer_to_hexlist(H),
	    {Port, PeerName};
	{error, _} ->
	    {-1, "unknown"}
    end;

peername({ssl, _}, Socket) ->
    case ssl:peername(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, PeerName};
	{error, _} ->
	    {-1, "unknown"}
    end.

%%-------------------------------------------------------------------------
%% resolve() -> HostName
%%     HostName = string()
%%     
%% Description: Returns the local hostname. 
%%-------------------------------------------------------------------------
resolve() ->
    {ok, Name} = inet:gethostname(),
    Name.


%%%========================================================================
%%% Internal functions
%%%========================================================================

%% Address any comes from directive: BindAddress "*"
sock_opt(ip_comm, any = Addr, Opt) -> 
    sock_opt1([{ip, Addr} | Opt]);
sock_opt(ip_comm, undefined, Opt) -> 
    sock_opt1(Opt);
sock_opt(_, any = Addr, Opt) ->
    sock_opt2([{ip, Addr} | Opt]);
sock_opt(_, undefined, Opt) ->
    sock_opt2(Opt);
sock_opt(_, Addr, Opt) when size(Addr) == 4 -> 
    sock_opt2([{ip, Addr} | Opt]);
sock_opt(ip_comm, Addr, Opt) -> 
    sock_opt2([inet6, {ip, Addr} | Opt]);
sock_opt(_, Addr, Opt) ->
    sock_opt2([{ip, Addr} | Opt]).

sock_opt1(Opt) ->
    case has_inet6_supported() of
	yes ->
	    sock_opt2([inet6 | Opt]);
	no ->
	    sock_opt2(Opt)
    end.

sock_opt2(Opt) ->
    [{packet, 0}, {active, false} | Opt].

has_inet6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
	{ok, {0, 0, 0, 0, 0, 16#ffff, _, _}} ->
	    no;
	{ok,_} -> yes;
	_ ->
	    no
    end.
