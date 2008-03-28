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
%% Description: Implements a request handler process for the HTTP server.  
%% 

-module(httpd_request_handler).

-behaviour(gen_server).

%% Application internal API
-export([start/2, start/3, socket_ownership_transfered/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("httpd.hrl").
-include("http_internal.hrl").

-record(state, {mod,     %% #mod{}
		manager, %% pid()
		status,  %% accept | busy | blocked
		mfa,     %% {Module, Function, Args} 
		max_keep_alive_request = infinity, %% integer() | infinity
		response_sent = false, %% true | false 
		timeout,  %% infinity | integer() > 0
		timer,     %% ref() - Request timer
		headers,  %% #http_request_h{}
		body      %% binary()
	       }).

%%====================================================================
%% Application internal API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid} | ignore | {error,Error}
%% Description: Starts a httpd-request handler process. Intended to be
%% called by the httpd acceptor process.
%%--------------------------------------------------------------------
start(Manager, ConfigDB) ->
    start(Manager, ConfigDB, 15000).
start(Manager, ConfigDB, AcceptTimeout) ->
    proc_lib:start(?MODULE, init, [[Manager, ConfigDB,AcceptTimeout]]).

%%--------------------------------------------------------------------
%% socket_ownership_transfered(Pid, SocketType, Socket) -> void()
%%
%% Pid = pid()
%% SocketType = ip_comm | ssl 
%% Socket = socket()
%%
%% Description: Send a message to the request handler process
%% confirming that the socket ownership has now sucssesfully been
%% transfered to it. Intended to be called by the httpd acceptor
%% process.
%%--------------------------------------------------------------------
socket_ownership_transfered(Pid, SocketType, Socket) ->
    Pid ! {socket_ownership_transfered, SocketType, Socket}.

%%--------------------------------------------------------------------
%% Function: init(Args) -> _
%%
%% Description: Initiates the server. Obs special init that uses 
%% gen_server:enter_loop/3. This is used instead of the normal
%% gen_server callback init, as a more complex init than the
%% gen_server provides is needed. 
%%--------------------------------------------------------------------
init([Manager, ConfigDB,AcceptTimeout]) ->
    %% Make sure this process terminates if the httpd manager process
    %% should die!
    link(Manager), 
    %% At this point the function httpd_request_handler:start/2 will return.
    proc_lib:init_ack({ok, self()}),
    
    {SocketType, Socket} = await_socket_ownership_transfer(AcceptTimeout),
    
    Resolve = http_transport:resolve(),
    Peername = httpd_socket:peername(SocketType, Socket),
    InitData = #init_data{peername = Peername, resolve = Resolve},
    Mod = #mod{config_db = ConfigDB, 
	       socket_type = SocketType, 
	       socket = Socket,
	       init_data = InitData},

    MaxHeaderSize = httpd_util:lookup(ConfigDB, max_header_size, 
				      ?HTTP_MAX_HEADER_SIZE),
    TimeOut = httpd_util:lookup(ConfigDB, keep_alive_timeout, 150000),
    NrOfRequest = httpd_util:lookup(ConfigDB, 
				    max_keep_alive_request, infinity),
    
    {_, Status} = httpd_manager:new_connection(Manager),
    
    
    State = #state{mod = Mod, manager = Manager, status = Status,
		   timeout = TimeOut, max_keep_alive_request = NrOfRequest,
		   mfa = {httpd_request, parse, [MaxHeaderSize]}}, 
    
    NewState = activate_request_timeout(State),

    http_transport:setopts(SocketType, Socket, [binary,{packet, 0},
						{active, once}]),
    gen_server:enter_loop(?MODULE, [], NewState).
	    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    {stop, {call_api_violation, Request, From}, State}.

%%--------------------------------------------------------------------
%% handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {reply, {cast_api_violation, Msg}, State}.

%%--------------------------------------------------------------------
%% handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Proto, Socket, Data}, State = 
	    #state{mfa = {Module, Function, Args},
		   mod = #mod{socket_type = SockType, 
			      socket = Socket} = ModData} 
	    = State) when Proto == tcp; Proto == ssl; Proto == dummy ->
  
    case Module:Function([Data | Args]) of
        {ok, Result} ->
	    NewState = cancel_request_timeout(State),
            handle_http_msg(Result, NewState); 
	{error, {header_too_long, MaxHeaderSize}, Version} ->
	    NewModData =  ModData#mod{http_version = Version},
	    httpd_response:send_status(NewModData, 413, "Header too big"),
	    Reason = io_lib:format("Header too big, max size is ~p~n", 
				   [MaxHeaderSize]),
	    error_log(Reason, NewModData),
	    {stop, normal, State#state{response_sent = true, 
				       mod = NewModData}};
	NewMFA ->
	    http_transport:setopts(SockType, Socket, [{active, once}]),
            {noreply, State#state{mfa = NewMFA}}
    end;

%% Error cases
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({ssl_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, _} = Reason, State) ->
    {stop, Reason, State};
handle_info({ssl_error, _, _} = Reason, State) ->
    {stop, Reason, State};

%% Timeouts
handle_info(timeout, #state{mod = ModData, mfa = {_, parse, _}} = State) ->
    error_log("No request received on keep-alive connection" 
	      "before server side timeout", ModData),
    %% No response should be sent!
    {stop, normal, State#state{response_sent = true}}; 
handle_info(timeout, #state{mod = ModData} = State) ->
    httpd_response:send_status(ModData, 408, "Request timeout"),
    error_log("The client did not send the whole request before the"
	      "server side timeout", ModData),
    {stop, normal, State#state{response_sent = true}};

%% Default case
handle_info(Info, #state{mod = ModData} = State) ->
    Error = lists:flatten(
	      io_lib:format("Unexpected message received: ~n~p~n", [Info])),
    error_log(Error, ModData),
    {noreply, State}.

%%--------------------------------------------------------------------
%% terminate(Reason, State) -> void()
%%
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(normal, State) ->
    do_terminate(State);
terminate(Reason, #state{response_sent = false, mod = ModData} = State) ->
    httpd_response:send_status(ModData, 500, none),
    error_log(httpd_util:reason_phrase(500), ModData),
    terminate(Reason, State#state{response_sent = true, mod = ModData});
terminate(_, State) ->
    do_terminate(State).

do_terminate(#state{mod = ModData, manager = Manager} = State) ->
    catch httpd_manager:done_connection(Manager),
    cancel_request_timeout(State),
    httpd_socket:close(ModData#mod.socket_type, ModData#mod.socket).

%%--------------------------------------------------------------------
%% code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
await_socket_ownership_transfer(AcceptTimeout) ->
    receive
	{socket_ownership_transfered, SocketType, Socket} ->
	    {SocketType, Socket}
    after AcceptTimeout ->
	    exit(accept_socket_timeout)
    end.

handle_http_msg({_, _, Version, {_, _}, _}, #state{status = busy,
						   mod = ModData} = State) -> 
    handle_manager_busy(State#state{mod = 
				    ModData#mod{http_version = Version}}),
    {stop, normal, State}; 

handle_http_msg({_, _, Version, {_, _}, _}, 
		#state{status = blocked, mod = ModData} = State) ->
    handle_manager_blocked(State#state{mod = 
				       ModData#mod{http_version = Version}}),
    {stop, normal, State}; 

handle_http_msg({Method, Uri, Version, {RecordHeaders, Headers}, Body},
		#state{status = accept, mod = ModData} = State) ->        
    case httpd_request:validate(Method, Uri, Version) of
	ok  ->
	    {ok, NewModData} = 
		httpd_request:update_mod_data(ModData, Method, Uri,
					      Version, Headers),
      
	    case is_host_specified_if_required(NewModData#mod.absolute_uri,
					       RecordHeaders, Version) of
		true ->
		    handle_body(State#state{headers = RecordHeaders,
					    body = Body,
					    mod = NewModData});
		false ->
		    httpd_response:send_status(ModData#mod{http_version = 
							   Version}, 
					       400, none),
		    {stop, normal, State#state{response_sent = true}}
	    end;
	{error, {not_supported, What}} ->
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       501, {Method, Uri, Version}),
	    Reason = io_lib:format("Not supported: ~p~n", [What]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}};
	{error, {bad_request, {forbidden, URI}}} ->
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       403, URI),
	    Reason = io_lib:format("Forbidden URI: ~p~n", [URI]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}};
	{error,{bad_request, {malformed_syntax, URI}}} ->
	    httpd_response:send_status(ModData#mod{http_version = Version},
				       400, URI),
	    Reason = io_lib:format("Malformed syntax in URI: ~p~n", [URI]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}}
    end;
handle_http_msg({ChunkedHeaders, Body}, 
		State = #state{headers = Headers}) ->
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    handle_response(State#state{headers = NewHeaders, body = Body});
handle_http_msg(Body, State) ->
    handle_response(State#state{body = Body}).

handle_manager_busy(#state{mod = #mod{config_db = ConfigDB}} = State) ->
    MaxClients = httpd_util:lookup(ConfigDB, max_clients, 150),
    Reason = io_lib:format("heavy load (>~w processes)", [MaxClients]),
    reject_connection(State, lists:flatten(Reason)).

handle_manager_blocked(State) ->
    Reason = "Server maintenance performed, try again later",
    reject_connection(State, Reason).

reject_connection(#state{mod = ModData} = State, Reason) ->
    httpd_response:send_status(ModData, 503, Reason),
    {stop, normal, State#state{response_sent = true}}. 

is_host_specified_if_required(nohost, #http_request_h{host = undefined}, 
			      "HTTP/1.1") ->
    false;
is_host_specified_if_required(_, _, _) ->
    true.

handle_body(#state{mod = #mod{config_db = ConfigDB}} = State) ->
    
    MaxHeaderSize =
	httpd_util:lookup(ConfigDB, max_header_size, ?HTTP_MAX_HEADER_SIZE),
    MaxBodySize = httpd_util:lookup(ConfigDB, max_body_size, nolimit),
   
    case handle_expect(State, MaxBodySize) of 
	ok ->
	    handle_body(State, MaxHeaderSize, MaxBodySize);
	Other ->
	    Other
    
    end.
	
handle_body(#state{headers = Headers, body = Body, mod = ModData} = State,
	    MaxHeaderSize, MaxBodySize) ->
    case Headers#http_request_h.'transfer-encoding' of
	"chunked" ->
	    case http_chunk:decode(Body, MaxBodySize, MaxHeaderSize) of
		{Module, Function, Args} ->
		    http_transport:setopts(ModData#mod.socket_type, 
					   ModData#mod.socket, 
					   [{active, once}]),
		    {noreply, State#state{mfa = 
					  {Module, Function, Args}}};
		{ok, {ChunkedHeaders, NewBody}} ->
		    NewHeaders = 
			http_chunk:handle_headers(Headers, ChunkedHeaders),
		    handle_response(State#state{headers = NewHeaders,
						body = NewBody})
	    end;
	Encoding when list(Encoding) ->
	    httpd_response:send_status(ModData, 501, 
				       "Unknown Transfer-Encoding"),
	    Reason = io_lib:format("Unknown Transfer-Encoding: ~p~n", 
				   [Encoding]),
	    error_log(Reason, ModData),
	    {stop, normal, State#state{response_sent = true}};
	_ -> 
	    Length = 
		list_to_integer(Headers#http_request_h.'content-length'),
	    case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
		true ->
		    case httpd_request:whole_body(Body, Length) of 
			{Module, Function, Args} ->
			    http_transport:setopts(ModData#mod.socket_type, 
						   ModData#mod.socket, 
						   [{active, once}]),
			    {noreply, State#state{mfa = 
						  {Module, Function, Args}}};
			
			{ok, NewBody} ->
			    handle_response(
			      State#state{headers = Headers,
					  body = NewBody})
		    end;
		false ->
		    httpd_response:send_status(ModData, 413, "Body too big"),
		    error_log("Body too big", ModData),
		    {stop, normal,  State#state{response_sent = true}}
	    end
    end.

handle_expect(#state{headers = Headers, mod = 
		     #mod{config_db = ConfigDB} = ModData} = State, 
	      MaxBodySize) ->
    Length = Headers#http_request_h.'content-length',
    case expect(Headers, ModData#mod.http_version, ConfigDB) of
	continue when MaxBodySize > Length; MaxBodySize == nolimit ->
	    httpd_response:send_status(ModData, 100, ""),
	    ok;
	continue when MaxBodySize < Length ->
	    httpd_response:send_status(ModData, 413, "Body too big"),
	    error_log("Body too big", ModData),
	    {stop, normal, State#state{response_sent = true}};
	{break, Value} ->
	    httpd_response:send_status(ModData, 417, 
				       "Unexpected expect value"),
	    Reason = io_lib:format("Unexpected expect value: ~p~n", [Value]),
	    error_log(Reason, ModData),
	    {stop, normal,  State#state{response_sent = true}};
	no_expect_header ->
	    ok;
	http_1_0_expect_header ->
	    httpd_response:send_status(ModData, 400, 
				       "Only HTTP/1.1 Clients "
				       "may use the Expect Header"),
	    error_log("Client with lower version than 1.1 tried to send"
		      "an expect header", ModData),
	    {stop, normal, State#state{response_sent = true}}
    end.

expect(Headers, "HTTP/1.1", _) ->
    case Headers#http_request_h.expect of
	"100-continue" ->
	    continue; 
	undefined ->
	    no_expect_header;
	Other ->
	    {break, Other}
    end;
expect(Headers, _, ConfigDB) ->
    case Headers#http_request_h.expect of
	undefined ->
	    no_expect_header;
	_ ->
	    case httpd_util:lookup(ConfigDB, expect, continue) of
		continue->
		    no_expect_header;
		_ ->
		    http_1_0_expect_header
	    end
    end.

handle_response(#state{body = Body, mod = ModData, headers = Headers,
		       max_keep_alive_request = Max} = State) when Max > 0 ->
    {NewBody, Data} = httpd_request:body_data(Headers, Body),
    ok = httpd_response:generate_and_send_response(
	   ModData#mod{entity_body = NewBody}),
    handle_next_request(State#state{response_sent = true}, Data);

handle_response(#state{body = Body, headers = Headers, 
		       mod = ModData} = State) ->
    {NewBody, _} = httpd_request:body_data(Headers, Body),
    ok = httpd_response:generate_and_send_response(
	   ModData#mod{entity_body = NewBody}),
    {stop, normal, State#state{response_sent = true}}.

handle_next_request(#state{mod = #mod{connection = true} = ModData,
			  max_keep_alive_request = Max} = State, Data) ->
    NewModData = #mod{socket_type = ModData#mod.socket_type, 
 		      socket = ModData#mod.socket, 
 		      config_db = ModData#mod.config_db, 
 		      init_data = ModData#mod.init_data},
    MaxHeaderSize =
	httpd_util:lookup(ModData#mod.config_db, 
			  max_header_size, ?HTTP_MAX_HEADER_SIZE),
   
     TmpState = State#state{mod = NewModData,
			   mfa = {httpd_request, parse, [MaxHeaderSize]},
			    max_keep_alive_request = decrease(Max),
			   headers = undefined, body = undefined,
			   response_sent = false},
    
    NewState = activate_request_timeout(TmpState),

    case Data of
	<<>> ->
	    http_transport:setopts(ModData#mod.socket_type,
				   ModData#mod.socket, [{active, once}]),
	    {noreply, NewState};
	_ ->
	    handle_info({dummy, ModData#mod.socket, Data}, NewState)
    end;

handle_next_request(State, _) ->
    {stop, normal, State}.

activate_request_timeout(#state{timeout = Time} = State) ->
    Ref = erlang:send_after(Time, self(), timeout),
    State#state{timer = Ref}.

cancel_request_timeout(#state{timer = undefined} = State) ->
    State;
cancel_request_timeout(#state{timer = Timer} = State) ->
    erlang:cancel_timer(Timer),
    receive 
	timeout ->
	    ok
    after 0 ->
	    ok
    end,
    State#state{timer = undefined}.

decrease(N) when integer(N)->
    N-1;
decrease(N) ->
    N.

error_log(ReasonString, #mod{socket = Socket, socket_type = SocketType,
			     config_db = ConfigDB, 
			     init_data = #init_data{peername = Peername}}) ->
    Error = lists:flatten(
	      io_lib:format("Error reading request: ~s",[ReasonString])),
    error_log(mod_log, SocketType, Socket, ConfigDB, Peername, Error),
    error_log(mod_disk_log, SocketType, Socket, ConfigDB, Peername, Error).

error_log(Mod, SocketType, Socket, ConfigDB, Peername, String) ->
    Modules = httpd_util:lookup(ConfigDB, modules,
				[mod_get, mod_head, mod_log]),
    case lists:member(Mod, Modules) of
	true ->
	    Mod:error_log(SocketType, Socket, ConfigDB, Peername, String);
	_ ->
	    ok
    end.

