% ``The contents of this file are subject to the Erlang Public License,
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

-module(httpc_handler).

-behaviour(gen_server).

-include("httpc_internal.hrl").
-include("http_internal.hrl").

%%--------------------------------------------------------------------
%% Application API
-export([start_link/2, send/2, cancel/2, stream/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(timers, {request_timers = [], % [ref()]
		 pipeline_timer % ref()
	      }).

-record(state, {request,        % #request{}
                session,        % #tcp_session{} 
                status_line,    % {Version, StatusCode, ReasonPharse}
                headers,        % #http_response_h{}
                body,           % binary()
                mfa,            % {Moduel, Function, Args}
                pipeline = queue:new(),% queue() 
		status = new,          % new | pipeline | close | ssl_tunnel
		canceled = [],	       % [RequestId]
                max_header_size = nolimit,   % nolimit | integer() 
                max_body_size = nolimit,     % nolimit | integer()
		options,                     % #options{}
		timers = #timers{}           % #timers{}
               }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid}
%%
%% Description: Starts a http-request handler process. Intended to be
%% called by the httpc manager process. 
%%
%% Note: Uses proc_lib and gen_server:enter_loop so that waiting
%% for gen_tcp:connect to timeout in init/1 will not
%% block the httpc manager process in odd cases such as trying to call
%% a server that does not exist. (See OTP-6735) The only API function
%% sending messages to the handler process that can be called before
%% init has compleated is cancel and that is not a problem! (Send and
%% stream will not be called before the first request has been sent and
%% the reply or part of it has arrived.)
%%--------------------------------------------------------------------
start_link(Request, ProxyOptions) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Request, ProxyOptions]])}.

%%--------------------------------------------------------------------
%% Function: send(Request, Pid) -> ok 
%%	Request = #request{}
%%      Pid = pid() - the pid of the http-request handler process.
%%
%% Description: Uses this handlers session to send a request. Intended
%% to be called by the httpc manager process.
%%--------------------------------------------------------------------
send(Request, Pid) ->
    call(Request, Pid, 5000).

%%--------------------------------------------------------------------
%% Function: cancel(RequestId, Pid) -> ok
%%	RequestId = ref()
%%      Pid = pid() -  the pid of the http-request handler process.
%%
%% Description: Cancels a request. Intended to be called by the httpc
%% manager process.
%%--------------------------------------------------------------------
cancel(RequestId, Pid) ->
    cast({cancel, RequestId}, Pid).

%%--------------------------------------------------------------------
%% Function: stream(BodyPart, Request, Code) -> _
%%	BodyPart = binary()
%%      Request = #request{}
%%      Code = integer()
%%
%% Description: Stream the HTTP body to the caller process (client) 
%%              or to a file. Note that the data that has been stream
%%              does not have to be saved. (We do not want to use up
%%              memory in vain.)
%%--------------------------------------------------------------------
%% Request should not be streamed
stream(BodyPart, Request = #request{stream = none}, _) ->
    {BodyPart, Request};

stream(BodyPart, Request = #request{stream = self}, 200) -> % Stream to caller
    httpc_response:send(Request#request.from, 
			{Request#request.id, stream, BodyPart}),
    {<<>>, Request};

stream(BodyPart, Request = #request{stream = Filename}, 200)
  when is_list(Filename) -> % Stream to file
    case file:open(Filename, [write, raw, append, delayed_write]) of
	{ok, Fd} ->
	    stream(BodyPart, Request#request{stream = Fd}, 200);
	{error, Reason} ->
	    exit({stream_to_file_failed, Reason})
    end;

stream(BodyPart, Request = #request{stream = Fd}, 200) -> % Stream to file
    case file:write(Fd, BodyPart) of
	ok ->
	    {<<>>, Request};
	{error, Reason} ->
	    exit({stream_to_file_failed, Reason})
    end;
stream(BodyPart, Request,_) -> % only 200 responses can be streamed
    {BodyPart, Request}.

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Session]) -> {ok, State} | 
%%                       {ok, State, Timeout} | ignore |{stop, Reason}
%%
%% Description: Initiates the httpc_handler process 
%%
%% Note: The init function may not fail, that will kill the
%% httpc_manager process. We could make the httpc_manager more comlex
%% but we do not want that so errors will be handled by the process
%% sending an init_error message to itself.
%%--------------------------------------------------------------------
init([Request, Options]) ->
    process_flag(trap_exit, true),
    handle_verbose(Options#options.verbose),
    Address = handle_proxy(Request#request.address, Options#options.proxy),
    {ok, State} =
	case {Address /= Request#request.address, Request#request.scheme} of
	    {true, https} ->
		Error = https_through_proxy_is_not_currently_supported,
		self() ! {init_error, 
			  Error, httpc_response:error(Request, Error)},
		{ok, #state{request = Request, options = Options,
			    status = ssl_tunnel}};
	    %% This is what we should do if and when ssl supports 
	    %% "socket upgrading"
	    %%send_ssl_tunnel_request(Address, Request,
	    %%		    #state{options = Options,
	    %%		   status = ssl_tunnel});
	    {_, _} ->
		send_first_request(Address, Request, #state{options = Options})
	end,
    gen_server:enter_loop(?MODULE, [], State).

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _, State = #state{session = Session =
				       #tcp_session{socket = Socket},
				       timers = Timers,
				       options = Options}) ->
    Address = handle_proxy(Request#request.address, Options#options.proxy),

    case httpc_request:send(Address, Request, Socket) of
        ok ->
	    %% Activate the request time out for the new request
	    NewState = activate_request_timeout(State#state{request =
							     Request}),

	    ClientClose = httpc_request:is_client_closing(
			    Request#request.headers),
            case State#state.request of
                #request{} -> %% Old request no yet finished
		    %% Make sure to use the new value of timers in state
		    NewTimers = NewState#state.timers,
                    NewPipeline = queue:in(Request, State#state.pipeline),
		    NewSession = 
			Session#tcp_session{pipeline_length = 
					    %% Queue + current
					    queue:len(NewPipeline) + 1,
					    client_close = ClientClose},
		    httpc_manager:insert_session(NewSession),
                    {reply, ok, State#state{pipeline = NewPipeline,
					    session = NewSession,
					    timers = NewTimers}};
		undefined ->
		    %% Note: tcp-message reciving has already been
		    %% activated by handle_pipeline/2. Also
		    %% the parsing-function #state.mfa is initiated
		    %% by handle_pipeline/2.
		    cancel_timer(Timers#timers.pipeline_timer, 
				 timeout_pipeline),
		    NewSession = 
			Session#tcp_session{pipeline_length = 1,
					    client_close = ClientClose},
		    httpc_manager:insert_session(NewSession),
		    {reply, ok, 
		     NewState#state{request = Request,
				    session = NewSession,
				    timers = 
				    Timers#timers{pipeline_timer =
						  undefined}}}
	    end;
	{error, Reason} ->
	    {reply, {pipline_failed, Reason}, State}
    end.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% When the request in process has been canceld the handler process is
%% stopped and the pipelined requests will be reissued. This is is
%% based on the assumption that it is proably cheaper to reissue the
%% requests than to wait for a potentiall large response that we then
%% only throw away. This of course is not always true maybe we could
%% do something smarter here?! If the request canceled is not
%% the one handled right now the same effect will take place in
%% handle_pipeline/2 when the canceled request is on turn.
handle_cast({cancel, RequestId}, State = #state{request = Request =
						#request{id = RequestId}}) ->
    httpc_manager:request_canceled(RequestId),
    {stop, normal, 
     State#state{canceled = [RequestId | State#state.canceled],
		 request = Request#request{from = answer_sent}}};
handle_cast({cancel, RequestId}, State) ->
    httpc_manager:request_canceled(RequestId),
    {noreply, State#state{canceled = [RequestId | State#state.canceled]}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Proto, _Socket, Data}, State = 
	    #state{mfa = {Module, Function, Args}, 
		   request = #request{method = Method, 
				      stream = Stream} = Request, 
		   session = Session, status_line = StatusLine}) 
  when Proto == tcp; Proto == ssl; Proto == httpc_handler ->

    case Module:Function([Data | Args]) of
        {ok, Result} ->
            handle_http_msg(Result, State); 
        {_, whole_body, _} when Method == head ->
	    handle_response(State#state{body = <<>>}); 
	{Module, whole_body, [Body, Length]} ->
	    {_, Code, _} = StatusLine,
	    {NewBody, NewRequest} = stream(Body, Request, Code),
	    %% When we stream we will not keep the already
	    %% streamed data, that would be a waste of memory.
	    NewLength = case Stream of
			    none ->   
				Length;
			    _ ->
				Length - size(Body)
			end,
	    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
                                   Session#tcp_session.socket, 
				   [{active, once}]),    
            {noreply, State#state{mfa = {Module, whole_body, 
					 [NewBody, NewLength]},
				  request = NewRequest}};
	NewMFA ->
	    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
                                   Session#tcp_session.socket, 
				   [{active, once}]),
            {noreply, State#state{mfa = NewMFA}}
    end;

%% The Server may close the connection to indicate that the
%% whole body is now sent instead of sending an lengh
%% indicator.
handle_info({tcp_closed, _}, State = #state{mfa = {_, whole_body, Args}}) ->
    handle_response(State#state{body = hd(Args)}); 
handle_info({ssl_closed, _}, State = #state{mfa = {_, whole_body, Args}}) ->
    handle_response(State#state{body = hd(Args)}); 

%%% Server closes idle pipeline
handle_info({tcp_closed, _}, State = #state{request = undefined}) ->
    {stop, normal, State};
handle_info({ssl_closed, _}, State = #state{request = undefined}) ->
    {stop, normal, State};

%%% Error cases
handle_info({tcp_closed, _}, State) ->
    {stop, session_remotly_closed, State};
handle_info({ssl_closed, _}, State) ->
    {stop, session_remotly_closed, State};
handle_info({tcp_error, _, _} = Reason, State) ->
    {stop, Reason, State};
handle_info({ssl_error, _, _} = Reason, State) ->
    {stop, Reason, State};

%%% Timeouts
%% Internaly, to a request handling process, a request time out is
%% seen as a canceld request.
handle_info({timeout, RequestId}, State = 
	    #state{request = Request = #request{id = RequestId}}) ->
    httpc_response:send(Request#request.from, 
		       httpc_response:error(Request,timeout)),
    {stop, normal, 
     State#state{canceled = [RequestId | State#state.canceled],
		 request = Request#request{from = answer_sent}}};
handle_info({timeout, RequestId}, State = #state{request = Request}) ->
    httpc_response:send(Request#request.from, 
		       httpc_response:error(Request,timeout)),
    {noreply, State#state{canceled = [RequestId | State#state.canceled]}};

handle_info(timeout_pipeline, State = #state{request = undefined}) ->
    {stop, normal, State};

%% Setting up the connection to the server somehow failed. 
handle_info({init_error, _, ClientErrMsg},
	    State = #state{request = Request}) ->
    NewState = answer_request(Request, ClientErrMsg, State),
    {stop, normal, NewState};

%%% httpc_manager process dies. 
handle_info({'EXIT', _, _}, State = #state{request = undefined}) ->
    {stop, normal, State};
%%Try to finish the current request anyway,
%% there is a fairly high probability that it can be done successfully.
%% Then close the connection, hopefully a new manager is started that
%% can retry requests in the pipeline.
handle_info({'EXIT', _, _}, State) ->
    {noreply, State#state{status = close}}.
    
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> _  (ignored by gen_server)
%% Description: Shutdown the httpc_handler
%%--------------------------------------------------------------------
terminate(normal, #state{session = undefined}) ->
    ok;  %% Init error there is no socket to be closed.
terminate(normal, #state{request = Request, 
		    session = #tcp_session{id = undefined,
					   socket = Socket}}) ->  
    %% Init error sending, no session information has been setup but
    %% there is a socket that needs closing.
    http_transport:close(socket_type(Request), Socket);

terminate(_, State = #state{session = Session, request = undefined,
			   timers = Timers}) -> 
    catch httpc_manager:delete_session(Session#tcp_session.id),
    
    case queue:is_empty(State#state.pipeline) of 
	false ->
	    retry_pipline(queue:to_list(State#state.pipeline), State);
	true ->
	    ok
    end,
    cancel_timer(Timers#timers.pipeline_timer, timeout_pipeline),
    http_transport:close(socket_type(Session#tcp_session.scheme),
			 Session#tcp_session.socket);

terminate(Reason, State = #state{request = Request})-> 
    NewState = case Request#request.from of
		   answer_sent ->
		       State;
		   _ ->
		       answer_request(Request, 
				      httpc_response:error(Request, Reason), 
				      State)
	       end,
    terminate(Reason, NewState#state{request = undefined}).


%%--------------------------------------------------------------------
%% Func: code_change(_OldVsn, State, Extra) -> {ok, NewState}
%% Purpose: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_first_request(Address, Request, State) ->
    Ipv6 = (State#state.options)#options.ipv6,
    SocketType = socket_type(Request),
    TimeOut = (Request#request.settings)#http_options.timeout,
    case http_transport:connect(SocketType, Address, Ipv6, TimeOut) of
	{ok, Socket} ->
	    case httpc_request:send(Address, Request, Socket) of
		ok ->
		    ClientClose = 
			httpc_request:is_client_closing(
			  Request#request.headers),
		    Session =
			#tcp_session{id = {Request#request.address, self()},
				     scheme = Request#request.scheme,
				     socket = Socket,
				     client_close = ClientClose},
		    TmpState = State#state{request = Request, 
					   session = Session, 
					   mfa = 
					   {httpc_response, parse,
					    [State#state.max_header_size]},
					   status_line = undefined,
					   headers = undefined,
					   body = undefined,
					   status = new},
		    http_transport:setopts(SocketType, 
					   Socket, [{active, once}]),
		    NewState = activate_request_timeout(TmpState),
		    {ok, NewState};
		{error, Reason} -> 
		    %% Commented out in wait of ssl support to avoid
		    %% dialyzer warning
		    %%case State#state.status of
		    %%	new -> % Called from init/1
		    self() ! {init_error, error_sending, 
			      httpc_response:error(Request, Reason)},
		    {ok, State#state{request = Request,
				     session = 
				     #tcp_session{socket = Socket}}}
		    %%ssl_tunnel -> % Not called from init/1
		    %%  NewState = 
		    %%	answer_request(Request, 
		    %%httpc_response:error(Request, 
		    %%Reason),
		    %%			       State),
		    %%	    {stop, normal, NewState}
		    %%    end
	    end;
	{error, Reason} -> 
	    %% Commented out in wait of ssl support to avoid
	    %% dialyzer warning
	    %% case State#state.status of
	    %%	new -> % Called from init/1
	    self() ! {init_error, error_connecting, 
		      httpc_response:error(Request, Reason)},
	    {ok, State#state{request = Request}}
	    %%	ssl_tunnel -> % Not called from init/1
	    %%    NewState = 
	    %%	answer_request(Request, 
	    %%		       httpc_response:error(Request, 
	    %%					    Reason),
	    %%		       State),
	    %%    {stop, normal, NewState}
	    %%end
    end.

handle_http_msg({Version, StatusCode, ReasonPharse, Headers, Body}, 
		State = #state{request = Request}) ->
    
    case Headers#http_response_h.'content-type' of
        "multipart/byteranges" ++ _Param ->
            exit(not_yet_implemented);
        _ ->
	    start_stream({{Version, StatusCode, ReasonPharse}, Headers}, 
			 Request),
            handle_http_body(Body, 
			     State#state{status_line = {Version, 
							StatusCode,
							ReasonPharse},
					 headers = Headers})
    end;
handle_http_msg({ChunkedHeaders, Body}, 
		State = #state{headers = Headers}) ->
    NewHeaders = http_chunk:handle_headers(Headers, ChunkedHeaders),
    handle_response(State#state{headers = NewHeaders, body = Body});
handle_http_msg(Body, State = #state{status_line = {_,Code, _}}) ->
    {NewBody, NewRequest}= stream(Body, State#state.request, Code),
    handle_response(State#state{body = NewBody, request = NewRequest}).

handle_http_body(<<>>, State = #state{request = #request{method = head}}) ->
    handle_response(State#state{body = <<>>});

handle_http_body(Body, State = #state{headers = Headers, session = Session,
				      max_body_size = MaxBodySize,
				      status_line = {_,Code, _},
				      request = Request}) ->
    case Headers#http_response_h.'transfer-encoding' of
        "chunked" ->
	    case http_chunk:decode(Body, State#state.max_body_size, 
				   State#state.max_header_size, 
				   {Code, Request}) of
		{Module, Function, Args} ->
		    http_transport:setopts(socket_type(
					     Session#tcp_session.scheme), 
					   Session#tcp_session.socket, 
					   [{active, once}]),
		    {noreply, State#state{mfa = 
					  {Module, Function, Args}}};
		{ok, {ChunkedHeaders, NewBody}} ->
		    NewHeaders = http_chunk:handle_headers(Headers, 
							   ChunkedHeaders),
		    handle_response(State#state{headers = NewHeaders, 
						body = NewBody})
	    end;
        Encoding when list(Encoding) ->
	    NewState = answer_request(Request, 
				      httpc_response:error(Request, 
							  unknown_encoding),
				     State),
	    {stop, normal, NewState};
        _ ->
            Length =
                list_to_integer(Headers#http_response_h.'content-length'),
            case ((Length =< MaxBodySize) or (MaxBodySize == nolimit)) of
                true ->
                    case httpc_response:whole_body(Body, Length) of
                        {ok, Body} ->
			    {NewBody, NewRequest}= stream(Body, Request, Code),
			    handle_response(State#state{body = NewBody,
							request = NewRequest});
                        MFA ->
                            http_transport:setopts(
			      socket_type(Session#tcp_session.scheme), 
			      Session#tcp_session.socket, 
			      [{active, once}]),
			    {noreply, State#state{mfa = MFA}}
		    end;
                false ->
		    NewState = 
			answer_request(Request,
				       httpc_response:error(Request, 
							   body_too_big),
				       State),
                    {stop, normal, NewState}
            end
    end.

%%% Normaly I do not comment out code, I throw it away. But this might
%%% actually be used on day if ssl is improved.
%% handle_response(State = #state{status = ssl_tunnel,
%% 			       request = Request,
%% 			       options = Options,
%% 			       session = #tcp_session{socket = Socket,
%% 						      scheme = Scheme},
%% 			       status_line = {_, 200, _}}) ->
%%     %%% Insert code for upgrading the socket if and when ssl supports this.  
%%     Address = handle_proxy(Request#request.address, Options#options.proxy),   
%%     send_first_request(Address, Request, State);
%% handle_response(State = #state{status = ssl_tunnel,
%% 			      request = Request}) ->
%%     NewState = answer_request(Request,
%% 			      httpc_response:error(Request,
%% 						   ssl_proxy_tunnel_failed),
%% 			      State),
%%                     {stop, normal, NewState};

handle_response(State = #state{status = new}) ->
   handle_response(try_to_enable_pipline(State));

handle_response(State = #state{request = Request,
			       status = Status,
			       session = Session, 
			       status_line = StatusLine,
			       headers = Headers, 
			       body = Body,
			       options = Options}) when Status =/= new ->

    handle_cookies(Headers, Request, Options),
    case httpc_response:result({StatusLine, Headers, Body}, Request) of
	%% 100-continue
	continue -> 
	    %% Send request body
	    {_, RequestBody} = Request#request.content,
	    http_transport:send(socket_type(Session#tcp_session.scheme), 
					    Session#tcp_session.socket, 
				RequestBody),
	    %% Wait for next response
	    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
				   Session#tcp_session.socket, 
				   [{active, once}]),
	    {noreply, 
	     State#state{mfa = {httpc_response, parse,
				[State#state.max_header_size]},
			 status_line = undefined,
			 headers = undefined,
			 body = undefined
			}};
	%% Ignore unexpected 100-continue response and receive the
	%% actual response that the server will send right away. 
	{ignore, Data} -> 
	    NewState = State#state{mfa = 
				   {httpc_response, parse,
				    [State#state.max_header_size]},
				   status_line = undefined,
				   headers = undefined,
				   body = undefined},
	    handle_info({httpc_handler, dummy, Data}, NewState);
	%% On a redirect or retry the current request becomes 
	%% obsolete and the manager will create a new request 
	%% with the same id as the current.
	{redirect, NewRequest, Data}->
	    ok = httpc_manager:redirect_request(NewRequest),
	    handle_pipeline(State#state{request = undefined}, Data);
	{retry, TimeNewRequest, Data}->
	    ok = httpc_manager:retry_request(TimeNewRequest),
	    handle_pipeline(State#state{request = undefined}, Data);
	{ok, Msg, Data} ->
	    end_stream(StatusLine, Request),
	    NewState = answer_request(Request, Msg, State),
	    handle_pipeline(NewState, Data); 
	{stop, Msg} ->
	    end_stream(StatusLine, Request),
	    NewState = answer_request(Request, Msg, State),
	    {stop, normal, NewState}
    end.

handle_cookies(_,_, #options{cookies = disabled}) ->
    ok;
%% User wants to verify the cookies before they are stored,
%% so the user will have to call a store command.
handle_cookies(_,_, #options{cookies = verify}) ->
    ok;
handle_cookies(Headers, Request, #options{cookies = enabled}) ->
    {Host, _ } = Request#request.address,
    Cookies = http_cookie:cookies(Headers#http_response_h.other, 
				  Request#request.path, Host),
    httpc_manager:store_cookies(Cookies, Request#request.address).

%% This request could not be pipelined
handle_pipeline(State = #state{status = close}, _) ->
    {stop, normal, State};

handle_pipeline(State = #state{status = pipeline, session = Session}, 
		Data) ->
    case queue:out(State#state.pipeline) of
	{empty, _} ->
	    %% The server may choose too teminate an idle pipeline
	    %% in this case we want to receive the close message
	    %% at once and not when trying to pipline the next
	    %% request.
	    http_transport:setopts(socket_type(Session#tcp_session.scheme), 
				   Session#tcp_session.socket, 
				   [{active, once}]),
	    %% If a pipeline that has been idle for some time is not
	    %% closed by the server, the client may want to close it.
	    NewState = activate_pipeline_timeout(State),
	    NewSession = Session#tcp_session{pipeline_length = 0},
	    httpc_manager:insert_session(NewSession),
	    {noreply, 
	     NewState#state{request = undefined, 
			    mfa = {httpc_response, parse,
				   [NewState#state.max_header_size]},
			    status_line = undefined,
			    headers = undefined,
			    body = undefined
			   }
	    };
	{{value, NextRequest}, Pipeline} ->    
	    case lists:member(NextRequest#request.id, 
			      State#state.canceled) of		
		true ->
		    %% See comment for handle_cast({cancel, RequestId})
		    {stop, normal, 
		     State#state{request = 
				 NextRequest#request{from = answer_sent}}};
		false ->
		    NewSession = 
			Session#tcp_session{pipeline_length =
					    %% Queue + current
					    queue:len(Pipeline) + 1},
		    httpc_manager:insert_session(NewSession),
		    NewState = 
			State#state{pipeline = Pipeline,
				    request = NextRequest,
				    mfa = {httpc_response, parse,
					   [State#state.max_header_size]},
				    status_line = undefined,
				    headers = undefined,
				    body = undefined},
		    case Data of
			<<>> ->
			    http_transport:setopts(
			      socket_type(Session#tcp_session.scheme), 
			      Session#tcp_session.socket, 
			      [{active, once}]),
			    {noreply, NewState};
			_ ->
			    %% If we already received some bytes of
			    %% the next response
			    handle_info({httpc_handler, dummy, Data}, 
					NewState) 
		    end
	    end
    end.

call(Msg, Pid, Timeout) ->
    gen_server:call(Pid, Msg, Timeout).

cast(Msg, Pid) ->
    gen_server:cast(Pid, Msg).

activate_request_timeout(State = #state{request = Request}) ->
    Time = (Request#request.settings)#http_options.timeout,
    case Time of
	infinity ->
	    State;
	_ ->
	    Ref = erlang:send_after(Time, self(), 
				    {timeout, Request#request.id}),
	    State#state
	      {timers = 
	       #timers{request_timers = 
		       [{Request#request.id, Ref}|
			(State#state.timers)#timers.request_timers]}}
    end.

activate_pipeline_timeout(State = #state{options = 
					 #options{pipeline_timeout = 
						  infinity}}) ->
    State;
activate_pipeline_timeout(State = #state{options = 
					 #options{pipeline_timeout = Time}}) ->
    Ref = erlang:send_after(Time, self(), timeout_pipeline),
    State#state{timers = #timers{pipeline_timer = Ref}}.

is_pipeline_capable_server("HTTP/1." ++ N, _) when hd(N) >= $1 ->
    true;
is_pipeline_capable_server("HTTP/1.0", 
			   #http_response_h{connection = "keep-alive"}) ->
    true;
is_pipeline_capable_server(_,_) ->
    false.

is_keep_alive_connection(Headers, Session) ->
    (not ((Session#tcp_session.client_close) or  
	  httpc_response:is_server_closing(Headers))).

try_to_enable_pipline(State = #state{session = Session, 
				     request = #request{method = Method},
				     status_line = {Version, _, _},
				     headers = Headers}) ->
    case (is_pipeline_capable_server(Version, Headers)) and  
	(is_keep_alive_connection(Headers, Session)) and 
	(httpc_request:is_idempotent(Method)) of
	true ->
	    httpc_manager:insert_session(Session),
	    State#state{status = pipeline};
	false ->
	    State#state{status = close}
    end.

answer_request(Request, Msg, State = #state{timers = Timers}) ->    
    httpc_response:send(Request#request.from, Msg),
    RequestTimers = Timers#timers.request_timers,
    TimerRef =
	http_util:key1search(RequestTimers, Request#request.id, undefined),
    Timer = {Request#request.id, TimerRef},
    cancel_timer(TimerRef, {timeout, Request#request.id}),
    State#state{request = Request#request{from = answer_sent},
		timers = 
		Timers#timers{request_timers =
			      lists:delete(Timer, RequestTimers)}}.
cancel_timer(undefined, _) ->
    ok;
cancel_timer(Timer, TimeoutMsg) ->
    erlang:cancel_timer(Timer),
    receive 
	TimeoutMsg ->
	    ok
    after 0 ->
	    ok
    end.

retry_pipline([], _) ->
    ok;

retry_pipline([Request |PipeLine],  State = #state{timers = Timers}) ->
    NewState =
	case (catch httpc_manager:retry_request(Request)) of
	    ok ->
		RequestTimers = Timers#timers.request_timers,
		Timer = {_, TimerRef} =
		    http_util:key1search(RequestTimers, Request#request.id, 
					  {undefined, undefined}),
		cancel_timer(TimerRef, {timeout, Request#request.id}),
		State#state{timers = Timers#timers{request_timers =
					  lists:delete(Timer,
						       RequestTimers)}};
	    Error ->
		answer_request(Request#request.from,
			       httpc_response:error(Request, Error), State) 
	end,
    retry_pipline(PipeLine, NewState).

%%% Check to see if the given {Host,Port} tuple is in the NoProxyList
%%% Returns an eventually updated {Host,Port} tuple, with the proxy address
handle_proxy(HostPort = {Host, _Port}, {Proxy, NoProxy}) ->
    case Proxy of
	undefined ->
	    HostPort;
	Proxy ->
	    case is_no_proxy_dest(Host, NoProxy) of
		true ->
		    HostPort;
		false ->
		    Proxy
	    end
    end.

is_no_proxy_dest(_, []) ->
    false;
is_no_proxy_dest(Host, [ "*." ++ NoProxyDomain | NoProxyDests]) ->    
    
    case is_no_proxy_dest_domain(Host, NoProxyDomain) of
	true ->
	    true;
	false ->
	    is_no_proxy_dest(Host, NoProxyDests)
    end;

is_no_proxy_dest(Host, [NoProxyDest | NoProxyDests]) ->
    IsNoProxyDest = case http_util:is_hostname(NoProxyDest) of
			true ->
			    fun is_no_proxy_host_name/2;
			false ->
			    fun is_no_proxy_dest_address/2
		    end,
    
    case IsNoProxyDest(Host, NoProxyDest) of
	true ->
	    true;
	false ->
	    is_no_proxy_dest(Host, NoProxyDests)
    end.

is_no_proxy_host_name(Host, Host) ->
    true;
is_no_proxy_host_name(_,_) ->
    false.

is_no_proxy_dest_domain(Dest, DomainPart) ->
    lists:suffix(DomainPart, Dest).

is_no_proxy_dest_address(Dest, Dest) ->
    true;
is_no_proxy_dest_address(Dest, AddressPart) ->
    lists:prefix(AddressPart, Dest).

socket_type(#request{scheme = http}) ->
    ip_comm;
socket_type(#request{scheme = https, settings = Settings}) ->
    {ssl, Settings#http_options.ssl};
socket_type(http) ->
    ip_comm;
socket_type(https) ->
    {ssl, []}. %% Dummy value ok for ex setops that does not use this value

start_stream(_, #request{stream = none}) ->
    ok;
start_stream({{_, 200, _}, Headers}, Request = #request{stream = self}) ->
    Msg = httpc_response:stream_start(Headers, Request),
    httpc_response:send(Request#request.from, Msg);
start_stream(_, _) ->
    ok.

%% Note the end stream message is handled by httpc_response and will
%% be sent by answer_request
end_stream(_, #request{stream = none}) ->
    ok;
end_stream(_, #request{stream = self}) ->
    ok;
end_stream({_,200,_}, #request{stream = Fd}) ->
    case file:close(Fd) of 
	ok ->
	    ok;
	{error, enospc} -> % Could be due to delayed_write
	    file:close(Fd)
    end;
end_stream(_, _) ->
    ok.

handle_verbose(verbose) ->
    dbg:p(self(), [r]);
handle_verbose(debug) ->
    dbg:p(self(), [call]),
    dbg:tp(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(trace) ->
    dbg:p(self(), [call]),
    dbg:tpl(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(_) ->
    ok.    

%%% Normaly I do not comment out code, I throw it away. But this might
%%% actually be used on day if ssl is improved.
%% send_ssl_tunnel_request(Address, Request = #request{address = {Host, Port}}, 
%% 			State) ->
%%     %% A ssl tunnel request is a special http request that looks like
%%     %% CONNECT host:port HTTP/1.1
%%     SslTunnelRequest = #request{method = connect, scheme = http,
%% 				headers = 
%% 				#http_request_h{
%% 				  host = Host, 
%% 				  address = Address, 
%% 				  path = Host ++ ":",
%% 				  pquery = integer_to_list(Port),
%% 				  other = [{ "Proxy-Connection", "keep-alive"}]},
%%     Ipv6 = (State#state.options)#options.ipv6,
%%     SocketType = socket_type(SslTunnelRequest),
%%     case http_transport:connect(SocketType, 
%%                                 SslTunnelRequest#request.address, Ipv6) of
%% 	{ok, Socket} ->
%% 	    case httpc_request:send(Address, SslTunnelRequest, Socket) of
%% 		ok ->
%% 		    Session = #tcp_session{id = 
%% 					   {SslTunnelRequest#request.address,
%% 					    self()},
%% 					   scheme = 
%% 					   SslTunnelRequest#request.scheme,
%% 					   socket = Socket},
%% 		    NewState = State#state{mfa = 
%% 					   {httpc_response, parse,
%% 					    [State#state.max_header_size]},
%% 					   request = Request,
%% 					   session = Session},
%% 		    http_transport:setopts(socket_type(
%%                                           SslTunnelRequest#request.scheme), 
%% 					   Socket, 
%% 					   [{active, once}]),
%% 		    {ok, NewState};
%% 		{error, Reason} -> 
%% 		    self() ! {init_error, error_sending, 
%% 			      httpc_response:error(Request, Reason)},
%% 		    {ok, State#state{request = Request,
%% 				     session = #tcp_session{socket = 
%% 							    Socket}}}
%% 	    end;
%% 	{error, Reason} ->
%% 	    self() ! {init_error, error_connecting, 
%% 		      httpc_response:error(Request, Reason)},
%% 	    {ok, State#state{request = Request}}
%%     end.
