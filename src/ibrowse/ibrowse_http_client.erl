%%%-------------------------------------------------------------------
%%% File    : ibrowse_http_client.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : The name says it all
%%%
%%% Created : 11 Oct 2003 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%%-------------------------------------------------------------------
-module(ibrowse_http_client).
-vsn('$Id: ibrowse_http_client.erl,v 1.18 2008/05/21 15:28:11 chandrusf Exp $ ').

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([
	 start_link/1,
	 start/1,
	 stop/1,
	 send_req/7
	]).

-ifdef(debug).
-compile(export_all).
-endif.

%% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-include("ibrowse.hrl").

-record(state, {host, port, 
		use_proxy = false, proxy_auth_digest,
		ssl_options = [], is_ssl = false, socket, 
		reqs=queue:new(), cur_req, status=idle, http_status_code, 
		reply_buffer=[], rep_buf_size=0, recvd_headers=[],
		is_closing, send_timer, content_length,
		deleted_crlf = false, transfer_encoding, chunk_size, 
		chunks=[], lb_ets_tid, cur_pipeline_size = 0}).

-record(request, {url, method, options, from,
		  stream_to, req_id,
		  save_response_to_file = false,
		  tmp_file_name, tmp_file_fd}).

-import(ibrowse_lib, [
		      parse_url/1,
		      printable_date/0,
		      get_value/2,
		      get_value/3,
		      do_trace/2
		     ]).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Args) ->
    gen_server:start(?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Conn_pid) ->
    gen_server:call(Conn_pid, stop).

send_req(Conn_Pid, Url, Headers, Method, Body, Options, Timeout) ->
    Timeout_1 = case Timeout of
		    infinity ->
			infinity;
		    _ when is_integer(Timeout) ->
			Timeout + 100
		end,
    gen_server:call(
      Conn_Pid,
      {send_req, {Url, Headers, Method, Body, Options, Timeout}}, Timeout_1).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init({Lb_Tid, #url{host = Host, port = Port}, {SSLOptions, Is_ssl}}) ->
    State = #state{host = Host,
		   port = Port,
		   ssl_options = SSLOptions,
		   is_ssl = Is_ssl,
		   lb_ets_tid = Lb_Tid},
    put(ibrowse_trace_token, [Host, $:, integer_to_list(Port)]),
    put(my_trace_flag, ibrowse_lib:get_trace_status(Host, Port)),
    {ok, State};
init({Host, Port}) ->
    State = #state{host = Host,
		   port = Port},
    put(ibrowse_trace_token, [Host, $:, integer_to_list(Port)]),
    put(my_trace_flag, ibrowse_lib:get_trace_status(Host, Port)),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
%% Received a request when the remote server has already sent us a
%% Connection: Close header
handle_call({send_req, _}, 
	    _From,
	    #state{is_closing=true}=State) ->
    {reply, {error, connection_closing}, State};

handle_call({send_req, {Url, Headers, Method, Body, Options, Timeout}}, 
	    From,
	    #state{socket=undefined,
		   host=Host, port=Port}=State) ->
    {Host_1, Port_1, State_1} =
	case get_value(proxy_host, Options, false) of
	    false ->
		{Host, Port, State};
	    PHost ->
		ProxyUser = get_value(proxy_user, Options, []),
		ProxyPassword = get_value(proxy_password, Options, []),
		Digest = http_auth_digest(ProxyUser, ProxyPassword),
		{PHost, get_value(proxy_port, Options, 80),
		 State#state{use_proxy = true,
			     proxy_auth_digest = Digest}}
	end,
    StreamTo = get_value(stream_to, Options, undefined),
    ReqId = make_req_id(),
    SaveResponseToFile = get_value(save_response_to_file, Options, false),
    NewReq = #request{url=Url, 
		      method=Method,
		      stream_to=StreamTo,
		      options=Options, 
		      req_id=ReqId,
		      save_response_to_file = SaveResponseToFile,
		      from=From},
    Reqs = queue:in(NewReq, State#state.reqs),
    State_2 = check_ssl_options(Options, State_1#state{reqs = Reqs}),
    do_trace("Connecting...~n", []),
    Timeout_1 = case Timeout of
		    infinity ->
			infinity;
		    _ ->
			round(Timeout*0.9)
		end,
    case do_connect(Host_1, Port_1, Options, State_2, Timeout_1) of
	{ok, Sock} ->
	    Ref = case Timeout of
		      infinity ->
			  undefined;
		      _ ->
			  erlang:send_after(Timeout, self(), {req_timedout, From})
		  end,
	    do_trace("Connected!~n", []),
	    case send_req_1(Url, Headers, Method, Body, Options, Sock, State_2) of
		ok ->
		    case StreamTo of
			undefined ->
			    ok;
			_ ->
			    gen_server:reply(From, {ibrowse_req_id, ReqId})
		    end,
		    State_3 = inc_pipeline_counter(State_2#state{socket = Sock,
								 send_timer = Ref,
								 cur_req = NewReq,
								 status = get_header}),
		    {noreply, State_3};
		Err ->
		    shutting_down(State_2),
		    do_trace("Send failed... Reason: ~p~n", [Err]),
		    gen_server:reply(From, {error, send_failed}),
		    {stop, normal, State_2}
	    end;
	Err ->
	    shutting_down(State_2),
	    do_trace("Error connecting. Reason: ~1000.p~n", [Err]),
	    gen_server:reply(From, {error, conn_failed}),
	    {stop, normal, State_2}
    end;

%% Request which is to be pipelined
handle_call({send_req, {Url, Headers, Method,
			 Body, Options, Timeout}},
	    From,
	    #state{socket=Sock, status=Status, reqs=Reqs}=State) ->
    do_trace("Recvd request in connected state. Status -> ~p NumPending: ~p~n", [Status, length(queue:to_list(Reqs))]),
    StreamTo = get_value(stream_to, Options, undefined),
    SaveResponseToFile = get_value(save_response_to_file, Options, false),
    ReqId = make_req_id(),
    NewReq = #request{url=Url, 
		      stream_to=StreamTo,
		      method=Method,
		      options=Options, 
		      req_id=ReqId,
		      save_response_to_file = SaveResponseToFile,
		      from=From},
    State_1 = State#state{reqs=queue:in(NewReq, State#state.reqs)},
    case send_req_1(Url, Headers, Method, Body, Options, Sock, State_1) of
	ok ->
	    State_2 = inc_pipeline_counter(State_1),
	    do_setopts(Sock, [{active, true}], State#state.is_ssl),
	    case Timeout of
		infinity ->
		    ok;
		_ ->
		    erlang:send_after(Timeout, self(), {req_timedout, From})
	    end,
	    State_3 = case Status of
			  idle ->
			      State_2#state{status = get_header,
					    cur_req = NewReq};
			  _ ->
			      State_2
		      end,
	    case StreamTo of
		undefined ->
		    ok;
		_ ->
		    gen_server:reply(From, {ibrowse_req_id, ReqId})
	    end,
	    {noreply, State_3};
	Err ->
	    shutting_down(State_1),
	    do_trace("Send request failed: Reason: ~p~n", [Err]),
	    gen_server:reply(From, {error, send_failed}),
	    do_error_reply(State, send_failed),
	    {stop, normal, State_1}
    end;

handle_call(stop, _From, #state{socket = Socket, is_ssl = Is_ssl} = State) ->
    do_close(Socket, Is_ssl),
    do_error_reply(State, closing_on_request),
    {stop, normal, State};

handle_call(Request, _From, State) ->
    Reply = {unknown_request, Request},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({tcp, _Sock, Data}, State) ->
    handle_sock_data(Data, State);
handle_info({ssl, _Sock, Data}, State) ->
    handle_sock_data(Data, State);

handle_info({tcp_closed, _Sock}, State) ->
    do_trace("TCP connection closed by peer!~n", []),
    handle_sock_closed(State),
    {stop, normal, State};
handle_info({ssl_closed, _Sock}, State) ->
    do_trace("SSL connection closed by peer!~n", []),
    handle_sock_closed(State),
    {stop, normal, State};

handle_info({req_timedout, From}, State) ->
    case lists:keysearch(From, #request.from, queue:to_list(State#state.reqs)) of
       false ->
          {noreply, State};
       {value, _} ->
          shutting_down(State),
          do_error_reply(State, req_timedout),
          {stop, normal, State}
    end;

handle_info({trace, Bool}, State) ->
    put(my_trace_flag, Bool),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Recvd unknown message ~p when in state: ~p~n", [Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    case State#state.socket of
	undefined ->
	    ok;
	Sock ->
	    do_close(Sock, State#state.is_ssl)
    end.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Handles data recvd on the socket
%%--------------------------------------------------------------------
handle_sock_data(Data, #state{status=idle}=State) ->
    do_trace("Data recvd on socket in state idle!. ~1000.p~n", [Data]),
    shutting_down(State),
    do_error_reply(State, data_in_status_idle),
    do_close(State#state.socket, State#state.is_ssl),
    {stop, normal, State};

handle_sock_data(Data, #state{status=get_header, socket=Sock}=State) ->
    case parse_response(Data, State) of
	{error, _Reason} ->
	    shutting_down(State),
	    {stop, normal, State};
	stop ->
	    shutting_down(State),
	    {stop, normal, State};
	State_1 ->
	    do_setopts(Sock, [{active, true}], State#state.is_ssl),
	    {noreply, State_1}
    end;

handle_sock_data(Data, #state{status=get_body, content_length=CL,
			      http_status_code = StatCode,
			      recvd_headers=Headers, 
			      chunk_size=CSz, socket=Sock}=State) ->
    case (CL == undefined) and (CSz == undefined) of
	true ->
	    case accumulate_response(Data, State) of
		{error, Reason} ->
		    shutting_down(State),
		    fail_pipelined_requests(State, 
					    {error, {Reason, {stat_code, StatCode}, Headers}}),
		    {stop, normal, State};
		State_1 ->
		    do_setopts(Sock, [{active, true}], State#state.is_ssl),
		    {noreply, State_1}
	    end;
	_ ->
	    case parse_11_response(Data, State) of
		{error, Reason} ->
		    shutting_down(State),
		    fail_pipelined_requests(State, 
					    {error, {Reason, {stat_code, StatCode}, Headers}}),
		    {stop, normal, State};
		stop ->
		    shutting_down(State),
		    {stop, normal, State};
		State_1 ->
		    do_setopts(Sock, [{active, true}], State#state.is_ssl),
		    {noreply, State_1}
	    end
    end.

accumulate_response(Data,
		    #state{
		      cur_req = #request{save_response_to_file = SaveResponseToFile,
					 tmp_file_fd = undefined} = CurReq,
		      http_status_code=[$2 | _]}=State) when SaveResponseToFile /= false ->
    TmpFilename = case SaveResponseToFile of
		      true -> make_tmp_filename();
		      F -> F
		  end,
    case file:open(TmpFilename, [write, delayed_write, raw]) of
	{ok, Fd} ->
	    accumulate_response(Data, State#state{
					cur_req = CurReq#request{
						    tmp_file_fd = Fd,
						    tmp_file_name = TmpFilename}});
	{error, Reason} ->
	    {error, {file_open_error, Reason}}
    end;
accumulate_response(Data, #state{cur_req = #request{save_response_to_file = SaveResponseToFile,
						    tmp_file_fd = Fd},
				 transfer_encoding=chunked,
				 chunks = Chunks,
				 http_status_code=[$2 | _]
				} = State) when SaveResponseToFile /= false ->
    case file:write(Fd, [Chunks | Data]) of
	ok ->
	    State#state{chunks = []};
	{error, Reason} ->
	    {error, {file_write_error, Reason}}
    end;
accumulate_response(Data, #state{cur_req = #request{save_response_to_file = SaveResponseToFile,
						    tmp_file_fd = Fd},
				 reply_buffer = RepBuf,
				 http_status_code=[$2 | _]
				} = State) when SaveResponseToFile /= false ->
    case file:write(Fd, [RepBuf | Data]) of
	ok ->
	    State#state{reply_buffer = []};
	{error, Reason} ->
	    {error, {file_write_error, Reason}}
    end;
accumulate_response([], State) ->
    State;
accumulate_response(Data, #state{reply_buffer = RepBuf,
				 cur_req = CurReq}=State) ->
    #request{stream_to=StreamTo, req_id=ReqId} = CurReq,
    case StreamTo of
	undefined ->
	    State#state{reply_buffer = [Data | RepBuf]};
	_ ->
	    do_interim_reply(StreamTo, ReqId, Data),
	    State
    end.

make_tmp_filename() ->
    DownloadDir = ibrowse:get_config_value(download_dir, filename:absname("./")),
    {A,B,C} = now(),
    filename:join([DownloadDir,
		   "ibrowse_tmp_file_"++
		   integer_to_list(A) ++
		   integer_to_list(B) ++
		   integer_to_list(C)]).


%%--------------------------------------------------------------------
%% Handles the case when the server closes the socket
%%--------------------------------------------------------------------
handle_sock_closed(#state{status=get_header}=State) ->
    shutting_down(State),
    do_error_reply(State, connection_closed);

handle_sock_closed(#state{cur_req=undefined} = State) ->
    shutting_down(State);

%% We check for IsClosing because this the server could have sent a 
%% Connection-Close header and has closed the socket to indicate end
%% of response. There maybe requests pipelined which need a response.
handle_sock_closed(#state{reply_buffer=Buf, reqs=Reqs, http_status_code=SC,
			  is_closing=IsClosing,
			  cur_req=#request{tmp_file_name=TmpFilename,
					   tmp_file_fd=Fd} = CurReq,
			  status=get_body, recvd_headers=Headers}=State) ->
    #request{from=From, stream_to=StreamTo, req_id=ReqId} = CurReq,
    case IsClosing of
	true ->
	    {_, Reqs_1} = queue:out(Reqs),
	    case TmpFilename of
		undefined ->
		    do_reply(State, From, StreamTo, ReqId,
			     {ok, SC, Headers,
			      lists:flatten(lists:reverse(Buf))});
		_ ->
		    file:close(Fd),
		    do_reply(State, From, StreamTo, ReqId,
			     {ok, SC, Headers, {file, TmpFilename}})
	    end,
	    do_error_reply(State#state{reqs = Reqs_1}, connection_closed),
	    State;
	_ ->
	    do_error_reply(State, connection_closed),
	    State
    end.

do_connect(Host, Port, _Options, #state{is_ssl=true, ssl_options=SSLOptions}, Timeout) ->
    ssl:connect(Host, Port, [{nodelay, true}, {active, false} | SSLOptions], Timeout);
do_connect(Host, Port, _Options, _State, Timeout) ->
    gen_tcp:connect(Host, Port, [{nodelay, true}, {active, false}], Timeout).

do_send(Sock, Req, true)  ->  ssl:send(Sock, Req);
do_send(Sock, Req, false) ->  gen_tcp:send(Sock, Req).

%% @spec do_send_body(Sock::socket_descriptor(), Source::source_descriptor(), IsSSL::boolean()) -> ok | error()
%% source_descriptor() = fun_arity_0           |
%%                       {fun_arity_0}         |
%%                       {fun_arity_1, term()}
%% error() = term()
do_send_body(Sock, Source, IsSSL) when is_function(Source) ->
    do_send_body(Sock, {Source}, IsSSL);
do_send_body(Sock, {Source}, IsSSL) when is_function(Source) ->
    do_send_body1(Sock, Source, IsSSL, Source());
do_send_body(Sock, {Source, State}, IsSSL) when is_function(Source) ->
    do_send_body1(Sock, Source, IsSSL, Source(State));
do_send_body(Sock, Body, IsSSL) ->
    do_send(Sock, Body, IsSSL).

do_send_body1(Sock, Source, IsSSL, Resp) ->
    case Resp of
	{ok, Data} ->
	    do_send(Sock, Data, IsSSL),
	    do_send_body(Sock, {Source}, IsSSL);
	{ok, Data, NewState} ->
	    do_send(Sock, Data, IsSSL),
	    do_send_body(Sock, {Source, NewState}, IsSSL);
	eof -> ok;
	Err -> Err
    end.

do_close(Sock, true)  ->  ssl:close(Sock);
do_close(Sock, false) ->  gen_tcp:close(Sock).

do_setopts(Sock, Opts, true)  ->  ssl:setopts(Sock, Opts);
do_setopts(Sock, Opts, false) ->  inet:setopts(Sock, Opts).

check_ssl_options(Options, State) ->
    case get_value(is_ssl, Options, false) of
	false ->
	    State;
	true ->
	    State#state{is_ssl=true, ssl_options=get_value(ssl_options, Options)}
    end.

send_req_1(#url{abspath = AbsPath,
		host = Host,
		port = Port, 
		path = RelPath} = Url,
	   Headers, Method, Body, Options, Sock, State) ->
    Headers_1 = add_auth_headers(Url, Options, Headers, State),
    HostHeaderValue = case lists:keysearch(host_header, 1, Options) of
			  false ->
			      case Port of
				  80 -> Host;
				  _ -> [Host, ":", integer_to_list(Port)]
			      end;
			  {value, {_, Host_h_val}} ->
			      Host_h_val
		      end,
    {Req, Body_1} = make_request(Method, 
				 [{"Host", HostHeaderValue} | Headers_1],
				 AbsPath, RelPath, Body, Options, State#state.use_proxy),
    case get(my_trace_flag) of 
	true ->
	    %%Avoid the binary operations if trace is not on...
	    NReq = binary_to_list(list_to_binary(Req)),
	    do_trace("Sending request: ~n"
		     "--- Request Begin ---~n~s~n"
		     "--- Request End ---~n", [NReq]);
	_ -> ok
    end,
    SndRes = case do_send(Sock, Req, State#state.is_ssl) of
		 ok -> do_send_body(Sock, Body_1, State#state.is_ssl);
		 Err -> 
		     io:format("Err: ~p~n", [Err]),
		     Err
	     end,
    do_setopts(Sock, [{active, true}], State#state.is_ssl),
    SndRes.

add_auth_headers(#url{username = User,
		      password = UPw}, 
		 Options,
		 Headers, 
		 #state{use_proxy = UseProxy,
		        proxy_auth_digest = ProxyAuthDigest}) ->
    Headers_1 = case User of
		    undefined ->
			case get_value(basic_auth, Options, undefined) of
			    undefined ->
				Headers;
			    {U,P} ->
				[{"Authorization", ["Basic ", http_auth_digest(U, P)]} | Headers]
			end;
		    _ ->
			[{"Authorization", ["Basic ", http_auth_digest(User, UPw)]} | Headers]
		end,
    case UseProxy of
	false ->
	    Headers_1;
	true when ProxyAuthDigest == [] ->
	    Headers_1;
	true ->
	    [{"Proxy-Authorization", ["Basic ", ProxyAuthDigest]} | Headers_1]
    end.
			
http_auth_digest([], []) ->
    [];
http_auth_digest(Username, Password) ->
    encode_base64(Username ++ [$: | Password]).

encode_base64([]) ->
    [];
encode_base64([A]) ->
    [e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A,B]) ->
    [e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)), e((B band 15) bsl 2), $=];
encode_base64([A,B,C|Ls]) ->
    encode_base64_do(A,B,C, Ls).
encode_base64_do(A,B,C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [e(BB bsr 18), e((BB bsr 12) band 63), 
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].

e(X) when X >= 0, X < 26 -> X+65;
e(X) when X>25, X<52     -> X+71;
e(X) when X>51, X<62     -> X-4;
e(62)                    -> $+;
e(63)                    -> $/;
e(X)                     -> exit({bad_encode_base64_token, X}).

make_request(Method, Headers, AbsPath, RelPath, Body, Options, UseProxy) ->
    HttpVsn = http_vsn_string(get_value(http_vsn, Options, {1,1})),
    Headers_1 =
	case get_value(content_length, Headers, false) of
	    false when (Body == []) or
	               (Body == <<>>) or
	               is_tuple(Body) or
	               is_function(Body) ->
		Headers;
	    false when is_binary(Body) ->
		[{"content-length", integer_to_list(size(Body))} | Headers];
	    false ->
		[{"content-length", integer_to_list(length(Body))} | Headers];
	    _ ->
		Headers
	end,
    {Headers_2, Body_1} = 
	case get_value(transfer_encoding, Options, false) of
	    false ->
		{Headers_1, Body};
	    {chunked, ChunkSize} ->
		{[{X, Y} || {X, Y} <- Headers_1, 
			    X /= "Content-Length",
			    X /= "content-length",
			    X /= content_length] ++
		 [{"Transfer-Encoding", "chunked"}],
		 chunk_request_body(Body, ChunkSize)}
	end,
    Headers_3 = cons_headers(Headers_2),
    Uri = case get_value(use_absolute_uri, Options, false) or UseProxy of
	      true ->
		  AbsPath;
	      false -> 
		  RelPath
	  end,
    {[method(Method), " ", Uri, " ", HttpVsn, crnl(), Headers_3, crnl()], Body_1}.

http_vsn_string({0,9}) -> "HTTP/0.9";
http_vsn_string({1,0}) -> "HTTP/1.0";
http_vsn_string({1,1}) -> "HTTP/1.1".

cons_headers(Headers) ->
    cons_headers(Headers, []).
cons_headers([], Acc) ->
    encode_headers(Acc);
cons_headers([{basic_auth, {U,P}} | T], Acc) ->
    cons_headers(T, [{"Authorization",
		      ["Basic ", ibrowse_lib:encode_base64(U++":"++P)]} | Acc]);
cons_headers([{cookie, Cookie} | T], Acc) ->
    cons_headers(T, [{"Cookie", Cookie} | Acc]);
cons_headers([{content_length, L} | T], Acc) ->
    cons_headers(T, [{"Content-Length", L} | Acc]);
cons_headers([{content_type, L} | T], Acc) ->
    cons_headers(T, [{"Content-Type", L} | Acc]);
cons_headers([H | T], Acc) ->
    cons_headers(T, [H | Acc]).

encode_headers(L) ->
    encode_headers(L, []).
encode_headers([{http_vsn, _Val} | T], Acc) ->
    encode_headers(T, Acc);
encode_headers([{Name,Val} | T], Acc) when list(Name) ->
    encode_headers(T, [[Name, ": ", fmt_val(Val), crnl()] | Acc]);
encode_headers([{Name,Val} | T], Acc) when atom(Name) ->
    encode_headers(T, [[atom_to_list(Name), ": ", fmt_val(Val), crnl()] | Acc]);
encode_headers([], Acc) ->
    lists:reverse(Acc).

chunk_request_body(Body, ChunkSize) ->
    chunk_request_body(Body, ChunkSize, []).

chunk_request_body(Body, _ChunkSize, Acc) when Body == <<>>; Body == [] ->
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk | Acc]);
chunk_request_body(Body, ChunkSize, Acc) when binary(Body),
                                              size(Body) >= ChunkSize ->
    <<ChunkBody:ChunkSize/binary, Rest/binary>> = Body,
    Chunk = [ibrowse_lib:dec2hex(4, ChunkSize),"\r\n",
	     ChunkBody, "\r\n"],
    chunk_request_body(Rest, ChunkSize, [Chunk | Acc]);
chunk_request_body(Body, _ChunkSize, Acc) when binary(Body) ->
    BodySize = size(Body),
    Chunk = [ibrowse_lib:dec2hex(4, BodySize),"\r\n",
	     Body, "\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk, Chunk | Acc]);
chunk_request_body(Body, ChunkSize, Acc) when list(Body),
                                              length(Body) >= ChunkSize ->
    {ChunkBody, Rest} = split_list_at(Body, ChunkSize),
    Chunk = [ibrowse_lib:dec2hex(4, ChunkSize),"\r\n",
	     ChunkBody, "\r\n"],
    chunk_request_body(Rest, ChunkSize, [Chunk | Acc]);
chunk_request_body(Body, _ChunkSize, Acc) when list(Body) ->
    BodySize = length(Body),
    Chunk = [ibrowse_lib:dec2hex(4, BodySize),"\r\n",
	     Body, "\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk, Chunk | Acc]).


parse_response(_Data, #state{cur_req = undefined}=State) ->
    State#state{status = idle};
parse_response(Data, #state{reply_buffer=Acc, reqs=Reqs,
			    cur_req=CurReq}=State) ->
    #request{from=From, stream_to=StreamTo, req_id=ReqId,
	     method=Method} = CurReq,
    MaxHeaderSize = ibrowse:get_config_value(max_headers_size, infinity),
    case scan_header(Data, Acc) of
	{yes, Headers, Data_1}  ->
	    do_trace("Recvd Header Data -> ~s~n----~n", [Headers]),
	    do_trace("Recvd headers~n--- Headers Begin ---~n~s~n--- Headers End ---~n~n", [Headers]),
	    {HttpVsn, StatCode, Headers_1} = parse_headers(Headers),
	    do_trace("HttpVsn: ~p StatusCode: ~p Headers_1 -> ~1000.p~n", [HttpVsn, StatCode, Headers_1]),
	    LCHeaders = [{to_lower(X), Y} || {X,Y} <- Headers_1],
	    ConnClose = to_lower(get_value("connection", LCHeaders, "false")),
	    IsClosing = is_connection_closing(HttpVsn, ConnClose),
	    case IsClosing of
		true ->
                    shutting_down(State);
		false ->
		    ok
	    end,
	    State_1 = State#state{recvd_headers=Headers_1, status=get_body, 
				  http_status_code=StatCode, is_closing=IsClosing},
	    put(conn_close, ConnClose),
	    TransferEncoding = to_lower(get_value("transfer-encoding", LCHeaders, "false")),
	    case get_value("content-length", LCHeaders, undefined) of
		_ when Method == head ->
		    {_, Reqs_1} = queue:out(Reqs),
		    send_async_headers(ReqId, StreamTo, StatCode, Headers_1),
		    State_1_1 = do_reply(State_1, From, StreamTo, ReqId, {ok, StatCode, Headers_1, []}),
		    cancel_timer(State_1_1#state.send_timer, {eat_message, {req_timedout, From}}),
		    State_2 = reset_state(State_1_1),
		    State_3 = set_cur_request(State_2#state{reqs = Reqs_1}),
		    parse_response(Data_1, State_3);
		_ when hd(StatCode) == $1 ->
		    %% No message body is expected. Server may send
		    %% one or more 1XX responses before a proper
		    %% response.
		    send_async_headers(ReqId, StreamTo, StatCode, Headers_1),
		    do_trace("Recvd a status code of ~p. Ignoring and waiting for a proper response~n", [StatCode]),
		    parse_response(Data_1, State_1#state{recvd_headers = [],
							 status = get_header});
		_ when StatCode == "204";
		       StatCode == "304" ->
		    %% No message body is expected for these Status Codes.
		    %% RFC2616 - Sec 4.4
		    {_, Reqs_1} = queue:out(Reqs),
		    send_async_headers(ReqId, StreamTo, StatCode, Headers_1),
		    State_1_1 = do_reply(State_1, From, StreamTo, ReqId, {ok, StatCode, Headers_1, []}),
		    cancel_timer(State_1_1#state.send_timer, {eat_message, {req_timedout, From}}),
		    State_2 = reset_state(State_1_1),
		    State_3 = set_cur_request(State_2#state{reqs = Reqs_1}),
		    parse_response(Data_1, State_3);
		_ when TransferEncoding == "chunked" ->
		    do_trace("Chunked encoding detected...~n",[]),
		    send_async_headers(ReqId, StreamTo, StatCode, Headers_1),
		    case parse_11_response(Data_1, State_1#state{transfer_encoding=chunked,
								 chunk_size=chunk_start,
								 reply_buffer=[], chunks=[]}) of
			{error, Reason} ->
			    fail_pipelined_requests(State_1, 
						    {error, {Reason,
							     {stat_code, StatCode}, Headers_1}}),
			    {error, Reason};
			State_2 ->
			    State_2
		    end;
		undefined when HttpVsn == "HTTP/1.0";
			       ConnClose == "close" ->
		    send_async_headers(ReqId, StreamTo, StatCode, Headers_1),
		    State_1#state{reply_buffer=[Data_1]};
		undefined ->
		    fail_pipelined_requests(State_1, 
					    {error, {content_length_undefined,
						     {stat_code, StatCode}, Headers}}),
		    {error, content_length_undefined};
		V ->
		    case catch list_to_integer(V) of
			V_1 when integer(V_1), V_1 >= 0 ->
			    send_async_headers(ReqId, StreamTo, StatCode, Headers_1),
			    do_trace("Recvd Content-Length of ~p~n", [V_1]),
			    State_2 = State_1#state{rep_buf_size=0,
						    reply_buffer=[],
						    content_length=V_1},
			    case parse_11_response(Data_1, State_2) of
				{error, Reason} ->
				    fail_pipelined_requests(State_1, 
							    {error, {Reason,
								     {stat_code, StatCode}, Headers_1}}),
				    {error, Reason};
				State_3 ->
				    State_3
			    end;
			_ ->
			    fail_pipelined_requests(State_1, 
					    {error, {content_length_undefined,
						     {stat_code, StatCode}, Headers}}),
			    {error, content_length_undefined}
		    end
	    end;
	{no, Acc_1} when MaxHeaderSize == infinity ->
	    State#state{reply_buffer=Acc_1};
	{no, Acc_1} when length(Acc_1) < MaxHeaderSize ->
	    State#state{reply_buffer=Acc_1};
	{no, _Acc_1} ->
	    fail_pipelined_requests(State, {error, max_headers_size_exceeded}),
	    {error, max_headers_size_exceeded}
    end.

is_connection_closing("HTTP/0.9", _)       -> true;
is_connection_closing(_, "close")          -> true;
is_connection_closing("HTTP/1.0", "false") -> true;
is_connection_closing(_, _)                -> false.

%% This clause determines the chunk size when given data from the beginning of the chunk
parse_11_response(DataRecvd, 
		  #state{transfer_encoding=chunked,
			 chunk_size=chunk_start,
			 cur_req=CurReq,
			 reply_buffer=Buf}=State) ->
    case scan_crlf(DataRecvd, Buf) of
	{yes, ChunkHeader, Data_1} ->
	    case parse_chunk_header(ChunkHeader) of
		{error, Reason} ->
		    {error, Reason};
		ChunkSize ->
		    #request{stream_to=StreamTo, req_id=ReqId} = CurReq,
		    %%
		    %% Do we have to preserve the chunk encoding when streaming?
		    %%
		    do_interim_reply(StreamTo, ReqId, {chunk_start, ChunkSize}),
		    RemLen = length(Data_1),
		    do_trace("Determined chunk size: ~p. Already recvd: ~p~n", [ChunkSize, RemLen]),
		    parse_11_response(Data_1, State#state{rep_buf_size=0, 
							  reply_buffer=[],
							  deleted_crlf=true,
							  chunk_size=ChunkSize})
	    end;
	{no, Data_1} ->
	    State#state{reply_buffer=Data_1, rep_buf_size=length(Data_1)}
    end;

%% This clause is there to remove the CRLF between two chunks
%% 
parse_11_response(DataRecvd, 
		  #state{transfer_encoding=chunked,
			 chunk_size=tbd,
			 chunks = Chunks,
			 cur_req=CurReq,
			 reply_buffer=Buf}=State) ->
    case scan_crlf(DataRecvd, Buf) of
	{yes, _, NextChunk} ->
	    #request{stream_to=StreamTo, req_id=ReqId} = CurReq,
	    %%
	    %% Do we have to preserve the chunk encoding when streaming?
	    %%
	    State_1 = State#state{chunk_size=chunk_start,
				  rep_buf_size=0, 
				  reply_buffer=[],
				  deleted_crlf=true},
	    State_2 = case StreamTo of
			  undefined ->
			      State_1#state{chunks = [Buf | Chunks]};
		_ ->
			      do_interim_reply(StreamTo, ReqId, chunk_end),
			      State_1
		      end,
	    parse_11_response(NextChunk, State_2);
	{no, Data_1} ->
	    State#state{reply_buffer=Data_1, rep_buf_size=length(Data_1)}
    end;

%% This clause deals with the end of a chunked transfer
parse_11_response(DataRecvd, 
		  #state{transfer_encoding=chunked, chunk_size=0,
			 cur_req=CurReq,
			 deleted_crlf = DelCrlf,
			 reply_buffer=Trailer, reqs=Reqs}=State) ->
    do_trace("Detected end of chunked transfer...~n", []),
    DataRecvd_1 = case DelCrlf of
		      false -> 
			  DataRecvd;
		      true ->
			  [$\r, $\n | DataRecvd]
		  end,
    #request{stream_to=StreamTo, req_id=ReqId} = CurReq,
    case scan_header(DataRecvd_1, Trailer) of
	{yes, _TEHeaders, Rem} ->
	    {_, Reqs_1} = queue:out(Reqs),
	    %%
	    %% Do we have to preserve the chunk encoding when streaming?
	    %%
	    do_interim_reply(StreamTo, ReqId, chunk_end),
	    State_1 = handle_response(CurReq, State#state{reqs=Reqs_1}),
	    parse_response(Rem, reset_state(State_1));
	{no, Rem} ->
	    State#state{reply_buffer=Rem, rep_buf_size=length(Rem), deleted_crlf=false}
    end;

%% This clause extracts a chunk, given the size.
parse_11_response(DataRecvd, 
		  #state{transfer_encoding=chunked, chunk_size=CSz,
			 rep_buf_size=RepBufSz}=State) ->
    NeedBytes = CSz - RepBufSz,
    DataLen = length(DataRecvd),
    do_trace("Recvd more data: size: ~p. NeedBytes: ~p~n", [DataLen, NeedBytes]),
    case DataLen >= NeedBytes of
	true ->
	    {RemChunk, RemData} = split_list_at(DataRecvd, NeedBytes),
	    do_trace("Recvd another chunk...~n", []),
	    do_trace("RemData -> ~p~n", [RemData]),
	    case accumulate_response(RemChunk, State) of
		{error, Reason} ->
		    do_trace("Error accumulating response --> ~p~n", [Reason]),
		    {error, Reason};
		#state{reply_buffer = NewRepBuf,
		       chunks = NewChunks} = State_1 ->
		    State_2 = State_1#state{reply_buffer=[],
					    chunks = [lists:reverse(NewRepBuf) | NewChunks],
					    rep_buf_size=0,
					    chunk_size=tbd},
		    parse_11_response(RemData, State_2)
	    end;
	false ->
	    accumulate_response(DataRecvd, State#state{rep_buf_size=RepBufSz + DataLen})
    end;

%% This clause to extract the body when Content-Length is specified
parse_11_response(DataRecvd, 
		  #state{content_length=CL, rep_buf_size=RepBufSz, 
			 reqs=Reqs}=State) ->
    NeedBytes = CL - RepBufSz,
    DataLen = length(DataRecvd),
    case DataLen >= NeedBytes of
	true ->
	    {RemBody, Rem} = split_list_at(DataRecvd, NeedBytes),
	    {_, Reqs_1} = queue:out(Reqs),
	    State_1 = accumulate_response(RemBody, State),
	    State_2 = handle_response(State_1#state.cur_req, State_1#state{reqs=Reqs_1}),
	    State_3 = reset_state(State_2),
	    parse_response(Rem, State_3);
	false ->
	    accumulate_response(DataRecvd, State#state{rep_buf_size=RepBufSz+DataLen})
    end.

handle_response(#request{from=From, stream_to=StreamTo, req_id=ReqId,
			 save_response_to_file = SaveResponseToFile, 
			 tmp_file_name = TmpFilename,
			 tmp_file_fd = Fd
			},
		#state{http_status_code = SCode,
		       send_timer = ReqTimer,
		       reply_buffer = RepBuf,
		       transfer_encoding = TEnc,
		       chunks = Chunks,
		       recvd_headers = RespHeaders}=State) when SaveResponseToFile /= false ->
    Body = case TEnc of
	       chunked ->
		   lists:flatten(lists:reverse(Chunks));
	       _ ->
		   lists:flatten(lists:reverse(RepBuf))
	   end,
    State_1 = set_cur_request(State),
    file:close(Fd),
    ResponseBody = case TmpFilename of
		       undefined ->
			   Body;
		       _ ->
			   {file, TmpFilename}
		   end,
    State_2 = do_reply(State_1, From, StreamTo, ReqId, {ok, SCode, RespHeaders, ResponseBody}),
    cancel_timer(ReqTimer, {eat_message, {req_timedout, From}}),
    State_2;
handle_response(#request{from=From, stream_to=StreamTo, req_id=ReqId},
		#state{http_status_code=SCode, recvd_headers=RespHeaders,
		       reply_buffer=RepBuf, transfer_encoding=TEnc,
		       chunks=Chunks, send_timer=ReqTimer}=State) ->
    Body = case TEnc of
	       chunked ->
		   lists:flatten(lists:reverse(Chunks));
	       _ ->
		   lists:flatten(lists:reverse(RepBuf))
	   end,
    State_1 = set_cur_request(State),
    case get(conn_close) of
	"close" ->
	    do_reply(State_1, From, StreamTo, ReqId, {ok, SCode, RespHeaders, Body}),
	    exit(normal);
	_ ->
	    State_2 = do_reply(State_1, From, StreamTo, ReqId, {ok, SCode, RespHeaders, Body}),
	    cancel_timer(ReqTimer, {eat_message, {req_timedout, From}}),
	    State_2
    end.

reset_state(State) ->
    State#state{status=get_header, rep_buf_size=0,content_length=undefined,
		reply_buffer=[], chunks=[], recvd_headers=[], deleted_crlf=false,
		http_status_code=undefined, chunk_size=undefined, transfer_encoding=undefined}.

set_cur_request(#state{reqs = Reqs} = State) ->
    case queue:to_list(Reqs) of
	[] ->
	    State#state{cur_req = undefined};
	[NextReq | _] ->
	    State#state{cur_req = NextReq}
    end.

parse_headers(Headers) ->
    case scan_crlf(Headers, []) of
	{yes, StatusLine, T} ->
	    Headers_1 = parse_headers_1(T),
	    case parse_status_line(StatusLine) of
		{ok, HttpVsn, StatCode, _Msg} ->
		    put(http_prot_vsn, HttpVsn),
		    {HttpVsn, StatCode, Headers_1};
		_ -> %% A HTTP 0.9 response?
		    put(http_prot_vsn, "HTTP/0.9"),
		    {"HTTP/0.9", undefined, Headers}
	    end;
	_ ->
	    {error, no_status_line}
    end.

% From RFC 2616
%
%    HTTP/1.1 header field values can be folded onto multiple lines if
%    the continuation line begins with a space or horizontal tab. All
%    linear white space, including folding, has the same semantics as
%    SP. A recipient MAY replace any linear white space with a single
%    SP before interpreting the field value or forwarding the message
%    downstream.
parse_headers_1(String) ->
    parse_headers_1(String, [], []).

parse_headers_1([$\n, H |T], [$\r | L], Acc) when H == 32;
						  H == $\t -> 
    parse_headers_1(lists:dropwhile(fun(X) ->
					    is_whitespace(X)
				    end, T), [32 | L], Acc);
parse_headers_1([$\n|T], [$\r | L], Acc) -> 
    case parse_header(lists:reverse(L)) of
	invalid ->
	    parse_headers_1(T, [], Acc);
	NewHeader ->
	    parse_headers_1(T, [], [NewHeader | Acc])
    end;
parse_headers_1([H|T],  L, Acc) -> 
    parse_headers_1(T, [H|L], Acc);
parse_headers_1([], [], Acc) ->
    lists:reverse(Acc);
parse_headers_1([], L, Acc) ->
    Acc_1 = case parse_header(lists:reverse(L)) of
		invalid ->
		    Acc;
		NewHeader ->
		    [NewHeader | Acc]
	    end,
    lists:reverse(Acc_1).

parse_status_line(Line) ->
    parse_status_line(Line, get_prot_vsn, [], []).
parse_status_line([32 | T], get_prot_vsn, ProtVsn, StatCode) ->
    parse_status_line(T, get_status_code, ProtVsn, StatCode);
parse_status_line([32 | T], get_status_code, ProtVsn, StatCode) ->
    {ok, lists:reverse(ProtVsn), lists:reverse(StatCode), T};
parse_status_line([H | T], get_prot_vsn, ProtVsn, StatCode) ->
    parse_status_line(T, get_prot_vsn, [H|ProtVsn], StatCode);
parse_status_line([H | T], get_status_code, ProtVsn, StatCode) ->
    parse_status_line(T, get_status_code, ProtVsn, [H | StatCode]);
parse_status_line([], _, _, _) ->
    http_09.

parse_header(L) ->
    parse_header(L, []).
parse_header([$: | V], Acc) ->
    {lists:reverse(Acc), string:strip(V)};
parse_header([H | T], Acc) ->
    parse_header(T, [H | Acc]);
parse_header([], _) ->
    invalid.

scan_header([$\n|T], [$\r,$\n,$\r|L]) -> {yes, lists:reverse([$\n,$\r| L]), T};
scan_header([H|T],  L)                -> scan_header(T, [H|L]);
scan_header([], L)                    -> {no, L}.

scan_crlf([$\n|T], [$\r | L]) -> {yes, lists:reverse(L), T};
scan_crlf([H|T],  L)          -> scan_crlf(T, [H|L]);
scan_crlf([], L)              -> {no, L}.

fmt_val(L) when list(L)    -> L;
fmt_val(I) when integer(I) -> integer_to_list(I);
fmt_val(A) when atom(A)    -> atom_to_list(A);
fmt_val(Term)              -> io_lib:format("~p", [Term]).

crnl() -> "\r\n".

method(get)       -> "GET";
method(post)      -> "POST";
method(head)      -> "HEAD";
method(options)   -> "OPTIONS";
method(put)       -> "PUT";
method(delete)    -> "DELETE";
method(trace)     -> "TRACE";
method(mkcol)     -> "MKCOL";
method(propfind)  -> "PROPFIND";
method(proppatch) -> "PROPPATCH";
method(lock)      -> "LOCK";
method(unlock)    -> "UNLOCK";
method(move)      -> "MOVE";
method(copy)      -> "COPY".

%% From RFC 2616
%%
% The chunked encoding modifies the body of a message in order to
% transfer it as a series of chunks, each with its own size indicator,
% followed by an OPTIONAL trailer containing entity-header
% fields. This allows dynamically produced content to be transferred
% along with the information necessary for the recipient to verify
% that it has received the full message.
% 	Chunked-Body = 	*chunk
% 			last-chunk
% 			trailer
% 			CRLF
% 	chunk = chunk-size [ chunk-extension ] CRLF
% 		chunk-data CRLF
% 	chunk-size = 1*HEX
% 	last-chunk = 1*("0") [ chunk-extension ] CRLF
% 	chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
% 	chunk-ext-name = token
% 	chunk-ext-val = token | quoted-string
% 	chunk-data = chunk-size(OCTET)
% 	trailer = *(entity-header CRLF)
% The chunk-size field is a string of hex digits indicating the size
% of the chunk. The chunked encoding is ended by any chunk whose size
% is zero, followed by the trailer, which is terminated by an empty
% line.
%%
%% The parsing implemented here discards all chunk extensions. It also
%% strips trailing spaces from the chunk size fields as Apache 1.3.27 was
%% sending them.
parse_chunk_header([]) ->
    throw({error, invalid_chunk_size});
parse_chunk_header(ChunkHeader) ->
    parse_chunk_header(ChunkHeader, []).

parse_chunk_header([$; | _], Acc) ->
    hexlist_to_integer(lists:reverse(Acc));
parse_chunk_header([H | T], Acc) ->
    case is_whitespace(H) of
	true ->
	    parse_chunk_header(T, Acc);
	false ->
	    parse_chunk_header(T, [H | Acc])
    end;
parse_chunk_header([], Acc) ->
    hexlist_to_integer(lists:reverse(Acc)).

is_whitespace(32)  -> true;
is_whitespace($\r) -> true;
is_whitespace($\n) -> true;
is_whitespace($\t) -> true;
is_whitespace(_)   -> false.


send_async_headers(_ReqId, undefined, _StatCode, _Headers) ->
    ok;
send_async_headers(ReqId, StreamTo, StatCode, Headers) ->
    catch StreamTo ! {ibrowse_async_headers, ReqId, StatCode, Headers}.

do_reply(State, From, undefined, _, Msg) ->
    gen_server:reply(From, Msg),
    dec_pipeline_counter(State);
do_reply(State, _From, StreamTo, ReqId, {ok, _, _, _}) ->
    State_1 = dec_pipeline_counter(State),
    catch StreamTo ! {ibrowse_async_response_end, ReqId},
    State_1;
do_reply(State, _From, StreamTo, ReqId, Msg) ->
    State_1 = dec_pipeline_counter(State),
    catch StreamTo ! {ibrowse_async_response, ReqId, Msg},
    State_1.

do_interim_reply(undefined, _ReqId, _Msg) ->
    ok;
do_interim_reply(StreamTo, ReqId, Msg) ->
    catch StreamTo ! {ibrowse_async_response, ReqId, Msg}.

do_error_reply(#state{reqs = Reqs} = State, Err) ->
    ReqList = queue:to_list(Reqs),
    lists:foreach(fun(#request{from=From, stream_to=StreamTo, req_id=ReqId}) ->
                          do_reply(State, From, StreamTo, ReqId, {error, Err})
		  end, ReqList).

fail_pipelined_requests(#state{reqs = Reqs, cur_req = CurReq} = State, Reply) ->
    {_, Reqs_1} = queue:out(Reqs),
    #request{from=From, stream_to=StreamTo, req_id=ReqId} = CurReq,
    do_reply(State, From, StreamTo, ReqId, Reply),
    do_error_reply(State#state{reqs = Reqs_1}, previous_request_failed).


split_list_at(List, N) ->
    split_list_at(List, N, []).
split_list_at([], _, Acc) ->
    {lists:reverse(Acc), []};
split_list_at(List2, 0, List1) ->
    {lists:reverse(List1), List2};
split_list_at([H | List2], N, List1) ->
    split_list_at(List2, N-1, [H | List1]).

hexlist_to_integer(List) ->
    hexlist_to_integer(lists:reverse(List), 1, 0).
hexlist_to_integer([H | T], Multiplier, Acc) ->
    hexlist_to_integer(T, Multiplier*16, Multiplier*to_ascii(H) + Acc);
hexlist_to_integer([], _, Acc) ->
    Acc.

to_ascii($A) -> 10;
to_ascii($a) -> 10;
to_ascii($B) -> 11;
to_ascii($b) -> 11;
to_ascii($C) -> 12;
to_ascii($c) -> 12;
to_ascii($D) -> 13;
to_ascii($d) -> 13;
to_ascii($E) -> 14;
to_ascii($e) -> 14;
to_ascii($F) -> 15;
to_ascii($f) -> 15;
to_ascii($1) -> 1;
to_ascii($2) -> 2;
to_ascii($3) -> 3;
to_ascii($4) -> 4;
to_ascii($5) -> 5;
to_ascii($6) -> 6;
to_ascii($7) -> 7;
to_ascii($8) -> 8;
to_ascii($9) -> 9;
to_ascii($0) -> 0.

cancel_timer(undefined) -> ok;
cancel_timer(Ref)       -> erlang:cancel_timer(Ref).

cancel_timer(Ref, {eat_message, Msg}) ->
    cancel_timer(Ref),
    receive 
	Msg ->
	    ok
    after 0 ->
	    ok
    end.

make_req_id() ->
    now().

to_lower(Str) ->
    to_lower(Str, []).
to_lower([H|T], Acc) when H >= $A, H =< $Z ->
    to_lower(T, [H+32|Acc]);
to_lower([H|T], Acc) ->
    to_lower(T, [H|Acc]);
to_lower([], Acc) ->
    lists:reverse(Acc).

shutting_down(#state{lb_ets_tid = undefined}) ->
    ok;
shutting_down(#state{lb_ets_tid = Tid,
		     cur_pipeline_size = Sz}) ->
    catch ets:delete(Tid, {Sz, self()}).

inc_pipeline_counter(#state{is_closing = true} = State) ->
    State;
inc_pipeline_counter(#state{cur_pipeline_size = Pipe_sz} = State) ->
    State#state{cur_pipeline_size = Pipe_sz + 1}.

dec_pipeline_counter(#state{is_closing = true} = State) ->
    State;
dec_pipeline_counter(#state{lb_ets_tid = undefined} = State) ->
    State;
dec_pipeline_counter(#state{cur_pipeline_size = Pipe_sz,
			    lb_ets_tid = Tid} = State) ->
    ets:delete(Tid, {Pipe_sz, self()}),
    ets:insert(Tid, {{Pipe_sz - 1, self()}, []}),
    State#state{cur_pipeline_size = Pipe_sz - 1}.
