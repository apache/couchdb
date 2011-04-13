%%%-------------------------------------------------------------------
%%% File    : ibrowse_http_client.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : The name says it all
%%%
%%% Created : 11 Oct 2003 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%%-------------------------------------------------------------------
-module(ibrowse_http_client).
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
-include_lib("kernel/include/inet.hrl").

-record(state, {host, port, connect_timeout,
                inactivity_timer_ref,
                use_proxy = false, proxy_auth_digest,
                ssl_options = [], is_ssl = false, socket,
                proxy_tunnel_setup = false,
                tunnel_setup_queue = [],
                reqs=queue:new(), cur_req, status=idle, http_status_code,
                reply_buffer = <<>>, rep_buf_size=0, streamed_size = 0,
                recvd_headers=[],
                status_line, raw_headers, 
                is_closing, send_timer, content_length,
                deleted_crlf = false, transfer_encoding,
                chunk_size, chunk_size_buffer = <<>>,
                recvd_chunk_size, interim_reply_sent = false,
                lb_ets_tid, cur_pipeline_size = 0, prev_req_id
               }).

-record(request, {url, method, options, from,
                  stream_to, caller_controls_socket = false, 
                  caller_socket_options = [],
                  req_id,
                  stream_chunk_size,
                  save_response_to_file = false, 
                  tmp_file_name, tmp_file_fd, preserve_chunked_encoding,
                  response_format}).

-import(ibrowse_lib, [
                      get_value/2,
                      get_value/3,
                      do_trace/2
                     ]).

-define(DEFAULT_STREAM_CHUNK_SIZE, 1024*1024).
-define(dec2hex(X), erlang:integer_to_list(X, 16)).
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
    case catch gen_server:call(Conn_pid, stop) of
        {'EXIT', {timeout, _}} ->
            exit(Conn_pid, kill),
            ok;
        _ ->
            ok
    end.

send_req(Conn_Pid, Url, Headers, Method, Body, Options, Timeout) ->
    gen_server:call(
      Conn_Pid,
      {send_req, {Url, Headers, Method, Body, Options, Timeout}}, Timeout).

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
init(Url) when is_list(Url) ->
    case catch ibrowse_lib:parse_url(Url) of
        #url{protocol = Protocol} = Url_rec ->
            init({undefined, Url_rec, {[], Protocol == https}});
        {'EXIT', _} ->
            {error, invalid_url}
    end;
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
handle_call({send_req, _}, _From, #state{is_closing = true} = State) ->
    {reply, {error, connection_closing}, State};

handle_call({send_req, {Url, Headers, Method, Body, Options, Timeout}},
            From, State) ->
    send_req_1(From, Url, Headers, Method, Body, Options, Timeout, State);

handle_call(stop, _From, State) ->
    do_close(State),
    do_error_reply(State, closing_on_request),
    {stop, normal, ok, State};

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
handle_info({tcp, _Sock, Data}, #state{status = Status} = State) ->
%%    io:format("Recvd data: ~p~n", [Data]),
    do_trace("Data recvd in state: ~p. Size: ~p. ~p~n~n", [Status, size(Data), Data]),
    handle_sock_data(Data, State);
handle_info({ssl, _Sock, Data}, State) ->
    handle_sock_data(Data, State);

handle_info({stream_next, Req_id}, #state{socket = Socket,
                                          cur_req = #request{req_id = Req_id}} = State) ->
    %% io:format("Client process set {active, once}~n", []),
    do_setopts(Socket, [{active, once}], State),
    {noreply, set_inac_timer(State)};

handle_info({stream_next, _Req_id}, State) ->
    _Cur_req_id = case State#state.cur_req of
                     #request{req_id = Cur} ->
                         Cur;
                     _ ->
                         undefined
                 end,
%%     io:format("Ignoring stream_next as ~1000.p is not cur req (~1000.p)~n",
%%               [_Req_id, _Cur_req_id]),
    {noreply, State};

handle_info({stream_close, _Req_id}, State) ->
    shutting_down(State),
    do_close(State),
    do_error_reply(State, closing_on_request),
    {stop, normal, State};

handle_info({tcp_closed, _Sock}, State) ->    
    do_trace("TCP connection closed by peer!~n", []),
    handle_sock_closed(State),
    {stop, normal, State};
handle_info({ssl_closed, _Sock}, State) ->
    do_trace("SSL connection closed by peer!~n", []),
    handle_sock_closed(State),
    {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
    do_trace("Error on connection to ~1000.p:~1000.p -> ~1000.p~n",
             [State#state.host, State#state.port, Reason]),
    handle_sock_closed(State),
    {stop, normal, State};
handle_info({ssl_error, _Sock, Reason}, State) ->
    do_trace("Error on SSL connection to ~1000.p:~1000.p -> ~1000.p~n",
             [State#state.host, State#state.port, Reason]),
    handle_sock_closed(State),
    {stop, normal, State};

handle_info({req_timedout, From}, State) ->
    case lists:keymember(From, #request.from, queue:to_list(State#state.reqs)) of
        false ->
            {noreply, State};
        true ->
            shutting_down(State),
            do_error_reply(State, req_timedout),
            {stop, normal, State}
    end;

handle_info(timeout, State) ->
    do_trace("Inactivity timeout triggered. Shutting down connection~n", []),
    shutting_down(State),
    do_error_reply(State, req_timedout),
    {stop, normal, State};

handle_info({trace, Bool}, State) ->
    put(my_trace_flag, Bool),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Unknown message recvd for ~1000.p:~1000.p -> ~p~n",
              [State#state.host, State#state.port, Info]),
    io:format("Recvd unknown message ~p when in state: ~p~n", [Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    do_close(State),
    ok.

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
    do_close(State),
    {stop, normal, State};

handle_sock_data(Data, #state{status = get_header}=State) ->
    case parse_response(Data, State) of
        {error, _Reason} ->
            shutting_down(State),
            {stop, normal, State};
        #state{socket = Socket, status = Status, cur_req = CurReq} = State_1 ->
            case {Status, CurReq} of
                {get_header, #request{caller_controls_socket = true}} ->
                    do_setopts(Socket, [{active, once}], State_1);
                _ ->
                    active_once(State_1)
            end,
            {noreply, set_inac_timer(State_1)}
    end;

handle_sock_data(Data, #state{status           = get_body,
                              socket           = Socket,
                              content_length   = CL,
                              http_status_code = StatCode,
                              recvd_headers    = Headers,
                              chunk_size       = CSz} = State) ->
    case (CL == undefined) and (CSz == undefined) of
        true ->
            case accumulate_response(Data, State) of
                {error, Reason} ->
                    shutting_down(State),
                    fail_pipelined_requests(State,
                                            {error, {Reason, {stat_code, StatCode}, Headers}}),
                    {stop, normal, State};
                State_1 ->
                    active_once(State_1),
                    State_2 = set_inac_timer(State_1),
                    {noreply, State_2}
            end;
        _ ->
            case parse_11_response(Data, State) of
                {error, Reason} ->
                    shutting_down(State),
                    fail_pipelined_requests(State,
                                            {error, {Reason, {stat_code, StatCode}, Headers}}),
                    {stop, normal, State};
                #state{cur_req = #request{caller_controls_socket = Ccs},
                       interim_reply_sent = Irs} = State_1 ->
                    case Irs of
                        true ->
                            active_once(State_1);
                        false when Ccs == true ->
                            do_setopts(Socket, [{active, once}], State);
                        false ->
                            active_once(State_1)
                    end,
                    State_2 = State_1#state{interim_reply_sent = false},
                    case Ccs of
                    true ->
                        cancel_timer(State_2#state.inactivity_timer_ref, {eat_message, timeout}),
                        {noreply, State_2#state{inactivity_timer_ref = undefined}};
                    _ ->
                        {noreply, set_inac_timer(State_2)}
                    end;
                State_1 ->
                    active_once(State_1),
                    State_2 = set_inac_timer(State_1),
                    {noreply, State_2}
            end
    end.

accumulate_response(Data,
                    #state{
                      cur_req = #request{save_response_to_file = Srtf,
                                         tmp_file_fd = undefined} = CurReq,
                      http_status_code=[$2 | _]}=State) when Srtf /= false ->
    TmpFilename = make_tmp_filename(Srtf),
    case file:open(TmpFilename, [write, delayed_write, raw]) of
        {ok, Fd} ->
            accumulate_response(Data, State#state{
                                        cur_req = CurReq#request{
                                                    tmp_file_fd = Fd,
                                                    tmp_file_name = TmpFilename}});
        {error, Reason} ->
            {error, {file_open_error, Reason}}
    end;
accumulate_response(Data, #state{cur_req = #request{save_response_to_file = Srtf,
                                                    tmp_file_fd = Fd},
                                 transfer_encoding=chunked,
                                 reply_buffer = Reply_buf,
                                 http_status_code=[$2 | _]
                                } = State) when Srtf /= false ->
    case file:write(Fd, [Reply_buf, Data]) of
        ok ->
            State#state{reply_buffer = <<>>};
        {error, Reason} ->
            {error, {file_write_error, Reason}}
    end;
accumulate_response(Data, #state{cur_req = #request{save_response_to_file = Srtf,
                                                    tmp_file_fd = Fd},
                                 reply_buffer = RepBuf,
                                 http_status_code=[$2 | _]
                                } = State) when Srtf /= false ->
    case file:write(Fd, [RepBuf, Data]) of
        ok ->
            State#state{reply_buffer = <<>>};
        {error, Reason} ->
            {error, {file_write_error, Reason}}
    end;
accumulate_response(Data, #state{reply_buffer      = RepBuf,
                                 rep_buf_size      = RepBufSize,
                                 streamed_size     = Streamed_size,
                                 cur_req           = CurReq}=State) ->
    #request{stream_to                 = StreamTo,
             req_id                    = ReqId,
             stream_chunk_size         = Stream_chunk_size,
             response_format           = Response_format,
             caller_controls_socket    = Caller_controls_socket} = CurReq,
    RepBuf_1 = <<RepBuf/binary, Data/binary>>,
    New_data_size = RepBufSize - Streamed_size,
    case StreamTo of
        undefined ->
            State#state{reply_buffer = RepBuf_1};
        _ when Caller_controls_socket == true ->
            do_interim_reply(StreamTo, Response_format, ReqId, RepBuf_1),
            State#state{reply_buffer = <<>>, 
                        interim_reply_sent = true,
                        streamed_size = Streamed_size + size(RepBuf_1)};
        _ when New_data_size >= Stream_chunk_size ->
            {Stream_chunk, Rem_data} = split_binary(RepBuf_1, Stream_chunk_size),
            do_interim_reply(StreamTo, Response_format, ReqId, Stream_chunk),
            State_1 = State#state{
                        reply_buffer = <<>>,
                        interim_reply_sent = true,
                        streamed_size = Streamed_size + Stream_chunk_size},
            case Rem_data of
                <<>> ->
                    State_1;
                _ ->
                    accumulate_response(Rem_data, State_1)
            end;
        _ ->
            State#state{reply_buffer = RepBuf_1}
    end.

make_tmp_filename(true) ->
    DownloadDir = ibrowse:get_config_value(download_dir, filename:absname("./")),
    {A,B,C} = now(),
    filename:join([DownloadDir,
                   "ibrowse_tmp_file_"++
                   integer_to_list(A) ++
                   integer_to_list(B) ++
                   integer_to_list(C)]);
make_tmp_filename(File) when is_list(File) ->
    File.


%%--------------------------------------------------------------------
%% Handles the case when the server closes the socket
%%--------------------------------------------------------------------
handle_sock_closed(#state{status=get_header} = State) ->
    shutting_down(State),
    do_error_reply(State, connection_closed);

handle_sock_closed(#state{cur_req=undefined} = State) ->
    shutting_down(State);

%% We check for IsClosing because this the server could have sent a
%% Connection-Close header and has closed the socket to indicate end
%% of response. There maybe requests pipelined which need a response.
handle_sock_closed(#state{reply_buffer = Buf, reqs = Reqs, http_status_code = SC,
                          is_closing = IsClosing,
                          cur_req = #request{tmp_file_name=TmpFilename,
                                             tmp_file_fd=Fd} = CurReq,
                          status = get_body,
                          recvd_headers = Headers,
                          status_line = Status_line,
                          raw_headers = Raw_headers
                         }=State) ->
    #request{from=From, stream_to=StreamTo, req_id=ReqId,
             response_format = Resp_format,
             options = Options} = CurReq,
    case IsClosing of
        true ->
            {_, Reqs_1} = queue:out(Reqs),
            Body = case TmpFilename of
                       undefined ->
                           Buf;
                       _ ->
                           ok = file:close(Fd),
                           {file, TmpFilename}
                   end,
            Reply = case get_value(give_raw_headers, Options, false) of
                          true ->
                            {ok, Status_line, Raw_headers, Body};
                        false ->
                            {ok, SC, Headers, Buf}
                    end,
            State_1 = do_reply(State, From, StreamTo, ReqId, Resp_format, Reply),
            ok = do_error_reply(State_1#state{reqs = Reqs_1}, connection_closed),
            State_1;
        _ ->
            ok = do_error_reply(State, connection_closed),
            State
    end.

do_connect(Host, Port, Options, #state{is_ssl      = true,
                                       use_proxy   = false,
                                       ssl_options = SSLOptions},
           Timeout) ->
    ssl:connect(Host, Port, get_sock_options(Host, Options, SSLOptions), Timeout);
do_connect(Host, Port, Options, _State, Timeout) ->
    gen_tcp:connect(Host, Port, get_sock_options(Host, Options, []), Timeout).

get_sock_options(Host, Options, SSLOptions) ->
    Caller_socket_options = get_value(socket_options, Options, []),
    Ipv6Options = case is_ipv6_host(Host) of
        true ->
            [inet6];
        false ->
            []
    end,
    Other_sock_options = filter_sock_options(SSLOptions ++ Caller_socket_options ++ Ipv6Options),
    case lists:keysearch(nodelay, 1, Other_sock_options) of
        false ->
            [{nodelay, true}, binary, {active, false} | Other_sock_options];
        {value, _} ->
            [binary, {active, false} | Other_sock_options]
    end.

is_ipv6_host(Host) ->
    case inet_parse:address(Host) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            true;
        {ok, {_, _, _, _}} ->
            false;
        _  ->
            case inet:gethostbyname(Host) of
                {ok, #hostent{h_addrtype = inet6}} ->
                    true;
                _ ->
                    false
            end
    end.

%% We don't want the caller to specify certain options
filter_sock_options(Opts) ->
    lists:filter(fun({active, _}) ->
                         false;
                    ({packet, _}) ->
                         false;
                    (list) ->
                         false;
                    (_) ->
                         true
                 end, Opts).

do_send(Req, #state{socket = Sock,
                    is_ssl = true,
                    use_proxy = true,
                    proxy_tunnel_setup = Pts}) when Pts /= done ->  gen_tcp:send(Sock, Req);
do_send(Req, #state{socket = Sock, is_ssl = true})  ->  ssl:send(Sock, Req);
do_send(Req, #state{socket = Sock, is_ssl = false}) ->  gen_tcp:send(Sock, Req).

%% @spec do_send_body(Sock::socket_descriptor(), Source::source_descriptor(), IsSSL::boolean()) -> ok | error()
%% source_descriptor() = fun_arity_0           |
%%                       {fun_arity_0}         |
%%                       {fun_arity_1, term()}
%% error() = term()
do_send_body(Source, State, TE) when is_function(Source) ->
    do_send_body({Source}, State, TE);
do_send_body({Source}, State, TE) when is_function(Source) ->
    do_send_body1(Source, Source(), State, TE);
do_send_body({Source, Source_state}, State, TE) when is_function(Source) ->
    do_send_body1(Source, Source(Source_state), State, TE);
do_send_body(Body, State, _TE) ->
    do_send(Body, State).

do_send_body1(Source, Resp, State, TE) ->
    case Resp of
        {ok, Data} ->
            do_send(maybe_chunked_encode(Data, TE), State),
            do_send_body({Source}, State, TE);
        {ok, Data, New_source_state} ->
            do_send(maybe_chunked_encode(Data, TE), State),
            do_send_body({Source, New_source_state}, State, TE);
        eof when TE == true ->
            do_send(<<"0\r\n\r\n">>, State),
            ok;
        eof ->
            ok;
        Err ->
            Err
    end.

maybe_chunked_encode(Data, false) ->
    Data;
maybe_chunked_encode(Data, true) ->
    [?dec2hex(iolist_size(Data)), "\r\n", Data, "\r\n"].

do_close(#state{socket = undefined})            ->  ok;
do_close(#state{socket = Sock,
                is_ssl = true,
                use_proxy = true,
                proxy_tunnel_setup = Pts
               }) when Pts /= done ->  catch gen_tcp:close(Sock);
do_close(#state{socket = Sock, is_ssl = true})  ->  catch ssl:close(Sock);
do_close(#state{socket = Sock, is_ssl = false}) ->  catch gen_tcp:close(Sock).

active_once(#state{cur_req = #request{caller_controls_socket = true}}) ->
    ok;
active_once(#state{socket = Socket} = State) ->
    do_setopts(Socket, [{active, once}], State).

do_setopts(_Sock, [],   _)    ->  ok;
do_setopts(Sock, Opts, #state{is_ssl = true,
                              use_proxy = true,
                              proxy_tunnel_setup = Pts}
                             ) when Pts /= done ->  inet:setopts(Sock, Opts);
do_setopts(Sock, Opts, #state{is_ssl = true}) -> ssl:setopts(Sock, Opts);
do_setopts(Sock, Opts, _) ->  inet:setopts(Sock, Opts).

check_ssl_options(Options, State) ->
    case get_value(is_ssl, Options, false) of
        false ->
            State;
        true ->
            State#state{is_ssl=true, ssl_options=get_value(ssl_options, Options)}
    end.

send_req_1(From,
           #url{host = Host,
                port = Port} = Url,
           Headers, Method, Body, Options, Timeout,
           #state{socket = undefined} = State) ->
    {Host_1, Port_1, State_1} =
        case get_value(proxy_host, Options, false) of
            false ->
                {Host, Port, State};
            PHost ->
                ProxyUser     = get_value(proxy_user, Options, []),
                ProxyPassword = get_value(proxy_password, Options, []),
                Digest        = http_auth_digest(ProxyUser, ProxyPassword),
                {PHost, get_value(proxy_port, Options, 80),
                 State#state{use_proxy = true,
                             proxy_auth_digest = Digest}}
        end,
    State_2 = check_ssl_options(Options, State_1),
    do_trace("Connecting...~n", []),
    Conn_timeout = get_value(connect_timeout, Options, Timeout),
    case do_connect(Host_1, Port_1, Options, State_2, Conn_timeout) of
        {ok, Sock} ->
            do_trace("Connected! Socket: ~1000.p~n", [Sock]),
            State_3 = State_2#state{socket = Sock,
                                    connect_timeout = Conn_timeout},
            send_req_1(From, Url, Headers, Method, Body, Options, Timeout, State_3);
        Err ->
            shutting_down(State_2),
            do_trace("Error connecting. Reason: ~1000.p~n", [Err]),
            gen_server:reply(From, {error, {conn_failed, Err}}),
            {stop, normal, State_2}
    end;

%% Send a CONNECT request.
%% Wait for 200 OK
%% Upgrade to SSL connection
%% Then send request

send_req_1(From,
           #url{
                host    = Server_host,
                port    = Server_port
                } = Url,
           Headers, Method, Body, Options, Timeout,
           #state{
                  proxy_tunnel_setup = false,
                  use_proxy = true,
                  is_ssl    = true} = State) ->
    NewReq = #request{
      method                    = connect,
      preserve_chunked_encoding = get_value(preserve_chunked_encoding, Options, false),
      options                   = Options
     },
    State_1 = State#state{reqs=queue:in(NewReq, State#state.reqs)},
    Pxy_auth_headers = maybe_modify_headers(Url, Method, Options, [], State_1),
    Path = [Server_host, $:, integer_to_list(Server_port)],
    {Req, Body_1} = make_request(connect, Pxy_auth_headers,
                                 Path, Path,
                                 [], Options, State_1, undefined),
    TE = is_chunked_encoding_specified(Options),
    trace_request(Req),
    case do_send(Req, State) of
        ok ->
            case do_send_body(Body_1, State_1, TE) of
                ok ->
                    trace_request_body(Body_1),
                    active_once(State_1),
                    Ref = case Timeout of
                              infinity ->
                                  undefined;
                              _ ->
                                  erlang:send_after(Timeout, self(), {req_timedout, From})
                          end,
                    State_2 = State_1#state{status     = get_header,
                                            cur_req    = NewReq,
                                            send_timer = Ref,
                                            proxy_tunnel_setup = in_progress,
                                            tunnel_setup_queue = [{From, Url, Headers, Method, Body, Options, Timeout}]},
                    State_3 = set_inac_timer(State_2),
                    {noreply, State_3};
                Err ->
                    shutting_down(State_1),
                    do_trace("Send failed... Reason: ~p~n", [Err]),
                    gen_server:reply(From, {error, {send_failed, Err}}),
                    {stop, normal, State_1}
            end;
        Err ->
            shutting_down(State_1),
            do_trace("Send failed... Reason: ~p~n", [Err]),
            gen_server:reply(From, {error, {send_failed, Err}}),
            {stop, normal, State_1}
    end;

send_req_1(From, Url, Headers, Method, Body, Options, Timeout, 
           #state{proxy_tunnel_setup = in_progress,
                  tunnel_setup_queue = Q} = State) ->
    do_trace("Queued SSL request awaiting tunnel setup: ~n"
             "URL     : ~s~n"
             "Method  : ~p~n"
             "Headers : ~p~n", [Url, Method, Headers]),
    {noreply, State#state{tunnel_setup_queue = [{From, Url, Headers, Method, Body, Options, Timeout} | Q]}};

send_req_1(From,
           #url{abspath = AbsPath,
                path    = RelPath} = Url,
           Headers, Method, Body, Options, Timeout,
           #state{status    = Status,
                  socket    = Socket} = State) ->
    cancel_timer(State#state.inactivity_timer_ref, {eat_message, timeout}),
    ReqId = make_req_id(),
    Resp_format = get_value(response_format, Options, list),
    Caller_socket_options = get_value(socket_options, Options, []),
    {StreamTo, Caller_controls_socket} =
        case get_value(stream_to, Options, undefined) of
            {Caller, once} when is_pid(Caller) or
                                is_atom(Caller) ->
                Async_pid_rec = {{req_id_pid, ReqId}, self()},
                true = ets:insert(ibrowse_stream, Async_pid_rec), 
                {Caller, true};
            undefined ->
                {undefined, false};
            Caller when is_pid(Caller) or
                        is_atom(Caller) ->
                {Caller, false};
            Stream_to_inv ->
                exit({invalid_option, {stream_to, Stream_to_inv}})
        end,
    SaveResponseToFile = get_value(save_response_to_file, Options, false),
    NewReq = #request{url                    = Url,
                      method                 = Method,
                      stream_to              = StreamTo,
                      caller_controls_socket = Caller_controls_socket,
                      caller_socket_options  = Caller_socket_options,
                      options                = Options,
                      req_id                 = ReqId,
                      save_response_to_file  = SaveResponseToFile,
                      stream_chunk_size      = get_stream_chunk_size(Options),
                      response_format        = Resp_format,
                      from                   = From,
                      preserve_chunked_encoding = get_value(preserve_chunked_encoding, Options, false)
                     },
    State_1 = State#state{reqs=queue:in(NewReq, State#state.reqs)},
    Headers_1 = maybe_modify_headers(Url, Method, Options, Headers, State_1),
    {Req, Body_1} = make_request(Method,
                                 Headers_1,
                                 AbsPath, RelPath, Body, Options, State_1,
                                 ReqId),
    trace_request(Req),
    do_setopts(Socket, Caller_socket_options, State_1),
    TE = is_chunked_encoding_specified(Options),
    case do_send(Req, State_1) of
        ok ->
            case do_send_body(Body_1, State_1, TE) of
                ok ->
                    trace_request_body(Body_1),
                    State_2 = inc_pipeline_counter(State_1),
                    active_once(State_2),
                    Ref = case Timeout of
                              infinity ->
                                  undefined;
                              _ ->
                                  erlang:send_after(Timeout, self(), {req_timedout, From})
                          end,
                    State_3 = case Status of
                                  idle ->
                                      State_2#state{status     = get_header,
                                                    cur_req    = NewReq,
                                                    send_timer = Ref};
                                  _ ->
                                      State_2#state{send_timer = Ref}
                              end,
                    case StreamTo of
                        undefined ->
                            ok;
                        _ ->
                            gen_server:reply(From, {ibrowse_req_id, ReqId})
                    end,
                    State_4 = set_inac_timer(State_3),
                    {noreply, State_4};
                Err ->
                    shutting_down(State_1),
                    do_trace("Send failed... Reason: ~p~n", [Err]),
                    gen_server:reply(From, {error, {send_failed, Err}}),
                    {stop, normal, State_1}
            end;
        Err ->
            shutting_down(State_1),
            do_trace("Send failed... Reason: ~p~n", [Err]),
            gen_server:reply(From, {error, {send_failed, Err}}),
            {stop, normal, State_1}
    end.

maybe_modify_headers(#url{}, connect, _, Headers, State) ->
    add_proxy_auth_headers(State, Headers);
maybe_modify_headers(#url{host = Host, port = Port} = Url,
                     _Method,
                     Options, Headers, State) ->
    case get_value(headers_as_is, Options, false) of
        false ->
            Headers_1 = add_auth_headers(Url, Options, Headers, State),
            HostHeaderValue = case lists:keysearch(host_header, 1, Options) of
                                  false ->
                                      case Port of
                                          80 -> Host;
                                          443 -> Host;
                                          _ -> [Host, ":", integer_to_list(Port)]
                                      end;
                                  {value, {_, Host_h_val}} ->
                                      Host_h_val
                              end,
            [{"Host", HostHeaderValue} | Headers_1];
        true ->
            Headers
    end.

add_auth_headers(#url{username = User,
                      password = UPw},
                 Options,
                 Headers,
                 State) ->
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
    add_proxy_auth_headers(State, Headers_1).

add_proxy_auth_headers(#state{use_proxy = false}, Headers) ->
    Headers;
add_proxy_auth_headers(#state{proxy_auth_digest = []}, Headers) ->
    Headers;
add_proxy_auth_headers(#state{proxy_auth_digest = Auth_digest}, Headers) ->
    [{"Proxy-Authorization", ["Basic ", Auth_digest]} | Headers].

http_auth_digest([], []) ->
    [];
http_auth_digest(Username, Password) ->
    ibrowse_lib:encode_base64(Username ++ [$: | Password]).

make_request(Method, Headers, AbsPath, RelPath, Body, Options,
             #state{use_proxy = UseProxy, is_ssl = Is_ssl}, ReqId) ->
    HttpVsn = http_vsn_string(get_value(http_vsn, Options, {1,1})),
    Fun1 = fun({X, Y}) when is_atom(X) ->
                   {to_lower(atom_to_list(X)), X, Y};
              ({X, Y}) when is_list(X) ->
                   {to_lower(X), X, Y}
           end,
    Headers_0 = [Fun1(X) || X <- Headers],
    Headers_1 =
        case lists:keysearch("content-length", 1, Headers_0) of
            false when (Body =:= [] orelse Body =:= <<>>) andalso
                       (Method =:= post orelse Method =:= put) ->
                [{"content-length", "Content-Length", "0"} | Headers_0];
            false when is_binary(Body) orelse is_list(Body) ->
                [{"content-length", "Content-Length", integer_to_list(iolist_size(Body))} | Headers_0];
            _ ->
                %% Content-Length is already specified or Body is a
                %% function or function/state pair
                Headers_0
        end,
    {Headers_2, Body_1} =
        case is_chunked_encoding_specified(Options) of
            false ->
                {[{Y, Z} || {_, Y, Z} <- Headers_1], Body};
            true ->
                Chunk_size_1 = case get_value(transfer_encoding, Options) of
                                  chunked ->
                                      5120;
                                  {chunked, Chunk_size} ->
                                      Chunk_size
                              end,
                {[{Y, Z} || {X, Y, Z} <- Headers_1,
                            X /= "content-length"] ++
                 [{"Transfer-Encoding", "chunked"}],
                 chunk_request_body(Body, Chunk_size_1)}
        end,
    Headers_3 = case lists:member({include_ibrowse_req_id, true}, Options) of
                    true ->
                        [{"x-ibrowse-request-id", io_lib:format("~1000.p",[ReqId])} | Headers_2];
                    false ->
                        Headers_2
                end,
    Headers_4 = cons_headers(Headers_3),
    Uri = case get_value(use_absolute_uri, Options, false) or UseProxy of
              true ->
                  case Is_ssl of
                      true ->
                          RelPath;
                      false ->
                          AbsPath
                  end;
              false ->
                  RelPath
          end,
    {[method(Method), " ", Uri, " ", HttpVsn, crnl(), Headers_4, crnl()], Body_1}.

is_chunked_encoding_specified(Options) ->
    case get_value(transfer_encoding, Options, false) of
        false ->
            false;
        {chunked, _} -> 
            true;
        chunked ->
            true
    end.

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
encode_headers([{Name,Val} | T], Acc) when is_list(Name) ->
    encode_headers(T, [[Name, ": ", fmt_val(Val), crnl()] | Acc]);
encode_headers([{Name,Val} | T], Acc) when is_atom(Name) ->
    encode_headers(T, [[atom_to_list(Name), ": ", fmt_val(Val), crnl()] | Acc]);
encode_headers([], Acc) ->
    lists:reverse(Acc).

chunk_request_body(Body, _ChunkSize) when is_tuple(Body) orelse
                                          is_function(Body) ->
    Body;
chunk_request_body(Body, ChunkSize) ->
    chunk_request_body(Body, ChunkSize, []).

chunk_request_body(Body, _ChunkSize, Acc) when Body == <<>>; Body == [] ->
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk | Acc]);
chunk_request_body(Body, ChunkSize, Acc) when is_binary(Body),
                                              size(Body) >= ChunkSize ->
    <<ChunkBody:ChunkSize/binary, Rest/binary>> = Body,
    Chunk = [?dec2hex(ChunkSize),"\r\n",
             ChunkBody, "\r\n"],
    chunk_request_body(Rest, ChunkSize, [Chunk | Acc]);
chunk_request_body(Body, _ChunkSize, Acc) when is_binary(Body) ->
    BodySize = size(Body),
    Chunk = [?dec2hex(BodySize),"\r\n",
             Body, "\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk, Chunk | Acc]);
chunk_request_body(Body, ChunkSize, Acc) when length(Body) >= ChunkSize ->
    {ChunkBody, Rest} = split_list_at(Body, ChunkSize),
    Chunk = [?dec2hex(ChunkSize),"\r\n",
             ChunkBody, "\r\n"],
    chunk_request_body(Rest, ChunkSize, [Chunk | Acc]);
chunk_request_body(Body, _ChunkSize, Acc) when is_list(Body) ->
    BodySize = length(Body),
    Chunk = [?dec2hex(BodySize),"\r\n",
             Body, "\r\n"],
    LastChunk = "0\r\n",
    lists:reverse(["\r\n", LastChunk, Chunk | Acc]).


parse_response(_Data, #state{cur_req = undefined}=State) ->
    State#state{status = idle};
parse_response(Data, #state{reply_buffer = Acc, reqs = Reqs,
                            cur_req = CurReq} = State) ->
    #request{from=From, stream_to=StreamTo, req_id=ReqId,
             method=Method, response_format = Resp_format,
             options = Options
            } = CurReq,
    MaxHeaderSize = ibrowse:get_config_value(max_headers_size, infinity),
    case scan_header(Acc, Data) of
        {yes, Headers, Data_1}  ->
            do_trace("Recvd Header Data -> ~s~n----~n", [Headers]),
            do_trace("Recvd headers~n--- Headers Begin ---~n~s~n--- Headers End ---~n~n", [Headers]),
            {HttpVsn, StatCode, Headers_1, Status_line, Raw_headers} = parse_headers(Headers),
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
            Give_raw_headers = get_value(give_raw_headers, Options, false),
            State_1 = case Give_raw_headers of
                          true ->
                              State#state{recvd_headers=Headers_1, status=get_body,
                                          reply_buffer = <<>>,
                                          status_line = Status_line,
                                          raw_headers = Raw_headers,
                                          http_status_code=StatCode, is_closing=IsClosing};
                          false ->
                              State#state{recvd_headers=Headers_1, status=get_body,
                                          reply_buffer = <<>>,
                                          http_status_code=StatCode, is_closing=IsClosing}
                      end,
            put(conn_close, ConnClose),
            TransferEncoding = to_lower(get_value("transfer-encoding", LCHeaders, "false")),
            case get_value("content-length", LCHeaders, undefined) of
                _ when Method == connect, 
                       hd(StatCode) == $2 ->
                    cancel_timer(State#state.send_timer),
                    {_, Reqs_1} = queue:out(Reqs),
                    upgrade_to_ssl(set_cur_request(State#state{reqs = Reqs_1,
                                                               recvd_headers = [],
                                                               status = idle
                                                              }));
                _ when Method == connect ->
                    {_, Reqs_1} = queue:out(Reqs),
                    do_error_reply(State#state{reqs = Reqs_1},
                                   {error, proxy_tunnel_failed}),
                    {error, proxy_tunnel_failed};
                _ when Method == head ->
                    {_, Reqs_1} = queue:out(Reqs),
                    send_async_headers(ReqId, StreamTo, Give_raw_headers, State_1),
                    State_1_1 = do_reply(State_1, From, StreamTo, ReqId, Resp_format,
                                         {ok, StatCode, Headers_1, []}),
                    cancel_timer(State_1_1#state.send_timer, {eat_message, {req_timedout, From}}),
                    State_2 = reset_state(State_1_1),
                    State_3 = set_cur_request(State_2#state{reqs = Reqs_1}),
                    parse_response(Data_1, State_3);
                _ when hd(StatCode) =:= $1 ->
                    %% No message body is expected. Server may send
                    %% one or more 1XX responses before a proper
                    %% response.
                    send_async_headers(ReqId, StreamTo, Give_raw_headers, State_1),
                    do_trace("Recvd a status code of ~p. Ignoring and waiting for a proper response~n", [StatCode]),
                    parse_response(Data_1, State_1#state{recvd_headers = [],
                                                         status = get_header});
                _ when StatCode =:= "204";
                       StatCode =:= "304" ->
                    %% No message body is expected for these Status Codes.
                    %% RFC2616 - Sec 4.4
                    {_, Reqs_1} = queue:out(Reqs),
                    send_async_headers(ReqId, StreamTo, Give_raw_headers, State_1),
                    State_1_1 = do_reply(State_1, From, StreamTo, ReqId, Resp_format,
                                         {ok, StatCode, Headers_1, []}),
                    cancel_timer(State_1_1#state.send_timer, {eat_message, {req_timedout, From}}),
                    State_2 = reset_state(State_1_1),
                    State_3 = set_cur_request(State_2#state{reqs = Reqs_1}),
                    parse_response(Data_1, State_3);
                _ when TransferEncoding =:= "chunked" ->
                    do_trace("Chunked encoding detected...~n",[]),
                    send_async_headers(ReqId, StreamTo, Give_raw_headers, State_1),
                    case parse_11_response(Data_1, State_1#state{transfer_encoding=chunked,
                                                                 chunk_size=chunk_start,
                                                                 reply_buffer = <<>>}) of
                        {error, Reason} ->
                            fail_pipelined_requests(State_1,
                                                    {error, {Reason,
                                                             {stat_code, StatCode}, Headers_1}}),
                            {error, Reason};
                        State_2 ->
                            State_2
                    end;
                undefined when HttpVsn =:= "HTTP/1.0";
                ConnClose =:= "close" ->
                    send_async_headers(ReqId, StreamTo, Give_raw_headers, State_1),
                    State_1#state{reply_buffer = Data_1};
                undefined ->
                    fail_pipelined_requests(State_1,
                                            {error, {content_length_undefined,
                                                     {stat_code, StatCode}, Headers}}),
                    {error, content_length_undefined};
                V ->
                    case catch list_to_integer(V) of
                        V_1 when is_integer(V_1), V_1 >= 0 ->
                            send_async_headers(ReqId, StreamTo, Give_raw_headers, State_1),
                            do_trace("Recvd Content-Length of ~p~n", [V_1]),
                            State_2 = State_1#state{rep_buf_size=0,
                                                    reply_buffer = <<>>,
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
            State#state{reply_buffer = Acc_1};
        {no, Acc_1} when size(Acc_1) < MaxHeaderSize ->
            State#state{reply_buffer = Acc_1};
        {no, _Acc_1} ->
            fail_pipelined_requests(State, {error, max_headers_size_exceeded}),
            {error, max_headers_size_exceeded}
    end.

upgrade_to_ssl(#state{socket = Socket, 
                      connect_timeout = Conn_timeout,
                      ssl_options = Ssl_options,
                      tunnel_setup_queue = Q} = State) ->
    case ssl:connect(Socket, Ssl_options, Conn_timeout) of
        {ok, Ssl_socket} ->
            do_trace("Upgraded to SSL socket!!~n", []),
            State_1 = State#state{socket = Ssl_socket,
                                  proxy_tunnel_setup = done},
            send_queued_requests(lists:reverse(Q), State_1);
        Err ->
            do_trace("Upgrade to SSL socket failed. Reson: ~p~n", [Err]),
            do_error_reply(State, {error, {send_failed, Err}}),
            {error, send_failed}
    end.

send_queued_requests([], State) ->
    do_trace("Sent all queued requests via SSL connection~n", []),
    State#state{tunnel_setup_queue = []};
send_queued_requests([{From, Url, Headers, Method, Body, Options, Timeout} | Q],
                     State) ->
    case send_req_1(From, Url, Headers, Method, Body, Options, Timeout, State) of
        {noreply, State_1} ->
            send_queued_requests(Q, State_1);
        Err ->
            do_trace("Error sending queued SSL request: ~n"
                     "URL     : ~s~n"
                     "Method  : ~p~n"
                     "Headers : ~p~n", [Url, Method, Headers]),
            do_error_reply(State, {error, {send_failed, Err}}),
            {error, send_failed}
    end.

is_connection_closing("HTTP/0.9", _)       -> true;
is_connection_closing(_, "close")          -> true;
is_connection_closing("HTTP/1.0", "false") -> true;
is_connection_closing(_, _)                -> false.

%% This clause determines the chunk size when given data from the beginning of the chunk
parse_11_response(DataRecvd,
                  #state{transfer_encoding = chunked, 
                         chunk_size        = chunk_start,
                         chunk_size_buffer = Chunk_sz_buf
                        } = State) ->
    case scan_crlf(Chunk_sz_buf, DataRecvd) of
        {yes, ChunkHeader, Data_1} ->
            State_1 = maybe_accumulate_ce_data(State, <<ChunkHeader/binary, $\r, $\n>>),
            ChunkSize = parse_chunk_header(ChunkHeader),
            %%
            %% Do we have to preserve the chunk encoding when
            %% streaming? NO. This should be transparent to the client
            %% process. Chunked encoding was only introduced to make
            %% it efficient for the server.
            %%
            RemLen = size(Data_1),
            do_trace("Determined chunk size: ~p. Already recvd: ~p~n",
                     [ChunkSize, RemLen]),
            parse_11_response(Data_1, State_1#state{chunk_size_buffer = <<>>,
                                                    deleted_crlf = true,
                                                    recvd_chunk_size = 0,
                                                    chunk_size = ChunkSize});
        {no, Data_1} ->
            State#state{chunk_size_buffer = Data_1}
    end;

%% This clause is to remove the CRLF between two chunks
%%
parse_11_response(DataRecvd,
                  #state{transfer_encoding = chunked, 
                         chunk_size = tbd,
                         chunk_size_buffer = Buf
                        } = State) ->
    case scan_crlf(Buf, DataRecvd) of
        {yes, _, NextChunk} ->
            State_1 = maybe_accumulate_ce_data(State, <<$\r, $\n>>),
            State_2 = State_1#state{chunk_size = chunk_start,
                                    chunk_size_buffer = <<>>,
                                    deleted_crlf = true},
            parse_11_response(NextChunk, State_2);
        {no, Data_1} ->
            State#state{chunk_size_buffer = Data_1}
    end;

%% This clause deals with the end of a chunked transfer. ibrowse does
%% not support Trailers in the Chunked Transfer encoding. Any trailer
%% received is silently discarded.
parse_11_response(DataRecvd,
                  #state{transfer_encoding = chunked, chunk_size = 0, 
                         cur_req           = CurReq,
                         deleted_crlf      = DelCrlf,
                         chunk_size_buffer = Trailer,
                         reqs              = Reqs} = State) ->
    do_trace("Detected end of chunked transfer...~n", []),
    DataRecvd_1 = case DelCrlf of
                      false ->
                          DataRecvd;
                      true ->
                          <<$\r, $\n, DataRecvd/binary>>
                  end,
    case scan_header(Trailer, DataRecvd_1) of
        {yes, TEHeaders, Rem} ->
            {_, Reqs_1} = queue:out(Reqs),
            State_1 = maybe_accumulate_ce_data(State, <<TEHeaders/binary, $\r, $\n>>),
            State_2 = handle_response(CurReq,
                                      State_1#state{reqs = Reqs_1}),
            parse_response(Rem, reset_state(State_2));
        {no, Rem} ->
            accumulate_response(<<>>, State#state{chunk_size_buffer = Rem, deleted_crlf = false})
    end;

%% This clause extracts a chunk, given the size.
parse_11_response(DataRecvd,
                  #state{transfer_encoding = chunked,
                         chunk_size = CSz,
                         recvd_chunk_size = Recvd_csz,
                         rep_buf_size = RepBufSz} = State) ->
    NeedBytes = CSz - Recvd_csz,
    DataLen = size(DataRecvd),
    do_trace("Recvd more data: size: ~p. NeedBytes: ~p~n", [DataLen, NeedBytes]),
    case DataLen >= NeedBytes of
        true ->
            {RemChunk, RemData} = split_binary(DataRecvd, NeedBytes),
            do_trace("Recvd another chunk...~p~n", [RemChunk]),
            do_trace("RemData -> ~p~n", [RemData]),
            case accumulate_response(RemChunk, State) of
                {error, Reason} ->
                    do_trace("Error accumulating response --> ~p~n", [Reason]),
                    {error, Reason};
                #state{} = State_1 ->
                    State_2 = State_1#state{chunk_size=tbd},
                    parse_11_response(RemData, State_2)
            end;
        false ->
            accumulate_response(DataRecvd,
                                State#state{rep_buf_size = RepBufSz + DataLen,
                                            recvd_chunk_size = Recvd_csz + DataLen})
    end;

%% This clause to extract the body when Content-Length is specified
parse_11_response(DataRecvd,
                  #state{content_length=CL, rep_buf_size=RepBufSz,
                         reqs=Reqs}=State) ->
    NeedBytes = CL - RepBufSz,
    DataLen = size(DataRecvd),
    case DataLen >= NeedBytes of
        true ->
            {RemBody, Rem} = split_binary(DataRecvd, NeedBytes),
            {_, Reqs_1} = queue:out(Reqs),
            State_1 = accumulate_response(RemBody, State),
            State_2 = handle_response(State_1#state.cur_req, State_1#state{reqs=Reqs_1}),
            State_3 = reset_state(State_2),
            parse_response(Rem, State_3);
        false ->
            accumulate_response(DataRecvd, State#state{rep_buf_size = (RepBufSz+DataLen)})
    end.

maybe_accumulate_ce_data(#state{cur_req = #request{preserve_chunked_encoding = false}} = State, _) ->
    State;
maybe_accumulate_ce_data(State, Data) ->
    accumulate_response(Data, State).

handle_response(#request{from=From, stream_to=StreamTo, req_id=ReqId,
                         response_format = Resp_format,
                         save_response_to_file = SaveResponseToFile,
                         tmp_file_name = TmpFilename,
                         tmp_file_fd = Fd,
                         options       = Options
                        },
                #state{http_status_code = SCode,
                       status_line   = Status_line,
                       raw_headers   = Raw_headers,
                       send_timer    = ReqTimer,
                       reply_buffer  = RepBuf,
                       recvd_headers = RespHeaders}=State) when SaveResponseToFile /= false ->
    Body = RepBuf,
    case Fd of
        undefined -> 
            ok;
        _ -> 
            ok = file:close(Fd)
    end,
    ResponseBody = case TmpFilename of
                       undefined ->
                           Body;
                       _ ->
                           {file, TmpFilename}
                   end,
    {Resp_headers_1, Raw_headers_1} = maybe_add_custom_headers(RespHeaders, Raw_headers, Options),
    Reply = case get_value(give_raw_headers, Options, false) of
                true ->
                    {ok, Status_line, Raw_headers_1, ResponseBody};
                false ->
                    {ok, SCode, Resp_headers_1, ResponseBody}
            end,
    State_1 = do_reply(State, From, StreamTo, ReqId, Resp_format, Reply),
    cancel_timer(ReqTimer, {eat_message, {req_timedout, From}}),
    set_cur_request(State_1);
handle_response(#request{from=From, stream_to=StreamTo, req_id=ReqId,
                         response_format = Resp_format,
                         options = Options},
                #state{http_status_code = SCode,
                       status_line      = Status_line,
                       raw_headers      = Raw_headers,
                       recvd_headers    = Resp_headers,
                       reply_buffer     = RepBuf,
                       send_timer       = ReqTimer} = State) ->
    Body = RepBuf,
    {Resp_headers_1, Raw_headers_1} = maybe_add_custom_headers(Resp_headers, Raw_headers, Options),
    Reply = case get_value(give_raw_headers, Options, false) of
                true ->
                    {ok, Status_line, Raw_headers_1, Body};
                false ->
                    {ok, SCode, Resp_headers_1, Body}
            end,
    State_1 = do_reply(State, From, StreamTo, ReqId, Resp_format, Reply),
    cancel_timer(ReqTimer, {eat_message, {req_timedout, From}}),
    set_cur_request(State_1).

reset_state(State) ->
    State#state{status            = get_header,
                rep_buf_size      = 0,
                streamed_size     = 0,
                content_length    = undefined,
                reply_buffer      = <<>>,
                chunk_size_buffer = <<>>,
                recvd_headers     = [],
                status_line       = undefined,
                raw_headers       = undefined,
                deleted_crlf      = false,
                http_status_code  = undefined,
                chunk_size        = undefined,
                transfer_encoding = undefined
               }.

set_cur_request(#state{reqs = Reqs, socket = Socket} = State) ->
    case queue:to_list(Reqs) of
        [] ->
            State#state{cur_req = undefined};
        [#request{caller_controls_socket = Ccs} = NextReq | _] ->
            case Ccs of
                true ->
                    do_setopts(Socket, [{active, once}], State);
                _ ->
                    ok
            end,
            State#state{cur_req = NextReq}
    end.

parse_headers(Headers) ->
    case scan_crlf(Headers) of
        {yes, StatusLine, T} ->
            parse_headers(StatusLine, T);
        {no, StatusLine} ->
            parse_headers(StatusLine, <<>>)
    end.

parse_headers(StatusLine, Headers) ->
    Headers_1 = parse_headers_1(Headers),
    case parse_status_line(StatusLine) of
        {ok, HttpVsn, StatCode, _Msg} ->
            put(http_prot_vsn, HttpVsn),
            {HttpVsn, StatCode, Headers_1, StatusLine, Headers};
        _ -> %% A HTTP 0.9 response?
            put(http_prot_vsn, "HTTP/0.9"),
            {"HTTP/0.9", undefined, Headers, StatusLine, Headers}
    end.

% From RFC 2616
%
%    HTTP/1.1 header field values can be folded onto multiple lines if
%    the continuation line begins with a space or horizontal tab. All
%    linear white space, including folding, has the same semantics as
%    SP. A recipient MAY replace any linear white space with a single
%    SP before interpreting the field value or forwarding the message
%    downstream.
parse_headers_1(B) when is_binary(B) ->
    parse_headers_1(binary_to_list(B));
parse_headers_1(String) ->
    parse_headers_1(String, [], []).

parse_headers_1([$\n, H |T], [$\r | L], Acc) when H =:= 32;
                                                  H =:= $\t ->
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

parse_status_line(Line) when is_binary(Line) ->
    parse_status_line(binary_to_list(Line));
parse_status_line(Line) ->
    parse_status_line(Line, get_prot_vsn, [], []).
parse_status_line([32 | T], get_prot_vsn, ProtVsn, StatCode) ->
    parse_status_line(T, get_status_code, ProtVsn, StatCode);
parse_status_line([32 | T], get_status_code, ProtVsn, StatCode) ->
    {ok, lists:reverse(ProtVsn), lists:reverse(StatCode), T};
parse_status_line([], get_status_code, ProtVsn, StatCode) ->
    {ok, lists:reverse(ProtVsn), lists:reverse(StatCode), []};
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

scan_header(Bin) ->
    case get_crlf_crlf_pos(Bin, 0) of
        {yes, Pos} ->
            {Headers, <<_:4/binary, Body/binary>>} = split_binary(Bin, Pos),
            {yes, Headers, Body};
        no ->
            {no, Bin}
    end.

scan_header(Bin1, Bin2) when size(Bin1) < 4 ->
    scan_header(<<Bin1/binary, Bin2/binary>>);
scan_header(Bin1, <<>>) ->
    scan_header(Bin1);
scan_header(Bin1, Bin2) ->
    Bin1_already_scanned_size = size(Bin1) - 4,
    <<Headers_prefix:Bin1_already_scanned_size/binary, Rest/binary>> = Bin1,
    Bin_to_scan = <<Rest/binary, Bin2/binary>>,
    case get_crlf_crlf_pos(Bin_to_scan, 0) of
        {yes, Pos} ->
            {Headers_suffix, <<_:4/binary, Body/binary>>} = split_binary(Bin_to_scan, Pos),
            {yes, <<Headers_prefix/binary, Headers_suffix/binary>>, Body};
        no ->
            {no, <<Bin1/binary, Bin2/binary>>}
    end.

get_crlf_crlf_pos(<<$\r, $\n, $\r, $\n, _/binary>>, Pos) -> {yes, Pos};
get_crlf_crlf_pos(<<_, Rest/binary>>, Pos)               -> get_crlf_crlf_pos(Rest, Pos + 1);
get_crlf_crlf_pos(<<>>, _)                               -> no.

scan_crlf(Bin) ->
    case get_crlf_pos(Bin) of
        {yes, Pos} ->
            {Prefix, <<_, _, Suffix/binary>>} = split_binary(Bin, Pos),
            {yes, Prefix, Suffix};
        no ->
            {no, Bin}
    end.

scan_crlf(<<>>, Bin2) ->
    scan_crlf(Bin2);
scan_crlf(Bin1, Bin2) when size(Bin1) < 2 ->
    scan_crlf(<<Bin1/binary, Bin2/binary>>);
scan_crlf(Bin1, Bin2) ->
    scan_crlf_1(size(Bin1) - 2, Bin1, Bin2).

scan_crlf_1(Bin1_head_size, Bin1, Bin2) ->
    <<Bin1_head:Bin1_head_size/binary, Bin1_tail/binary>> = Bin1,
    Bin3 = <<Bin1_tail/binary, Bin2/binary>>,
    case get_crlf_pos(Bin3) of
        {yes, Pos} ->
            {Prefix, <<_, _, Suffix/binary>>} = split_binary(Bin3, Pos),
            {yes, list_to_binary([Bin1_head, Prefix]), Suffix};
        no ->
            {no, list_to_binary([Bin1, Bin2])}
    end.

get_crlf_pos(Bin) ->
    get_crlf_pos(Bin, 0).

get_crlf_pos(<<$\r, $\n, _/binary>>, Pos) -> {yes, Pos};
get_crlf_pos(<<_, Rest/binary>>, Pos)     -> get_crlf_pos(Rest, Pos + 1);
get_crlf_pos(<<>>, _)                     -> no.

fmt_val(L) when is_list(L)    -> L;
fmt_val(I) when is_integer(I) -> integer_to_list(I);
fmt_val(A) when is_atom(A)    -> atom_to_list(A);
fmt_val(Term)                 -> io_lib:format("~p", [Term]).

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
method(copy)      -> "COPY";
method(connect)   -> "CONNECT".

%% From RFC 2616
%%
% The chunked encoding modifies the body of a message in order to
% transfer it as a series of chunks, each with its own size indicator,
% followed by an OPTIONAL trailer containing entity-header
% fields. This allows dynamically produced content to be transferred
% along with the information necessary for the recipient to verify
% that it has received the full message.
%       Chunked-Body =  *chunk
%                       last-chunk
%                       trailer
%                       CRLF
%       chunk = chunk-size [ chunk-extension ] CRLF
%               chunk-data CRLF
%       chunk-size = 1*HEX
%       last-chunk = 1*("0") [ chunk-extension ] CRLF
%       chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
%       chunk-ext-name = token
%       chunk-ext-val = token | quoted-string
%       chunk-data = chunk-size(OCTET)
%       trailer = *(entity-header CRLF)
% The chunk-size field is a string of hex digits indicating the size
% of the chunk. The chunked encoding is ended by any chunk whose size
% is zero, followed by the trailer, which is terminated by an empty
% line.
%%
%% The parsing implemented here discards all chunk extensions. It also
%% strips trailing spaces from the chunk size fields as Apache 1.3.27 was
%% sending them.
parse_chunk_header(ChunkHeader) ->
    parse_chunk_header(ChunkHeader, []).

parse_chunk_header(<<$;, _/binary>>, Acc) ->
    hexlist_to_integer(lists:reverse(Acc));
parse_chunk_header(<<H, T/binary>>, Acc) ->
    case is_whitespace(H) of
        true ->
            parse_chunk_header(T, Acc);
        false ->
            parse_chunk_header(T, [H | Acc])
    end;
parse_chunk_header(<<>>, Acc) ->
    hexlist_to_integer(lists:reverse(Acc)).

is_whitespace($\s)  -> true;
is_whitespace($\r) -> true;
is_whitespace($\n) -> true;
is_whitespace($\t) -> true;
is_whitespace(_)   -> false.

send_async_headers(_ReqId, undefined, _, _State) ->
    ok;
send_async_headers(ReqId, StreamTo, Give_raw_headers, 
                   #state{status_line = Status_line, raw_headers = Raw_headers, 
                          recvd_headers = Headers, http_status_code = StatCode,
                          cur_req = #request{options = Opts}
                         }) ->
    {Headers_1, Raw_headers_1} = maybe_add_custom_headers(Headers, Raw_headers, Opts),
    case Give_raw_headers of
        false ->
            catch StreamTo ! {ibrowse_async_headers, ReqId, StatCode, Headers_1};
        true ->
            catch StreamTo ! {ibrowse_async_headers, ReqId, Status_line, Raw_headers_1}
    end.

maybe_add_custom_headers(Headers, Raw_headers, Opts) ->
    Custom_headers = get_value(add_custom_headers, Opts, []),
    Headers_1 = Headers ++ Custom_headers,
    Raw_headers_1 = case Custom_headers of
                        [_ | _] when is_binary(Raw_headers) ->
                            Custom_headers_bin = list_to_binary(string:join([[X, $:, Y] || {X, Y} <- Custom_headers], "\r\n")),
                            <<Raw_headers/binary, "\r\n", Custom_headers_bin/binary>>;
                        _ ->
                            Raw_headers
                    end,
    {Headers_1, Raw_headers_1}.

format_response_data(Resp_format, Body) ->
    case Resp_format of
        list when is_list(Body) ->
            flatten(Body);
        list when is_binary(Body) ->
            binary_to_list(Body);
        binary when is_list(Body) ->
            list_to_binary(Body);
        _ ->
            %% This is to cater for sending messages such as
            %% {chunk_start, _}, chunk_end etc
            Body
    end.

do_reply(State, From, undefined, _, Resp_format, {ok, St_code, Headers, Body}) ->
    Msg_1 = {ok, St_code, Headers, format_response_data(Resp_format, Body)},
    gen_server:reply(From, Msg_1),
    dec_pipeline_counter(State);
do_reply(State, From, undefined, _, _, Msg) ->
    gen_server:reply(From, Msg),
    dec_pipeline_counter(State);
do_reply(#state{prev_req_id = Prev_req_id} = State,
         _From, StreamTo, ReqId, Resp_format, {ok, _, _, Body}) ->
    State_1 = dec_pipeline_counter(State),
    case Body of
        [] ->
            ok;
        _ ->
            Body_1 = format_response_data(Resp_format, Body),
            catch StreamTo ! {ibrowse_async_response, ReqId, Body_1}
    end,
    catch StreamTo ! {ibrowse_async_response_end, ReqId},
    %% We don't want to delete the Req-id to Pid mapping straightaway
    %% as the client may send a stream_next message just while we are
    %% sending back this ibrowse_async_response_end message. If we
    %% deleted this mapping straightaway, the caller will see a
    %% {error, unknown_req_id} when it calls ibrowse:stream_next/1. To
    %% get around this, we store the req id, and clear it after the
    %% next request. If there are wierd combinations of stream,
    %% stream_once and sync requests on the same connection, it will
    %% take a while for the req_id-pid mapping to get cleared, but it
    %% should do no harm.
    ets:delete(ibrowse_stream, {req_id_pid, Prev_req_id}),
    State_1#state{prev_req_id = ReqId};
do_reply(State, _From, StreamTo, ReqId, Resp_format, Msg) ->
    State_1 = dec_pipeline_counter(State),
    Msg_1 = format_response_data(Resp_format, Msg),
    catch StreamTo ! {ibrowse_async_response, ReqId, Msg_1},
    State_1.

do_interim_reply(undefined, _, _ReqId, _Msg) ->
    ok;
do_interim_reply(StreamTo, Response_format, ReqId, Msg) ->
    Msg_1 = format_response_data(Response_format, Msg),
    catch StreamTo ! {ibrowse_async_response, ReqId, Msg_1}.

do_error_reply(#state{reqs = Reqs, tunnel_setup_queue = Tun_q} = State, Err) ->
    ReqList = queue:to_list(Reqs),
    lists:foreach(fun(#request{from=From, stream_to=StreamTo, req_id=ReqId,
                               response_format = Resp_format}) ->
                          ets:delete(ibrowse_stream, {req_id_pid, ReqId}),
                          do_reply(State, From, StreamTo, ReqId, Resp_format, {error, Err})
                  end, ReqList),
    lists:foreach(
      fun({From, _Url, _Headers, _Method, _Body, _Options, _Timeout}) ->
              do_reply(State, From, undefined, undefined, undefined, Err)
      end, Tun_q).

fail_pipelined_requests(#state{reqs = Reqs, cur_req = CurReq} = State, Reply) ->
    {_, Reqs_1} = queue:out(Reqs),
    #request{from=From, stream_to=StreamTo, req_id=ReqId,
             response_format = Resp_format} = CurReq,
    State_1 = do_reply(State, From, StreamTo, ReqId, Resp_format, Reply),
    do_error_reply(State_1#state{reqs = Reqs_1}, previous_request_failed).

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
cancel_timer(Ref)       -> _ = erlang:cancel_timer(Ref),
                           ok.

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

flatten([H | _] = L) when is_integer(H) ->
    L;
flatten([H | _] = L) when is_list(H) ->
    lists:flatten(L);
flatten([]) ->
    [].

get_stream_chunk_size(Options) ->
    case lists:keysearch(stream_chunk_size, 1, Options) of
        {value, {_, V}} when V > 0 ->
            V;
        _ ->
            ?DEFAULT_STREAM_CHUNK_SIZE
    end.

set_inac_timer(State) ->
    cancel_timer(State#state.inactivity_timer_ref),
    set_inac_timer(State#state{inactivity_timer_ref = undefined},
                   get_inac_timeout(State)).

set_inac_timer(State, Timeout) when is_integer(Timeout) ->
    Ref = erlang:send_after(Timeout, self(), timeout),
    State#state{inactivity_timer_ref = Ref};
set_inac_timer(State, _) ->
    State.

get_inac_timeout(#state{cur_req = #request{options = Opts}}) -> 
    get_value(inactivity_timeout, Opts, infinity);
get_inac_timeout(#state{cur_req = undefined}) ->
    case ibrowse:get_config_value(inactivity_timeout, undefined) of
        Val when is_integer(Val) ->
            Val;
        _ ->
            case application:get_env(ibrowse, inactivity_timeout) of
                {ok, Val} when is_integer(Val), Val > 0 ->
                    Val;
                _ ->
                    10000
            end
    end.

trace_request(Req) ->
    case get(my_trace_flag) of
        true ->
            %%Avoid the binary operations if trace is not on...
            NReq = to_binary(Req),
            do_trace("Sending request: ~n"
                     "--- Request Begin ---~n~s~n"
                     "--- Request End ---~n", [NReq]);
        _ -> ok
    end.

trace_request_body(Body) ->
    case get(my_trace_flag) of
        true ->
            %%Avoid the binary operations if trace is not on...
            NBody = to_binary(Body),
            case size(NBody) > 1024 of
                true ->
                    ok;
                false ->
                    do_trace("Sending request body: ~n"
                             "--- Request Body Begin ---~n~s~n"
                             "--- Request Body End ---~n", [NBody])
            end;
        false ->
            ok
    end.

to_binary(X) when is_list(X)   -> list_to_binary(X); 
to_binary(X) when is_binary(X) -> X.
