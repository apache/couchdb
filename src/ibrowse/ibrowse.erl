%%%-------------------------------------------------------------------
%%% File    : ibrowse.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : Load balancer process for HTTP client connections.
%%%
%%% Created : 11 Oct 2003 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%%-------------------------------------------------------------------
%% @author Chandrashekhar Mullaparthi <chandrashekhar dot mullaparthi at gmail dot com>
%% @copyright 2005-2008 Chandrashekhar Mullaparthi
%% @version 1.4
%% @doc The ibrowse application implements an HTTP 1.1 client. This
%% module implements the API of the HTTP client. There is one named
%% process called 'ibrowse' which assists in load balancing and maintaining configuration. There is one load balancing process per unique webserver. There is
%% one process to handle one TCP connection to a webserver
%% (implemented in the module ibrowse_http_client). Multiple connections to a
%% webserver are setup based on the settings for each webserver. The
%% ibrowse process also determines which connection to pipeline a
%% certain request on.  The functions to call are send_req/3,
%% send_req/4, send_req/5, send_req/6.
%%
%% <p>Here are a few sample invocations.</p>
%%
%% <code>
%% ibrowse:send_req("http://intranet/messenger/", [], get). 
%% <br/><br/>
%% 
%% ibrowse:send_req("http://www.google.com/", [], get, [], 
%% 		 [{proxy_user, "XXXXX"},
%% 		  {proxy_password, "XXXXX"},
%% 		  {proxy_host, "proxy"},
%% 		  {proxy_port, 8080}], 1000). 
%% <br/><br/>
%%
%%ibrowse:send_req("http://www.erlang.org/download/otp_src_R10B-3.tar.gz", [], get, [],
%% 		 [{proxy_user, "XXXXX"},
%% 		  {proxy_password, "XXXXX"},
%% 		  {proxy_host, "proxy"},
%% 		  {proxy_port, 8080},
%% 		  {save_response_to_file, true}], 1000).
%% <br/><br/>
%%
%% ibrowse:send_req("http://www.erlang.org", [], head).
%%
%% <br/><br/>
%% ibrowse:send_req("http://www.sun.com", [], options).
%%
%% <br/><br/>
%% ibrowse:send_req("http://www.bbc.co.uk", [], trace).
%%
%% <br/><br/>
%% ibrowse:send_req("http://www.google.com", [], get, [], 
%%                   [{stream_to, self()}]).
%% </code>
%%
%% <p>A driver exists which implements URL encoding in C, but the
%% speed achieved using only erlang has been good enough, so the
%% driver isn't actually used.</p>

-module(ibrowse).
-vsn('$Id: ibrowse.erl,v 1.7 2008/05/21 15:28:11 chandrusf Exp $ ').

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% API interface
-export([
	 rescan_config/0,
	 rescan_config/1,
	 get_config_value/1,
	 get_config_value/2,
	 spawn_worker_process/2,
	 spawn_link_worker_process/2,
	 stop_worker_process/1,
	 send_req/3,
	 send_req/4,
	 send_req/5,
	 send_req/6,
	 send_req_direct/4,
	 send_req_direct/5,
	 send_req_direct/6,
	 send_req_direct/7,
	 set_max_sessions/3,
	 set_max_pipeline_size/3,
	 set_dest/3,
	 trace_on/0,
	 trace_off/0,
	 trace_on/2,
	 trace_off/2,
	 show_dest_status/2
	]).

-ifdef(debug).
-compile(export_all).
-endif.

-import(ibrowse_lib, [
		      parse_url/1,
		      printable_date/0,
		      get_value/2,
		      get_value/3,
		      do_trace/2
		     ]).
		      
-record(state, {trace = false}).

-include("ibrowse.hrl").

-define(DEF_MAX_SESSIONS,10).
-define(DEF_MAX_PIPELINE_SIZE,10).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @doc Starts the ibrowse process linked to the calling process. Usually invoked by the supervisor ibrowse_sup
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts the ibrowse process without linking. Useful when testing using the shell
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], [{debug, []}]).

%% @doc Stop the ibrowse process. Useful when testing using the shell.
stop() ->
    catch gen_server:call(ibrowse, stop).

%% @doc This is the basic function to send a HTTP request.
%% The Status return value indicates the HTTP status code returned by the webserver
%% @spec send_req(Url::string(), Headers::headerList(), Method::method()) -> response()
%% headerList() = [{header(), value()}]
%% header() = atom() | string()
%% value() = term()
%% method() = get | post | head | options | put | delete | trace | mkcol | propfind | proppatch | lock | unlock | move | copy
%% Status = string()
%% ResponseHeaders = [respHeader()]
%% respHeader() = {headerName(), headerValue()}
%% headerName() = string()
%% headerValue() = string()
%% response() = {ok, Status, ResponseHeaders, ResponseBody} | {error, Reason}
%% ResponseBody = string() | {file, Filename}
%% Reason = term()
send_req(Url, Headers, Method) ->
    send_req(Url, Headers, Method, [], []).

%% @doc Same as send_req/3. 
%% If a list is specified for the body it has to be a flat list. The body can also be a fun/0 or a fun/1. <br/>
%% If fun/0, the connection handling process will repeatdely call the fun until it returns an error or eof. <pre>Fun() = {ok, Data} | eof</pre><br/>
%% If fun/1, the connection handling process will repeatedly call the fun with the supplied state until it returns an error or eof. <pre>Fun(State) = {ok, Data} | {ok, Data, NewState} | eof</pre>
%% @spec send_req(Url, Headers, Method::method(), Body::body()) -> response()
%% body() = [] | string() | binary() | fun_arity_0() | {fun_arity_1(), initial_state()}
%% initial_state() = term()
send_req(Url, Headers, Method, Body) ->
    send_req(Url, Headers, Method, Body, []).

%% @doc Same as send_req/4. 
%% For a description of SSL Options, look in the ssl manpage. If the
%% HTTP Version to use is not specified, the default is 1.1.
%% <br/>
%% <p>The <code>host_header</code> is useful in the case where ibrowse is
%% connecting to a component such as <a
%% href="http://www.stunnel.org">stunnel</a> which then sets up a
%% secure connection to a webserver. In this case, the URL supplied to
%% ibrowse must have the stunnel host/port details, but that won't
%% make sense to the destination webserver. This option can then be
%% used to specify what should go in the <code>Host</code> header in
%% the request.</p>
%% <ul>
%% <li>When both the options <code>save_response_to_file</code> and <code>stream_to</code> 
%% are specified, the former takes precedence.</li>
%%
%% <li>For the <code>save_response_to_file</code> option, the response body is saved to
%% file only if the status code is in the 200-299 range. If not, the response body is returned
%% as a string.</li>
%% <li>Whenever an error occurs in the processing of a request, ibrowse will return as much
%% information as it has, such as HTTP Status Code and HTTP Headers. When this happens, the response
%% is of the form <code>{error, {Reason, {stat_code, StatusCode}, HTTP_headers}}</code></li>
%% </ul>
%% @spec send_req(Url::string(), Headers::headerList(), Method::method(), Body::body(), Options::optionList()) -> response()
%% optionList() = [option()]
%% option() = {max_sessions, integer()}        |
%%          {max_pipeline_size, integer()}     |
%%          {trace, boolean()}                 | 
%%          {is_ssl, boolean()}                |
%%          {ssl_options, [SSLOpt]}            |
%%          {pool_name, atom()}                |
%%          {proxy_host, string()}             |
%%          {proxy_port, integer()}            |
%%          {proxy_user, string()}             |
%%          {proxy_password, string()}         |
%%          {use_absolute_uri, boolean()}      |
%%          {basic_auth, {username(), password()}} |
%%          {cookie, string()}                 |
%%          {content_length, integer()}        |
%%          {content_type, string()}           |
%%          {save_response_to_file, srtf()}    |
%%          {stream_to, process()}             |
%%          {http_vsn, {MajorVsn, MinorVsn}}   |
%%          {host_header, string()}            |
%%          {transfer_encoding, {chunked, ChunkSize}}
%% 
%% process() = pid() | atom()
%% username() = string()
%% password() = string()
%% SSLOpt = term()
%% ChunkSize = integer()
%% srtf() = boolean() | filename()
%% filename() = string()
%% 
send_req(Url, Headers, Method, Body, Options) ->
    send_req(Url, Headers, Method, Body, Options, 30000).

%% @doc Same as send_req/5. 
%% All timeout values are in milliseconds.
%% @spec send_req(Url, Headers::headerList(), Method::method(), Body::body(), Options::optionList(), Timeout) -> response()
%% Timeout = integer() | infinity
send_req(Url, Headers, Method, Body, Options, Timeout) ->
    case catch parse_url(Url) of
	#url{host = Host,
	     port = Port} = Parsed_url ->
	    Lb_pid = case ets:lookup(ibrowse_lb, {Host, Port}) of
			 [] ->
			     get_lb_pid(Parsed_url);
			 [#lb_pid{pid = Lb_pid_1}] ->
			     Lb_pid_1
		     end,
	    Max_sessions = get_max_sessions(Host, Port, Options),
	    Max_pipeline_size = get_max_pipeline_size(Host, Port, Options),
	    Options_1 = merge_options(Host, Port, Options),
	    {SSLOptions, IsSSL} =
		case get_value(is_ssl, Options_1, false) of
		    false -> {[], false};
		    true -> {get_value(ssl_options, Options_1), true}
		end,
	    case ibrowse_lb:spawn_connection(Lb_pid, Parsed_url,
					     Max_sessions, 
					     Max_pipeline_size,
					     {SSLOptions, IsSSL}) of
		{ok, Conn_Pid} ->
		    do_send_req(Conn_Pid, Parsed_url, Headers,
				Method, Body, Options_1, Timeout);
		Err ->
		    Err
	    end;
	Err ->
	    {error, {url_parsing_failed, Err}}
    end.

merge_options(Host, Port, Options) ->
    Config_options = get_config_value({options, Host, Port}, []),
    lists:foldl(
      fun({Key, Val}, Acc) ->
			case lists:keysearch(Key, 1, Options) of
			    false ->
				[{Key, Val} | Acc];
			    _ ->
				Acc
			end
      end, Options, Config_options).

get_lb_pid(Url) ->
    gen_server:call(?MODULE, {get_lb_pid, Url}).

get_max_sessions(Host, Port, Options) ->
    get_value(max_sessions, Options,
	      get_config_value({max_sessions, Host, Port}, ?DEF_MAX_SESSIONS)).

get_max_pipeline_size(Host, Port, Options) ->
    get_value(max_pipeline_size, Options,
	      get_config_value({max_pipeline_size, Host, Port}, ?DEF_MAX_PIPELINE_SIZE)).

%% @doc Deprecated. Use set_max_sessions/3 and set_max_pipeline_size/3
%% for achieving the same effect.
set_dest(Host, Port, [{max_sessions, Max} | T]) ->
    set_max_sessions(Host, Port, Max),
    set_dest(Host, Port, T);
set_dest(Host, Port, [{max_pipeline_size, Max} | T]) ->
    set_max_pipeline_size(Host, Port, Max),
    set_dest(Host, Port, T);
set_dest(Host, Port, [{trace, Bool} | T]) when Bool == true; Bool == false ->
    ibrowse ! {trace, true, Host, Port},
    set_dest(Host, Port, T);
set_dest(_Host, _Port, [H | _]) ->
    exit({invalid_option, H});
set_dest(_, _, []) ->
    ok.
    
%% @doc Set the maximum number of connections allowed to a specific Host:Port.
%% @spec set_max_sessions(Host::string(), Port::integer(), Max::integer()) -> ok
set_max_sessions(Host, Port, Max) when is_integer(Max), Max > 0 ->
    gen_server:call(?MODULE, {set_config_value, {max_sessions, Host, Port}, Max}).

%% @doc Set the maximum pipeline size for each connection to a specific Host:Port.
%% @spec set_max_pipeline_size(Host::string(), Port::integer(), Max::integer()) -> ok
set_max_pipeline_size(Host, Port, Max) when is_integer(Max), Max > 0 ->
    gen_server:call(?MODULE, {set_config_value, {max_pipeline_size, Host, Port}, Max}).

do_send_req(Conn_Pid, Parsed_url, Headers, Method, Body, Options, Timeout) ->
    case catch ibrowse_http_client:send_req(Conn_Pid, Parsed_url,
					    Headers, Method, Body,
					    Options, Timeout) of
	{'EXIT', {timeout, _}} ->
	    {error, req_timedout};
	{'EXIT', Reason} ->
	    {error, {'EXIT', Reason}};
	Ret ->
	    Ret
    end.

%% @doc Creates a HTTP client process to the specified Host:Port which
%% is not part of the load balancing pool. This is useful in cases
%% where some requests to a webserver might take a long time whereas
%% some might take a very short time. To avoid getting these quick
%% requests stuck in the pipeline behind time consuming requests, use
%% this function to get a handle to a connection process. <br/>
%% <b>Note:</b> Calling this function only creates a worker process. No connection
%% is setup. The connection attempt is made only when the first
%% request is sent via any of the send_req_direct/4,5,6,7 functions.<br/>
%% <b>Note:</b> It is the responsibility of the calling process to control
%% pipeline size on such connections.
%%
%% @spec spawn_worker_process(Host::string(), Port::integer()) -> {ok, pid()}
spawn_worker_process(Host, Port) ->
    ibrowse_http_client:start({Host, Port}).

%% @doc Same as spawn_worker_process/2 except the the calling process
%% is linked to the worker process which is spawned.
spawn_link_worker_process(Host, Port) ->
    ibrowse_http_client:start_link({Host, Port}).

%% @doc Terminate a worker process spawned using
%% spawn_worker_process/2 or spawn_link_worker_process/2. Requests in
%% progress will get the error response <pre>{error, closing_on_request}</pre>
%% @spec stop_worker_process(Conn_pid::pid()) -> ok
stop_worker_process(Conn_pid) ->
    ibrowse_http_client:stop(Conn_pid).

%% @doc Same as send_req/3 except that the first argument is the PID
%% returned by spawn_worker_process/2 or spawn_link_worker_process/2
send_req_direct(Conn_pid, Url, Headers, Method) ->
    send_req_direct(Conn_pid, Url, Headers, Method, [], []).

%% @doc Same as send_req/4 except that the first argument is the PID
%% returned by spawn_worker_process/2 or spawn_link_worker_process/2
send_req_direct(Conn_pid, Url, Headers, Method, Body) ->
    send_req_direct(Conn_pid, Url, Headers, Method, Body, []).

%% @doc Same as send_req/5 except that the first argument is the PID
%% returned by spawn_worker_process/2 or spawn_link_worker_process/2
send_req_direct(Conn_pid, Url, Headers, Method, Body, Options) ->
    send_req_direct(Conn_pid, Url, Headers, Method, Body, Options, 30000).

%% @doc Same as send_req/6 except that the first argument is the PID
%% returned by spawn_worker_process/2 or spawn_link_worker_process/2
send_req_direct(Conn_pid, Url, Headers, Method, Body, Options, Timeout) ->
    case catch parse_url(Url) of
	#url{host = Host,
	     port = Port} = Parsed_url ->
	    Options_1 = merge_options(Host, Port, Options),
	    case do_send_req(Conn_pid, Parsed_url, Headers, Method, Body, Options_1, Timeout) of
		{error, {'EXIT', {noproc, _}}} ->
		    {error, worker_is_dead};
		Ret ->
		    Ret
	    end;
	Err ->
	    {error, {url_parsing_failed, Err}}
    end.
    
%% @doc Turn tracing on for the ibrowse process
trace_on() ->
    ibrowse ! {trace, true}.
%% @doc Turn tracing off for the ibrowse process
trace_off() ->
    ibrowse ! {trace, false}.

%% @doc Turn tracing on for all connections to the specified HTTP
%% server. Host is whatever is specified as the domain name in the URL
%% @spec trace_on(Host, Port) -> term() 
%% Host = string() 
%% Port = integer()
trace_on(Host, Port) ->
    ibrowse ! {trace, true, Host, Port}.

%% @doc Turn tracing OFF for all connections to the specified HTTP
%% server.
%% @spec trace_off(Host, Port) -> term()
trace_off(Host, Port) ->
    ibrowse ! {trace, false, Host, Port}.

%% @doc Shows some internal information about load balancing to a
%% specified Host:Port. Info about workers spawned using
%% spawn_worker_process/2 or spawn_link_worker_process/2 is not
%% included.
show_dest_status(Host, Port) ->
    case ets:lookup(ibrowse_lb, {Host, Port}) of
	[] ->
	    no_active_processes;
	[#lb_pid{pid = Lb_pid}] ->
	    io:format("Load Balancer Pid     : ~p~n", [Lb_pid]),
	    io:format("LB process msg q size : ~p~n", [(catch process_info(Lb_pid, message_queue_len))]),
	    case lists:dropwhile(
		   fun(Tid) ->
			   ets:info(Tid, owner) /= Lb_pid
		   end, ets:all()) of
		[] ->
		    io:format("Couldn't locate ETS table for ~p~n", [Lb_pid]);
		[Tid | _] ->
		    First = ets:first(Tid),
		    Last = ets:last(Tid),
		    Size = ets:info(Tid, size),
		    io:format("LB ETS table id       : ~p~n", [Tid]),
		    io:format("Num Connections       : ~p~n", [Size]),
		    case Size of
			0 ->
			    ok;
			_ ->
			    {First_p_sz, _} = First,
			    {Last_p_sz, _} = Last,
			    io:format("Smallest pipeline     : ~1000.p~n", [First_p_sz]),
			    io:format("Largest pipeline      : ~1000.p~n", [Last_p_sz])
		    end
	    end
    end.

%% @doc Clear current configuration for ibrowse and load from the file
%% ibrowse.conf in the IBROWSE_EBIN/../priv directory. Current
%% configuration is cleared only if the ibrowse.conf file is readable
%% using file:consult/1
rescan_config() ->
    gen_server:call(?MODULE, rescan_config).

%% Clear current configuration for ibrowse and load from the specified
%% file. Current configuration is cleared only if the specified
%% file is readable using file:consult/1
rescan_config(File) when is_list(File) ->
    gen_server:call(?MODULE, {rescan_config, File}).

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
init(_) ->
    process_flag(trap_exit, true),
    State = #state{},
    put(my_trace_flag, State#state.trace),
    put(ibrowse_trace_token, "ibrowse"),
    ets:new(ibrowse_lb, [named_table, public, {keypos, 2}]),
    ets:new(ibrowse_conf, [named_table, protected, {keypos, 2}]),
    import_config(),
    {ok, #state{}}.

import_config() ->
    case code:priv_dir(ibrowse) of
	{error, _} = Err ->
	    Err;
	PrivDir ->
	    Filename = filename:join(PrivDir, "ibrowse.conf"),
	    import_config(Filename)
    end.

import_config(Filename) ->
    case file:consult(Filename) of
	{ok, Terms} ->
	    ets:delete_all_objects(ibrowse_conf),
	    Fun = fun({dest, Host, Port, MaxSess, MaxPipe, Options}) 
		     when list(Host), integer(Port),
		     integer(MaxSess), MaxSess > 0,
		     integer(MaxPipe), MaxPipe > 0, list(Options) ->
			  I = [{{max_sessions, Host, Port}, MaxSess},
			       {{max_pipeline_size, Host, Port}, MaxPipe},
			       {{options, Host, Port}, Options}],
			  lists:foreach(
			    fun({X, Y}) ->
				    ets:insert(ibrowse_conf,
					       #ibrowse_conf{key = X, 
							     value = Y})
			    end, I);
		     ({K, V}) ->
			  ets:insert(ibrowse_conf,
				     #ibrowse_conf{key = K,
						   value = V});
		     (X) ->
			  io:format("Skipping unrecognised term: ~p~n", [X])
		  end,
	    lists:foreach(Fun, Terms);
	Err ->
	    Err
    end.

%% @doc Internal export
get_config_value(Key) ->
    [#ibrowse_conf{value = V}] = ets:lookup(ibrowse_conf, Key),
    V.

%% @doc Internal export
get_config_value(Key, DefVal) ->
    case ets:lookup(ibrowse_conf, Key) of
	[] ->
	    DefVal;
	[#ibrowse_conf{value = V}] ->
	    V
    end.

set_config_value(Key, Val) ->
    ets:insert(ibrowse_conf, #ibrowse_conf{key = Key, value = Val}).
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
handle_call({get_lb_pid, #url{host = Host, port = Port} = Url}, _From, State) ->
    Pid = do_get_connection(Url, ets:lookup(ibrowse_lb, {Host, Port})),
    {reply, Pid, State};

handle_call(stop, _From, State) ->
    do_trace("IBROWSE shutting down~n", []),
    {stop, normal, ok, State};

handle_call({set_config_value, Key, Val}, _From, State) ->
    set_config_value(Key, Val),
    {reply, ok, State};

handle_call(rescan_config, _From, State) ->
    Ret = (catch import_config()),
    {reply, Ret, State};

handle_call({rescan_config, File}, _From, State) ->
    Ret = (catch import_config(File)),
    {reply, Ret, State};

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
handle_info({trace, Bool}, State) ->
    put(my_trace_flag, Bool),
    {noreply, State};

handle_info({trace, Bool, Host, Port}, State) ->
    Fun = fun(#lb_pid{host_port = {H, P}, pid = Pid}, _)
	     when H == Host,
		  P == Port ->
		  catch Pid ! {trace, Bool};
	     (#client_conn{key = {H, P, Pid}}, _)
	     when H == Host,
		  P == Port ->
		  catch Pid ! {trace, Bool};
	     (_, Acc) ->
		  Acc
	  end,
    ets:foldl(Fun, undefined, ibrowse_lb),
    ets:insert(ibrowse_conf, #ibrowse_conf{key = {trace, Host, Port},
					   value = Bool}),
    {noreply, State};
		     
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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
do_get_connection(#url{host = Host, port = Port}, []) ->
    {ok, Pid} = ibrowse_lb:start_link([Host, Port]),
    ets:insert(ibrowse_lb, #lb_pid{host_port = {Host, Port}, pid = Pid}),
    Pid;
do_get_connection(_Url, [#lb_pid{pid = Pid}]) ->
    Pid.
