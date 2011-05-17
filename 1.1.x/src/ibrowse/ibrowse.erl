%%%-------------------------------------------------------------------
%%% File    : ibrowse.erl
%%% Author  : Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : Load balancer process for HTTP client connections.
%%%
%%% Created : 11 Oct 2003 by Chandrashekhar Mullaparthi <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%%-------------------------------------------------------------------
%% @author Chandrashekhar Mullaparthi <chandrashekhar dot mullaparthi at gmail dot com>
%% @copyright 2005-2011 Chandrashekhar Mullaparthi
%% @version 2.1.3
%% @doc The ibrowse application implements an HTTP 1.1 client in erlang. This
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
%%               [{proxy_user, "XXXXX"},
%%                {proxy_password, "XXXXX"},
%%                {proxy_host, "proxy"},
%%                {proxy_port, 8080}], 1000). 
%% <br/><br/>
%%
%%ibrowse:send_req("http://www.erlang.org/download/otp_src_R10B-3.tar.gz", [], get, [],
%%               [{proxy_user, "XXXXX"},
%%                {proxy_password, "XXXXX"},
%%                {proxy_host, "proxy"},
%%                {proxy_port, 8080},
%%                {save_response_to_file, true}], 1000).
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

-module(ibrowse).
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
         spawn_worker_process/1,
         spawn_worker_process/2,
         spawn_link_worker_process/1,
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
         stream_next/1,
         stream_close/1,
         set_max_sessions/3,
         set_max_pipeline_size/3,
         set_dest/3,
         trace_on/0,
         trace_off/0,
         trace_on/2,
         trace_off/2,
         all_trace_off/0,
         show_dest_status/0,
         show_dest_status/2
        ]).

-ifdef(debug).
-compile(export_all).
-endif.

-import(ibrowse_lib, [
                      parse_url/1,
                      get_value/3,
                      do_trace/2
                     ]).
                      
-record(state, {trace = false}).

-include("ibrowse.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

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
%% response() = {ok, Status, ResponseHeaders, ResponseBody} | {ibrowse_req_id, req_id() } | {error, Reason}
%% req_id() = term()
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
%% For a description of SSL Options, look in the <a href="http://www.erlang.org/doc/apps/ssl/index.html">ssl</a> manpage. If the
%% HTTP Version to use is not specified, the default is 1.1.
%% <br/>
%% <ul>
%% <li>The <code>host_header</code> option is useful in the case where ibrowse is
%% connecting to a component such as <a
%% href="http://www.stunnel.org">stunnel</a> which then sets up a
%% secure connection to a webserver. In this case, the URL supplied to
%% ibrowse must have the stunnel host/port details, but that won't
%% make sense to the destination webserver. This option can then be
%% used to specify what should go in the <code>Host</code> header in
%% the request.</li>
%% <li>The <code>stream_to</code> option can be used to have the HTTP
%% response streamed to a process as messages as data arrives on the
%% socket. If the calling process wishes to control the rate at which
%% data is received from the server, the option <code>{stream_to,
%% {process(), once}}</code> can be specified. The calling process
%% will have to invoke <code>ibrowse:stream_next(Request_id)</code> to
%% receive the next packet.</li>
%%
%% <li>When both the options <code>save_response_to_file</code> and <code>stream_to</code> 
%% are specified, the former takes precedence.</li>
%%
%% <li>For the <code>save_response_to_file</code> option, the response body is saved to
%% file only if the status code is in the 200-299 range. If not, the response body is returned
%% as a string.</li>
%% <li>Whenever an error occurs in the processing of a request, ibrowse will return as much
%% information as it has, such as HTTP Status Code and HTTP Headers. When this happens, the response
%% is of the form <code>{error, {Reason, {stat_code, StatusCode}, HTTP_headers}}</code></li>
%%
%% <li>The <code>inactivity_timeout</code> option is useful when
%% dealing with large response bodies and/or slow links. In these
%% cases, it might be hard to estimate how long a request will take to
%% complete. In such cases, the client might want to timeout if no
%% data has been received on the link for a certain time interval.
%% 
%% This value is also used to close connections which are not in use for 
%% the specified timeout value.
%% </li>
%%
%% <li>
%% The <code>connect_timeout</code> option is to specify how long the
%% client process should wait for connection establishment. This is
%% useful in scenarios where connections to servers are usually setup
%% very fast, but responses might take much longer compared to
%% connection setup. In such cases, it is better for the calling
%% process to timeout faster if there is a problem (DNS lookup
%% delays/failures, network routing issues, etc). The total timeout
%% value specified for the request will enforced. To illustrate using
%% an example:
%% <code>
%% ibrowse:send_req("http://www.example.com/cgi-bin/request", [], get, [], [{connect_timeout, 100}], 1000).
%% </code>
%% In the above invocation, if the connection isn't established within
%% 100 milliseconds, the request will fail with 
%% <code>{error, conn_failed}</code>.<br/>
%% If connection setup succeeds, the total time allowed for the
%% request to complete will be 1000 milliseconds minus the time taken
%% for connection setup.
%% </li>
%% 
%% <li> The <code>socket_options</code> option can be used to set
%% specific options on the socket. The <code>{active, true | false | once}</code> 
%% and <code>{packet_type, Packet_type}</code> will be filtered out by ibrowse.  </li>
%%
%% <li> The <code>headers_as_is</code> option is to enable the caller
%% to send headers exactly as specified in the request without ibrowse
%% adding some of its own. Required for some picky servers apparently.  </li>
%%
%% <li>The <code>give_raw_headers</code> option is to enable the
%% caller to get access to the raw status line and raw unparsed
%% headers. Not quite sure why someone would want this, but one of my
%% users asked for it, so here it is. </li>
%%
%% <li> The <code>preserve_chunked_encoding</code> option enables the caller
%% to receive the raw data stream when the Transfer-Encoding of the server
%% response is Chunked.
%% </li>
%% </ul>
%%
%% @spec send_req(Url::string(), Headers::headerList(), Method::method(), Body::body(), Options::optionList()) -> response()
%% optionList() = [option()]
%% option() = {max_sessions, integer()}        |
%%          {response_format,response_format()}|
%%          {stream_chunk_size, integer()}     |
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
%%          {stream_to, stream_to()}           |
%%          {http_vsn, {MajorVsn, MinorVsn}}   |
%%          {host_header, string()}            |
%%          {inactivity_timeout, integer()}    |
%%          {connect_timeout, integer()}       |
%%          {socket_options, Sock_opts}        |
%%          {transfer_encoding, {chunked, ChunkSize}} | 
%%          {headers_as_is, boolean()}         |
%%          {give_raw_headers, boolean()}      |
%%          {preserve_chunked_encoding,boolean()}
%%
%% stream_to() = process() | {process(), once}
%% process() = pid() | atom()
%% username() = string()
%% password() = string()
%% SSLOpt = term()
%% Sock_opts = [Sock_opt]
%% Sock_opt = term()
%% ChunkSize = integer()
%% srtf() = boolean() | filename()
%% filename() = string()
%% response_format() = list | binary
send_req(Url, Headers, Method, Body, Options) ->
    send_req(Url, Headers, Method, Body, Options, 30000).

%% @doc Same as send_req/5. 
%% All timeout values are in milliseconds.
%% @spec send_req(Url, Headers::headerList(), Method::method(), Body::body(), Options::optionList(), Timeout) -> response()
%% Timeout = integer() | infinity
send_req(Url, Headers, Method, Body, Options, Timeout) ->
    case catch parse_url(Url) of
        #url{host = Host,
             port = Port,
             protocol = Protocol} = Parsed_url ->
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
                case (Protocol == https) orelse
                    get_value(is_ssl, Options_1, false) of
                    false -> {[], false};
                    true -> {get_value(ssl_options, Options_1, []), true}
                end,
            try_routing_request(Lb_pid, Parsed_url,
                                Max_sessions, 
                                Max_pipeline_size,
                                {SSLOptions, IsSSL}, 
                                Headers, Method, Body, Options_1, Timeout, 0);
        Err ->
            {error, {url_parsing_failed, Err}}
    end.

try_routing_request(Lb_pid, Parsed_url,
                    Max_sessions, 
                    Max_pipeline_size,
                    {SSLOptions, IsSSL}, 
                    Headers, Method, Body, Options_1, Timeout, Try_count) when Try_count < 3 ->
    case ibrowse_lb:spawn_connection(Lb_pid, Parsed_url,
                                             Max_sessions, 
                                             Max_pipeline_size,
                                             {SSLOptions, IsSSL}) of
        {ok, Conn_Pid} ->
            case do_send_req(Conn_Pid, Parsed_url, Headers,
                             Method, Body, Options_1, Timeout) of
                {error, sel_conn_closed} ->
                    try_routing_request(Lb_pid, Parsed_url,
                                        Max_sessions, 
                                        Max_pipeline_size,
                                        {SSLOptions, IsSSL}, 
                                        Headers, Method, Body, Options_1, Timeout, Try_count + 1);
                Res ->
                    Res
            end;
        Err ->
            Err
    end;
try_routing_request(_, _, _, _, _, _, _, _, _, _, _) ->
    {error, retry_later}.

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
              get_config_value({max_sessions, Host, Port},
                               default_max_sessions())).

get_max_pipeline_size(Host, Port, Options) ->
    get_value(max_pipeline_size, Options,
              get_config_value({max_pipeline_size, Host, Port},
                               default_max_pipeline_size())).

default_max_sessions() ->
    safe_get_env(ibrowse, default_max_sessions, ?DEF_MAX_SESSIONS).

default_max_pipeline_size() ->
    safe_get_env(ibrowse, default_max_pipeline_size, ?DEF_MAX_PIPELINE_SIZE).

safe_get_env(App, Key, Def_val) ->
    case application:get_env(App, Key) of
        undefined ->
            Def_val;
        {ok, Val} ->
            Val
    end.

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
                                            Headers, Method, ensure_bin(Body),
                                            Options, Timeout) of
        {'EXIT', {timeout, _}} ->
            {error, req_timedout};
        {'EXIT', {noproc, {gen_server, call, [Conn_Pid, _, _]}}} ->
            {error, sel_conn_closed};
        {error, connection_closed} ->
            {error, sel_conn_closed};
        {'EXIT', Reason} ->
            {error, {'EXIT', Reason}};
        {ok, St_code, Headers, Body} = Ret when is_binary(Body) ->
            case get_value(response_format, Options, list) of
                list ->
                    {ok, St_code, Headers, binary_to_list(Body)};
                binary ->
                    Ret
            end;
        Ret ->
            Ret
    end.

ensure_bin(L) when is_list(L)                     -> list_to_binary(L);
ensure_bin(B) when is_binary(B)                   -> B;
ensure_bin(Fun) when is_function(Fun)             -> Fun;
ensure_bin({Fun}) when is_function(Fun)           -> Fun;
ensure_bin({Fun, _} = Body) when is_function(Fun) -> Body.

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
%% @spec spawn_worker_process(Url::string()) -> {ok, pid()}
spawn_worker_process(Url) ->
    ibrowse_http_client:start(Url).

%% @doc Same as spawn_worker_process/1 but takes as input a Host and Port
%% instead of a URL.
%% @spec spawn_worker_process(Host::string(), Port::integer()) -> {ok, pid()}
spawn_worker_process(Host, Port) ->
    ibrowse_http_client:start({Host, Port}).

%% @doc Same as spawn_worker_process/1 except the the calling process
%% is linked to the worker process which is spawned.
%% @spec spawn_link_worker_process(Url::string()) -> {ok, pid()}
spawn_link_worker_process(Url) ->
    ibrowse_http_client:start_link(Url).

%% @doc Same as spawn_worker_process/2 except the the calling process
%% is linked to the worker process which is spawned.
%% @spec spawn_link_worker_process(Host::string(), Port::integer()) -> {ok, pid()}
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

%% @doc Tell ibrowse to stream the next chunk of data to the
%% caller. Should be used in conjunction with the
%% <code>stream_to</code> option
%% @spec stream_next(Req_id :: req_id()) -> ok | {error, unknown_req_id}
stream_next(Req_id) ->    
    case ets:lookup(ibrowse_stream, {req_id_pid, Req_id}) of
        [] ->
            {error, unknown_req_id};
        [{_, Pid}] ->
            catch Pid ! {stream_next, Req_id},
            ok
    end.

%% @doc Tell ibrowse to close the connection associated with the
%% specified stream.  Should be used in conjunction with the
%% <code>stream_to</code> option. Note that all requests in progress on
%% the connection which is serving this Req_id will be aborted, and an
%% error returned.
%% @spec stream_close(Req_id :: req_id()) -> ok | {error, unknown_req_id}
stream_close(Req_id) ->    
    case ets:lookup(ibrowse_stream, {req_id_pid, Req_id}) of
        [] ->
            {error, unknown_req_id};
        [{_, Pid}] ->
            catch Pid ! {stream_close, Req_id},
            ok
    end.

%% @doc Turn tracing on for the ibrowse process
trace_on() ->
    ibrowse ! {trace, true}.
%% @doc Turn tracing off for the ibrowse process
trace_off() ->
    ibrowse ! {trace, false}.

%% @doc Turn tracing on for all connections to the specified HTTP
%% server. Host is whatever is specified as the domain name in the URL
%% @spec trace_on(Host, Port) -> ok
%% Host = string() 
%% Port = integer()
trace_on(Host, Port) ->
    ibrowse ! {trace, true, Host, Port},
    ok.

%% @doc Turn tracing OFF for all connections to the specified HTTP
%% server.
%% @spec trace_off(Host, Port) -> ok
trace_off(Host, Port) ->
    ibrowse ! {trace, false, Host, Port},
    ok.

%% @doc Turn Off ALL tracing
%% @spec all_trace_off() -> ok
all_trace_off() ->
    ibrowse ! all_trace_off,
    ok.

%% @doc Shows some internal information about load balancing. Info
%% about workers spawned using spawn_worker_process/2 or
%% spawn_link_worker_process/2 is not included.
show_dest_status() ->
    Dests = lists:filter(fun({lb_pid, {Host, Port}, _}) when is_list(Host),
                                                             is_integer(Port) ->
                                 true;
                            (_) ->
                                 false
                         end, ets:tab2list(ibrowse_lb)),
    All_ets = ets:all(),
    io:format("~-40.40s | ~-5.5s | ~-10.10s | ~s~n",
              ["Server:port", "ETS", "Num conns", "LB Pid"]),
    io:format("~80.80.=s~n", [""]),
    lists:foreach(fun({lb_pid, {Host, Port}, Lb_pid}) ->
                          case lists:dropwhile(
                                 fun(Tid) ->
                                         ets:info(Tid, owner) /= Lb_pid
                                 end, All_ets) of
                              [] ->
                                  io:format("~40.40s | ~-5.5s | ~-5.5s | ~s~n",
                                            [Host ++ ":" ++ integer_to_list(Port),
                                             "",
                                             "",
                                             io_lib:format("~p", [Lb_pid])]
                                           );
                              [Tid | _] ->
                                  catch (
                                    begin
                                        Size = ets:info(Tid, size),
                                        io:format("~40.40s | ~-5.5s | ~-5.5s | ~s~n",
                                                  [Host ++ ":" ++ integer_to_list(Port),
                                                   io_lib:format("~p", [Tid]),
                                                   integer_to_list(Size),
                                                   io_lib:format("~p", [Lb_pid])]
                                                 )
                                    end
                                   )
                                  end
                  end, Dests).
                                          
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
    ibrowse_lb     = ets:new(ibrowse_lb, [named_table, public, {keypos, 2}]),
    ibrowse_conf   = ets:new(ibrowse_conf, [named_table, protected, {keypos, 2}]),
    ibrowse_stream = ets:new(ibrowse_stream, [named_table, public]),
    import_config(),
    {ok, #state{}}.

import_config() ->
    case code:priv_dir(ibrowse) of
        {error, _} ->
            ok;
        PrivDir ->
            Filename = filename:join(PrivDir, "ibrowse.conf"),
            import_config(Filename)
    end.

import_config(Filename) ->
    case file:consult(Filename) of
        {ok, Terms} ->
            ets:delete_all_objects(ibrowse_conf),
            Fun = fun({dest, Host, Port, MaxSess, MaxPipe, Options}) 
                     when is_list(Host), is_integer(Port),
                          is_integer(MaxSess), MaxSess > 0,
                          is_integer(MaxPipe), MaxPipe > 0, is_list(Options) ->
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
        _Err ->
            ok
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
    ets:foldl(fun(#lb_pid{pid = Pid}, Acc) ->
                      ibrowse_lb:stop(Pid),
                      Acc
              end, [], ibrowse_lb),
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
handle_info(all_trace_off, State) ->
    Mspec = [{{ibrowse_conf,{trace,'$1','$2'},true},[],[{{'$1','$2'}}]}],
    Trace_on_dests = ets:select(ibrowse_conf, Mspec),
    Fun = fun(#lb_pid{host_port = {H, P}, pid = Pid}, _) ->
                  case lists:member({H, P}, Trace_on_dests) of
                      false ->
                          ok;
                      true ->
                          catch Pid ! {trace, false}
                  end;
             (_, Acc) ->
                  Acc
          end,
    ets:foldl(Fun, undefined, ibrowse_lb),
    ets:select_delete(ibrowse_conf, [{{ibrowse_conf,{trace,'$1','$2'},true},[],['true']}]),
    {noreply, State};
                                  
handle_info({trace, Bool}, State) ->
    put(my_trace_flag, Bool),
    {noreply, State};

handle_info({trace, Bool, Host, Port}, State) ->
    Fun = fun(#lb_pid{host_port = {H, P}, pid = Pid}, _)
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
