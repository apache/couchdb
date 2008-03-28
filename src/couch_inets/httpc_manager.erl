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

-module(httpc_manager).

-behaviour(gen_server).

-include("httpc_internal.hrl").
-include("http_internal.hrl").

%% Application API
-export([start_link/1, request/1, cancel_request/1,
	 request_canceled/1, retry_request/1, redirect_request/1,
	 insert_session/1, delete_session/1, set_options/1, store_cookies/2,
	 cookies/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {
	  cancel = [],	 % [{RequestId, HandlerPid, ClientPid}]  
	  handler_db,    % ets() - Entry: {Requestid, HandlerPid, ClientPid}
	  cookie_db,     % {ets(), dets()} - {session_cookie_db, cookie_db} 
	  options = #options{}
	 }).

%%====================================================================
%% Application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid}
%%
%% Description: Starts the http request manger process. (Started by
%% the intes supervisor.)
%%--------------------------------------------------------------------
start_link({default, CookieDir}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 
			  [{http_default_cookie_db, CookieDir}], []). 

%%--------------------------------------------------------------------
%% Function: request() -> {ok, Requestid} | {error, Reason}
%%	Request = #request{}
%%
%% Description: Sends a request to the httpc manager process.
%%--------------------------------------------------------------------
request(Request) ->
    call({request, Request}, infinity).

%%--------------------------------------------------------------------
%% Function: retry_request(Request) -> _
%%	Request = #request{}
%%
%% Description: Resends a request to the httpc manager process, intended
%% to be called by the httpc handler process if it has to terminate with
%% a non empty pipeline.
%%--------------------------------------------------------------------
retry_request(Request) ->
    cast({retry_or_redirect_request, Request}).

%%--------------------------------------------------------------------
%% Function: redirect_request(Request) -> _
%%	Request = #request{}
%%
%% Description: Sends an atoumatic redirect request to the httpc
%% manager process, intended to be called by the httpc handler process
%% when the automatic redirect option is set.
%%--------------------------------------------------------------------
redirect_request(Request) ->
    cast({retry_or_redirect_request, Request}).

%%--------------------------------------------------------------------
%% Function: cancel_request(RequestId) -> ok
%%	RequestId - ref()
%%
%% Description: Cancels the request with <RequestId>.
%%--------------------------------------------------------------------
cancel_request(RequestId) ->
    call({cancel_request, RequestId}, infinity).

%%--------------------------------------------------------------------
%% Function: request_canceled(RequestId) -> ok
%%	RequestId - ref()
%%
%% Description: Confirms that a request has been canceld. Intended to
%% be called by the httpc handler process.
%%--------------------------------------------------------------------
request_canceled(RequestId) ->
    cast({request_canceled, RequestId}).

%%--------------------------------------------------------------------
%% Function: insert_session(Session) -> _
%%	Session - #tcp_session{}
%%
%% Description: Inserts session information into the httpc manager table
%% httpc_manager_session_db. Intended to be called by the httpc request
%% handler process.
%%--------------------------------------------------------------------
insert_session(Session) ->
    ets:insert(httpc_manager_session_db, Session).

%%--------------------------------------------------------------------
%% Function: delete_session(SessionId) -> _
%%	SessionId -  {{Host, Port}, HandlerPid}
%% 
%% Description: Deletes session information from the httpc manager table
%% httpc_manager_session_db. Intended to be called by the httpc request
%% handler process.
%%--------------------------------------------------------------------
delete_session(SessionId) ->
    ets:delete(httpc_manager_session_db, SessionId).

%%--------------------------------------------------------------------
%% Function: set_options(Options) -> ok
%%
%% Options = [Option]
%% Option = {proxy, {Proxy, [NoProxy]}} | {max_pipeline_length, integer()} |
%%          {max_sessions, integer()} | {pipeline_timeout, integer()}
%% Proxy = {Host, Port}
%% NoProxy - [Domain | HostName | IPAddress]     
%% Max - integer() 
%% 
%% Description: Sets the options to be used by the client.
%%--------------------------------------------------------------------
set_options(Options) ->
    cast({set_options, Options}).

%%--------------------------------------------------------------------
%% Function: store_cookies(Cookies, Address) -> ok
%%
%% Cookies = [Cookie]
%% Cookie = #http_cookie{}
%% 
%% Description: Stores cookies from the server.
%%--------------------------------------------------------------------
store_cookies([], _) ->
    ok;
store_cookies(Cookies, Address) ->
    cast({store_cookies, {Cookies, Address}}).

%%--------------------------------------------------------------------
%% Function: cookies(Url) -> ok
%%
%% Url = string()
%%
%% Description: Retrieves the cookies that  
%%--------------------------------------------------------------------
cookies(Url) ->
    call({cookies, Url}, infinity).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Session]) -> {ok, State} | 
%%                       {ok, State, Timeout} | ignore |{stop, Reason}
%% Description: Initiates the httpc_manger process
%%--------------------------------------------------------------------
init([CookiesConf|_Options]) ->
    process_flag(trap_exit, true),
    ets:new(httpc_manager_session_db, 
	    [public, set, named_table, {keypos, #tcp_session.id}]),
    {ok, #state{handler_db = ets:new(handler_db, [protected, set]),
		cookie_db = 
		http_cookie:open_cookie_db({CookiesConf, 
					    http_session_cookie_db})
	       }}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({request, Request}, _, State) ->
    case (catch handle_request(Request, State)) of
	{reply, Msg, NewState} ->
	    {reply, Msg, NewState};
	Error ->
	    {stop, Error, httpc_response:error(Request, Error), State}
    end;
	
handle_call({cancel_request, RequestId}, From, State) ->
    case ets:lookup(State#state.handler_db, RequestId) of
	[] ->
	    ok, %% Nothing to cancel
	      {reply, ok, State};
	[{_, Pid, _}] ->
	    httpc_handler:cancel(RequestId, Pid),
	    {noreply, State#state{cancel = 
				  [{RequestId, Pid, From} |
				   State#state.cancel]}}
    end;

handle_call({cookies, Url}, _, State) ->
    case http_uri:parse(Url) of
	{Scheme, _, Host, Port, Path, _} ->
	    CookieHeaders = 
		http_cookie:header(Scheme, {Host, Port}, 
				   Path, State#state.cookie_db),
	    {reply, CookieHeaders, State};
	Msg ->
	    {reply, Msg, State}
    end;

handle_call(Msg, From, State) ->
    error_logger:error_report("HTTPC_MANAGER recived unkown call: ~p"
			      "from: ~p~n", [Msg, From]),
    {reply, {error, 'API_violation'}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({retry_or_redirect_request, {Time, Request}}, State) ->
    {ok, _} = timer:apply_after(Time, ?MODULE, retry_request, [Request]),
    {noreply, State};

handle_cast({retry_or_redirect_request, Request}, State) ->
    case (catch handle_request(Request, State)) of
	{reply, {ok, _}, NewState} ->
	    {noreply, NewState};
	Error  ->
	    httpc_response:error(Request, Error),
	    {stop, Error, State}
    end;

handle_cast({request_canceled, RequestId}, State) ->
    ets:delete(State#state.handler_db, RequestId),
    case lists:keysearch(RequestId, 1, State#state.cancel) of
	{value, Entry = {RequestId, _, From}} ->
	    gen_server:reply(From, ok),
	    {noreply, 
	     State#state{cancel = lists:delete(Entry, State#state.cancel)}};
	_ ->
	   {noreply, State}
    end;
handle_cast({set_options, Options}, State = #state{options = OldOptions}) ->
    NewOptions = 
	#options{proxy = 
		 http_util:key1search(Options, proxy,
				       OldOptions#options.proxy),
		 pipeline_timeout = 
		 http_util:key1search(Options, pipeline_timeout, 
				       OldOptions#options.pipeline_timeout),
		 max_pipeline_length =
		 http_util:key1search(Options, max_pipeline_length, 
				       OldOptions#options.max_pipeline_length),
		 max_sessions = 
		 http_util:key1search(Options, max_sessions, 
				       OldOptions#options.max_sessions),
		 cookies = http_util:key1search(Options, cookies, 
						 OldOptions#options.cookies),
		 ipv6 = http_util:key1search(Options, ipv6, 
					     OldOptions#options.ipv6),
		 verbose = http_util:key1search(Options, verbose, 
						OldOptions#options.verbose)
		}, 
    case {OldOptions#options.verbose, NewOptions#options.verbose} of
	{Same, Same} ->
	    ok;
	{_, false} ->
	    dbg:stop();
	{false, Level}  ->
	    dbg:tracer(),
	    handle_verbose(Level);
	{_, Level} ->
	    dbg:stop(),
	    dbg:tracer(),
	    handle_verbose(Level)
    end,

    {noreply, State#state{options = NewOptions}};

handle_cast({store_cookies, _}, 
	    State = #state{options = #options{cookies = disabled}}) ->
    {noreply, State};

handle_cast({store_cookies, {Cookies, _}}, State) ->
    ok = do_store_cookies(Cookies, State),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:error_report("HTTPC_MANAGER recived unkown cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% Description: Handling all non call/cast messages
%%---------------------------------------------------------
handle_info({'EXIT', _, _}, State) ->
    %% Handled in DOWN
    {noreply, State};
handle_info({'DOWN', _, _, Pid, _}, State) ->
    ets:match_delete(State#state.handler_db, {'_', Pid, '_'}),

    %% If there where any canceled request, handled by the
    %% the process that now has terminated, the
    %% cancelation can be viewed as sucessfull!
    NewCanceldList = 
	lists:foldl(fun(Entry = {_, HandlerPid, From}, Acc)  ->
			    case HandlerPid of
				Pid ->
				    gen_server:reply(From, ok),
				    lists:delete(Entry, Acc);
				_ ->
				    Acc
			    end 
		    end, State#state.cancel, State#state.cancel),
    {noreply, State#state{cancel = NewCanceldList}};    
handle_info(Info, State) ->
    error_logger:error_report("Unknown message in "
			      "httpc_manager:handle_info ~p~n", [Info]),
    {noreply, State}. 
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> _  (ignored by gen_server)
%% Description: Shutdown the httpc_handler
%%--------------------------------------------------------------------
terminate(_, State) ->
    http_cookie:close_cookie_db(State#state.cookie_db),
    ets:delete(httpc_manager_session_db),
    ets:delete(State#state.handler_db).

%%--------------------------------------------------------------------
%% Func: code_change(_OldVsn, State, Extra) -> {ok, NewState}
%% Purpose: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
handle_request(Request, State) ->
    NewRequest = handle_cookies(generate_request_id(Request), State),
    case select_session(Request#request.method, 
			Request#request.address, 
			Request#request.scheme, State) of
	{ok, HandlerPid} ->
	    pipeline(NewRequest, HandlerPid, State);
	no_connection ->
	    start_handler(NewRequest, State);
	{no_session,  OpenSessions} when OpenSessions 
	< State#options.max_sessions ->
	    start_handler(NewRequest, State);
	{no_session, _} ->
	    %% Do not start any more persistent connections
	    %% towards this server.
	    NewHeaders = 
		(NewRequest#request.headers)#http_request_h{connection
							    = "close"},
	    start_handler(NewRequest#request{headers = NewHeaders}, State)
    end,
    {reply, {ok, NewRequest#request.id}, State}.

select_session(Method, HostPort, Scheme, #state{options = 
						#options{max_pipeline_length =
							 Max}}) ->
    case httpc_request:is_idempotent(Method) of
	true ->
	    Candidates = ets:match(httpc_manager_session_db,
				   {'_', {HostPort, '$1'}, 
				    false, Scheme, '_', '$2'}),
	    select_session(Candidates, Max);
	false ->
	    no_connection
    end.
    
select_session(Candidates, MaxPipeline) ->
    case Candidates of 
	[] ->
	  no_connection; 
	_ ->
	    NewCandidates = 
		lists:foldl(
		  fun([Pid, PipelineLength], Acc) when 
			    PipelineLength =< MaxPipeline ->
			  [{Pid, PipelineLength} | Acc];
		     (_, Acc) ->
			  Acc
		  end, [], Candidates),
	    
	    case lists:keysort(2, NewCandidates) of
		[] ->
		    {no_session, length(Candidates)};
		[{HandlerPid, _} | _] ->
		    {ok, HandlerPid}
	    end
    end.
	    
pipeline(Request, HandlerPid, State) ->
    case (catch httpc_handler:send(Request, HandlerPid)) of
	ok ->
	    ets:insert(State#state.handler_db, {Request#request.id, 
						HandlerPid,
						Request#request.from});
	_  -> %timeout pipelining failed 
	    start_handler(Request, State)
    end.

start_handler(Request, State) ->
    {ok, Pid} = httpc_handler:start_link(Request, State#state.options),
    ets:insert(State#state.handler_db, {Request#request.id, 
					Pid, Request#request.from}),
    erlang:monitor(process, Pid).

generate_request_id(Request) ->
    case Request#request.id of
	undefined ->
	    RequestId = make_ref(),
	    Request#request{id = RequestId};
	_ ->
	    %% This is an automatic redirect or a retryed pipelined
	    %% request keep the old id.
	    Request
    end.

handle_cookies(Request, #state{options = #options{cookies = disabled}}) ->
    Request;
handle_cookies(Request = #request{scheme = Scheme, address = Address,
				  path = Path, headers = 
		 Headers = #http_request_h{other = Other}}, 
	       #state{cookie_db = Db}) ->
    case http_cookie:header(Scheme, Address, Path, Db) of
	{"cookie", ""} ->
	    Request;
	CookieHeader ->
	    NewHeaders = 
		Headers#http_request_h{other = [CookieHeader | Other]},
	    Request#request{headers = NewHeaders}
    end.

do_store_cookies([], _) ->
    ok;
do_store_cookies([Cookie | Cookies], State) ->
    ok = http_cookie:insert(Cookie, State#state.cookie_db),
    do_store_cookies(Cookies, State).

call(Msg, Timeout) ->
    gen_server:call(?MODULE, Msg, Timeout).

cast(Msg) ->
   gen_server:cast(?MODULE, Msg).

handle_verbose(debug) ->
    dbg:p(self(), [call]),
    dbg:tp(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(trace) ->
    dbg:p(self(), [call]),
    dbg:tpl(?MODULE, [{'_', [], [{return_trace}]}]);
handle_verbose(_) ->
    ok.  

