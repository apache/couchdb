%%%-------------------------------------------------------------------
%%% File    : ibrowse_lb.erl
%%% Author  : chandru <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : 
%%%
%%% Created :  6 Mar 2008 by chandru <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%%-------------------------------------------------------------------
-module(ibrowse_lb).

-vsn('$Id: ibrowse_lb.erl,v 1.1 2008/03/27 01:36:21 chandrusf Exp $ ').
-author(chandru).
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([
	 start_link/1,
	 spawn_connection/5
	]).

%% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-record(state, {parent_pid,
		ets_tid,
		host,
		port,
		max_sessions,
		max_pipeline_size,
		num_cur_sessions = 0}).

-import(ibrowse_lib, [
		      parse_url/1,
		      printable_date/0,
		      get_value/3
		     ]).
		      

-include("ibrowse.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

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
init([Host, Port]) ->
    process_flag(trap_exit, true),
    Max_sessions = ibrowse:get_config_value({max_sessions, Host, Port}, 10),
    Max_pipe_sz = ibrowse:get_config_value({max_pipeline_size, Host, Port}, 10),
    put(my_trace_flag, ibrowse_lib:get_trace_status(Host, Port)),
    put(ibrowse_trace_token, ["LB: ", Host, $:, integer_to_list(Port)]),
    Tid = ets:new(ibrowse_lb, [public, ordered_set]),
    {ok, #state{parent_pid = whereis(ibrowse),
		host = Host,
		port = Port,
		ets_tid = Tid,
		max_pipeline_size = Max_pipe_sz,
	        max_sessions = Max_sessions}}.

spawn_connection(Lb_pid, Url,
		 Max_sessions,
		 Max_pipeline_size,
		 SSL_options)
  when is_pid(Lb_pid),
       is_record(Url, url),
       is_integer(Max_pipeline_size),
       is_integer(Max_sessions) ->
    gen_server:call(Lb_pid,
		    {spawn_connection, Url, Max_sessions, Max_pipeline_size, SSL_options}).
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
% handle_call({spawn_connection, _Url, Max_sess, Max_pipe, _}, _From,
% 	    #state{max_sessions = Max_sess,
% 		   ets_tid = Tid,
% 		   max_pipeline_size = Max_pipe_sz,
% 		   num_cur_sessions = Num} = State) 
%     when Num >= Max ->
%     Reply = find_best_connection(Tid),
%     {reply, sorry_dude_reuse, State};

%% Update max_sessions in #state with supplied value
handle_call({spawn_connection, _Url, Max_sess, Max_pipe, _}, _From,
	    #state{ets_tid = Tid,
		   num_cur_sessions = Num} = State) 
    when Num >= Max_sess ->
    Reply = find_best_connection(Tid, Max_pipe),
    {reply, Reply, State#state{max_sessions = Max_sess}};

handle_call({spawn_connection, Url, _Max_sess, _Max_pipe, SSL_options}, _From,
	    #state{num_cur_sessions = Cur,
		   ets_tid = Tid} = State) ->
    {ok, Pid} = ibrowse_http_client:start_link({Tid, Url, SSL_options}),
    ets:insert(Tid, {{1, Pid}, []}),
    {reply, {ok, Pid}, State#state{num_cur_sessions = Cur + 1}};

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
handle_info({'EXIT', Parent, _Reason}, #state{parent_pid = Parent} = State) ->
    {stop, normal, State};

handle_info({'EXIT', Pid, _Reason},
	    #state{num_cur_sessions = Cur,
		   ets_tid = Tid} = State) ->
    ets:match_delete(Tid, {{'_', Pid}, '_'}),
    {noreply, State#state{num_cur_sessions = Cur - 1}};

handle_info({trace, Bool}, State) ->
    put(my_trace_flag, Bool),
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
find_best_connection(Tid, Max_pipe) ->
    case ets:first(Tid) of
	{Cur_sz, Pid} when Cur_sz < Max_pipe ->
	    ets:delete(Tid, {Cur_sz, Pid}),
	    ets:insert(Tid, {{Cur_sz + 1, Pid}, []}),
	    {ok, Pid};
	_ ->
	    {error, retry_later}
    end.
