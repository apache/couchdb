%%%-------------------------------------------------------------------
%%% File    : ibrowse_lb.erl
%%% Author  : chandru <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%% Description : 
%%%
%%% Created :  6 Mar 2008 by chandru <chandrashekhar.mullaparthi@t-mobile.co.uk>
%%%-------------------------------------------------------------------
-module(ibrowse_lb).
-author(chandru).
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([
	 start_link/1,
	 spawn_connection/5,
         stop/1
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

stop(Lb_pid) ->
    case catch gen_server:call(Lb_pid, stop) of
        {'EXIT', {timeout, _}} ->
            exit(Lb_pid, kill);
        ok ->
            ok
    end.
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
	    #state{num_cur_sessions = Num} = State) 
    when Num >= Max_sess ->
    State_1 = maybe_create_ets(State),
    Reply = find_best_connection(State_1#state.ets_tid, Max_pipe),
    {reply, Reply, State_1#state{max_sessions = Max_sess}};

handle_call({spawn_connection, Url, _Max_sess, _Max_pipe, SSL_options}, _From,
	    #state{num_cur_sessions = Cur} = State) ->
    State_1 = maybe_create_ets(State),
    Tid = State_1#state.ets_tid,
    {ok, Pid} = ibrowse_http_client:start_link({Tid, Url, SSL_options}),
    ets:insert(Tid, {{1, Pid}, []}),
    {reply, {ok, Pid}, State_1#state{num_cur_sessions = Cur + 1}};

handle_call(stop, _From, #state{ets_tid = undefined} = State) ->
    gen_server:reply(_From, ok),
    {stop, normal, State};

handle_call(stop, _From, #state{ets_tid = Tid} = State) ->
    ets:foldl(fun({{_, Pid}, _}, Acc) ->
                      ibrowse_http_client:stop(Pid),
                      Acc
              end, [], Tid),
    gen_server:reply(_From, ok),
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
handle_info({'EXIT', Parent, _Reason}, #state{parent_pid = Parent} = State) ->
    {stop, normal, State};

handle_info({'EXIT', _Pid, _Reason}, #state{ets_tid = undefined} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, _Reason},
	    #state{num_cur_sessions = Cur,
		   ets_tid = Tid} = State) ->
    ets:match_delete(Tid, {{'_', Pid}, '_'}),
    Cur_1 = Cur - 1,
    State_1 = case Cur_1 of
		  0 ->
		      ets:delete(Tid),
		      State#state{ets_tid = undefined};
		  _ ->
		      State
	      end,
    {noreply, State_1#state{num_cur_sessions = Cur_1}};

handle_info({trace, Bool}, #state{ets_tid = undefined} = State) ->
    put(my_trace_flag, Bool),
    {noreply, State};

handle_info({trace, Bool}, #state{ets_tid = Tid} = State) ->
    ets:foldl(fun({{_, Pid}, _}, Acc) when is_pid(Pid) ->
		      catch Pid ! {trace, Bool},
		      Acc;
		 (_, Acc) ->
		      Acc
	      end, undefined, Tid),
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

maybe_create_ets(#state{ets_tid = undefined} = State) ->
    Tid = ets:new(ibrowse_lb, [public, ordered_set]),
    State#state{ets_tid = Tid};
maybe_create_ets(State) ->
    State.
