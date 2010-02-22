%%%-------------------------------------------------------------------
%%% File:      dynomite_prof.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-02-15 by Cliff Moon
%%%-------------------------------------------------------------------
-module(dynomite_prof).
-author('cliff@powerset.com').

-behaviour(gen_server).

%% API
-export([start_link/0, start_prof/1, stop_prof/1, stats/1, averages/0, balance_prof/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {ets,balance}).

-record(profile, {name, count, sum}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, dynomite_prof}, ?MODULE, [], []).

stats(Id) ->
  gen_server:call(dynomite_prof, {stats, Id}).

balance_prof() ->
  gen_server:cast(dynomite_prof, {balance, self(), lib_misc:now_float()}).

start_prof(Id) ->
  gen_server:cast(dynomite_prof, {start, self(), Id, lib_misc:now_float()}).

stop_prof(Id) ->
  gen_server:cast(dynomite_prof, {stop, self(), Id, lib_misc:now_float()}).

averages() ->
  gen_server:call(dynomite_prof, averages).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
  Tid = ets:new(profiling, [set, {keypos, 2}]),
  Bal = ets:new(balance, [set]),
  {ok, #state{ets=Tid, balance=Bal}}.

%%--------------------------------------------------------------------
%% @spec
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({stats, Id}, _From, State = #state{ets=Ets}) ->
  Reply = ets:lookup(Ets, Id),
  {reply, Reply, State};

handle_call(table, _From, State = #state{ets=Ets}) ->
  {reply, Ets, State};

handle_call(averages, _From, State = #state{ets=Ets,balance=Bal}) ->
  Avgs = ets:foldl(fun(#profile{name=Name,count=Count,sum=Sum}, List) ->
      [{Name, Sum/Count}|List]
    end, [], Ets),
  {_, MaxCount} = ets:foldl(fun
      ({Pid, Count}, {_P, M}) when Count > M -> {Pid, Count};
      (_, {P, M}) -> {P, M}
    end, {pid, 0}, Bal),
  Balances = ets:foldl(fun({Pid, Count}, List) ->
      [{Pid, Count / MaxCount} | List]
    end, [], Bal),
  {reply, [Balances, Avgs], State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({balance, Pid, _Time}, State = #state{balance=Ets}) ->
  case ets:lookup(Ets, Pid) of
    [] -> ets:insert(Ets, {Pid, 1});
    [{Pid, Count}] -> ets:insert(Ets, {Pid, Count+1})
  end,
  {noreply, State};

handle_cast({start, Pid, Id, Time}, State = #state{ets=_Ets}) ->
  put({Pid,Id}, Time),
  {noreply, State};

handle_cast({stop, Pid, Id, Time}, State = #state{ets=Ets}) ->
  case get({Pid, Id}) of
    undefined -> ok;
    OldTime ->
      erase({Pid, Id}),
      increment_time(Ets, Time-OldTime, Id)
  end,
  {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
increment_time(Ets, Time, Id) ->
  case ets:lookup(Ets, Id) of
    [] -> ets:insert(Ets, #profile{name=Id,count=1,sum=Time});
    [#profile{name=Id,count=Count,sum=Sum}] -> ets:insert(Ets, #profile{name=Id,count=Count+1,sum=Sum+Time})
  end.
