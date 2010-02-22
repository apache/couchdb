%%%-------------------------------------------------------------------
%%% File:      bootstrap_manager.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc  This is the bootstrap manager for a cluster.
%%%
%%% @end
%%%
%%% @since 2009-07-29 by Cliff Moon
%%%-------------------------------------------------------------------
-module(bootstrap_manager).
-author('cliff@powerset.com').
-author('brad@cloudant.com').

-behaviour(gen_server).

%% API
-export([start_bootstrap/3, end_bootstrap/1,
         start_link/3, start/3, stop/0,
         start_transfers/0, transfers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {transfer_list, nodes, transfers, futurefullmap}).
-record(transfer, {partition, receivers, rate=0, status=starting}).

-include("../include/config.hrl").
-include("../include/common.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_bootstrap(State=#membership{node=Node, nodes=Nodes},
                OldFullMap, NewFullMap) ->
  case partitions:diff(OldFullMap, NewFullMap) of
  [] ->
      % no difference in pmaps
      {NewFullMap, State#membership{fullmap=NewFullMap}};
  TransferList when is_list(TransferList) ->
      ?LOG_DEBUG("~nBootstrap~nNode         : ~p~nTransferList :~n~p~n",
                 [Node, partitions:pp_diff(TransferList)]),
      case start_link(TransferList, Nodes, NewFullMap) of
      {ok, _Pid} ->
          start_transfers();
      Other -> throw(Other)
      end,

      % bootstrap has some stuff to do (async), so just give the state
      % passed in for now. end_bootstrap will be called with the resulting
      % state when it completes
      {OldFullMap, State};
  Other ->
      % probably occurs b/c T (# of nodes) < N currently.
      % more nodes joining should help avoid this error.
      ?LOG_ERROR("no_bootstrap - Other: ~p", [Other]),
      {NewFullMap, State#membership{fullmap=NewFullMap}}
  end.


end_bootstrap(#state{futurefullmap=FutureFullMap}) ->
  end_bootstrap(FutureFullMap);

end_bootstrap(NewFullMap) ->
  gen_server:call(membership, {newfullmap, NewFullMap}),
  stop().


start(TransferList, Nodes, FutureFullMap) ->
  gen_server:start({global, bootstrap_manager}, ?MODULE,
                   [TransferList, Nodes, FutureFullMap], []).


start_link(TransferList, Nodes, FutureFullMap) ->
  gen_server:start_link({global, bootstrap_manager}, ?MODULE,
                        [TransferList, Nodes, FutureFullMap], []).


stop() ->
  gen_server:cast({global, bootstrap_manager}, stop).


start_transfers() ->
  gen_server:cast({global, bootstrap_manager}, start_transfers).


transfers() ->
  gen_server:call({global, bootstrap_manager}, transfers).


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
init([TransferList, Nodes, FutureFullMap]) ->
  process_flag(trap_exit, true),
  {ok, #state{transfer_list=TransferList,nodes=Nodes,
              futurefullmap=FutureFullMap}}.


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
handle_call(average_transfer_rate, _From,
            State=#state{transfers=Transfers}) ->
  {Sum, Cardinality} = ets:foldl(
                         fun(#transfer{rate=Rate}, {Sum, Cardinality}) ->
                                 {Sum+Rate,Cardinality+1}
                         end, {0, 0}, Transfers),
  AverageRate = Sum / Cardinality,
  {reply, AverageRate, State};

handle_call(aggregate_transfer_rate, _From,
            State=#state{transfers=Transfers}) ->
  Sum = ets:foldl(fun(#transfer{rate=Rate}, Sum) ->
      Rate + Sum
    end, 0, Transfers),
  {reply, Sum, State};

handle_call(transfers, _From,
            State=#state{transfers=Transfers}) ->
  {reply, {ok, ets:tab2list(Transfers)}, State};

%% at least reply that this 'catch-all' was ignored
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.


%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(start_transfers,
            State=#state{transfer_list=TransferList}) ->
  Transfers = start_transfers(TransferList, State),
  {noreply, State#state{transfers=Transfers}};

handle_cast(_Msg, State) ->
  {noreply, State}.


%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------

handle_info({receiver_done, FromNode, _ToNode, Partition, DbName, Receiver},
            State = #state{transfers=Transfers}) ->
  %% TODO use bring_online & ToNode? instead of waiting until end & installing
  %%      NewFullMap into mem2

  %% handle the old file
  membership2:decommission_part(FromNode, Partition, DbName),

  %% remove from Transfers table
  case ets:lookup(Transfers, Partition) of
  [Transfer] = [#transfer{receivers=Receivers}] ->
      NewReceivers = lists:delete(Receiver, Receivers),
      if
        length(NewReceivers) == 0 -> ets:delete(Transfers, Partition);
        true -> ets:insert(Transfers, Transfer#transfer{receivers=NewReceivers})
      end;
  _ -> ok
  end,
  case ets:first(Transfers) of
  '$end_of_table' ->
      end_bootstrap(State),
      {noreply, State};
  _ -> {noreply, State}
  end;

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
start_transfers([], State) ->
  no_transfers, % no diff in pmaps, so no transfers
  end_bootstrap(State);

start_transfers(Diff, State=#state{nodes=Nodes}) ->
  case showroom_db:all_databases("") of
  {ok, AllDbs} when length(AllDbs) > 0 ->
      start_transfers(Diff, Nodes, configuration:get_config(), AllDbs,
                      ets:new(transfers, [public, set, {keypos, 2}]));
  {ok, []} -> end_bootstrap(State); % no databases, so bootstrap not needed
  Other -> throw(Other) % problem getting list of dbs
  end.


start_transfers([], _, _, _, Transfers) ->
  Transfers;

start_transfers([{FromNode, ToNode, Partition} | Diff], Nodes, Config,
                AllDbs, Transfers) ->
  membership2:take_offline(FromNode, Partition),
  Receivers = lists:map(
    fun(DbName) ->
        {ok, Receiver} =
          bootstrap_receiver:start_link(FromNode, ToNode, Partition,
                                        DbName, 10000, self()),
        Receiver
    end, AllDbs),
  % NOTE: by using AllDbs, we are omitting .deleted.couch files
  ets:insert(Transfers, #transfer{partition=Partition,
                                  receivers=Receivers}),
  start_transfers(Diff, Nodes, Config, AllDbs, Transfers).
