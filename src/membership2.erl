%%%-------------------------------------------------------------------
%%% File:      membership2.erl
%%% @author    Cliff Moon <cliff@powerset.com> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-05-04 by Cliff Moon
%%%-------------------------------------------------------------------
-module(membership2).
-author('cliff@powerset.com').
-author('brad@cloudant.com').

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, stop/1, check_nodes/0,
         partitions/0, partition_for_key/1, fullmap/0,
         all_nodes_parts/1, clock/0,
         nodes/0, nodeparts_for_key/1, nodes_for_part/1, nodes_for_part/2,
         nodes_for_shard/1, nodes_down/0,
         parts_for_node/1,
         take_offline/2, bring_online/2,
         decommission_part/3, pp_fullmap/0, snafu/1, snafu/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% includes
-include("../include/config.hrl").
-include("../include/common.hrl").
-include("../include/profile.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------

start_link(Node, Nodes) ->
  start_link(Node, Nodes, []).


start_link(Node, Nodes, Args) ->
  gen_server:start_link({local, membership}, ?MODULE, [Node, Nodes, Args], []).


stop(Server) ->
  gen_server:cast(Server, stop).


%% @doc for when things have really gone south.  Install a new state on all
%%      nodes, given a filename, or node list, partition map, and fullmap.
%% @end
snafu(Filename) ->
  NewState = case file:consult(Filename) of
  {ok, [Terms]} ->
      Terms;
  Error ->
      throw(Error)
  end,
  #membership{nodes=Nodes, partitions=PMap, fullmap=Fullmap} = NewState,
  snafu(Nodes, PMap, Fullmap).


snafu(Nodes, PMap, Fullmap) ->
  NewState = #membership{node=node(), nodes=Nodes,
      partitions=PMap, fullmap=Fullmap, version=vector_clock:create(dbcore)},
  update_ets(ets_name(node()), NewState),
  fire_gossip(node(), Nodes, NewState),
  save(NewState).


check_nodes() ->
  ErlangNodes = lists:usort([node() | erlang:nodes()]),
  {ok, MemNodeList} = membership2:nodes(),
  MemNodes = lists:usort(MemNodeList),
  {PMapNodeList, _PMapPartList} = lists:unzip(partitions()),
  PMapNodes = lists:usort(PMapNodeList),
  case ErlangNodes =:= MemNodes andalso
    ErlangNodes =:= PMapNodes andalso
    MemNodes =:= PMapNodes of
  true -> true;
  _ ->
      Msg = "membership: Node Lists do not match.~n"
        "Erlang Nodes     : ~p~n"
        "Membership Nodes : ~p~n"
        "PMap Nodes       : ~p~n",
      Lst = [ErlangNodes, MemNodes, PMapNodes],
      showroom_log:message(error, Msg, Lst),
      io:format(Msg, Lst),
      false
  end.


%% @doc retrieve the primary partition map.  This is a list of partitions and
%%      their corresponding primary node, no replication partner nodes.
partitions() ->
  ets_pmap().


%% @doc retrieve the full partition map, like above, but including replication
%%      partner nodes.  List should number 2^Q * N
fullmap() ->
  lists:keysort(2, ets_fullmap()).


%% @doc pretty-print the full partition map (sorted by node, then part)
pp_fullmap() ->
   lists:foreach(
     fun({N,P}) ->
         io:format("~-60s ~s~n", [N, showroom_utils:int_to_hexstr(P)])
     end,
     lists:sort(membership2:all_nodes_parts(true))).


%% @doc get the current vector clock from membership state
clock() ->
  gen_server:call(membership, clock).


%% @doc get the list of cluster nodes (according to membership module)
%%      This may differ from erlang:nodes()
nodes() ->
  gen_server:call(membership, nodes).


%% @doc get all the responsible nodes for a given partition, including
%%      replication partner nodes
nodes_for_part(Part) ->
  nodes_for_part(Part, all_nodes_parts(true)).


nodes_for_part(Part, NodePartList) ->
  Filtered = lists:filter(fun({_N, P}) -> P =:= Part end, NodePartList),
  {Nodes, _Parts} = lists:unzip(Filtered),
  lists:usort(Nodes).


nodes_for_shard(ShardName) when is_binary(ShardName) ->
    nodes_for_shard(binary_to_list(ShardName));

nodes_for_shard(ShardName) when is_list(ShardName) ->
    HexPart = case string:rchr(ShardName, $_) + 1 of
        1 -> ShardName;
        Last -> string:substr(ShardName, Last)
    end,
    Int = showroom_utils:hexstr_to_int(HexPart),
    {_, Parts} = lists:unzip(membership2:partitions()),
    nodes_for_part(partitions:int_to_partition(Int, Parts)).


%% @doc get all the responsible nodes and partitions for a given key, including
%%      nodes/parts on replication partner nodes
nodeparts_for_key(Key) ->
  int_node_parts_for_key(Key).


%% @doc get a list of all the nodes marked down in this node's fullmap
nodes_down() ->
  Downs = lists:foldl(fun({N,_P,{nodedown, _T}}, AccIn) -> [N|AccIn];
                         (_, AccIn) -> AccIn end, [], fullmap()),
  lists:usort(Downs).


%% @doc return the partition responsible for the given Key
partition_for_key(Key) ->
  Config = configuration:get_config(),
  Hash = lib_misc:hash(Key),
  partitions:hash_to_partition(Hash, Config#config.q).


%% @doc return the partitions that reside on a given node
parts_for_node(Node) ->
  lists:sort(lists:foldl(fun({N,P,_Type}, AccIn) ->
                             case N of
                                 Node -> [P | AccIn];
                                 _ -> AccIn
                             end
                         end, [], fullmap())).


%% @doc get all the nodes and partitions in the cluster.  Depending on the
%%      AllPartners param, you get only primary nodes or replication partner
%%      nodes, as well.
%%      No nodes/parts currently down are returned.
all_nodes_parts(false) ->
  ets_pmap();
all_nodes_parts(true) ->
  mem_utils:nodeparts_up(ets_fullmap()).


%% @doc If a local storage server exists for this partition it will be taken
%%      out of rotation until put back in.
%% @end
take_offline(Node, Partition) when Node =:= node() ->
    gen_server:call(membership, {take_offline, Partition});

take_offline(Node, Partition)->
    gen_server:call({membership, Node}, {take_offline, Partition}).


%% @doc Brings a storage server that has been taken offline back online.
%% @end
bring_online(Node, Partition) ->
  showroom_log:message(debug, "membership: bring_online Node: ~p Partition: ~p",
      [Node, Partition]),
  gen_server:call({membership, Node}, {bring_online, Partition}).


%% @doc cleans up the remaining .couch shard/partition file after it has been
%%      moved to a new node.
decommission_part(Node, Part, DbName) ->
  gen_server:cast({membership, Node}, {decommission, Part, DbName}).


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
init([Node, Nodes, Args]) ->
  process_flag(trap_exit,true),
  showroom_log:message(info, "membership: membership server starting...", []),
  Options = lists:flatten(Args),
  showroom_log:message(info, "membership: options ~p", [Options]),
  net_kernel:monitor_nodes(true),
  Config = configuration:get_config(),
  PersistentState=#membership{partitions=PersistentParts} = load(Node),
  PartnersPlus = replication:partners_plus(Node, Nodes),
  State =
    case mem_utils:use_persistent(PartnersPlus, PersistentParts) of
      false ->
        showroom_log:message(info, "membership: not using persisted state", []),
        % didn't find persistent state on disk or this node was nodedown
        % so we don't want to use persisted state
        PartialNodes = lists:usort(Nodes),
        {NewVersion, RemoteNodes, NewPMap1, NewFullMap1} =
          join_to(Node, PartnersPlus, Options),
        NewWorldNodes = lists:usort(PartialNodes ++ RemoteNodes),
        NewPMap = case NewPMap1 of
          [] -> partitions:create_partitions(Config#config.q, Node,
                                             NewWorldNodes);
          _ -> NewPMap1
        end,
        NewFullMap = case NewFullMap1 of
          [] -> make_all_nodes_parts(NewPMap);
          _ -> NewFullMap1
        end,
        #membership{
          node=Node,
          nodes=NewWorldNodes,
          partitions=lists:keysort(2,NewPMap),
          % version=vector_clock:increment(dbcore, NewVersion),
          version=NewVersion,
          fullmap=NewFullMap};
      _ ->
        % found persistent state on disk
        showroom_log:message(info, "membership: using persisted state", []),
        case Options of
          [] -> ok;
          _ ->
            showroom_log:message(info, "membership: options ~p ignored.", [Options])
        end,
        %% fire gossip even if state comes from disk
        fire_gossip(Node, Nodes, PersistentState),
        PersistentState
    end,
  save(State),
  % ets table is an optimization for cluster_ops performance
  Ets = ets:new(ets_name(Node), [public, set, named_table]),
  update_ets(Ets, State),
  {ok, State}.


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

%% join
handle_call({join, JoiningNode, Options}, _From,
            State = #membership{version=Version, node=Node, nodes=Nodes,
                                partitions=Partitions, fullmap=OldFullMap}) ->
  JoinType = mem_utils:join_type(JoiningNode, OldFullMap, Options),
  showroom_log:message(alert, "membership: node ~p wants to join, type '~p'",
                       [JoiningNode, JoinType]),
  {PMap, NewFullmap} = case JoinType of
  rejoin ->
    mem_utils:fix_mappings(rejoin, JoiningNode, OldFullMap);
  {replace, OldNode} ->
    mem_utils:fix_mappings(replace, {OldNode, JoiningNode}, OldFullMap);
  new ->
    Hints = proplists:get_value(hints, Options),
    PMap1 = case partitions:join(JoiningNode, Partitions, Hints) of
    {ok, Table} -> Table;
    {error, Error, _Table} -> throw({join_error, Error})
    end,
    Fullmap1 = make_all_nodes_parts(PMap1),
    {PMap1, Fullmap1}
  end,
  WorldNodes = lists:usort(Nodes ++ [JoiningNode]),
  NewVersion = vector_clock:increment(dbcore, Version),
  NewState1 = State#membership{nodes=WorldNodes, partitions=PMap,
                              version=NewVersion},
  {Fullmap, NewState2} = case proplists:get_value(bootstrap, Options) of
    true ->
      % join not complete until bootstrap finishes,
      % so this NewState isn't the final (i.e. NewState1 will be installed)
      showroom_log:message(info, "membership: bootstrap process starting", []),
      bootstrap_manager:start_bootstrap(NewState1, OldFullMap, NewFullmap);
    _ ->
      % no bootstrap, so install NewFullmap now
      showroom_log:message(info, "membership: no bootstrap", []),
      {NewFullmap, NewState1#membership{fullmap=NewFullmap}}
  end,
  save(NewState2),
  update_ets(ets_name(node()), NewState2),
  notify(node_join, [JoiningNode]),
  fire_gossip(Node, WorldNodes, NewState2),
  % If we're bootstrapping, then the join is not complete.
  % So return FullMap for now.  bootstrap_manager:end_bootstrap will fix it
  {reply, {ok, NewVersion, WorldNodes, PMap, Fullmap}, NewState2};

%% clock
handle_call(clock, _From, State = #membership{version=Version}) ->
  {reply, Version, State};

%% state
handle_call(state, _From, State) ->
  {reply, State, State};

%% newfullmap
handle_call({newfullmap, NewFullMap}, _From,
            State = #membership{node=Node, nodes=Nodes, version=Version}) ->
  NewVersion = vector_clock:increment(dbcore, Version),
  NewState = State#membership{version=NewVersion, fullmap=NewFullMap},
  save(NewState),
  update_ets(ets_name(node()), NewState),
  fire_gossip(Node, Nodes, NewState),
  {reply, installed, NewState};

%% partitions
handle_call(partitions, _From, State = #membership{partitions=Parts}) ->
  {reply, {ok, Parts}, State};

%% fullmap
handle_call(fullmap, _From, State = #membership{fullmap=FullMap}) ->
  {reply, {ok, FullMap}, State};

%% nodes
handle_call(nodes, _From, State = #membership{nodes=Nodes}) ->
  {reply, {ok, Nodes}, State};

%% take_offline
handle_call({take_offline, Partition}, _From,
            State = #membership{node=Node, nodes=Nodes, fullmap=OldFullMap}) ->
  showroom_log:message(info, "membership: take_offline Node: ~p Partition: ~p",
      [Node, Partition]),
  NewFullMap = mem_utils:remove_partition(OldFullMap, Node, Partition),
  NewState = State#membership{fullmap=NewFullMap},
  fire_gossip(Node, Nodes, NewState),
  update_ets(ets_name(node()), NewState),
  {reply, {offline, Node, Partition}, NewState};

%% at least reply that this 'catch-all' was ignored
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.


%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------

handle_cast({gossip, RemoteState = #membership{node=RemoteNode}},
            LocalState = #membership{node=_Me}) ->
  showroom_log:message(info, "membership: received gossip from ~p",
    [RemoteNode]),
  {MergeType, MergedState = #membership{nodes=_MergedNodes}} =
    merge_state(RemoteState, LocalState),
  case MergeType of
  equal -> {noreply, MergedState};
  merged ->
      showroom_log:message(info, "membership: merged new gossip", []),
      % fire_gossip(Me, MergedNodes, MergedState),
      update_ets(ets_name(node()), MergedState),
      save(MergedState),
      {noreply, MergedState}
  end;

% decommission
% renaming for now, until case 1245 can be completed
handle_cast({decommission, Part, DbName}, State) ->
  {{Y,Mon,D}, {H,Min,S}} = calendar:universal_time(),
  Directory = couch_config:get("couchdb", "database_dir"),
  OrigFilename = showroom_utils:full_filename(Part, DbName, Directory),
  Moved = lists:flatten(io_lib:format(".~w~2.10.0B~2.10.0B." ++
        "~2.10.0B~2.10.0B~2.10.0B.moved.couch", [Y,Mon,D,H,Min,S])),
  % Note: this MovedFilename bit below gives weird results:
  %  ["/Users/brad/dev/erlang/dbcore/tmp/lib/x800000/test_800000",
  %   ".20091001.162640.moved.couch"] but list/string behavior handles it.
  MovedFilename = lists:map(fun(E) -> binary_to_list(E) end,
                            re:replace(OrigFilename, "\.couch", Moved, [])),
  ok = file:rename(OrigFilename, MovedFilename),
  {noreply, State}.


%% @doc handle nodedown messages because we have
%%      net_kernel:monitor_nodes(true)
handle_info({nodedown, Node},
            State = #membership{nodes=OldNodes, fullmap=OldFullmap,
                                version=OldVersion}) ->
  showroom_log:message(alert, "membership: nodedown from ~p", [Node]),
  case lists:member(Node, OldNodes) of
  true ->
      notify(nodedown, [Node]),
      % clean up membership state
      Nodes = lists:delete(Node, OldNodes),
      {PMap, Fullmap} = mem_utils:fix_mappings(nodedown, Node, OldFullmap),
      % Do we increment clock here?  w/o gossip?
      % This is happening near simultaneously on the other nodes, too :\
      % Only reason to increment is persisted clock on down node will be older
      % when it returns
      Version = vector_clock:increment(dbcore, OldVersion),
      NewState = State#membership{nodes=Nodes, partitions=PMap, fullmap=Fullmap,
                                  version=Version},
      update_ets(ets_name(node()), NewState),
      save(NewState),
      {noreply, NewState};
  _ -> {noreply, State}
  end;

%% @doc handle nodeup messages because we have
%%      net_kernel:monitor_nodes(true)
handle_info({nodeup, Node}, State) ->
  showroom_log:message(alert, "membership: nodeup Node: ~p", [Node]),
  {noreply, State};

handle_info(Info, State) ->
  showroom_log:message(info, "membership: handle_info Info: ~p", [Info]),
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

%% 0.5.6 to 0.5.7
code_change(184380560337424323902805568963460261434, State, _Extra) ->
    backup_old_config_file(),
    % update State to the new version
    {membership, _Hdr, Node, Nodes, PMap, Version} = State,
    NewState = #membership{
        node = Node,
        nodes = Nodes,
        partitions = PMap,
        version = Version,
        fullmap = make_all_nodes_parts(PMap)
    },
    save(NewState),
    % also create new ets table
    Ets = ets:new(ets_name(Node), [public, set, named_table]),
    update_ets(Ets, NewState),
    {ok, NewState};

%% 0.8.8 to 0.9.0
code_change(239470595681156900105628017899543243419, State, _Extra) ->
  net_kernel:monitor_nodes(true),
  {ok, State};

code_change(OldVsn, State, _Extra) ->
  io:format("Unknown Old Version!~nOldVsn: ~p~nState : ~p~n", [OldVsn, State]),
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

backup_old_config_file() ->
  Config = configuration:get_config(),
  FileName = filename:join([Config#config.directory,
                            lists:concat([node:name(node()), ".state"])]),
  BackupName = filename:join([Config#config.directory,
                            lists:concat([node:name(node()), ".state.bak"])]),
  file:copy(FileName, BackupName).


%% return State from membership file
load(Node) ->
  Config = configuration:get_config(),
  case file:consult(filename:join([Config#config.directory,
           lists:concat([node:name(Node), ".state"])])) of
    {error, Reason} ->
      showroom_log:message(info, "membership: could not load state: ~p~n",
          [Reason]),
      #membership{nodes=[]};
    {ok, [Terms]} ->
      Terms
  end.


%% save the State to a file
save(State) ->
  Config = configuration:get_config(),
  Filename = filename:join([Config#config.directory,
               lists:concat([node:name(State#membership.node), ".state"])]),
  {ok, File} = file:open(Filename, [binary, write]),
  io:format(File, "~w.~n", [State]),
  file:close(File).


%% joining is bi-directional, as opposed to gossip which is unidirectional
%% we want to collect the list of known nodes to compute the partition map
%% which isn't necessarily the same as the list of running nodes
join_to(Node, Partners, Options) ->
  join_to(Node, Partners,
          {vector_clock:create(dbcore), [], [], []}, Options).


%% @doc join this node to one of its partners (or PartnersPlus if no partners
%%      are available).
join_to(_, [], {Version, World, PMap, FullMap}, _Options) ->
  {Version, World, PMap, FullMap};

join_to(Node, [Partner|Rest], {Version, World, PMap, FullMap}, Options) ->
  case call_join(Partner, Node, Options) of
    {ok, RemoteVersion, NewNodes, NewPMap, NewFullMap} ->
      {vector_clock:merge(Version, RemoteVersion),
       lists:usort(World ++ NewNodes),
       NewPMap,
       NewFullMap};
    Other ->
      showroom_log:message(info, "membership: join_to Other: ~p~n", [Other]),
      join_to(Node, Rest, {Version, World, PMap, FullMap}, Options)
  end.


%% @doc make the join call to Remote node (usually a partner of Node)
call_join(Remote, Node, Options) ->
  showroom_log:message(info, "membership: call_join From: ~p To: ~p",
      [Node, Remote]),
  catch gen_server:call({membership, node:name(Remote)},
                        {join, Node, Options}).


merge_state(_RemoteState=#membership{version=RemoteVersion, nodes=RemoteNodes,
                                    partitions=RemotePMap,
                                    fullmap=RemoteFullMap},
            LocalState=#membership{version=LocalVersion, nodes=LocalNodes,
                                   partitions=LocalPMap,
                                   fullmap=LocalFullMap}) ->
    case vector_clock:equals(RemoteVersion, LocalVersion) of
    true ->
            {equal, LocalState};
    false ->
        % Note, we're matching MergedVersion from these funs.
        % They should be the same.
        {MergedVersion, MergedNodes} =
            merge_nodes(RemoteVersion, RemoteNodes, LocalVersion, LocalNodes),
        {MergedVersion, MergedPMap} =
            merge_pmaps(RemoteVersion, RemotePMap, LocalVersion, LocalPMap),
        {MergedVersion, MergedFullMap} =
            merge_fullmaps(RemoteVersion, RemoteFullMap,
                           LocalVersion, LocalFullMap),

        % notify of arrivals & departures
        Arrived = MergedNodes -- LocalNodes,
        notify(node_join, Arrived),
        % Departed = LocalNodes -- MergedNodes,
        % notify(node_leave, Departed),

        {merged, LocalState#membership{version=MergedVersion, nodes=MergedNodes,
                                       partitions=MergedPMap,
                                       fullmap=MergedFullMap}}
    end.


merge_nodes(RemoteVersion, RemoteNodes, LocalVersion, LocalNodes) ->
  {MergedVersion, Merged} = vector_clock:resolve({RemoteVersion, RemoteNodes},
                                                  {LocalVersion, LocalNodes}),
  {MergedVersion, lists:usort(Merged)}.


merge_pmaps(RemoteVersion, RemotePMap, LocalVersion, LocalPMap) ->
  {MergedVersion, Merged} = vector_clock:resolve({RemoteVersion, RemotePMap},
                                                  {LocalVersion, LocalPMap}),
  {MergedVersion, lists:ukeysort(2, Merged)}.


merge_fullmaps(RemoteVersion, RemoteFullMap, LocalVersion, LocalFullMap) ->
  {MergedVersion, Merged} = vector_clock:resolve({RemoteVersion, RemoteFullMap},
                                                  {LocalVersion, LocalFullMap}),
  {MergedVersion, lists:usort(Merged)}.


notify(Type, Nodes) ->
  lists:foreach(fun(Node) ->
                    gen_event:notify(membership_events, {Type, Node})
                end, Nodes).


%% @doc fires a gossip message (membership state) to partners nodes in the
%%      cluster.
%% @end
fire_gossip(Me, WorldNodes, Gossip) ->
  % GossipPartners = partners_plus(Me, WorldNodes),
  % random experiment, gossip with all nodes, not just partners_plus
  GossipPartners = lists:delete(Me, WorldNodes),
  lists:foreach(fun(TargetNode) ->
      showroom_log:message(info, "membership: firing gossip from ~p to ~p",
          [Me, TargetNode]),
      gen_server:cast({membership, TargetNode}, {gossip, Gossip})
  end, GossipPartners).


%% @doc construct a table with all partitions, with the primary node and all
%%      replication partner nodes as well.
make_all_nodes_parts(PMap) ->
  {Nodes, _Parts} = lists:unzip(PMap),
  NodeParts = lists:flatmap(
    fun({Node,Part}) ->
        Partners = replication:partners(Node, lists:usort(Nodes)),
        PartnerList = [{Partner, Part, partner} || Partner <- Partners],
        [{Node, Part, primary} | PartnerList]
    end, PMap),
  NodeParts.


%% @doc for the given key, return a list of {Node,Part} tuples.  Nodes are both
%%      primary and replication partner nodes, and should number N.
int_node_parts_for_key(Key) ->
  Config = configuration:get_config(),
  Hash = lib_misc:hash(Key),
  Part = partitions:hash_to_partition(Hash, Config#config.q),
  NodePartList = all_nodes_parts(true),
  lists:filter(fun({_N,P}) -> P =:= Part end, NodePartList).


%% ets table helper functions
ets_name(Node) ->
    list_to_atom(lists:concat(["mem_", atom_to_list(Node)])).


update_ets(Table, #membership{partitions=PMap, fullmap=FullMap}) ->
    ets:insert(Table, {pmap, PMap}),
    ets:insert(Table, {fullmap, FullMap}),
    ok.


ets_pmap() ->
  [{pmap, PMap}] = ets:lookup(ets_name(node()), pmap),
  PMap.


ets_fullmap() ->
  [{fullmap, FullMap}] = ets:lookup(ets_name(node()), fullmap),
  FullMap.
