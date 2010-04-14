%%% membership module
%%%
%%% State of the gen_server is a #mem record
%%%
%%% Nodes and Gossip are the same thing, and are a list of three-tuples like:
%%%
%%%  [ {Pos,NodeName,Options} | _ ]
%%%
%%%  Position is 1-based incrementing in order of node joining
%%%
%%%  Options is a proplist, with [{hints, [Part1|_]}] denoting that the node
%%%   is responsible for the extra partitions too.
%%%
%%% TODO: dialyzer type specs
%%%
-module(mem3).
-author('brad@cloudant.com').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0, stop/1, reset/0]).
-export([join/2, clock/0, state/0]).
-export([partitions/0, fullmap/0]).
-export([nodes/0, nodes_for_part/1, nodes_for_part/2, all_nodes_parts/1]).
-export([parts_for_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% includes
-include("../include/config.hrl").
-include("../include/common.hrl").

-define(SERVER, membership).

%% types - stick somewhere in includes?
-type join_type() :: first | new | replace.
-type join_order() :: non_neg_integer().
-type options() :: list().
-type mem_node() :: {join_order(), node(), options()}.
-type mem_node_list() :: [mem_node()].
-type arg_options() :: {test, boolean()} | {config, #config{}}.
-type args() :: [] | [arg_options()].
-type mem_state() :: #mem{}.
-type epoch() :: float().
-type clock() :: {node(), epoch()}.
-type vector_clock() :: [clock()].

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    start_link([]).


-spec start_link(args()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).


-spec stop() -> ok.
stop() ->
    stop(?MODULE).


-spec stop(atom()) -> ok.
stop(Server) ->
    gen_server:cast(Server, stop).


-spec join(join_type(), mem_node_list()) -> ok.
join(JoinType, Nodes) ->
    gen_server:call(?SERVER, {join, JoinType, Nodes}).


-spec clock() -> vector_clock().
clock() ->
    gen_server:call(?SERVER, clock).


-spec state() -> mem_state().
state() ->
    gen_server:call(?SERVER, state).


-spec reset() -> ok | not_reset.
reset() ->
    gen_server:call(?SERVER, reset).


%% @doc retrieve the primary partition map.  This is a list of partitions and
%%      their corresponding primary node, no replication partner nodes.
partitions() ->
    mochiglobal:get(pmap).


%% @doc retrieve the full partition map, like above, but including replication
%%      partner nodes.  List should number 2^Q * N
fullmap() ->
    lists:keysort(2, mochiglobal:get(fullmap)).


%% @doc get the list of cluster nodes (according to membership module)
%%      This may differ from erlang:nodes()
nodes() ->
  gen_server:call(?SERVER, nodes).


%% @doc get all the responsible nodes for a given partition, including
%%      replication partner nodes
nodes_for_part(Part) ->
    nodes_for_part(Part, mochiglobal:get(fullmap)).


nodes_for_part(Part, NodePartList) ->
    Filtered = lists:filter(fun({_N, P}) -> P =:= Part end, NodePartList),
    {Nodes, _Parts} = lists:unzip(Filtered),
    lists:usort(Nodes).


%% @doc return the partitions that reside on a given node
parts_for_node(Node) ->
    lists:sort(lists:foldl(fun({N,P}, AccIn) ->
        case N of
        Node -> [P | AccIn];
        _ -> AccIn
        end
    end, [], mochiglobal:get(fullmap))).


%% @doc get all the nodes and partitions in the cluster.  Depending on the
%%      AllPartners param, you get only primary nodes or replication partner
%%      nodes, as well.
%%      No nodes/parts currently down are returned.
all_nodes_parts(false) ->
    mochiglobal:get(pmap);
all_nodes_parts(true) ->
    mochiglobal:get(fullmap).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% start up membership server
-spec init(args()) -> {ok, mem_state()}.
init(Args) ->
    process_flag(trap_exit,true),
    Config = get_config(Args),
    Test = proplists:get_value(test, Args),
    OldState = read_latest_state_file(Test, Config),
    State = handle_init(Test, OldState),
    {ok, State#mem{args=Args}}.


%% new node(s) joining to this node
handle_call({join, JoinType, ExtNodes}, _From,
            #mem{args=Args} = State) ->
    Config = get_config(Args),
    try
        NewState = handle_join(JoinType, ExtNodes, State, Config),
        gossip(NewState),
        {reply, ok, NewState}
    catch _:Error ->
        showroom_log:message(error, "~p", [Error]),
        {reply, Error, State}
    end;

%% clock
handle_call(clock, _From, #mem{clock=Clock} = State) ->
    {reply, {ok, Clock}, State};

%% state
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};

%% reset - but only if we're in test mode
handle_call(reset, _From, #mem{args=Args} = State) ->
    Test = proplists:get_value(test, Args),
    case Test of
    undefined -> {reply, not_reset, State};
    _ ->
        mochiglobal:delete(pmap),
        mochiglobal:delete(fullmap),
        {reply, ok, int_reset(Test, State)}
    end;

%% nodes
handle_call(nodes, _From, #mem{nodes=NodeList} = State) ->
    {_,Nodes,_} = lists:unzip3(NodeList),
    {reply, {ok, Nodes}, State};

%% ignored call
handle_call(Msg, _From, State) ->
    showroom_log:message(info, "membership: ignored call: ~p", [Msg]),
    {reply, ignored, State}.


%% stop
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({gossip, #mem{node=RemoteNode} = RemoteState}, LocalState) ->
    showroom_log:message(info, "membership: received gossip from ~p",
        [RemoteNode]),
    {MergeType, MergedState} = merge_state(RemoteState, LocalState),
    case MergeType of
    equal -> {noreply, MergedState};
    merged ->
        showroom_log:message(info, "membership: merged new gossip", []),
        update_cache(MergedState),
        gossip(MergedState),
        {noreply, MergedState}
    end;

%% ignored cast
handle_cast(Msg, State) ->
    showroom_log:message(info, "membership: ignored cast: ~p", [Msg]),
    {noreply, State}.


%% @doc handle nodedown messages because we have
%%      net_kernel:monitor_nodes(true)
handle_info({nodedown, Node}, State) ->
    showroom_log:message(alert, "membership: nodedown from ~p", [Node]),
    {noreply, State};

%% @doc handle nodeup messages because we have
%%      net_kernel:monitor_nodes(true)
handle_info({nodeup, Node}, State) ->
    showroom_log:message(alert, "membership: nodeup Node: ~p", [Node]),
    {noreply, State};

%% ignored info
handle_info(Info, State) ->
    showroom_log:message(info, "membership: ignored info: ~p", [Info]),
    {noreply, State}.


% terminate
terminate(_Reason, _State) ->
    ok.


% ignored code change
code_change(OldVsn, State, _Extra) ->
    io:format("Unknown Old Version~nOldVsn: ~p~nState : ~p~n", [OldVsn, State]),
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc if Args has config use it, otherwise call configuration module
%%      most times Args will have config during testing runs
get_config(Args) ->
    case proplists:get_value(config, Args) of
    undefined -> configuration:get_config();
    Any -> Any
    end.


% we could be automatically:
%  1. rejoining a cluster after some downtime
%
% we could be manually:
%  2. beginning a cluster with only this node
%  3. joining a cluster as a new node
%  4. replacing a node in an existing cluster

handle_init(Test, nil) ->
    showroom_log:message(info, "membership: membership server starting...", []),
    net_kernel:monitor_nodes(true),
    int_reset(Test);

handle_init(_Test, _OldState) ->
    ?debugHere,
    % there's an old state, let's try to rejoin automatically
    %  but only if we can compare our old state to all other
    %  available nodes and get a match... otherwise get a human involved
    % TODO implement me
    #mem{}.


%% handle join activities, return NewState
handle_join(JoinType, ExtNodes,
            #mem{node=Node, nodes=Nodes, clock=Clock} = State, Config)
  when JoinType == first  orelse JoinType == new ->
    {Pmap, Fullmap} = create_maps(Config, JoinType, ExtNodes, Nodes),
    update_cache(Pmap, Fullmap),
    NewClock = vector_clock:increment(Node, Clock),
    State#mem{nodes=ExtNodes, clock=NewClock};

handle_join(replace, [_OldNode | _], _State, _Config) ->
    % TODO implement me
    ok;

handle_join(JoinType, _, _, _) ->
    showroom_log:message(info, "membership: unknown join type: ~p", [JoinType]),
    {error, {unknown_join_type, JoinType}}.


gossip(#mem{args=Args} = NewState) ->
    Test = proplists:get_value(test, Args),
    gossip(Test, NewState).


gossip(undefined, #mem{node=Node, nodes=StateNodes} = NewState) ->
    {_, Nodes, _} = lists:unzip3(StateNodes),
    PartnersPlus = replication:partners_plus(Node, Nodes),
    lists:foreach(fun(TargetNode) ->
        showroom_log:message(info, "membership: firing gossip from ~p to ~p",
        [Node, TargetNode]),
        gen_server:cast({?MODULE, TargetNode}, {gossip, NewState})
    end, PartnersPlus);
gossip(_,_) ->
    % testing, so don't gossip
    ok.

%% @doc find the latest state file on disk
find_latest_state_filename(Config) ->
    Dir = Config#config.directory,
    case file:list_dir(Dir) of
    {ok, Filenames} ->
        Timestamps = [list_to_integer(TS) || {"state", TS} <-
           [list_to_tuple(string:tokens(FN, ".")) || FN <- Filenames]],
        SortedTimestamps = lists:reverse(lists:sort(Timestamps)),
        case SortedTimestamps of
        [Latest | _] ->
            {ok, Dir ++ "/state." ++ integer_to_list(Latest)};
        _ ->
            throw({error, not_found})
        end;
    {error, Reason} ->
        throw({error, Reason})
    end.


%% (Test, Config)
read_latest_state_file(undefined, Config) ->
    try
        {ok, File} = find_latest_state_filename(Config),
        case file:consult(File) of
        {ok, #mem{}=State} -> State;
        _Else -> throw({error, bad_mem_state_file})
        end
    catch _:Error ->
        showroom_log:message(info, "membership: ~p", [Error]),
        nil
    end;
read_latest_state_file(_, _) ->
    nil.


%% @doc given Config and a list of ExtNodes, construct a {Pmap,Fullmap}
%%      This is basically replaying all the mem events that have happened.
create_maps(#config{q=Q} = Config, JoinType, ExtNodes, Nodes) ->
    [{_,FirstNode,_}|_] = ExtNodes,
    Fun = fun({Pos, Node, Options}, Pmap) ->
        check_pos(Pos, Node, Nodes),
        Hints = proplists:get_value(hints, Options),
        {ok, NewPmap} = partitions:join(Node, Pmap, Hints),
        NewPmap
    end,
    Acc0 = case JoinType of
    first -> partitions:create_partitions(Q, FirstNode);
    new -> mochiglobal:get(pmap)
    end,
    Pmap = lists:foldl(Fun, Acc0, lists:keysort(1, ExtNodes)),
    {Pmap, make_fullmap(Pmap, Config)}.


%% @doc construct a table with all partitions, with the primary node and all
%%      replication partner nodes as well.
make_fullmap(PMap, Config) ->
    {Nodes, _Parts} = lists:unzip(PMap),
    NodeParts = lists:flatmap(
        fun({Node,Part}) ->
            Partners = replication:partners(Node, lists:usort(Nodes), Config),
            PartnerList = [{Partner, Part} || Partner <- Partners],
            [{Node, Part} | PartnerList]
        end, PMap),
    NodeParts.


%% cache table helper functions
update_cache(#mem{nodes=Nodes, args=Args}) ->
    Config = get_config(Args),
    {Pmap, Fullmap} = create_maps(Config, first, Nodes, []),
    update_cache(Pmap, Fullmap).


update_cache(Pmap, Fullmap) ->
    mochiglobal:put(pmap, Pmap),
    mochiglobal:put(fullmap, Fullmap).


check_pos(Pos, Node, Nodes) ->
    Found = lists:keyfind(Pos, 1, Nodes),
    case Found of
    false -> ok;
    _ ->
        {_,OldNode,_} = Found,
        if
        OldNode =:= Node ->
            throw({error, {node_exists_at_position, Pos, Node}});
        true ->
            throw({error, {position_exists, Pos, OldNode}})
        end
    end.


int_reset(Test) ->
    int_reset(Test, #mem{}).


int_reset(Test, State) ->
    Node = case Test of
    undefined -> node();
    _ -> Test
    end,
    Nodes = [{0, Node, []}],
    Clock = vector_clock:create(Node),
    State#mem{node=Node, nodes=Nodes, clock=Clock}.


merge_state(_RemoteState=#mem{clock=RemoteClock, nodes=RemoteNodes},
            LocalState=#mem{clock=LocalClock, nodes=LocalNodes}) ->
    case vector_clock:equals(RemoteClock, LocalClock) of
    true ->
        {equal, LocalState};
    false ->
        {MergedClock, MergedNodes} =
            merge_nodes(RemoteClock, RemoteNodes, LocalClock, LocalNodes),

%        % notify of arrivals & departures
%        Arrived = MergedNodes -- LocalNodes,
%        notify(node_join, Arrived),
%        Departed = LocalNodes -- MergedNodes,
%        notify(node_leave, Departed),

        {merged, LocalState#mem{clock=MergedClock, nodes=MergedNodes}}
    end.


merge_nodes(RemoteClock, RemoteNodes, LocalClock, LocalNodes) ->
    {MergedClock, Merged} =
        vector_clock:resolve({RemoteClock, RemoteNodes},
                             {LocalClock, LocalNodes}),
    {MergedClock, lists:keysort(1, Merged)}.


% notify(Type, Nodes) ->
%   lists:foreach(fun(Node) ->
%                     gen_event:notify(membership_events, {Type, Node})
%                 end, Nodes).
