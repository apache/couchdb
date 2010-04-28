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
-export([join/3, clock/0, state/0, start_gossip/0]).
-export([partitions/0, fullmap/0]).
-export([nodes/0, nodes_for_part/1, nodes_for_part/2, all_nodes_parts/1]).
-export([parts_for_node/1]).

%% for testing more than anything else
-export([merge_nodes/2, next_up_node/1, next_up_node/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% includes
-include("../include/config.hrl").
-include("../include/common.hrl").

-define(SERVER, membership).
-define(STATE_FILE_PREFIX, "membership").

%% types - stick somewhere in includes?
-type join_type() :: first | new | replace.
-type join_order() :: non_neg_integer().
-type options() :: list().
-type mem_node() :: {join_order(), node(), options()}.
-type mem_node_list() :: [mem_node()].
-type config() :: #config{}.
-type arg_options() :: {test, boolean()} | {config, config()}.
-type args() :: [] | [arg_options()].
-type mem_state() :: #mem{}.
-type test() :: undefined | node().
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


-spec join(join_type(), mem_node_list(), node() | nil) -> ok.
join(JoinType, Nodes, PingNode) ->
    gen_server:call(?SERVER, {join, JoinType, Nodes, PingNode}).


-spec clock() -> vector_clock().
clock() ->
    gen_server:call(?SERVER, clock).


-spec state() -> mem_state().
state() ->
    gen_server:call(?SERVER, state).


-spec start_gossip() -> ok.
start_gossip() ->
    gen_server:call(?SERVER, start_gossip).


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
    Test = get_test(Args),
    OldState = read_latest_state_file(Test, Config),
    showroom_log:message(info, "membership: membership server starting...", []),
    net_kernel:monitor_nodes(true),
    State = handle_init(Test, OldState),
    {ok, State#mem{args=Args}}.


%% new node(s) joining to this node
handle_call({join, JoinType, ExtNodes, PingNode}, _From, State) ->
    % NewState = handle_join(JoinType, ExtNodes, PingNode, State),
    % {reply, ok, NewState};
    try
        NewState = handle_join(JoinType, ExtNodes, PingNode, State),
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
    Test = get_test(Args),
    case Test of
    undefined -> {reply, not_reset, State};
    _ -> {reply, ok, int_reset(Test, State)}
    end;

%% nodes
handle_call(nodes, _From, #mem{nodes=Nodes} = State) ->
    {_,NodeList,_} = lists:unzip3(Nodes),
    {reply, {ok, NodeList}, State};

%% gossip
handle_call({gossip, RemoteState}, {Pid,_Tag} = From, LocalState) ->
    showroom_log:message(info, "membership: received gossip from ~p",
                         [erlang:node(Pid)]),
    handle_gossip(From, RemoteState, LocalState);

% start_gossip
handle_call(start_gossip, _From, State) ->
    NewState = gossip(State),
    {reply, ok, NewState};

%% ignored call
handle_call(Msg, _From, State) ->
    showroom_log:message(info, "membership: ignored call: ~p", [Msg]),
    {reply, ignored, State}.


%% stop
handle_cast(stop, State) ->
    {stop, normal, State};

%% ignored cast
handle_cast(Msg, State) ->
    showroom_log:message(info, "membership: ignored cast: ~p", [Msg]),
    {noreply, State}.


%% @doc handle nodedown messages because we have
%%      net_kernel:monitor_nodes(true)
handle_info({nodedown, Node}, State) ->
    showroom_log:message(alert, "membership: nodedown ~p", [Node]),
    {noreply, State};

%% @doc handle nodeup messages because we have
%%      net_kernel:monitor_nodes(true)
handle_info({nodeup, Node}, State) ->
    showroom_log:message(alert, "membership: nodeup   ~p", [Node]),
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


get_test(Args) ->
    proplists:get_value(test, Args).


% we could be automatically:
%  1. rejoining a cluster after some downtime
%
% we could be manually:
%  2. beginning a cluster with only this node
%  3. joining a cluster as a new node
%  4. replacing a node in an existing cluster

handle_init(Test, nil) ->
    int_reset(Test);

handle_init(_Test, #mem{nodes=Nodes, args=Args} = OldState) ->
    % there's an old state, let's try to rejoin automatically
    %  but only if we can compare our old state to other available
    %  nodes and get a match... otherwise get a human involved
    {_, NodeList, _} = lists:unzip3(Nodes),
    ping_all_yall(NodeList),
    {RemoteStates, _BadNodes} = get_remote_states(NodeList),
    Test = get_test(Args),
    case compare_state_with_rest(OldState, RemoteStates) of
    match ->
        showroom_log:message(info, "membership: rejoined successfully", []),
        OldState;
    Other ->
        showroom_log:message(error, "membership: rejoin failed: ~p", [Other]),
        int_reset(Test)
    end.


%% handle join activities, return NewState
handle_join(first, ExtNodes, nil, State) ->
    {_,Nodes,_} = lists:unzip3(ExtNodes),
    ping_all_yall(Nodes),
    int_join(ExtNodes, State);

handle_join(new, ExtNodes, PingNode, #mem{args=Args} = State) ->
    NewState = case get_test(Args) of
    undefined ->
        % ping the PingNode and get its state
        pong = net_adm:ping(PingNode),
        timer:sleep(1000), % let dist. erl get set up... sigh.
        {ok, RemoteState} = rpc:call(PingNode, mem3, state, []),
        RemoteState;
    _ ->
        % testing, so meh
        State
    end,
    % now use this info to join the ring
    int_join(ExtNodes, NewState);

handle_join(replace, [_OldNode | _], _PingNode, _State) ->
    % TODO implement me
    ok;

handle_join(JoinType, _, PingNode, _) ->
    showroom_log:message(info, "membership: unknown join type: ~p "
                         "for ping node: ~p", [JoinType, PingNode]),
    {error, {unknown_join_type, JoinType}}.


int_join(ExtNodes, #mem{nodes=Nodes, clock=Clock} = State) ->
    NewNodes = lists:foldl(fun({Pos, N, _Options}=New, AccIn) ->
        check_pos(Pos, N, Nodes),
        [New|AccIn]
    end, Nodes, ExtNodes),
    NewNodes1 = lists:sort(NewNodes),
    NewClock = vector_clock:increment(node(), Clock),
    NewState = State#mem{nodes=NewNodes1, clock=NewClock},
    install_new_state(NewState),
    NewState.


%% @doc handle the gossip messages
%%      We're not using vector_clock:resolve b/c we need custom merge strategy
handle_gossip(From, RemoteState=#mem{clock=RemoteClock},
              LocalState=#mem{clock=LocalClock}) ->
    case vector_clock:compare(RemoteClock, LocalClock) of
    equal ->
        {reply, ok, LocalState};
    less ->
        % remote node needs updating
        {reply, {new_state, LocalState}, LocalState};
    greater ->
        % local node needs updating
        gen_server:reply(From, ok), % reply to sender first
        {noreply, install_new_state(RemoteState)};
    concurrent ->
        % ick, so let's resolve and merge states
        showroom_log:message(info,
            "membership: Concurrent Clocks~n"
            "RemoteState : ~p~nLocalState : ~p~n"
            , [RemoteState, LocalState]),
        MergedState = merge_states(RemoteState, LocalState),
        gen_server:reply(From, {new_state, MergedState}), % reply to sender
        {noreply, install_new_state(MergedState)}
    end.


merge_states(#mem{clock=RemoteClock, nodes=RemoteNodes} = _RemoteState,
             #mem{clock=LocalClock, nodes=LocalNodes} = LocalState) ->
    MergedClock = vector_clock:merge(RemoteClock, LocalClock),
    MergedNodes = merge_nodes(RemoteNodes, LocalNodes),
    LocalState#mem{clock=MergedClock, nodes=MergedNodes}.


%% this will give one of the lists back, deterministically
merge_nodes(Remote, Local) ->
    % get rid of the initial 0 node if it's still there, and sort
    Remote1 = lists:usort(lists:keydelete(0,1,Remote)),
    Local1 = lists:usort(lists:keydelete(0,1,Local)),
    % handle empty lists as well as other cases
    case {Remote1, Local1} of
    {[], L} -> L;
    {R, []} -> R;
    _ -> erlang:min(Remote1, Local1)
    end.


gossip(#mem{args=Args} = NewState) ->
    Test = get_test(Args),
    gossip(Test, NewState).


-spec gossip(test(), mem_state()) -> mem_state().
gossip(undefined, #mem{nodes=StateNodes} = State) ->
    {_, Nodes, _} = lists:unzip3(StateNodes),
    TargetNode = next_up_node(Nodes),
    showroom_log:message(info, "membership: firing gossip from ~p to ~p",
        [node(), TargetNode]),
    case gen_server:call({?SERVER, TargetNode}, {gossip, State}) of
    ok -> State;
    {new_state, NewState} -> NewState;
    Error -> throw({unknown_gossip_response, Error})
    end;

gossip(_,_) ->
    % testing, so don't gossip
    ok.


next_up_node(Nodes) ->
    Node = node(),
    next_up_node(Node, Nodes, up_nodes()).


next_up_node(Node, Nodes, UpNodes) ->
    {A, [Node|B]} = lists:splitwith(fun(N) -> N /= Node end, Nodes),
    List = lists:append(B, A), % be sure to eliminate Node
    DownNodes = Nodes -- UpNodes,
    case List -- DownNodes of
    [Target|_] -> Target;
    [] -> throw({error, no_gossip_targets_available})
    end.


up_nodes() ->
    % TODO: implement cache (fb 9704 & 9449)
    erlang:nodes().


%% @doc find the latest state file on disk
find_latest_state_filename(Config) ->
    Dir = Config#config.directory,
    case file:list_dir(Dir) of
    {ok, Filenames} ->
        Timestamps = [list_to_integer(TS) || {?STATE_FILE_PREFIX, TS} <-
           [list_to_tuple(string:tokens(FN, ".")) || FN <- Filenames]],
        SortedTimestamps = lists:reverse(lists:sort(Timestamps)),
        case SortedTimestamps of
        [Latest | _] ->
            {ok, Dir ++ "/" ++ ?STATE_FILE_PREFIX ++ "." ++
             integer_to_list(Latest)};
        _ ->
            throw({error, mem_state_file_not_found})
        end;
    {error, Reason} ->
        throw({error, Reason})
    end.


%% (Test, Config)
read_latest_state_file(undefined, Config) ->
    try
        {ok, File} = find_latest_state_filename(Config),
        case file:consult(File) of
        {ok, [#mem{}=State]} -> State;
        _Else ->
                throw({error, bad_mem_state_file})
        end
    catch _:Error ->
        showroom_log:message(info, "membership: ~p", [Error]),
        nil
    end;
read_latest_state_file(_, _) ->
    nil.


install_new_state(#mem{args=Args} = State) ->
    Config = get_config(Args),
    Test = get_test(Args),
    save_state_file(Test, State, Config),
    gossip(State).


%% @doc save the state file to disk, with current timestamp.
%%      thx to riak_ring_manager:do_write_ringfile/1
-spec save_state_file(test(), mem_state(), config()) -> ok.
save_state_file(undefined, State, Config) ->
    Dir = Config#config.directory,
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:universal_time(),
    TS = io_lib:format("~B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B",
                       [Year, Month, Day, Hour, Minute, Second]),
    FN = Dir ++ "/" ++ ?STATE_FILE_PREFIX ++ "." ++ TS,
    ok = filelib:ensure_dir(FN),
    {ok, File} = file:open(FN, [binary, write]),
    io:format(File, "~w.~n", [State]),
    file:close(File);

save_state_file(_,_,_) -> ok. % don't save if testing


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


int_reset(_Test, State) ->
    State#mem{nodes=[], clock=[]}.


ping_all_yall(Nodes) ->
    lists:map(fun(Node) -> net_adm:ping(Node) end, Nodes).


get_remote_states(NodeList) ->
    NodeList1 = lists:delete(node(), NodeList),
    {States1, BadNodes} = rpc:multicall(NodeList1, mem3, state, [], 5000),
    {_Status, States2} = lists:unzip(States1),
    {lists:zip(NodeList1,States2), BadNodes}.


%% @doc compare state with states based on vector clock
%%      return match | {bad_state_match, Node, NodesThatDontMatch}
compare_state_with_rest(#mem{clock=Clock} = _State, States) ->
    Results = lists:map(fun({Node, #mem{clock=Clock1}}) ->
        {vector_clock:equals(Clock, Clock1), Node}
    end, States),
    BadResults = lists:foldl(fun({true, _N}, AccIn) -> AccIn;
                                ({false, N}, AccIn) -> [N | AccIn]
    end, [], Results),
    if
    length(BadResults) == 0 -> match;
    true -> {bad_state_match, node(), BadResults}
    end.
