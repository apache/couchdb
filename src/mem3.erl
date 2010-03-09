
-module(mem3).
-author('brad@cloudant.com').

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, stop/0, stop/1]).
-export([clock/0, state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% includes
-include("../include/config.hrl").
-include("../include/common.hrl").


%%====================================================================
%% API
%%====================================================================

start_link(Node, ErlNodes) ->
    start_link(Node, ErlNodes, []).


start_link(Node, ErlNodes, Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Node, ErlNodes, Args], []).


stop() ->
    stop(?MODULE).


stop(Server) ->
    gen_server:cast(Server, stop).


clock() ->
    gen_server:call(?MODULE, clock).


state() ->
    gen_server:call(?MODULE, state).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% start up membership server
init([Node, Nodes, Args]) ->
    process_flag(trap_exit,true),
    showroom_log:message(info, "membership: membership server starting...", []),
    net_kernel:monitor_nodes(true),
    Options = lists:flatten(Args),
    Config = configuration:get_config(),
    OldState = read_latest_state_file(Config),
    State = handle_init(Node, Nodes, Options, OldState, Config),
    {ok, State}.


%% new node joining to this node
handle_call({join, _JoiningNode, _Options}, _From, State) ->
    {reply, ok, State};

%% clock
handle_call(clock, _From, State = #mem{clock=Clock}) ->
    {reply, Clock, State};

%% state
handle_call(state, _From, State) ->
    {reply, State, State};

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
    io:format("Unknown Old Version!~nOldVsn: ~p~nState : ~p~n", [OldVsn, State]),
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% we could be:
%  1. starting fresh node into a fresh cluster (we're one of first nodes)
%  2. starting fresh node into an existing cluster (need to join)
%  3. rejoining a cluster after some downtime
%  4. replacing a node in an existing cluster

handle_init(Node, [], nil, Options, Config) ->
    % no other erlang nodes, no old state
    Hints = proplists:get_value(hints, Options),
    Map = create_map(Config, [{Node, Hints}]),
    ?debugFmt("~nmap: ~p~n", [Map]);

handle_init(_Node, [], _OldState, _Options, _Config) ->
    % no other erlang nodes, old state
    % network partition?
    ok;

handle_init(_Node, _ErlNodes, nil, _Options, _Config) ->
    % other erlang nodes, no old state
    ok;

handle_init(_Node, _ErlNodes, _OldState, _Options, _Config) ->
    % other erlang nodes, old state
    % network partition?
    ok.


find_latest_state_filename(Config) ->
    ?debugFmt("~nConfig: ~p~n", [Config]),
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


read_latest_state_file(Config) ->
    try
        {ok, File} = find_latest_state_filename(Config),
        case file:consult(File) of
        {ok, #mem{}=State} -> State;
        _Else -> throw({error, bad_mem_state_file})
        end
    catch
        _:Error ->
            showroom_log:message(info, "membership: ~p", [Error]),
            nil
    end.


%% @doc given Config and a list of Nodes, construct a Fullmap
create_map(#config{q=Q}, Nodes) ->
    [{FirstNode,_}|_] = Nodes,
    Pmap = lists:foldl(fun({Node, Hints}, Map) ->
        partitions:join(Node, Map, Hints)
    end, partitions:create(Q, FirstNode), Nodes),
    make_fullmap(Pmap).


%% @doc construct a table with all partitions, with the primary node and all
%%      replication partner nodes as well.
make_fullmap(PMap) ->
  {Nodes, _Parts} = lists:unzip(PMap),
  NodeParts = lists:flatmap(
    fun({Node,Part}) ->
        Partners = replication:partners(Node, lists:usort(Nodes)),
        PartnerList = [{Partner, Part, partner} || Partner <- Partners],
        [{Node, Part, primary} | PartnerList]
    end, PMap),
  NodeParts.
