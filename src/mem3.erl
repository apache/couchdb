-module(mem3).
-author('Brad Anderson <brad@cloudant.com>').

-export([start/0, stop/0, restart/0, state/0]).

-include("mem3.hrl").

-define(SERVER, mem3_server).

start() ->
    application:start(mem3).

stop() ->
    application:stop(mem3).

restart() ->
    stop(),
    start().

%% @doc Detailed report of cluster-wide membership state.  Queries the state
%%      on all member nodes and builds a dictionary with unique states as the
%%      key and the nodes holding that state as the value.  Also reports member
%%      nodes which fail to respond and nodes which are connected but are not
%%      cluster members.  Useful for debugging.
-spec state() -> [{mem_state() | bad_nodes | non_member_nodes, [node()]}].
state() ->
    {ok, Nodes} = mem3:nodes(),
    AllNodes = erlang:nodes([this, visible]),
    {Replies, BadNodes} = gen_server:multi_call(Nodes, ?SERVER, state),
    Dict = lists:foldl(fun({Node, {ok,State}}, D) ->
        orddict:append(State, Node, D)
    end, orddict:new(), Replies),
    [{non_member_nodes, AllNodes -- Nodes}, {bad_nodes, BadNodes} | Dict].