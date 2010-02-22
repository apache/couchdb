%%%-------------------------------------------------------------------
%%% File:      dynomite_sup.erl
%%% @author    Cliff Moon <cliff@powerset.com> []
%%% @copyright 2008 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2008-06-27 by Cliff Moon
%%%-------------------------------------------------------------------
-module(dynomite_sup).
-author('cliff@powerset.com').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("../include/config.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link(Hints) ->
    supervisor:start_link(?MODULE, [Hints]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Node = node(),
    Nodes = running_nodes() ++ [node()],
    Membership = {membership,
                  {membership2, start_link, [Node, Nodes, Args]},
                  permanent,
                  1000,
                  worker,
                  [membership2]},
    MemEventMgr = {mem_event_manager,
                   {gen_event, start_link, [{local, membership_events}]},
                   permanent,
                   1000,
                   worker,
                   []},
    {ok, {{one_for_one,10,1}, [Membership, MemEventMgr]}}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc get a list of running nodes visible to this local node
running_nodes() ->
    [Node || Node <- nodes([this,visible]), running(Node)].

%% @doc monitor the membership server on Node from here
running(Node) ->
    Ref = erlang:monitor(process, {membership, Node}),
    R = receive
            {'DOWN', Ref, _, _, _} -> false
        after 1 ->
                true
        end,
    erlang:demonitor(Ref),
    R.
