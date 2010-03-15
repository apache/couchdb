-module(dynomite_sup).
-author('brad@cloudant.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
start_link() ->
    supervisor:start_link(?MODULE, []).

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
init(_Args) ->
    Membership = {membership,
                  {mem3, start_link, []},
                  permanent,
                  1000,
                  worker,
                  [mem3]},
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
