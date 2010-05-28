-module(membership_sup).
-author('brad@cloudant.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    Membership = {membership,
                  {mem3, start_link, []},
                  permanent,
                  1000,
                  worker,
                  [mem3]},
    {ok, {{one_for_one,10,1}, [Membership]}}.
