% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {
        {one_for_one, 5, 10},
        [
            ?CHILD(custodian_server, worker),
            ?CHILD(custodian_db_checker, worker)
        ]
    }}.

