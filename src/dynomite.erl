%%% @doc convenience start/stop functions for Dynomite
%%%
-module(dynomite).
-author('Brad Anderson <brad@cloudant.com>').

-export([start/0, stop/0, restart/0]).


%% @doc start Dynomite app with no args, for -s at the command-line
start() ->
    application:start(dynomite).


%% @doc stops the Dynomite application
stop() ->
    application:stop(dynomite).


%% @doc restart Dynomite app, with no args
restart() ->
    stop(),
    start().
