-module(rexi_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("eunit/include/eunit.hrl").

start(_Type, StartArgs) ->
    rexi_sup:start_link(StartArgs).

stop(_State) ->
    ok.
