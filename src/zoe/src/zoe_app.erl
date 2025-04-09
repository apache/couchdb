%%%-------------------------------------------------------------------
%% @doc zoe public API
%% @end
%%%-------------------------------------------------------------------

-module(zoe_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    zoe_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
