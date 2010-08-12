-module(rexi_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1]).

-include_lib("eunit/include/eunit.hrl").

start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
    Mod = rexi_server,
    Spec = {Mod, {Mod,start_link,[]}, permanent, 100, worker, [Mod]},
    {ok, {{one_for_one, 3, 10}, [Spec]}}.
