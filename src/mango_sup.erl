-module(mango_sup).

-behavior(supervisor).

-export([
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Arg) ->
    Children = [],
    {ok, {{one_for_one, 10, 1}, Children}}.

