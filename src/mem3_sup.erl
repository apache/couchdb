-module(mem3_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    Children = [
        child(mem3_server),
        child(mem3_event),
        child(mem3_sync),
        child(mem3_cache)
    ],
    {ok, {{one_for_one,10,1}, Children}}.

child(mem3_event) ->
    MFA = {gen_event, start_link, [{local,mem3_event}]},
    {mem3_event, MFA, permanent, 1000, worker, dynamic};
child(Child) ->
    {Child, {Child, start_link, []}, permanent, 1000, worker, [Child]}.
    