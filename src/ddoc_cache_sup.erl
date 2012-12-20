% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache_sup).
-behaviour(supervisor).


-export([
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Children = [
        {
            ddoc_cache_server,
            {ddoc_cache_server, start_link, []},
            permanent,
            5000,
            worker,
            [ddoc_cache_server]
        }
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

