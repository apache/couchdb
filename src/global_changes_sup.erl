% Copyright 2013 Cloudant. All rights reserved.

-module(global_changes_sup).
-behavior(supervisor).


-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, {
        {one_for_one, 5, 10}, [
            {
                global_changes_server,
                {global_changes_server, start_link, []},
                permanent,
                5000,
                worker,
                [global_changes_server]
            },
            {
                global_changes_config_listener,
                {global_changes_config_listener, start_link, []},
                permanent,
                5000,
                worker,
                [global_changes_config_listener]
            }
    ]}}.
