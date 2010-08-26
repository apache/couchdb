-module(couch_primary_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local,couch_primary_services}, ?MODULE, []).

init([]) ->
    Children = [
        {collation_driver,
            {couch_drv, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_drv]},
        {couch_task_status,
            {couch_task_status, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_task_status]},
        {couch_server,
            {couch_server, sup_start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_server]},
        {couch_db_update_event,
            {gen_event, start_link, [{local, couch_db_update}]},
            permanent,
            brutal_kill,
            worker,
            dynamic},
        {couch_replication_supervisor,
            {couch_rep_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_rep_sup]},
        {couch_log,
            {couch_log, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_log]}
    ],
    {ok, {{one_for_one, 10, 3600}, Children}}.

