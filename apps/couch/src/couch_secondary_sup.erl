-module(couch_secondary_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local,couch_secondary_services}, ?MODULE, []).
init([]) ->
    SecondarySupervisors = [
        {couch_db_update_notifier_sup,
            {couch_db_update_notifier_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_db_update_notifier_sup]},
        {couch_metrics_event_manager,
            {gen_event, start_link, [{local, couch_metrics_event_manager}]},
            permanent,
            brutal_kill,
            worker,
            dynamic}
    ],
    Children = SecondarySupervisors ++ [
        begin
            {ok, {Module, Fun, Args}} = couch_util:parse_term(SpecStr),

            {list_to_atom(Name),
                {Module, Fun, Args},
                permanent,
                brutal_kill,
                worker,
                [Module]}
        end
        || {Name, SpecStr}
        <- couch_config:get("daemons"), SpecStr /= ""],
    {ok, {{one_for_one, 10, 3600}, Children}}.
