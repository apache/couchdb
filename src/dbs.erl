-module(dbs).
-behaviour(supervisor).

-export([start_link/0, init/1, childspec/1, sup_upgrade_notify/2]).

-include("membership.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, MemNodes} = mem3:nodes(),
    LiveNodes = nodes(),
    ChildSpecs = [childspec(N) || N <- MemNodes, lists:member(N, LiveNodes)],
    %gen_event:add_handler(membership_events, showroom_dbs_event, []),
    {ok, {{one_for_one, 10, 8}, ChildSpecs}}.

childspec(Node) ->
    ?LOG_INFO("dbs repl ~p --> ~p starting", [node(), Node]),
    PostBody = {[
        {<<"source">>, <<"dbs">>},
        {<<"target">>, {[{<<"node">>, Node}, {<<"name">>, <<"dbs">>}]}},
        {<<"continuous">>, true}
    ]},
    Id = couch_util:to_hex(erlang:md5(term_to_binary([node(), Node]))),
    MFA = {couch_rep, start_link, [Id, PostBody, #user_ctx{}]},
    {Node, MFA, permanent, 100, worker, [couch_rep]}.

% from http://code.google.com/p/erlrc/wiki/ErlrcHowto
sup_upgrade_notify (_Old, _New) ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun(Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end,
              ok,
              Kill),
    [supervisor:start_child (?MODULE, Spec) || Spec <- Specs ],
    ok.
