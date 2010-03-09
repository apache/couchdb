%%%-------------------------------------------------------------------
%%% File:      dynomite.erl
%%% @author    Cliff Moon <cliff@powerset.com> []
%%% @copyright 2008 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2008-06-27 by Cliff Moon
%%%-------------------------------------------------------------------
-module(dynomite_app).
-author('cliff@powerset.com').
-author('brad@cloudant.com').

-behaviour(application).

-include("../include/config.hrl").
-include("../../couch/src/couch_db.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_CLUSTER_URL, "http://localhost:5984/_cluster").

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%% @doc This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------


%% @doc start required apps, join cluster, start dynomite supervisor
start(_Type, _StartArgs) ->
    % get process_dict hack for startargs (i.e. not from .app file)
    PdStartArgs = case erase(startargs) of
    undefined ->
        [];
    Args ->
        Args
    end,

    % start dynomite supervisor
    ok = start_node(),
    case dynomite_sup:start_link(PdStartArgs) of
    {ok, Supervisor} ->
        {ok, Supervisor};
    Error ->
        Error
    end.


%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
stop({_, Sup}) ->
    showroom_log:message(alert, "dynomite application stopped", []),
    exit(Sup, normal),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

%% @spec start_node() -> ok | {error, Reason}
%% @doc start this node (join to dist. erlang cluster)
start_node() ->
    PingUrl = couch_config:get("cluster","ping", ?DEFAULT_CLUSTER_URL),
    ?LOG_DEBUG("PingUrl: ~p", [PingUrl]),
    Result = case get_pingnode(PingUrl, 1) of
    {ok, PingNode} ->
        join(PingNode);
    _ ->
        ?LOG_INFO("No pingnode found.  Becoming single-node cluster", [])
    end,
    couch_api:create_db(<<"users">>, []), % all nodes have local 'users' db
    Result.


%% @spec get_pingnode(Url::string(), Retries::int()) -> node() |
%%       {error, Reason}
%% @doc make a http get call to Url to get cluster information
get_pingnode(Url, Retries) ->
    try couch_rep_httpc:request(#http_db{url=Url, retries=Retries}) of
    {[{<<"ping_node">>, Node}]} ->
        {ok, list_to_atom(binary_to_list(Node))};
    _ ->
        {error, pingnode_not_found}
    catch
      _:_ ->
        {error, pingnode_not_found}
    end.


join(PingNode) ->
    if
    node() =:= PingNode ->
        ok; % we must be brain, so we'll take over the world
    true ->
        case net_adm:ping(PingNode) of
        pong ->
            % there is a cluster, we just joined it
                ?LOG_DEBUG("ping successful, we're in.", []),
                timer:sleep(1000); %% grr, what a hack, erlang.  rly?
        pang ->
            ?LOG_ERROR("ping not successful.", []),
            throw({cluster_error, ?l2b("cluster ping not successful")})
        end
    end,
    ok.
