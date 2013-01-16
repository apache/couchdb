% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_server).
-behaviour(gen_server).
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

% public api.
-export([start_link/0]).

% gen_server api.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2]).

% exported for callback.
-export([update_event_handler/1]).

% private records.
-record(state, {
    update_notifier
}).

% public functions.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server functions.
init(_) ->
    net_kernel:monitor_nodes(true),
    {ok, Pid} = start_update_notifier(),
    send_alerts(),
    {ok, #state{update_notifier=Pid}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(refresh, State) ->
    send_alerts(),
    {noreply, State}.

handle_info({nodeup, _}, State) ->
    send_alerts(),
    {noreply, State};

handle_info({nodedown, _}, State) ->
    send_alerts(),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #state{update_notifier=Pid}=State) ->
    twig:log(notice, "update notifier died ~p", [Reason]),
    {ok, Pid1} = start_update_notifier(),
    {noreply, State#state{update_notifier=Pid1}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions

start_update_notifier() ->
    couch_db_update_notifier:start_link(fun ?MODULE:update_event_handler/1).

update_event_handler({updated, <<"dbs">>}) ->
    gen_server:cast(?MODULE, refresh);
update_event_handler(_) ->
    ok.

send_alerts() ->
    Live = [node() | nodes()],
    N = list_to_integer(couch_config:get("cluster", "n", "3")),
    Acc0 = {Live, N, 0, 0},
    {ok, Db} = custodian_util:ensure_dbs_exists(),
    try
        {ok, _, {_, _, Unavailable, Impaired}} = couch_db:enum_docs(Db, fun fold_dbs/3, Acc0, []),
        case {Unavailable, Impaired} of
            {0, 0} ->
                send_all_available_alert();
            {Unavailable, 0} ->
                send_unavailable_alert(Unavailable),
                twig:log(crit, "~B unavailable shards in this cluster", [Unavailable]);
            {0, Impaired} ->
                send_impaired_alert(Impaired),
                twig:log(crit, "~B impaired shards in this cluster", [Impaired]);
            {Unavailable, Impaired} ->
                send_unavailable_alert(Unavailable),
                send_impaired_alert(Impaired),
                twig:log(crit, "~B unavailable and ~B impaired shards in this cluster", [Unavailable, Impaired])
        end
    after
        couch_db:close(Db)
    end.

fold_dbs(#full_doc_info{id = <<"_design/", _/binary>>}, _, Acc) ->
    {ok, Acc};
fold_dbs(#full_doc_info{deleted=true}, _, Acc) ->
    {ok, Acc};
fold_dbs(#full_doc_info{id = Id}, _, Acc) ->
    Shards = mem3:shards(Id),
    Rs = [R || #shard{range=R} <- lists:ukeysort(#shard.range, Shards)],
    ActualN = [{R1, [N || #shard{node=N,range=R2} <- Shards, R1 == R2]} ||  R1 <- Rs],
    fold_dbs(Id, ActualN, Acc);
fold_dbs(_Id, [], Acc) ->
    {ok, Acc};
fold_dbs(Id, [{Range, Nodes}|Rest], {Live, N, Unavailable, Impaired}) ->
    Nodes1 = maybe_redirect(Nodes),
    Nodes2 = [Node || Node <- Nodes1, lists:member(Node, Live)],
    case length(Nodes2) of
        0 ->
            fold_dbs(Id, Rest, {Live, N, Unavailable + 1, Impaired});
        N1 when N1 < N ->
            fold_dbs(Id, Rest, {Live, N, Unavailable, Impaired + 1});
        _ ->
            fold_dbs(Id, Rest, {Live, N, Unavailable, Impaired})
    end.

maybe_redirect(Nodes) ->
    maybe_redirect(Nodes, []).

maybe_redirect([], Acc) ->
    Acc;
maybe_redirect([Node|Rest], Acc) ->
    case couch_config:get("mem3.redirects", atom_to_list(Node)) of
        undefined ->
            maybe_redirect(Rest, [Node|Acc]);
        Redirect ->
            maybe_redirect(Rest, [list_to_atom(Redirect)|Acc])
    end.

send_unavailable_alert(Count) when is_integer(Count) ->
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreShardsUnavailableEvent -o cloudantDbcoreShardCount="
           ++ integer_to_list(Count)).

send_impaired_alert(Count) when is_integer(Count) ->
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreShardsImpairedEvent -o cloudantDbcoreShardCount="
           ++ integer_to_list(Count)).

send_all_available_alert() ->
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreAllShardsAvailableEvent").
