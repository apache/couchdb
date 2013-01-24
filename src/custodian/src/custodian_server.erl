% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_server).
-behaviour(gen_server).

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
    {Unavailable, Impaired} = custodian:summary(),
    send_unavailable_alert(Unavailable),
    send_impaired_alert(Impaired).

send_impaired_alert(0) ->
    twig:log(notice, "No shards impaired in this cluster", []),
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreAllShardsUnimpairedEvent");
send_impaired_alert(Count) when is_integer(Count) ->
    twig:log(crit, "~B impaired shards in this cluster", [Count]),
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreShardsImpairedEvent -o cloudantDbcoreShardCount:INTEGER:"
           ++ integer_to_list(Count)).

send_unavailable_alert(0) ->
    twig:log(notice, "All shards are available in this cluster", []),
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreAllShardsAvailableEvent");
send_unavailable_alert(Count) when is_integer(Count) ->
    twig:log(crit, "~B unavailable shards in this cluster", [Count]),
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreShardsUnavailableEvent -o cloudantDbcoreShardCount:INTEGER:"
           ++ integer_to_list(Count)).
