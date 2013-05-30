% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_server).
-behaviour(gen_server).

% public api.
-export([start_link/0]).

% gen_server api.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2]).

% exported for callback.
-export([
    check_shards/0,
    handle_db_event/3
]).

% private records.
-record(state, {
    event_listener,
    shard_checker,
    rescan=false
}).

% public functions.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen_server functions.
init(_) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    {ok, LisPid} = start_event_listener(),
    {ok, start_shard_checker(#state{
        event_listener=LisPid
    })}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(refresh, State) ->
    {noreply, start_shard_checker(State)}.

handle_info({nodeup, _}, State) ->
    {noreply, start_shard_checker(State)};

handle_info({nodedown, _}, State) ->
    {noreply, start_shard_checker(State)};

handle_info({'EXIT', Pid, normal}, #state{shard_checker=Pid}=State) ->
    NewState = State#state{shard_checker=undefined},
    case State#state.rescan of
        true ->
            {noreply, start_shard_checker(NewState)};
        false ->
            {noreply, NewState}
    end;

handle_info({'EXIT', Pid, Reason}, #state{shard_checker=Pid}=State) ->
    twig:log(notice, "custodian shard checker died ~p", [Reason]),
    NewState = State#state{shard_checker=undefined},
    {noreply, start_shard_checker(NewState)};

handle_info({'EXIT', Pid, Reason}, #state{event_listener=Pid}=State) ->
    twig:log(notice, "custodian update notifier died ~p", [Reason]),
    {ok, Pid1} = start_event_listener(),
    {noreply, State#state{event_listener=Pid1}}.

terminate(_Reason, State) ->
    couch_event:stop_listener(State#state.event_listener),
    couch_util:shutdown_sync(State#state.shard_checker),
    ok.

code_change(_OldVsn, {state, Pid}, _Extra) ->
    {ok, #state{
        event_listener=Pid,
        shard_checker=undefined,
        rescan=false
    }};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions


start_shard_checker(#state{shard_checker=undefined}=State) ->
    State#state{
        shard_checker=spawn_link(fun ?MODULE:check_shards/0),
        rescan=false
    };
start_shard_checker(#state{shard_checker=Pid}=State) when is_pid(Pid) ->
    State#state{rescan=true}.


start_event_listener() ->
    couch_event:link_listener(
            ?MODULE, handle_db_event, nil, [{dbname, <<"dbs">>}]
        ).

handle_db_event(_DbName, updated, _St) ->
    gen_server:cast(?MODULE, refresh),
    {ok, nil};
handle_db_event(_DbName, _Event, _St) ->
    {ok, nil}.

check_shards() ->
    {Unavailable, Impaired, Conflicted} = custodian:summary(),
    send_conflicted_alert(Conflicted),
    send_unavailable_alert(Unavailable),
    send_impaired_alert(Impaired).

send_conflicted_alert(0) ->
    twig:log(notice, "No partition tables conflicted in this cluster", []),
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcoreNoPartitionTablesConflictedEvent");
send_conflicted_alert(Count) when is_integer(Count) ->
    twig:log(crit, "~B conflicted partition tables in this cluster", [Count]),
    os:cmd("send_snmptrap --trap CLOUDANT-DBCORE-MIB::cloudantDbcorePartitionTablesConflictedEvent -o cloudantDbcoreShardCount:INTEGER:"
           ++ integer_to_list(Count)).

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
