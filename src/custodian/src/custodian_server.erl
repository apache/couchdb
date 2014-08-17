% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_server).
-behaviour(gen_server).
-vsn(1).
-behaviour(config_listener).

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

% config_listener callback
-export([handle_config_change/5]).

% private records.
-record(state, {
    event_listener,
    shard_checker,
    rescan=false
}).

-define(VSN_0_2_7, 184129240591641721395874905059581858099).

% public functions.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_config_change("cloudant", "maintenance_mode", _, _, S) ->
    ok = gen_server:cast(S, refresh),
    {ok, S};
handle_config_change(_, _, _, _, S) ->
    {ok, S}.

% gen_server functions.
init(_) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    ok = config:listen_for_changes(?MODULE, self()),
    {ok, LisPid} = start_event_listener(),
    {ok, start_shard_checker(#state{
        event_listener=LisPid
    })}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(refresh, State) ->
    {noreply, start_shard_checker(State)}.

handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, self()),
    {noreply, State};

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
    couch_log:notice("custodian shard checker died ~p", [Reason]),
    NewState = State#state{shard_checker=undefined},
    {noreply, start_shard_checker(NewState)};

handle_info({'EXIT', Pid, Reason}, #state{event_listener=Pid}=State) ->
    couch_log:notice("custodian update notifier died ~p", [Reason]),
    {ok, Pid1} = start_event_listener(),
    {noreply, State#state{event_listener=Pid1}}.

terminate(_Reason, State) ->
    couch_event:stop_listener(State#state.event_listener),
    couch_util:shutdown_sync(State#state.shard_checker),
    ok.

code_change(?VSN_0_2_7, State, _Extra) ->
    ok = config:listen_for_changes(?MODULE, self()),
    {ok, State};
code_change(_OldVsn, #state{}=State, _Extra) ->
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
    [send_sensu_event(Item) || Item <- custodian:summary()].

send_sensu_event({_, Count} = Item) ->
    Level = case Count of
        0 ->
            "--ok";
        1 ->
            couch_log:critical("~s", [describe(Item)]),
            "--critical";
        _ ->
            couch_log:warning("~s", [describe(Item)]),
            "--warning"
    end,
    Cmd = lists:concat([
        "send-sensu-event --standalone ",
        Level,
        " --output=\"",
        describe(Item),
        "\" ",
        check_name(Item)
    ]),
    os:cmd(Cmd).

describe({{safe, N}, Count}) ->
    lists:concat([Count, " ", shards(Count), " in cluster with only ", N,
                  " ", copies(N), " on nodes that are currently up"]);
describe({{live, N}, Count}) ->
    lists:concat([Count, " ", shards(Count), " in cluster with only ",
                  N, " ", copies(N), " on nodes not in maintenance mode"]);
describe({conflicted, Count}) ->
    lists:concat([Count, " conflicted ", shards(Count), " in cluster"]).

check_name({{Type, N}, _}) ->
    lists:concat(["custodian-", N, "-", Type, "-shards-check"]);
check_name({Type, _}) ->
    lists:concat(["custodian-", Type, "-shards-check"]).

shards(1) ->
    "shard";
shards(_) ->
    "shards".

copies(1) ->
    "copy";
copies(_) ->
    "copies".
