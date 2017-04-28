% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


% Maintain cluster membership and stability notifications for replications.
% On changes to cluster membership, broadcast events to `replication` gen_event.
% Listeners will get `{cluster, stable}` or `{cluster, unstable}` events.
%
% Cluster stability is defined as "there have been no nodes added or removed in
% last `QuietPeriod` seconds". QuietPeriod value is configurable. To ensure a
% speedier startup, during initialization there is a shorter StartupPeriod
% in effect (also configurable).
%
% This module is also in charge of calculating ownership of replications based
% on where their _replicator db documents shards live.


-module(couch_replicator_clustering).

-behaviour(gen_server).
-behaviour(config_listener).

-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_info/2,
    handle_cast/2,
    code_change/3
]).

-export([
    owner/2,
    owner/3,
    is_stable/0,
    link_cluster_event_listener/3
]).

% config_listener callbacks
-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(DEFAULT_QUIET_PERIOD, 60). % seconds
-define(DEFAULT_START_PERIOD, 5). % seconds
-define(RELISTEN_DELAY, 5000).

-record(state, {
    start_time :: erlang:timestamp(),
    last_change :: erlang:timestamp(),
    period = ?DEFAULT_QUIET_PERIOD :: non_neg_integer(),
    start_period = ?DEFAULT_START_PERIOD :: non_neg_integer(),
    timer :: reference()
}).


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% owner/2 function computes ownership for a {DbName, DocId} tuple
% `unstable` if cluster is considered to be unstable i.e. it has changed
% recently, or returns node() which of the owner.
%
-spec owner(Dbname :: binary(), DocId :: binary()) -> node() | unstable.
owner(<<"shards/", _/binary>> = DbName, DocId) ->
    case is_stable() of
        false ->
            unstable;
        true ->
            owner_int(DbName, DocId)
    end;
owner(_DbName, _DocId) ->
    node().


% Direct calculation of node membership. This is the algorithm part. It
% doesn't read the shard map, just picks owner based on a hash.
-spec owner(binary(), binary(), [node()]) -> node().
owner(DbName, DocId, Nodes) ->
    hd(mem3_util:rotate_list({DbName, DocId}, lists:usort(Nodes))).


-spec is_stable() -> true | false.
is_stable() ->
    gen_server:call(?MODULE, is_stable).


-spec link_cluster_event_listener(atom(), atom(), list()) -> pid().
link_cluster_event_listener(Mod, Fun, Args)
        when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    CallbackFun =
        fun(Event = {cluster, _}) -> erlang:apply(Mod, Fun, Args ++ [Event]);
           (_) -> ok
        end,
    {ok, Pid} = couch_replicator_notifier:start_link(CallbackFun),
    Pid.


% gen_server callbacks

init([]) ->
    net_kernel:monitor_nodes(true),
    ok = config:listen_for_changes(?MODULE, nil),
    Period = abs(config:get_integer("replicator", "cluster_quiet_period",
        ?DEFAULT_QUIET_PERIOD)),
    StartPeriod = abs(config:get_integer("replicator", "cluster_start_period",
        ?DEFAULT_START_PERIOD)),
    couch_log:debug("Initialized clustering gen_server ~w", [self()]),
    couch_stats:update_gauge([couch_replicator, cluster_is_stable], 0),
    {ok, #state{
        start_time = os:timestamp(),
        last_change = os:timestamp(),
        period = Period,
        start_period = StartPeriod,
        timer = new_timer(StartPeriod)
    }}.


terminate(_Reason, _State) ->
    ok.


handle_call(is_stable, _From, State) ->
    {reply, is_stable(State), State}.


handle_cast({set_period, QuietPeriod}, State) ->
    {noreply, State#state{period = QuietPeriod}}.


handle_info({nodeup, Node}, State) ->
    Timer = new_timer(interval(State)),
    couch_replicator_notifier:notify({cluster, unstable}),
    couch_stats:update_gauge([couch_replicator, cluster_is_stable], 0),
    couch_log:notice("~s : nodeup ~s, cluster unstable", [?MODULE, Node]),
    {noreply, State#state{last_change = os:timestamp(), timer = Timer}};

handle_info({nodedown, Node}, State) ->
    Timer = new_timer(interval(State)),
    couch_replicator_notifier:notify({cluster, unstable}),
    couch_stats:update_gauge([couch_replicator, cluster_is_stable], 0),
    couch_log:notice("~s : nodedown ~s, cluster unstable", [?MODULE, Node]),
    {noreply, State#state{last_change = os:timestamp(), timer = Timer}};

handle_info(stability_check, State) ->
   erlang:cancel_timer(State#state.timer),
   case is_stable(State) of
       true ->
           couch_replicator_notifier:notify({cluster, stable}),
           couch_stats:update_gauge([couch_replicator, cluster_is_stable], 1),
           couch_log:notice("~s : publish cluster `stable` event", [?MODULE]),
           {noreply, State};
       false ->
           Timer = new_timer(interval(State)),
           {noreply, State#state{timer = Timer}}
   end;

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions

-spec new_timer(non_neg_integer()) -> reference().
new_timer(IntervalSec) ->
    erlang:send_after(IntervalSec * 1000, self(), stability_check).


% For the first Period seconds after node boot we check cluster stability every
% StartPeriod seconds. Once the initial Period seconds have passed we continue
% to monitor once every Period seconds
-spec interval(#state{}) -> non_neg_integer().
interval(#state{period = Period, start_period = StartPeriod,
        start_time = T0}) ->
    case now_diff_sec(T0) > Period of
        true ->
            % Normal operation
            Period;
        false ->
            % During startup
            StartPeriod
    end.


-spec is_stable(#state{}) -> boolean().
is_stable(#state{last_change = TS} = State) ->
    now_diff_sec(TS) > interval(State).


-spec now_diff_sec(erlang:timestamp()) -> non_neg_integer().
now_diff_sec(Time) ->
    case timer:now_diff(os:timestamp(), Time) of
        USec when USec < 0 ->
            0;
        USec when USec >= 0 ->
             USec / 1000000
    end.


handle_config_change("replicator", "cluster_quiet_period", V, _, S) ->
    ok = gen_server:cast(?MODULE, {set_period, list_to_integer(V)}),
    {ok, S};
handle_config_change(_, _, _, _, S) ->
    {ok, S}.


handle_config_terminate(_, stop, _) -> ok;
handle_config_terminate(_S, _R, _St) ->
    Pid = whereis(?MODULE),
    erlang:send_after(?RELISTEN_DELAY, Pid, restart_config_listener).


-spec owner_int(binary(), binary()) -> node().
owner_int(ShardName, DocId) ->
    DbName = mem3:dbname(ShardName),
    Live = [node() | nodes()],
    Shards = mem3:shards(DbName, DocId),
    Nodes = [N || #shard{node=N} <- Shards, lists:member(N, Live)],
    owner(DbName, DocId, Nodes).
