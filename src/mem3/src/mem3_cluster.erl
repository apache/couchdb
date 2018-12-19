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

% Maintain cluster stability information. A cluster is considered stable if there
% were no changes to during a given period of time.
%
% To be notified of cluster stability / instability the owner module must
% implement the mem3_cluster behavior. When cluster membership changes,
% cluster_unstable behavior callback will be called. After that is are no more
% changes to the cluster, then cluster_stable callback will be called.
%
% The period is passed in as start argument but it can also be set dynamically
% via the set_period/2 API call.
%
% In some cases it might be useful to have a shorter pariod during startup.
% That can be configured via the StartPeriod argument. If the time since start
% is less than a full period, then the StartPeriod is used as the period.


-module(mem3_cluster).

-behaviour(gen_server).

-export([
    start_link/4,
    set_period/2
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-callback cluster_stable(Context :: term()) -> NewContext :: term().
-callback cluster_unstable(Context :: term()) -> NewContext :: term().


-record(state, {
    mod :: atom(),
    ctx :: term(),
    start_time :: erlang:timestamp(),
    last_change :: erlang:timestamp(),
    period :: integer(),
    start_period :: integer(),
    timer :: reference()
}).


-spec start_link(module(), term(), integer(), integer()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Module, Context, StartPeriod, Period)
        when is_atom(Module), is_integer(StartPeriod), is_integer(Period) ->
    gen_server:start_link(?MODULE, [Module, Context, StartPeriod, Period], []).


-spec set_period(pid(), integer()) -> ok.
set_period(Server, Period) when is_pid(Server), is_integer(Period) ->
    gen_server:cast(Server, {set_period, Period}).


% gen_server callbacks

init([Module, Context, StartPeriod, Period]) ->
    net_kernel:monitor_nodes(true),
    {ok, #state{
        mod = Module,
        ctx = Context,
        start_time = os:timestamp(),
        last_change = os:timestamp(),
        period = Period,
        start_period = StartPeriod,
        timer = new_timer(StartPeriod)
     }}.


terminate(_Reason, _State) ->
    ok.

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.


handle_cast({set_period, Period}, State) ->
    {noreply, State#state{period = Period}}.


handle_info({nodeup, _Node}, State) ->
    {noreply, cluster_changed(State)};

handle_info({nodedown, _Node}, State) ->
    {noreply, cluster_changed(State)};

handle_info(stability_check, #state{mod = Mod, ctx = Ctx} = State) ->
   erlang:cancel_timer(State#state.timer),
   case now_diff_sec(State#state.last_change) > interval(State) of
       true ->
           {noreply, State#state{ctx = Mod:cluster_stable(Ctx)}};
       false ->
           Timer = new_timer(interval(State)),
           {noreply, State#state{timer = Timer}}
   end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions

-spec cluster_changed(#state{}) -> #state{}.
cluster_changed(#state{mod = Mod, ctx = Ctx} = State) ->
    State#state{
        last_change = os:timestamp(),
        timer = new_timer(interval(State)),
        ctx = Mod:cluster_unstable(Ctx)
    }.


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


-spec now_diff_sec(erlang:timestamp()) -> non_neg_integer().
now_diff_sec(Time) ->
    case timer:now_diff(os:timestamp(), Time) of
        USec when USec < 0 ->
            0;
        USec when USec >= 0 ->
             USec / 1000000
    end.
