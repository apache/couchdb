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

% couch_stats_server is in charge of initially loading stats definition into a
% persistent term then recycling(resetting to 0) stale histogram counters.
%

-module(couch_stats_server).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% config_listener
-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

-record(st, {
    histograms,
    clean_tref
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = config:listen_for_changes(?MODULE, self()),
    St = #st{
        clean_tref = erlang:send_after(clean_msec(), self(), clean),
        histograms = couch_stats_util:histograms(couch_stats_util:load())
    },
    {ok, St}.

handle_call(Msg, _From, #st{} = St) ->
    {stop, {unknown_call, Msg}, unknown_call, St}.

handle_cast(Msg, #st{} = St) ->
    {stop, {unknown_cast, Msg}, St}.

handle_info(clean, #st{} = St) ->
    {noreply, do_clean(St)};
handle_info(restart_config_listener, #st{} = St) ->
    ok = config:listen_for_changes(?MODULE, self()),
    {noreply, St};
handle_info(Msg, #st{} = St) ->
    {stop, {unknown_info, Msg}, St}.

handle_config_change("stats", "interval", _, _, Pid) ->
    Pid ! clean,
    {ok, Pid};
handle_config_change(_, _, _, _, Pid) ->
    {ok, Pid}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, Pid) ->
    erlang:send_after(1000, Pid, restart_config_listener).

do_clean(#st{} = St) ->
    timer:cancel(St#st.clean_tref),
    HistTRef = erlang:send_after(clean_msec(), self(), clean),
    NowSec = erlang:monotonic_time(second),
    BufferSec = couch_stats_util:histogram_safety_buffer_size_sec(),
    IntervalSec = couch_stats_util:histogram_interval_sec(),
    % The histogram timeline looks something like:
    %
    %  |<--buffer-->|<--stale-->|<--buffer-->|<--current-->|
    %                ^                                    ^
    %                StartSec                             NowSec
    %
    % To get to the start of "stale" part to clean it, subtract one interval,
    % then a buffer, then another interval from NowSec.
    %
    StartSec = NowSec - IntervalSec - BufferSec - (IntervalSec - 1),
    % Last -1 is because the interval ends are inclusive
    maps:foreach(
        fun(_, {_, Ctx, _}) ->
            couch_stats_histogram:clear(Ctx, StartSec, IntervalSec)
        end,
        St#st.histograms
    ),
    St#st{clean_tref = HistTRef}.

clean_msec() ->
    % We want to wake up more often than our interval so we decide to wake
    % about twice as often. If the interval is 10 seconds, we'd wake up every 5
    % seconds and clean the most stale 10 seconds. It's a bit wasteful but it's
    % a safety feature to ensure we don't miss anything.
    (couch_stats_util:histogram_interval_sec() * 1000) div 2.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_stats_server_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_server_starts),
            ?TDEF_FE(t_cleaning_works, 10),
            ?TDEF_FE(t_invalid_call),
            ?TDEF_FE(t_invalid_cast),
            ?TDEF_FE(t_invalid_msg)
        ]
    }.

setup() ->
    test_util:start_couch().

teardown(Ctx) ->
    config:delete("stats", "interval", _Persist = false),
    test_util:stop_couch(Ctx).

t_server_starts(_) ->
    ?assert(is_process_alive(whereis(?MODULE))).

t_cleaning_works(_) ->
    config:set("stats", "interval", "1", false),
    sys:log(?MODULE, {true, 100}),
    timer:sleep(2000),
    {ok, Events} = sys:log(?MODULE, get),
    ok = sys:log(?MODULE, false),
    config:set("stats", "interval", "10", false),
    % Events looks like: [{in, Msg} | {noreply, ...} | {out, ..}, ...]
    CleanEvents = [clean || {in, clean} <- Events],
    ?assert(length(CleanEvents) >= 3).

t_invalid_call(_) ->
    Pid = whereis(?MODULE),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(unknown_call, gen_server:call(Pid, potato)),
    test_util:wait_value(fun() -> is_process_alive(Pid) end, false),
    ?assertNot(is_process_alive(Pid)).

t_invalid_cast(_) ->
    Pid = whereis(?MODULE),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:cast(Pid, potato),
    test_util:wait_value(fun() -> is_process_alive(Pid) end, false),
    ?assertNot(is_process_alive(Pid)).

t_invalid_msg(_) ->
    Pid = whereis(?MODULE),
    ?assert(is_process_alive(Pid)),
    Pid ! potato,
    test_util:wait_value(fun() -> is_process_alive(Pid) end, false),
    ?assertNot(is_process_alive(Pid)).

-endif.
