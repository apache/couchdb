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

% couch_stats_server is in charge of:
%   - Initial metric loading from application stats descriptions.
%   - Recycling(resetting to 0) stale histogram counters.
%   - Checking and reloading if stats descriptions change.
%   - Checking and reloading if histogram interval config value changes.
%

-module(couch_stats_server).

-behaviour(gen_server).

-export([
    reload/0
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(RELOAD_INTERVAL_SEC, 600).

-record(st, {
    hist_interval,
    histograms,
    clean_tref,
    reload_tref
}).

reload() ->
    gen_server:call(?MODULE, reload).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    St = #st{
        hist_interval = config:get("stats", "interval"),
        clean_tref = erlang:send_after(clean_msec(), self(), clean),
        reload_tref = erlang:send_after(reload_msec(), self(), reload)
    },
    {_, Stats} = try_reload(St),
    {ok, St#st{histograms = couch_stats_util:histograms(Stats)}}.

handle_call(reload, _From, #st{} = St) ->
    {reply, ok, do_reload(St)};
handle_call(Msg, _From, #st{} = St) ->
    {stop, {unknown_call, Msg}, unknown_call, St}.

handle_cast(Msg, #st{} = St) ->
    {stop, {unknown_cast, Msg}, St}.

handle_info(reload, #st{} = St) ->
    {noreply, do_reload(St)};
handle_info(clean, #st{} = St) ->
    {noreply, do_clean(St)};
handle_info(Msg, #st{} = St) ->
    {stop, {unknown_info, Msg}, St}.

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

do_reload(#st{} = St) ->
    timer:cancel(St#st.reload_tref),
    RTRef = erlang:send_after(reload_msec(), self(), reload),
    case try_reload(St) of
        {true, NewStats} ->
            timer:cancel(St#st.clean_tref),
            Histograms = couch_stats_util:histograms(NewStats),
            HTRef = erlang:send_after(clean_msec(), self(), clean),
            St#st{
                histograms = Histograms,
                clean_tref = HTRef,
                reload_tref = RTRef,
                hist_interval = config:get("stats", "interval")
            };
        {false, _} ->
            St#st{reload_tref = RTRef}
    end.

try_reload(#st{} = St) ->
    NewDefs = couch_stats_util:load_metrics_for_applications(),
    Stats = couch_stats_util:stats(),
    MetricsChanged = couch_stats_util:metrics_changed(Stats, NewDefs),
    IntervalChanged = interval_changed(St),
    case MetricsChanged orelse IntervalChanged of
        true ->
            couch_stats_util:reset_histogram_interval_sec(),
            NewStats = couch_stats_util:create_metrics(NewDefs),
            couch_stats_util:replace_stats(NewStats),
            {true, NewStats};
        false ->
            {false, Stats}
    end.

interval_changed(#st{hist_interval = OldInterval}) ->
    case config:get("stats", "interval") of
        Interval when OldInterval =:= Interval ->
            false;
        _ ->
            true
    end.

reload_msec() ->
    1000 * ?RELOAD_INTERVAL_SEC.

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
            ?TDEF_FE(t_reload_with_no_changes_works),
            ?TDEF_FE(t_reload_with_changes_works),
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

t_reload_with_no_changes_works(_) ->
    Pid = whereis(?MODULE),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(ok, reload()),
    ?assertEqual(Pid, whereis(?MODULE)),
    ?assert(is_process_alive(Pid)),
    % Let's reload a few more hundred times
    lists:foreach(
        fun(_) ->
            ?assertEqual(ok, reload()),
            ?assertEqual(Pid, whereis(?MODULE)),
            ?assert(is_process_alive(Pid))
        end,
        lists:seq(1, 100)
    ).

t_reload_with_changes_works(_) ->
    Pid = whereis(?MODULE),
    ?assert(is_process_alive(Pid)),
    #st{hist_interval = Interval0} = sys:get_state(Pid),
    ?assertEqual(undefined, Interval0),

    config:set("stats", "interval", "7", false),
    ?assertEqual(ok, reload()),
    ?assertEqual(Pid, whereis(?MODULE)),
    ?assert(is_process_alive(Pid)),
    #st{hist_interval = Interval1} = sys:get_state(Pid),
    ?assertEqual("7", Interval1),

    #st{histograms = Hists} = sys:get_state(Pid),
    [{_Key, {histogram, HCtx1, _Desc}} | _] = maps:to_list(Hists),
    % Histogram window size should now be shorter
    % 7 (active time window) + 7 (stale) + 5 + 5 for buffers = 24.
    ?assertEqual(24, tuple_size(HCtx1)).

t_cleaning_works(_) ->
    config:set("stats", "interval", "1", false),
    sys:log(?MODULE, {true, 100}),
    ok = reload(),
    timer:sleep(2000),
    {ok, Events} = sys:log(?MODULE, get),
    ok = sys:log(?MODULE, false),
    config:set("stats", "interval", "10", false),
    ok = reload(),
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
