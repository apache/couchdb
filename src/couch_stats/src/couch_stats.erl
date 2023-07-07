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

-module(couch_stats).

-export([
    fetch/0,
    reload/0,
    sample/1,
    increment_counter/1,
    increment_counter/2,
    decrement_counter/1,
    decrement_counter/2,
    update_histogram/2,
    update_gauge/2
]).

-type response() :: ok | {error, unknown_metric} | {error, invalid_metric}.
-type stat() :: {any(), [{atom(), any()}]}.

fetch() ->
    Seconds = couch_stats_util:histogram_interval_sec(),
    StartSec = now_sec() - (Seconds - 1),
    % Last -1 is because the interval ends are inclusive
    couch_stats_util:fetch(stats(), StartSec, Seconds).

reload() ->
    couch_stats_server:reload().

-spec sample(any()) -> stat().
sample(Name) ->
    Seconds = couch_stats_util:histogram_interval_sec(),
    StartSec = now_sec() - (Seconds - 1),
    % Last -1 is because the interval ends are inclusive
    couch_stats_util:sample(Name, stats(), StartSec, Seconds).

-spec increment_counter(any()) -> response().
increment_counter(Name) ->
    increment_counter(Name, 1).

-spec increment_counter(any(), pos_integer()) -> response().
increment_counter(Name, Value) ->
    case couch_stats_util:get_counter(Name, stats()) of
        {ok, Ctx} -> couch_stats_counter:increment(Ctx, Value);
        {error, Error} -> {error, Error}
    end.

-spec decrement_counter(any()) -> response().
decrement_counter(Name) ->
    decrement_counter(Name, 1).

-spec decrement_counter(any(), pos_integer()) -> response().
decrement_counter(Name, Value) ->
    case couch_stats_util:get_counter(Name, stats()) of
        {ok, Ctx} -> couch_stats_counter:decrement(Ctx, Value);
        {error, Error} -> {error, Error}
    end.

-spec update_gauge(any(), number()) -> response().
update_gauge(Name, Value) ->
    case couch_stats_util:get_gauge(Name, stats()) of
        {ok, Ctx} -> couch_stats_gauge:update(Ctx, Value);
        {error, Error} -> {error, Error}
    end.

-spec update_histogram
    (any(), number()) -> response();
    (any(), function()) -> any().
update_histogram(Name, Fun) when is_function(Fun, 0) ->
    Begin = erlang:monotonic_time(),
    Result = Fun(),
    Dt = erlang:monotonic_time() - Begin,
    Duration = erlang:convert_time_unit(Dt, native, millisecond),
    case update_histogram(Name, Duration) of
        ok ->
            Result;
        {error, unknown_metric} ->
            throw({unknown_metric, Name});
        {error, invalid_metric} ->
            throw({invalid_metric, Name})
    end;
update_histogram(Name, Value) when is_number(Value) ->
    case couch_stats_util:get_histogram(Name, stats()) of
        {ok, Ctx} -> couch_stats_histogram:update(Ctx, now_sec(), Value);
        {error, Error} -> {error, Error}
    end.

stats() ->
    couch_stats_util:stats().

now_sec() ->
    erlang:monotonic_time(second).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_stats_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_fetch_metrics),
            ?TDEF_FE(t_sample_metrics),
            ?TDEF_FE(t_reload),
            ?TDEF_FE(t_increment_counter),
            ?TDEF_FE(t_decrement_counter),
            ?TDEF_FE(t_update_gauge),
            ?TDEF_FE(t_update_histogram),
            ?TDEF_FE(t_update_histogram_fun),
            ?TDEF_FE(t_access_invalid_metrics)
        ]
    }.

setup() ->
    test_util:start_couch([couch_replicator]).

teardown(Ctx) ->
    config:delete("stats", "interval", _Persist = false),
    test_util:stop_couch(Ctx).

t_fetch_metrics(_) ->
    Metrics = fetch(),
    ?assertEqual(map_size(stats()), length(Metrics)),
    ?assertMatch([{_, [{value, _}, {type, _}, {desc, _}]} | _], Metrics).

t_sample_metrics(_) ->
    Hist = sample([fsync, time]),
    ?assertMatch([{_Name, _Val} | _], Hist),

    Count = sample([fsync, count]),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0),

    ?assertEqual(0, sample([couch_replicator, jobs, total])).

t_reload(_) ->
    % This is tested in detail in couch_stats_server.
    ?assertEqual(ok, reload()).

t_increment_counter(_) ->
    [increment_counter([fsync, count]) || _ <- lists:seq(1, 1000)],
    ?assert(sample([fsync, count]) > 1000).

t_decrement_counter(_) ->
    [decrement_counter([fsync, count]) || _ <- lists:seq(1, 10000)],
    ?assert(sample([fsync, count]) < 10).

t_update_gauge(_) ->
    application:stop(couch_replicator),
    % We don't want replicator to reset the gauge back to 0
    update_gauge([couch_replicator, jobs, total], 42),
    ?assertEqual(42, sample([couch_replicator, jobs, total])).

t_update_histogram(_) ->
    [update_histogram([fsync, time], rand:uniform(1000)) || _ <- lists:seq(1, 1000)],
    Hist = sample([fsync, time]),
    N = proplists:get_value(n, Hist),
    ?assert(is_integer(N)),
    ?assert(N >= 1000).

t_update_histogram_fun(_) ->
    Fun = fun() -> timer:sleep(rand:uniform(2)) end,
    [update_histogram([fsync, time], Fun) || _ <- lists:seq(1, 100)],
    Hist = sample([fsync, time]),
    N = proplists:get_value(n, Hist),
    ?assert(is_integer(N)),
    ?assert(N >= 100).

t_access_invalid_metrics(_) ->
    Fun = fun() -> ok end,
    ?assertThrow(unknown_metric, sample([invalid])),
    ?assertEqual({error, unknown_metric}, increment_counter([invalid], 100)),
    ?assertEqual({error, unknown_metric}, decrement_counter([invalid], 100)),
    ?assertEqual({error, unknown_metric}, update_gauge([invalid], 100)),
    ?assertEqual({error, unknown_metric}, update_histogram([invalid], 100)),
    ?assertThrow({unknown_metric, _}, update_histogram([invalid], Fun)),
    % Invalid metric types
    ?assertEqual({error, invalid_metric}, increment_counter([fsync, time], 100)),
    ?assertEqual({error, invalid_metric}, decrement_counter([fsync, time], 100)),
    ?assertEqual({error, invalid_metric}, update_gauge([fsync, count], 100)),
    ?assertEqual({error, invalid_metric}, update_histogram([fsync, count], 100)),
    ?assertThrow({invalid_metric, _}, update_histogram([fsync, count], Fun)),
    InvalidMetrics = #{[bad] => {invalid, <<"desc">>}},
    ?assertThrow({unknown_metric, _}, couch_stats_util:create_metrics(InvalidMetrics)).

-endif.
