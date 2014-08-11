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

-module(couch_stats_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(STATS_CFG_FIXTURE,
    filename:join([?FIXTURESDIR, "couch_stats_aggregates.cfg"])).
-define(STATS_INI_FIXTURE,
    filename:join([?FIXTURESDIR, "couch_stats_aggregates.ini"])).
-define(TIMEOUT, 1000).
-define(TIMEWAIT, 500).


setup_collector() ->
    couch_stats_collector:start(),
    ok.

setup_aggregator(_) ->
    {ok, Pid} = couch_config:start_link([?STATS_INI_FIXTURE]),
    {ok, _} = couch_stats_collector:start(),
    {ok, _} = couch_stats_aggregator:start(?STATS_CFG_FIXTURE),
    Pid.

teardown_collector(_) ->
    couch_stats_collector:stop(),
    ok.

teardown_aggregator(_, Pid) ->
    couch_stats_aggregator:stop(),
    couch_stats_collector:stop(),
    erlang:monitor(process, Pid),
    couch_config:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, config_stop})
    end,
    ok.


couch_stats_collector_test_() ->
    {
        "CouchDB stats collector tests",
        {
            foreach,
            fun setup_collector/0, fun teardown_collector/1,
            [
                should_increment_counter(),
                should_decrement_counter(),
                should_increment_and_decrement_counter(),
                should_record_absolute_values(),
                should_clear_absolute_values(),
                should_track_process_count(),
                should_increment_counter_multiple_times_per_pid(),
                should_decrement_counter_on_process_exit(),
                should_decrement_for_each_track_process_count_call_on_exit(),
                should_return_all_counters_and_absolute_values(),
                should_return_incremental_counters(),
                should_return_absolute_values()
            ]
        }
    }.

couch_stats_aggregator_test_() ->
    Funs = [
        fun should_init_empty_aggregate/2,
        fun should_get_empty_aggregate/2,
        fun should_change_stats_on_values_add/2,
        fun should_change_stats_for_all_times_on_values_add/2,
        fun should_change_stats_on_values_change/2,
        fun should_change_stats_for_all_times_on_values_change/2,
        fun should_not_remove_data_after_some_time_for_0_sample/2,
        fun should_remove_data_after_some_time_for_other_samples/2
    ],
    {
        "CouchDB stats aggregator tests",
        [
            {
                "Absolute values",
                {
                    foreachx,
                    fun setup_aggregator/1, fun teardown_aggregator/2,
                    [{absolute, Fun} || Fun <- Funs]
                }
            },
            {
                "Counters",
                {
                    foreachx,
                    fun setup_aggregator/1, fun teardown_aggregator/2,
                    [{counter, Fun} || Fun <- Funs]
                }
            }
        ]
    }.


should_increment_counter() ->
    ?_assertEqual(100,
        begin
            AddCount = fun() -> couch_stats_collector:increment(foo) end,
            repeat(AddCount, 100),
            couch_stats_collector:get(foo)
        end).

should_decrement_counter() ->
    ?_assertEqual(67,
        begin
            AddCount = fun() -> couch_stats_collector:increment(foo) end,
            RemCount = fun() -> couch_stats_collector:decrement(foo) end,
            repeat(AddCount, 100),
            repeat(RemCount, 33),
            couch_stats_collector:get(foo)
        end).

should_increment_and_decrement_counter() ->
    ?_assertEqual(0,
        begin
            AddCount = fun() -> couch_stats_collector:increment(foo) end,
            RemCount = fun() -> couch_stats_collector:decrement(foo) end,
            repeat(AddCount, 100),
            repeat(RemCount, 25),
            repeat(AddCount, 10),
            repeat(RemCount, 5),
            repeat(RemCount, 80),
            couch_stats_collector:get(foo)
        end).

should_record_absolute_values() ->
    ?_assertEqual(lists:seq(1, 15),
        begin
            lists:map(fun(Val) ->
                couch_stats_collector:record(bar, Val)
            end, lists:seq(1, 15)),
            couch_stats_collector:get(bar)
        end).

should_clear_absolute_values() ->
    ?_assertEqual(nil,
        begin
            lists:map(fun(Val) ->
                couch_stats_collector:record(bar, Val)
            end, lists:seq(1, 15)),
            couch_stats_collector:clear(bar),
            couch_stats_collector:get(bar)
        end).

should_track_process_count() ->
    ?_assertMatch({_, 1}, spawn_and_count(1)).

should_increment_counter_multiple_times_per_pid() ->
    ?_assertMatch({_, 3}, spawn_and_count(3)).

should_decrement_counter_on_process_exit() ->
    ?_assertEqual(2,
        begin
            {Pid, 1} = spawn_and_count(1),
            spawn_and_count(2),
            RefMon = erlang:monitor(process, Pid),
            Pid ! sepuku,
            receive
                {'DOWN', RefMon, _, _, _} -> ok
            after ?TIMEOUT ->
                throw(timeout)
            end,
            % sleep for awhile to let collector handle the updates
            % suddenly, it couldn't notice process death instantly
            timer:sleep(?TIMEWAIT),
            couch_stats_collector:get(hoopla)
        end).

should_decrement_for_each_track_process_count_call_on_exit() ->
    ?_assertEqual(2,
        begin
            {_, 2} = spawn_and_count(2),
            {Pid, 6} = spawn_and_count(4),
            RefMon = erlang:monitor(process, Pid),
            Pid ! sepuku,
            receive
                {'DOWN', RefMon, _, _, _} -> ok
            after ?TIMEOUT ->
                throw(timeout)
            end,
            timer:sleep(?TIMEWAIT),
            couch_stats_collector:get(hoopla)
        end).

should_return_all_counters_and_absolute_values() ->
    ?_assertEqual([{bar,[1.0,0.0]}, {foo,1}],
        begin
            couch_stats_collector:record(bar, 0.0),
            couch_stats_collector:record(bar, 1.0),
            couch_stats_collector:increment(foo),
            lists:sort(couch_stats_collector:all())
        end).

should_return_incremental_counters() ->
    ?_assertEqual([{foo,1}],
        begin
            couch_stats_collector:record(bar, 0.0),
            couch_stats_collector:record(bar, 1.0),
            couch_stats_collector:increment(foo),
            lists:sort(couch_stats_collector:all(incremental))
        end).

should_return_absolute_values() ->
    ?_assertEqual([{bar,[1.0,0.0]}, {zing, "Z"}],
        begin
            couch_stats_collector:record(bar, 0.0),
            couch_stats_collector:record(bar, 1.0),
            couch_stats_collector:record(zing, 90),
            couch_stats_collector:increment(foo),
            lists:sort(couch_stats_collector:all(absolute))
        end).

should_init_empty_aggregate(absolute, _) ->
    {Aggs} = couch_stats_aggregator:all(),
    ?_assertEqual({[{'11', make_agg(<<"randomosity">>,
                                    null, null, null, null, null)}]},
                  couch_util:get_value(number, Aggs));
should_init_empty_aggregate(counter, _) ->
    {Aggs} = couch_stats_aggregator:all(),
    ?_assertEqual({[{stuff, make_agg(<<"yay description">>,
                                     null, null, null, null, null)}]},
                  couch_util:get_value(testing, Aggs)).

should_get_empty_aggregate(absolute, _) ->
    ?_assertEqual(make_agg(<<"randomosity">>, null, null, null, null, null),
             couch_stats_aggregator:get_json({number, '11'}));
should_get_empty_aggregate(counter, _) ->
    ?_assertEqual(make_agg(<<"yay description">>, null, null, null, null, null),
             couch_stats_aggregator:get_json({testing, stuff})).

should_change_stats_on_values_add(absolute, _) ->
    lists:foreach(fun(X) ->
        couch_stats_collector:record({number, 11}, X)
    end, lists:seq(0, 10)),
    couch_stats_aggregator:collect_sample(),
    ?_assertEqual(make_agg(<<"randomosity">>, 5.0, 5.0, null, 5.0, 5.0),
                  couch_stats_aggregator:get_json({number, 11}));
should_change_stats_on_values_add(counter, _) ->
    lists:foreach(fun(_) ->
        couch_stats_collector:increment({testing, stuff})
    end, lists:seq(1, 100)),
    couch_stats_aggregator:collect_sample(),
    ?_assertEqual(make_agg(<<"yay description">>, 100.0, 100.0, null, 100, 100),
                  couch_stats_aggregator:get_json({testing, stuff})).

should_change_stats_for_all_times_on_values_add(absolute, _) ->
    lists:foreach(fun(X) ->
        couch_stats_collector:record({number, 11}, X)
    end, lists:seq(0, 10)),
    couch_stats_aggregator:collect_sample(),
    ?_assertEqual(make_agg(<<"randomosity">>, 5.0, 5.0, null, 5.0, 5.0),
                  couch_stats_aggregator:get_json({number, 11}, 1));
should_change_stats_for_all_times_on_values_add(counter, _) ->
    lists:foreach(fun(_) ->
        couch_stats_collector:increment({testing, stuff})
    end, lists:seq(1, 100)),
    couch_stats_aggregator:collect_sample(),
    ?_assertEqual(make_agg(<<"yay description">>, 100.0, 100.0, null, 100, 100),
                  couch_stats_aggregator:get_json({testing, stuff}, 1)).

should_change_stats_on_values_change(absolute, _) ->
    ?_assertEqual(make_agg(<<"randomosity">>, 20.0, 10.0, 7.071, 5.0, 15.0),
        begin
            lists:foreach(fun(X) ->
                couch_stats_collector:record({number, 11}, X)
            end, lists:seq(0, 10)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_collector:record({number, 11}, 15),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({number, 11})
        end);
should_change_stats_on_values_change(counter, _) ->
    ?_assertEqual(make_agg(<<"yay description">>, 100.0, 50.0, 70.711, 0, 100),
        begin
            lists:foreach(fun(_) ->
                couch_stats_collector:increment({testing, stuff})
            end, lists:seq(1, 100)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({testing, stuff})
        end).

should_change_stats_for_all_times_on_values_change(absolute, _) ->
    ?_assertEqual(make_agg(<<"randomosity">>, 20.0, 10.0, 7.071, 5.0, 15.0),
        begin
            lists:foreach(fun(X) ->
                couch_stats_collector:record({number, 11}, X)
            end, lists:seq(0, 10)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_collector:record({number, 11}, 15),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({number, 11}, 1)
        end);
should_change_stats_for_all_times_on_values_change(counter, _) ->
    ?_assertEqual(make_agg(<<"yay description">>, 100.0, 50.0, 70.711, 0, 100),
        begin
            lists:foreach(fun(_) ->
                couch_stats_collector:increment({testing, stuff})
            end, lists:seq(1, 100)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({testing, stuff}, 1)
        end).

should_not_remove_data_after_some_time_for_0_sample(absolute, _) ->
    ?_assertEqual(make_agg(<<"randomosity">>, 20.0, 10.0, 7.071, 5.0, 15.0),
        begin
            lists:foreach(fun(X) ->
                couch_stats_collector:record({number, 11}, X)
            end, lists:seq(0, 10)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_collector:record({number, 11}, 15),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({number, 11})
        end);
should_not_remove_data_after_some_time_for_0_sample(counter, _) ->
    ?_assertEqual(make_agg(<<"yay description">>, 100.0, 33.333, 57.735, 0, 100),
        begin
            lists:foreach(fun(_) ->
                couch_stats_collector:increment({testing, stuff})
            end, lists:seq(1, 100)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({testing, stuff})
        end).

should_remove_data_after_some_time_for_other_samples(absolute, _) ->
    ?_assertEqual(make_agg(<<"randomosity">>, 15.0, 15.0, null, 15.0, 15.0),
        begin
            lists:foreach(fun(X) ->
                couch_stats_collector:record({number, 11}, X)
            end, lists:seq(0, 10)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_collector:record({number, 11}, 15),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({number, 11}, 1)
        end);
should_remove_data_after_some_time_for_other_samples(counter, _) ->
    ?_assertEqual(make_agg(<<"yay description">>, 0, 0.0, 0.0, 0, 0),
        begin
            lists:foreach(fun(_) ->
                couch_stats_collector:increment({testing, stuff})
            end, lists:seq(1, 100)),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            timer:sleep(?TIMEWAIT),
            couch_stats_aggregator:collect_sample(),
            couch_stats_aggregator:get_json({testing, stuff}, 1)
        end).


spawn_and_count(N) ->
    Self = self(),
    Pid = spawn(fun() ->
        lists:foreach(
            fun(_) ->
                couch_stats_collector:track_process_count(hoopla)
            end, lists:seq(1,N)),
        Self ! reporting,
        receive
            sepuku -> ok
        end
    end),
    receive reporting -> ok end,
    {Pid, couch_stats_collector:get(hoopla)}.

repeat(_, 0) ->
    ok;
repeat(Fun, Count) ->
    Fun(),
    repeat(Fun, Count-1).

make_agg(Desc, Sum, Mean, StdDev, Min, Max) ->
    {[
        {description, Desc},
        {current, Sum},
        {sum, Sum},
        {mean, Mean},
        {stddev, StdDev},
        {min, Min},
        {max, Max}
    ]}.
