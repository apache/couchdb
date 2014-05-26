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

-define(TIMEOUT, 1000).
-define(SLEEPTIME, 100).


setup_collector() ->
    {ok, Pid} = couch_stats_collector:start(),
    Pid.

teardown_collector(Pid) ->
    erlang:monitor(process, Pid),
    couch_stats_collector:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, couch_stats_collector})
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
            timer:sleep(?SLEEPTIME),
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
            timer:sleep(?SLEEPTIME),
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
