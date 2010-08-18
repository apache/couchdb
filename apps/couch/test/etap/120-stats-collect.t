#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    test_util:init_code_path(),
    etap:plan(11),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    couch_stats_collector:start(),
    ok = test_counters(),
    ok = test_abs_values(),
    ok = test_proc_counting(),
    ok = test_all(),
    ok.

test_counters() ->
    AddCount = fun() -> couch_stats_collector:increment(foo) end,
    RemCount = fun() -> couch_stats_collector:decrement(foo) end,
    repeat(AddCount, 100),
    repeat(RemCount, 25),
    repeat(AddCount, 10),
    repeat(RemCount, 5),
    etap:is(
        couch_stats_collector:get(foo),
        80,
        "Incrememnt tracks correctly."
    ),

    repeat(RemCount, 80),
    etap:is(
        couch_stats_collector:get(foo),
        0,
        "Decremented to zaro."
    ),
    ok.

test_abs_values() ->
    lists:map(fun(Val) ->
        couch_stats_collector:record(bar, Val)
    end, lists:seq(1, 15)),
    etap:is(
        couch_stats_collector:get(bar),
        lists:seq(1, 15),
        "Absolute values are recorded correctly."
    ),
    
    couch_stats_collector:clear(bar),
    etap:is(
        couch_stats_collector:get(bar),
        nil,
        "Absolute values are cleared correctly."
    ),
    ok.

test_proc_counting() ->
    Self = self(),
    OnePid = spawn(fun() ->
        couch_stats_collector:track_process_count(hoopla),
        Self ! reporting,
        receive sepuku -> ok end
    end),
    R1 = erlang:monitor(process, OnePid),
    receive reporting -> ok end,
    etap:is(
        couch_stats_collector:get(hoopla),
        1,
        "track_process_count incrememnts the counter."
    ),
    
    TwicePid = spawn(fun() ->
        couch_stats_collector:track_process_count(hoopla),
        couch_stats_collector:track_process_count(hoopla),
        Self ! reporting,
        receive sepuku -> ok end
    end),
    R2 = erlang:monitor(process, TwicePid),
    receive reporting -> ok end,
    etap:is(
        couch_stats_collector:get(hoopla),
        3,
        "track_process_count allows more than one incrememnt per Pid"
    ),
    
    OnePid ! sepuku,
    receive {'DOWN', R1, _, _, _} -> ok end,
    timer:sleep(250),
    etap:is(
        couch_stats_collector:get(hoopla),
        2,
        "Process count is decremented when process exits."
    ),
    
    TwicePid ! sepuku,
    receive {'DOWN', R2, _, _, _} -> ok end,
    timer:sleep(250),
    etap:is(
        couch_stats_collector:get(hoopla),
        0,
        "Process count is decremented for each call to track_process_count."
    ),
    ok.

test_all() ->
    couch_stats_collector:record(bar, 0.0),
    couch_stats_collector:record(bar, 1.0),
    etap:is(
        couch_stats_collector:all(),
        [{foo, 0}, {hoopla, 0}, {bar, [1.0, 0.0]}],
        "all/0 returns all counters and absolute values."
    ),
    
    etap:is(
        couch_stats_collector:all(incremental),
        [{foo, 0}, {hoopla, 0}],
        "all/1 returns only the specified type."
    ),
    
    couch_stats_collector:record(zing, 90),
    etap:is(
        couch_stats_collector:all(absolute),
        [{zing, [90]}, {bar, [1.0, 0.0]}],
        "all/1 returns only the specified type."
    ),
    ok.

repeat(_, 0) ->
    ok;
repeat(Fun, Count) ->
    Fun(),
    repeat(Fun, Count-1).
