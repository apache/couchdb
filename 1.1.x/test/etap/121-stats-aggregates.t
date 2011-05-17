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

ini_file() ->
    test_util:source_file("test/etap/121-stats-aggregates.ini").

cfg_file() ->
    test_util:source_file("test/etap/121-stats-aggregates.cfg").

main(_) ->
    test_util:init_code_path(),
    etap:plan(17),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

test() ->
    couch_config:start_link([ini_file()]),
    couch_stats_collector:start(),
    couch_stats_aggregator:start(cfg_file()),
    ok = test_all_empty(),
    ok = test_get_empty(),
    ok = test_count_stats(),
    ok = test_abs_stats(),
    ok.

test_all_empty() ->
    {Aggs} = couch_stats_aggregator:all(),

    etap:is(length(Aggs), 2, "There are only two aggregate types in testing."),
    etap:is(
        couch_util:get_value(testing, Aggs),
        {[{stuff, make_agg(<<"yay description">>,
            null, null, null, null, null)}]},
        "{testing, stuff} is empty at start."
    ),
    etap:is(
        couch_util:get_value(number, Aggs),
        {[{'11', make_agg(<<"randomosity">>,
            null, null, null, null, null)}]},
        "{number, '11'} is empty at start."
    ),
    ok.
    
test_get_empty() ->
    etap:is(
        couch_stats_aggregator:get_json({testing, stuff}),
        make_agg(<<"yay description">>, null, null, null, null, null),
        "Getting {testing, stuff} returns an empty aggregate."
    ),
    etap:is(
        couch_stats_aggregator:get_json({number, '11'}),
        make_agg(<<"randomosity">>, null, null, null, null, null),
        "Getting {number, '11'} returns an empty aggregate."
    ),
    ok.

test_count_stats() ->
    lists:foreach(fun(_) ->
        couch_stats_collector:increment({testing, stuff})
    end, lists:seq(1, 100)),
    couch_stats_aggregator:collect_sample(),
    etap:is(
        couch_stats_aggregator:get_json({testing, stuff}),
        make_agg(<<"yay description">>, 100, 100, null, 100, 100),
        "COUNT: Adding values changes the stats."
    ),
    etap:is(
        couch_stats_aggregator:get_json({testing, stuff}, 1),
        make_agg(<<"yay description">>, 100, 100, null, 100, 100),
        "COUNT: Adding values changes stats for all times."
    ),

    timer:sleep(500),
    couch_stats_aggregator:collect_sample(),
    etap:is(
        couch_stats_aggregator:get_json({testing, stuff}),
        make_agg(<<"yay description">>, 100, 50, 70.711, 0, 100),
        "COUNT: Removing values changes stats."
    ),
    etap:is(
        couch_stats_aggregator:get_json({testing, stuff}, 1),
        make_agg(<<"yay description">>, 100, 50, 70.711, 0, 100),
        "COUNT: Removing values changes stats for all times."
    ),

    timer:sleep(600),
    couch_stats_aggregator:collect_sample(),
    etap:is(
        couch_stats_aggregator:get_json({testing, stuff}),
        make_agg(<<"yay description">>, 100, 33.333, 57.735, 0, 100),
        "COUNT: Letting time passes doesn't remove data from time 0 aggregates"
    ),
    etap:is(
        couch_stats_aggregator:get_json({testing, stuff}, 1),
        make_agg(<<"yay description">>, 0, 0, 0, 0, 0),
        "COUNT: Letting time pass removes data from other time aggregates."
    ),
    ok.

test_abs_stats() ->
    lists:foreach(fun(X) ->
        couch_stats_collector:record({number, 11}, X)
    end, lists:seq(0, 10)),
    couch_stats_aggregator:collect_sample(),
    etap:is(
        couch_stats_aggregator:get_json({number, 11}),
        make_agg(<<"randomosity">>, 5, 5, null, 5, 5),
        "ABS: Adding values changes the stats."
    ),
    etap:is(
        couch_stats_aggregator:get_json({number, 11}, 1),
        make_agg(<<"randomosity">>, 5, 5, null, 5, 5),
        "ABS: Adding values changes stats for all times."
    ),

    timer:sleep(500),
    couch_stats_collector:record({number, 11}, 15),
    couch_stats_aggregator:collect_sample(),
    etap:is(
        couch_stats_aggregator:get_json({number, 11}),
        make_agg(<<"randomosity">>, 20, 10, 7.071, 5, 15),
        "ABS: New values changes stats"
    ),
    etap:is(
        couch_stats_aggregator:get_json({number, 11}, 1),
        make_agg(<<"randomosity">>, 20, 10, 7.071, 5, 15),
        "ABS: Removing values changes stats for all times."
    ),

    timer:sleep(600),
    couch_stats_aggregator:collect_sample(),
    etap:is(
        couch_stats_aggregator:get_json({number, 11}),
        make_agg(<<"randomosity">>, 20, 10, 7.071, 5, 15),
        "ABS: Letting time passes doesn't remove data from time 0 aggregates"
    ),
    etap:is(
        couch_stats_aggregator:get_json({number, 11}, 1),
        make_agg(<<"randomosity">>, 15, 15, null, 15, 15),
        "ABS: Letting time pass removes data from other time aggregates."
    ),
    ok.

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
