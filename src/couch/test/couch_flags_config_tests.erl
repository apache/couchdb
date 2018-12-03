-module(couch_flags_config_tests).
-include_lib("eunit/include/eunit.hrl").

%% value copied from couch_flags_config
-define(MAX_FLAG_NAME_LENGTH, 256).

all_combinations_return_same_result_test_() ->
    Config = [
         {"foo, bar||*", "true"},
         {"baz, qux||*", "false"},
         {"baz||shards/test*", "true"},
         {"baz||shards/blacklist*", "false"},
         {"bar||shards/test*", "false"},
         {"bar||shards/test/blacklist*", "true"}
    ],
    Expected = [
        {{<<"shards/test/blacklist*">>},{<<"shards/test/blacklist*">>,22,[bar, foo]}},
        {{<<"shards/test*">>},{<<"shards/test*">>, 12, [baz, foo]}},
        {{<<"shards/blacklist*">>},{<<"shards/blacklist*">>, 17, [bar, foo]}},
        {{<<"*">>},{<<"*">>, 1, [bar, foo]}}
    ],
    Combinations = couch_tests_combinatorics:permutations(Config),
    [{test_id(Items), ?_assertEqual(Expected, couch_flags_config:data(Items))}
        || Items <- Combinations].

rules_are_sorted_test() ->
    Expected = [
        {{<<"shards/test/exact">>},{<<"shards/test/exact">>, 17, [baz,flag_bar,flag_foo]}},
        {{<<"shards/test/blacklist*">>},{<<"shards/test/blacklist*">>,22,[flag_foo]}},
        {{<<"shards/test*">>},{<<"shards/test*">>, 12, [baz,flag_bar,flag_foo]}},
        {{<<"shards/exact">>},{<<"shards/exact">>, 12, [flag_bar,flag_foo]}},
        {{<<"shards/blacklist*">>},{<<"shards/blacklist*">>, 17, []}},
        {{<<"*">>},{<<"*">>, 1, [flag_foo]}}
    ],
    ?assertEqual(Expected, couch_flags_config:data(test_config())).

latest_overide_wins_test_() ->
    Cases = [
        {[
            {"flag||*", "false"}, {"flag||a*", "true"},
            {"flag||ab*", "true"}, {"flag||abc*", "true"}
        ], true},
        {[
            {"flag||*", "true"}, {"flag||a*", "false"},
            {"flag||ab*", "true"}, {"flag||abc*", "false"}
        ], false}
    ],
    [{test_id(Rules, Expected),
        ?_assertEqual(Expected, lists:member(flag,
            flags(hd(couch_flags_config:data(Rules)))))}
            || {Rules, Expected} <- Cases].

flags({{_Pattern}, {_Pattern, _Size, Flags}}) ->
    Flags.

test_id(Items, ExpectedResult) ->
    lists:flatten(io_lib:format("~p -> ~p", [[P || {P, _} <- Items], ExpectedResult])).


test_id(Items) ->
    lists:flatten(io_lib:format("~p", [[P || {P, _} <- Items]])).

test_config() ->
    [
        {"flag_foo||*", "true"},
        {"flag_bar||*", "false"},
        {"flag_bar||shards/test*", "true"},
        {"flag_foo||shards/blacklist*", "false"},
        {"baz||shards/test*", "true"},
        {"baz||shards/test/blacklist*", "false"},
        {"flag_bar||shards/exact", "true"},
        {"flag_bar||shards/test/exact", "true"}
    ].

parse_flags_term_test_() ->
    LongBinary = binary:copy(<<"a">>, ?MAX_FLAG_NAME_LENGTH + 1),
    ExpectedError = {error, {"Cannot parse list of tags: ~n~p",
       [{too_long, LongBinary}]}},
    ExpectedUnknownError = {error,{"Cannot parse list of tags: ~n~p",
       [{invalid_flag,<<"dddddddd">>}]}},
	[
		{"empty binary", ?_assertEqual(
		    [], couch_flags_config:parse_flags_term(<<>>))},
		{"single flag", ?_assertEqual(
		    [fff], couch_flags_config:parse_flags_term(<<"fff">>))},
		{"sorted", ?_assertEqual(
		    [aaa,bbb,fff], couch_flags_config:parse_flags_term(<<"fff,aaa,bbb">>))},
		{"whitespace", ?_assertEqual(
		    [aaa,bbb,fff], couch_flags_config:parse_flags_term(<<"fff , aaa, bbb ">>))},
		{"error", ?_assertEqual(
		    ExpectedError, couch_flags_config:parse_flags_term(LongBinary))},
		{"unknown_flag", ?_assertEqual(
		    ExpectedUnknownError, couch_flags_config:parse_flags_term(<<"dddddddd">>))}
	].

