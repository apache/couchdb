-module(test_SUITE).

-compile(export_all).

-include_lib("ct.hrl").

all() ->
    [simple_test].

simple_test(Config) ->
    io:format("Test: ~p\n", [Config]).
