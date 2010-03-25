-module(mem3_test).

-include("../include/common.hrl").
-include("../include/config.hrl").
-include_lib("eunit/include/eunit.hrl").

%% TEST SETUP

all_tests_test_() ->
    {"membership3 tests",
     [
      {setup,
       fun test_setup/0,
       fun test_teardown/1,
       fun(Pid) ->
           {with, Pid,
            [
             fun init/1,
             fun join_first/1
            ]}
       end}
     ]
    }.


test_setup() ->
    Config = #config{n=3,r=2,w=2,q=3,directory="/srv/db",
                     storage_mod="dynomite_couch_storage"},
    {ok, Pid} = mem3:start_link([{test,true}, {config, Config}]),
    Pid.


test_teardown(Pid) ->
    exit(Pid, shutdown).


%% TESTS

init(_Pid) ->
    #mem{args=Args} = mem3:state(),
    Test = proplists:get_value(test, Args),
    ?assertEqual(true, Test).


join_first(_Pid) ->
    mem3:join(first, [{1, a, []}, {2, b, []}]),
    Fullmap = mem3:fullmap(),
    ?assertEqual(16, length(Fullmap)),
    Pmap = mem3:partitions(),
    ?assertEqual(8, length(Pmap)),
    ok.
