-module(mem3_test).

-include("../include/common.hrl").
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
             fun init/1
            ]}
       end}
     ]
    }.


test_setup() ->
    {ok, Pid} = mem3:start_link(test),
    Pid.


test_teardown(Pid) ->
    exit(Pid, shutdown).


%% TESTS

init(_Pid) ->
    State = #mem{test=Test} = mem3:state(),
    ?debugFmt("~nState: ~p~n", [State]),
    ?assertEqual(true, Test).
