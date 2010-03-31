-module(mem3_test).

-include("../include/common.hrl").
-include("../include/config.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HINT_C1, 365375409332725729550921208179070754913983135744).
-define(HINT_C2, 1096126227998177188652763624537212264741949407232).

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
             fun clock/1,
             fun join_first/1,
             fun join_first_with_hints/1
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


clock(_Pid) ->
    Node = node(),
    Clock = mem3:clock(),
    ?assertMatch([{Node, _}], Clock).


join_first(_Pid) ->
    mem3:join(first, [{1, a, []}, {2, b, []}]),
    Fullmap = mem3:fullmap(),
    ?assertEqual(16, length(Fullmap)),
    Pmap = mem3:partitions(),
    ?assertEqual(8, length(Pmap)),
    ok.


join_first_with_hints(_Pid) ->
    mem3:join(first, [{1, a, []},
                      {2, b, []},
                      {3, c, [{hints, [?HINT_C1, ?HINT_C2]}]},
                      {4, d, []},
                      {5, e, []}]),
    Fullmap = mem3:fullmap(),
    ?assertEqual(24, length(Fullmap)),
    Pmap = mem3:partitions(),
    ?assertEqual(8, length(Pmap)),
    %?debugFmt("~nFullmap: ~p~n", [Fullmap]),
    ?assertEqual([c,d,e], mem3:nodes_for_part(?HINT_C1)),
    ?assertEqual([c,d,e], mem3:nodes_for_part(?HINT_C2)),
    ok.
