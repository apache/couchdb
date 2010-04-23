-module(mem3_test).

-include("../include/common.hrl").
-include("../include/config.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_NODE_NAME, a).
-define(HINT_C1, 365375409332725729550921208179070754913983135744).
-define(HINT_C2, 1096126227998177188652763624537212264741949407232).
-define(PARTS_FOR_D1, [365375409332725729550921208179070754913983135744,
                      548063113999088594326381812268606132370974703616,
                      730750818665451459101842416358141509827966271488,
                      913438523331814323877303020447676887284957839360,
                      1096126227998177188652763624537212264741949407232,
                      1278813932664540053428224228626747642198940975104]).
-define(x40, 365375409332725729550921208179070754913983135744).
-define(x60, 548063113999088594326381812268606132370974703616).

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
             fun join_first_with_hints/1,
             fun join_new_node/1,
             fun join_two_new_nodes/1,
             fun join_with_wrong_order/1
            ]}
       end}
     ]
    }.


test_setup() ->
    Config = #config{n=3,r=2,w=2,q=3,directory="/srv/db",
                     storage_mod="dynomite_couch_storage"},
    {ok, Pid} = mem3:start_link([{test,?TEST_NODE_NAME}, {config, Config}]),
    Pid.


test_teardown(Pid) ->
    exit(Pid, shutdown).


%% TESTS

init(_Pid) ->
    {ok, #mem{args=Args}} = mem3:state(),
    Test = proplists:get_value(test, Args),
    ?assertEqual(?TEST_NODE_NAME, Test).


clock(_Pid) ->
    {ok, Clock} = mem3:clock(),
    ?assertMatch([{?TEST_NODE_NAME, _}], Clock).


join_first(_Pid) ->
    mem3:reset(),
    mem3:join(first, [{1, a, []}, {2, b, []}]),
    Fullmap = mem3:fullmap(),
    ?assertEqual(16, length(Fullmap)),
    Pmap = mem3:partitions(),
    ?assertEqual(8, length(Pmap)),
    ok.


join_first_with_hints(_Pid) ->
    mem3:reset(),
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


join_new_node(_Pid) ->
    mem3:reset(),
    mem3:join(first, [{1, a, []}, {2, b, []}, {3, c, []}]),
    ?assertEqual(24, length(mem3:fullmap())),
    ?assertEqual([], mem3:parts_for_node(d)),
    mem3:join(new, [{4, d, []}]),
    ?assertEqual(?PARTS_FOR_D1, mem3:parts_for_node(d)),
    %?debugFmt("~nFullmap: ~p~n", [mem3:fullmap()]),
    ok.


join_two_new_nodes(_Pid) ->
    mem3:reset(),
    mem3:join(first, [{1, a, []}, {2, b, []}, {3, c, []}]),
    ?assertEqual([], mem3:parts_for_node(d)),
    Res = mem3:join(new, [{4, d, []}, {5, e, []}]),
    ?assertEqual(ok, Res),
    ?assertEqual([a,d,e], mem3:nodes_for_part(?x40)),
    ?assertEqual([c,d,e], mem3:nodes_for_part(?x60)),
    %?debugFmt("~nFullmap: ~p~n", [mem3:fullmap()]),
    ok.


join_with_wrong_order(_Pid) ->
    mem3:reset(),
    mem3:join(first, [{1, a, []}, {2, b, []}, {3, c, []}]),
    ?assertEqual([], mem3:parts_for_node(d)),
    %?debugFmt("~nFullmap: ~p~n", [mem3:fullmap()]),
    Res = mem3:join(new, [{3, d, []}]),
    ?assertEqual({error,{position_exists,3,c}}, Res),
    %?debugFmt("~nFullmap: ~p~n", [mem3:fullmap()]),
    ok.


merge_nodes_test() ->
    A = [{1,a1,[]},{2,a2,[]},{3,a3,[]}],
    B = [{1,a1,[]},{2,a2,[]},{3,b3,[]}],
    ?assertEqual(A, mem3:merge_nodes(A,B)),
    ?assertEqual(mem3:merge_nodes(A,B), mem3:merge_nodes(B,A)),
    C = [{1,c1,[]},{2,c2,[]},{3,c3,[]}],
    ?assertEqual(A, mem3:merge_nodes(A,C)),
    ?assertEqual(A, mem3:merge_nodes(C,A)),
    ok.


merge_nodes_with_init_nodelist_test() ->
    A = [{1,a1,[]},{2,a2,[]},{3,a3,[]}],
    B = [{0, b, []}],
    ?assertEqual(A, mem3:merge_nodes(A,B)),
    ?assertEqual(mem3:merge_nodes(A,B), mem3:merge_nodes(B,A)),
    ok.
