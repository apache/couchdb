-module(mem3_test).

-include("../include/common.hrl").
-include("../include/config.hrl").
-include_lib("eunit/include/eunit.hrl").

%% version 3 of membership state
-record(mem, {header=3,
              nodes=[],
              clock=[],
              args
             }).

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
             fun join_init/1,
             fun join_init_with_hints/1,
             fun join_new_node/1,
             fun join_two_new_nodes/1,
             fun join_with_wrong_order/1
            ]}
       end}
     ]
    }.


test_setup() ->
    % Config = #config{n=3,r=2,w=2,q=3,directory="/srv/db",
    %                  storage_mod="dynomite_couch_storage"},
    {ok, Pid} = mem3:start_link([{test,?TEST_NODE_NAME}]),
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
    ?assertMatch([], Clock).


join_init(_Pid) ->
    mem3:reset(),
    mem3:join(init, [{1, a, []}, {2, b, []}], nil),
    {ok, Nodes} = mem3:nodes(),
    ?assertEqual(2, length(Nodes)),
    ok.


join_init_with_hints(_Pid) ->
    mem3:reset(),
    mem3:join(init, [{1, a, []},
                      {2, b, []},
                      {3, c, [{hints, [?HINT_C1, ?HINT_C2]}]},
                      {4, d, []},
                      {5, e, []}],
              nil),
    {ok, Nodes} = mem3:nodes(),
    ?assertEqual(5, length(Nodes)),
    %?debugFmt("~nFullmap: ~p~n", [Fullmap]),
%    ?assertEqual([c,d,e], mem3:nodes_for_part(?HINT_C1)),
%    ?assertEqual([c,d,e], mem3:nodes_for_part(?HINT_C2)),
    ok.


join_new_node(_Pid) ->
    mem3:reset(),
    mem3:join(init, [{1, a, []}, {2, b, []}, {3, c, []}], nil),
    {ok, Nodes1} = mem3:nodes(),
    ?assertEqual(3, length(Nodes1)),
    mem3:join(join, [{4, d, []}], a),
    {ok, Nodes2} = mem3:nodes(),
    ?assertEqual(4, length(Nodes2)),
    ok.


join_two_new_nodes(_Pid) ->
    mem3:reset(),
    mem3:join(init, [{1, a, []}, {2, b, []}, {3, c, []}], nil),
    {ok, Nodes1} = mem3:nodes(),
    ?assertEqual(3, length(Nodes1)),
    Res = mem3:join(join, [{4, d, []}, {5, e, []}], b),
    ?assertEqual(ok, Res),
    {ok, Nodes2} = mem3:nodes(),
    ?assertEqual(5, length(Nodes2)),
    %?debugFmt("~nFullmap: ~p~n", [mem3:fullmap()]),
    ok.


join_with_wrong_order(_Pid) ->
    mem3:reset(),
    mem3:join(init, [{1, a, []}, {2, b, []}, {3, c, []}], nil),
%    ?assertEqual([], mem3:parts_for_node(d)),
    %?debugFmt("~nFullmap: ~p~n", [mem3:fullmap()]),
    Res = mem3:join(join, [{3, d, []}], c),
    ?assertEqual({error, <<"position_exists_3">>}, Res),
    %?debugFmt("~nFullmap: ~p~n", [mem3:fullmap()]),
    ok.


%%
%% tests without running gen_server
%%
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


next_up_nodes_test() ->
    Nodes = [a,b,c,d],
    UpNodes = [a,b,d],
    ?assertEqual(b, mem3:next_up_node(a,Nodes,UpNodes)),
    ?assertEqual(d, mem3:next_up_node(b,Nodes,UpNodes)),
    ?assertEqual(a, mem3:next_up_node(d,Nodes,UpNodes)),
    ?assertThrow({error, no_gossip_targets_available},
                 mem3:next_up_node(a,[a,b,c],[])),
    ?assertEqual(b, mem3:next_up_node(a,[a,b],[a,b])),
    ok.
