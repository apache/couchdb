%%% -*-  erlang-indent-level:2  -*-
-module(partitions_test).
-author('brad@cloudant.com').

-include("../include/config.hrl").
-include("../include/common.hrl").
-include("../include/test.hrl").


join_test() ->
  TableA = [{a,1},{a,2},{a,3},{a,4},{a,5},{a,6},{a,7},{a,8}],
  TableB = [{a,1},{a,2},{a,3},{a,4},{b,5},{b,6},{b,7},{b,8}],
  TableC = [{a,1},{a,2},{a,3},{c,4},{b,5},{b,6},{b,7},{c,8}],
  TableD = [{a,1},{a,2},{d,3},{c,4},{b,5},{b,6},{d,7},{c,8}],
  TableE = [{a,1},{a,2},{d,3},{c,4},{b,5},{b,6},{e,7},{c,8}],
  TableF = [{a,1},{a,2},{d,3},{c,4},{b,5},{b,6},{e,7},{f,8}],
  TableG = [{a,1},{a,2},{d,3},{c,4},{b,5},{g,6},{e,7},{f,8}],
  TableH = [{a,1},{h,2},{d,3},{c,4},{b,5},{g,6},{e,7},{f,8}],
  ?assertEqual({ok,TableA}, partitions:join(a, TableA, [])),
  ?assertEqual({ok,TableB}, partitions:join(b, TableA, [])),
  ?assertEqual({ok,TableC}, partitions:join(c, TableB, [])),
  ?assertEqual({ok,TableD}, partitions:join(d, TableC, [])),
  ?assertEqual({ok,TableE}, partitions:join(e, TableD, [])),
  ?assertEqual({ok,TableF}, partitions:join(f, TableE, [])),
  ?assertEqual({ok,TableG}, partitions:join(g, TableF, [])),
  ?assertEqual({ok,TableH}, partitions:join(h, TableG, [])),
  ?assertEqual({error, "Too many nodes vs partitions", TableH},
               partitions:join(i, TableH, [])),
  ok.


hints_test() ->
  TableA = [{a,1},{a,2},{a,3},{a,4},{a,5},{a,6},{a,7},{a,8}],
  TableB = [{a,1},{b,2},{a,3},{a,4},{a,5},{b,6},{b,7},{b,8}],
  TableC = [{a,1},{a,2},{a,3},{a,4},{c,5},{c,6},{c,7},{c,8}],
  TableD = [{d,1},{d,2},{d,3},{d,4},{a,5},{a,6},{a,7},{a,8}],
  ?assertEqual({ok, TableB}, partitions:join(b, TableA, [2])),
  ?assertEqual({ok, TableC}, partitions:join(c, TableA, [0])),
  ?assertEqual({ok, TableD}, partitions:join(d, TableA, [1,2,3,4])),
  ok.


shard_name_test() ->
  ?assertEqual(<<"x000000/dbname_000000">>,
               partitions:shard_name(0, <<"dbname">>)),
  ok.


%% note: fullmaps used here
diff_same_length_test() ->
  OldMap = [{a,1, type},{a,2, type},{b,3, type},{b,4, type}],
  NewMap = [{a,1, type},{a,2, type},{b,3, type},{c,4, type}],
  ?assertEqual([{b,c,4}], partitions:diff(OldMap, NewMap)),
  ok.


diff_dupes_test() ->
  OldMap = [{'node1@node1.boorad.local',0,primary},
          {'node2@node2.boorad.local',0,partner},
          {'node3@node3.boorad.local',0,partner},
          {'node1@node1.boorad.local',182687704666362864775460604089535377456991567872, primary},
          {'node2@node2.boorad.local',182687704666362864775460604089535377456991567872, partner},
          {'node3@node3.boorad.local',182687704666362864775460604089535377456991567872, partner},
          {'node1@node1.boorad.local',365375409332725729550921208179070754913983135744, primary},
          {'node2@node2.boorad.local',365375409332725729550921208179070754913983135744, partner},
          {'node3@node3.boorad.local',365375409332725729550921208179070754913983135744, partner},
          {'node1@node1.boorad.local',548063113999088594326381812268606132370974703616, partner},
          {'node2@node2.boorad.local',548063113999088594326381812268606132370974703616, partner},
          {'node3@node3.boorad.local',548063113999088594326381812268606132370974703616, primary},
          {'node1@node1.boorad.local',730750818665451459101842416358141509827966271488, partner},
          {'node2@node2.boorad.local',730750818665451459101842416358141509827966271488, primary},
          {'node3@node3.boorad.local',730750818665451459101842416358141509827966271488, partner},
          {'node1@node1.boorad.local',913438523331814323877303020447676887284957839360, partner},
          {'node2@node2.boorad.local',913438523331814323877303020447676887284957839360, primary},
          {'node3@node3.boorad.local',913438523331814323877303020447676887284957839360, partner},
          {'node1@node1.boorad.local',1096126227998177188652763624537212264741949407232, partner},
          {'node2@node2.boorad.local',1096126227998177188652763624537212264741949407232, primary},
          {'node3@node3.boorad.local',1096126227998177188652763624537212264741949407232, partner},
          {'node1@node1.boorad.local',1278813932664540053428224228626747642198940975104, partner},
          {'node2@node2.boorad.local',1278813932664540053428224228626747642198940975104, partner},
          {'node3@node3.boorad.local',1278813932664540053428224228626747642198940975104, primary}],
  NewMap = [{'node1@node1.boorad.local',0,primary},
          {'node2@node2.boorad.local',0,partner},
          {'node3@node3.boorad.local',0,partner},
          {'node1@node1.boorad.local',182687704666362864775460604089535377456991567872, primary},
          {'node2@node2.boorad.local',182687704666362864775460604089535377456991567872, partner},
          {'node3@node3.boorad.local',182687704666362864775460604089535377456991567872, partner},
          {'node1@node1.boorad.local',365375409332725729550921208179070754913983135744, partner},
          {'node2@node2.boorad.local',365375409332725729550921208179070754913983135744, partner},
          {'node4@node4.boorad.local',365375409332725729550921208179070754913983135744, primary},
          {'node1@node1.boorad.local',548063113999088594326381812268606132370974703616, partner},
          {'node3@node3.boorad.local',548063113999088594326381812268606132370974703616, primary},
          {'node4@node4.boorad.local',548063113999088594326381812268606132370974703616, partner},
          {'node2@node2.boorad.local',730750818665451459101842416358141509827966271488, primary},
          {'node3@node3.boorad.local',730750818665451459101842416358141509827966271488, partner},
          {'node4@node4.boorad.local',730750818665451459101842416358141509827966271488, partner},
          {'node2@node2.boorad.local',913438523331814323877303020447676887284957839360, primary},
          {'node3@node3.boorad.local',913438523331814323877303020447676887284957839360, partner},
          {'node4@node4.boorad.local',913438523331814323877303020447676887284957839360, partner},
          {'node1@node1.boorad.local',1096126227998177188652763624537212264741949407232, partner},
          {'node2@node2.boorad.local',1096126227998177188652763624537212264741949407232, partner},
          {'node4@node4.boorad.local',1096126227998177188652763624537212264741949407232, primary},
          {'node1@node1.boorad.local',1278813932664540053428224228626747642198940975104, partner},
          {'node3@node3.boorad.local',1278813932664540053428224228626747642198940975104, primary},
          {'node4@node4.boorad.local',1278813932664540053428224228626747642198940975104, partner}],

  Diff = [{'node3@node3.boorad.local','node4@node4.boorad.local',
           365375409332725729550921208179070754913983135744},
          {'node2@node2.boorad.local','node4@node4.boorad.local',
           548063113999088594326381812268606132370974703616},
          {'node1@node1.boorad.local','node4@node4.boorad.local',
           730750818665451459101842416358141509827966271488},
          {'node1@node1.boorad.local','node4@node4.boorad.local',
           913438523331814323877303020447676887284957839360},
          {'node3@node3.boorad.local','node4@node4.boorad.local',
           1096126227998177188652763624537212264741949407232},
          {'node2@node2.boorad.local','node4@node4.boorad.local',
           1278813932664540053428224228626747642198940975104}],

  ?assertEqual(Diff, partitions:diff(OldMap, NewMap)),
  ok.
