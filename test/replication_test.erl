%%% -*-  erlang-indent-level:2  -*-
-module(replication_test).
-author('brad@cloudant.com').

-include("../include/config.hrl").
-include("../include/test.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(NODEA, {a, ["d", "1", "4"]}).
-define(NODEB, {b, ["e", "3", "1"]}).
-define(NODEC, {c, ["f", "1", "2"]}).
-define(NODED, {d, ["e", "1", "2"]}).
-define(NODEE, {e, ["e", "2", "2"]}).
-define(NODES, [?NODEA, ?NODEB, ?NODEC, ?NODED, ?NODEE]).

%% TODO: give this some effigy love, mock configuration up all of these
%%       different ways.

metadata_level_1_test() ->
  configuration:start_link(#config{n=3,r=1,w=1,q=6,
                                   directory=?TMP_DIR,
                                   meta=[{datacenter,roundrobin},
                                         {rack, roundrobin},
                                         {slot, roundrobin}
                                        ]}),
  Partners = replication:partners(?NODEA,
                                  [?NODEA, ?NODEB, ?NODEC],
                                  configuration:get_config()),
  ?assertEqual([?NODEB, ?NODEC], Partners),
  configuration:stop().


metadata_level_2_test() ->
  configuration:start_link(#config{n=3,r=1,w=1,q=6,
                                   directory=?TMP_DIR,
                                   meta=[{datacenter,roundrobin},
                                         {rack, roundrobin},
                                         {slot, roundrobin}
                                        ]}),
  Partners = replication:partners(?NODEA,
                                  ?NODES,
                                  configuration:get_config()),
  ?assertEqual([?NODED,?NODEE], Partners),
  configuration:stop().


no_metadata_test() ->
  configuration:start_link(#config{n=2,r=1,w=1,q=6,
                                   directory=?TMP_DIR,
                                   meta=[]}),
  Partners = replication:partners(a,
                                  [a,b,c,d],
                                  configuration:get_config()),
  ?assertEqual([b], Partners),
  configuration:stop().


wrap_test() ->
  configuration:start_link(#config{n=3,r=1,w=1,q=6,
                                   directory=?TMP_DIR,
                                   meta=[]}),
  Wrap1Partners = replication:partners(c,
                                      [a,b,c,d],
                                      configuration:get_config()),
  ?assertEqual([a,d], Wrap1Partners),
  Wrap2Partners = replication:partners(d,
                                      [a,b,c,d],
                                      configuration:get_config()),
  ?assertEqual([a,b], Wrap2Partners),
  configuration:stop().


self_test() ->
    configuration:start_link(#config{n=3,r=1,w=1,q=6,
                                   directory=?TMP_DIR,
                                   meta=[]}),
    Partners = replication:partners(a, [a],
                                    configuration:get_config()),
    ?assertEqual([], Partners),
    configuration:stop().


remove_self_test() ->
    configuration:start_link(
        #config{n=4,r=1,w=1,q=6, directory=?TMP_DIR, meta=[]}),
    Partners = replication:partners(a, [a,b], configuration:get_config()),
    ?assertEqual([b], Partners),
    configuration:stop().
