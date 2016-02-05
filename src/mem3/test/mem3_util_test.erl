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

-module(mem3_util_test).

-include("mem3.hrl").
-include_lib("eunit/include/eunit.hrl").

hash_test() ->
    ?assertEqual(1624516141,mem3_util:hash(0)),
    ?assertEqual(3816901808,mem3_util:hash("0")),
    ?assertEqual(3523407757,mem3_util:hash(<<0>>)),
    ?assertEqual(4108050209,mem3_util:hash(<<"0">>)),
    ?assertEqual(3094724072,mem3_util:hash(zero)),
    ok.

name_shard_test() ->
    Shard1 = #shard{},
    ?assertError(function_clause, mem3_util:name_shard(Shard1, ".1234")),

    Shard2 = #shard{dbname = <<"testdb">>, range = [0,100]},
    #shard{name=Name2} = mem3_util:name_shard(Shard2, ".1234"),
    ?assertEqual(<<"shards/00000000-00000064/testdb.1234">>, Name2),

    ok.

create_partition_map_test() ->
    {DbName1, N1, Q1, Nodes1} = {<<"testdb1">>, 3, 4, [a,b,c,d]},
    Map1 = mem3_util:create_partition_map(DbName1, N1, Q1, Nodes1),
    ?assertEqual(12, length(Map1)),

    {DbName2, N2, Q2, Nodes2} = {<<"testdb2">>, 1, 1, [a,b,c,d]},
    [#shard{name=Name2,node=Node2}] = Map2 =
        mem3_util:create_partition_map(DbName2, N2, Q2, Nodes2, ".1234"),
    ?assertEqual(1, length(Map2)),
    ?assertEqual(<<"shards/00000000-ffffffff/testdb2.1234">>, Name2),
    ?assertEqual(a, Node2),
    ok.

build_shards_test() ->
    DocProps1 =
         [{<<"changelog">>,
            [[<<"add">>,<<"00000000-1fffffff">>,
              <<"bigcouch@node.local">>],
             [<<"add">>,<<"20000000-3fffffff">>,
              <<"bigcouch@node.local">>],
             [<<"add">>,<<"40000000-5fffffff">>,
              <<"bigcouch@node.local">>],
             [<<"add">>,<<"60000000-7fffffff">>,
              <<"bigcouch@node.local">>],
             [<<"add">>,<<"80000000-9fffffff">>,
              <<"bigcouch@node.local">>],
             [<<"add">>,<<"a0000000-bfffffff">>,
              <<"bigcouch@node.local">>],
             [<<"add">>,<<"c0000000-dfffffff">>,
              <<"bigcouch@node.local">>],
             [<<"add">>,<<"e0000000-ffffffff">>,
              <<"bigcouch@node.local">>]]},
           {<<"by_node">>,
            {[{<<"bigcouch@node.local">>,
               [<<"00000000-1fffffff">>,<<"20000000-3fffffff">>,
                <<"40000000-5fffffff">>,<<"60000000-7fffffff">>,
                <<"80000000-9fffffff">>,<<"a0000000-bfffffff">>,
                <<"c0000000-dfffffff">>,<<"e0000000-ffffffff">>]}]}},
           {<<"by_range">>,
            {[{<<"00000000-1fffffff">>,[<<"bigcouch@node.local">>]},
              {<<"20000000-3fffffff">>,[<<"bigcouch@node.local">>]},
              {<<"40000000-5fffffff">>,[<<"bigcouch@node.local">>]},
              {<<"60000000-7fffffff">>,[<<"bigcouch@node.local">>]},
              {<<"80000000-9fffffff">>,[<<"bigcouch@node.local">>]},
              {<<"a0000000-bfffffff">>,[<<"bigcouch@node.local">>]},
              {<<"c0000000-dfffffff">>,[<<"bigcouch@node.local">>]},
              {<<"e0000000-ffffffff">>,[<<"bigcouch@node.local">>]}]}}],
    Shards1 = mem3_util:build_shards(<<"testdb1">>, DocProps1),
    ExpectedShards1 =
        [{shard,<<"shards/00000000-1fffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [0,536870911],
          undefined,[]},
         {shard,<<"shards/20000000-3fffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [536870912,1073741823],
          undefined,[]},
         {shard,<<"shards/40000000-5fffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [1073741824,1610612735],
          undefined,[]},
         {shard,<<"shards/60000000-7fffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [1610612736,2147483647],
          undefined,[]},
         {shard,<<"shards/80000000-9fffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [2147483648,2684354559],
          undefined,[]},
         {shard,<<"shards/a0000000-bfffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [2684354560,3221225471],
          undefined,[]},
         {shard,<<"shards/c0000000-dfffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [3221225472,3758096383],
          undefined,[]},
         {shard,<<"shards/e0000000-ffffffff/testdb1">>,
          'bigcouch@node.local',<<"testdb1">>,
          [3758096384,4294967295],
          undefined,[]}],
    ?assertEqual(ExpectedShards1, Shards1),
    ok.


%% n_val tests

nval_test_() ->
    {"n_val tests explicit",
     [
      {setup,
       fun () ->
               meck:new([couch_log]),
               meck:expect(couch_log, error, fun(_, _) -> ok end),
               ok
       end,
       fun (_) -> meck:unload([couch_log]) end,
       [
        ?_assertEqual(2, mem3_util:n_val(2,4)),
        ?_assertEqual(1, mem3_util:n_val(-1,4)),
        ?_assertEqual(4, mem3_util:n_val(6,4))
        ]
       }
     ]
    }.


config_01_setup() ->
    Ini = filename:join([code:lib_dir(mem3, test), "01-config-default.ini"]),
    {ok, Pid} = config:start_link([Ini]),
    Pid.

config_teardown(_Pid) ->
    config:stop().


n_val_test_() ->
    {"n_val tests with config",
     [
      {setup,
       fun config_01_setup/0,
       fun config_teardown/1,
       fun(Pid) ->
           {with, Pid, [
               fun n_val_1/1
            ]}
       end}
     ]
    }.

n_val_1(_Pid) ->
    ?assertEqual(3, mem3_util:n_val(undefined, 4)).
