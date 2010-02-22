-module(mem_utils_test).

-include_lib("eunit/include/eunit.hrl").


join_type_test() ->
  Options = [{replace,node3}],
  ?assertEqual({replace,node3}, mem_utils:join_type(dummy,dummy,Options)).


pmap_from_full_test() ->
  ?assertEqual([{n1,0},{n2,1},{n3,2},{n4,3}],
               mem_utils:pmap_from_full(t_fullmap(0))).


fix_mappings_nodedown_test() ->
  {PMap0, Fullmap0} = mem_utils:fix_mappings(nodedown, n3, t_fullmap(0)),
  % with n3 down, n1 takes over
  ?assertEqual([{n1,0},{n2,1},{n1,2},{n4,3}], PMap0),
  ?assertEqual(t_fullmap(1), lists:sort(Fullmap0)).


fix_mappings_rejoin_test() ->
  {PMap0, Fullmap0} = mem_utils:fix_mappings(nodedown, n3, t_fullmap(0)),
  % with n3 down, n1 takes over
  ?assertEqual([{n1,0},{n2,1},{n1,2},{n4,3}], PMap0),
  ?assertEqual(t_fullmap(1), lists:sort(Fullmap0)),
  % now have n3 rejoin
  {PMap1, Fullmap1} = mem_utils:fix_mappings(rejoin, n3, Fullmap0),
  ?assertEqual([{n1,0},{n2,1},{n3,2},{n4,3}], PMap1),
  ?assertEqual(lists:sort(t_fullmap(0)), lists:sort(Fullmap1)).


fix_mappings_replace_test() ->
  {PMap0, Fullmap0} = mem_utils:fix_mappings(nodedown, n3, t_fullmap(0)),
  % with n3 down, n1 takes over
  ?assertEqual([{n1,0},{n2,1},{n1,2},{n4,3}], PMap0),
  ?assertEqual(t_fullmap(1), lists:sort(Fullmap0)),
  % now replace n3 with n5
  {PMap2, Fullmap2} = mem_utils:fix_mappings(replace, {n3,n5}, Fullmap0),
  ?assertEqual([{n1,0},{n2,1},{n5,2},{n4,3}], PMap2),
  ?assertEqual(lists:sort(t_fullmap(2)), lists:sort(Fullmap2)).


fix_mappings_already_down_test() ->
  {_PMap0, Fullmap0} = mem_utils:fix_mappings(nodedown, n3, t_fullmap(1)),
  ?assertEqual(t_fullmap(1), lists:sort(Fullmap0)).


was_i_nodedown_test() ->
  ?assertEqual(true, mem_utils:was_i_nodedown(n3, t_fullmap(1))),
  ?assertEqual(false, mem_utils:was_i_nodedown(n3, t_fullmap(0))).


%% test helper funs

t_fullmap(0) ->  % four node, four part fullmap (unsorted)
  [{n1,0,primary},
   {n2,0,partner},
   {n3,0,partner},
   {n2,1,primary},
   {n3,1,partner},
   {n4,1,partner},
   {n3,2,primary},
   {n4,2,partner},
   {n1,2,partner},
   {n4,3,primary},
   {n1,3,partner},
   {n2,3,partner}];
t_fullmap(1) -> % like (0) above, but n3 is down (sorted)
  [{n1,0,primary},
   {n1,2,partner},
   {n1,3,partner},
   {n2,0,partner},
   {n2,1,primary},
   {n2,3,partner},
   {n3,0,{nodedown,partner}},
   {n3,1,{nodedown,partner}},
   {n3,2,{nodedown,primary}},
   {n4,1,partner},
   {n4,2,partner},
   {n4,3,primary}];
t_fullmap(2) ->  % like (0) above, but n3 is replaced w/ n5 (unsorted)
  [{n1,0,primary},
   {n2,0,partner},
   {n5,0,partner},
   {n2,1,primary},
   {n5,1,partner},
   {n4,1,partner},
   {n5,2,primary},
   {n4,2,partner},
   {n1,2,partner},
   {n4,3,primary},
   {n1,3,partner},
   {n2,3,partner}];
t_fullmap(_Huh) ->
  huh.
