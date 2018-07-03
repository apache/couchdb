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

-module(test_engine_get_set_props).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").


cet_default_props() ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    {ok, St} = Engine:init(DbPath, [
            create,
            {default_security_object, dso}
        ]),

    Node = node(),

    ?assertEqual(0, Engine:get_doc_count(St)),
    ?assertEqual(0, Engine:get_del_doc_count(St)),
    ?assertEqual(true, is_list(Engine:get_size_info(St))),
    ?assertEqual(true, is_integer(Engine:get_disk_version(St))),
    ?assertEqual(0, Engine:get_update_seq(St)),
    ?assertEqual(0, Engine:get_purge_seq(St)),
    ?assertEqual([], Engine:get_last_purged(St)),
    ?assertEqual(dso, Engine:get_security(St)),
    ?assertEqual(1000, Engine:get_revs_limit(St)),
    ?assertMatch(<<_:32/binary>>, Engine:get_uuid(St)),
    ?assertEqual([{Node, 0}], Engine:get_epochs(St)),
    ?assertEqual(0, Engine:get_compacted_seq(St)).


cet_set_security() ->
    check_prop_set(get_security, set_security, dso, [{<<"readers">>, []}]).


cet_set_revs_limit() ->
    check_prop_set(get_revs_limit, set_revs_limit, 1000, 50).


cet_set_props_at_init() ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    {ok, St} = Engine:init(DbPath, [
            create,
            {default_security_object, dso},
            {default_props, [{shardkey, true}]}
        ]),
    
    ?assertEqual({ok, true}, Engine:get_prop(St, shardkey)).


cet_set_prop() ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    {ok, St0} = Engine:init(DbPath, [
            create,
            {default_security_object, dso}
        ]),
    ?assertEqual({error, no_value}, Engine:get_prop(St0, shardkey)),

    ?assertEqual({ok, false}, Engine:get_prop(St0, shardkey, false)),

    {ok, St1} = Engine:set_prop(St0, shardkey, true),
    ?assertEqual({ok, true}, Engine:get_prop(St1, shardkey)),

    {ok, St2} = Engine:commit_data(St1),
    Engine:terminate(normal, St2),

    {ok, St3} = Engine:init(DbPath, []),
    ?assertEqual({ok, true}, Engine:get_prop(St3, shardkey)).


check_prop_set(GetFun, SetFun, Default, Value) ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    {ok, St0} = Engine:init(DbPath, [
            create,
            {default_security_object, dso}
        ]),
    ?assertEqual(Default, Engine:GetFun(St0)),

    {ok, St1} = Engine:SetFun(St0, Value),
    ?assertEqual(Value, Engine:GetFun(St1)),

    {ok, St2} = Engine:commit_data(St1),
    Engine:terminate(normal, St2),

    {ok, St3} = Engine:init(DbPath, []),
    ?assertEqual(Value, Engine:GetFun(St3)).
