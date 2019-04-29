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

-module(fabric2_db_misc_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/1}).


misc_test_() ->
    {
        "Test database miscellaney",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun empty_db_info/1,
                fun accessors/1,
                fun is_system_db/1,
                fun ensure_full_commit/1
            ]}
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    DbName = ?tempdb(),
    {ok, Db} = fabric2_db:create(DbName, []),
    {DbName, Db, Ctx}.


cleanup({_DbName, Db, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


empty_db_info({DbName, Db, _}) ->
    {ok, Info} = fabric2_db:get_db_info(Db),
    ?assertEqual(DbName, fabric2_util:get_value(db_name, Info)),
    ?assertEqual(0, fabric2_util:get_value(doc_count, Info)),
    ?assertEqual(0, fabric2_util:get_value(doc_del_count, Info)),
    ?assert(is_binary(fabric2_util:get_value(update_seq, Info))).


accessors({DbName, Db, _}) ->
    SeqZero = fabric2_util:to_hex(<<0:80>>),
    ?assertEqual(DbName, fabric2_db:name(Db)),
    ?assertEqual(0, fabric2_db:get_instance_start_time(Db)),
    ?assertEqual(nil, fabric2_db:get_pid(Db)),
    ?assertEqual(undefined, fabric2_db:get_before_doc_update_fun(Db)),
    ?assertEqual(undefined, fabric2_db:get_after_doc_read_fun(Db)),
    ?assertEqual(SeqZero, fabric2_db:get_committed_update_seq(Db)),
    ?assertEqual(SeqZero, fabric2_db:get_compacted_seq(Db)),
    ?assertEqual(SeqZero, fabric2_db:get_update_seq(Db)),
    ?assertEqual(nil, fabric2_db:get_compactor_pid(Db)),
    ?assertEqual(1000, fabric2_db:get_revs_limit(Db)),
    ?assertMatch(<<_:32/binary>>, fabric2_db:get_uuid(Db)),
    ?assertEqual(true, fabric2_db:is_db(Db)),
    ?assertEqual(false, fabric2_db:is_db(#{})),
    ?assertEqual(false, fabric2_db:is_partitioned(Db)).


is_system_db({DbName, Db, _}) ->
    ?assertEqual(false, fabric2_db:is_system_db(Db)),
    ?assertEqual(false, fabric2_db:is_system_db_name("foo")),
    ?assertEqual(false, fabric2_db:is_system_db_name(DbName)),
    ?assertEqual(true, fabric2_db:is_system_db_name(<<"_replicator">>)),
    ?assertEqual(true, fabric2_db:is_system_db_name("_replicator")),
    ?assertEqual(true, fabric2_db:is_system_db_name(<<"foo/_replicator">>)),
    ?assertEqual(false, fabric2_db:is_system_db_name(<<"f.o/_replicator">>)),
    ?assertEqual(false, fabric2_db:is_system_db_name(<<"foo/bar">>)).


ensure_full_commit({_, Db, _}) ->
    ?assertEqual({ok, 0}, fabric2_db:ensure_full_commit(Db)),
    ?assertEqual({ok, 0}, fabric2_db:ensure_full_commit(Db, 5)).
