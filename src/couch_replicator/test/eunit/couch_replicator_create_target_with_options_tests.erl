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

-module(couch_replicator_create_target_with_options_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

create_target_with_options_replication_test_() ->
    {
        "Create target with range partitions tests",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_create_target_with_q_4),
                ?TDEF_FE(should_create_target_with_q_2_n_1),
                ?TDEF_FE(should_create_target_with_default),
                ?TDEF_FE(should_not_create_target_with_q_any)
            ]
        }
    }.

should_create_target_with_q_4({_Ctx, {Source, Target}}) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"create_target">>, true},
            {<<"create_target_params">>, {[{<<"q">>, <<"4">>}]}}
        ]},
    create_doc(Source),
    delete_db(Target),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    ?assertEqual(4, couch_util:get_value(q, ClusterInfo)).

should_create_target_with_q_2_n_1({_Ctx, {Source, Target}}) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"create_target">>, true},
            {<<"create_target_params">>, {[{<<"q">>, <<"2">>}, {<<"n">>, <<"1">>}]}}
        ]},
    create_doc(Source),
    delete_db(Target),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    ?assertEqual(2, couch_util:get_value(q, ClusterInfo)),
    ?assertEqual(1, couch_util:get_value(n, ClusterInfo)).

should_create_target_with_default({_Ctx, {Source, Target}}) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"create_target">>, true}
        ]},
    create_doc(Source),
    delete_db(Target),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    Q = config:get_integer("cluster", "q", 2),
    ?assertEqual(Q, couch_util:get_value(q, ClusterInfo)).

should_not_create_target_with_q_any({_Ctx, {Source, Target}}) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"create_target">>, false},
            {<<"create_target_params">>, {[{<<"q">>, <<"1">>}]}}
        ]},
    create_doc(Source),
    delete_db(Target),
    {error, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    DbExist = is_list(catch mem3:shards(Target)),
    ?assertEqual(false, DbExist).

create_doc(DbName) ->
    Body = {[{<<"foo">>, <<"bar">>}]},
    NewDoc = #doc{body = Body},
    {ok, _} = fabric:update_doc(DbName, NewDoc, [?ADMIN_CTX]).

delete_db(DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).
