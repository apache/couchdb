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
-include_lib("couch_replicator/src/couch_replicator.hrl").

-define(USERNAME, "rep_admin").
-define(PASSWORD, "secret").

setup() ->
    Ctx = test_util:start_couch([fabric, mem3, couch_replicator, chttpd]),
    Hashed = couch_passwords:hash_admin_password(?PASSWORD),
    ok = config:set("admins", ?USERNAME, ?b2l(Hashed), _Persist = false),
    Source = ?tempdb(),
    Target = ?tempdb(),
    {Ctx, {Source, Target}}.

teardown({Ctx, {_Source, _Target}}) ->
    config:delete("admins", ?USERNAME),
    ok = test_util:stop_couch(Ctx).

create_target_with_options_replication_test_() ->
    {
        "Create target with range partitions tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun should_create_target_with_q_4/1,
                fun should_create_target_with_q_2_n_1/1,
                fun should_create_target_with_default/1,
                fun should_not_create_target_with_q_any/1
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
    create_db(Source),
    create_doc(Source),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),

    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    delete_db(Source),
    delete_db(Target),
    ?_assertEqual(4, couch_util:get_value(q, ClusterInfo)).

should_create_target_with_q_2_n_1({_Ctx, {Source, Target}}) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"create_target">>, true},
            {<<"create_target_params">>, {[{<<"q">>, <<"2">>}, {<<"n">>, <<"1">>}]}}
        ]},
    create_db(Source),
    create_doc(Source),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),

    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    delete_db(Source),
    delete_db(Target),
    [
        ?_assertEqual(2, couch_util:get_value(q, ClusterInfo)),
        ?_assertEqual(1, couch_util:get_value(n, ClusterInfo))
    ].

should_create_target_with_default({_Ctx, {Source, Target}}) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"create_target">>, true}
        ]},
    create_db(Source),
    create_doc(Source),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),

    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    Q = config:get_integer("cluster", "q", 2),
    delete_db(Source),
    delete_db(Target),
    ?_assertEqual(Q, couch_util:get_value(q, ClusterInfo)).

should_not_create_target_with_q_any({_Ctx, {Source, Target}}) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"create_target">>, false},
            {<<"create_target_params">>, {[{<<"q">>, <<"1">>}]}}
        ]},
    create_db(Source),
    create_doc(Source),
    {error, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    DbExist = is_list(catch mem3:shards(Target)),
    delete_db(Source),
    ?_assertEqual(false, DbExist).

create_doc(DbName) ->
    Body = {[{<<"foo">>, <<"bar">>}]},
    NewDoc = #doc{body = Body},
    {ok, _} = fabric:update_doc(DbName, NewDoc, [?ADMIN_CTX]).

create_db(DbName) ->
    ok = fabric:create_db(DbName, [?ADMIN_CTX]).

delete_db(DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).

db_url(DbName) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    ?l2b(
        io_lib:format("http://~s:~s@~s:~b/~s", [
            ?USERNAME,
            ?PASSWORD,
            Addr,
            Port,
            DbName
        ])
    ).
