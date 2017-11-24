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


setup(_) ->
    Ctx1 = test_util:start_couch([fabric, mem3, couch_replicator]),
    Ctx2 = chttpd_test_util:start_couch(),
    Source = ?tempdb(),
    Target = ?tempdb(),
    {Ctx1, Ctx2, {Source, Target}}.


teardown(_, {Ctx1, Ctx2, {_Source, _Target}}) ->
    ok = test_util:stop_couch(Ctx1),
    ok = chttpd_test_util:stop_couch(Ctx2).


create_target_with_options_replication_test_() ->
    Ps = [{local, remote}, {remote, remote}],
    {
        "Create target with range partitions tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{P, fun should_create_target_with_q_4/2} || P <- Ps] ++
            [{P, fun should_create_target_with_q_2_n_1/2} || P <- Ps] ++
            [{P, fun should_create_target_with_default/2} || P <- Ps] ++
            [{P, fun should_not_create_target_with_q_any/2} || P <- Ps]
        }
    }.


should_create_target_with_q_4({From, To}, {_Ctx1, _Ctx2, {Source, Target}}) ->
    RepObject = {[
        {<<"source">>, db_url(From, Source)},
        {<<"target">>, db_url(To, Target)},
        {<<"create_target">>, true},
        {<<"create_target_params">>, {[{<<"q">>, <<"4">>}]}}
    ]},
    create_db(From, Source),
    create_doc(From, Source),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),

    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    delete_db(From, Source),
    delete_db(To, Target),
    ?_assertEqual(4, couch_util:get_value(q, ClusterInfo)).


should_create_target_with_q_2_n_1(
    {From, To}, {_Ctx1, _Ctx2, {Source, Target}}) ->
    RepObject = {[
        {<<"source">>, db_url(From, Source)},
        {<<"target">>, db_url(To, Target)},
        {<<"create_target">>, true},
        {<<"create_target_params">>,
            {[{<<"q">>, <<"2">>}, {<<"n">>, <<"1">>}]}}
    ]},
    create_db(From, Source),
    create_doc(From, Source),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),

    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    delete_db(From, Source),
    delete_db(To, Target),
    [
        ?_assertEqual(2, couch_util:get_value(q, ClusterInfo)),
        ?_assertEqual(1, couch_util:get_value(n, ClusterInfo))
    ].


should_create_target_with_default(
    {From, To}, {_Ctx1, _Ctx2, {Source, Target}}) ->
    RepObject = {[
        {<<"source">>, db_url(From, Source)},
        {<<"target">>, db_url(To, Target)},
        {<<"create_target">>, true}
    ]},
    create_db(From, Source),
    create_doc(From, Source),
    {ok, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),

    {ok, TargetInfo} = fabric:get_db_info(Target),
    {ClusterInfo} = couch_util:get_value(cluster, TargetInfo),
    Q = config:get("cluster", "q", "8"),
    delete_db(From, Source),
    delete_db(To, Target),
    ?_assertEqual(list_to_integer(Q), couch_util:get_value(q, ClusterInfo)).


should_not_create_target_with_q_any(
    {From, To}, {_Ctx1, _Ctx2, {Source, Target}}) ->
    RepObject = {[
        {<<"source">>, db_url(From, Source)},
        {<<"target">>, db_url(To, Target)},
        {<<"create_target">>, false},
        {<<"create_target_params">>, {[{<<"q">>, <<"1">>}]}}
    ]},
    create_db(From, Source),
    create_doc(From, Source),
    {error, _} = couch_replicator:replicate(RepObject, ?ADMIN_USER),
    DbExist = is_list(catch mem3:shards(Target)),
    delete_db(From, Source),
    ?_assertEqual(false, DbExist).


create_doc(local, DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    Body = {[{<<"foo">>, <<"bar">>}]},
    NewDoc = #doc{body = Body},
    {ok, _} = couch_db:update_doc(Db, NewDoc, []),
    couch_db:close(Db);
create_doc(remote, DbName) ->
    Body = {[{<<"foo">>, <<"bar">>}]},
    NewDoc = #doc{body = Body},
    {ok, _} = fabric:update_doc(DbName, NewDoc, [?ADMIN_CTX]).


create_db(local, DbName) ->
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db);
create_db(remote, DbName) ->
    ok = fabric:create_db(DbName, [?ADMIN_CTX]).


delete_db(local, DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]);
delete_db(remote, DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).


db_url(local, DbName) ->
    DbName;
db_url(remote, DbName) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    ?l2b(io_lib:format("http://~s:~b/~s", [Addr, Port, DbName])).
