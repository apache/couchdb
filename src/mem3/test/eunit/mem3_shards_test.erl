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

-module(mem3_shards_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/src/mem3_reshard.hrl").
% for all_docs function
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(ID, <<"_id">>).
-define(TIMEOUT, 60).

setup() ->
    DbName = ?tempdb(),
    PartProps = [{partitioned, true}, {hash, [couch_partition, hash, []]}],
    create_db(DbName, [{q, 8}, {n, 1}, {props, PartProps}]),
    {ok, DbDoc} = mem3_util:open_db_doc(DbName),
    #{dbname => DbName, dbdoc => DbDoc}.

teardown(#{dbname := DbName}) ->
    delete_db(DbName).

start_couch() ->
    test_util:start_couch(?CONFIG_CHAIN, [mem3, fabric]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).

mem3_shards_db_create_props_test_() ->
    {
        "mem3 shards partition query database properties tests",
        {
            setup,
            fun start_couch/0,
            fun stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(partitioned_shards_recreated_properly, ?TIMEOUT),
                    ?TDEF_FE(update_props, ?TIMEOUT)
                ]
            }
        }
    }.

% This asserts that when the mem3_shards's changes listener on the shards db
% encounters a db doc update for a db that has a missing shard on the local
% instance, the shard creation logic will properly propagate the db's config
% properties.
% SEE: apache/couchdb#3631
partitioned_shards_recreated_properly(#{dbname := DbName, dbdoc := DbDoc}) ->
    #doc{body = {Body0}} = DbDoc,
    Body1 = [{<<"foo">>, <<"bar">>} | Body0],
    Shards = [Shard | _] = lists:sort(mem3:shards(DbName)),
    ShardName = Shard#shard.name,
    ?assert(is_partitioned(Shards)),
    ok = with_proc(fun() -> couch_server:delete(ShardName, []) end),
    ?assertThrow({not_found, no_db_file}, is_partitioned(Shard)),
    ok = mem3_util:update_db_doc(DbDoc#doc{body = {Body1}}),
    Shards =
        [Shard | _] = test_util:wait_value(
            fun() ->
                lists:sort(mem3:shards(DbName))
            end,
            Shards
        ),
    ?assertEqual(
        true,
        test_util:wait_value(
            fun() ->
                catch is_partitioned(Shard)
            end,
            true
        )
    ).

update_props(#{dbname := DbName, dbdoc := DbDoc}) ->
    {ok, Doc} = mem3:get_db_doc(DbName),
    ?assertEqual(DbDoc, Doc),
    #doc{body = {Body0}} = Doc,
    {Props} = couch_util:get_value(<<"props">>, Body0, {[]}),
    Props1 = couch_util:set_value(<<"baz">>, Props, <<"bar">>),
    Body1 = couch_util:set_value(<<"props">>, Body0, {Props1}),
    ResUpdate = mem3:update_db_doc(Doc#doc{body = {Body1}}),
    ?assertMatch({ok, _}, ResUpdate),
    {ok, Doc2} = mem3:get_db_doc(DbName),
    #doc{body = {Body2}} = Doc2,
    {Props2} = couch_util:get_value(<<"props">>, Body2, {[]}),
    ?assertEqual(<<"bar">>, couch_util:get_value(<<"baz">>, Props2)),
    ?assertEqual({error, conflict}, mem3:update_db_doc(Doc#doc{body = {Body1}})).

is_partitioned([#shard{} | _] = Shards) ->
    lists:all(fun is_partitioned/1, Shards);
is_partitioned(#shard{name = Name}) ->
    couch_util:with_db(Name, fun couch_db:is_partitioned/1);
is_partitioned(Db) ->
    couch_db:is_partitioned(Db).

create_db(DbName, Opts) ->
    GL = group_leader(),
    with_proc(fun() -> fabric:create_db(DbName, Opts) end, GL).

delete_db(DbName) ->
    GL = group_leader(),
    with_proc(fun() -> fabric:delete_db(DbName, [?ADMIN_CTX]) end, GL).

with_proc(Fun) ->
    with_proc(Fun, undefined, 30000).

with_proc(Fun, GroupLeader) ->
    with_proc(Fun, GroupLeader, 30000).

with_proc(Fun, GroupLeader, Timeout) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        case GroupLeader of
            undefined -> ok;
            _ -> group_leader(GroupLeader, self())
        end,
        exit({with_proc_res, Fun()})
    end),
    receive
        {'DOWN', Ref, process, Pid, {with_proc_res, Res}} ->
            Res;
        {'DOWN', Ref, process, Pid, Error} ->
            error(Error)
    after Timeout ->
        demonitor(Ref, [flush]),
        exit(Pid, kill),
        error({with_proc_timeout, Fun, Timeout})
    end.
