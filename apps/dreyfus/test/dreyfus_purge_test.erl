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

-module(dreyfus_purge_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("dreyfus/include/dreyfus.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("mem3/include/mem3.hrl").

-export([
    test_purge_single/0,
    test_purge_multiple/0,
    test_purge_multiple2/0,
    test_purge_conflict/0,
    test_purge_conflict2/0,
    test_purge_conflict3/0,
    test_purge_conflict4/0,
    test_purge_update/0,
    test_purge_update2/0,
    test_delete/0,
    test_delete_purge_conflict/0,
    test_delete_conflict/0,
    test_all/0
]).
-export([
    test_verify_index_exists1/0,
    test_verify_index_exists2/0,
    test_verify_index_exists_failed/0,
    test_local_doc/0,
    test_delete_local_doc/0,
    test_purge_search/0
]).

-compile(export_all).
-compile(nowarn_export_all).

test_all() ->
    test_purge_single(),
    test_purge_multiple(),
    test_purge_multiple2(),
    test_purge_conflict(),
    test_purge_conflict2(),
    test_purge_conflict3(),
    test_purge_conflict4(),
    test_purge_update(),
    test_purge_update2(),
    test_delete(),
    test_delete_purge_conflict(),
    test_delete_conflict(),
    test_verify_index_exists1(),
    test_verify_index_exists2(),
    test_verify_index_exists_failed(),
    test_delete_local_doc(),
    test_local_doc(),
    test_purge_search(),
    ok.

test_purge_single() ->
    DbName = db_name(),
    create_db_docs(DbName),
    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount1, 1),
    purge_docs(DbName, [<<"apple">>]),
    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount2, 0),
    delete_db(DbName),
    ok.

test_purge_multiple() ->
    Query = <<"color:red">>,

    %create the db and docs
    DbName = db_name(),
    create_db_docs(DbName),

    %first search request
    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, Query),

    ?assertEqual(HitCount1, 5),

    %purge 5 docs
    purge_docs(DbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %second search request
    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, Query),

    ?assertEqual(HitCount2, 0),

    %delete the db
    delete_db(DbName),
    ok.

test_purge_multiple2() ->
    %create the db and docs
    DbName = db_name(),
    create_db_docs(DbName),

    Query = <<"color:red">>,

    %first search request
    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, Query),

    ?assertEqual(HitCount1, 5),

    %purge 2 docs
    purge_docs(DbName, [<<"apple">>, <<"tomato">>]),

    %second search request
    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, Query),

    ?assertEqual(HitCount2, 3),

    %purge 2 docs
    purge_docs(DbName, [<<"cherry">>, <<"haw">>]),

    %third search request
    {ok, _, HitCount3, _, _, _} = dreyfus_search(DbName, Query),

    ?assertEqual(HitCount3, 1),

    %delete the db
    delete_db(DbName),
    ok.

test_purge_conflict() ->
    %create dbs and docs
    SourceDbName = db_name(),
    timer:sleep(2000),
    TargetDbName = db_name(),

    create_db_docs(SourceDbName),
    create_db_docs(TargetDbName, <<"green">>),

    %first search
    {ok, _, RedHitCount1, _RedHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount1, _GreenHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount1 + GreenHitCount1),

    %do replicate and make conflicted docs
    {ok, _} = fabric:update_doc(
        <<"_replicator">>,
        make_replicate_doc(
            SourceDbName, TargetDbName
        ),
        [?ADMIN_CTX]
    ),

    %%check doc version
    wait_for_replicate(
        TargetDbName,
        [
            <<"apple">>,
            <<"tomato">>,
            <<"cherry">>,
            <<"haw">>,
            <<"strawberry">>
        ],
        2,
        5
    ),

    %second search
    {ok, _, RedHitCount2, _RedHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount2, _GreenHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount2 + GreenHitCount2),

    purge_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %third search
    {ok, _, RedHitCount3, _RedHits3, _, _} = dreyfus_search(
        TargetDbName,
        <<"color:red">>
    ),
    {ok, _, GreenHitCount3, _GreenHits3, _, _} = dreyfus_search(
        TargetDbName,
        <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount3 + GreenHitCount3),
    ?assertEqual(RedHitCount2, GreenHitCount3),
    ?assertEqual(GreenHitCount2, RedHitCount3),

    delete_db(SourceDbName),
    delete_db(TargetDbName),
    ok.

test_purge_conflict2() ->
    %create dbs and docs
    SourceDbName = db_name(),
    timer:sleep(2000),
    TargetDbName = db_name(),

    create_db_docs(SourceDbName),
    create_db_docs(TargetDbName, <<"green">>),

    %first search
    {ok, _, RedHitCount1, _RedHits1, _, _} = dreyfus_search(
        TargetDbName,
        <<"color:red">>
    ),
    {ok, _, GreenHitCount1, _GreenHits1, _, _} = dreyfus_search(
        TargetDbName,
        <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount1 + GreenHitCount1),

    %do replicate and make conflicted docs
    {ok, _} = fabric:update_doc(
        <<"_replicator">>,
        make_replicate_doc(
            SourceDbName, TargetDbName
        ),
        [?ADMIN_CTX]
    ),

    wait_for_replicate(
        TargetDbName,
        [
            <<"apple">>,
            <<"tomato">>,
            <<"cherry">>,
            <<"haw">>,
            <<"strawberry">>
        ],
        2,
        5
    ),

    %second search
    {ok, _, RedHitCount2, _RedHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount2, _GreenHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount2 + GreenHitCount2),

    purge_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),
    purge_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %third search
    {ok, _, RedHitCount3, _RedHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount3, _GreenHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(0, RedHitCount3 + GreenHitCount3),

    delete_db(SourceDbName),
    delete_db(TargetDbName),
    ok.

test_purge_conflict3() ->
    %create dbs and docs
    SourceDbName = db_name(),
    timer:sleep(2000),
    TargetDbName = db_name(),

    create_db_docs(SourceDbName),
    create_db_docs(TargetDbName, <<"green">>),

    %first search
    {ok, _, RedHitCount1, _RedHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount1, _GreenHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount1 + GreenHitCount1),

    %do replicate and make conflicted docs
    {ok, _} = fabric:update_doc(
        <<"_replicator">>,
        make_replicate_doc(
            SourceDbName, TargetDbName
        ),
        [?ADMIN_CTX]
    ),

    %%check doc version
    wait_for_replicate(
        TargetDbName,
        [
            <<"apple">>,
            <<"tomato">>,
            <<"cherry">>,
            <<"haw">>,
            <<"strawberry">>
        ],
        2,
        5
    ),

    %second search
    {ok, _, RedHitCount2, _RedHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount2, _GreenHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount2 + GreenHitCount2),

    purge_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %third search
    {ok, _, RedHitCount3, _RedHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount3, _GreenHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount3 + GreenHitCount3),
    ?assertEqual(RedHitCount2, GreenHitCount3),
    ?assertEqual(GreenHitCount2, RedHitCount3),

    purge_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),
    {ok, _, RedHitCount4, _, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount4, _, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(0, RedHitCount4 + GreenHitCount4),

    delete_db(SourceDbName),
    delete_db(TargetDbName),
    ok.

test_purge_conflict4() ->
    %create dbs and docs
    SourceDbName = db_name(),
    timer:sleep(2000),
    TargetDbName = db_name(),

    create_db_docs(SourceDbName, <<"green">>),
    create_db_docs(TargetDbName, <<"red">>),

    %first search
    {ok, _, RedHitCount1, _RedHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount1, _GreenHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount1 + GreenHitCount1),

    %do replicate and make conflicted docs
    {ok, _} = fabric:update_doc(
        <<"_replicator">>,
        make_replicate_doc(
            SourceDbName, TargetDbName
        ),
        [?ADMIN_CTX]
    ),

    %%check doc version
    wait_for_replicate(
        TargetDbName,
        [
            <<"apple">>,
            <<"tomato">>,
            <<"cherry">>,
            <<"haw">>,
            <<"strawberry">>
        ],
        2,
        5
    ),

    %second search
    {ok, _, RedHitCount2, _RedHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount2, _GreenHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount2 + GreenHitCount2),

    purge_docs_with_all_revs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %third search
    {ok, _, RedHitCount3, _RedHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount3, _GreenHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(0, RedHitCount3 + GreenHitCount3),

    delete_db(SourceDbName),
    delete_db(TargetDbName),
    ok.

test_purge_update() ->
    %create the db and docs
    DbName = db_name(),
    create_db_docs(DbName),

    QueryRed = <<"color:red">>,
    QueryGreen = <<"color:green">>,

    %first search request
    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, QueryRed),

    ?assertEqual(HitCount1, 5),

    %update doc
    Rev = get_rev(DbName, <<"apple">>),
    Doc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"apple">>},
            {<<"_rev">>, couch_doc:rev_to_str(Rev)},
            {<<"color">>, <<"green">>},
            {<<"size">>, 8}
        ]}
    ),
    {ok, _} = fabric:update_docs(DbName, [Doc], [?ADMIN_CTX]),

    %second search request
    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, QueryRed),
    {ok, _, HitCount3, _, _, _} = dreyfus_search(DbName, QueryGreen),

    % 4 red and 1 green
    ?assertEqual(HitCount2, 4),
    ?assertEqual(HitCount3, 1),

    % purge 2 docs, 1 red and 1 green
    purge_docs(DbName, [<<"apple">>, <<"tomato">>]),

    % third search request
    {ok, _, HitCount4, _, _, _} = dreyfus_search(DbName, QueryRed),
    {ok, _, HitCount5, _, _, _} = dreyfus_search(DbName, QueryGreen),

    % 3 red and 0 green
    ?assertEqual(HitCount4, 3),
    ?assertEqual(HitCount5, 0),

    delete_db(DbName),
    ok.

test_purge_update2() ->
    %create the db and docs
    DbName = db_name(),
    create_db_docs(DbName),

    Query1 = <<"size:1">>,
    Query1000 = <<"size:1000">>,

    %first search request
    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, Query1),
    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, Query1000),

    ?assertEqual(HitCount1, 5),
    ?assertEqual(HitCount2, 0),

    %update doc 999 times, it will take about 30 seconds.
    update_doc(DbName, <<"apple">>, 999),

    %second search request
    {ok, _, HitCount3, _, _, _} = dreyfus_search(DbName, Query1),
    {ok, _, HitCount4, _, _, _} = dreyfus_search(DbName, Query1000),

    % 4 value(1) and 1 value(1000)
    ?assertEqual(HitCount3, 4),
    ?assertEqual(HitCount4, 1),

    % purge doc
    purge_docs(DbName, [<<"apple">>]),

    % third search request
    {ok, _, HitCount5, _, _, _} = dreyfus_search(DbName, Query1),
    {ok, _, HitCount6, _, _, _} = dreyfus_search(DbName, Query1000),

    % 4 value(1) and 0 value(1000)
    ?assertEqual(HitCount5, 4),
    ?assertEqual(HitCount6, 0),

    delete_db(DbName),
    ok.

test_delete() ->
    DbName = db_name(),
    create_db_docs(DbName),
    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount1, 1),
    ok = delete_docs(DbName, [<<"apple">>]),
    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount2, 0),
    delete_db(DbName),
    ok.

test_delete_conflict() ->
    %create dbs and docs
    SourceDbName = db_name(),
    timer:sleep(2000),
    TargetDbName = db_name(),

    create_db_docs(SourceDbName),
    create_db_docs(TargetDbName, <<"green">>),

    %first search
    {ok, _, RedHitCount1, _RedHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount1, _GreenHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount1 + GreenHitCount1),

    %do replicate and make conflicted docs
    {ok, _} = fabric:update_doc(
        <<"_replicator">>,
        make_replicate_doc(
            SourceDbName, TargetDbName
        ),
        [?ADMIN_CTX]
    ),

    wait_for_replicate(
        TargetDbName,
        [
            <<"apple">>,
            <<"tomato">>,
            <<"cherry">>,
            <<"haw">>,
            <<"strawberry">>
        ],
        2,
        5
    ),

    %second search
    {ok, _, RedHitCount2, _RedHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount2, _GreenHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount2 + GreenHitCount2),

    %delete docs
    delete_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %third search
    {ok, _, RedHitCount3, _RedHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount3, _GreenHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount3 + GreenHitCount3),
    ?assertEqual(RedHitCount2, GreenHitCount3),
    ?assertEqual(GreenHitCount2, RedHitCount3),

    delete_db(SourceDbName),
    delete_db(TargetDbName),
    ok.

test_delete_purge_conflict() ->
    %create dbs and docs
    SourceDbName = db_name(),
    timer:sleep(2000),
    TargetDbName = db_name(),

    create_db_docs(SourceDbName),
    create_db_docs(TargetDbName, <<"green">>),

    %first search
    {ok, _, RedHitCount1, _RedHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount1, _GreenHits1, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount1 + GreenHitCount1),

    %do replicate and make conflicted docs
    {ok, _} = fabric:update_doc(
        <<"_replicator">>,
        make_replicate_doc(
            SourceDbName, TargetDbName
        ),
        [?ADMIN_CTX]
    ),

    wait_for_replicate(
        TargetDbName,
        [
            <<"apple">>,
            <<"tomato">>,
            <<"cherry">>,
            <<"haw">>,
            <<"strawberry">>
        ],
        2,
        5
    ),

    %second search
    {ok, _, RedHitCount2, _RedHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount2, _GreenHits2, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(5, RedHitCount2 + GreenHitCount2),

    %purge docs
    purge_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %delete docs
    delete_docs(TargetDbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"haw">>,
        <<"strawberry">>
    ]),

    %third search
    {ok, _, RedHitCount3, _RedHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:red">>
    ),
    {ok, _, GreenHitCount3, _GreenHits3, _, _} = dreyfus_search(
        TargetDbName, <<"color:green">>
    ),

    ?assertEqual(RedHitCount3, 0),
    ?assertEqual(GreenHitCount3, 0),
    ?assertEqual(GreenHitCount3, 0),
    ?assertEqual(RedHitCount3, 0),

    delete_db(SourceDbName),
    delete_db(TargetDbName),
    ok.

test_local_doc() ->
    DbName = db_name(),
    create_db_docs(DbName),

    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount1, 1),
    purge_docs(DbName, [
        <<"apple">>,
        <<"tomato">>,
        <<"cherry">>,
        <<"strawberry">>
    ]),
    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount2, 0),

    %get local doc
    [Sig | _] = get_sigs(DbName),
    LocalId = dreyfus_util:get_local_purge_doc_id(Sig),
    LocalShards = mem3:local_shards(DbName),
    PurgeSeqs = lists:map(
        fun(Shard) ->
            {ok, Db} = couch_db:open_int(Shard#shard.name, [?ADMIN_CTX]),
            {ok, LDoc} = couch_db:open_doc(Db, LocalId, []),
            {Props} = couch_doc:to_json_obj(LDoc, []),
            dreyfus_util:get_value_from_options(<<"updated_on">>, Props),
            PurgeSeq = dreyfus_util:get_value_from_options(<<"purge_seq">>, Props),
            Type = dreyfus_util:get_value_from_options(<<"type">>, Props),
            ?assertEqual(<<"dreyfus">>, Type),
            couch_db:close(Db),
            PurgeSeq
        end,
        LocalShards
    ),
    ?assertEqual(lists:sum(PurgeSeqs), 4),

    delete_db(DbName),
    ok.

test_verify_index_exists1() ->
    DbName = db_name(),
    create_db_docs(DbName),

    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount1, 1),

    ok = purge_docs(DbName, [<<"apple">>]),

    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount2, 0),

    ShardNames = [Sh || #shard{name = Sh} <- mem3:local_shards(DbName)],
    [ShardDbName | _Rest] = ShardNames,
    {ok, Db} = couch_db:open(ShardDbName, [?ADMIN_CTX]),
    {ok, LDoc} = couch_db:open_doc(
        Db,
        dreyfus_util:get_local_purge_doc_id(
            <<"49e82c2a910b1046b55cc45ad058a7ee">>
        ),
        []
    ),
    #doc{body = {Props}} = LDoc,
    ?assertEqual(true, dreyfus_util:verify_index_exists(ShardDbName, Props)),
    delete_db(DbName),
    ok.

test_verify_index_exists2() ->
    DbName = db_name(),
    create_db_docs(DbName),

    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount1, 1),

    ShardNames = [Sh || #shard{name = Sh} <- mem3:local_shards(DbName)],
    [ShardDbName | _Rest] = ShardNames,
    {ok, Db} = couch_db:open(ShardDbName, [?ADMIN_CTX]),
    {ok, LDoc} = couch_db:open_doc(
        Db,
        dreyfus_util:get_local_purge_doc_id(
            <<"49e82c2a910b1046b55cc45ad058a7ee">>
        ),
        []
    ),
    #doc{body = {Props}} = LDoc,
    ?assertEqual(true, dreyfus_util:verify_index_exists(ShardDbName, Props)),

    delete_db(DbName),
    ok.

test_verify_index_exists_failed() ->
    DbName = db_name(),
    create_db_docs(DbName),

    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount1, 1),

    ShardNames = [Sh || #shard{name = Sh} <- mem3:local_shards(DbName)],
    [ShardDbName | _Rest] = ShardNames,
    {ok, Db} = couch_db:open(ShardDbName, [?ADMIN_CTX]),
    {ok, LDoc} = couch_db:open_doc(
        Db,
        dreyfus_util:get_local_purge_doc_id(
            <<"49e82c2a910b1046b55cc45ad058a7ee">>
        ),
        []
    ),
    #doc{body = {Options}} = LDoc,
    OptionsDbErr = [
        {<<"indexname">>, dreyfus_util:get_value_from_options(<<"indexname">>, Options)},
        {<<"ddoc_id">>, dreyfus_util:get_value_from_options(<<"ddoc_id">>, Options)},
        {<<"signature">>, dreyfus_util:get_value_from_options(<<"signature">>, Options)}
    ],
    ?assertEqual(
        false,
        dreyfus_util:verify_index_exists(
            ShardDbName, OptionsDbErr
        )
    ),

    OptionsIdxErr = [
        {<<"indexname">>, <<"someindex">>},
        {<<"ddoc_id">>, dreyfus_util:get_value_from_options(<<"ddoc_id">>, Options)},
        {<<"signature">>, dreyfus_util:get_value_from_options(<<"signature">>, Options)}
    ],
    ?assertEqual(
        false,
        dreyfus_util:verify_index_exists(
            ShardDbName, OptionsIdxErr
        )
    ),

    OptionsDDocErr = [
        {<<"indexname">>, dreyfus_util:get_value_from_options(<<"indexname">>, Options)},
        {<<"ddoc_id">>, <<"somedesigndoc">>},
        {<<"signature">>, dreyfus_util:get_value_from_options(<<"signature">>, Options)}
    ],
    ?assertEqual(
        false,
        dreyfus_util:verify_index_exists(
            ShardDbName, OptionsDDocErr
        )
    ),

    OptionsSigErr = [
        {<<"indexname">>, dreyfus_util:get_value_from_options(<<"indexname">>, Options)},
        {<<"ddoc_id">>, dreyfus_util:get_value_from_options(<<"ddoc_id">>, Options)},
        {<<"signature">>, <<"12345678901234567890123456789012">>}
    ],
    ?assertEqual(
        false,
        dreyfus_util:verify_index_exists(
            ShardDbName, OptionsSigErr
        )
    ),

    delete_db(DbName),
    ok.

test_delete_local_doc() ->
    DbName = db_name(),
    create_db_docs(DbName),

    {ok, _, HitCount1, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount1, 1),

    ok = purge_docs(DbName, [<<"apple">>]),

    {ok, _, HitCount2, _, _, _} = dreyfus_search(DbName, <<"apple">>),
    ?assertEqual(HitCount2, 0),

    LDocId = dreyfus_util:get_local_purge_doc_id(
        <<"49e82c2a910b1046b55cc45ad058a7ee">>
    ),
    ShardNames = [Sh || #shard{name = Sh} <- mem3:local_shards(DbName)],
    [ShardDbName | _Rest] = ShardNames,
    {ok, Db} = couch_db:open(ShardDbName, [?ADMIN_CTX]),
    {ok, _} = couch_db:open_doc(Db, LDocId, []),

    delete_docs(DbName, [<<"_design/search">>]),
    io:format("DbName ~p~n", [DbName]),
    ?debugFmt("Converting ... ~n~p~n", [DbName]),

    dreyfus_fabric_cleanup:go(DbName),
    {ok, Db2} = couch_db:open(ShardDbName, [?ADMIN_CTX]),
    {not_found, _} = couch_db:open_doc(Db2, LDocId, []),

    delete_db(DbName),
    ok.

test_purge_search() ->
    DbName = db_name(),
    create_db_docs(DbName),
    purge_docs(DbName, [<<"apple">>, <<"tomato">>, <<"haw">>]),
    {ok, _, HitCount, _, _, _} = dreyfus_search(DbName, <<"color:red">>),
    ?assertEqual(HitCount, 2),
    delete_db(DbName),
    ok.

%private API
db_name() ->
    iolist_to_binary([
        "dreyfus-test-db-",
        [
            integer_to_list(I)
         || I <- [
                erlang:unique_integer([positive]),
                rand:uniform(10000)
            ]
        ]
    ]).

purge_docs(DBName, DocIds) ->
    IdsRevs = [{DocId, [get_rev(DBName, DocId)]} || DocId <- DocIds],
    {ok, _} = fabric:purge_docs(DBName, IdsRevs, []),
    ok.

purge_docs_with_all_revs(DBName, DocIds) ->
    IdsRevs = [{DocId, get_revs(DBName, DocId)} || DocId <- DocIds],
    {ok, _} = fabric:purge_docs(DBName, IdsRevs, []),
    ok.

dreyfus_search(DbName, KeyWord) ->
    QueryArgs = #index_query_args{q = KeyWord},
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/search">>, []),
    dreyfus_fabric_search:go(DbName, DDoc, <<"index">>, QueryArgs).

create_db_docs(DbName) ->
    create_db(DbName),
    create_docs(DbName, 5, <<"red">>).

create_db_docs(DbName, Color) ->
    create_db(DbName),
    create_docs(DbName, 5, Color).

create_docs(DbName, Count, Color) ->
    {ok, _} = fabric:update_docs(DbName, make_docs(Count, Color), [?ADMIN_CTX]),
    {ok, _} = fabric:update_doc(DbName, make_design_doc(dreyfus), [?ADMIN_CTX]).

create_db(DbName) ->
    ok = fabric:create_db(DbName, [?ADMIN_CTX, {q, 1}]).

delete_db(DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).

make_docs(Count, Color) ->
    [make_doc(I, Color) || I <- lists:seq(1, Count)].

make_doc(Id, Color) ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, get_value(Id)},
            {<<"color">>, Color},
            {<<"size">>, 1}
        ]}
    ).

get_value(Key) ->
    case Key of
        1 -> <<"apple">>;
        2 -> <<"tomato">>;
        3 -> <<"cherry">>;
        4 -> <<"strawberry">>;
        5 -> <<"haw">>;
        6 -> <<"carrot">>;
        7 -> <<"pitaya">>;
        8 -> <<"grape">>;
        9 -> <<"date">>;
        10 -> <<"watermelon">>
    end.

make_design_doc(dreyfus) ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/search">>},
            {<<"language">>, <<"javascript">>},
            {<<"indexes">>,
                {[
                    {<<"index">>,
                        {[
                            {<<"analyzer">>, <<"standard">>},
                            {<<"index">>, <<
                                "function (doc) { \n"
                                "  index(\"default\", doc._id);\n"
                                "  if(doc.color) {\n"
                                "    index(\"color\", doc.color);\n"
                                "  }\n"
                                "  if(doc.size) {\n"
                                "    index(\"size\", doc.size);\n"
                                "  }\n"
                                "}"
                            >>}
                        ]}}
                ]}}
        ]}
    ).

make_replicate_doc(SourceDbName, TargetDbName) ->
    couch_doc:from_json_obj(
        {[
            {<<"_id">>,
                list_to_binary(
                    "replicate_fm_" ++
                        binary_to_list(SourceDbName) ++ "_to_" ++ binary_to_list(TargetDbName)
                )},
            {<<"source">>, list_to_binary("http://localhost:15984/" ++ SourceDbName)},
            {<<"target">>, list_to_binary("http://localhost:15984/" ++ TargetDbName)}
        ]}
    ).

get_rev(DbName, DocId) ->
    FDI = fabric:get_full_doc_info(DbName, DocId, []),
    #doc_info{revs = [#rev_info{} = PrevRev | _]} = couch_doc:to_doc_info(FDI),
    PrevRev#rev_info.rev.

get_revs(DbName, DocId) ->
    FDI = fabric:get_full_doc_info(DbName, DocId, []),
    #doc_info{revs = Revs} = couch_doc:to_doc_info(FDI),
    [Rev#rev_info.rev || Rev <- Revs].

update_doc(_, _, 0) ->
    ok;
update_doc(DbName, DocId, Times) ->
    Rev = get_rev(DbName, DocId),
    Doc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"apple">>},
            {<<"_rev">>, couch_doc:rev_to_str(Rev)},
            {<<"size">>, 1001 - Times}
        ]}
    ),
    {ok, _} = fabric:update_docs(DbName, [Doc], [?ADMIN_CTX]),
    update_doc(DbName, DocId, Times - 1).

delete_docs(DbName, DocIds) ->
    lists:foreach(
        fun(DocId) -> ok = delete_doc(DbName, DocId) end,
        DocIds
    ).

delete_doc(DbName, DocId) ->
    Rev = get_rev(DbName, DocId),
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DocId},
            {<<"_rev">>, couch_doc:rev_to_str(Rev)},
            {<<"_deleted">>, true}
        ]}
    ),
    {ok, _} = fabric:update_doc(DbName, DDoc, [?ADMIN_CTX]),
    ok.

wait_for_replicate(_, _, _, 0) ->
    couch_log:notice("[~p] wait time out", [?MODULE]),
    ok;
wait_for_replicate(DbName, DocIds, ExpectRevCount, TimeOut) when
    is_list(DocIds)
->
    [wait_for_replicate(DbName, DocId, ExpectRevCount, TimeOut) || DocId <- DocIds];
wait_for_replicate(DbName, DocId, ExpectRevCount, TimeOut) ->
    FDI = fabric:get_full_doc_info(DbName, DocId, []),
    #doc_info{revs = Revs} = couch_doc:to_doc_info(FDI),
    case erlang:length(Revs) of
        ExpectRevCount ->
            couch_log:notice(
                "[~p] wait end by expect, time used:~p, DocId:~p",
                [?MODULE, 5 - TimeOut, DocId]
            ),
            ok;
        true ->
            timer:sleep(1000),
            wait_for_replicate(DbName, DocId, ExpectRevCount, TimeOut - 1)
    end,
    ok.

get_sigs(DbName) ->
    {ok, DesignDocs} = fabric:design_docs(DbName),
    lists:usort(
        lists:flatmap(
            fun active_sigs/1,
            [couch_doc:from_json_obj(DD) || DD <- DesignDocs]
        )
    ).

active_sigs(#doc{body = {Fields}} = Doc) ->
    {RawIndexes} = couch_util:get_value(<<"indexes">>, Fields, {[]}),
    {IndexNames, _} = lists:unzip(RawIndexes),
    [
        begin
            {ok, Index} = dreyfus_index:design_doc_to_index(Doc, IndexName),
            Index#index.sig
        end
     || IndexName <- IndexNames
    ].
