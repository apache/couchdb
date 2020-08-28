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

-module(couch_replicator_filtered_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

-define(DDOC_ID, <<"_design/filter_ddoc">>).
-define(DDOC, #{
    <<"_id">> => ?DDOC_ID,
    <<"filters">> => #{
        <<"testfilter">> => <<"
            function(doc, req){if (doc.class == 'mammal') return true;}
        ">>,
        <<"queryfilter">> => <<"
            function(doc, req) {
                if (doc.class && req.query.starts) {
                    return doc.class.indexOf(req.query.starts) === 0;
                }
                else {
                    return false;
                }
            }
        ">>
    },
    <<"views">> => #{
        <<"mammals">> => #{
            <<"map">> => <<"
                function(doc) {
                    if (doc.class == 'mammal') {
                        emit(doc._id, null);
                    }
                }
            ">>
        }
    }
}).


filtered_replication_test_() ->
    {
        "Replications with filters tests",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(filtered_replication_test),
                    ?TDEF_FE(query_filtered_replication_test),
                    ?TDEF_FE(view_filtered_replication_test),
                    ?TDEF_FE(replication_id_changes_if_filter_changes, 15)
                ]
            }
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    create_docs(Source),
    Target = couch_replicator_test_helper:create_db(),
    config:set("replicator", "stats_update_interval_sec", "0", false),
    config:set("replicator", "interval_sec", "1", false),
    {Source, Target}.


teardown({Source, Target}) ->
    config:delete("replicator", "stats_update_interval_sec", false),
    config:delete("replicator", "checkpoint_interval", false),
    config:delete("replicator", "interval_sec", false),
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


filtered_replication_test({Source, Target}) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"filter">>, <<"filter_ddoc/testfilter">>}
    ]},
    {ok, _} = couch_replicator_test_helper:replicate(RepObject),
    FilterFun = fun(_DocId, {Props}) ->
        couch_util:get_value(<<"class">>, Props) == <<"mammal">>
    end,
    {ok, TargetDbInfo, AllReplies} = compare_dbs(Source, Target, FilterFun),
    ?assertEqual(1, proplists:get_value(doc_count, TargetDbInfo)),
    ?assertEqual(0, proplists:get_value(doc_del_count, TargetDbInfo)),
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies)).


query_filtered_replication_test({Source, Target}) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"filter">>, <<"filter_ddoc/queryfilter">>},
        {<<"query_params">>, {[
            {<<"starts">>, <<"a">>}
        ]}}
    ]},
    {ok, _} = couch_replicator_test_helper:replicate(RepObject),
    FilterFun = fun(_DocId, {Props}) ->
        case couch_util:get_value(<<"class">>, Props) of
            <<"a", _/binary>> -> true;
            _ -> false
        end
    end,
    {ok, TargetDbInfo, AllReplies} = compare_dbs(Source, Target, FilterFun),
    ?assertEqual(2, proplists:get_value(doc_count, TargetDbInfo)),
    ?assertEqual(0, proplists:get_value(doc_del_count, TargetDbInfo)),
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies)).


view_filtered_replication_test({Source, Target}) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"filter">>, <<"_view">>},
        {<<"query_params">>, {[
            {<<"view">>, <<"filter_ddoc/mammals">>}
        ]}}
    ]},
    {ok, _} = couch_replicator_test_helper:replicate(RepObject),
    FilterFun = fun(_DocId, {Props}) ->
        couch_util:get_value(<<"class">>, Props) == <<"mammal">>
    end,
    {ok, TargetDbInfo, AllReplies} = compare_dbs(Source, Target, FilterFun),
    ?assertEqual(1, proplists:get_value(doc_count, TargetDbInfo)),
    ?assertEqual(0, proplists:get_value(doc_del_count, TargetDbInfo)),
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies)).


replication_id_changes_if_filter_changes({Source, Target}) ->
    config:set("replicator", "checkpoint_interval", "500", false),
    Rep = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"filter">>, <<"filter_ddoc/testfilter">>},
        {<<"continuous">>, true}
    ]},
    {ok, _, RepId1} = couch_replicator_test_helper:replicate_continuous(Rep),

    wait_scheduler_docs_written(1),

    ?assertMatch([#{<<"id">> := RepId1}],
        couch_replicator_test_helper:scheduler_jobs()),

    FilterFun1 = fun(_, {Props}) ->
        couch_util:get_value(<<"class">>, Props) == <<"mammal">>
    end,
    {ok, TargetDbInfo1, AllReplies1} = compare_dbs(Source, Target, FilterFun1),
    ?assertEqual(1, proplists:get_value(doc_count, TargetDbInfo1)),
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies1)),

    {ok, SourceDb} = fabric2_db:open(Source, [?ADMIN_CTX]),
    {ok, DDoc1} = fabric2_db:open_doc(SourceDb, ?DDOC_ID),
    Flt = <<"function(doc, req) {if (doc.class == 'reptiles') return true};">>,
    DDoc2 = DDoc1#doc{body = {[
        {<<"filters">>, {[
            {<<"testfilter">>, Flt}
        ]}}
    ]}},
    {ok, {_, _}} = fabric2_db:update_doc(SourceDb, DDoc2),
    Info = wait_scheduler_repid_change(RepId1),

    RepId2 = maps:get(<<"id">>, Info),
    ?assert(RepId1 =/= RepId2),

    wait_scheduler_docs_written(1),

    FilterFun2 = fun(_, {Props}) ->
        Class = couch_util:get_value(<<"class">>, Props),
        Class == <<"mammal">> orelse Class == <<"reptiles">>
    end,
    {ok, TargetDbInfo2, AllReplies2} = compare_dbs(Source, Target, FilterFun2),
    ?assertEqual(2, proplists:get_value(doc_count, TargetDbInfo2)),
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies2)),

    couch_replicator_test_helper:cancel(RepId2).


compare_dbs(Source, Target, FilterFun) ->
    {ok, TargetDb} = fabric2_db:open(Target, [?ADMIN_CTX]),
    {ok, TargetDbInfo} = fabric2_db:get_db_info(TargetDb),
    Fun = fun(SrcDoc, TgtDoc, Acc) ->
        case FilterFun(SrcDoc#doc.id, SrcDoc#doc.body) of
            true -> [SrcDoc == TgtDoc | Acc];
            false -> [not_found == TgtDoc | Acc]
        end
    end,
    Res = couch_replicator_test_helper:compare_fold(Source, Target, Fun, []),
    {ok, TargetDbInfo, Res}.


create_docs(DbName) ->
    couch_replicator_test_helper:create_docs(DbName, [
        ?DDOC,
        #{
            <<"_id">> => <<"doc1">>,
            <<"class">> => <<"mammal">>,
            <<"value">> => 1
        },
        #{
            <<"_id">> => <<"doc2">>,
            <<"class">> => <<"amphibians">>,
            <<"value">> => 2
        },
        #{
            <<"_id">> => <<"doc3">>,
            <<"class">> => <<"reptiles">>,
            <<"value">> => 3
        },
        #{
            <<"_id">> => <<"doc4">>,
            <<"class">> => <<"arthropods">>,
            <<"value">> => 2
        }
    ]).


wait_scheduler_docs_written(DocsWritten) ->
    test_util:wait(fun() ->
        case couch_replicator_test_helper:scheduler_jobs() of
            [] ->
                wait;
            [#{<<"info">> := null}] ->
                wait;
            [#{<<"info">> := Info}] ->
                case Info of
                    #{<<"docs_written">> := DocsWritten} -> Info;
                    #{} -> wait
                end
        end
    end, 10000, 250).


wait_scheduler_repid_change(OldRepId) ->
    test_util:wait(fun() ->
        case couch_replicator_test_helper:scheduler_jobs() of
            [] ->
                wait;
            [#{<<"id">> := OldRepId}] ->
                wait;
            [#{<<"id">> := null}] ->
                wait;
            [#{<<"id">> := NewId} = Info] when is_binary(NewId) ->
                Info
        end
    end, 10000, 250).
