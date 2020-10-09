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

-module(couch_replicator_db_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


couch_replicator_db_test_() ->
    {
        "Replications are started from docs in _replicator dbs",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(default_replicator_db_is_created),
                    ?TDEF_FE(continuous_replication_created_from_doc, 15),
                    ?TDEF_FE(normal_replication_created_from_doc, 15),
                    ?TDEF_FE(replicator_db_deleted, 15),
                    ?TDEF_FE(replicator_db_recreated, 15),
                    ?TDEF_FE(invalid_replication_docs),
                    ?TDEF_FE(duplicate_persistent_replication, 15),
                    ?TDEF_FE(duplicate_transient_replication, 30)
                ]
            }
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    create_doc(Source, #{<<"_id">> => <<"doc1">>}),
    Target = couch_replicator_test_helper:create_db(),
    Name = ?tempdb(),
    RepDb = couch_replicator_test_helper:create_db(<<Name/binary,
        "/_replicator">>),
    config:set("replicator", "stats_update_interval_sec", "0", false),
    config:set("replicator", "create_replicator_db", "false", false),
    config:set("couchdb", "enable_database_recovery", "false", false),
    config:set("replicator", "min_backoff_penalty_sec", "1", false),
    {Source, Target, RepDb}.


teardown({Source, Target, RepDb}) ->
    config:delete("replicator", "stats_update_interval_sec", false),
    config:delete("replicator", "create_replicator_db", false),
    config:delete("couchdb", "enable_database_recovery", false),
    config:delete("replicator", "min_backoff_penalty_sec", false),

    couch_replicator_test_helper:delete_db(RepDb),
    couch_replicator_test_helper:delete_db(?REP_DB_NAME),
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


default_replicator_db_is_created({_, _, _}) ->
    config:set("replicator", "create_replicator_db", "true", false),
    ?assertEqual(ignore, couch_replicator:ensure_rep_db_exists()),
    ?assertMatch({ok, #{}}, fabric2_db:open(?REP_DB_NAME, [])).


continuous_replication_created_from_doc({Source, Target, RepDb}) ->
    DocId = <<"rdoc1">>,
    RDoc = rep_doc(Source, Target, DocId, #{<<"continuous">> => true}),
    create_doc(RepDb, RDoc),
    wait_scheduler_docs_state(RepDb, DocId, <<"running">>),

    {Code, DocInfo} = scheduler_docs(RepDb, DocId),
    ?assertEqual(200, Code),
    ?assertMatch(#{
        <<"database">> := RepDb,
        <<"doc_id">> := DocId
    }, DocInfo),

    RepId = maps:get(<<"id">>, DocInfo),

    ?assertMatch([#{
        <<"database">> := RepDb,
        <<"doc_id">> := DocId,
        <<"id">> := RepId,
        <<"state">> := <<"running">>
    }], couch_replicator_test_helper:scheduler_jobs()),

    ?assertMatch({200, #{
        <<"database">> := RepDb,
        <<"doc_id">> := DocId,
        <<"id">> := RepId,
        <<"state">> := <<"running">>
    }}, scheduler_jobs(RepId)),

    delete_doc(RepDb, DocId),
    wait_scheduler_docs_not_found(RepDb, DocId),
    ?assertMatch({404, #{}}, scheduler_jobs(RepId)).


normal_replication_created_from_doc({Source, Target, RepDb}) ->
    DocId = <<"rdoc2">>,
    RDoc = rep_doc(Source, Target, DocId),
    create_doc(RepDb, RDoc),
    wait_scheduler_docs_state(RepDb, DocId, <<"completed">>),

    {Code, DocInfo} = scheduler_docs(RepDb, DocId),
    ?assertEqual(200, Code),
    ?assertMatch(#{
        <<"database">> := RepDb,
        <<"doc_id">> := DocId,
        <<"state">> := <<"completed">>,
        <<"info">> := #{
            <<"docs_written">> := 1,
            <<"docs_read">> := 1,
            <<"missing_revisions_found">> := 1
        }
    }, DocInfo),

    wait_doc_state(RepDb, DocId, <<"completed">>),
    ?assertMatch(#{
        <<"_replication_state">> := <<"completed">>,
        <<"_replication_stats">> := #{
            <<"docs_written">> := 1,
            <<"docs_read">> := 1,
            <<"missing_revisions_found">> := 1
        }
    }, read_doc(RepDb, DocId)),

    delete_doc(RepDb, DocId),
    wait_scheduler_docs_not_found(RepDb, DocId).


replicator_db_deleted({Source, Target, RepDb}) ->
    DocId = <<"rdoc3">>,
    RDoc = rep_doc(Source, Target, DocId, #{<<"continuous">> => true}),
    create_doc(RepDb, RDoc),
    wait_scheduler_docs_state(RepDb, DocId, <<"running">>),
    fabric2_db:delete(RepDb, [?ADMIN_CTX]),
    wait_scheduler_docs_not_found(RepDb, DocId).


replicator_db_recreated({Source, Target, RepDb}) ->
    DocId = <<"rdoc4">>,
    RDoc = rep_doc(Source, Target, DocId, #{<<"continuous">> => true}),
    create_doc(RepDb, RDoc),
    wait_scheduler_docs_state(RepDb, DocId, <<"running">>),

    config:set("couchdb", "enable_database_recovery", "true", false),
    fabric2_db:delete(RepDb, [?ADMIN_CTX]),
    wait_scheduler_docs_not_found(RepDb, DocId),

    Opts = [{start_key, RepDb}, {end_key, RepDb}],
    {ok, [DbInfo]} = fabric2_db:list_deleted_dbs_info(Opts),
    {_, Timestamp} = lists:keyfind(timestamp, 1, DbInfo),
    ok = fabric2_db:undelete(RepDb, RepDb, Timestamp, [?ADMIN_CTX]),
    wait_scheduler_docs_state(RepDb, DocId, <<"running">>),

    config:set("couchdb", "enable_database_recovery", "false", false),
    fabric2_db:delete(RepDb, [?ADMIN_CTX]),
    wait_scheduler_docs_not_found(RepDb, DocId).


invalid_replication_docs({_, _, RepDb}) ->
    Docs = [
        #{
            <<"_id">> => <<"1">>,
            <<"source">> => <<"http://127.0.0.1:1000">>
        },
        #{
            <<"_id">> => <<"1">>,
            <<"target">> => <<"http://127.0.0.1:1001">>
        },
        #{
            <<"_id">> => <<"1">>
        },
        #{
            <<"_id">> => <<"1">>,
            <<"source">> => <<"http://127.0.0.1:1002">>,
            <<"target">> => <<"http://127.0.0.1:1003">>,
            <<"create_target">> => <<"bad">>
        },
        #{
            <<"_id">> => <<"1">>,
            <<"source">> => #{<<"junk">> => 42},
            <<"target">> => <<"http://127.0.0.1:1004">>
        },
        #{
            <<"_id">> => <<"1">>,
            <<"source">> => <<"http://127.0.0.1:1005">>,
            <<"target">> => <<"http://127.0.0.1:1006">>,
            <<"selector">> => #{},
            <<"filter">> => <<"a/b">>
        },
        #{
            <<"_id">> => <<"1">>,
            <<"source">> => <<"http://127.0.0.1:1007">>,
            <<"target">> => <<"https://127.0.0.1:1008">>,
            <<"doc_ids">> => 42
        }
    ],
    lists:foreach(fun(Doc) ->
        ?assertThrow({forbidden, _}, create_doc(RepDb, Doc))
    end, Docs).


duplicate_persistent_replication({Source, Target, RepDb}) ->
    DocId1 = <<"rdoc5">>,
    RDoc1 = rep_doc(Source, Target, DocId1, #{<<"continuous">> => true}),
    create_doc(RepDb, RDoc1),
    wait_scheduler_docs_state(RepDb, DocId1, <<"running">>),

    DocId2 = <<"rdoc6">>,
    RDoc2 = rep_doc(Source, Target, DocId2, #{<<"continuous">> => true}),
    create_doc(RepDb, RDoc2),
    wait_scheduler_docs_state(RepDb, DocId2, <<"failed">>),

    delete_doc(RepDb, DocId1),
    delete_doc(RepDb, DocId2),

    wait_scheduler_docs_not_found(RepDb, DocId1),
    wait_scheduler_docs_not_found(RepDb, DocId2).


duplicate_transient_replication({Source, Target, RepDb}) ->
    {ok, _Pid, RepId} = couch_replicator_test_helper:replicate_continuous(
        Source, Target),

    DocId = <<"rdoc7">>,
    RDoc = rep_doc(Source, Target, DocId, #{<<"continuous">> => true}),
    create_doc(RepDb, RDoc),
    wait_scheduler_docs_state(RepDb, DocId, <<"crashing">>),

    couch_replicator_test_helper:cancel(RepId),
    wait_reschedule_docs_state(RepDb, DocId, <<"running">>),

    delete_doc(RepDb, DocId),
    wait_scheduler_docs_not_found(RepDb, DocId).


scheduler_jobs(Id) ->
    SUrl = couch_replicator_test_helper:server_url(),
    Url = lists:flatten(io_lib:format("~s/_scheduler/jobs/~s", [SUrl, Id])),
    {ok, Code, _, Body} = test_request:get(Url, []),
    {Code, jiffy:decode(Body, [return_maps])}.


scheduler_docs(DbName, DocId) ->
    SUrl = couch_replicator_test_helper:server_url(),
    Fmt = "~s/_scheduler/docs/~s/~s",
    Url = lists:flatten(io_lib:format(Fmt, [SUrl, DbName, DocId])),
    {ok, Code, _, Body} = test_request:get(Url, []),
    {Code, jiffy:decode(Body, [return_maps])}.


rep_doc(Source, Target, DocId) ->
    rep_doc(Source, Target, DocId, #{}).


rep_doc(Source, Target, DocId, #{} = Extra) ->
    maps:merge(#{
        <<"_id">> => DocId,
        <<"source">> => couch_replicator_test_helper:db_url(Source),
        <<"target">> => couch_replicator_test_helper:db_url(Target)
    }, Extra).


create_doc(DbName, Doc) ->
    couch_replicator_test_helper:create_docs(DbName, [Doc]).


delete_doc(DbName, DocId) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    {ok, Doc} = fabric2_db:open_doc(Db, DocId),
    Doc1 = Doc#doc{deleted = true},
    {ok, _} = fabric2_db:update_doc(Db, Doc1, []).


read_doc(DbName, DocId) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    {ok, Doc} = fabric2_db:open_doc(Db, DocId, [ejson_body]),
    Body = Doc#doc.body,
    couch_util:json_decode(couch_util:json_encode(Body), [return_maps]).


wait_scheduler_docs_state(DbName, DocId, State) ->
    test_util:wait(fun() ->
        case scheduler_docs(DbName, DocId) of
            {200, #{<<"state">> := State} = Res} -> Res;
            {_, _} -> wait
        end
    end, 10000, 250).


wait_scheduler_docs_not_found(DbName, DocId) ->
    test_util:wait(fun() ->
        case scheduler_docs(DbName, DocId) of
            {404, _} -> ok;
            {_, _} -> wait
        end
    end, 10000, 250).


wait_reschedule_docs_state(DbName, DocId, State) ->
    test_util:wait(fun() ->
        couch_replicator_job_server:reschedule(),
        case scheduler_docs(DbName, DocId) of
            {200, #{<<"state">> := State} = Res} -> Res;
            {_, _} -> wait
        end
    end, 10000, 500).


wait_doc_state(DbName, DocId, State) ->
    test_util:wait(fun() ->
        case read_doc(DbName, DocId) of
            #{<<"_replication_state">> := State} -> ok;
            #{} -> wait
        end
    end, 10000, 250).
