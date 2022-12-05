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

-module(couch_replicator_retain_stats_between_job_runs).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").

-define(DELAY, 500).
-define(TIMEOUT, 60000).

stats_retained_test_() ->
    {
        foreach,
        fun couch_replicator_test_helper:test_setup/0,
        fun couch_replicator_test_helper:test_teardown/1,
        [
            ?TDEF_FE(t_stats_retained_by_scheduler),
            ?TDEF_FE(t_stats_retained_on_job_removal)
        ]
    }.

t_stats_retained_by_scheduler({_Ctx, {Source, Target}}) ->
    {ok, _} = add_vdu(Target),
    populate_db_reject_even_docs(Source, 1, 10),
    {ok, RepPid, RepId} = replicate(Source, Target),
    wait_target_in_sync(6, Target),

    check_active_tasks(10, 5, 5),
    check_scheduler_jobs(10, 5, 5),

    stop_job(RepPid),
    check_scheduler_jobs(10, 5, 5),

    start_job(),
    check_active_tasks(10, 5, 5),
    check_scheduler_jobs(10, 5, 5),
    couch_replicator_scheduler:remove_job(RepId).

t_stats_retained_on_job_removal({_Ctx, {Source, Target}}) ->
    {ok, _} = add_vdu(Target),
    populate_db_reject_even_docs(Source, 1, 10),
    {ok, _, RepId} = replicate(Source, Target),
    % 5 + 1 vdu
    wait_target_in_sync(6, Target),

    check_active_tasks(10, 5, 5),
    check_scheduler_jobs(10, 5, 5),

    couch_replicator_scheduler:remove_job(RepId),

    populate_db_reject_even_docs(Source, 11, 20),
    {ok, _, RepId} = replicate(Source, Target),
    % 6 + 5
    wait_target_in_sync(11, Target),

    check_scheduler_jobs(20, 10, 10),
    check_active_tasks(20, 10, 10),

    couch_replicator_scheduler:remove_job(RepId),

    populate_db_reject_even_docs(Source, 21, 30),
    {ok, _, RepId} = replicate(Source, Target),
    % 11 + 5
    wait_target_in_sync(16, Target),

    check_scheduler_jobs(30, 15, 15),
    check_active_tasks(30, 15, 15),

    couch_replicator_scheduler:remove_job(RepId).

stop_job(RepPid) ->
    Ref = erlang:monitor(process, RepPid),
    gen_server:cast(couch_replicator_scheduler, {set_max_jobs, 0}),
    couch_replicator_scheduler:reschedule(),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after ?TIMEOUT ->
        erlang:error(timeout)
    end.

start_job() ->
    gen_server:cast(couch_replicator_scheduler, {set_max_jobs, 500}),
    couch_replicator_scheduler:reschedule().

check_active_tasks(DocsRead, DocsWritten, DocsFailed) ->
    RepTask = wait_for_task_status(DocsWritten),
    ?assertNotEqual(timeout, RepTask),
    ?assertEqual(DocsRead, couch_util:get_value(docs_read, RepTask)),
    ?assertEqual(DocsRead, couch_util:get_value(bulk_get_docs, RepTask)),
    ?assertEqual(DocsRead, couch_util:get_value(bulk_get_attempts, RepTask)),
    ?assertEqual(DocsWritten, couch_util:get_value(docs_written, RepTask)),
    ?assertEqual(
        DocsFailed,
        couch_util:get_value(
            doc_write_failures,
            RepTask
        )
    ).

check_scheduler_jobs(DocsRead, DocsWritten, DocFailed) ->
    Info = wait_scheduler_info(DocsWritten),
    ?assert(maps:is_key(<<"changes_pending">>, Info)),
    ?assert(maps:is_key(<<"doc_write_failures">>, Info)),
    ?assert(maps:is_key(<<"docs_read">>, Info)),
    ?assert(maps:is_key(<<"bulk_get_docs">>, Info)),
    ?assert(maps:is_key(<<"bulk_get_attempts">>, Info)),
    ?assert(maps:is_key(<<"docs_written">>, Info)),
    ?assert(maps:is_key(<<"missing_revisions_found">>, Info)),
    ?assert(maps:is_key(<<"checkpointed_source_seq">>, Info)),
    ?assert(maps:is_key(<<"source_seq">>, Info)),
    ?assert(maps:is_key(<<"revisions_checked">>, Info)),
    ?assertMatch(#{<<"docs_read">> := DocsRead}, Info),
    ?assertMatch(#{<<"bulk_get_docs">> := DocsRead}, Info),
    ?assertMatch(#{<<"bulk_get_attempts">> := DocsRead}, Info),
    ?assertMatch(#{<<"docs_written">> := DocsWritten}, Info),
    ?assertMatch(#{<<"doc_write_failures">> := DocFailed}, Info).

replication_tasks() ->
    lists:filter(
        fun(P) ->
            couch_util:get_value(type, P) =:= replication
        end,
        couch_task_status:all()
    ).

wait_for_task_status(DocsWritten) ->
    test_util:wait(fun() ->
        case replication_tasks() of
            [] ->
                wait;
            [RepTask] ->
                case couch_util:get_value(docs_written, RepTask) of
                    DocsWritten -> RepTask;
                    _Other -> wait
                end
        end
    end).

wait_scheduler_info(DocsWritten) ->
    test_util:wait(fun() ->
        case scheduler_jobs() of
            [] ->
                wait;
            [#{<<"info">> := null}] ->
                wait;
            [#{<<"info">> := Info}] ->
                case maps:get(<<"docs_written">>, Info, undefined) of
                    DocsWritten -> Info;
                    _Other -> wait
                end
        end
    end).

populate_db_reject_even_docs(DbName, Start, End) ->
    BodyFun = fun(Id) ->
        case Id rem 2 == 0 of
            true -> {[{<<"nope">>, true}]};
            false -> {[]}
        end
    end,
    populate_db(DbName, Start, End, BodyFun).

populate_db(DbName, Start, End, BodyFun) when is_function(BodyFun, 1) ->
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = integer_to_binary(DocIdCounter),
            Doc = #doc{id = Id, body = BodyFun(DocIdCounter)},
            [Doc | Acc]
        end,
        [],
        lists:seq(Start, End)
    ),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

wait_target_in_sync(DocCount, Target) when is_integer(DocCount) ->
    wait_target_in_sync_loop(DocCount, Target, 300).

wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    erlang:error(
        {assertion_failed, [
            {module, ?MODULE},
            {line, ?LINE},
            {reason, "Could not get source and target databases in sync"}
        ]}
    );
wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft) ->
    {ok, TargetDocCount} = fabric:get_doc_count(TargetName),
    case TargetDocCount == DocCount of
        true ->
            true;
        false ->
            ok = timer:sleep(?DELAY),
            wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft - 1)
    end.

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target) ->
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"continuous">>, true}
        ]},
    {ok, Rep} = couch_replicator_parse:parse_rep_doc(RepObject, ?ADMIN_USER),
    ok = couch_replicator_scheduler:add_job(Rep),
    couch_replicator_scheduler:reschedule(),
    Pid = couch_replicator_test_helper:get_pid(Rep#rep.id),
    {ok, Pid, Rep#rep.id}.

scheduler_jobs() ->
    JobsUrl = couch_replicator_test_helper:cluster_db_url(<<"_scheduler/jobs">>),
    {ok, 200, _, Body} = test_request:get(?b2l(JobsUrl), []),
    Json = jiffy:decode(Body, [return_maps]),
    maps:get(<<"jobs">>, Json).

vdu() ->
    <<
        "function(newDoc, oldDoc, userCtx) {\n"
        "        if(newDoc.nope === true) {\n"
        "            throw({forbidden: 'nope'});\n"
        "        } else {\n"
        "            return;\n"
        "        }\n"
        "    }"
    >>.

add_vdu(DbName) ->
    DocProps = [
        {<<"_id">>, <<"_design/vdu">>},
        {<<"language">>, <<"javascript">>},
        {<<"validate_doc_update">>, vdu()}
    ],
    Doc = couch_doc:from_json_obj({DocProps}, []),
    {ok, _Rev} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]).
