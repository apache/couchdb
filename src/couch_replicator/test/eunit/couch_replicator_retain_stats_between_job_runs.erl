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


setup_all() ->
    test_util:start_couch([couch_replicator, chttpd, mem3, fabric]).


teardown_all(Ctx) ->
    ok = test_util:stop_couch(Ctx).


setup() ->
    Source = setup_db(),
    Target = setup_db(),
    {Source, Target}.


teardown({Source, Target}) ->
    teardown_db(Source),
    teardown_db(Target),
    ok.


stats_retained_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun t_stats_retained_by_scheduler/1,
                fun t_stats_retained_on_job_removal/1
            ]
        }
    }.


t_stats_retained_by_scheduler({Source, Target}) ->
    ?_test(begin
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
        couch_replicator_scheduler:remove_job(RepId)
    end).


t_stats_retained_on_job_removal({Source, Target}) ->
    ?_test(begin
        {ok, _} = add_vdu(Target),
        populate_db_reject_even_docs(Source, 1, 10),
        {ok, _, RepId} = replicate(Source, Target),
        wait_target_in_sync(6, Target),  % 5 + 1 vdu

        check_active_tasks(10, 5, 5),
        check_scheduler_jobs(10, 5, 5),

        couch_replicator_scheduler:remove_job(RepId),

        populate_db_reject_even_docs(Source, 11, 20),
        {ok, _, RepId} = replicate(Source, Target),
        wait_target_in_sync(11, Target),  % 6 + 5

        check_scheduler_jobs(20, 10, 10),
        check_active_tasks(20, 10, 10),

        couch_replicator_scheduler:remove_job(RepId),

        populate_db_reject_even_docs(Source, 21, 30),
        {ok, _, RepId} = replicate(Source, Target),
        wait_target_in_sync(16, Target),  % 11 + 5

        check_scheduler_jobs(30, 15, 15),
        check_active_tasks(30, 15, 15),

        couch_replicator_scheduler:remove_job(RepId)
    end).


setup_db() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.


teardown_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.


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
    RepTask = wait_for_task_status(),
    ?assertNotEqual(timeout, RepTask),
    ?assertEqual(DocsRead, couch_util:get_value(docs_read, RepTask)),
    ?assertEqual(DocsWritten, couch_util:get_value(docs_written, RepTask)),
    ?assertEqual(DocsFailed, couch_util:get_value(doc_write_failures,
        RepTask)).


check_scheduler_jobs(DocsRead, DocsWritten, DocFailed) ->
    Info = wait_scheduler_info(),
    ?assert(maps:is_key(<<"changes_pending">>, Info)),
    ?assert(maps:is_key(<<"doc_write_failures">>, Info)),
    ?assert(maps:is_key(<<"docs_read">>, Info)),
    ?assert(maps:is_key(<<"docs_written">>, Info)),
    ?assert(maps:is_key(<<"missing_revisions_found">>, Info)),
    ?assert(maps:is_key(<<"checkpointed_source_seq">>, Info)),
    ?assert(maps:is_key(<<"source_seq">>, Info)),
    ?assert(maps:is_key(<<"revisions_checked">>, Info)),
    ?assertMatch(#{<<"docs_read">> := DocsRead}, Info),
    ?assertMatch(#{<<"docs_written">> := DocsWritten}, Info),
    ?assertMatch(#{<<"doc_write_failures">> := DocFailed}, Info).


replication_tasks() ->
    lists:filter(fun(P) ->
        couch_util:get_value(type, P) =:= replication
    end, couch_task_status:all()).


wait_for_task_status() ->
    test_util:wait(fun() ->
        case replication_tasks() of
            [] -> wait;
            [RepTask] -> RepTask
        end
    end).


wait_scheduler_info() ->
    test_util:wait(fun() ->
        case scheduler_jobs() of
            [] -> wait;
            [#{<<"info">> := null}] -> wait;
            [#{<<"info">> := Info}] -> Info
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
    {ok, Db} = couch_db:open_int(DbName, []),
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = integer_to_binary(DocIdCounter),
            Doc = #doc{id = Id, body = BodyFun(DocIdCounter)},
            [Doc | Acc]
        end,
        [], lists:seq(Start, End)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    ok = couch_db:close(Db).


wait_target_in_sync(DocCount, Target) when is_integer(DocCount) ->
    wait_target_in_sync_loop(DocCount, Target, 300).


wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    erlang:error({assertion_failed, [
          {module, ?MODULE}, {line, ?LINE},
          {reason, "Could not get source and target databases in sync"}
    ]});

wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft) ->
    {ok, Target} = couch_db:open_int(TargetName, []),
    {ok, TargetInfo} = couch_db:get_db_info(Target),
    ok = couch_db:close(Target),
    TargetDocCount = couch_util:get_value(doc_count, TargetInfo),
    case TargetDocCount  == DocCount of
        true ->
            true;
        false ->
            ok = timer:sleep(?DELAY),
            wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft - 1)
    end.


replicate(Source, Target) ->
    SrcUrl = couch_replicator_test_helper:db_url(Source),
    TgtUrl = couch_replicator_test_helper:db_url(Target),
    RepObject = {[
        {<<"source">>, SrcUrl},
        {<<"target">>, TgtUrl},
        {<<"continuous">>, true}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepObject, ?ADMIN_USER),
    ok = couch_replicator_scheduler:add_job(Rep),
    couch_replicator_scheduler:reschedule(),
    Pid = couch_replicator_test_helper:get_pid(Rep#rep.id),
    {ok, Pid, Rep#rep.id}.


scheduler_jobs() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:flatten(io_lib:format("http://~s:~b/_scheduler/jobs", [Addr, Port])),
    {ok, 200, _, Body} = test_request:get(Url, []),
    Json = jiffy:decode(Body, [return_maps]),
    maps:get(<<"jobs">>, Json).


vdu() ->
    <<"function(newDoc, oldDoc, userCtx) {
        if(newDoc.nope === true) {
            throw({forbidden: 'nope'});
        } else {
            return;
        }
    }">>.


add_vdu(DbName) ->
    DocProps = [
        {<<"_id">>, <<"_design/vdu">>},
        {<<"language">>, <<"javascript">>},
        {<<"validate_doc_update">>, vdu()}
    ],
    Doc = couch_doc:from_json_obj({DocProps}, []),
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    try
        {ok, _Rev} = couch_db:update_doc(Db, Doc, [])
    after
        couch_db:close(Db)
    end.
