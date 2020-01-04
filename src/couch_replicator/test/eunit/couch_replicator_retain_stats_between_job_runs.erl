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
-define(i2l(I), integer_to_list(I)).
-define(io2b(Io), iolist_to_binary(Io)).


setup() ->
    Ctx = test_util:start_couch([couch_replicator, chttpd, mem3, fabric]),
    Source = setup_db(),
    Target = setup_db(),
    {Ctx, {Source, Target}}.


teardown({Ctx, {Source, Target}}) ->
    teardown_db(Source),
    teardown_db(Target),
    ok = application:stop(couch_replicator),
    ok = test_util:stop_couch(Ctx).


stats_retained_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun t_stats_retained/1
    }.


t_stats_retained({_Ctx, {Source, Target}}) ->
    ?_test(begin
        populate_db(Source, 42),
        {ok, RepPid, RepId} = replicate(Source, Target),

        wait_target_in_sync(Source, Target),
        check_active_tasks(42, 42),
        check_scheduler_jobs(42, 42),

        stop_job(RepPid),
        check_scheduler_jobs(42, 42),

        start_job(),
        check_active_tasks(42, 42),
        check_scheduler_jobs(42, 42),
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


check_active_tasks(DocsRead, DocsWritten) ->
    RepTask = wait_for_task_status(),
    ?assertNotEqual(timeout, RepTask),
    ?assertEqual(DocsRead, couch_util:get_value(docs_read, RepTask)),
    ?assertEqual(DocsWritten, couch_util:get_value(docs_written, RepTask)).


check_scheduler_jobs(DocsRead, DocsWritten) ->
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
    ?assertMatch(#{<<"docs_written">> := DocsWritten}, Info).


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


populate_db(DbName, DocCount) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = ?io2b(["doc", ?i2l(DocIdCounter)]),
            Doc = #doc{id = Id, body = {[]}},
            [Doc | Acc]
        end,
        [], lists:seq(1, DocCount)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    ok = couch_db:close(Db).


wait_target_in_sync(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, SourceInfo} = couch_db:get_db_info(SourceDb),
    ok = couch_db:close(SourceDb),
    SourceDocCount = couch_util:get_value(doc_count, SourceInfo),
    wait_target_in_sync_loop(SourceDocCount, Target, 300).


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
    case TargetDocCount == DocCount of
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
