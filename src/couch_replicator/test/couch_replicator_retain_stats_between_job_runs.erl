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
    Ctx = test_util:start_couch([couch_replicator]),
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
    Priority = erlang:get(io_priority),
    ?_test(begin
        erlang:put(io_priority, Priority),
        populate_db(Source, 42),
        {ok, RepPid, RepId} = replicate(Source, Target),
        wait_target_in_sync(Source, Target),
        check_active_tasks(42, 42),
        reschedule_job(RepPid),
        check_active_tasks(42, 42),
        couch_replicator_scheduler:remove_job(RepId)
    end).


setup_db() ->
    DbName = ?tempdb(),
    erlang:put(io_priority, {interactive, DbName}),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.


teardown_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.


reschedule_job(RepPid) ->
    Ref = erlang:monitor(process, RepPid),
    gen_server:cast(couch_replicator_scheduler, {set_max_jobs, 0}),
    couch_replicator_scheduler:reschedule(),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after ?TIMEOUT ->
        erlang:error(timeout)
    end,
    gen_server:cast(couch_replicator_scheduler, {set_max_jobs, 500}),
    couch_replicator_scheduler:reschedule().


check_active_tasks(DocsRead, DocsWritten) ->
    RepTask = wait_for_task_status(),
    ?assertNotEqual(timeout, RepTask),
    ?assertEqual(DocsRead, couch_util:get_value(docs_read, RepTask)),
    ?assertEqual(DocsWritten, couch_util:get_value(docs_written, RepTask)).


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
