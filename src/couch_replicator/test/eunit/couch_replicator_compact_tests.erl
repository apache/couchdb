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

-module(couch_replicator_compact_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").

-define(ATTFILE, filename:join([?FIXTURESDIR, "logo.png"])).
-define(DELAY, 500).
-define(TIMEOUT, 360000).
-define(TIMEOUT_WRITER, 100000).
-define(TIMEOUT_EUNIT, ?TIMEOUT div 1000 + 70).
-define(WRITE_BATCH_SIZE, 25).

setup_db() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.

teardown_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

test_setup() ->
    Ctx = test_util:start_couch([couch_replicator]),
    Source = setup_db(),
    Target = setup_db(),
    {Ctx, {Source, Target}}.

test_teardown({Ctx, {Source, Target}}) ->
    teardown_db(Source),
    teardown_db(Target),
    ok = test_util:stop_couch(Ctx).

compact_test_() ->
    {
        "Compaction during replication tests",
        {
            foreach,
            fun test_setup/0,
            fun test_teardown/1,
            [
                ?TDEF_FE(populate_replicate_compact, ?TIMEOUT_EUNIT)
            ]
        }
    }.

populate_replicate_compact({_Ctx, {Source, Target}}) ->
    {ok, RepPid, RepId} = replicate(Source, Target),
    check_active_tasks(RepPid, RepId, Source, Target),
    all_processes_are_alive(RepPid, Source, Target),
    populate_and_compact(RepPid, Source, Target, 50, 3),
    wait_target_in_sync(Source, Target),
    check_active_tasks(RepPid, RepId, Source, Target),
    cancel_replication(RepId, RepPid),
    compare_databases(Source, Target).

all_processes_are_alive(RepPid, Source, Target) ->
    {ok, SourceDb} = reopen_db(Source),
    {ok, TargetDb} = reopen_db(Target),
    ?assert(is_process_alive(RepPid)),
    ?assert(is_process_alive(couch_db:get_pid(SourceDb))),
    ?assert(is_process_alive(couch_db:get_pid(TargetDb))).

check_active_tasks(RepPid, {BaseId, Ext} = _RepId, Src, Tgt) ->
    Source = <<(db_url(Src))/binary, $/>>,
    Target = <<(db_url(Tgt))/binary, $/>>,
    FullRepId = ?l2b(BaseId ++ Ext),
    Pid = ?l2b(pid_to_list(RepPid)),
    RepTasks = wait_for_task_status(),
    ?assertNotEqual(timeout, RepTasks),
    [RepTask] = RepTasks,
    ?assertEqual(Pid, couch_util:get_value(pid, RepTask)),
    ?assertEqual(FullRepId, couch_util:get_value(replication_id, RepTask)),
    ?assertEqual(true, couch_util:get_value(continuous, RepTask)),
    ?assertEqual(Source, couch_util:get_value(source, RepTask)),
    ?assertEqual(Target, couch_util:get_value(target, RepTask)),
    ?assert(is_integer(couch_util:get_value(docs_read, RepTask))),
    ?assert(is_integer(couch_util:get_value(docs_written, RepTask))),
    ?assert(is_integer(couch_util:get_value(doc_write_failures, RepTask))),
    ?assert(is_integer(couch_util:get_value(revisions_checked, RepTask))),
    ?assert(is_integer(couch_util:get_value(missing_revisions_found, RepTask))),
    ?assert(is_binary(couch_util:get_value(checkpointed_source_seq, RepTask))),
    ?assert(is_binary(couch_util:get_value(source_seq, RepTask))),
    Pending = couch_util:get_value(changes_pending, RepTask),
    ?assert(is_integer(Pending)).

replication_tasks() ->
    lists:filter(
        fun(P) ->
            couch_util:get_value(type, P) =:= replication
        end,
        couch_task_status:all()
    ).

wait_for_task_status() ->
    test_util:wait(fun() ->
        case replication_tasks() of
            [] ->
                wait;
            Tasks ->
                Tasks
        end
    end).

cancel_replication(RepId, RepPid) ->
    ok = couch_replicator_scheduler:remove_job(RepId),
    ?assertNot(is_process_alive(RepPid)).

populate_and_compact(RepPid, Source, Target, BatchSize, Rounds) ->
    {ok, SourceDb0} = reopen_db(Source),
    Writer = spawn_writer(SourceDb0),
    lists:foreach(
        fun(N) ->
            {ok, SourceDb} = reopen_db(Source),
            {ok, TargetDb} = reopen_db(Target),
            pause_writer(Writer),

            compact_db("source", SourceDb),
            ?assert(is_process_alive(RepPid)),
            ?assert(is_process_alive(couch_db:get_pid(SourceDb))),
            wait_for_compaction("source", SourceDb),

            compact_db("target", TargetDb),
            ?assert(is_process_alive(RepPid)),
            ?assert(is_process_alive(couch_db:get_pid(TargetDb))),
            wait_for_compaction("target", TargetDb),

            {ok, SourceDb2} = reopen_db(SourceDb),
            {ok, TargetDb2} = reopen_db(TargetDb),

            resume_writer(Writer),
            wait_writer(Writer, BatchSize * N),

            compact_db("source", SourceDb2),
            ?assert(is_process_alive(RepPid)),
            ?assert(is_process_alive(couch_db:get_pid(SourceDb2))),
            pause_writer(Writer),
            wait_for_compaction("source", SourceDb2),
            resume_writer(Writer),

            compact_db("target", TargetDb2),
            ?assert(is_process_alive(RepPid)),
            ?assert(is_process_alive(couch_db:get_pid(TargetDb2))),
            pause_writer(Writer),
            wait_for_compaction("target", TargetDb2),
            resume_writer(Writer)
        end,
        lists:seq(1, Rounds)
    ),
    stop_writer(Writer).

wait_target_in_sync(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, SourceInfo} = couch_db:get_db_info(SourceDb),
    ok = couch_db:close(SourceDb),
    SourceDocCount = couch_util:get_value(doc_count, SourceInfo),
    wait_target_in_sync_loop(SourceDocCount, Target, 300).

wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    erlang:error(
        {assertion_failed, [
            {module, ?MODULE},
            {line, ?LINE},
            {reason, "Could not get source and target databases in sync"}
        ]}
    );
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

compare_databases(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, TargetDb} = couch_db:open_int(Target, []),
    Fun = fun(FullDocInfo, Acc) ->
        {ok, Doc} = couch_db:open_doc(SourceDb, FullDocInfo),
        {Props} = DocJson = couch_doc:to_json_obj(Doc, [attachments]),
        DocId = couch_util:get_value(<<"_id">>, Props),
        DocTarget =
            case couch_db:open_doc(TargetDb, DocId) of
                {ok, DocT} ->
                    DocT;
                Error ->
                    erlang:error(
                        {assertion_failed, [
                            {module, ?MODULE},
                            {line, ?LINE},
                            {reason,
                                lists:concat([
                                    "Error opening document '",
                                    ?b2l(DocId),
                                    "' from target: ",
                                    couch_util:to_list(Error)
                                ])}
                        ]}
                    )
            end,
        DocTargetJson = couch_doc:to_json_obj(DocTarget, [attachments]),
        ?assertEqual(DocJson, DocTargetJson),
        {ok, Acc}
    end,
    {ok, _} = couch_db:fold_docs(SourceDb, Fun, [], []),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb).

reopen_db(DbName) when is_binary(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ok = couch_db:close(Db),
    {ok, Db};
reopen_db(Db) ->
    reopen_db(couch_db:name(Db)).

compact_db(Type, Db0) ->
    Name = couch_db:name(Db0),
    {ok, Db} = couch_db:open_int(Name, []),
    {ok, CompactPid} = couch_db:start_compact(Db),
    MonRef = erlang:monitor(process, CompactPid),
    receive
        {'DOWN', MonRef, process, CompactPid, normal} ->
            ok;
        {'DOWN', MonRef, process, CompactPid, noproc} ->
            ok;
        {'DOWN', MonRef, process, CompactPid, Reason} ->
            erlang:error(
                {assertion_failed, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {reason,
                        lists:concat([
                            "Error compacting ",
                            Type,
                            " database ",
                            ?b2l(Name),
                            ": ",
                            couch_util:to_list(Reason)
                        ])}
                ]}
            )
    after ?TIMEOUT ->
        erlang:error(
            {assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason,
                    lists:concat([
                        "Compaction for ",
                        Type,
                        " database ",
                        ?b2l(Name),
                        " didn't finish"
                    ])}
            ]}
        )
    end,
    ok = couch_db:close(Db).

wait_for_compaction(Type, Db) ->
    case couch_db:wait_for_compaction(Db) of
        ok ->
            ok;
        {error, noproc} ->
            ok;
        {error, Reason} ->
            erlang:error(
                {assertion_failed, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {reason,
                        lists:concat([
                            "Compaction of ",
                            Type,
                            " database failed with: ",
                            Reason
                        ])}
                ]}
            )
    end.

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

wait_writer(Pid, NumDocs) ->
    case get_writer_num_docs_written(Pid) of
        N when N >= NumDocs ->
            ok;
        _ ->
            wait_writer(Pid, NumDocs)
    end.

spawn_writer(Db) ->
    Parent = self(),
    Pid = spawn(fun() -> writer_loop(Db, Parent, 0) end),
    Pid.

pause_writer(Pid) ->
    Ref = make_ref(),
    Pid ! {pause, Ref},
    receive
        {paused, Ref} ->
            ok
    after ?TIMEOUT_WRITER ->
        erlang:error(
            {assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason, "Failed to pause source database writer"}
            ]}
        )
    end.

resume_writer(Pid) ->
    Ref = make_ref(),
    Pid ! {continue, Ref},
    receive
        {ok, Ref} ->
            ok
    after ?TIMEOUT_WRITER ->
        erlang:error(
            {assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason, "Failed to pause source database writer"}
            ]}
        )
    end.

get_writer_num_docs_written(Pid) ->
    Ref = make_ref(),
    Pid ! {get_count, Ref},
    receive
        {count, Ref, Count} ->
            Count
    after ?TIMEOUT_WRITER ->
        erlang:error(
            {assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason,
                    "Timeout getting number of documents written"
                    " from source database writer"}
            ]}
        )
    end.

stop_writer(Pid) ->
    Ref = make_ref(),
    Pid ! {stop, Ref},
    receive
        {stopped, Ref, DocsWritten} ->
            MonRef = erlang:monitor(process, Pid),
            receive
                {'DOWN', MonRef, process, Pid, _Reason} ->
                    DocsWritten
            after ?TIMEOUT ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, "Timeout stopping source database writer"}
                    ]}
                )
            end
    after ?TIMEOUT_WRITER ->
        erlang:error(
            {assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason, "Timeout stopping source database writer"}
            ]}
        )
    end.

writer_loop(Db0, Parent, Counter) ->
    DbName = couch_db:name(Db0),
    {ok, Data} = file:read_file(?ATTFILE),
    maybe_pause(Parent, Counter),
    Docs = lists:map(
        fun(I) ->
            couch_doc:from_json_obj(
                {[
                    {<<"_id">>, ?l2b(integer_to_list(Counter + I))},
                    {<<"value">>, Counter + I},
                    {<<"_attachments">>,
                        {[
                            {<<"icon1.png">>,
                                {[
                                    {<<"data">>, base64:encode(Data)},
                                    {<<"content_type">>, <<"image/png">>}
                                ]}},
                            {<<"icon2.png">>,
                                {[
                                    {<<"data">>, base64:encode(iolist_to_binary([Data, Data]))},
                                    {<<"content_type">>, <<"image/png">>}
                                ]}}
                        ]}}
                ]}
            )
        end,
        lists:seq(1, ?WRITE_BATCH_SIZE)
    ),
    maybe_pause(Parent, Counter),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    ok = couch_db:close(Db),
    receive
        {get_count, Ref} ->
            Parent ! {count, Ref, Counter + ?WRITE_BATCH_SIZE},
            writer_loop(Db, Parent, Counter + ?WRITE_BATCH_SIZE);
        {stop, Ref} ->
            Parent ! {stopped, Ref, Counter + ?WRITE_BATCH_SIZE}
    after 0 ->
        timer:sleep(?DELAY),
        writer_loop(Db, Parent, Counter + ?WRITE_BATCH_SIZE)
    end.

maybe_pause(Parent, Counter) ->
    receive
        {get_count, Ref} ->
            Parent ! {count, Ref, Counter};
        {pause, Ref} ->
            Parent ! {paused, Ref},
            receive
                {continue, Ref2} ->
                    Parent ! {ok, Ref2}
            end
    after 0 ->
        ok
    end.

db_url(DbName) ->
    % Note we're returning the backend (local) URL here
    iolist_to_binary([
        "http://",
        config:get("httpd", "bind_address", "127.0.0.1"),
        ":",
        integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
        "/",
        DbName
    ]).
