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

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").

-define(ADMIN_ROLE, #user_ctx{roles=[<<"_admin">>]}).
-define(ADMIN_USER, {user_ctx, ?ADMIN_ROLE}).
-define(ATTFILE, filename:join([?FIXTURESDIR, "logo.png"])).
-define(DELAY, 100).
-define(TIMEOUT, 30000).
-define(TIMEOUT_STOP, 1000).
-define(TIMEOUT_WRITER, 3000).
-define(TIMEOUT_EUNIT, ?TIMEOUT div 1000 + 5).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    ok = couch_db:close(Db),
    DbName.

setup(local) ->
    setup();
setup(remote) ->
    {remote, setup()};
setup({A, B}) ->
    {ok, _} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Source = setup(A),
    Target = setup(B),
    {Source, Target}.

teardown({remote, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_USER]),
    ok.

teardown(_, {Source, Target}) ->
    teardown(Source),
    teardown(Target),

    Pid = whereis(couch_server_sup),
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT_STOP ->
        throw({timeout, server_stop})
    end.


compact_test_() ->
    Pairs = [{local, local}, {local, remote},
             {remote, local}, {remote, remote}],
    {
        "Compaction during replication tests",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_populate_replicate_compact/2}
             || Pair <- Pairs]
        }
    }.


should_populate_replicate_compact({From, To}, {Source, Target}) ->
    {ok, RepPid, RepId} = replicate(Source, Target),
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
     {inorder, [
         should_run_replication(RepPid, RepId, Source, Target),
         should_all_processes_be_alive(RepPid, Source, Target),
         should_populate_and_compact(RepPid, Source, Target, 50, 5),
         should_wait_target_in_sync(Source, Target),
         should_ensure_replication_still_running(RepPid, RepId, Source, Target),
         should_cancel_replication(RepId, RepPid),
         should_compare_databases(Source, Target)
     ]}}.

should_all_processes_be_alive(RepPid, Source, Target) ->
    ?_test(begin
        {ok, SourceDb} = reopen_db(Source),
        {ok, TargetDb} = reopen_db(Target),
        ?assert(is_process_alive(RepPid)),
        ?assert(is_process_alive(SourceDb#db.main_pid)),
        ?assert(is_process_alive(TargetDb#db.main_pid))
    end).

should_run_replication(RepPid, RepId, Source, Target) ->
    ?_test(check_active_tasks(RepPid, RepId, Source, Target)).

should_ensure_replication_still_running(RepPid, RepId, Source, Target) ->
    ?_test(check_active_tasks(RepPid, RepId, Source, Target)).

check_active_tasks(RepPid, {BaseId, Ext} = _RepId, Src, Tgt) ->
    Source = case Src of
        {remote, NameSrc} ->
            <<(db_url(NameSrc))/binary, $/>>;
        _ ->
            Src
    end,
    Target = case Tgt of
        {remote, NameTgt} ->
            <<(db_url(NameTgt))/binary, $/>>;
        _ ->
            Tgt
    end,
    FullRepId = ?l2b(BaseId ++ Ext),
    Pid = ?l2b(pid_to_list(RepPid)),
    [RepTask] = couch_task_status:all(),
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
    ?assert(is_integer(couch_util:get_value(checkpointed_source_seq, RepTask))),
    ?assert(is_integer(couch_util:get_value(source_seq, RepTask))),
    Progress = couch_util:get_value(progress, RepTask),
    ?assert(is_integer(Progress)),
    ?assert(Progress =< 100).

should_cancel_replication(RepId, RepPid) ->
    ?_assertNot(begin
        {ok, _} = couch_replicator:cancel_replication(RepId),
        is_process_alive(RepPid)
    end).

should_populate_and_compact(RepPid, Source, Target, BatchSize, Rounds) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(begin
        {ok, SourceDb0} = reopen_db(Source),
        Writer = spawn_writer(SourceDb0),
        lists:foreach(
            fun(N) ->
                {ok, SourceDb} = reopen_db(Source),
                {ok, TargetDb} = reopen_db(Target),
                pause_writer(Writer),

                compact_db("source", SourceDb),
                ?assert(is_process_alive(RepPid)),
                ?assert(is_process_alive(SourceDb#db.main_pid)),
                check_ref_counter("source", SourceDb),

                compact_db("target", TargetDb),
                ?assert(is_process_alive(RepPid)),
                ?assert(is_process_alive(TargetDb#db.main_pid)),
                check_ref_counter("target", TargetDb),

                {ok, SourceDb2} = reopen_db(SourceDb),
                {ok, TargetDb2} = reopen_db(TargetDb),

                resume_writer(Writer),
                wait_writer(Writer, BatchSize * N),

                compact_db("source", SourceDb2),
                ?assert(is_process_alive(RepPid)),
                ?assert(is_process_alive(SourceDb2#db.main_pid)),
                pause_writer(Writer),
                check_ref_counter("source", SourceDb2),
                resume_writer(Writer),

                compact_db("target", TargetDb2),
                ?assert(is_process_alive(RepPid)),
                ?assert(is_process_alive(TargetDb2#db.main_pid)),
                pause_writer(Writer),
                check_ref_counter("target", TargetDb2),
                resume_writer(Writer)
            end, lists:seq(1, Rounds)),
        stop_writer(Writer)
    end)}.

should_wait_target_in_sync({remote, Source}, Target) ->
    should_wait_target_in_sync(Source, Target);
should_wait_target_in_sync(Source, {remote, Target}) ->
    should_wait_target_in_sync(Source, Target);
should_wait_target_in_sync(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_assert(begin
        {ok, SourceDb} = couch_db:open_int(Source, []),
        {ok, SourceInfo} = couch_db:get_db_info(SourceDb),
        ok = couch_db:close(SourceDb),
        SourceDocCount = couch_util:get_value(doc_count, SourceInfo),
        wait_target_in_sync_loop(SourceDocCount, Target, 300)
    end)}.

wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    erlang:error(
        {assertion_failed,
         [{module, ?MODULE}, {line, ?LINE},
          {reason, "Could not get source and target databases in sync"}]});
wait_target_in_sync_loop(DocCount, {remote, TargetName}, RetriesLeft) ->
    wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft);
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

should_compare_databases({remote, Source}, Target) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, {remote, Target}) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, Target) ->
    {timeout, 35, ?_test(begin
        {ok, SourceDb} = couch_db:open_int(Source, []),
        {ok, TargetDb} = couch_db:open_int(Target, []),
        Fun = fun(FullDocInfo, _, Acc) ->
            {ok, Doc} = couch_db:open_doc(SourceDb, FullDocInfo),
            {Props} = DocJson = couch_doc:to_json_obj(Doc, [attachments]),
            DocId = couch_util:get_value(<<"_id">>, Props),
            DocTarget = case couch_db:open_doc(TargetDb, DocId) of
                {ok, DocT} ->
                    DocT;
                Error ->
                    erlang:error(
                        {assertion_failed,
                         [{module, ?MODULE}, {line, ?LINE},
                          {reason, lists:concat(["Error opening document '",
                                                 ?b2l(DocId), "' from target: ",
                                                 couch_util:to_list(Error)])}]})
            end,
            DocTargetJson = couch_doc:to_json_obj(DocTarget, [attachments]),
            ?assertEqual(DocJson, DocTargetJson),
            {ok, Acc}
        end,
        {ok, _, _} = couch_db:enum_docs(SourceDb, Fun, [], []),
        ok = couch_db:close(SourceDb),
        ok = couch_db:close(TargetDb)
    end)}.


reopen_db({remote, Db}) ->
    reopen_db(Db);
reopen_db(#db{name=DbName}) ->
    reopen_db(DbName);
reopen_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ok = couch_db:close(Db),
    {ok, Db}.

compact_db(Type, #db{name = Name}) ->
    {ok, Db} = couch_db:open_int(Name, []),
    {ok, CompactPid} = couch_db:start_compact(Db),
    MonRef = erlang:monitor(process, CompactPid),
    receive
        {'DOWN', MonRef, process, CompactPid, normal} ->
            ok;
        {'DOWN', MonRef, process, CompactPid, Reason} ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason,
                   lists:concat(["Error compacting ", Type, " database ",
                                 ?b2l(Name), ": ",
                                 couch_util:to_list(Reason)])}]})
    after ?TIMEOUT ->
        erlang:error(
            {assertion_failed,
             [{module, ?MODULE}, {line, ?LINE},
              {reason, lists:concat(["Compaction for ", Type, " database ",
                                     ?b2l(Name), " didn't finish"])}]})
    end,
    ok = couch_db:close(Db).

check_ref_counter(Type, #db{name = Name, fd_ref_counter = OldRefCounter}) ->
    MonRef = erlang:monitor(process, OldRefCounter),
    receive
        {'DOWN', MonRef, process, OldRefCounter, _} ->
            ok
        after ?TIMEOUT ->
            erlang:error(
                {assertion_failed,
                 [{module, ?MODULE}, {line, ?LINE},
                  {reason, lists:concat(["Old ", Type,
                                         " database ref counter didn't"
                                         " terminate"])}]})
    end,
    {ok, #db{fd_ref_counter = NewRefCounter} = Db} = couch_db:open_int(Name, []),
    ok = couch_db:close(Db),
    ?assertNotEqual(OldRefCounter, NewRefCounter).

db_url(DbName) ->
    iolist_to_binary([
        "http://", couch_config:get("httpd", "bind_address", "127.0.0.1"),
        ":", integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
        "/", DbName
    ]).

replicate({remote, Db}, Target) ->
    replicate(db_url(Db), Target);

replicate(Source, {remote, Db}) ->
    replicate(Source, db_url(Db));

replicate(Source, Target) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"continuous">>, true}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepObject, ?ADMIN_ROLE),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
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
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Failed to pause source database writer"}]})
    end.

resume_writer(Pid) ->
    Ref = make_ref(),
    Pid ! {continue, Ref},
    receive
        {ok, Ref} ->
            ok
    after ?TIMEOUT_WRITER ->
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Failed to pause source database writer"}]})
    end.

get_writer_num_docs_written(Pid) ->
    Ref = make_ref(),
    Pid ! {get_count, Ref},
    receive
        {count, Ref, Count} ->
            Count
    after ?TIMEOUT_WRITER ->
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Timeout getting number of documents written"
                                " from source database writer"}]})
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
                erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Timeout stopping source database writer"}]})
            end
    after ?TIMEOUT_WRITER ->
        erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {reason, "Timeout stopping source database writer"}]})
    end.

writer_loop(#db{name = DbName}, Parent, Counter) ->
    {ok, Data} = file:read_file(?ATTFILE),
    maybe_pause(Parent, Counter),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, ?l2b(integer_to_list(Counter + 1))},
        {<<"value">>, Counter + 1},
        {<<"_attachments">>, {[
            {<<"icon1.png">>, {[
                {<<"data">>, base64:encode(Data)},
                {<<"content_type">>, <<"image/png">>}
            ]}},
            {<<"icon2.png">>, {[
                {<<"data">>, base64:encode(iolist_to_binary([Data, Data]))},
                {<<"content_type">>, <<"image/png">>}
            ]}}
        ]}}
    ]}),
    maybe_pause(Parent, Counter),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = couch_db:update_doc(Db, Doc, []),
    ok = couch_db:close(Db),
    receive
        {get_count, Ref} ->
            Parent ! {count, Ref, Counter + 1},
            writer_loop(Db, Parent, Counter + 1);
        {stop, Ref} ->
            Parent ! {stopped, Ref, Counter + 1}
    after 0 ->
        timer:sleep(?DELAY),
        writer_loop(Db, Parent, Counter + 1)
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
