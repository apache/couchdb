#!/usr/bin/env escript
%% -*- erlang -*-
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

% Verify that compacting databases that are being used as the source or
% target of a replication doesn't affect the replication and that the
% replication doesn't hold their reference counters forever.

-define(b2l(B), binary_to_list(B)).

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

-record(db, {
    main_pid = nil,
    update_pid = nil,
    compactor_pid = nil,
    instance_start_time, % number of microsecs since jan 1 1970 as a binary string
    fd,
    updater_fd,
    fd_ref_counter,
    header = nil,
    committed_update_seq,
    fulldocinfo_by_id_btree,
    docinfo_by_seq_btree,
    local_docs_btree,
    update_seq,
    name,
    filepath,
    validate_doc_funs = [],
    security = [],
    security_ptr = nil,
    user_ctx = #user_ctx{},
    waiting_delayed_commit = nil,
    revs_limit = 1000,
    fsync_options = [],
    options = [],
    compression,
    before_doc_update,
    after_doc_read
}).

-record(rep, {
    id,
    source,
    target,
    options,
    user_ctx,
    doc_id
}).


source_db_name() -> <<"couch_test_rep_db_a">>.
target_db_name() -> <<"couch_test_rep_db_b">>.


main(_) ->
    test_util:init_code_path(),

    etap:plan(376),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.


test() ->
    couch_server_sup:start_link(test_util:config_files()),
    ibrowse:start(),

    Pairs = [
        {source_db_name(), target_db_name()},
        {{remote, source_db_name()}, target_db_name()},
        {source_db_name(), {remote, target_db_name()}},
        {{remote, source_db_name()}, {remote, (target_db_name())}}
    ],

    lists:foreach(
        fun({Source, Target}) ->
            {ok, SourceDb} = create_db(source_db_name()),
            etap:is(couch_db:is_idle(SourceDb), true,
                "Source database is idle before starting replication"),

            {ok, TargetDb} = create_db(target_db_name()),
            etap:is(couch_db:is_idle(TargetDb), true,
                "Target database is idle before starting replication"),

            {ok, RepPid, RepId} = replicate(Source, Target),
            check_active_tasks(RepPid, RepId, Source, Target),
            {ok, DocsWritten} = populate_and_compact_test(
                RepPid, SourceDb, TargetDb),

            wait_target_in_sync(DocsWritten, TargetDb),
            check_active_tasks(RepPid, RepId, Source, Target),
            cancel_replication(RepId, RepPid),
            compare_dbs(SourceDb, TargetDb),

            delete_db(SourceDb),
            delete_db(TargetDb),
            couch_server_sup:stop(),
            ok = timer:sleep(1000),
            couch_server_sup:start_link(test_util:config_files())
        end,
        Pairs),

    couch_server_sup:stop(),
    ok.


populate_and_compact_test(RepPid, SourceDb0, TargetDb0) ->
    etap:is(is_process_alive(RepPid), true, "Replication process is alive"),
    check_db_alive("source", SourceDb0),
    check_db_alive("target", TargetDb0),

    Writer = spawn_writer(SourceDb0),

    lists:foldl(
        fun(_, {SourceDb, TargetDb, DocCount}) ->
            pause_writer(Writer),

            compact_db("source", SourceDb),
            etap:is(is_process_alive(RepPid), true,
                "Replication process is alive after source database compaction"),
            check_db_alive("source", SourceDb),
            check_ref_counter("source", SourceDb),

            compact_db("target", TargetDb),
            etap:is(is_process_alive(RepPid), true,
                "Replication process is alive after target database compaction"),
            check_db_alive("target", TargetDb),
            check_ref_counter("target", TargetDb),

            {ok, SourceDb2} = reopen_db(SourceDb),
            {ok, TargetDb2} = reopen_db(TargetDb),

            resume_writer(Writer),
            wait_writer(Writer, DocCount),

            compact_db("source", SourceDb2),
            etap:is(is_process_alive(RepPid), true,
                "Replication process is alive after source database compaction"),
            check_db_alive("source", SourceDb2),
            pause_writer(Writer),
            check_ref_counter("source", SourceDb2),
            resume_writer(Writer),

            compact_db("target", TargetDb2),
            etap:is(is_process_alive(RepPid), true,
                "Replication process is alive after target database compaction"),
            check_db_alive("target", TargetDb2),
            pause_writer(Writer),
            check_ref_counter("target", TargetDb2),
            resume_writer(Writer),

            {ok, SourceDb3} = reopen_db(SourceDb2),
            {ok, TargetDb3} = reopen_db(TargetDb2),
            {SourceDb3, TargetDb3, DocCount + 50}
        end,
        {SourceDb0, TargetDb0, 50}, lists:seq(1, 5)),

    DocsWritten = stop_writer(Writer),
    {ok, DocsWritten}.


check_db_alive(Type, #db{main_pid = Pid}) ->
    etap:is(is_process_alive(Pid), true,
        "Local " ++ Type ++ " database main pid is alive").


compact_db(Type, #db{name = Name}) ->
    {ok, Db} = couch_db:open_int(Name, []),
    {ok, CompactPid} = couch_db:start_compact(Db),
    MonRef = erlang:monitor(process, CompactPid),
    receive
    {'DOWN', MonRef, process, CompactPid, normal} ->
        ok;
    {'DOWN', MonRef, process, CompactPid, Reason} ->
        etap:bail("Error compacting " ++ Type ++ " database " ++ ?b2l(Name) ++
            ": " ++ couch_util:to_list(Reason))
    after 30000 ->
        etap:bail("Compaction for " ++ Type ++ " database " ++ ?b2l(Name) ++
            " didn't finish")
    end,
    ok = couch_db:close(Db).


check_ref_counter(Type, #db{name = Name, fd_ref_counter = OldRefCounter}) ->
    MonRef = erlang:monitor(process, OldRefCounter),
    receive
    {'DOWN', MonRef, process, OldRefCounter, _} ->
        etap:diag("Old " ++ Type ++ " database ref counter terminated")
    after 30000 ->
        etap:bail("Old " ++ Type ++ " database ref counter didn't terminate")
    end,
    {ok, #db{fd_ref_counter = NewRefCounter} = Db} = couch_db:open_int(Name, []),
    ok = couch_db:close(Db),
    etap:isnt(
        NewRefCounter, OldRefCounter, Type ++ " database has new ref counter").


reopen_db(#db{name = Name}) ->
    {ok, Db} = couch_db:open_int(Name, []),
    ok = couch_db:close(Db),
    {ok, Db}.


wait_target_in_sync(DocCount, #db{name = TargetName}) ->
    wait_target_in_sync_loop(DocCount, TargetName, 300).


wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    etap:bail("Could not get source and target databases in sync");
wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft) ->
    {ok, Target} = couch_db:open_int(TargetName, []),
    {ok, TargetInfo} = couch_db:get_db_info(Target),
    ok = couch_db:close(Target),
    TargetDocCount = couch_util:get_value(doc_count, TargetInfo),
    case TargetDocCount == DocCount of
    true ->
        etap:diag("Source and target databases are in sync");
    false ->
        ok = timer:sleep(100),
        wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft - 1)
    end.


compare_dbs(#db{name = SourceName}, #db{name = TargetName}) ->
    {ok, SourceDb} = couch_db:open_int(SourceName, []),
    {ok, TargetDb} = couch_db:open_int(TargetName, []),
    Fun = fun(FullDocInfo, _, Acc) ->
        {ok, Doc} = couch_db:open_doc(SourceDb, FullDocInfo),
        {Props} = DocJson = couch_doc:to_json_obj(Doc, [attachments]),
        DocId = couch_util:get_value(<<"_id">>, Props),
        DocTarget = case couch_db:open_doc(TargetDb, DocId) of
        {ok, DocT} ->
            DocT;
        Error ->
            etap:bail("Error opening document '" ++ ?b2l(DocId) ++
                "' from target: " ++ couch_util:to_list(Error))
        end,
        DocTargetJson = couch_doc:to_json_obj(DocTarget, [attachments]),
        case DocTargetJson of
        DocJson ->
            ok;
        _ ->
            etap:bail("Content from document '" ++ ?b2l(DocId) ++
                "' differs in target database")
        end,
        {ok, Acc}
    end,
    {ok, _, _} = couch_db:enum_docs(SourceDb, Fun, [], []),
    etap:diag("Target database has the same documents as the source database"),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb).


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
    FullRepId = list_to_binary(BaseId ++ Ext),
    Pid = list_to_binary(pid_to_list(RepPid)),
    [RepTask] = couch_task_status:all(),
    etap:is(couch_util:get_value(pid, RepTask), Pid,
        "_active_tasks entry has correct pid property"),
    etap:is(couch_util:get_value(replication_id, RepTask), FullRepId,
        "_active_tasks entry has right replication id"),
    etap:is(couch_util:get_value(continuous, RepTask), true,
        "_active_tasks entry has continuous property set to true"),
    etap:is(couch_util:get_value(source, RepTask), Source,
        "_active_tasks entry has correct source property"),
    etap:is(couch_util:get_value(target, RepTask), Target,
        "_active_tasks entry has correct target property"),
    etap:is(is_integer(couch_util:get_value(docs_read, RepTask)), true,
        "_active_tasks entry has integer docs_read property"),
    etap:is(is_integer(couch_util:get_value(docs_written, RepTask)), true,
        "_active_tasks entry has integer docs_written property"),
    etap:is(is_integer(couch_util:get_value(doc_write_failures, RepTask)), true,
        "_active_tasks entry has integer doc_write_failures property"),
    etap:is(is_integer(couch_util:get_value(revisions_checked, RepTask)), true,
        "_active_tasks entry has integer revisions_checked property"),
    etap:is(is_integer(couch_util:get_value(missing_revisions_found, RepTask)), true,
        "_active_tasks entry has integer missing_revisions_found property"),
    etap:is(is_integer(couch_util:get_value(checkpointed_source_seq, RepTask)), true,
        "_active_tasks entry has integer checkpointed_source_seq property"),
    etap:is(is_integer(couch_util:get_value(source_seq, RepTask)), true,
        "_active_tasks entry has integer source_seq property"),
    Progress = couch_util:get_value(progress, RepTask),
    etap:is(is_integer(Progress), true,
        "_active_tasks entry has an integer progress property"),
    etap:is(Progress =< 100, true, "Progress is not greater than 100%").


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
    etap:diag("Started source database writer"),
    Pid.


pause_writer(Pid) ->
    Ref = make_ref(),
    Pid ! {pause, Ref},
    receive
    {paused, Ref} ->
        ok
    after 30000 ->
        etap:bail("Failed to pause source database writer")
    end.


resume_writer(Pid) ->
    Ref = make_ref(),
    Pid ! {continue, Ref},
    receive
    {ok, Ref} ->
        ok
    after 30000 ->
        etap:bail("Failed to unpause source database writer")
    end.


get_writer_num_docs_written(Pid) ->
    Ref = make_ref(),
    Pid ! {get_count, Ref},
    receive
    {count, Ref, Count} ->
        Count
    after 30000 ->
        etap:bail("Timeout getting number of documents written from "
            "source database writer")
    end.


stop_writer(Pid) ->
    Ref = make_ref(),
    Pid ! {stop, Ref},
    receive
    {stopped, Ref, DocsWritten} ->
        MonRef = erlang:monitor(process, Pid),
        receive
        {'DOWN', MonRef, process, Pid, _Reason} ->
            etap:diag("Stopped source database writer"),
            DocsWritten
        after 30000 ->
            etap:bail("Timeout stopping source database writer")
        end
    after 30000 ->
        etap:bail("Timeout stopping source database writer")
    end.


writer_loop(#db{name = DbName}, Parent, Counter) ->
    maybe_pause(Parent, Counter),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Counter + 1))},
        {<<"value">>, Counter + 1},
        {<<"_attachments">>, {[
            {<<"icon1.png">>, {[
                {<<"data">>, base64:encode(att_data())},
                {<<"content_type">>, <<"image/png">>}
            ]}},
            {<<"icon2.png">>, {[
                {<<"data">>, base64:encode(iolist_to_binary(
                    [att_data(), att_data()]))},
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
        ok = timer:sleep(500),
        writer_loop(Db, Parent, Counter + 1)
    end.


maybe_pause(Parent, Counter) ->
    receive
    {get_count, Ref} ->
        Parent ! {count, Ref, Counter};
    {pause, Ref} ->
        Parent ! {paused, Ref},
        receive {continue, Ref2} -> Parent ! {ok, Ref2} end
    after 0 ->
        ok
    end.


db_url(DbName) ->
    iolist_to_binary([
        "http://", couch_config:get("httpd", "bind_address", "127.0.0.1"),
        ":", integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
        "/", DbName
    ]).


create_db(DbName) ->
    {ok, Db} = couch_db:create(
        DbName,
        [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}, overwrite]),
    couch_db:close(Db),
    {ok, Db}.


delete_db(#db{name = DbName, main_pid = Pid}) ->
    ok = couch_server:delete(
        DbName, [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]),
    MonRef = erlang:monitor(process, Pid),
    receive
    {'DOWN', MonRef, process, Pid, _Reason} ->
        ok
    after 30000 ->
        etap:bail("Timeout deleting database")
    end.


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
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(
        RepObject, #user_ctx{roles = [<<"_admin">>]}),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
    {ok, Pid, Rep#rep.id}.


cancel_replication(RepId, RepPid) ->
    {ok, _} = couch_replicator:cancel_replication(RepId),
    etap:is(is_process_alive(RepPid), false,
        "Replication process is no longer alive after cancel").


att_data() ->
    {ok, Data} = file:read_file(
        test_util:source_file("share/www/image/logo.png")),
    Data.
