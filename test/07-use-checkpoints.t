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

-record(doc, {
    id = <<"">>,
    revs = {0, []},
    body = {[]},
    atts = [],
    deleted = false,
    meta = []
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

    etap:plan(16),
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

    test_use_checkpoints(false),
    test_use_checkpoints(true),

    couch_server_sup:stop(),
    ok.


test_use_checkpoints(UseCheckpoints) ->
    Pairs = [
        {source_db_name(), target_db_name()},
        {{remote, source_db_name()}, target_db_name()},
        {source_db_name(), {remote, target_db_name()}},
        {{remote, source_db_name()}, {remote, (target_db_name())}}
    ],

    ListenerFun = case UseCheckpoints of
    false ->
        fun({finished, _, {CheckpointHistory}}) ->
            etap:is(CheckpointHistory,
            [{<<"use_checkpoints">>,false}],
            "No checkpoints found");
        (_) ->
            ok
        end;
    true ->
        fun({finished, _, {CheckpointHistory}}) ->
            SessionId = lists:keyfind(
                <<"session_id">>, 1, CheckpointHistory),
            etap:isnt(SessionId, false, "There's a checkpoint");
        (_) ->
            ok
        end
    end,
    {ok, Listener} = couch_replicator_notifier:start_link(ListenerFun),

    lists:foreach(
        fun({Source, Target}) ->
            {ok, SourceDb} = create_db(source_db_name()),
            etap:diag("Populating source database"),
            populate_db(SourceDb, 100),
            ok = couch_db:close(SourceDb),

            etap:diag("Creating target database"),
            {ok, TargetDb} = create_db(target_db_name()),
            ok = couch_db:close(TargetDb),

            etap:diag("Setup replicator notifier listener"),

            etap:diag("Triggering replication"),
            replicate(Source, Target, UseCheckpoints),

            etap:diag("Replication finished, comparing source and target databases"),
            compare_dbs(SourceDb, TargetDb),

            etap:diag("Deleting databases"),
            delete_db(TargetDb),
            delete_db(SourceDb),

            ok = timer:sleep(1000)
        end,
        Pairs),

    couch_replicator_notifier:stop(Listener).


populate_db(Db, DocCount) ->
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = iolist_to_binary(["doc", integer_to_list(DocIdCounter)]),
            Value = iolist_to_binary(["val", integer_to_list(DocIdCounter)]),
            Doc = #doc{
                id = Id,
                body = {[ {<<"value">>, Value} ]}
            },
            [Doc | Acc]
        end,
        [], lists:seq(1, DocCount)),
    {ok, _} = couch_db:update_docs(Db, Docs, []).


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


db_url(DbName) ->
    iolist_to_binary([
        "http://", config:get("httpd", "bind_address", "127.0.0.1"),
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


replicate({remote, Db}, Target, UseCheckpoints) ->
    replicate(db_url(Db), Target, UseCheckpoints);

replicate(Source, {remote, Db}, UseCheckpoints) ->
    replicate(Source, db_url(Db), UseCheckpoints);

replicate(Source, Target, UseCheckpoints) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target},
        {<<"use_checkpoints">>, UseCheckpoints}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(
        RepObject, #user_ctx{roles = [<<"_admin">>]}),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
    MonRef = erlang:monitor(process, Pid),
    receive
    {'DOWN', MonRef, process, Pid, Reason} ->
        etap:is(Reason, normal, "Replication finished successfully")
    after 300000 ->
        etap:bail("Timeout waiting for replication to finish")
    end.
