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

main_db_name() -> <<"couch_test_view_group_shutdown">>.


main(_) ->
    test_util:init_code_path(),

    etap:plan(17),
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
    ok = couch_config:set("couchdb", "max_dbs_open", "3", false),
    ok = couch_config:set("couchdb", "delayed_commits", "false", false),
    crypto:start(),

    % Test that while a view group is being compacted its database can not
    % be closed by the database LRU system.
    test_view_group_compaction(),

    couch_server_sup:stop(),
    ok.


test_view_group_compaction() ->
    {ok, DbWriter3} = create_db(<<"couch_test_view_group_shutdown_w3">>),
    ok = couch_db:close(DbWriter3),

    {ok, MainDb} = create_main_db(),
    ok = couch_db:close(MainDb),

    {ok, DbWriter1} = create_db(<<"couch_test_view_group_shutdown_w1">>),
    ok = couch_db:close(DbWriter1),

    {ok, DbWriter2} = create_db(<<"couch_test_view_group_shutdown_w2">>),
    ok = couch_db:close(DbWriter2),

    Writer1 = spawn_writer(DbWriter1#db.name),
    Writer2 = spawn_writer(DbWriter2#db.name),
    etap:is(is_process_alive(Writer1), true, "Spawned writer 1"),
    etap:is(is_process_alive(Writer2), true, "Spawned writer 2"),

    etap:is(get_writer_status(Writer1), ok, "Writer 1 opened his database"),
    etap:is(get_writer_status(Writer2), ok, "Writer 2 opened his database"),

    {ok, CompactPid} = couch_view_compactor:start_compact(
        MainDb#db.name, <<"foo">>),
    MonRef = erlang:monitor(process, CompactPid),

    % Add some more docs to database and trigger view update
    {ok, MainDb2} = couch_db:open_int(MainDb#db.name, []),
    ok = populate_main_db(MainDb2, 3, 3),
    update_view(MainDb2#db.name, <<"_design/foo">>, <<"foo">>),
    ok = couch_db:close(MainDb2),

    % Assuming the view compaction takes more than 50ms to complete
    ok = timer:sleep(50),
    Writer3 = spawn_writer(DbWriter3#db.name),
    etap:is(is_process_alive(Writer3), true, "Spawned writer 3"),

    etap:is(get_writer_status(Writer3), {error, all_dbs_active},
        "Writer 3 got {error, all_dbs_active} when opening his database"),

    etap:is(is_process_alive(Writer1), true, "Writer 1 still alive"),
    etap:is(is_process_alive(Writer2), true, "Writer 2 still alive"),
    etap:is(is_process_alive(Writer3), true, "Writer 3 still alive"),

    receive
    {'DOWN', MonRef, process, CompactPid, normal} ->
         etap:diag("View group compaction successful"),
         ok;
    {'DOWN', MonRef, process, CompactPid, _Reason} ->
         etap:bail("Failure compacting view group")
    end,

    ok = timer:sleep(2000),

    etap:is(writer_try_again(Writer3), ok,
        "Told writer 3 to try open his database again"),
    etap:is(get_writer_status(Writer3), ok,
        "Writer 3 was able to open his database"),

    etap:is(is_process_alive(Writer1), true, "Writer 1 still alive"),
    etap:is(is_process_alive(Writer2), true, "Writer 2 still alive"),
    etap:is(is_process_alive(Writer3), true, "Writer 3 still alive"),

    etap:is(stop_writer(Writer1), ok, "Stopped writer 1"),
    etap:is(stop_writer(Writer2), ok, "Stopped writer 2"),
    etap:is(stop_writer(Writer3), ok, "Stopped writer 3"),

    delete_db(MainDb),
    delete_db(DbWriter1),
    delete_db(DbWriter2),
    delete_db(DbWriter3).


create_main_db() ->
    {ok, Db} = create_db(main_db_name()),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/foo">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"foo">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo2">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo3">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo4">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}},
            {<<"foo5">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, doc); }">>}
            ]}}
        ]}}
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, []),
    ok = populate_main_db(Db, 1000, 20000),
    update_view(Db#db.name, <<"_design/foo">>, <<"foo">>),
    {ok, Db}.


populate_main_db(Db, BatchSize, N) when N > 0 ->
    Docs = lists:map(
        fun(_) ->
            couch_doc:from_json_obj({[
                {<<"_id">>, couch_uuids:new()},
                {<<"value">>, base64:encode(crypto:rand_bytes(1000))}
            ]})
        end,
        lists:seq(1, BatchSize)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    populate_main_db(Db, BatchSize, N - length(Docs));
populate_main_db(_Db, _, _) ->
    ok.


update_view(DbName, DDocName, ViewName) ->
    % Use a dedicated process -  we can't explicitly drop the #group ref counter
    Pid = spawn(fun() ->
        {ok, Db} = couch_db:open_int(DbName, []),
        couch_view:get_map_view(Db, DDocName, ViewName, false),
        ok = couch_db:close(Db)
    end),
    MonRef = erlang:monitor(process, Pid),
    receive
    {'DOWN', MonRef, process, Pid, normal} ->
         etap:diag("View group updated"),
         ok;
    {'DOWN', MonRef, process, Pid, _Reason} ->
         etap:bail("Failure updating view group")
    end.

create_db(DbName) ->
    {ok, Db} = couch_db:create(
        DbName,
        [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}, overwrite]),
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


spawn_writer(DbName) ->
    Parent = self(),
    spawn(fun() ->
        process_flag(priority, high),
        writer_loop(DbName, Parent)
    end).


get_writer_status(Writer) ->
    Ref = make_ref(),
    Writer ! {get_status, Ref},
    receive
    {db_open, Ref} ->
        ok;
    {db_open_error, Error, Ref} ->
        Error
    after 5000 ->
        timeout
    end.


writer_try_again(Writer) ->
    Ref = make_ref(),
    Writer ! {try_again, Ref},
    receive
    {ok, Ref} ->
        ok
    after 5000 ->
        timeout
    end.


stop_writer(Writer) ->
    Ref = make_ref(),
    Writer ! {stop, Ref},
    receive
    {ok, Ref} ->
        ok
    after 5000 ->
        etap:bail("Timeout stopping writer process")
    end.


% Just keep the database open, no need to actually do something on it.
writer_loop(DbName, Parent) ->
    case couch_db:open_int(DbName, []) of
    {ok, Db} ->
        writer_loop_1(Db, Parent);
    Error ->
        writer_loop_2(DbName, Parent, Error)
    end.

writer_loop_1(Db, Parent) ->
    receive
    {get_status, Ref} ->
        Parent ! {db_open, Ref},
        writer_loop_1(Db, Parent);
    {stop, Ref} ->
        ok = couch_db:close(Db),
        Parent ! {ok, Ref}
    end.

writer_loop_2(DbName, Parent, Error) ->
    receive
    {get_status, Ref} ->
        Parent ! {db_open_error, Error, Ref},
        writer_loop_2(DbName, Parent, Error);
    {try_again, Ref} ->
        Parent ! {ok, Ref},
        writer_loop(DbName, Parent)
    end.
