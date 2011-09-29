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
    is_sys_db = false
}).

test_db_name() -> <<"couch_test_view_group_db_leaks">>.
ddoc_name() -> <<"foo">>.

main(_) ->
    test_util:init_code_path(),

    etap:plan(18),
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
    timer:sleep(1000),
    put(addr, couch_config:get("httpd", "bind_address", "127.0.0.1")),
    put(port, integer_to_list(mochiweb_socket_server:get(couch_httpd, port))),

    delete_db(),
    create_db(),

    create_docs(),
    create_design_doc(),

    ViewGroup = couch_view:get_group_server(
        test_db_name(), <<"_design/", (ddoc_name())/binary>>),
    etap:is(is_pid(ViewGroup), true, "got view group pid"),
    etap:is(is_process_alive(ViewGroup), true, "view group pid is alive"),

    query_view(),
    check_db_ref_count(),
    etap:is(is_process_alive(ViewGroup), true, "view group pid is alive"),

    create_new_doc(<<"doc1000">>),
    query_view(),
    check_db_ref_count(),
    etap:is(is_process_alive(ViewGroup), true, "view group pid is alive"),

    Ref1 = get_db_ref_counter(),
    compact_db(),
    check_db_ref_count(),
    Ref2 = get_db_ref_counter(),
    etap:isnt(Ref1, Ref2,  "DB ref counter changed"),
    etap:is(false, is_process_alive(Ref1), "old DB ref counter is not alive"),
    etap:is(is_process_alive(ViewGroup), true, "view group pid is alive"),

    compact_view_group(),
    check_db_ref_count(),
    Ref3 = get_db_ref_counter(),
    etap:is(Ref3, Ref2,  "DB ref counter didn't change"),
    etap:is(is_process_alive(ViewGroup), true, "view group pid is alive"),

    create_new_doc(<<"doc1001">>),
    query_view(),
    check_db_ref_count(),
    etap:is(is_process_alive(ViewGroup), true, "view group pid is alive"),

    MonRef = erlang:monitor(process, ViewGroup),
    ok = couch_server:delete(test_db_name(), []),
    receive
    {'DOWN', MonRef, _, _, _} ->
        etap:diag("view group is dead after DB deletion")
    after 5000 ->
        etap:bail("view group did not die after DB deletion")
    end,

    ok = timer:sleep(1000),
    delete_db(),
    couch_server_sup:stop(),
    ok.

admin_user_ctx() ->
    {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

create_db() ->
    {ok, Db} = couch_db:create(test_db_name(), [admin_user_ctx()]),
    ok = couch_db:close(Db).

delete_db() ->
    couch_server:delete(test_db_name(), [admin_user_ctx()]).

compact_db() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    ok = couch_db:start_compact(Db),
    ok = couch_db:close(Db),
    wait_db_compact_done(10).

wait_db_compact_done(0) ->
    etap:bail("DB compaction failed to finish.");
wait_db_compact_done(N) ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    ok = couch_db:close(Db),
    case is_pid(Db#db.compactor_pid) of
    false ->
        ok;
    true ->
        ok = timer:sleep(500),
        wait_db_compact_done(N - 1)
    end.

compact_view_group() ->
    {ok, _} = couch_view_compactor:start_compact(test_db_name(), ddoc_name()),
    wait_view_compact_done(10).

wait_view_compact_done(0) ->
    etap:bail("View group compaction failed to finish.");
wait_view_compact_done(N) ->
    {ok, Code, _Headers, Body} = test_util:request(
        db_url() ++ "/_design/" ++ binary_to_list(ddoc_name()) ++ "/_info",
        [],
        get),
    case Code of
        200 -> ok;
        _ -> etap:bail("Invalid view group info.")
    end,
    {Info} = couch_util:json_decode(Body),
    {IndexInfo} = couch_util:get_value(<<"view_index">>, Info),
    CompactRunning = couch_util:get_value(<<"compact_running">>, IndexInfo),
    case CompactRunning of
    false ->
        ok;
    true ->
        ok = timer:sleep(500),
        wait_view_compact_done(N - 1)
    end.

get_db_ref_counter() ->
    {ok, #db{fd_ref_counter = Ref} = Db} = couch_db:open_int(test_db_name(), []),
    ok = couch_db:close(Db),
    Ref.

check_db_ref_count() ->
    {ok, #db{fd_ref_counter = Ref} = Db} = couch_db:open_int(test_db_name(), []),
    ok = couch_db:close(Db),
    etap:is(couch_ref_counter:count(Ref), 2,
        "DB ref counter is only held by couch_db and couch_db_updater"),
    ok.

create_docs() ->
    {ok, Db} = couch_db:open(test_db_name(), [admin_user_ctx()]),
    Doc1 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc1">>},
        {<<"value">>, 1}
    ]}),
    Doc2 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc2">>},
        {<<"value">>, 2}

    ]}),
    Doc3 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc3">>},
        {<<"value">>, 3}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc1, Doc2, Doc3]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

create_design_doc() ->
    {ok, Db} = couch_db:open(test_db_name(), [admin_user_ctx()]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/", (ddoc_name())/binary>>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"bar">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, null); }">>}
            ]}}
        ]}}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [DDoc]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

create_new_doc(Id) ->
    {ok, Db} = couch_db:open(test_db_name(), [admin_user_ctx()]),
    Doc666 = couch_doc:from_json_obj({[
        {<<"_id">>, Id},
        {<<"value">>, 999}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc666]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

db_url() ->
    "http://" ++ get(addr) ++ ":" ++ get(port) ++ "/" ++
    binary_to_list(test_db_name()).

query_view() ->
    {ok, Code, _Headers, _Body} = test_util:request(
        db_url() ++ "/_design/" ++ binary_to_list(ddoc_name()) ++ "/_view/bar",
        [],
        get),
    etap:is(Code, 200, "got view response"),
    ok.
