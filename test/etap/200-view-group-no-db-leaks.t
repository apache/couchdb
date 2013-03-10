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
    compactor_pid = nil,
    instance_start_time, % number of microsecs since jan 1 1970 as a binary string
    fd,
    fd_monitor,
    header = nil,
    committed_update_seq,
    id_tree,
    seq_tree,
    local_tree,
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

test_db_name() -> <<"couch_test_view_group_db_leaks">>.
ddoc_name() -> <<"foo">>.

main(_) ->
    test_util:init_code_path(),

    etap:plan(25),
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
    put(addr, config:get("httpd", "bind_address", "127.0.0.1")),
    put(port, integer_to_list(mochiweb_socket_server:get(couch_httpd, port))),

    delete_db(),
    create_db(),

    create_docs(),
    {ok, DDocRev} = create_design_doc(),

    {ok, IndexerPid} = couch_index_server:get_index(
        couch_mrview_index, test_db_name(), <<"_design/", (ddoc_name())/binary>>
    ),
    etap:is(is_pid(IndexerPid), true, "got view group pid"),
    etap:is(is_process_alive(IndexerPid), true, "view group pid is alive"),

    query_view(3, null, false),
    check_db_monitor(),
    etap:is(is_process_alive(IndexerPid), true, "view group pid is alive"),

    create_new_doc(<<"doc1000">>),
    query_view(4, null, false),
    check_db_monitor(),
    etap:is(is_process_alive(IndexerPid), true, "view group pid is alive"),

    compact_db(),
    check_db_monitor(),
    etap:is(is_process_alive(IndexerPid), true, "view group pid is alive"),

    compact_view_group(),
    check_db_monitor(),
    etap:is(is_process_alive(IndexerPid), true, "view group pid is alive"),

    create_new_doc(<<"doc1001">>),
    query_view(5, null, false),
    check_db_monitor(),
    etap:is(is_process_alive(IndexerPid), true, "view group pid is alive"),

    etap:diag("updating the design document with a new view definition"),
    {ok, _NewDDocRev} = update_ddoc_view(DDocRev),

    {ok, NewIndexerPid} = couch_index_server:get_index(
        couch_mrview_index, test_db_name(), <<"_design/", (ddoc_name())/binary>>
    ),
    etap:is(is_pid(NewIndexerPid), true, "got new view group pid"),
    etap:is(is_process_alive(NewIndexerPid), true, "new view group pid is alive"),
    etap:isnt(NewIndexerPid, IndexerPid, "new view group has a different pid"),
    etap:diag("querying view with ?stale=ok, must return empty row set"),
    query_view(0, foo, ok),
    etap:diag("querying view (without stale), must return 5 rows with value 1"),
    query_view(5, 1, false),
    MonRef = erlang:monitor(process, IndexerPid),
    receive
    {'DOWN', MonRef, _, _, _} ->
        etap:diag("old view group is dead after ddoc update")
    after 5000 ->
        etap:bail("old view group is not dead after ddoc update")
    end,

    etap:diag("deleting database"),
    MonRef2 = erlang:monitor(process, NewIndexerPid),
    ok = couch_server:delete(test_db_name(), []),
    receive
    {'DOWN', MonRef2, _, _, _} ->
        etap:diag("new view group is dead after DB deletion")
    after 5000 ->
        etap:bail("new view group did not die after DB deletion")
    end,

    ok = timer:sleep(1000),
    delete_db(),
    couch_server_sup:stop(),
    ok.

admin_user_ctx() ->
    {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

create_db() ->
    {ok, #db{main_pid = Pid} = Db} = couch_db:create(
        test_db_name(), [admin_user_ctx()]),
    put(db_main_pid, Pid),
    ok = couch_db:close(Db).

delete_db() ->
    couch_server:delete(test_db_name(), [admin_user_ctx()]).

compact_db() ->
    {ok, Db} = couch_db:open_int(test_db_name(), []),
    {ok, _} = couch_db:start_compact(Db),
    couch_db:wait_for_compaction(Db, 5000),
    ok = couch_db:close(Db).

compact_view_group() ->
    DDoc = list_to_binary("_design/" ++ binary_to_list(ddoc_name())),
    {ok, Ref} = couch_mrview:compact(test_db_name(), DDoc, [monitor]),
    receive {'DOWN', Ref, _, _, _} ->
        ok
    after 5000 ->
        etap:bail("View group compaction failed to finish.")
    end.

check_db_monitor() ->
    {ok, #db{fd=Fd} = Db} = couch_db:open_int(test_db_name(), []),
    ok = couch_db:close(Db),
    {monitored_by, Monitors} = process_info(Fd, monitored_by),
    if length(Monitors) == 2 -> ok; true ->
        etap:diag("Monitors: ~p ~p", [self(), Monitors]),
        lists:foreach(fun(P) ->
            etap:diag("~n~n======~n~p~n-----", [P]),
            etap:diag("Stack:~n~s", [element(2, process_info(P, backtrace))])
        end, Monitors)
    end,
    etap:is(length(Monitors), 2,
        "DB fd is only monitored by couch_db_updater and couch_stats_collector"),
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
    {ok, Rev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db),
    {ok, Rev}.

update_ddoc_view(DDocRev) ->
    {ok, Db} = couch_db:open(test_db_name(), [admin_user_ctx()]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/", (ddoc_name())/binary>>},
        {<<"_rev">>, couch_doc:rev_to_str(DDocRev)},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"bar">>, {[
                {<<"map">>, <<"function(doc) { emit(doc._id, 1); }">>}
            ]}}
        ]}}
    ]}),
    {ok, NewRev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db),
    {ok, NewRev}.

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

query_view(ExpectedRowCount, ExpectedRowValue, Stale) ->
    {ok, Code, _Headers, Body} = test_util:request(
        db_url() ++ "/_design/" ++ binary_to_list(ddoc_name()) ++ "/_view/bar"
          ++ case Stale of
                 false -> [];
                 _ -> "?stale=" ++ atom_to_list(Stale)
             end,
        [{"Connection", "close"}],
        get),
    etap:is(Code, 200, "got view response"),
    {Props} = ejson:decode(Body),
    Rows = couch_util:get_value(<<"rows">>, Props, []),
    etap:is(length(Rows), ExpectedRowCount, "result set has correct # of rows"),
    lists:foreach(
        fun({Row}) ->
            case couch_util:get_value(<<"value">>, Row) of
            ExpectedRowValue ->
                ok;
            _ ->
                etap:bail("row has incorrect value")
            end
        end,
        Rows).
