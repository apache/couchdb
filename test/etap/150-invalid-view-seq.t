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

test_db_name() ->
    <<"couch_test_invalid_view_seq">>.

main(_) ->
    test_util:init_code_path(),

    etap:plan(10),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

%% NOTE: since during the test we stop the server,
%%       a huge and ugly but harmless stack trace is sent to stderr
%%
test() ->
    couch_server_sup:start_link(test_util:config_files()),
    timer:sleep(1000),
    delete_db(),
    create_db(),

    create_docs(),
    create_design_doc(),

    % make DB file backup
    backup_db_file(),

    put(addr, couch_config:get("httpd", "bind_address", "127.0.0.1")),
    put(port, integer_to_list(mochiweb_socket_server:get(couch_httpd, port))),

    create_new_doc(),
    query_view_before_restore_backup(),

    % restore DB file backup after querying view
    restore_backup_db_file(),

    query_view_after_restore_backup(),

    delete_db(),
    couch_server_sup:stop(),
    ok.

admin_user_ctx() ->
    {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

create_db() ->
    {ok, _} = couch_db:create(test_db_name(), [admin_user_ctx()]).

delete_db() ->
    couch_server:delete(test_db_name(), [admin_user_ctx()]).

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
        {<<"_id">>, <<"_design/foo">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"bar">>, {[
                {<<"map">>, <<"function(doc) { emit(doc.value, 1); }">>}
            ]}}
        ]}}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [DDoc]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

backup_db_file() ->
    DbFile = test_util:build_file("tmp/lib/" ++
        binary_to_list(test_db_name()) ++ ".couch"),
    {ok, _} = file:copy(DbFile, DbFile ++ ".backup"),
    ok.

create_new_doc() ->
    {ok, Db} = couch_db:open(test_db_name(), [admin_user_ctx()]),
    Doc666 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc666">>},
        {<<"value">>, 999}

    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc666]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

db_url() ->
    "http://" ++ get(addr) ++ ":" ++ get(port) ++ "/" ++
    binary_to_list(test_db_name()).

query_view_before_restore_backup() ->
    {ok, Code, _Headers, Body} = test_util:request(
        db_url() ++ "/_design/foo/_view/bar", [], get),
    etap:is(Code, 200, "Got view response before restoring backup."),
    ViewJson = ejson:decode(Body),
    Rows = couch_util:get_nested_json_value(ViewJson, [<<"rows">>]),
    HasDoc1 = has_doc("doc1", Rows),
    HasDoc2 = has_doc("doc2", Rows),
    HasDoc3 = has_doc("doc3", Rows),
    HasDoc666 = has_doc("doc666", Rows),
    etap:is(HasDoc1, true, "Before backup restore, view has doc1"),
    etap:is(HasDoc2, true, "Before backup restore, view has doc2"),
    etap:is(HasDoc3, true, "Before backup restore, view has doc3"),
    etap:is(HasDoc666, true, "Before backup restore, view has doc666"),
    ok.

has_doc(DocId1, Rows) ->
    DocId = iolist_to_binary(DocId1),
    lists:any(
        fun({R}) -> lists:member({<<"id">>, DocId}, R) end,
        Rows
    ).

restore_backup_db_file() ->
    couch_server_sup:stop(),
    timer:sleep(3000),
    DbFile = test_util:build_file("tmp/lib/" ++
        binary_to_list(test_db_name()) ++ ".couch"),
    ok = file:delete(DbFile),
    ok = file:rename(DbFile ++ ".backup", DbFile),
    couch_server_sup:start_link(test_util:config_files()),
    timer:sleep(1000),
    put(port, integer_to_list(mochiweb_socket_server:get(couch_httpd, port))),
    ok.

query_view_after_restore_backup() ->
    {ok, Code, _Headers, Body} = test_util:request(
        db_url() ++ "/_design/foo/_view/bar", [], get),
    etap:is(Code, 200, "Got view response after restoring backup."),
    ViewJson = ejson:decode(Body),
    Rows = couch_util:get_nested_json_value(ViewJson, [<<"rows">>]),
    HasDoc1 = has_doc("doc1", Rows),
    HasDoc2 = has_doc("doc2", Rows),
    HasDoc3 = has_doc("doc3", Rows),
    HasDoc666 = has_doc("doc666", Rows),
    etap:is(HasDoc1, true, "After backup restore, view has doc1"),
    etap:is(HasDoc2, true, "After backup restore, view has doc2"),
    etap:is(HasDoc3, true, "After backup restore, view has doc3"),
    etap:is(HasDoc666, false, "After backup restore, view does not have doc666"),
    ok.
