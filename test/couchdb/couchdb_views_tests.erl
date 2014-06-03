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

-module(couchdb_views_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(TIMEOUT, 1000).

start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    DbName = ?tempdb(),
    {ok, _} = couch_db:create(DbName, [?ADMIN_USER]),
    FooRev = create_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
    query_view(DbName, "foo", "bar"),
    BooRev = create_design_doc(DbName, <<"_design/boo">>, <<"baz">>),
    query_view(DbName, "boo", "baz"),
    {DbName, {FooRev, BooRev}}.

setup_with_docs() ->
    DbName = ?tempdb(),
    {ok, _} = couch_db:create(DbName, [?ADMIN_USER]),
    create_docs(DbName),
    create_design_doc(DbName, <<"_design/foo">>, <<"bar">>),
    DbName.

teardown({DbName, _}) ->
    teardown(DbName);
teardown(DbName) when is_binary(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_USER]),
    ok.


view_indexes_cleanup_test_() ->
    {
        "View indexes cleanup",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_have_two_indexes_alive_before_deletion/1,
                    fun should_cleanup_index_file_after_ddoc_deletion/1,
                    fun should_cleanup_all_index_files/1
                ]
            }
        }
    }.

should_not_remember_docs_in_index_after_backup_restore_test() ->
    %% COUCHDB-640
    start(),
    DbName = setup_with_docs(),

    ok = backup_db_file(DbName),
    create_doc(DbName),

    Body0 = query_view(DbName, "foo", "bar"),
    ViewJson0 = ejson:decode(Body0),
    Rows0 = couch_util:get_nested_json_value(ViewJson0, [<<"rows">>]),
    ?assert(has_doc("doc1", Rows0)),
    ?assert(has_doc("doc2", Rows0)),
    ?assert(has_doc("doc3", Rows0)),
    ?assert(has_doc("doc666", Rows0)),

    restore_backup_db_file(DbName),

    Body1 = query_view(DbName, "foo", "bar"),
    ViewJson1 = ejson:decode(Body1),
    Rows1 = couch_util:get_nested_json_value(ViewJson1, [<<"rows">>]),
    ?assert(has_doc("doc1", Rows1)),
    ?assert(has_doc("doc2", Rows1)),
    ?assert(has_doc("doc3", Rows1)),
    ?assertNot(has_doc("doc666", Rows1)),

    teardown(DbName),
    stop(whereis(couch_server_sup)).


should_have_two_indexes_alive_before_deletion({DbName, _}) ->
    view_cleanup(DbName),
    ?_assertEqual(2, count_index_files(DbName)).

should_cleanup_index_file_after_ddoc_deletion({DbName, {FooRev, _}}) ->
    delete_design_doc(DbName, <<"_design/foo">>, FooRev),
    view_cleanup(DbName),
    ?_assertEqual(1, count_index_files(DbName)).

should_cleanup_all_index_files({DbName, {FooRev, BooRev}})->
    delete_design_doc(DbName, <<"_design/foo">>, FooRev),
    delete_design_doc(DbName, <<"_design/boo">>, BooRev),
    view_cleanup(DbName),
    ?_assertEqual(0, count_index_files(DbName)).


create_doc(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    Doc666 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc666">>},
        {<<"value">>, 999}

    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc666]),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db).

create_docs(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
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

create_design_doc(DbName, DDName, ViewName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {ViewName, {[
                {<<"map">>, <<"function(doc) { emit(doc.value, 1); }">>}
            ]}}
        ]}}
    ]}),
    {ok, Rev} = couch_db:update_doc(Db, DDoc, []),
    couch_db:ensure_full_commit(Db),
    couch_db:close(Db),
    Rev.

delete_design_doc(DbName, DDName, Rev) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"_rev">>, couch_doc:rev_to_str(Rev)},
        {<<"_deleted">>, true}
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, [Rev]),
    couch_db:close(Db).

db_url(DbName) ->
    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(DbName).

query_view(DbName, DDoc, View) ->
    {ok, Code, _Headers, Body} = test_request:get(
        db_url(DbName) ++ "/_design/" ++ DDoc ++ "/_view/" ++ View),
    ?assertEqual(200, Code),
    Body.

view_cleanup(DbName) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER]),
    couch_mrview:cleanup(Db),
    couch_db:close(Db).

count_index_files(DbName) ->
    % call server to fetch the index files
    RootDir = couch_config:get("couchdb", "view_index_dir"),
    length(filelib:wildcard(RootDir ++ "/." ++
        binary_to_list(DbName) ++ "_design"++"/mrview/*")).

has_doc(DocId1, Rows) ->
    DocId = iolist_to_binary(DocId1),
    lists:any(fun({R}) -> lists:member({<<"id">>, DocId}, R) end, Rows).

backup_db_file(DbName) ->
    DbDir = couch_config:get("couchdb", "database_dir"),
    DbFile = filename:join([DbDir, ?b2l(DbName) ++ ".couch"]),
    {ok, _} = file:copy(DbFile, DbFile ++ ".backup"),
    ok.

restore_backup_db_file(DbName) ->
    DbDir = couch_config:get("couchdb", "database_dir"),
    stop(whereis(couch_server_sup)),
    DbFile = filename:join([DbDir, ?b2l(DbName) ++ ".couch"]),
    ok = file:delete(DbFile),
    ok = file:rename(DbFile ++ ".backup", DbFile),
    start(),
    ok.
