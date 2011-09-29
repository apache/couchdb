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

-define(TEST_DB, <<"etap-test-db">>).

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

-define(ADMIN_USER, #user_ctx{roles=[<<"_admin">>]}).

main(_) ->
    test_util:init_code_path(),

    etap:plan(7),
    try test() of
        ok ->
            etap:end_tests()
    catch
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            timer:sleep(1000),
            etap:bail(Other)
    end,
    ok.

test() ->

    {ok, _} = couch_server_sup:start_link(test_util:config_files()),
    couch_server:delete(?TEST_DB, []),
    timer:sleep(1000),

    couch_db:create(?TEST_DB, []),

    {ok, AllDbs} = couch_server:all_databases(),
    etap:ok(lists:member(?TEST_DB, AllDbs), "Database was created."),

    FooRev = create_design_doc(<<"_design/foo">>, <<"bar">>),
    query_view("foo", "bar"),

    BoozRev = create_design_doc(<<"_design/booz">>, <<"baz">>),
    query_view("booz", "baz"),

    {ok, Db} = couch_db:open(?TEST_DB, [{user_ctx, ?ADMIN_USER}]),
    view_cleanup(),
    etap:is(count_index_files(), 2,
        "Two index files before any deletions."),

    delete_design_doc(<<"_design/foo">>, FooRev),
    view_cleanup(),
    etap:is(count_index_files(), 1,
        "One index file after first deletion and cleanup."),

    delete_design_doc(<<"_design/booz">>, BoozRev),
    view_cleanup(),
    etap:is(count_index_files(), 0,
        "No index files after second deletion and cleanup."),

    couch_server:delete(?TEST_DB, []),
    {ok, AllDbs2} = couch_server:all_databases(),
    etap:ok(not lists:member(?TEST_DB, AllDbs2),
        "Database was deleted."),
    ok.

create_design_doc(DDName, ViewName) ->
    {ok, Db} = couch_db:open(?TEST_DB, [{user_ctx, ?ADMIN_USER}]),
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

delete_design_doc(DDName, Rev) ->
    {ok, Db} = couch_db:open(?TEST_DB, [{user_ctx, ?ADMIN_USER}]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DDName},
        {<<"_rev">>, couch_doc:rev_to_str(Rev)},
        {<<"_deleted">>, true}
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, [Rev]),
    couch_db:close(Db).

db_url() ->
    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++
        binary_to_list(?TEST_DB).

query_view(DDoc, View) ->
    {ok, Code, _Headers, _Body} = test_util:request(
        db_url() ++ "/_design/" ++ DDoc ++ "/_view/" ++ View, [], get),
    etap:is(Code, 200, "Built view index for " ++ DDoc ++ "."),
    ok.

view_cleanup() ->
    {ok, Db} = couch_db:open(?TEST_DB, [{user_ctx, ?ADMIN_USER}]),
    couch_view:cleanup_index_files(Db),
    couch_db:close(Db).

count_index_files() ->
    % call server to fetch the index files
    RootDir = couch_config:get("couchdb", "view_index_dir"),
    length(filelib:wildcard(RootDir ++ "/." ++
        binary_to_list(?TEST_DB) ++ "_design"++"/*")).
