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
    header,
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
    before_doc_update = nil, % nil | fun(Doc, Db) -> NewDoc
    after_doc_read = nil     % nil | fun(Doc, Db) -> NewDoc
}).

auth_db_name() -> <<"couch_test_auth_db">>.
auth_db_2_name() -> <<"couch_test_auth_db_2">>.
salt() -> <<"SALT">>.


main(_) ->
    test_util:init_code_path(),

    etap:plan(19),
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
    OrigName = couch_config:get("couch_httpd_auth", "authentication_db"),
    couch_config:set(
        "couch_httpd_auth", "authentication_db",
        binary_to_list(auth_db_name()), false),

    test_auth_db_crash(),

    couch_config:set("couch_httpd_auth", "authentication_db", OrigName, false),
    delete_db(auth_db_name()),
    delete_db(auth_db_2_name()),
    couch_server_sup:stop(),
    ok.


test_auth_db_crash() ->
    Creds0 = couch_auth_cache:get_user_creds("joe"),
    etap:is(Creds0, nil, "Got nil when getting joe's credentials"),

    etap:diag("Adding first version of Joe's user doc"),
    PasswordHash1 = hash_password("pass1"),
    {ok, Rev1} = update_user_doc(auth_db_name(), "joe", "pass1"),

    Creds1 = couch_auth_cache:get_user_creds("joe"),
    etap:is(is_list(Creds1), true, "Got joe's credentials from cache"),
    etap:is(couch_util:get_value(<<"password_sha">>, Creds1), PasswordHash1,
            "Cached credentials have the right password"),

    etap:diag("Updating Joe's user doc password"),
    PasswordHash2 = hash_password("pass2"),
    {ok, _Rev2} = update_user_doc(auth_db_name(), "joe", "pass2", Rev1),

    Creds2 = couch_auth_cache:get_user_creds("joe"),
    etap:is(is_list(Creds2), true, "Got joe's credentials from cache"),
    etap:is(couch_util:get_value(<<"password_sha">>, Creds2), PasswordHash2,
            "Cached credentials have the new password"),

    etap:diag("Shutting down the auth database process"),
    shutdown_db(auth_db_name()),

    {ok, UpdateRev} = get_doc_rev(auth_db_name(), "joe"),
    PasswordHash3 = hash_password("pass3"),
    {ok, _Rev3} = update_user_doc(auth_db_name(), "joe", "pass3", UpdateRev),

    etap:is(get_user_doc_password_sha(auth_db_name(), "joe"),
            PasswordHash3,
            "Latest Joe's doc revision has the new password hash"),

    Creds3 = couch_auth_cache:get_user_creds("joe"),
    etap:is(is_list(Creds3), true, "Got joe's credentials from cache"),
    etap:is(couch_util:get_value(<<"password_sha">>, Creds3), PasswordHash3,
            "Cached credentials have the new password"),

    etap:diag("Deleting Joe's user doc"),
    delete_user_doc(auth_db_name(), "joe"),
    Creds4 = couch_auth_cache:get_user_creds("joe"),
    etap:is(nil, Creds4,
            "Joe's credentials not found in cache after user doc was deleted"),

    etap:diag("Adding new user doc for Joe"),
    PasswordHash5 = hash_password("pass5"),
    {ok, _NewRev1} = update_user_doc(auth_db_name(), "joe", "pass5"),

    Creds5 = couch_auth_cache:get_user_creds("joe"),
    etap:is(is_list(Creds5), true, "Got joe's credentials from cache"),
    etap:is(couch_util:get_value(<<"password_sha">>, Creds5), PasswordHash5,
            "Cached credentials have the right password"),

    full_commit(auth_db_name()),

    etap:diag("Changing the auth database"),
    couch_config:set(
        "couch_httpd_auth", "authentication_db",
        binary_to_list(auth_db_2_name()), false),
    ok = timer:sleep(500),

    Creds6 = couch_auth_cache:get_user_creds("joe"),
    etap:is(nil, Creds6,
            "Joe's credentials not found in cache after auth database changed"),

    etap:diag("Adding first version of Joe's user doc to new auth database"),
    PasswordHash7 = hash_password("pass7"),
    {ok, _} = update_user_doc(auth_db_2_name(), "joe", "pass7"),

    Creds7 = couch_auth_cache:get_user_creds("joe"),
    etap:is(is_list(Creds7), true, "Got joe's credentials from cache"),
    etap:is(couch_util:get_value(<<"password_sha">>, Creds7), PasswordHash7,
            "Cached credentials have the right password"),

    etap:diag("Shutting down the auth database process"),
    shutdown_db(auth_db_2_name()),

    {ok, UpdateRev2} = get_doc_rev(auth_db_2_name(), "joe"),
    PasswordHash8 = hash_password("pass8"),
    {ok, _Rev8} = update_user_doc(auth_db_2_name(), "joe", "pass8", UpdateRev2),

    etap:is(get_user_doc_password_sha(auth_db_2_name(), "joe"),
            PasswordHash8,
            "Latest Joe's doc revision has the new password hash"),

    Creds8 = couch_auth_cache:get_user_creds("joe"),
    etap:is(is_list(Creds8), true, "Got joe's credentials from cache"),
    etap:is(couch_util:get_value(<<"password_sha">>, Creds8), PasswordHash8,
            "Cached credentials have the new password"),

    etap:diag("Changing the auth database again"),
    couch_config:set(
        "couch_httpd_auth", "authentication_db",
        binary_to_list(auth_db_name()), false),
    ok = timer:sleep(500),

    Creds9 = couch_auth_cache:get_user_creds("joe"),
    etap:is(Creds9, Creds5,
            "Got same credentials as before the firt auth database change"),
    etap:is(couch_util:get_value(<<"password_sha">>, Creds9), PasswordHash5,
            "Cached credentials have the right password"),
    ok.


update_user_doc(DbName, UserName, Password) ->
    update_user_doc(DbName, UserName, Password, nil).

update_user_doc(DbName, UserName, Password, Rev) ->
    User = iolist_to_binary(UserName),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"org.couchdb.user:", User/binary>>},
        {<<"name">>, User},
        {<<"type">>, <<"user">>},
        {<<"salt">>, salt()},
        {<<"password_sha">>, hash_password(Password)},
        {<<"roles">>, []}
    ] ++ case Rev of
        nil -> [];
        _ ->   [{<<"_rev">>, Rev}]
    end}),
    {ok, AuthDb} = open_auth_db(DbName),
    {ok, NewRev} = couch_db:update_doc(AuthDb, Doc, []),
    ok = couch_db:close(AuthDb),
    {ok, couch_doc:rev_to_str(NewRev)}.


hash_password(Password) ->
    list_to_binary(
        couch_util:to_hex(crypto:sha(iolist_to_binary([Password, salt()])))).


shutdown_db(DbName) ->
    {ok, AuthDb} = open_auth_db(DbName),
    ok = couch_db:close(AuthDb),
    couch_util:shutdown_sync(AuthDb#db.main_pid),
    ok = timer:sleep(1000).


get_doc_rev(DbName, UserName) ->
    DocId = iolist_to_binary([<<"org.couchdb.user:">>, UserName]),
    {ok, AuthDb} = open_auth_db(DbName),
    UpdateRev =
    case couch_db:open_doc(AuthDb, DocId, []) of
    {ok, Doc} ->
        {Props} = couch_doc:to_json_obj(Doc, []),
        couch_util:get_value(<<"_rev">>, Props);
    {not_found, missing} ->
        nil
    end,
    ok = couch_db:close(AuthDb),
    {ok, UpdateRev}.


get_user_doc_password_sha(DbName, UserName) ->
    DocId = iolist_to_binary([<<"org.couchdb.user:">>, UserName]),
    {ok, AuthDb} = open_auth_db(DbName),
    {ok, Doc} = couch_db:open_doc(AuthDb, DocId, []),
    ok = couch_db:close(AuthDb),
    {Props} = couch_doc:to_json_obj(Doc, []),
    couch_util:get_value(<<"password_sha">>, Props).


delete_user_doc(DbName, UserName) ->
    DocId = iolist_to_binary([<<"org.couchdb.user:">>, UserName]),
    {ok, AuthDb} = open_auth_db(DbName),
    {ok, Doc} = couch_db:open_doc(AuthDb, DocId, []),
    {Props} = couch_doc:to_json_obj(Doc, []),
    DeletedDoc = couch_doc:from_json_obj({[
        {<<"_id">>, DocId},
        {<<"_rev">>, couch_util:get_value(<<"_rev">>, Props)},
        {<<"_deleted">>, true}
    ]}),
    {ok, _} = couch_db:update_doc(AuthDb, DeletedDoc, []),
    ok = couch_db:close(AuthDb).


full_commit(DbName) ->
    {ok, AuthDb} = open_auth_db(DbName),
    {ok, _} = couch_db:ensure_full_commit(AuthDb),
    ok = couch_db:close(AuthDb).


open_auth_db(DbName) ->
    couch_db:open_int(
        DbName, [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]).


delete_db(Name) ->
    ok = couch_server:delete(
        Name, [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]).
