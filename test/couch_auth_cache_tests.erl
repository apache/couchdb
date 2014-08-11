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

-module(couch_auth_cache_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(SALT, <<"SALT">>).
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
    couch_config:set("couch_httpd_auth", "authentication_db",
                     ?b2l(DbName), false),
    DbName.

teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_USER]),
    ok.


couch_auth_cache_test_() ->
    {
        "CouchDB auth cache tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_get_nil_on_missed_cache/1,
                    fun should_get_right_password_hash/1,
                    fun should_ensure_doc_hash_equals_cached_one/1,
                    fun should_update_password/1,
                    fun should_cleanup_cache_after_userdoc_deletion/1,
                    fun should_restore_cache_after_userdoc_recreation/1,
                    fun should_drop_cache_on_auth_db_change/1,
                    fun should_restore_cache_on_auth_db_change/1,
                    fun should_recover_cache_after_shutdown/1
                ]
            }
        }
    }.


should_get_nil_on_missed_cache(_) ->
    ?_assertEqual(nil, couch_auth_cache:get_user_creds("joe")).

should_get_right_password_hash(DbName) ->
    ?_test(begin
        PasswordHash = hash_password("pass1"),
        {ok, _} = update_user_doc(DbName, "joe", "pass1"),
        Creds = couch_auth_cache:get_user_creds("joe"),
        ?assertEqual(PasswordHash,
                      couch_util:get_value(<<"password_sha">>, Creds))
    end).

should_ensure_doc_hash_equals_cached_one(DbName) ->
    ?_test(begin
        {ok, _} = update_user_doc(DbName, "joe", "pass1"),
        Creds = couch_auth_cache:get_user_creds("joe"),

        CachedHash = couch_util:get_value(<<"password_sha">>, Creds),
        StoredHash = get_user_doc_password_sha(DbName, "joe"),
        ?assertEqual(StoredHash, CachedHash)
    end).

should_update_password(DbName) ->
    ?_test(begin
        PasswordHash = hash_password("pass2"),
        {ok, Rev} = update_user_doc(DbName, "joe", "pass1"),
        {ok, _} = update_user_doc(DbName, "joe", "pass2", Rev),
        Creds = couch_auth_cache:get_user_creds("joe"),
        ?assertEqual(PasswordHash,
                      couch_util:get_value(<<"password_sha">>, Creds))
    end).

should_cleanup_cache_after_userdoc_deletion(DbName) ->
    ?_test(begin
        {ok, _} = update_user_doc(DbName, "joe", "pass1"),
        delete_user_doc(DbName, "joe"),
        ?assertEqual(nil, couch_auth_cache:get_user_creds("joe"))
    end).

should_restore_cache_after_userdoc_recreation(DbName) ->
    ?_test(begin
        PasswordHash = hash_password("pass5"),
        {ok, _} = update_user_doc(DbName, "joe", "pass1"),
        delete_user_doc(DbName, "joe"),
        ?assertEqual(nil, couch_auth_cache:get_user_creds("joe")),

        {ok, _} = update_user_doc(DbName, "joe", "pass5"),
        Creds = couch_auth_cache:get_user_creds("joe"),

        ?assertEqual(PasswordHash,
                      couch_util:get_value(<<"password_sha">>, Creds))
    end).

should_drop_cache_on_auth_db_change(DbName) ->
    ?_test(begin
        {ok, _} = update_user_doc(DbName, "joe", "pass1"),
        full_commit(DbName),
        couch_config:set("couch_httpd_auth", "authentication_db",
                         ?b2l(?tempdb()), false),
        ?assertEqual(nil, couch_auth_cache:get_user_creds("joe"))
    end).

should_restore_cache_on_auth_db_change(DbName) ->
    ?_test(begin
        PasswordHash = hash_password("pass1"),
        {ok, _} = update_user_doc(DbName, "joe", "pass1"),
        Creds = couch_auth_cache:get_user_creds("joe"),
        full_commit(DbName),

        DbName1 = ?tempdb(),
        couch_config:set("couch_httpd_auth", "authentication_db",
                         ?b2l(DbName1), false),

        {ok, _} = update_user_doc(DbName1, "joe", "pass5"),
        full_commit(DbName1),

        couch_config:set("couch_httpd_auth", "authentication_db",
                         ?b2l(DbName), false),

        Creds = couch_auth_cache:get_user_creds("joe"),
        ?assertEqual(PasswordHash,
                      couch_util:get_value(<<"password_sha">>, Creds))
    end).

should_recover_cache_after_shutdown(DbName) ->
    ?_test(begin
        PasswordHash = hash_password("pass2"),
        {ok, Rev0} = update_user_doc(DbName, "joe", "pass1"),
        {ok, Rev1} = update_user_doc(DbName, "joe", "pass2", Rev0),
        full_commit(DbName),
        shutdown_db(DbName),
        {ok, Rev1} = get_doc_rev(DbName, "joe"),
        ?assertEqual(PasswordHash, get_user_doc_password_sha(DbName, "joe"))
    end).


update_user_doc(DbName, UserName, Password) ->
    update_user_doc(DbName, UserName, Password, nil).

update_user_doc(DbName, UserName, Password, Rev) ->
    User = iolist_to_binary(UserName),
    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"org.couchdb.user:", User/binary>>},
        {<<"name">>, User},
        {<<"type">>, <<"user">>},
        {<<"salt">>, ?SALT},
        {<<"password_sha">>, hash_password(Password)},
        {<<"roles">>, []}
    ] ++ case Rev of
            nil -> [];
            _ ->   [{<<"_rev">>, Rev}]
         end
    }),
    {ok, AuthDb} = couch_db:open_int(DbName, [?ADMIN_USER]),
    {ok, NewRev} = couch_db:update_doc(AuthDb, Doc, []),
    ok = couch_db:close(AuthDb),
    {ok, couch_doc:rev_to_str(NewRev)}.

hash_password(Password) ->
    ?l2b(couch_util:to_hex(crypto:sha(iolist_to_binary([Password, ?SALT])))).

shutdown_db(DbName) ->
    {ok, AuthDb} = couch_db:open_int(DbName, [?ADMIN_USER]),
    ok = couch_db:close(AuthDb),
    couch_util:shutdown_sync(AuthDb#db.main_pid),
    ok = timer:sleep(1000).

get_doc_rev(DbName, UserName) ->
    DocId = iolist_to_binary([<<"org.couchdb.user:">>, UserName]),
    {ok, AuthDb} = couch_db:open_int(DbName, [?ADMIN_USER]),
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
    {ok, AuthDb} = couch_db:open_int(DbName, [?ADMIN_USER]),
    {ok, Doc} = couch_db:open_doc(AuthDb, DocId, []),
    ok = couch_db:close(AuthDb),
    {Props} = couch_doc:to_json_obj(Doc, []),
    couch_util:get_value(<<"password_sha">>, Props).

delete_user_doc(DbName, UserName) ->
    DocId = iolist_to_binary([<<"org.couchdb.user:">>, UserName]),
    {ok, AuthDb} = couch_db:open_int(DbName, [?ADMIN_USER]),
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
    {ok, AuthDb} = couch_db:open_int(DbName, [?ADMIN_USER]),
    {ok, _} = couch_db:ensure_full_commit(AuthDb),
    ok = couch_db:close(AuthDb).
