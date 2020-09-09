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

-module(chttpd_deleted_dbs_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).


setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/"]).


teardown(_Url) ->
    ok = config:delete("couchdb", "enable_database_recovery", false),
    ok = config:delete("admins", ?USER, _Persist=false).


create_db(Url) ->
    {ok, Status, _, _} = http(put, Url, ""),
    ?assert(Status =:= 201 orelse Status =:= 202).


delete_db(Url) ->
    {ok, 200, _, _} = http(delete, Url).


deleted_dbs_test_() ->
    {
        "chttpd deleted dbs tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_return_error_for_unsupported_method/1,
                    fun should_list_deleted_dbs/1,
                    fun should_list_deleted_dbs_info/1,
                    fun should_undelete_db/1,
                    fun should_remove_deleted_db/1,
                    fun should_undelete_db_to_target_db/1,
                    fun should_not_undelete_db_to_existing_db/1
                ]
            }
        }
    }.


should_return_error_for_unsupported_method(Url) ->
    ?_test(begin
        {ok, Code, _, Body} = http(delete, mk_url(Url)),

        ?assertEqual(405, Code),
        ?assertEqual(<<"method_not_allowed">>, get_json(<<"error">>, Body))
    end).


should_list_deleted_dbs(Url) ->
    ?_test(begin
        DbName1 = create_and_delete_db(Url),
        DbName2 = create_and_delete_db(Url),
        {ok, Code, _, Body} = http(get, mk_url(Url)),
        DeletedDbs = get_db_names(Body),

        ?assertEqual(200, Code),
        ?assertEqual(true, lists:member(DbName1, DeletedDbs)),
        ?assertEqual(true, lists:member(DbName2, DeletedDbs))
    end).


should_list_deleted_dbs_info(Url) ->
    ?_test(begin
         DbName = create_and_delete_db(Url),
         {ok, _, _, Body} = http(get, mk_url(Url, DbName)),
         [{Props}] = jiffy:decode(Body),

         ?assertEqual(DbName, couch_util:get_value(<<"db_name">>, Props))
     end).


should_undelete_db(Url) ->
    ?_test(begin
        DbName = create_and_delete_db(Url),
        {ok, _, _, ResultBody} = http(get, mk_url(Url, DbName)),
        [{Props}] = jiffy:decode(ResultBody),
        TimeStamp = couch_util:get_value(<<"timestamp">>, Props),

        ErlJSON = {[
            {undelete, {[
                {source, DbName},
                {timestamp, TimeStamp}
            ]}}
        ]},

        {ok, Code1, _, _} = http(get, Url ++ DbName),
        ?assertEqual(404, Code1),

        {ok, Code2, _, _} = http(post, mk_url(Url), ErlJSON),
        ?assertEqual(200, Code2),

        {ok, Code3, _, _} = http(get, Url ++ DbName),
        ?assertEqual(200, Code3)
    end).


should_remove_deleted_db(Url) ->
    ?_test(begin
        DbName = create_and_delete_db(Url),
        {ok, _, _, Body1} = http(get, mk_url(Url, DbName)),
        [{Props}] = jiffy:decode(Body1),
        TimeStamp = couch_util:get_value(<<"timestamp">>, Props),

        {ok, Code, _, _} = http(delete, mk_url(Url, DbName, TimeStamp)),
        ?assertEqual(200, Code),

        {ok, _, _, Body2} = http(get, mk_url(Url, DbName)),
        ?assertEqual([], jiffy:decode(Body2))
    end).


should_undelete_db_to_target_db(Url) ->
    ?_test(begin
        DbName = create_and_delete_db(Url),
        {ok, _, _, Body} = http(get, mk_url(Url, DbName)),
        [{Props}] = jiffy:decode(Body),
        TimeStamp = couch_util:get_value(<<"timestamp">>, Props),

        NewDbName = ?tempdb(),
        ErlJSON = {[
            {undelete, {[
                {source, DbName},
                {timestamp, TimeStamp},
                {target, NewDbName}
            ]}}
        ]},

        {ok, Code1, _, _} = http(get, Url ++ NewDbName),
        ?assertEqual(404, Code1),

        {ok, Code2, _, _} = http(post, mk_url(Url), ErlJSON),
        ?assertEqual(200, Code2),

        {ok, Code3, _, _} = http(get, Url ++ NewDbName),
        ?assertEqual(200, Code3)
    end).


should_not_undelete_db_to_existing_db(Url) ->
    ?_test(begin
        DbName = create_and_delete_db(Url),
        {ok, _, _, ResultBody} = http(get, mk_url(Url, DbName)),
        [{Props}] = jiffy:decode(ResultBody),
        TimeStamp = couch_util:get_value(<<"timestamp">>, Props),

        NewDbName = ?tempdb(),
        create_db(Url ++ NewDbName),
        ErlJSON = {[
            {undelete, {[
                {source, DbName},
                {timestamp, TimeStamp},
                {target, NewDbName}
            ]}}
        ]},
        {ok, Code2, _, ResultBody2} = http(post, mk_url(Url), ErlJSON),
        ?assertEqual(412, Code2),
        ?assertEqual(<<"file_exists">>, get_json(<<"error">>, ResultBody2))
    end).


create_and_delete_db(BaseUrl) ->
    DbName = ?tempdb(),
    DbUrl = BaseUrl ++ DbName,
    create_db(DbUrl),
    ok = config:set("couchdb", "enable_database_recovery", "true", false),
    delete_db(DbUrl),
    DbName.


http(Verb, Url) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    test_request:Verb(Url, Headers).


http(Verb, Url, Body) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    test_request:Verb(Url, Headers, jiffy:encode(Body)).


mk_url(Url) ->
    Url ++ "/_deleted_dbs".


mk_url(Url, DbName) ->
    Url ++ "/_deleted_dbs?key=\"" ++ ?b2l(DbName) ++ "\"".


mk_url(Url, DbName, TimeStamp) ->
    Url ++ "/_deleted_dbs/" ++ ?b2l(DbName) ++ "?timestamp=\"" ++
        ?b2l(TimeStamp) ++ "\"".


get_json(Key, Body) ->
    {Props} = jiffy:decode(Body),
    couch_util:get_value(Key, Props).


get_db_names(Body) ->
    RevDbNames = lists:foldl(fun({DbInfo}, Acc) ->
        DbName = couch_util:get_value(<<"db_name">>, DbInfo),
        [DbName | Acc]
    end, [], jiffy:decode(Body)),
    lists:reverse(RevDbNames).
