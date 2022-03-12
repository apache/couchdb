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

-module(chttpd_dbs_info_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("chttpd_test.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

start() ->
    Ctx = chttpd_test_util:start_couch(),
    DbDir = config:get("couchdb", "database_dir"),
    Suffix = ?b2l(couch_uuids:random()),
    test_util:with_couch_server_restart(fun() ->
        config:set("couchdb", "database_dir", DbDir ++ "/" ++ Suffix, false)
    end),
    mock([fabric_util, chttpd_util]),
    Ctx.

stop(Ctx) ->
    config:delete("couchdb", "database_dir", false),
    chttpd_test_util:stop_couch(Ctx).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Suffix = ?b2l(couch_uuids:random()),
    Db1 = testdb("db1", Suffix),
    Db2 = testdb("db2", Suffix),
    create_db(base_url(Db1)),
    create_db(base_url(Db2)),
    {Suffix, Db1, Db2}.

teardown({_, Db1, Db2}) ->
    meck:unload(),
    delete_db(base_url(Db1)),
    delete_db(base_url(Db2)),
    ok = config:delete("admins", ?USER, _Persist = false).

dbs_info_test_() ->
    {
        "chttpd dbs info tests",
        {
            setup,
            fun start/0,
            fun stop/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(get_db_info_should_return_db_info),
                    ?TDEF_FE(get_db_info_should_return_error_when_db_not_exist),
                    ?TDEF_FE(get_db_info_should_return_error_when_time_out),
                    ?TDEF_FE(should_return_error_for_put_dbs_info),
                    ?TDEF_FE(should_return_dbs_info_for_get_dbs_info),
                    ?TDEF_FE(should_return_nothing_when_db_not_exist_for_get_dbs_info),
                    ?TDEF_FE(should_return_500_time_out_when_time_is_not_enough_for_get_dbs_info),
                    ?TDEF_FE(should_return_db2_for_get_dbs_info_with_descending),
                    ?TDEF_FE(should_return_db1_for_get_dbs_info_with_limit_1),
                    ?TDEF_FE(should_return_db2_for_get_dbs_info_with_skip_1),
                    ?TDEF_FE(should_return_dbs_info_with_correct_start_end_key),
                    ?TDEF_FE(should_return_empty_list_with_wrong_start_end_key),
                    ?TDEF_FE(should_return_dbs_info_for_single_db),
                    ?TDEF_FE(should_return_dbs_info_for_multiple_dbs),
                    ?TDEF_FE(should_return_error_for_exceeded_keys),
                    ?TDEF_FE(should_return_error_for_missing_keys),
                    ?TDEF_FE(should_return_dbs_info_for_dbs_with_mixed_state)
                ]
            }
        }
    }.

get_db_info_should_return_db_info({_, Db1, _}) ->
    DbInfo = fabric:get_db_info(Db1),
    ?assertEqual(DbInfo, chttpd_util:get_db_info(Db1)).

get_db_info_should_return_error_when_db_not_exist(_) ->
    ?assertEqual(
        {error, database_does_not_exist},
        chttpd_util:get_db_info("db_not_exist")
    ).

get_db_info_should_return_error_when_time_out({_, Db1, _}) ->
    mock_timeout(),
    ?assertEqual({error, timeout}, chttpd_util:get_db_info(Db1)).

should_return_error_for_put_dbs_info(_) ->
    {ok, Code, _, ResultBody} = test_request:put(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH], ""
    ),
    {Body} = jiffy:decode(ResultBody),
    ?assertEqual(
        <<"method_not_allowed">>,
        couch_util:get_value(<<"error">>, Body)
    ),
    ?assertEqual(405, Code).

should_return_dbs_info_for_get_dbs_info({Suffix, Db1, Db2}) ->
    {ok, _, _, ResultBody} = test_request:get(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH]
    ),
    FilteredDbs = filter_dbs(Suffix, ResultBody),
    ?assertEqual([Db1, Db2], FilteredDbs).

should_return_nothing_when_db_not_exist_for_get_dbs_info(_) ->
    mock_db_not_exist(),
    {ok, Code, _, ResultBody} = test_request:get(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH]
    ),
    Info = jiffy:decode(ResultBody),
    ?assertEqual([], Info),
    ?assertEqual(200, Code).

should_return_500_time_out_when_time_is_not_enough_for_get_dbs_info(_) ->
    mock_timeout(),
    {ok, Code, _, ResultBody} = test_request:get(
        dbs_info_url("buffer_response=true"), [?CONTENT_JSON, ?AUTH]
    ),
    {Body} = jiffy:decode(ResultBody),
    ?assertEqual(<<"timeout">>, couch_util:get_value(<<"error">>, Body)),
    ?assertEqual(500, Code).

should_return_db2_for_get_dbs_info_with_descending({Suffix, Db1, Db2}) ->
    {ok, _, _, ResultBody} = test_request:get(
        dbs_info_url("descending=true"), [?CONTENT_JSON, ?AUTH]
    ),
    FilteredDbs = filter_dbs(Suffix, ResultBody),
    ?assertEqual([Db2, Db1], FilteredDbs).

should_return_db1_for_get_dbs_info_with_limit_1({Suffix, Db1, _}) ->
    {ok, _, _, ResultBody} = test_request:get(
        dbs_info_url("limit=1"), [?CONTENT_JSON, ?AUTH]
    ),
    FilteredDbs = filter_dbs(Suffix, ResultBody),
    ?assertEqual([Db1], FilteredDbs).

should_return_db2_for_get_dbs_info_with_skip_1({Suffix, _, Db2}) ->
    {ok, _, _, ResultBody} = test_request:get(
        dbs_info_url("skip=1"), [?CONTENT_JSON, ?AUTH]
    ),
    FilteredDbs = filter_dbs(Suffix, ResultBody),
    ?assertEqual([Db2], FilteredDbs).

should_return_dbs_info_with_correct_start_end_key({Suffix, Db1, _}) ->
    {ok, _, _, ResultBody} = test_request:get(
        dbs_info_url("startkey=\"db1\"&endkey=\"db2\""), [?CONTENT_JSON, ?AUTH]
    ),
    FilteredDbs = filter_dbs(Suffix, ResultBody),
    ?assertEqual([Db1], FilteredDbs).

should_return_empty_list_with_wrong_start_end_key(_) ->
    {ok, _, _, ResultBody} = test_request:get(
        dbs_info_url("startkey=\"db3\"&endkey=\"db4\""), [?CONTENT_JSON, ?AUTH]
    ),
    ?assertEqual([], jiffy:decode(ResultBody)).

should_return_dbs_info_for_single_db({_, Db1, _}) ->
    NewDoc = "{\"keys\": [\"" ++ Db1 ++ "\"]}",
    {ok, _, _, ResultBody} = test_request:post(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH], NewDoc
    ),
    BodyJson = jiffy:decode(ResultBody),
    {Db1Data} = lists:nth(1, BodyJson),
    ?assertEqual(?l2b(Db1), couch_util:get_value(<<"key">>, Db1Data)),
    ?assertNotEqual(undefined, couch_util:get_value(<<"info">>, Db1Data)).

should_return_dbs_info_for_multiple_dbs({_, Db1, Db2}) ->
    NewDoc = "{\"keys\": [\"" ++ Db1 ++ "\", \"" ++ Db2 ++ "\"]}",
    {ok, _, _, ResultBody} = test_request:post(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH], NewDoc
    ),
    BodyJson = jiffy:decode(ResultBody),
    {Db1Data} = lists:nth(1, BodyJson),
    {Db2Data} = lists:nth(2, BodyJson),
    ?assertEqual(?l2b(Db1), couch_util:get_value(<<"key">>, Db1Data)),
    ?assertNotEqual(undefined, couch_util:get_value(<<"info">>, Db1Data)),
    ?assertEqual(?l2b(Db2), couch_util:get_value(<<"key">>, Db2Data)),
    ?assertNotEqual(undefined, couch_util:get_value(<<"info">>, Db2Data)).

should_return_error_for_exceeded_keys({_, Db1, Db2}) ->
    NewDoc = "{\"keys\": [\"" ++ Db1 ++ "\", \"" ++ Db2 ++ "\"]}",
    ok = config:set("chttpd", "max_db_number_for_dbs_info_req", "1"),
    {ok, Code, _, ResultBody} = test_request:post(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH], NewDoc
    ),
    {Body} = jiffy:decode(ResultBody),
    ok = config:delete("chttpd", "max_db_number_for_dbs_info_req"),
    ?assertEqual(<<"bad_request">>, couch_util:get_value(<<"error">>, Body)),
    ?assertEqual(400, Code).

should_return_error_for_missing_keys({_, Db1, Db2}) ->
    NewDoc = "{\"missingkeys\": [\"" ++ Db1 ++ "\", \"" ++ Db2 ++ "\"]}",
    {ok, Code, _, ResultBody} = test_request:post(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH], NewDoc
    ),
    {Body} = jiffy:decode(ResultBody),
    ?assertEqual(<<"bad_request">>, couch_util:get_value(<<"error">>, Body)),
    ?assertEqual(400, Code).

should_return_dbs_info_for_dbs_with_mixed_state({_, Db1, _}) ->
    NewDoc = "{\"keys\": [\"" ++ Db1 ++ "\", \"noexisteddb\"]}",
    {ok, _, _, ResultBody} = test_request:post(
        dbs_info_url(), [?CONTENT_JSON, ?AUTH], NewDoc
    ),
    Json = jiffy:decode(ResultBody),
    {Db1Data} = lists:nth(1, Json),
    {Db2Data} = lists:nth(2, Json),
    ?assertEqual(?l2b(Db1), couch_util:get_value(<<"key">>, Db1Data)),
    ?assertNotEqual(undefined, couch_util:get_value(<<"info">>, Db1Data)),
    ?assertEqual(<<"noexisteddb">>, couch_util:get_value(<<"key">>, Db2Data)),
    ?assertEqual(undefined, couch_util:get_value(<<"info">>, Db2Data)).

%% Utility functions
testdb(Name, Suffix) ->
    Name ++ "-" ++ Suffix.

base_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/"]).

base_url(Path) ->
    base_url() ++ Path.

dbs_info_url() ->
    base_url() ++ "_dbs_info".

dbs_info_url(Option) ->
    dbs_info_url() ++ "?" ++ Option.

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

mock(Modules) ->
    lists:foreach(fun(Mod) -> meck:new(Mod, [passthrough]) end, Modules).

mock_timeout() ->
    meck:expect(fabric_util, request_timeout, fun() -> 0 end).

mock_db_not_exist() ->
    meck:expect(
        chttpd_util,
        get_db_info,
        fun(_) -> {error, database_does_not_exist} end
    ).

filter_dbs(Suffix, ResultBody) ->
    Dbs = jiffy:decode(ResultBody, [return_maps]),
    SuffixBin = ?l2b(Suffix),
    SuffixSize = size(SuffixBin),
    FilterFun =
        fun(Db) ->
            Name = maps:get(<<"key">>, Db),
            size(Name) > SuffixSize andalso
                binary:part(Name, size(Name), -SuffixSize) =:= SuffixBin
        end,
    [?b2l(maps:get(<<"key">>, Db)) || Db <- Dbs, FilterFun(Db)].
