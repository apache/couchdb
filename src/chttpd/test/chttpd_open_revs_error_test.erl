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

-module(chttpd_open_revs_error_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(CONTENT_MULTI_FORM, {"Content-Type",
    "multipart/form-data;boundary=\"bound\""}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    mock(fabric),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    (catch meck:unload(fabric)),
    ok = config:delete("admins", ?USER, _Persist=false).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


create_doc(Url, Id) ->
    test_request:put(Url ++ "/" ++ Id,
        [?CONTENT_JSON, ?AUTH], "{\"mr\": \"rockoartischocko\"}").

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

open_revs_error_test_() ->
    {
        "open revs error tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_return_503_error_for_open_revs_get/1,
                    fun should_return_503_error_for_open_revs_post_form/1
                ]
            }
        }
    }.

should_return_503_error_for_open_revs_get(Url) ->
    {ok, _, _, Body} = create_doc(Url, "testdoc"),
    {Json} = ?JSON_DECODE(Body),
    Ref = couch_util:get_value(<<"rev">>, Json, undefined),
    mock_open_revs({error, all_workers_died}),
    {ok, Code, _, _} = test_request:get(Url ++
        "/testdoc?rev=" ++ ?b2l(Ref), [?AUTH]),
     ?_assertEqual(503, Code).

should_return_503_error_for_open_revs_post_form(Url) ->
    Port = mochiweb_socket_server:get(chttpd, port),
    Host = lists:concat([ "http://127.0.0.1:", Port]),
    Referer = {"Referer", Host},
    Body1 = "{\"body\":\"This is a body.\"}",
    DocBeg = "--bound\r\nContent-Disposition: form-data; name=\"_doc\"\r\n\r\n",
    DocRev = "--bound\r\nContent-Disposition: form-data; name=\"_rev\"\r\n\r\n",
    DocRest = "\r\n--bound\r\nContent-Disposition:"
        "form-data; name=\"_attachments\"; filename=\"file.txt\"\r\n"
        "Content-Type: text/plain\r\n\r\ncontents of file.txt\r\n\r\n"
        "--bound--",
    Doc1 = lists:concat([DocBeg, Body1, DocRest]),
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/" ++ "RevDoc",
        [?CONTENT_MULTI_FORM, ?AUTH, Referer], Doc1),
    {Json} = ?JSON_DECODE(ResultBody),
    Ref = couch_util:get_value(<<"rev">>, Json, undefined),
    Doc2 = lists:concat([DocRev, ?b2l(Ref) , DocRest]),

    mock_open_revs({error, all_workers_died}),
    {ok, Code, _, ResultBody1} = test_request:post(Url ++ "/" ++ "RevDoc",
        [?CONTENT_MULTI_FORM, ?AUTH, Referer], Doc2),
    {Json1} = ?JSON_DECODE(ResultBody1),
    ErrorMessage = couch_util:get_value(<<"error">>, Json1),
    [
        ?_assertEqual(503, Code),
        ?_assertEqual(<<"service unvailable">>, ErrorMessage)
    ].

mock_open_revs(RevsResp) ->
    ok = meck:expect(fabric, open_revs, fun(_, _, _, _) -> RevsResp end).

mock(fabric) ->
    ok = meck:new(fabric, [passthrough]).
