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

-module(couch_db_mpr_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(USER, "couch_db_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(JSON_BODY, "{\"foo\": \"bar\"}").
-define(CONTENT_MULTI_RELATED,
        {"Content-Type", "multipart/related;boundary=\"bound\""}).


setup() ->
    ok = config:set("admins", ?USER, ?PASS, _Persist=false),
    TmpDb = ?tempdb(),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(couch_httpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    Url.


teardown(Url) ->
    catch delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist=false).


create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).


delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).


create_doc(Url, Id, Body, Type) ->
    test_request:put(Url ++ "/" ++ Id, [Type, ?AUTH], Body).


delete_doc(Url, Id, Rev) ->
    test_request:delete(Url ++ "/" ++ Id ++ "?rev=" ++ ?b2l(Rev)).


couch_db_mpr_test_() ->
    {
        "multi-part attachment tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [{with, [
                    fun recreate_with_mpr/1
                ]}]
            }
        }
    }.


recreate_with_mpr(Url) ->
    DocId1 = "foo",
    DocId2 = "bar",

    create_db(Url),
    create_and_delete_doc(Url, DocId1),
    Rev1 = create_with_mpr(Url, DocId1),
    delete_db(Url),

    create_db(Url),
    create_and_delete_doc(Url, DocId1),
    % We create a second unrelated doc to change the
    % position on disk where the attachment is written
    % so that we can assert that the position on disk
    % is not included when calculating a revision.
    create_and_delete_doc(Url, DocId2),
    Rev2 = create_with_mpr(Url, DocId1),
    delete_db(Url),

    ?assertEqual(Rev1, Rev2).


create_and_delete_doc(Url, DocId) ->
    {ok, _, _, Resp} = create_doc(Url, DocId, ?JSON_BODY, ?CONTENT_JSON),
    {Props} = ?JSON_DECODE(Resp),
    Rev = couch_util:get_value(<<"rev">>, Props, undefined),
    ?assert(is_binary(Rev)),
    {ok, _, _, _} = delete_doc(Url, DocId, Rev).


create_with_mpr(Url, DocId) ->
    {ok, _, _, Resp} = create_doc(Url, DocId, mpr(), ?CONTENT_MULTI_RELATED),
    {Props} = ?JSON_DECODE(Resp),
    Rev = couch_util:get_value(<<"rev">>, Props, undefined),
    ?assert(is_binary(Rev)),
    Rev.


mpr() ->
    lists:concat([
        "--bound\r\n",
        "Content-Type: application/json\r\n\r\n",
        "{",
            "\"body\":\"stuff\","
            "\"_attachments\":",
            "{\"foo.txt\":{",
                "\"follows\":true,",
                "\"content_type\":\"text/plain\","
                "\"length\":21",
            "}}"
        "}",
        "\r\n--bound\r\n\r\n",
        "this is 21 chars long",
        "\r\n--bound--epilogue"
    ]).
