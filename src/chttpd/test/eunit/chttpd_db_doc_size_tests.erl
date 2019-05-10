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

-module(chttpd_db_doc_size_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(CONTENT_MULTI_RELATED, {"Content-Type",
    "multipart/related;boundary=\"bound\""}).
-define(CONTENT_MULTI_FORM, {"Content-Type",
    "multipart/form-data;boundary=\"bound\""}).


setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist=false),
    ok = config:set("couchdb", "max_document_size", "50"),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist=false),
    ok = config:delete("couchdb", "max_document_size").

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    case Status of
        201 -> ok;
        202 -> ok;
        _ -> io:format(user, "~n HTTP Status Code: ~p~n", [Status])
    end,
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

all_test_() ->
    {
        "chttpd db max_document_size tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun post_single_doc/1,
                    fun put_single_doc/1,
                    fun bulk_doc/1,
                    fun put_post_doc_attach_inline/1,
                    fun put_multi_part_related/1,
                    fun post_multi_part_form/1
                ]
            }
        }
    }.

post_single_doc(Url) ->
    NewDoc = "{\"post_single_doc\": \"some_doc\",
        \"_id\": \"testdoc\", \"should_be\" : \"too_large\"}",
    {ok, _, _, ResultBody} = test_request:post(Url,
        [?CONTENT_JSON, ?AUTH], NewDoc),
    {[ErrorMsg | _]} = ?JSON_DECODE(ResultBody),
    ?_assertEqual({<<"error">>, <<"document_too_large">>}, ErrorMsg).

put_single_doc(Url) ->
    NewDoc = "{\"post_single_doc\": \"some_doc\",
        \"_id\": \"testdoc\", \"should_be\" : \"too_large\"}",
    {ok, _, _, ResultBody} = test_request:put(Url ++ "/" ++ "testid",
        [?CONTENT_JSON, ?AUTH], NewDoc),
    {[ErrorMsg | _]} = ?JSON_DECODE(ResultBody),
    ?_assertEqual({<<"error">>, <<"document_too_large">>}, ErrorMsg).

bulk_doc(Url) ->
    NewDoc = "{\"docs\": [{\"doc1\": 1}, {\"errordoc\":
        \"this_should_be_the_too_large_error_document\"}]}",
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_bulk_docs/",
        [?CONTENT_JSON, ?AUTH], NewDoc),
    ResultJson = ?JSON_DECODE(ResultBody),
    Expect = {[{<<"error">>,<<"document_too_large">>},{<<"reason">>,<<>>}]},
    ?_assertEqual(Expect, ResultJson).

put_post_doc_attach_inline(Url) ->
    Body1 = "{\"body\":\"This is a body.\",",
    Body2 = lists:concat(["{\"body\":\"This is a body it should fail",
        "because there are too many characters.\","]),
    DocRest =  lists:concat(["\"_attachments\":{\"foo.txt\":{",
        "\"content_type\":\"text/plain\",",
        "\"data\": \"VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ=\"}}}"]),
    Doc1 = lists:concat([Body1, DocRest]),
    Doc2 = lists:concat([Body2, DocRest]),

    {ok, _, _, ResultBody} = test_request:post(Url,
        [?CONTENT_JSON, ?AUTH], Doc1),
    {[Msg | _]} = ?JSON_DECODE(ResultBody),
    {ok, _, _, ResultBody1} = test_request:post(Url,
        [?CONTENT_JSON, ?AUTH], Doc2),
    {[Msg1 | _]} = ?JSON_DECODE(ResultBody1),

    {ok, _, _, ResultBody2} = test_request:put(Url ++ "/" ++ "accept",
        [?CONTENT_JSON, ?AUTH], Doc1),
    {[Msg2 | _]} = ?JSON_DECODE(ResultBody2),
    {ok, _, _, ResultBody3} = test_request:put(Url ++ "/" ++ "fail",
        [?CONTENT_JSON, ?AUTH], Doc2),
    {[Msg3 | _]} = ?JSON_DECODE(ResultBody3),
    [
        ?_assertEqual({<<"ok">>, true}, Msg),
        ?_assertEqual({<<"error">>, <<"document_too_large">>}, Msg1),
        ?_assertEqual({<<"ok">>, true}, Msg2),
        ?_assertEqual({<<"error">>, <<"document_too_large">>}, Msg3)
    ].

put_multi_part_related(Url) ->
    Body1 = "{\"body\":\"This is a body.\",",
    Body2 = lists:concat(["{\"body\":\"This is a body it should fail",
        "because there are too many characters.\","]),
    DocBeg = "--bound\r\nContent-Type: application/json\r\n\r\n",
    DocRest =  lists:concat(["\"_attachments\":{\"foo.txt\":{\"follows\":true,",
        "\"content_type\":\"text/plain\",\"length\":21},\"bar.txt\":",
        "{\"follows\":true,\"content_type\":\"text/plain\",",
        "\"length\":20}}}\r\n--bound\r\n\r\nthis is 21 chars long",
        "\r\n--bound\r\n\r\nthis is 20 chars lon\r\n--bound--epilogue"]),
    Doc1 = lists:concat([DocBeg, Body1, DocRest]),
    Doc2 = lists:concat([DocBeg, Body2, DocRest]),
    {ok, _, _, ResultBody} = test_request:put(Url ++ "/" ++ "accept",
        [?CONTENT_MULTI_RELATED, ?AUTH], Doc1),
    {[Msg | _]} = ?JSON_DECODE(ResultBody),
       {ok, _, _, ResultBody1} = test_request:put(Url ++ "/" ++ "faildoc",
        [?CONTENT_MULTI_RELATED, ?AUTH], Doc2),
    {[Msg1 | _]} = ?JSON_DECODE(ResultBody1),
    [
        ?_assertEqual({<<"ok">>, true}, Msg),
        ?_assertEqual({<<"error">>, <<"document_too_large">>}, Msg1)
    ].

post_multi_part_form(Url) ->
    Port = mochiweb_socket_server:get(chttpd, port),
    Host = lists:concat([ "http://127.0.0.1:", Port]),
    Referer = {"Referer", Host},
    Body1 = "{\"body\":\"This is a body.\"}",
    Body2 = lists:concat(["{\"body\":\"This is a body it should fail",
        "because there are too many characters.\"}"]),
    DocBeg = "--bound\r\nContent-Disposition: form-data; name=\"_doc\"\r\n\r\n",
    DocRest = lists:concat(["\r\n--bound\r\nContent-Disposition:",
        "form-data; name=\"_attachments\"; filename=\"file.txt\"\r\n",
        "Content-Type: text/plain\r\n\r\ncontents of file.txt\r\n\r\n",
        "--bound--"]),
    Doc1 = lists:concat([DocBeg, Body1, DocRest]),
    Doc2 = lists:concat([DocBeg, Body2, DocRest]),
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/" ++ "accept",
        [?CONTENT_MULTI_FORM, ?AUTH, Referer], Doc1),
    {[Msg | _]} = ?JSON_DECODE(ResultBody),
    {ok, _, _, ResultBody1} = test_request:post(Url ++ "/" ++ "fail",
        [?CONTENT_MULTI_FORM, ?AUTH, Referer], Doc2),
    {[Msg1 | _]} = ?JSON_DECODE(ResultBody1),
    [
        ?_assertEqual({<<"ok">>, true}, Msg),
        ?_assertEqual({<<"error">>, <<"document_too_large">>}, Msg1)
    ].
