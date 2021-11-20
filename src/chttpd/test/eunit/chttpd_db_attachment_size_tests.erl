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

-module(chttpd_db_attachment_size_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_att_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(CONTENT_MULTI_RELATED, {"Content-Type", "multipart/related;boundary=\"bound\""}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    ok = config:set("couchdb", "max_attachment_size", "50", _Persist = false),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    Url = "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(TmpDb),
    create_db(Url),
    add_doc(Url, "doc1"),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist = false),
    ok = config:delete("couchdb", "max_attachment_size").

attachment_size_test_() ->
    {
        "chttpd max_attachment_size tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun put_inline/1,
                    fun put_simple/1,
                    fun put_simple_chunked/1,
                    fun put_mp_related/1,
                    fun put_chunked_mp_related/1
                ]
            }
        }
    }.

put_inline(Url) ->
    ?_test(begin
        Status = put_inline(Url, "doc2", 50),
        ?assert(Status =:= 201 orelse Status =:= 202),
        ?assertEqual(413, put_inline(Url, "doc3", 51))
    end).

put_simple(Url) ->
    ?_test(begin
        Headers = [{"Content-Type", "app/binary"}],
        Rev1 = doc_rev(Url, "doc1"),
        Data1 = data(50),
        Status1 = put_req(Url ++ "/doc1/att2?rev=" ++ Rev1, Headers, Data1),
        ?assert(Status1 =:= 201 orelse Status1 =:= 202),
        Data2 = data(51),
        Rev2 = doc_rev(Url, "doc1"),
        Status2 = put_req(Url ++ "/doc1/att3?rev=" ++ Rev2, Headers, Data2),
        ?assertEqual(413, Status2)
    end).

put_simple_chunked(Url) ->
    ?_test(begin
        Headers = [{"Content-Type", "app/binary"}],
        Rev1 = doc_rev(Url, "doc1"),
        DataFun1 = data_stream_fun(50),
        Status1 = put_req_chunked(Url ++ "/doc1/att2?rev=" ++ Rev1, Headers, DataFun1),
        ?assert(Status1 =:= 201 orelse Status1 =:= 202),
        DataFun2 = data_stream_fun(51),
        Rev2 = doc_rev(Url, "doc1"),
        Status2 = put_req_chunked(Url ++ "/doc1/att3?rev=" ++ Rev2, Headers, DataFun2),
        ?assertEqual(413, Status2)
    end).

put_mp_related(Url) ->
    ?_test(begin
        Headers = [?CONTENT_MULTI_RELATED],
        Body1 = mp_body(50),
        Status1 = put_req(Url ++ "/doc2", Headers, Body1),
        ?assert(Status1 =:= 201 orelse Status1 =:= 202),
        Body2 = mp_body(51),
        Status2 = put_req(Url ++ "/doc3", Headers, Body2),
        ?assertEqual(413, Status2)
    end).

put_chunked_mp_related(Url) ->
    ?_test(begin
        Headers = [?CONTENT_MULTI_RELATED],
        Body = mp_body(50),
        Status = put_req_chunked(Url ++ "/doc4", Headers, Body),
        ?assert(Status =:= 201 orelse Status =:= 202)
    end).

% Helper functions

create_db(Url) ->
    Status = put_req(Url, "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

add_doc(Url, DocId) ->
    Status = put_req(Url ++ "/" ++ DocId, "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

put_inline(Url, DocId, Size) ->
    Doc =
        "{\"_attachments\": {\"att1\":{"
        "\"content_type\": \"app/binary\", "
        "\"data\": \"" ++ data_b64(Size) ++
            "\""
            "}}}",
    put_req(Url ++ "/" ++ DocId, Doc).

mp_body(AttSize) ->
    AttData = data(AttSize),
    SizeStr = integer_to_list(AttSize),
    string:join(
        [
            "--bound",

            "Content-Type: application/json",

            "",

            "{\"_id\":\"doc2\", \"_attachments\":{\"att\":"
            "{\"content_type\":\"app/binary\", \"length\":" ++ SizeStr ++
                ","
                "\"follows\":true}}}",

            "--bound",

            "Content-Disposition: attachment; filename=\"att\"",

            "Content-Type: app/binary",

            "",

            AttData,

            "--bound--"
        ],
        "\r\n"
    ).

doc_rev(Url, DocId) ->
    {200, ResultProps} = get_req(Url ++ "/" ++ DocId),
    {<<"_rev">>, BinRev} = lists:keyfind(<<"_rev">>, 1, ResultProps),
    binary_to_list(BinRev).

put_req(Url, Body) ->
    put_req(Url, [], Body).

put_req(Url, Headers, Body) ->
    {ok, Status, _, _} = test_request:put(Url, Headers ++ [?AUTH], Body),
    Status.

put_req_chunked(Url, Headers, Body) ->
    Opts = [{transfer_encoding, {chunked, 1}}],
    {ok, Status, _, _} = test_request:put(Url, Headers ++ [?AUTH], Body, Opts),
    Status.

get_req(Url) ->
    {ok, Status, _, ResultBody} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
    {[_ | _] = ResultProps} = ?JSON_DECODE(ResultBody),
    {Status, ResultProps}.

% Data streaming generator for ibrowse client. ibrowse will repeatedly call the
% function with State and it should return {ok, Data, NewState} or eof at end.
data_stream_fun(Size) ->
    Fun = fun
        (0) -> eof;
        (BytesLeft) -> {ok, <<"x">>, BytesLeft - 1}
    end,
    {Fun, Size}.

data(Size) ->
    string:copies("x", Size).

data_b64(Size) ->
    base64:encode_to_string(data(Size)).
