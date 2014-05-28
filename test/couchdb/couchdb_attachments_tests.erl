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

-module(couchdb_attachments_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(TIMEOUT, 1000).
-define(TIMEWAIT, 100).
-define(i2l(I), integer_to_list(I)).


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
    {ok, Db} = couch_db:create(DbName, []),
    ok = couch_db:close(Db),
    Addr = couch_config:get("httpd", "bind_address", any),
    Port = mochiweb_socket_server:get(couch_httpd, port),
    Host = Addr ++ ":" ++ ?i2l(Port),
    {Host, ?b2l(DbName)}.

teardown({_, DbName}) ->
    ok = couch_server:delete(?l2b(DbName), []),
    ok.


attachments_test_() ->
    {
        "Attachments tests",
        {
            setup,
            fun start/0, fun stop/1,
            [
                attachments_md5_tests()
            ]
        }
    }.

attachments_md5_tests() ->
    {
        "Attachments MD5 tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_upload_attachment_without_md5/1,
                fun should_upload_attachment_by_chunks_without_md5/1,
                fun should_upload_attachment_with_valid_md5_header/1,
                fun should_upload_attachment_by_chunks_with_valid_md5_header/1,
                fun should_upload_attachment_by_chunks_with_valid_md5_trailer/1,
                fun should_reject_attachment_with_invalid_md5/1,
                fun should_reject_chunked_attachment_with_invalid_md5/1,
                fun should_reject_chunked_attachment_with_invalid_md5_trailer/1
            ]
        }
    }.


should_upload_attachment_without_md5({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        Body = "We all live in a yellow submarine!",
        Headers = [
            {"Content-Length", "34"},
            {"Content-Type", "text/plain"},
            {"Host", Host}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(201, Code),
        ?assertEqual(true, get_json(Json, [<<"ok">>]))
    end).

should_upload_attachment_by_chunks_without_md5({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        AttData = <<"We all live in a yellow submarine!">>,
        <<Part1:21/binary, Part2:13/binary>> = AttData,
        Body = chunked_body([Part1, Part2]),
        Headers = [
            {"Content-Type", "text/plain"},
            {"Transfer-Encoding", "chunked"},
            {"Host", Host}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(201, Code),
        ?assertEqual(true, get_json(Json, [<<"ok">>]))
    end).

should_upload_attachment_with_valid_md5_header({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        Body = "We all live in a yellow submarine!",
        Headers = [
            {"Content-Length", "34"},
            {"Content-Type", "text/plain"},
            {"Content-MD5", ?b2l(base64:encode(couch_util:md5(Body)))},
            {"Host", Host}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(201, Code),
        ?assertEqual(true, get_json(Json, [<<"ok">>]))
    end).

should_upload_attachment_by_chunks_with_valid_md5_header({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        AttData = <<"We all live in a yellow submarine!">>,
        <<Part1:21/binary, Part2:13/binary>> = AttData,
        Body = chunked_body([Part1, Part2]),
        Headers = [
            {"Content-Type", "text/plain"},
            {"Content-MD5", ?b2l(base64:encode(couch_util:md5(AttData)))},
            {"Host", Host},
            {"Transfer-Encoding", "chunked"}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(201, Code),
        ?assertEqual(true, get_json(Json, [<<"ok">>]))
    end).

should_upload_attachment_by_chunks_with_valid_md5_trailer({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        AttData = <<"We all live in a yellow submarine!">>,
        <<Part1:21/binary, Part2:13/binary>> = AttData,
        Body = [chunked_body([Part1, Part2]),
                "Content-MD5: ", base64:encode(couch_util:md5(AttData)),
                "\r\n"],
        Headers = [
            {"Content-Type", "text/plain"},
            {"Host", Host},
            {"Trailer", "Content-MD5"},
            {"Transfer-Encoding", "chunked"}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(201, Code),
        ?assertEqual(true, get_json(Json, [<<"ok">>]))
    end).

should_reject_attachment_with_invalid_md5({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        Body = "We all live in a yellow submarine!",
        Headers = [
            {"Content-Length", "34"},
            {"Content-Type", "text/plain"},
            {"Content-MD5", ?b2l(base64:encode(<<"foobar!">>))},
            {"Host", Host}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(400, Code),
        ?assertEqual(<<"content_md5_mismatch">>,
                     get_json(Json, [<<"error">>]))
    end).


should_reject_chunked_attachment_with_invalid_md5({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        AttData = <<"We all live in a yellow submarine!">>,
        <<Part1:21/binary, Part2:13/binary>> = AttData,
        Body = chunked_body([Part1, Part2]),
        Headers = [
            {"Content-Type", "text/plain"},
            {"Content-MD5", ?b2l(base64:encode(<<"foobar!">>))},
            {"Host", Host},
            {"Transfer-Encoding", "chunked"}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(400, Code),
        ?assertEqual(<<"content_md5_mismatch">>,
                     get_json(Json, [<<"error">>]))
    end).

should_reject_chunked_attachment_with_invalid_md5_trailer({Host, DbName}) ->
    ?_test(begin
        AttUrl = string:join(["", DbName, ?docid(), "readme.txt"], "/"),
        AttData = <<"We all live in a yellow submarine!">>,
        <<Part1:21/binary, Part2:13/binary>> = AttData,
        Body = [chunked_body([Part1, Part2]),
                "Content-MD5: ", base64:encode(<<"foobar!">>),
                "\r\n"],
        Headers = [
            {"Content-Type", "text/plain"},
            {"Host", Host},
            {"Trailer", "Content-MD5"},
            {"Transfer-Encoding", "chunked"}
        ],
        {ok, Code, Json} = request("PUT", AttUrl, Headers, Body),
        ?assertEqual(400, Code),
        ?assertEqual(<<"content_md5_mismatch">>,
                     get_json(Json, [<<"error">>]))
    end).


get_json(Json, Path) ->
    couch_util:get_nested_json_value(Json, Path).

to_hex(Val) ->
    to_hex(Val, []).

to_hex(0, Acc) ->
    Acc;
to_hex(Val, Acc) ->
    to_hex(Val div 16, [hex_char(Val rem 16) | Acc]).

hex_char(V) when V < 10 -> $0 + V;
hex_char(V) -> $A + V - 10.

chunked_body(Chunks) ->
    chunked_body(Chunks, []).

chunked_body([], Acc) ->
    iolist_to_binary(lists:reverse(Acc, "0\r\n"));
chunked_body([Chunk | Rest], Acc) ->
    Size = to_hex(size(Chunk)),
    chunked_body(Rest, ["\r\n", Chunk, "\r\n", Size | Acc]).

get_socket() ->
    Options = [binary, {packet, 0}, {active, false}],
    Addr = couch_config:get("httpd", "bind_address", any),
    Port = mochiweb_socket_server:get(couch_httpd, port),
    {ok, Sock} = gen_tcp:connect(Addr, Port, Options),
    Sock.

request(Method, Url, Headers, Body) ->
    RequestHead = [Method, " ", Url, " HTTP/1.1"],
    RequestHeaders = [[string:join([Key, Value], ": "), "\r\n"]
                      || {Key, Value} <- Headers],
    Request = [RequestHead, "\r\n", RequestHeaders, "\r\n", Body, "\r\n"],
    Sock = get_socket(),
    gen_tcp:send(Sock, list_to_binary(lists:flatten(Request))),
    timer:sleep(?TIMEWAIT),  % must wait to receive complete response
    {ok, R} = gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock),
    [Header, Body1] = re:split(R, "\r\n\r\n", [{return, binary}]),
    {ok, {http_response, _, Code, _}, _} =
        erlang:decode_packet(http, Header, []),
    Json = ejson:decode(Body1),
    {ok, Code, Json}.
