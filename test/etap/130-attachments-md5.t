#!/usr/bin/env escript
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

test_db_name() ->
    <<"etap-test-db">>.

docid() ->
    case get(docid) of
        undefined ->
            put(docid, 1),
            "1";
        Count ->
            put(docid, Count+1),
            integer_to_list(Count+1)
    end.

main(_) ->
    test_util:init_code_path(),
    
    etap:plan(16),
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
    Addr = couch_config:get("httpd", "bind_address", any),
    put(addr, Addr),
    put(port, mochiweb_socket_server:get(couch_httpd, port)),
    timer:sleep(1000),

    couch_server:delete(test_db_name(), []),
    couch_db:create(test_db_name(), []),

    test_identity_without_md5(),
    test_chunked_without_md5(),

    test_identity_with_valid_md5(),
    test_chunked_with_valid_md5_header(),
    test_chunked_with_valid_md5_trailer(),

    test_identity_with_invalid_md5(),
    test_chunked_with_invalid_md5_header(),
    test_chunked_with_invalid_md5_trailer(),

    couch_server:delete(test_db_name(), []),
    couch_server_sup:stop(),
    ok.

test_identity_without_md5() ->
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Content-Length: 34\r\n",
        "\r\n",
        "We all live in a yellow submarine!"],

    {Code, Json} = do_request(Data),
    etap:is(Code, 201, "Stored with identity encoding and no MD5"),
    etap:is(get_json(Json, [<<"ok">>]), true, "Body indicates success.").

test_chunked_without_md5() ->
    AttData = <<"We all live in a yellow submarine!">>,
    <<Part1:21/binary, Part2:13/binary>> = AttData,
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Transfer-Encoding: chunked\r\n",
        "\r\n",
        to_hex(size(Part1)), "\r\n",
        Part1, "\r\n",
        to_hex(size(Part2)), "\r\n",
        Part2, "\r\n"
        "0\r\n"
        "\r\n"],

    {Code, Json} = do_request(Data),
    etap:is(Code, 201, "Stored with chunked encoding and no MD5"),
    etap:is(get_json(Json, [<<"ok">>]), true, "Body indicates success.").

test_identity_with_valid_md5() ->
    AttData = "We all live in a yellow submarine!",
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Content-Length: 34\r\n",
        "Content-MD5: ", base64:encode(couch_util:md5(AttData)), "\r\n",
        "\r\n",
        AttData],

    {Code, Json} = do_request(Data),
    etap:is(Code, 201, "Stored with identity encoding and valid MD5"),
    etap:is(get_json(Json, [<<"ok">>]), true, "Body indicates success.").

test_chunked_with_valid_md5_header() ->
    AttData = <<"We all live in a yellow submarine!">>,
    <<Part1:21/binary, Part2:13/binary>> = AttData,
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Transfer-Encoding: chunked\r\n",
        "Content-MD5: ", base64:encode(couch_util:md5(AttData)), "\r\n",
        "\r\n",
        to_hex(size(Part1)), "\r\n",
        Part1, "\r\n",
        to_hex(size(Part2)), "\r\n",
        Part2, "\r\n",
        "0\r\n",
        "\r\n"],

    {Code, Json} = do_request(Data),
    etap:is(Code, 201, "Stored with chunked encoding and valid MD5 header."),
    etap:is(get_json(Json, [<<"ok">>]), true, "Body indicates success.").

test_chunked_with_valid_md5_trailer() ->
    AttData = <<"We all live in a yellow submarine!">>,
    <<Part1:21/binary, Part2:13/binary>> = AttData,
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Transfer-Encoding: chunked\r\n",
        "Trailer: Content-MD5\r\n",
        "\r\n",
        to_hex(size(Part1)), "\r\n",
        Part1, "\r\n",
        to_hex(size(Part2)), "\r\n",
        Part2, "\r\n",
        "0\r\n",
        "Content-MD5: ", base64:encode(couch_util:md5(AttData)), "\r\n",
        "\r\n"],

    {Code, Json} = do_request(Data),
    etap:is(Code, 201, "Stored with chunked encoding and valid MD5 trailer."),
    etap:is(get_json(Json, [<<"ok">>]), true, "Body indicates success.").

test_identity_with_invalid_md5() ->
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Content-Length: 34\r\n",
        "Content-MD5: ", base64:encode(<<"foobar!">>), "\r\n",
        "\r\n",
        "We all live in a yellow submarine!"],

    {Code, Json} = do_request(Data),
    etap:is(Code, 400, "Invalid MD5 header causes an error: identity"),
    etap:is(
        get_json(Json, [<<"error">>]),
        <<"content_md5_mismatch">>,
        "Body indicates reason for failure."
    ).

test_chunked_with_invalid_md5_header() ->
    AttData = <<"We all live in a yellow submarine!">>,
    <<Part1:21/binary, Part2:13/binary>> = AttData,
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Transfer-Encoding: chunked\r\n",
        "Content-MD5: ", base64:encode(<<"so sneaky...">>), "\r\n",
        "\r\n",
        to_hex(size(Part1)), "\r\n",
        Part1, "\r\n",
        to_hex(size(Part2)), "\r\n",
        Part2, "\r\n",
        "0\r\n",
        "\r\n"],

    {Code, Json} = do_request(Data),
    etap:is(Code, 400, "Invalid MD5 header causes an error: chunked"),
    etap:is(
        get_json(Json, [<<"error">>]),
        <<"content_md5_mismatch">>,
        "Body indicates reason for failure."
    ).

test_chunked_with_invalid_md5_trailer() ->
    AttData = <<"We all live in a yellow submarine!">>,
    <<Part1:21/binary, Part2:13/binary>> = AttData,
    Data = [
        "PUT /", test_db_name(), "/", docid(), "/readme.txt HTTP/1.1\r\n",
        "Content-Type: text/plain\r\n",
        "Transfer-Encoding: chunked\r\n",
        "Trailer: Content-MD5\r\n",
        "\r\n",
        to_hex(size(Part1)), "\r\n",
        Part1, "\r\n",
        to_hex(size(Part2)), "\r\n",
        Part2, "\r\n",
        "0\r\n",
        "Content-MD5: ", base64:encode(<<"Kool-Aid Fountain!">>), "\r\n",
        "\r\n"],

    {Code, Json} = do_request(Data),
    etap:is(Code, 400, "Invalid MD5 Trailer causes an error"),
    etap:is(
        get_json(Json, [<<"error">>]),
        <<"content_md5_mismatch">>,
        "Body indicates reason for failure."
    ).


get_socket() ->
    Options = [binary, {packet, 0}, {active, false}],
    {ok, Sock} = gen_tcp:connect(get(addr), get(port), Options),
    Sock.

do_request(Request) ->
    Sock = get_socket(),
    gen_tcp:send(Sock, list_to_binary(lists:flatten(Request))),
    timer:sleep(1000),
    {ok, R} = gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock),
    [Header, Body] = re:split(R, "\r\n\r\n", [{return, binary}]),
    {ok, {http_response, _, Code, _}, _} =
        erlang:decode_packet(http, Header, []),
    Json = ejson:decode(Body),
    {Code, Json}.

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

