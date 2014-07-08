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

-define(COMPRESSION_LEVEL, 8).
-define(ATT_BIN_NAME, <<"logo.png">>).
-define(ATT_TXT_NAME, <<"file.erl">>).
-define(FIXTURE_PNG, filename:join([?FIXTURESDIR, "logo.png"])).
-define(FIXTURE_TXT, ?FILE).
-define(TIMEOUT, 1000).
-define(TIMEOUT_EUNIT, 10).
-define(TIMEWAIT, 100).
-define(i2l(I), integer_to_list(I)).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    % ensure in default compression settings for attachments_compression_tests
    couch_config:set("attachments", "compression_level",
                     ?i2l(?COMPRESSION_LEVEL), false),
    couch_config:set("attachments", "compressible_types", "text/*", false),
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

setup({binary, standalone}) ->
    {Host, DbName} = setup(),
        setup_att(fun create_standalone_png_att/2, Host, DbName, ?FIXTURE_PNG);
setup({text, standalone}) ->
    {Host, DbName} = setup(),
    setup_att(fun create_standalone_text_att/2, Host, DbName, ?FIXTURE_TXT);
setup({binary, inline}) ->
    {Host, DbName} = setup(),
    setup_att(fun create_inline_png_att/2, Host, DbName, ?FIXTURE_PNG);
setup({text, inline}) ->
    {Host, DbName} = setup(),
    setup_att(fun create_inline_text_att/2, Host, DbName, ?FIXTURE_TXT);
setup(compressed) ->
    {Host, DbName} = setup(),
    setup_att(fun create_already_compressed_att/2, Host, DbName, ?FIXTURE_TXT).
setup_att(Fun, Host, DbName, File) ->
    HttpHost = "http://" ++ Host,
    AttUrl = Fun(HttpHost, DbName),
    {ok, Data} = file:read_file(File),
    DocUrl = string:join([HttpHost, DbName, "doc"], "/"),
    Helpers = {DbName, DocUrl, AttUrl},
    {Data, Helpers}.

teardown(_, {_, {DbName, _, _}}) ->
    teardown(DbName).

teardown({_, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(?l2b(DbName), []),
    ok.


attachments_test_() ->
    {
        "Attachments tests",
        {
            setup,
            fun start/0, fun stop/1,
            [
                attachments_md5_tests(),
                attachments_compression_tests()
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

attachments_compression_tests() ->
    Funs = [
         fun should_get_att_without_accept_gzip_encoding/2,
         fun should_get_att_with_accept_gzip_encoding/2,
         fun should_get_att_with_accept_deflate_encoding/2,
         fun should_return_406_response_on_unsupported_encoding/2,
         fun should_get_doc_with_att_data/2,
         fun should_get_doc_with_att_data_stub/2
    ],
    {
        "Attachments compression tests",
        [
            {
                "Created via Attachments API",
                created_attachments_compression_tests(standalone, Funs)
            },
            {
                "Created inline via Document API",
                created_attachments_compression_tests(inline, Funs)
            },
            {
                "Created already been compressed via Attachments API",
                {
                    foreachx,
                    fun setup/1, fun teardown/2,
                    [{compressed, Fun} || Fun <- Funs]
                }
            },
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_not_create_compressed_att_with_deflate_encoding/1,
                    fun should_not_create_compressed_att_with_compress_encoding/1,
                    fun should_create_compressible_att_with_ctype_params/1
                ]
            }
        ]
    }.

created_attachments_compression_tests(Mod, Funs) ->
    [
        {
            "Compressiable attachments",
            {
                foreachx,
                fun setup/1, fun teardown/2,
                [{{text, Mod}, Fun} || Fun <- Funs]
            }
        },
        {
            "Uncompressiable attachments",
            {
                foreachx,
                fun setup/1, fun teardown/2,
                [{{binary, Mod}, Fun} || Fun <- Funs]
            }
        }
    ].



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
        ?assertEqual(<<"content_md5_mismatch">>, get_json(Json, [<<"error">>]))
    end).

should_get_att_without_accept_gzip_encoding(_, {Data, {_, _, AttUrl}}) ->
    ?_test(begin
        {ok, Code, Headers, Body} = test_request:get(AttUrl),
        ?assertEqual(200, Code),
        ?assertNot(lists:member({"Content-Encoding", "gzip"}, Headers)),
        ?assertEqual(Data, iolist_to_binary(Body))
    end).

should_get_att_with_accept_gzip_encoding(compressed, {Data, {_, _, AttUrl}}) ->
    ?_test(begin
        {ok, Code, Headers, Body} = test_request:get(
            AttUrl, [{"Accept-Encoding", "gzip"}]),
        ?assertEqual(200, Code),
        ?assert(lists:member({"Content-Encoding", "gzip"}, Headers)),
        ?assertEqual(Data, zlib:gunzip(iolist_to_binary(Body)))
    end);
should_get_att_with_accept_gzip_encoding({text, _}, {Data, {_, _, AttUrl}}) ->
    ?_test(begin
        {ok, Code, Headers, Body} = test_request:get(
            AttUrl, [{"Accept-Encoding", "gzip"}]),
        ?assertEqual(200, Code),
        ?assert(lists:member({"Content-Encoding", "gzip"}, Headers)),
        ?assertEqual(Data, zlib:gunzip(iolist_to_binary(Body)))
    end);
should_get_att_with_accept_gzip_encoding({binary, _}, {Data, {_, _, AttUrl}}) ->
    ?_test(begin
        {ok, Code, Headers, Body} = test_request:get(
            AttUrl, [{"Accept-Encoding", "gzip"}]),
        ?assertEqual(200, Code),
        ?assertEqual(undefined,
                     couch_util:get_value("Content-Encoding", Headers)),
        ?assertEqual(Data, iolist_to_binary(Body))
    end).

should_get_att_with_accept_deflate_encoding(_, {Data, {_, _, AttUrl}}) ->
    ?_test(begin
        {ok, Code, Headers, Body} = test_request:get(
            AttUrl, [{"Accept-Encoding", "deflate"}]),
        ?assertEqual(200, Code),
        ?assertEqual(undefined,
                     couch_util:get_value("Content-Encoding", Headers)),
        ?assertEqual(Data, iolist_to_binary(Body))
    end).

should_return_406_response_on_unsupported_encoding(_, {_, {_, _, AttUrl}}) ->
    ?_assertEqual(406,
        begin
            {ok, Code, _, _} = test_request:get(
                AttUrl, [{"Accept-Encoding", "deflate, *;q=0"}]),
            Code
        end).

should_get_doc_with_att_data(compressed, {Data, {_, DocUrl, _}}) ->
    ?_test(begin
        Url = DocUrl ++ "?attachments=true",
        {ok, Code, _, Body} = test_request:get(
            Url, [{"Accept", "application/json"}]),
        ?assertEqual(200, Code),
        Json = ejson:decode(Body),
        AttJson = couch_util:get_nested_json_value(
            Json, [<<"_attachments">>, ?ATT_TXT_NAME]),
        AttData = couch_util:get_nested_json_value(
            AttJson, [<<"data">>]),
        ?assertEqual(
            <<"text/plain">>,
            couch_util:get_nested_json_value(AttJson,[<<"content_type">>])),
        ?assertEqual(Data, base64:decode(AttData))
    end);
should_get_doc_with_att_data({text, _}, {Data, {_, DocUrl, _}}) ->
    ?_test(begin
        Url = DocUrl ++ "?attachments=true",
        {ok, Code, _, Body} = test_request:get(
            Url, [{"Accept", "application/json"}]),
        ?assertEqual(200, Code),
        Json = ejson:decode(Body),
        AttJson = couch_util:get_nested_json_value(
            Json, [<<"_attachments">>, ?ATT_TXT_NAME]),
        AttData = couch_util:get_nested_json_value(
            AttJson, [<<"data">>]),
        ?assertEqual(
            <<"text/plain">>,
            couch_util:get_nested_json_value(AttJson,[<<"content_type">>])),
        ?assertEqual(Data, base64:decode(AttData))
    end);
should_get_doc_with_att_data({binary, _}, {Data, {_, DocUrl, _}}) ->
    ?_test(begin
        Url = DocUrl ++ "?attachments=true",
        {ok, Code, _, Body} = test_request:get(
            Url, [{"Accept", "application/json"}]),
        ?assertEqual(200, Code),
        Json = ejson:decode(Body),
        AttJson = couch_util:get_nested_json_value(
            Json, [<<"_attachments">>, ?ATT_BIN_NAME]),
        AttData = couch_util:get_nested_json_value(
            AttJson, [<<"data">>]),
        ?assertEqual(
            <<"image/png">>,
            couch_util:get_nested_json_value(AttJson,[<<"content_type">>])),
        ?assertEqual(Data, base64:decode(AttData))
    end).

should_get_doc_with_att_data_stub(compressed, {Data, {_, DocUrl, _}}) ->
    ?_test(begin
        Url = DocUrl ++ "?att_encoding_info=true",
        {ok, Code, _, Body} = test_request:get(
            Url, [{"Accept", "application/json"}]),
        ?assertEqual(200, Code),
        Json = ejson:decode(Body),
        {AttJson} = couch_util:get_nested_json_value(
            Json, [<<"_attachments">>, ?ATT_TXT_NAME]),
        ?assertEqual(<<"gzip">>,
                     couch_util:get_value(<<"encoding">>, AttJson)),
        AttLength = couch_util:get_value(<<"length">>, AttJson),
        EncLength = couch_util:get_value(<<"encoded_length">>, AttJson),
        ?assertEqual(AttLength, EncLength),
        ?assertEqual(iolist_size(zlib:gzip(Data)), AttLength)
    end);
should_get_doc_with_att_data_stub({text, _}, {Data, {_, DocUrl, _}}) ->
    ?_test(begin
        Url = DocUrl ++ "?att_encoding_info=true",
        {ok, Code, _, Body} = test_request:get(
            Url, [{"Accept", "application/json"}]),
        ?assertEqual(200, Code),
        Json = ejson:decode(Body),
        {AttJson} = couch_util:get_nested_json_value(
            Json, [<<"_attachments">>, ?ATT_TXT_NAME]),
        ?assertEqual(<<"gzip">>,
                     couch_util:get_value(<<"encoding">>, AttJson)),
        AttEncLength = iolist_size(gzip(Data)),
        ?assertEqual(AttEncLength,
                     couch_util:get_value(<<"encoded_length">>, AttJson)),
        ?assertEqual(byte_size(Data),
                     couch_util:get_value(<<"length">>, AttJson))
    end);
should_get_doc_with_att_data_stub({binary, _}, {Data, {_, DocUrl, _}}) ->
    ?_test(begin
        Url = DocUrl ++ "?att_encoding_info=true",
        {ok, Code, _, Body} = test_request:get(
            Url, [{"Accept", "application/json"}]),
        ?assertEqual(200, Code),
        Json = ejson:decode(Body),
        {AttJson} = couch_util:get_nested_json_value(
            Json, [<<"_attachments">>, ?ATT_BIN_NAME]),
        ?assertEqual(undefined,
                     couch_util:get_value(<<"encoding">>, AttJson)),
        ?assertEqual(undefined,
                     couch_util:get_value(<<"encoded_length">>, AttJson)),
        ?assertEqual(byte_size(Data),
                     couch_util:get_value(<<"length">>, AttJson))
    end).

should_not_create_compressed_att_with_deflate_encoding({Host, DbName}) ->
    ?_assertEqual(415,
        begin
            HttpHost = "http://" ++ Host,
            AttUrl = string:join([HttpHost, DbName, ?docid(), "file.txt"], "/"),
            {ok, Data} = file:read_file(?FIXTURE_TXT),
            Body = zlib:compress(Data),
            Headers = [
                {"Content-Encoding", "deflate"},
                {"Content-Type", "text/plain"}
            ],
            {ok, Code, _, _} = test_request:put(AttUrl, Headers, Body),
            Code
        end).

should_not_create_compressed_att_with_compress_encoding({Host, DbName}) ->
    % Note: As of OTP R13B04, it seems there's no LZW compression
    % (i.e. UNIX compress utility implementation) lib in OTP.
    % However there's a simple working Erlang implementation at:
    % http://scienceblogs.com/goodmath/2008/01/simple_lempelziv_compression_i.php
    ?_assertEqual(415,
        begin
            HttpHost = "http://" ++ Host,
            AttUrl = string:join([HttpHost, DbName, ?docid(), "file.txt"], "/"),
            {ok, Data} = file:read_file(?FIXTURE_TXT),
            Headers = [
                {"Content-Encoding", "compress"},
                {"Content-Type", "text/plain"}
            ],
            {ok, Code, _, _} = test_request:put(AttUrl, Headers, Data),
            Code
        end).

should_create_compressible_att_with_ctype_params({Host, DbName}) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(begin
        HttpHost = "http://" ++ Host,
        DocUrl = string:join([HttpHost, DbName, ?docid()], "/"),
        AttUrl = string:join([DocUrl, ?b2l(?ATT_TXT_NAME)], "/"),
        {ok, Data} = file:read_file(?FIXTURE_TXT),
        Headers = [{"Content-Type", "text/plain; charset=UTF-8"}],
        {ok, Code0, _, _} = test_request:put(AttUrl, Headers, Data),
        ?assertEqual(201, Code0),

        {ok, Code1, _, Body} = test_request:get(
            DocUrl ++ "?att_encoding_info=true"),
        ?assertEqual(200, Code1),
        Json = ejson:decode(Body),
        {AttJson} = couch_util:get_nested_json_value(
            Json, [<<"_attachments">>, ?ATT_TXT_NAME]),
        ?assertEqual(<<"gzip">>,
                     couch_util:get_value(<<"encoding">>, AttJson)),
        AttEncLength = iolist_size(gzip(Data)),
        ?assertEqual(AttEncLength,
                     couch_util:get_value(<<"encoded_length">>, AttJson)),
        ?assertEqual(byte_size(Data),
                     couch_util:get_value(<<"length">>, AttJson))
    end)}.


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

create_standalone_text_att(Host, DbName) ->
    {ok, Data} = file:read_file(?FIXTURE_TXT),
    Url = string:join([Host, DbName, "doc", ?b2l(?ATT_TXT_NAME)], "/"),
    {ok, Code, _Headers, _Body} = test_request:put(
        Url, [{"Content-Type", "text/plain"}], Data),
    ?assertEqual(201, Code),
    Url.

create_standalone_png_att(Host, DbName) ->
    {ok, Data} = file:read_file(?FIXTURE_PNG),
    Url = string:join([Host, DbName, "doc", ?b2l(?ATT_BIN_NAME)], "/"),
    {ok, Code, _Headers, _Body} = test_request:put(
        Url, [{"Content-Type", "image/png"}], Data),
    ?assertEqual(201, Code),
    Url.

create_inline_text_att(Host, DbName) ->
    {ok, Data} = file:read_file(?FIXTURE_TXT),
    Url = string:join([Host, DbName, "doc"], "/"),
    Doc = {[
        {<<"_attachments">>, {[
            {?ATT_TXT_NAME, {[
                {<<"content_type">>, <<"text/plain">>},
                {<<"data">>, base64:encode(Data)}
            ]}
        }]}}
    ]},
    {ok, Code, _Headers, _Body} = test_request:put(
        Url, [{"Content-Type", "application/json"}], ejson:encode(Doc)),
    ?assertEqual(201, Code),
    string:join([Url, ?b2l(?ATT_TXT_NAME)], "/").

create_inline_png_att(Host, DbName) ->
    {ok, Data} = file:read_file(?FIXTURE_PNG),
    Url = string:join([Host, DbName, "doc"], "/"),
    Doc = {[
        {<<"_attachments">>, {[
            {?ATT_BIN_NAME, {[
                {<<"content_type">>, <<"image/png">>},
                {<<"data">>, base64:encode(Data)}
            ]}
        }]}}
    ]},
    {ok, Code, _Headers, _Body} = test_request:put(
        Url, [{"Content-Type", "application/json"}], ejson:encode(Doc)),
    ?assertEqual(201, Code),
    string:join([Url, ?b2l(?ATT_BIN_NAME)], "/").

create_already_compressed_att(Host, DbName) ->
    {ok, Data} = file:read_file(?FIXTURE_TXT),
    Url = string:join([Host, DbName, "doc", ?b2l(?ATT_TXT_NAME)], "/"),
    {ok, Code, _Headers, _Body} = test_request:put(
        Url, [{"Content-Type", "text/plain"}, {"Content-Encoding", "gzip"}],
        zlib:gzip(Data)),
    ?assertEqual(201, Code),
    Url.

gzip(Data) ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z, ?COMPRESSION_LEVEL, deflated, 16 + 15, 8, default),
    zlib:deflate(Z, Data),
    Last = zlib:deflate(Z, [], finish),
    ok = zlib:deflateEnd(Z),
    ok = zlib:close(Z),
    Last.
