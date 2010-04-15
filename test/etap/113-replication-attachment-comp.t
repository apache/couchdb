#!/usr/bin/env escript
%% -*- erlang -*-

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

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

default_config() ->
    test_util:build_file("etc/couchdb/default_dev.ini").

test_db_a_name() ->
    <<"couch_test_rep_att_comp_a">>.

test_db_b_name() ->
    <<"couch_test_rep_att_comp_b">>.

main(_) ->
    test_util:init_code_path(),
    etap:plan(30),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server_sup:start_link([default_config()]),
    put(addr, couch_config:get("httpd", "bind_address", "127.0.0.1")),
    put(port, couch_config:get("httpd", "port", "5984")),
    application:start(inets),
    ibrowse:start(),
    timer:sleep(1000),

    %
    % test pull replication
    %

    delete_db(test_db_a_name()),
    delete_db(test_db_b_name()),
    create_db(test_db_a_name()),
    create_db(test_db_b_name()),

    % enable compression
    couch_config:set("attachments", "compression_level", "8"),
    couch_config:set("attachments", "compressible_types", "text/*"),

    % store doc with text attachment in DB A
    put_text_att(test_db_a_name()),

    % disable attachment compression
    couch_config:set("attachments", "compression_level", "0"),

    % do pull replication
    do_pull_replication(test_db_a_name(), test_db_b_name()),

    % verify that DB B has the attachment stored in compressed form
    check_att_is_compressed(test_db_b_name()),
    check_server_can_decompress_att(test_db_b_name()),
    check_att_stubs(test_db_a_name(), test_db_b_name()),

    %
    % test push replication
    %

    delete_db(test_db_a_name()),
    delete_db(test_db_b_name()),
    create_db(test_db_a_name()),
    create_db(test_db_b_name()),

    % enable compression
    couch_config:set("attachments", "compression_level", "8"),
    couch_config:set("attachments", "compressible_types", "text/*"),

    % store doc with text attachment in DB A
    put_text_att(test_db_a_name()),

    % disable attachment compression
    couch_config:set("attachments", "compression_level", "0"),

    % do push replication
    do_push_replication(test_db_a_name(), test_db_b_name()),

    % verify that DB B has the attachment stored in compressed form
    check_att_is_compressed(test_db_b_name()),
    check_server_can_decompress_att(test_db_b_name()),
    check_att_stubs(test_db_a_name(), test_db_b_name()),

    timer:sleep(3000), % to avoid mochiweb socket closed exceptions
    delete_db(test_db_a_name()),
    delete_db(test_db_b_name()),
    couch_server_sup:stop(),
    ok.

put_text_att(DbName) ->
    {ok, {{_, Code, _}, _Headers, _Body}} = http:request(
        put,
        {db_url(DbName) ++ "/testdoc1/readme.txt", [],
        "text/plain", test_text_data()},
        [],
        [{sync, true}]),
    etap:is(Code, 201, "Created text attachment"),
    ok.

do_pull_replication(SourceDbName, TargetDbName) ->
    RepObj = {[
        {<<"source">>, list_to_binary(db_url(SourceDbName))},
        {<<"target">>, TargetDbName}
    ]},
    {ok, {{_, Code, _}, _Headers, Body}} = http:request(
        post,
        {rep_url(), [],
        "application/json", list_to_binary(couch_util:json_encode(RepObj))},
        [],
        [{sync, true}]),
    etap:is(Code, 200, "Pull replication successfully triggered"),
    Json = couch_util:json_decode(Body),
    RepOk = couch_util:get_nested_json_value(Json, [<<"ok">>]),
    etap:is(RepOk, true, "Pull replication completed with success"),
    ok.

do_push_replication(SourceDbName, TargetDbName) ->
    RepObj = {[
        {<<"source">>, SourceDbName},
        {<<"target">>, list_to_binary(db_url(TargetDbName))}
    ]},
    {ok, {{_, Code, _}, _Headers, Body}} = http:request(
        post,
        {rep_url(), [],
        "application/json", list_to_binary(couch_util:json_encode(RepObj))},
        [],
        [{sync, true}]),
    etap:is(Code, 200, "Push replication successfully triggered"),
    Json = couch_util:json_decode(Body),
    RepOk = couch_util:get_nested_json_value(Json, [<<"ok">>]),
    etap:is(RepOk, true, "Push replication completed with success"),
    ok.

check_att_is_compressed(DbName) ->
    {ok, {{_, Code, _}, Headers, Body}} = http:request(
        get,
        {db_url(DbName) ++ "/testdoc1/readme.txt",
        [{"Accept-Encoding", "gzip"}]},
        [],
        [{sync, true}]),
    etap:is(Code, 200, "HTTP response code for the attachment request is 200"),
    Gziped = lists:member({"content-encoding", "gzip"}, Headers),
    etap:is(Gziped, true, "The attachment was received in compressed form"),
    Uncompressed = binary_to_list(zlib:gunzip(list_to_binary(Body))),
    etap:is(
        Uncompressed,
        test_text_data(),
        "The attachment content is valid after decompression at the client side"
    ),
    ok.

check_server_can_decompress_att(DbName) ->
    {ok, {{_, Code, _}, Headers, Body}} = http:request(
        get,
        {db_url(DbName) ++ "/testdoc1/readme.txt", []},
        [],
        [{sync, true}]),
    etap:is(Code, 200, "HTTP response code for the attachment request is 200"),
    Gziped = lists:member({"content-encoding", "gzip"}, Headers),
    etap:is(
        Gziped, false, "The attachment was not received in compressed form"
    ),
    etap:is(
        Body,
        test_text_data(),
        "The attachment content is valid after server decompression"
    ),
    ok.

check_att_stubs(SourceDbName, TargetDbName) ->
    {ok, {{_, Code1, _}, _Headers1, Body1}} = http:request(
        get,
        {db_url(SourceDbName) ++ "/testdoc1?att_encoding_info=true", []},
        [],
        [{sync, true}]),
    etap:is(
        Code1,
        200,
        "HTTP response code is 200 for the source DB doc request"
    ),
    Json1 = couch_util:json_decode(Body1),
    SourceAttStub = couch_util:get_nested_json_value(
        Json1,
        [<<"_attachments">>, <<"readme.txt">>]
    ),
    {ok, {{_, Code2, _}, _Headers2, Body2}} = http:request(
        get,
        {db_url(TargetDbName) ++ "/testdoc1?att_encoding_info=true", []},
        [],
        [{sync, true}]),
    etap:is(
        Code2,
        200,
        "HTTP response code is 200 for the target DB doc request"
    ),
    Json2 = couch_util:json_decode(Body2),
    TargetAttStub = couch_util:get_nested_json_value(
        Json2,
        [<<"_attachments">>, <<"readme.txt">>]
    ),
    IdenticalStubs = (SourceAttStub =:= TargetAttStub),
    etap:is(IdenticalStubs, true, "Attachment stubs are identical"),
    TargetAttStubLength = couch_util:get_nested_json_value(
        TargetAttStub,
        [<<"length">>]
    ),
    TargetAttStubEnc = couch_util:get_nested_json_value(
        TargetAttStub,
        [<<"encoding">>]
    ),
    etap:is(
        TargetAttStubEnc,
        <<"gzip">>,
        "Attachment stub has encoding property set to gzip"
    ),
    TargetAttStubEncLength = couch_util:get_nested_json_value(
        TargetAttStub,
        [<<"encoded_length">>]
    ),
    EncLengthDefined = is_integer(TargetAttStubEncLength),
    etap:is(
        EncLengthDefined,
        true,
        "Stubs have the encoded_length field properly defined"
    ),
    EncLengthSmaller = (TargetAttStubEncLength < TargetAttStubLength),
    etap:is(
        EncLengthSmaller,
        true,
        "Stubs have the encoded_length field smaller than their length field"
    ),
    ok.

admin_user_ctx() ->
    {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

create_db(DbName) ->
    {ok, _} = couch_db:create(DbName, [admin_user_ctx()]).

delete_db(DbName) ->
    couch_server:delete(DbName, [admin_user_ctx()]).

db_url(DbName) ->
    "http://" ++ get(addr) ++ ":" ++ get(port) ++ "/" ++
    binary_to_list(DbName).

rep_url() ->
    "http://" ++ get(addr) ++ ":" ++ get(port) ++ "/_replicate".

test_text_data() ->
    {ok, Data} = file:read_file(test_util:source_file("README")),
    binary_to_list(Data).
