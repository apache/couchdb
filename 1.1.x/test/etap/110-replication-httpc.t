#!/usr/bin/env escript
%% -*- erlang -*-

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

%% XXX: Figure out how to -include("couch_rep.hrl")
-record(http_db, {
    url,
    auth = [],
    resource = "",
    headers = [
        {"User-Agent", "CouchDB/"++couch_server:get_version()},
        {"Accept", "application/json"},
        {"Accept-Encoding", "gzip"}
    ],
    qs = [],
    method = get,
    body = nil,
    options = [
        {response_format,binary},
        {inactivity_timeout, 30000}
    ],
    retries = 10,
    pause = 1,
    conn = nil
}).

server() ->
    lists:concat([
        "http://127.0.0.1:", mochiweb_socket_server:get(couch_httpd, port), "/"
    ]).

dbname() -> "etap-test-db".

main(_) ->
    test_util:init_code_path(),
    
    etap:plan(6),
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
    ibrowse:start(),
    crypto:start(),

    couch_server:delete(list_to_binary(dbname()), []),
    {ok, Db} = couch_db:create(list_to_binary(dbname()), []),

    test_welcome(),
    test_binary_url(),
    test_put(),
    test_qs(),
    test_db_exists(),

    couch_db:close(Db),
    couch_server:delete(list_to_binary(dbname()), []),
    ok.

test_welcome() ->
    WelcomeReq = #http_db{url=server()},
    Expect = {[
        {<<"couchdb">>, <<"Welcome">>},
        {<<"version">>, list_to_binary(couch_server:get_version())}
    ]},
    etap:is(
        couch_rep_httpc:request(WelcomeReq),
        Expect,
        "welcome request with url-as-list"
    ).

test_binary_url() ->
    Req = #http_db{url=list_to_binary(server())},
    Expect = {[
        {<<"couchdb">>, <<"Welcome">>},
        {<<"version">>, list_to_binary(couch_server:get_version())}
    ]},
    etap:is(
        couch_rep_httpc:request(Req),
        Expect,
        "welcome request with url-as-binary"
    ).

test_put() ->
    Req = #http_db{
        url = server() ++ dbname() ++ "/",
        resource = "test_put",
        body = {[{<<"foo">>, <<"bar">>}]},
        method = put
    },
    {Resp} = couch_rep_httpc:request(Req),
    etap:ok(couch_util:get_value(<<"ok">>, Resp), "ok:true on upload"),
    etap:is(<<"test_put">>, couch_util:get_value(<<"id">>, Resp), "id is correct").

test_qs() ->
    Req = #http_db{
        url = server() ++ dbname() ++ "/",
        resource = "foo",
        qs = [
            {bar, true},
            {baz, 1.03},
            {bif, mochijson2:encode(<<"1-23456">>)}
        ]
    },
    Expect = server() ++ dbname() ++ "/foo?bar=true&baz=1.03&bif=\"1-23456\"",
    etap:is(
        couch_rep_httpc:full_url(Req),
        Expect,
        "query-string proplist encoding ok"
    ).

test_db_exists() ->
    Req1 = #http_db{url=server() ++ dbname() ++ "/"},
    Req2 = #http_db{url=server() ++ dbname() ++ "_foo/"},
    etap:is(couch_rep_httpc:db_exists(Req1), Req1, "db_exists true check").
    % etap:is(couch_rep_httpc:db_exists(Req2), false, "db_exists false check").
