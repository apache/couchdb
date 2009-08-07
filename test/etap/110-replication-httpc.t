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
        {"User-Agent", "CouchDb/"++couch_server:get_version()},
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

-define(SERVER, "http://127.0.0.1:5984/").
-define(DBNAME, "etap-test-db").

main(_) ->
    code:add_pathz("src/couchdb"),
    code:add_pathz("src/ibrowse"),
    code:add_pathz("src/mochiweb"),
    code:add_pathz("src/erlang-oauth"),
    
    etap:plan(7),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server:start(
        ["etc/couchdb/default_dev.ini", "etc/couchdb/local_dev.ini"]
    ),
    ibrowse:start(),
    crypto:start(),

    couch_server:delete(list_to_binary(?DBNAME), []),
    {ok, Db} = couch_db:create(list_to_binary(?DBNAME), []),

    test_welcome(),
    test_binary_url(),
    test_put(),
    test_qs(),
    test_db_exists(),

    couch_db:close(Db),
    couch_server:delete(list_to_binary(?DBNAME), []),
    ok.

test_welcome() ->
    WelcomeReq = #http_db{url=?SERVER},
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
    Req = #http_db{url=list_to_binary(?SERVER)},
    Expect = {[
        {<<"couchdb">>, <<"Welcome">>},
        {<<"version">>, list_to_binary(couch_server:get_version())}
    ]},
    Fun = fun(Expect) -> true; (_) -> false end,
    etap:is(
        couch_rep_httpc:request(Req),
        Expect,
        "welcome request with url-as-binary"
    ).

test_put() ->
    Req = #http_db{
        url = ?SERVER ++ ?DBNAME ++ "/",
        resource = "test_put",
        body = {[{<<"foo">>, <<"bar">>}]},
        method = put
    },
    {Resp} = couch_rep_httpc:request(Req),
    etap:ok(proplists:get_value(<<"ok">>, Resp), "ok:true on upload"),
    etap:is(<<"test_put">>, proplists:get_value(<<"id">>, Resp), "id is correct").

test_qs() ->
    Req = #http_db{
        url = ?SERVER ++ ?DBNAME ++ "/",
        resource = "foo",
        qs = [
            {bar, true},
            {baz, 1.03},
            {bif, mochijson2:encode(<<"1-23456">>)}
        ]
    },
    Expect = ?SERVER ++ ?DBNAME ++ "/foo?bar=true&baz=1.03&bif=\"1-23456\"",
    etap:is(
        couch_rep_httpc:full_url(Req),
        Expect,
        "query-string proplist encoding ok"
    ).

test_db_exists() ->
    Req1 = #http_db{url=?SERVER ++ ?DBNAME ++ "/"},
    Req2 = #http_db{url=?SERVER ++ ?DBNAME ++ "_foo/"},
    etap:ok(couch_rep_httpc:db_exists(Req1), "db_exists true check"),
    etap:is(couch_rep_httpc:db_exists(Req2), false, "db_exists false check").
