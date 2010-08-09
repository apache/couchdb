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

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

server() -> "http://127.0.0.1:5984/".
dbname() -> "etap-test-db".
admin_user_ctx() -> {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

config_files() ->
    lists:map(fun test_util:build_file/1, [
        "etc/couchdb/default_dev.ini",
        "etc/couchdb/local_dev.ini"
    ]).

main(_) ->
    test_util:init_code_path(),

    etap:plan(4),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server_sup:start_link(config_files()),
    ibrowse:start(),
    crypto:start(),

    couch_server:delete(list_to_binary(dbname()), [admin_user_ctx()]),
    {ok, Db} = couch_db:create(list_to_binary(dbname()), [admin_user_ctx()]),

    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc1">>},
        {<<"value">>, 666}
    ]}),
    {ok, _} = couch_db:update_docs(Db, [Doc]),
    couch_db:ensure_full_commit(Db),

    %% end boilerplate, start test

    couch_config:set("vhosts", "example.com", "/etap-test-db", false),
    test_regular_request(),
    test_vhost_request(),
    test_vhost_request_with_qs(),
    test_vhost_request_with_global(),
    
    %% restart boilerplate
    couch_db:close(Db),
    couch_server:delete(list_to_binary(dbname()), []),
    ok.

test_regular_request() ->
    case ibrowse:send_req(server(), [], get, []) of
        {ok, _, _, Body} ->
            {[{<<"couchdb">>, <<"Welcome">>},
              {<<"version">>,_}
            ]} = couch_util:json_decode(Body),
            etap:is(true, true, "should return server info");
        _Else -> false
    end.

test_vhost_request() ->
    case ibrowse:send_req(server(), [], get, [], [{host_header, "example.com"}]) of
        {ok, _, _, Body} ->
            {[{<<"db_name">>, <<"etap-test-db">>},_,_,_,_,_,_,_,_,_]}
                = couch_util:json_decode(Body),
            etap:is(true, true, "should return database info");
        _Else -> false
    end.

test_vhost_request_with_qs() ->
    Url = server() ++ "doc1?revs_info=true",
    case ibrowse:send_req(Url, [], get, [], [{host_header, "example.com"}]) of
        {ok, _, _, Body} ->
            {JsonProps} = couch_util:json_decode(Body),
            HasRevsInfo = proplists:is_defined(<<"_revs_info">>, JsonProps),
            etap:is(HasRevsInfo, true, "should return _revs_info");
        _Else -> false
    end.

test_vhost_request_with_global() ->
    Url2 = server() ++ "_utils/index.html",
    case ibrowse:send_req(Url2, [], get, [], [{host_header, "example.com"}]) of
        {ok, _, _, Body2} ->
            "<!DOCTYPE" ++ _Foo = Body2,
            etap:is(true, true, "should serve /_utils even inside vhosts");
        _Else -> false
    end.
