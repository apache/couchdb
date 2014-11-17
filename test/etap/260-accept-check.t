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

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

server() ->
    lists:concat([
        "http://127.0.0.1:", mochiweb_socket_server:get(couch_httpd, port), "/"
    ]).

dbname() -> "etap-test-db".
admin_user_ctx() -> {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

set_admin_password(UserName, Password) ->
    Hashed = couch_passwords:hash_admin_password(Password),
    couch_config:set("admins", UserName, Hashed, false).

main(_) ->
    test_util:init_code_path(),

    etap:plan(1),
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

    timer:sleep(1000),
    couch_server:delete(list_to_binary(dbname()), [admin_user_ctx()]),
    {ok, Db} = couch_db:create(list_to_binary(dbname()), [admin_user_ctx()]),
    couch_db:ensure_full_commit(Db),
    SecObj = {[{<<"admins">>,{[{<<"names">>,[]},{<<"roles">>,[<<"nobody">>]}]}},{<<"members">>,{[{<<"names">>,[]},{<<"roles">>,[<<"nobody">>]}]}}]},
    couch_db:set_security(Db, SecObj),
    couch_db:ensure_full_commit(Db),
    ok = set_admin_password("test", <<"test">>),

    %% end boilerplate, start test

    test_accept_check(),

    %% restart boilerplate
    couch_db:close(Db),
    ok = couch_server:delete(couch_db:name(Db), [admin_user_ctx()]),
    timer:sleep(3000),
    couch_server_sup:stop(),

    ok.

test_accept_check() ->
    Headers = [{"Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"}],
    case ibrowse:send_req(server() ++ dbname(), Headers, get) of
        {ok, "302", _, _} ->
            etap:ok(true, "Browser is redirected");
	{ok, Code, _, Body} ->
            {JsonBody} = ejson:decode(Body),
            etap:isnt(
                couch_util:get_value(<<"error">>, JsonBody),
                <<"unauthorized">>,
                "Failed because the Browser received JSON with code: " ++ Code ++ " body: " ++ Body);
        Error ->
           etap:bail("Request didn't get unauthorized: " ++
               couch_util:to_list(Error))
    end.
