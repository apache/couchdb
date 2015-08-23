% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(global_changes_hooks_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([allowed_owner/2]).

-define(t2l(V), lists:flatten(io_lib:format("~p", [V]))).

start() ->
    Ctx = test_util:start_couch([chttpd]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX]),
    application:set_env(global_changes, dbname, DbName),
    {Ctx, DbName}.

stop({Ctx, DbName}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    test_util:stop_couch(Ctx),
    ok.

setup(default) ->
    add_admin("admin", <<"pass">>),
    config:delete("couch_httpd_auth", "authentication_redirect", false),
    config:set("couch_httpd_auth", "require_valid_user", "false", false),
    get_host();
setup(A) ->
    Host = setup(default),
    ok = config:set("global_changes", "allowed_owner",
        ?t2l({?MODULE, allowed_owner, A}), false),
    Host.

teardown(_) ->
    delete_admin("admin"),
    config:delete("global_changes", "allowed_owner", false),
    ok.

allowed_owner(Req, "throw") ->
    throw({unauthorized, <<"Exception thrown.">>});
allowed_owner(Req, "pass") ->
    "super".

allowed_owner_hook_test_() ->
    {
        "Check allowed_owner hook",
        {
            setup,
            fun start/0, fun stop/1,
            [
                disabled_allowed_owner_integration_point(),
                enabled_allowed_owner_integration_point()
            ]
        }
    }.

disabled_allowed_owner_integration_point() ->
    {
        "disabled allowed_owner integration point",
        {
            foreach,
            fun() -> setup(default) end, fun teardown/1,
            [
                fun should_not_fail_for_admin/1,
                fun should_fail_for_non_admin/1
             ]
        }
    }.

enabled_allowed_owner_integration_point() ->
    {
        "enabled allowed_owner integration point",
        [
            {
                foreach,
                fun() -> setup("throw") end, fun teardown/1,
                [fun should_throw/1]
            },
            {
                foreach,
                fun() -> setup("pass") end, fun teardown/1,
                [fun should_pass/1]
            }
        ]
    }.

should_not_fail_for_admin(Host) ->
    ?_test(begin
        Headers = [{basic_auth, {"admin", "pass"}}],
        {Status, [Error, Reason]} =
            request(Host, Headers, [<<"error">>, <<"reason">>]),
        ?assertEqual(200, Status),
        ?assertEqual(undefined, Error),
        ?assertEqual(undefined, Reason)
    end).

should_fail_for_non_admin(Host) ->
    ?_test(begin
        Headers = [],
        {Status, [Error, Reason]} =
            request(Host, Headers, [<<"error">>, <<"reason">>]),
        ?assertEqual(401, Status),
        ?assertEqual(<<"unauthorized">>, Error),
        ?assertEqual(<<"You are not a server admin.">>, Reason)
    end).

should_pass(Host) ->
    ?_test(begin
        Headers = [{basic_auth, {"admin", "pass"}}],
        {Status, [Error, Reason]} =
            request(Host, Headers, [<<"error">>, <<"reason">>]),
        ?assertEqual(200, Status),
        ?assertEqual(undefined, Error),
        ?assertEqual(undefined, Reason)
    end).

should_throw(Host) ->
    ?_test(begin
        Headers = [{basic_auth, {"admin", "pass"}}],
        {Status, [Error, Reason]} =
            request(Host, Headers, [<<"error">>, <<"reason">>]),
        ?assertEqual(401, Status),
        ?assertEqual(<<"unauthorized">>, Error),
        ?assertEqual(<<"Exception thrown.">>, Reason)
    end).

request(Host, Headers, ToDecode) ->
    Url = Host ++ "/_db_updates",
    {ok, Status, _Headers, BinBody} = test_request:get(Url, Headers),
    {Body} = jiffy:decode(BinBody),
    Values = [couch_util:get_value(Key, Body) || Key <- ToDecode],
    {Status, Values}.

add_admin(User, Pass) ->
    Hashed = couch_passwords:hash_admin_password(Pass),
    config:set("admins", User, ?b2l(Hashed), false).

delete_admin(User) ->
    config:delete("admins", User, false).

get_host() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = config:get("chttpd", "port", "5984"),
    Host = "http://" ++ Addr ++ ":" ++ Port,
    Host.
