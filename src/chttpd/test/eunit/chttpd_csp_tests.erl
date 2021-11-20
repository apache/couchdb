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

-module(chttpd_csp_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(ADM_USER, "adm_user").
-define(ADM_PASS, "adm_pass").
-define(ADM, {?ADM_USER, ?ADM_PASS}).
-define(ACC_USER, "acc").
-define(ACC_PASS, "pass").
-define(ACC, {?ACC_USER, ?ACC_PASS}).
-define(DOC1, "doc1").
-define(DDOC1, "_design/ddoc1").
-define(DDOC1_PATH_ENC, "_design%2Fddoc1").
-define(LDOC1, "_local/ldoc1").
-define(LDOC1_PATH_ENC, "_local%2Fldoc1").
-define(ATT1, "att1").
-define(VIEW1, "view1").
-define(SHOW1, "show1").
-define(LIST1, "list1").
-define(SALT, <<"01234567890123456789012345678901">>).
-define(TDEF(Name), {atom_to_list(Name), fun Name/1}).
-define(TDEF(Name, Timeout), {atom_to_list(Name), Timeout, fun Name/1}).
-define(TDEF_FE(Name), fun(Arg) -> {atom_to_list(Name), ?_test(Name(Arg))} end).
-define(TDEF_FE(Name, Timeout), fun(Arg) ->
    {atom_to_list(Name), {timeout, Timeout, ?_test(Name(Arg))}}
end).

csp_test_() ->
    {
        "CSP Tests",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(plain_docs_not_sandboxed),
                    ?TDEF_FE(plain_ddocs_not_sandboxed),
                    ?TDEF_FE(local_docs_not_sandboxed),
                    ?TDEF_FE(sandbox_doc_attachments),
                    ?TDEF_FE(sandbox_ddoc_attachments),
                    ?TDEF_FE(sandbox_shows),
                    ?TDEF_FE(sandbox_lists),
                    fun should_not_return_any_csp_headers_when_disabled/1,
                    fun should_apply_default_policy_with_legacy_config/1,
                    fun should_apply_default_policy/1,
                    fun should_return_custom_policy/1
                ]
            }
        }
    }.

plain_docs_not_sandboxed(DbName) ->
    DbUrl = base_url() ++ "/" ++ DbName,
    Url = DbUrl ++ "/" ++ ?DOC1,
    ?assertEqual({200, false}, req(get, ?ACC, Url)),
    config:set("csp", "attachments_enable", "false", false),
    ?assertEqual({200, false}, req(get, ?ACC, Url)).

plain_ddocs_not_sandboxed(DbName) ->
    DbUrl = base_url() ++ "/" ++ DbName,
    Url = DbUrl ++ "/" ++ ?DDOC1,
    ?assertEqual({200, false}, req(get, ?ACC, Url)),
    config:set("csp", "attachments_enable", "false", false),
    ?assertEqual({200, false}, req(get, ?ACC, Url)).

local_docs_not_sandboxed(DbName) ->
    DbUrl = base_url() ++ "/" ++ DbName,
    Url = DbUrl ++ "/" ++ ?LDOC1,
    ?assertEqual({200, false}, req(get, ?ACC, Url)),
    config:set("csp", "attachments_enable", "false", false),
    ?assertEqual({200, false}, req(get, ?ACC, Url)).

sandbox_doc_attachments(DbName) ->
    DbUrl = base_url() ++ "/" ++ DbName,
    Url = DbUrl ++ "/" ++ ?DOC1 ++ "/" ++ ?ATT1,
    ?assertEqual({200, true}, req(get, ?ACC, Url)),
    config:set("csp", "attachments_enable", "false", false),
    ?assertEqual({200, false}, req(get, ?ACC, Url)).

sandbox_ddoc_attachments(DbName) ->
    DbUrl = base_url() ++ "/" ++ DbName,
    Url = DbUrl ++ "/" ++ ?DDOC1 ++ "/" ++ ?ATT1,
    ?assertEqual({200, true}, req(get, ?ACC, Url)),
    config:set("csp", "attachments_enable", "false", false),
    ?assertEqual({200, false}, req(get, ?ACC, Url)).

sandbox_shows(DbName) ->
    DbUrl = base_url() ++ "/" ++ DbName,
    DDocUrl = DbUrl ++ "/" ++ ?DDOC1,
    Url = DDocUrl ++ "/_show/" ++ ?SHOW1 ++ "/" ++ ?DOC1,
    ?assertEqual({200, true}, req(get, ?ACC, Url)),
    config:set("csp", "showlist_enable", "false", false),
    ?assertEqual({200, false}, req(get, ?ACC, Url)).

sandbox_lists(DbName) ->
    DbUrl = base_url() ++ "/" ++ DbName,
    DDocUrl = DbUrl ++ "/" ++ ?DDOC1,
    Url = DDocUrl ++ "/_list/" ++ ?LIST1 ++ "/" ++ ?VIEW1,
    ?assertEqual({200, true}, req(get, ?ACC, Url)),
    config:set("csp", "showlist_enable", "false", false),
    ?assertEqual({200, false}, req(get, ?ACC, Url)).

should_not_return_any_csp_headers_when_disabled(_DbName) ->
    ?_assertEqual(
        undefined,
        begin
            ok = config:set("csp", "utils_enable", "false", false),
            ok = config:set("csp", "enable", "false", false),
            {ok, _, Headers, _} = test_request:get(base_url() ++ "/_utils/"),
            proplists:get_value("Content-Security-Policy", Headers)
        end
    ).

should_apply_default_policy(_DbName) ->
    ?_assertEqual(
        "child-src 'self' data: blob:; default-src 'self'; img-src 'self' data:; font-src 'self'; "
        "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';",
        begin
            {ok, _, Headers, _} = test_request:get(base_url() ++ "/_utils/"),
            proplists:get_value("Content-Security-Policy", Headers)
        end
    ).

should_apply_default_policy_with_legacy_config(_DbName) ->
    ?_assertEqual(
        "child-src 'self' data: blob:; default-src 'self'; img-src 'self' data:; font-src 'self'; "
        "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';",
        begin
            ok = config:set("csp", "utils_enable", "false", false),
            ok = config:set("csp", "enable", "true", false),
            {ok, _, Headers, _} = test_request:get(base_url() ++ "/_utils/"),
            proplists:get_value("Content-Security-Policy", Headers)
        end
    ).

should_return_custom_policy(_DbName) ->
    ?_assertEqual(
        "default-src 'http://example.com';",
        begin
            ok = config:set(
                "csp",
                "utils_header_value",
                "default-src 'http://example.com';",
                false
            ),
            {ok, _, Headers, _} = test_request:get(base_url() ++ "/_utils/"),
            proplists:get_value("Content-Security-Policy", Headers)
        end
    ).

% Utility functions

setup_all() ->
    Ctx = test_util:start_couch([chttpd]),
    Hashed = couch_passwords:hash_admin_password(?ADM_PASS),
    config:set("admins", ?ADM_USER, ?b2l(Hashed), false),
    config:set("log", "level", "debug", false),
    Ctx.

teardown_all(Ctx) ->
    test_util:stop_couch(Ctx).

setup() ->
    UsersDb = ?b2l(?tempdb()),
    config:set("chttpd_auth", "authentication_db", UsersDb, false),
    UsersDbUrl = base_url() ++ "/" ++ UsersDb,
    {201, _} = req(put, ?ADM, UsersDbUrl),
    % Since we're dealing with the auth cache and ets_lru, it's best to just
    % restart the whole application.
    application:stop(chttpd),
    ok = application:start(chttpd, permanent),
    ok = create_user(UsersDb, <<?ACC_USER>>, <<?ACC_PASS>>, []),
    DbName = ?b2l(?tempdb()),
    DbUrl = base_url() ++ "/" ++ DbName,
    {201, _} = req(put, ?ADM, DbUrl),
    ok = create_doc(?ACC, DbName, #{
        <<"_id">> => <<?DOC1>>,
        <<"_attachments">> => #{
            <<?ATT1>> => #{
                <<"data">> => base64:encode(<<"att1_data">>)
            }
        }
    }),
    ok = create_doc(?ADM, DbName, #{
        <<"_id">> => <<?DDOC1>>,
        <<"_attachments">> => #{
            <<?ATT1>> => #{
                <<"data">> => base64:encode(<<"att1_data">>)
            }
        },
        <<"views">> => #{
            <<?VIEW1>> => #{
                <<"map">> => <<"function(doc) {emit(doc._id, doc._rev)}">>
            }
        },
        <<"shows">> => #{
            <<?SHOW1>> => <<"function(doc, req) {return '<h1>show1!</h1>';}">>
        },
        <<"lists">> => #{
            <<?LIST1>> =>
                <<"function(head, req) {", "var row;", "while(row = getRow()){ send(row.key); };",
                    "}">>
        }
    }),
    ok = create_doc(?ACC, DbName, #{<<"_id">> => <<?LDOC1>>}),
    DbName.

cleanup(DbName) ->
    config:delete("csp", "utils_enable", _Persist = false),
    config:delete("csp", "attachments_enable", _Persist = false),
    config:delete("csp", "showlist_enable", _Persist = false),
    DbUrl = base_url() ++ "/" ++ DbName,
    {200, _} = req(delete, ?ADM, DbUrl),
    UsersDb = config:get("chttpd_auth", "authentication_db"),
    config:delete("chttpd_auth", "authentication_db", false),
    UsersDbUrl = base_url() ++ "/" ++ UsersDb,
    {200, _} = req(delete, ?ADM, UsersDbUrl).

base_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

create_user(UsersDb, Name, Pass, Roles) when
    is_list(UsersDb),
    is_binary(Name),
    is_binary(Pass),
    is_list(Roles)
->
    Body = #{
        <<"name">> => Name,
        <<"type">> => <<"user">>,
        <<"roles">> => Roles,
        <<"password_sha">> => hash_password(Pass),
        <<"salt">> => ?SALT
    },
    Url = base_url() ++ "/" ++ UsersDb ++ "/" ++ "org.couchdb.user:" ++ ?b2l(Name),
    {201, _} = req(put, ?ADM, Url, Body),
    ok.

hash_password(Password) when is_binary(Password) ->
    couch_passwords:simple(Password, ?SALT).

create_doc(Auth, DbName, Body) ->
    Url = base_url() ++ "/" ++ DbName,
    {201, _} = req(post, Auth, Url, Body),
    ok.

req(Method, {_, _} = Auth, Url) ->
    Hdrs = [{basic_auth, Auth}],
    {ok, Code, RespHdrs, _} = test_request:request(Method, Url, Hdrs),
    {Code, is_sandboxed(RespHdrs)}.

req(Method, {_, _} = Auth, Url, #{} = Body) ->
    req(Method, {_, _} = Auth, Url, "application/json", #{} = Body).

req(Method, {_, _} = Auth, Url, ContentType, #{} = Body) ->
    Hdrs = [{basic_auth, Auth}, {"Content-Type", ContentType}],
    Body1 = jiffy:encode(Body),
    {ok, Code, RespHdrs, _} = test_request:request(Method, Url, Hdrs, Body1),
    {Code, is_sandboxed(RespHdrs)}.

is_sandboxed(Headers) ->
    lists:member({"Content-Security-Policy", "sandbox"}, Headers).
