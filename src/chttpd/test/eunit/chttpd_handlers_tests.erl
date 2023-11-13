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

-module(chttpd_handlers_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_replicate_handler_test").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).

replicate_endpoint_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(should_escape_local_dbname_on_replicate),
            ?TDEF_FE(require_authenticated_user)
        ]
    }.

should_escape_local_dbname_on_replicate({_Ctx, Url}) ->
    SrcPrefix = ?tempdb(),
    TgtPrefix = ?tempdb(),
    SrcDb = <<SrcPrefix/binary, "%2Fsrc">>,
    TgtDb = <<TgtPrefix/binary, "%2Ftgt">>,
    create_db(Url, SrcDb),
    post(Url ++ binary_to_list(SrcDb), [?AUTH], #{}),
    create_db(Url, TgtDb),
    Res = post(Url ++ "_replicate", [?AUTH], #{
        <<"source">> => endpoint(Url, SrcDb, ?USER, ?PASS),
        <<"target">> => endpoint("", <<TgtPrefix/binary, "/tgt">>, ?USER, ?PASS)
    }),
    ?assertMatch({200, #{<<"ok">> := true}}, Res),
    delete_db(Url, SrcDb),
    delete_db(Url, TgtDb).

require_authenticated_user({_Ctx, Url}) ->
    SrcDb = ?tempdb(),
    TgtDb = ?tempdb(),
    create_db(Url, SrcDb),
    post(Url ++ binary_to_list(SrcDb), [?AUTH], #{}),
    create_db(Url, TgtDb),

    % Try as an unauthenticated user
    ResNoAuth = post(Url ++ "_replicate", [], #{
        <<"source">> => endpoint(Url, SrcDb, ?USER, ?PASS),
        <<"target">> => endpoint(Url, TgtDb, ?USER, ?PASS)
    }),
    ?assertMatch(
        {401, #{
            <<"error">> := <<"unauthorized">>,
            <<"reason">> := <<"You are not an authenticated user">>
        }},
        ResNoAuth
    ),

    % Now try as an authenticated user
    ResAuth = post(Url ++ "_replicate", [?AUTH], #{
        <<"source">> => endpoint(Url, SrcDb, ?USER, ?PASS),
        <<"target">> => endpoint(Url, TgtDb, ?USER, ?PASS)
    }),
    ?assertMatch({200, #{<<"ok">> := true}}, ResAuth),
    delete_db(Url, SrcDb),
    delete_db(Url, TgtDb).

endpoint(Url, Db, User, Pass) ->
    UrlBin = list_to_binary(Url),
    #{
        <<"url">> => <<UrlBin/binary, Db/binary>>,
        <<"auth">> => #{
            <<"basic">> => #{
                <<"username">> => list_to_binary(User),
                <<"password">> => list_to_binary(Pass)
            }
        }
    }.

setup() ->
    Ctx = test_util:start_couch([chttpd, couch_replicator]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    {Ctx, Url}.

teardown({Ctx, _Url}) ->
    ok = config:delete("admins", ?USER, _Persist = false),
    test_util:stop_couch(Ctx).

create_db(Top, Db) when is_binary(Db) ->
    Url = Top ++ binary_to_list(Db) ++ "?q=1",
    {ok, Status, _, _} = test_request:put(Url, [?JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Top, Db) when is_binary(Db) ->
    Url = Top ++ binary_to_list(Db),
    case test_request:get(Url, [?AUTH]) of
        {ok, 404, _, _} ->
            not_found;
        {ok, 200, _, _} ->
            {ok, 200, _, _} = test_request:delete(Url, [?AUTH]),
            ok
    end.

post(Url, Headers0, #{} = Body) when is_list(Headers0), is_list(Url) ->
    BodyBin = jiffy:encode(Body),
    {ok, Code, _, Res} = test_request:request(post, Url, [?JSON | Headers0], BodyBin),
    {Code, jiffy:decode(Res, [return_maps])}.
