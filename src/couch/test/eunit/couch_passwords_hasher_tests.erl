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

-module(couch_passwords_hasher_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(USER, "couch_passwords_hash_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(RANDOM_USER, "user-" ++ ?b2l(couch_uuids:random())).
-define(MOD, couch_password_hasher).

setup(Scheme) ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    config:set("admins", ?USER, ?b2l(Hashed), false),
    Db = ?b2l(?tempdb()),
    create_db(Db),
    config:set("chttpd_auth", "authentication_db", Db, false),
    config:set("chttpd_auth", "password_scheme", Scheme, false),
    meck:new(?MOD, [passthrough]),
    Db.

teardown(_, Db) ->
    delete_db(Db),
    config:delete("admins", ?USER, false),
    config:delete("chttpd_auth", "authentication_db", false),
    config:delete("chttpd_auth", "password_scheme", false),
    meck:unload().

couch_password_hasher_test_() ->
    {
        "couch_password_hasher tests",
        {
            setup,
            fun() -> test_util:start_couch([chttpd]) end,
            fun test_util:stop_couch/1,
            [
                upgrade_password_hash_tests("simple"),
                upgrade_password_hash_tests("pbkdf2")
            ]
        }
    }.

upgrade_password_hash_tests(Scheme) ->
    {
        "password scheme " ++ Scheme ++ " tests",
        foreachx,
        fun setup/1,
        fun teardown/2,
        [
            {Scheme, Test}
         || Test <-
                [
                    fun create_user_by_admin_should_not_upgrade_password_hash/2,
                    fun request_by_user_should_not_upgrade_password_hash/2,
                    fun update_user_password_by_user_should_not_upgrade_password_hash/2
                ]
        ]
    }.

create_user_by_admin_should_not_upgrade_password_hash(_, Db) ->
    ?_test(begin
        meck:reset(?MOD),
        User = ?RANDOM_USER,
        create_user(Db, User, ?PASS),
        ?assertNot(
            meck:called(?MOD, handle_cast, [
                {upgrade_password_hash, '_', ?l2b(User), '_', '_', '_', '_'}, '_'
            ])
        )
    end).

request_by_user_should_not_upgrade_password_hash(_, Db) ->
    ?_test(begin
        User = ?RANDOM_USER,
        create_user(Db, User, ?PASS),
        {200, _} = req(get, url(Db, "org.couchdb.user:" ++ User)),
        ?assertNot(
            meck:called(?MOD, handle_cast, [
                {upgrade_password_hash, '_', ?l2b(User), '_', '_', '_', '_'}, '_'
            ])
        ),

        meck:reset(?MOD),
        Headers = [{basic_auth, {User, ?PASS}}],
        {200, _} = req(get, url(), Headers, []),
        ?assertNot(
            meck:called(?MOD, handle_cast, [
                {upgrade_password_hash, '_', ?l2b(User), '_', '_', '_', '_'}, '_'
            ])
        )
    end).

update_user_password_by_user_should_not_upgrade_password_hash(_, Db) ->
    ?_test(begin
        User = ?RANDOM_USER,
        ?debugVal(User),
        create_user(Db, User, ?PASS),
        {200, #{<<"_rev">> := Rev}} = req(get, url(Db, "org.couchdb.user:" ++ User)),
        ?assertNot(
            meck:called(?MOD, handle_cast, [
                {upgrade_password_hash, '_', ?l2b(User), '_', '_', '_', '_'}, '_'
            ])
        ),

        meck:reset(?MOD),
        NewPass = "new_password",
        update_password(Db, User, NewPass, ?b2l(Rev)),
        ?assertNot(
            meck:called(?MOD, handle_cast, [
                {upgrade_password_hash, '_', ?l2b(User), '_', '_', '_', '_'}, '_'
            ])
        ),

        OldAuth = [{basic_auth, {User, ?PASS}}],
        {401, _} = req(get, url(), OldAuth, []),
        ?assertNot(
            meck:called(?MOD, handle_cast, [
                {upgrade_password_hash, '_', ?l2b(User), '_', '_', '_', '_'}, '_'
            ])
        ),
        NewAuth = [{basic_auth, {User, NewPass}}],
        {200, _} = req(get, url(), NewAuth, []),
        ?assertNot(
            meck:called(?MOD, handle_cast, [
                {upgrade_password_hash, '_', ?l2b(User), '_', '_', '_', '_'}, '_'
            ])
        )
    end).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port]).

url(Db) ->
    url() ++ "/" ++ Db.

url(Db, Path) ->
    url(Db) ++ "/" ++ Path.

create_db(Db) ->
    case req(put, url(Db)) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_test_db, Db, Error})
    end.

delete_db(Db) ->
    case req(delete, url(Db)) of
        {200, #{}} -> ok;
        Error -> error({failed_to_delete_test_db, Db, Error})
    end.

create_user(Db, UserName, Password) ->
    ok = couch_auth_cache:ensure_users_db_exists(),
    User = ?l2b(UserName),
    Pass = ?l2b(Password),
    case req(put, url(Db, "org.couchdb.user:" ++ UserName), user_doc(User, Pass)) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_user, UserName, Error})
    end.

update_password(Db, UserName, NewPassword, Rev) ->
    User = ?l2b(UserName),
    NewPass = ?l2b(NewPassword),
    Headers = [?AUTH, {"If-Match", Rev}],
    case req(put, url(Db, "org.couchdb.user:" ++ UserName), Headers, user_doc(User, NewPass)) of
        {201, #{}} -> ok;
        Error -> error({failed_to_update_password, UserName, Error})
    end.

user_doc(User, Pass) ->
    jiffy:encode(
        {[
            {<<"name">>, User},
            {<<"password">>, Pass},
            {<<"roles">>, []},
            {<<"type">>, <<"user">>}
        ]}
    ).

req(Method, Url) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, Body) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, Headers, Body) ->
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.
