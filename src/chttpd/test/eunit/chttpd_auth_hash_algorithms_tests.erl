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
-module(chttpd_auth_hash_algorithms_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("chttpd/test/eunit/chttpd_test.hrl").

-define(ADM_USER, "adm_user").
-define(ADM_PASS, "adm_pass").
-define(ALLOWED_HASHES, "sha256, sha512, sha, blake2s").
-define(DISALLOWED_HASHES, "md4, md5, ripemd160").

hash_algorithms_test_() ->
    {
        "Testing hash algorithms for cookie auth",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(test_hash_algorithms_should_work),
                ?TDEF_FE(test_hash_algorithms_should_fail)
            ]
        }
    }.

% Test utility functions
setup() ->
    Ctx = test_util:start_couch([chttpd]),
    Hashed = couch_passwords:hash_admin_password(?ADM_PASS),
    NewSecret = ?b2l(couch_uuids:random()),
    config:set("admins", ?ADM_USER, ?b2l(Hashed), false),
    config:set("chttpd_auth", "secret", NewSecret, false),
    config:set("chttpd", "require_valid_user", "true", false),
    config:set("chttpd_auth", "hash_algorithms", ?ALLOWED_HASHES, false),
    AllowedHashes = re:split(config:get("chttpd_auth", "hash_algorithms"), "\\s*,\\s*", [
        trim, {return, binary}
    ]),
    DisallowedHashes = re:split(?DISALLOWED_HASHES, "\\s*,\\s*", [trim, {return, binary}]),
    {Ctx, {AllowedHashes, DisallowedHashes}}.

teardown({Ctx, _}) ->
    config:delete("chttpd_auth", "hash_algorithms", false),
    config:delete("chttpd", "require_valid_user", false),
    config:delete("chttpd_auth", "secret", false),
    config:delete("admins", ?ADM_USER, false),
    test_util:stop_couch(Ctx).

% Helper functions
base_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

make_auth_session_string(HashAlgorithm, User, Secret, TimeStamp) ->
    SessionData = User ++ ":" ++ erlang:integer_to_list(TimeStamp, 16),
    Hash = couch_util:hmac(HashAlgorithm, Secret, SessionData),
    "AuthSession=" ++ couch_util:encodeBase64Url(SessionData ++ ":" ++ ?b2l(Hash)).

get_user_props(User) ->
    couch_auth_cache:get_user_creds(User).

get_full_secret(User) ->
    {ok, UserProps, _AuthCtx} = get_user_props(User),
    UserSalt = couch_util:get_value(<<"salt">>, UserProps, <<"">>),
    Secret = ?l2b(chttpd_util:get_chttpd_auth_config("secret")),
    <<Secret/binary, UserSalt/binary>>.

% Test functions
test_hash_algorithm([], _) ->
    ok;
test_hash_algorithm([DefaultHashAlgorithm | DecodingHashAlgorithmsList] = _, Status) ->
    CurrentTime = couch_httpd_auth:make_cookie_time(),
    Cookie = make_auth_session_string(
        erlang:binary_to_existing_atom(DefaultHashAlgorithm),
        ?ADM_USER,
        get_full_secret(?ADM_USER),
        CurrentTime
    ),
    {ok, ReqStatus, _, _} = test_request:request(get, base_url(), [{cookie, Cookie}]),
    ?assertEqual(Status, ReqStatus),
    test_hash_algorithm(DecodingHashAlgorithmsList, Status).

test_hash_algorithms_should_work({_, {AllowedHashes, _}} = _) ->
    test_hash_algorithm(AllowedHashes, 200).

test_hash_algorithms_should_fail({_, {_, DisallowedHashes}} = _) ->
    test_hash_algorithm(DisallowedHashes, 401).
