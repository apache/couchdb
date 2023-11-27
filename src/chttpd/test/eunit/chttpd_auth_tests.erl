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

-module(chttpd_auth_tests).

-define(WORKING_HASHES, "sha256, sha512, sha, blake2s").
-define(FAILING_HASHES, "md4, md5, ripemd160").

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    BaseUrl = lists:concat(["http://", Addr, ":", Port]),
    BaseUrl.

teardown(_Url) ->
    ok.

setup_proxy_auth() ->
    {StartCtx, ProxyCfgFile} = start_couch_with_cfg("{chttpd_auth, proxy_authentication_handler}"),
    config:set("chttpd", "require_valid_user", "false", false),
    config:set("chttpd_auth", "hash_algorithms", ?WORKING_HASHES, false),
    config:set("chttpd_auth", "proxy_use_secret", "true", false),
    config:set("chttpd_auth", "secret", "the_secret", false),
    HashesShouldWork = re:split(?WORKING_HASHES, "\\s*,\\s*", [
        trim, {return, binary}
    ]),
    HashesShouldFail = re:split(?FAILING_HASHES, "\\s*,\\s*", [trim, {return, binary}]),
    SupportedHashAlgorithms = crypto:supports(hashs),
    {{StartCtx, ProxyCfgFile}, HashesShouldWork, HashesShouldFail, SupportedHashAlgorithms}.

teardown_proxy_auth({{Ctx, ProxyCfgFile}, _, _, _}) ->
    config:delete("chttpd_auth", "hash_algorithms", false),
    config:delete("chttpd_auth", "secret", false),
    config:delete("chttpd_auth", "proxy_use_secret", false),
    config:delete("chttpd", "require_valid_user", false),
    ok = file:delete(ProxyCfgFile),
    test_util:stop_couch(Ctx).

require_valid_user_exception_test_() ->
    {
        "_up",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_handle_require_valid_user_except_up_on_up_route/1,
                    fun should_handle_require_valid_user_except_up_on_non_up_routes/1
                ]
            }
        }
    }.

proxy_auth_test_() ->
    {
        "Testing hash algorithms for proxy auth",
        {
            setup,
            fun setup_proxy_auth/0,
            fun teardown_proxy_auth/1,
            with([
                ?TDEF(test_hash_algorithms_with_proxy_auth_should_work),
                ?TDEF(test_hash_algorithms_with_proxy_auth_should_fail)
            ])
        }
    }.

set_require_user_false() ->
    ok = config:set("chttpd", "require_valid_user", "false", _Persist = false).

set_require_user_true() ->
    ok = config:set("chttpd", "require_valid_user", "true", _Persist = false).

set_require_user_except_for_up_false() ->
    ok = config:set("chttpd", "require_valid_user_except_for_up", "false", _Persist = false).

set_require_user_except_for_up_true() ->
    ok = config:set("chttpd", "require_valid_user_except_for_up", "true", _Persist = false).

should_handle_require_valid_user_except_up_on_up_route(_Url) ->
    ?_test(begin
        %    require_valid_user |   require_valid_user_except_up | up needs auth
        % 1  F                  |   F                            | F
        % 2  F                  |   T                            | F
        % 3  T                  |   F                            | T
        % 4  T                  |   T                            | F

        UpRequest = #httpd{path_parts = [<<"_up">>]},
        % we use ?ADMIN_USER here because these tests run under admin party
        % so this is equivalent to an unauthenticated request
        ExpectAuth = {unauthorized, <<"Authentication required.">>},
        ExpectNoAuth = #httpd{user_ctx = ?ADMIN_USER, path_parts = [<<"_up">>]},

        % 1
        set_require_user_false(),
        set_require_user_except_for_up_false(),
        Result1 = chttpd_auth:party_mode_handler(UpRequest),
        ?assertEqual(ExpectNoAuth, Result1),

        % 2
        set_require_user_false(),
        set_require_user_except_for_up_true(),
        Result2 = chttpd_auth:party_mode_handler(UpRequest),
        ?assertEqual(ExpectNoAuth, Result2),

        % 3
        set_require_user_true(),
        set_require_user_except_for_up_false(),
        ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(UpRequest)),

        % 4
        set_require_user_true(),
        set_require_user_except_for_up_true(),
        Result4 = chttpd_auth:party_mode_handler(UpRequest),
        ?assertEqual(ExpectNoAuth, Result4)
    end).

should_handle_require_valid_user_except_up_on_non_up_routes(_Url) ->
    ?_test(begin
        %    require_valid_user |  require_valid_user_except_up | everything not _up requires auth
        % 5  F                  |  F                            | F
        % 6  F                  |  T                            | T
        % 7  T                  |  F                            | T
        % 8  T                  |  T                            | T

        NonUpRequest = #httpd{path_parts = [<<"/">>]},
        ExpectAuth = {unauthorized, <<"Authentication required.">>},
        ExpectNoAuth = #httpd{user_ctx = ?ADMIN_USER, path_parts = [<<"/">>]},
        % 5
        set_require_user_false(),
        set_require_user_except_for_up_false(),
        Result5 = chttpd_auth:party_mode_handler(NonUpRequest),
        ?assertEqual(ExpectNoAuth, Result5),

        % 6
        set_require_user_false(),
        set_require_user_except_for_up_true(),
        ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(NonUpRequest)),

        % 7
        set_require_user_true(),
        set_require_user_except_for_up_false(),
        ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(NonUpRequest)),

        % 8
        set_require_user_true(),
        set_require_user_except_for_up_true(),
        ?assertThrow(ExpectAuth, chttpd_auth:party_mode_handler(NonUpRequest))
    end).

% Helper functions
base_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    "http://" ++ Addr ++ ":" ++ Port.

append_to_cfg_chain(Cfg) ->
    CfgDir = filename:dirname(lists:last(?CONFIG_CHAIN)),
    CfgFile = filename:join([CfgDir, "chttpd_proxy_auth_cfg.ini"]),
    CfgSect = io_lib:format("[chttpd]~nauthentication_handlers = ~s~n", [Cfg]),
    ok = file:write_file(CfgFile, CfgSect),
    ?CONFIG_CHAIN ++ [CfgFile].

start_couch_with_cfg(Cfg) ->
    CfgChain = append_to_cfg_chain(Cfg),
    StartCtx = test_util:start_couch(CfgChain, [chttpd]),
    ProxyCfgFile = lists:last(CfgChain),
    {StartCtx, ProxyCfgFile}.

% Test functions
test_hash_algorithm([]) ->
    ok;
test_hash_algorithm([DefaultHashAlgorithm | DecodingHashAlgorithmsList] = _) ->
    Secret = chttpd_util:get_chttpd_auth_config("secret"),
    Token = couch_util:to_hex(couch_util:hmac(DefaultHashAlgorithm, Secret, "PROXY-USER")),
    Headers = [
        {"X-Auth-CouchDB-UserName", "PROXY-USER"},
        {"X-Auth-CouchDB-Roles", "PROXY-USER-ROLE1, PROXY-USER-ROLE2"},
        {"X-Auth-CouchDB-Token", Token}
    ],
    {ok, RespStatus, _, RespBody} = test_request:get(base_url() ++ "/_session", Headers),
    ?assertMatch({200, _}, {RespStatus, RespBody}),
    IsAuthenticatedViaProxy = couch_util:get_nested_json_value(
        jiffy:decode(RespBody), [<<"info">>, <<"authenticated">>]
    ),
    ?assertEqual(IsAuthenticatedViaProxy, <<"proxy">>),
    test_hash_algorithm(DecodingHashAlgorithmsList).

test_hash_algorithms_with_proxy_auth_should_work(
    {_Ctx, WorkingHashes, _FailingHashes, SupportedHashAlgorithms} = _
) ->
    Hashes = couch_util:verify_hash_names(WorkingHashes, SupportedHashAlgorithms),
    test_hash_algorithm(Hashes).

test_hash_algorithms_with_proxy_auth_should_fail(
    {_Ctx, _WorkingHashes, FailingHashes, SupportedHashAlgorithms} = _
) ->
    Hashes = couch_util:verify_hash_names(FailingHashes, SupportedHashAlgorithms),
    ?assertThrow({not_found, _}, test_hash_algorithm(Hashes)).
