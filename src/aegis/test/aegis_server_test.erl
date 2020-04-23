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

-module(aegis_server_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(DB, #{uuid => <<0:64>>, db_options => [], user_ctx => []}).
-define(VALUE, <<0:8>>).
-define(ENCRYPTED, <<1,155,242,89,190,54,112,151,18,145,25,251,217,
    49,147,125,14,162,146,201,189,100,232,38,239,111,163,84,25,60,
    147,167,237,107,24,204,171,232,227,16,72,203,101,118,150,252,
    204,80,245,66,98,213,223,63,111,105,101,154>>).
-define(TIMEOUT, 10000).



basic_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            {"init_db returns true when encryption enabled",
            {timeout, ?TIMEOUT, fun test_init_db/0}},
            {"open_db returns true when encryption enabled",
            {timeout, ?TIMEOUT, fun test_open_db/0}},
            {"init_db caches key",
            {timeout, ?TIMEOUT, fun test_init_db_cache/0}},
            {"open_db caches key",
            {timeout, ?TIMEOUT, fun test_open_db_cache/0}},
            {"encrypt fetches and caches key when it's missing",
            {timeout, ?TIMEOUT, fun test_encrypt_cache/0}},
            {"decrypt fetches and caches key when it's missing",
            {timeout, ?TIMEOUT, fun test_decrypt_cache/0}}
        ]
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    meck:new([?AEGIS_KEY_MANAGER], [passthrough]),
    ok = meck:expect(?AEGIS_KEY_MANAGER, init_db, 2, {ok, <<0:256>>}),
    ok = meck:expect(?AEGIS_KEY_MANAGER, open_db, 2, {ok, <<0:256>>}),
    Ctx.


teardown(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).


test_init_db() ->
    ?assert(aegis_server:init_db(?DB, [])),
    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, init_db, 2)).


test_open_db() ->
    ?assert(aegis_server:open_db(?DB, [])),
    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)).


test_init_db_cache() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, init_db, 2)),

    ?assert(aegis_server:init_db(?DB, [])),

    lists:foreach(fun(I) ->
        Encrypted = aegis_server:encrypt(?DB, <<I:64>>, ?VALUE),
        ?assertNotEqual(?VALUE, Encrypted),
        ?assertMatch(<<1:8, _/binary>>, Encrypted)
    end, lists:seq(1, 12)),

    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, init_db, 2)).


test_open_db_cache() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)),

    ?assert(aegis_server:open_db(?DB, [])),

    lists:foreach(fun(I) ->
        Encrypted = aegis_server:encrypt(?DB, <<I:64>>, ?VALUE),
        ?assertNotEqual(?VALUE, Encrypted),
        ?assertMatch(<<1:8, _/binary>>, Encrypted)
    end, lists:seq(1, 12)),

    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)).


test_encrypt_cache() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)),

    Encrypted = aegis_server:encrypt(?DB, <<1:64>>, ?VALUE),
    ?assertNotEqual(?VALUE, Encrypted),
    ?assertMatch(<<1:8, _/binary>>, Encrypted),

    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)).


test_decrypt_cache() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)),

    Decrypted = aegis_server:decrypt(?DB, <<1:64>>, ?ENCRYPTED),
    ?assertEqual(<<0>>, Decrypted),

    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)).



disabled_test_() ->
    {
        foreach,
        fun() ->
            Ctx = setup(),
            ok = meck:delete(?AEGIS_KEY_MANAGER, init_db, 2),
            ok = meck:expect(?AEGIS_KEY_MANAGER, init_db, 2, false),
            ok = meck:delete(?AEGIS_KEY_MANAGER, open_db, 2),
            ok = meck:expect(?AEGIS_KEY_MANAGER, open_db, 2, false),
            Ctx
        end,
        fun teardown/1,
        [
            {"init_db returns false when encryptions disabled",
            {timeout, ?TIMEOUT, fun test_disabled_init_db/0}},
            {"open_db returns false when encryptions disabled",
            {timeout, ?TIMEOUT, fun test_disabled_open_db/0}},
            {"pass through on encrypt when encryption disabled",
            {timeout, ?TIMEOUT, fun test_disabled_encrypt/0}},
            {"pass through on decrypt when encryption disabled",
            {timeout, ?TIMEOUT, fun test_disabled_decrypt/0}}
        ]
    }.


test_disabled_init_db() ->
    ?assertNot(aegis_server:init_db(?DB, [])),
    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, init_db, 2)).


test_disabled_open_db() ->
    ?assertNot(aegis_server:open_db(?DB, [])),
    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 2)).


test_disabled_encrypt() ->
    Db = ?DB#{is_encrypted => aegis_server:open_db(?DB, [])},
    Encrypted = aegis:encrypt(Db, <<1:64>>, ?VALUE),
    ?assertEqual(?VALUE, Encrypted).


test_disabled_decrypt() ->
    Db = ?DB#{is_encrypted => aegis_server:open_db(?DB, [])},
    Decrypted = aegis:decrypt(Db, <<1:64>>, ?ENCRYPTED),
    ?assertEqual(?ENCRYPTED, Decrypted).
