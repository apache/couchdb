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

-define(DB, #{uuid => <<0:64>>}).
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
    ok = meck:expect(?AEGIS_KEY_MANAGER, open_db, 1, {ok, <<0:256>>}),
    Ctx.


teardown(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).


test_init_db() ->
    ?assert(aegis_server:init_db(?DB, [])),
    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, init_db, 2)).


test_open_db() ->
    ?assert(aegis_server:open_db(?DB)),
    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).


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
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    ?assert(aegis_server:open_db(?DB)),

    lists:foreach(fun(I) ->
        Encrypted = aegis_server:encrypt(?DB, <<I:64>>, ?VALUE),
        ?assertNotEqual(?VALUE, Encrypted),
        ?assertMatch(<<1:8, _/binary>>, Encrypted)
    end, lists:seq(1, 12)),

    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).


test_encrypt_cache() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    Encrypted = aegis_server:encrypt(?DB, <<1:64>>, ?VALUE),
    ?assertNotEqual(?VALUE, Encrypted),
    ?assertMatch(<<1:8, _/binary>>, Encrypted),

    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).


test_decrypt_cache() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    Decrypted = aegis_server:decrypt(?DB, <<1:64>>, ?ENCRYPTED),
    ?assertEqual(<<0>>, Decrypted),

    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).



disabled_test_() ->
    {
        foreach,
        fun() ->
            Ctx = setup(),
            ok = meck:delete(?AEGIS_KEY_MANAGER, init_db, 2),
            ok = meck:expect(?AEGIS_KEY_MANAGER, init_db, 2, false),
            ok = meck:delete(?AEGIS_KEY_MANAGER, open_db, 1),
            ok = meck:expect(?AEGIS_KEY_MANAGER, open_db, 1, false),
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
    ?assertNot(aegis_server:open_db(?DB)),
    ?assertEqual(1, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).


test_disabled_encrypt() ->
    Db = ?DB#{is_encrypted => aegis_server:open_db(?DB)},
    Encrypted = aegis:encrypt(Db, <<1:64>>, ?VALUE),
    ?assertEqual(?VALUE, Encrypted).


test_disabled_decrypt() ->
    Db = ?DB#{is_encrypted => aegis_server:open_db(?DB)},
    Decrypted = aegis:decrypt(Db, <<1:64>>, ?ENCRYPTED),
    ?assertEqual(?ENCRYPTED, Decrypted).



lru_cache_with_expiration_test_() ->
    {
        foreach,
        fun() ->
            %% this has to be be set before start of aegis server
            %% for config param "cache_expiration_check_sec" to be picked up
            meck:new([config, aegis_server, fabric2_util], [passthrough]),
            ok = meck:expect(config, get_integer, fun
                ("aegis", "cache_limit", _) -> 5;
                ("aegis", "cache_max_age_sec", _) -> 130;
                ("aegis", "cache_expiration_check_sec", _) -> 1;
                (_, _, Default) -> Default
            end),
            Ctx = setup(),
            ok = meck:expect(fabric2_util, now, fun(sec) ->
                get(time) == undefined andalso put(time, 10),
                Now = get(time),
                put(time, Now + 10),
                Now
            end),
            Ctx
        end,
        fun teardown/1,
        [
            {"counter moves forward on access bump",
            {timeout, ?TIMEOUT, fun test_advance_counter/0}},
            {"oldest entries evicted",
            {timeout, ?TIMEOUT, fun test_evict_old_entries/0}},
            {"access bump preserves entries",
            {timeout, ?TIMEOUT, fun test_bump_accessed/0}},
            {"expired entries removed",
            {timeout, ?TIMEOUT, fun test_remove_expired/0}}
        ]
    }.


test_advance_counter() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    ok = meck:expect(aegis_server, handle_cast, fun({accessed, _} = Msg, St) ->
        #{counter := Counter} = St,
        get(counter) == undefined andalso put(counter, 0),
        OldCounter = get(counter),
        put(counter, Counter),
        ?assert(Counter > OldCounter),
        meck:passthrough([Msg, St])
    end),

    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<I:64>>, ?VALUE),
        aegis_server:encrypt(Db, <<(I+1):64>>, ?VALUE)
    end, lists:seq(1, 10)),

    ?assertEqual(10, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).


test_evict_old_entries() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% overflow cache
    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<I:64>>, ?VALUE)
    end, lists:seq(1, 10)),

    ?assertEqual(10, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% confirm that newest keys are still in cache
    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<(I+1):64>>, ?VALUE)
    end, lists:seq(6, 10)),

    ?assertEqual(10, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% confirm that oldest keys been eviced and needed re-fetch
    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<(I+1):64>>, ?VALUE)
    end, lists:seq(1, 5)),

    ?assertEqual(15, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).


test_bump_accessed() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% fill the cache
    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<I:64>>, ?VALUE)
    end, lists:seq(1, 5)),

    ?assertEqual(5, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% bump oldest key and then insert a new key to trigger eviction
    aegis_server:encrypt(?DB#{uuid => <<1:64>>}, <<1:64>>, ?VALUE),
    aegis_server:encrypt(?DB#{uuid => <<6:64>>}, <<6:64>>, ?VALUE),
    ?assertEqual(6, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% confirm that former oldest key is still in cache
    aegis_server:encrypt(?DB#{uuid => <<1:64>>}, <<2:64>>, ?VALUE),
    ?assertEqual(6, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% confirm that the second oldest key been evicted by the new insert
    aegis_server:encrypt(?DB#{uuid => <<2:64>>}, <<3:64>>, ?VALUE),
    ?assertEqual(7, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).


test_remove_expired() ->
    ?assertEqual(0, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% to detect when maybe_remove_expired called
    ok = meck:expect(aegis_server, handle_info,fun
        (maybe_remove_expired, St) ->
            meck:passthrough([maybe_remove_expired, St])
    end),

    %% fill the cache. first key expires a 140, last at 180 of "our" time
    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<I:64>>, ?VALUE)
    end, lists:seq(1, 5)),

    ?assertEqual(5, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% confirm enties are still in cache and wind up our "clock" to 160
    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<I:64>>, ?VALUE)
    end, lists:seq(1, 5)),

    ?assertEqual(5, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)),

    %% wait for remove_expired_entries to be triggered
    meck:reset(aegis_server),
    meck:wait(aegis_server, handle_info, [maybe_remove_expired, '_'], 2500),

    %% 3 "oldest" entries should be removed, 2 yet to expire still in cache
    lists:foreach(fun(I) ->
        Db = ?DB#{uuid => <<I:64>>},
        aegis_server:encrypt(Db, <<I:64>>, ?VALUE)
    end, lists:seq(1, 5)),

    ?assertEqual(8, meck:num_calls(?AEGIS_KEY_MANAGER, open_db, 1)).
