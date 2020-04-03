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

-module(fabric2_encryption_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


-define(PLUGIN, fabric2_encryption_provider).
-define(WK, <<0:256>>).
-define(FIXTURE, #{
    db => <<"db">>,
    doc_id => <<"0001">>,
    doc_rev => <<"1-abcdef">>,
    val => term_to_binary({[], [], false}),
    enc => <<16#32AA68545ACE6A71466E25089101C5BC378019F304582A9FF9BD4A6F4F:232>>
}).


encryption_basic_test_() ->
    {
        setup,
        fun basic_setup/0,
        fun teardown/1,
        [
            fun test_get_wrapped_kek/0,
            fun test_basic_encrypt/0,
            fun test_basic_decrypt/0
        ]
    }.


basic_setup() ->
    Ctx = test_util:start_couch([fabric]),
    meck:new([?PLUGIN], [passthrough]),
    ok = meck:expect(?PLUGIN, get_kek, 1, {ok, ?WK, ?WK}),
    ok = meck:expect(?PLUGIN, unwrap_kek, 2, {ok, ?WK, ?WK}),
    Ctx.


teardown(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).


test_get_wrapped_kek() ->
    #{
        db := DbName
    } = ?FIXTURE,

    Result = fabric2_encryption:get_wrapped_kek(DbName),
    ?assertMatch({ok, _}, Result),

    {ok, WrappedKEK} = Result,
    ?assertEqual(?WK, WrappedKEK).


test_basic_encrypt() ->
    #{
        db := DbName,
        doc_id := DocId,
        doc_rev := DocRev,
        val := Value,
        enc := Expected
    } = ?FIXTURE,

    Result = fabric2_encryption:encrypt(?WK, DbName, DocId, DocRev, Value),
    ?assertMatch({ok, _}, Result),

    {ok, Encrypted} = Result,
    ?assertNotEqual(Value, Encrypted),
    ?assertEqual(Expected, Encrypted).


test_basic_decrypt() ->
    #{
        db := DbName,
        doc_id := DocId,
        doc_rev := DocRev,
        val := Value,
        enc := Expected
    } = ?FIXTURE,

    Result = fabric2_encryption:decrypt(?WK, DbName, DocId, DocRev, Expected),
    ?assertMatch({ok, _}, Result),

    {ok, Decrypted} = Result,
    ?assertNotEqual(Expected, Decrypted),
    ?assertEqual(Value, Decrypted).



encryption_cache_test_() ->
    {
        foreach,
        fun cache_setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(test_cache_encrypt),
            ?TDEF_FE(test_cache_decrypt)
        ]
    }.


cache_setup() ->
    Ctx = test_util:start_couch([fabric]),
    meck:new([?PLUGIN], [passthrough]),
    ok = meck:expect(?PLUGIN, unwrap_kek, 2, {ok, ?WK, ?WK}),
    Ctx.


test_cache_encrypt(_) ->
    #{
        db := DbName,
        doc_id := DocId,
        doc_rev := DocRev,
        val := Value,
        enc := Expected
    } = ?FIXTURE,

    {ok, V1} = fabric2_encryption:encrypt(?WK, DbName, DocId, DocRev, Value),
    {ok, V2} = fabric2_encryption:encrypt(?WK, DbName, DocId, DocRev, Value),

    ?assertEqual(Expected, V1),
    ?assertEqual(Expected, V2),
    ?assertEqual(1, meck:num_calls(?PLUGIN, unwrap_kek, 2)).


test_cache_decrypt(_) ->
    #{
        db := DbName,
        doc_id := DocId,
        doc_rev := DocRev,
        val := Value,
        enc := Expected
    } = ?FIXTURE,

    {ok, V1} = fabric2_encryption:decrypt(?WK, DbName, DocId, DocRev, Expected),
    {ok, V2} = fabric2_encryption:decrypt(?WK, DbName, DocId, DocRev, Expected),

    ?assertEqual(Value, V1),
    ?assertEqual(Value, V2),
    ?assertEqual(1, meck:num_calls(?PLUGIN, unwrap_kek, 2)).



encryption_failure_test_() ->
    {
        foreach,
        fun faiure_setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(test_failure_encrypt),
            ?TDEF_FE(test_failure_decrypt)
        ]
    }.


faiure_setup() ->
    Ctx = test_util:start_couch([fabric]),
    meck:new([?PLUGIN], [passthrough]),
    ok = meck:expect(?PLUGIN, unwrap_kek, 2, {error, unable_to_unwrap_kek}),
    Ctx.


test_failure_encrypt(_) ->
    #{
        db := DbName,
        doc_id := DocId,
        doc_rev := DocRev,
        val := Value
    } = ?FIXTURE,

    Result1 = fabric2_encryption:encrypt(?WK, DbName, DocId, DocRev, Value),
    Result2 = fabric2_encryption:encrypt(?WK, DbName, DocId, DocRev, Value),

    %% make sure we don't overwrap error messages and don't cache the errors
    ?assertEqual({error, unable_to_unwrap_kek}, Result1),
    ?assertEqual({error, unable_to_unwrap_kek}, Result2),
    ?assertEqual(2, meck:num_calls(?PLUGIN, unwrap_kek, 2)).


test_failure_decrypt(_) ->
    #{
        db := DbName,
        doc_id := DocId,
        doc_rev := DocRev,
        enc := Expected
    } = ?FIXTURE,

    Result1 = fabric2_encryption:decrypt(?WK, DbName, DocId, DocRev, Expected),
    Result2 = fabric2_encryption:decrypt(?WK, DbName, DocId, DocRev, Expected),

    ?assertEqual({error, unable_to_unwrap_kek}, Result1),
    ?assertEqual({error, unable_to_unwrap_kek}, Result2),
    ?assertEqual(2, meck:num_calls(?PLUGIN, unwrap_kek, 2)).
