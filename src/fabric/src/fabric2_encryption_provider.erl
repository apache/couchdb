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

-module(fabric2_encryption_provider).

-export([
    get_aad/1,
    get_kek/1,
    unwrap_kek/1
]).


-include_lib("syntax_tools/include/merl.hrl").


-define(MEK_CACHE_MODULE, fabric2_encryption_master_key).


get_aad(DbName) ->
    FdbDirs = fabric2_server:fdb_directory(),
    FdbDir = iolist_to_binary(FdbDirs),
    {ok, <<FdbDir/binary, 0:8, DbName/binary>>}.


get_kek(_DbName) ->
    case get_mek() of
        {ok, MEK} ->
            KEK = crypto:strong_rand_bytes(32),
            WrappedKEK = couch_keywrap:key_wrap(MEK, KEK),
            {ok, KEK, WrappedKEK};
        {error, Error} ->
            {error, Error}
    end.


unwrap_kek(WrappedKEK) ->
    case get_mek() of
        {ok, MEK} ->
            case couch_keywrap:key_unwrap(MEK, WrappedKEK) of
                fail ->
                    {error, unable_to_unwrap_kek};
                KEK ->
                    {ok, KEK, WrappedKEK}
            end;
        {error, Error} ->
            {error, Error}
    end.



get_mek() ->
    case erlang:function_exported(?MEK_CACHE_MODULE, get, 0) of
        true ->
            ?MEK_CACHE_MODULE:get();
        false ->
            maybe_cache_mek()
    end.


maybe_cache_mek() ->
    KeyProvider = config:get("encryption", "key_provider"),
    case iolist_to_binary(os:cmd(KeyProvider)) of
        MEK when bit_size(MEK) rem 64 == 0, bit_size(MEK) =< 256 ->
            ModuleName = ?MEK_CACHE_MODULE,
            Module = ?Q("-module('@ModuleName@')."),
            Export = ?Q("-export([get/0])."),
            Function = erl_syntax:function(merl:term(get), [
                ?Q("() -> {ok, _@MEK@}")
            ]),
            merl:compile_and_load([Module, Export, Function],
                [no_spawn_compiler_process, no_line_info]),
            {ok, MEK};
        _ ->
            {error, invalid_key_length}
    end.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


get_mek_failure_test_() ->
    {
        setup,
        fun() ->
            code:delete(?MEK_CACHE_MODULE),
            ok = meck:new([config], [passthrough]),
            ok = meck:expect(config, get, fun("encryption", "key_provider") ->
                "echo 0"
            end)
        end,
        fun(ok) ->
            meck:unload()
        end,
        fun(ok) ->
            [
                {"should return error when failed to acquire mek",
                ?_assertEqual({error, invalid_key_length}, get_mek())}
            ]
        end
    }.


get_unwrap_kek_test_() ->
    {
        setup,
        fun() ->
            ok = meck:new([config], [passthrough]),
            ok = meck:expect(config, get, fun("encryption", "key_provider") ->
                "echo 0000000000000000000000000000000"
            end)
        end,
        fun(ok) ->
            meck:unload()
        end,
        fun(ok) ->
            [
                {"should cache acquired mek",
                fun test_get_mek/0},
                {"should gernerate and wrap kek",
                fun test_get_kek/0},
                {"should unwrap valid wrapped kek",
                fun test_unwrap_kek/0},
                {"should return error on invalid wrapped key",
                ?_assertMatch({error, _}, unwrap_kek(<<0:320>>))}
            ]
        end
    }.


test_get_mek() ->
    {ok, MEK1} = get_mek(),
    {ok, MEK2} = get_mek(),
    ?assertEqual(MEK2, MEK1),
    N1 = meck:num_calls(config, get, ["encryption", "key_provider"]),
    ?assertEqual(1, N1).


test_get_kek() ->
    Resp = get_kek(<<"db">>),
    ?assertMatch({ok, _, _}, Resp),
    {ok, KEK, WrappedKEK} = Resp,
    ?assertEqual(256, bit_size(KEK)),
    ?assertNotEqual(WrappedKEK, KEK).


test_unwrap_kek() ->
    WrappedKEK = <<16#0c714838ba4b937fdde5a2ca8a318ead3c2c49ddfc77eef90e1a954f18962848f601d18f7cf32bb9:320>>,
    Resp = unwrap_kek(WrappedKEK),
    ?assertMatch({ok, _, _}, Resp),
    {ok, KEK, WrappedKEK2} = Resp,
    ?assertEqual(256, bit_size(KEK)),
    ?assertEqual(WrappedKEK, WrappedKEK2),
    ?assertNotEqual(WrappedKEK2, KEK).

-endif.
