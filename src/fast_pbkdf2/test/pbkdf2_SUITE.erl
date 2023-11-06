-module(pbkdf2_SUITE).

%% API
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% test cases
-export([
    erlang_and_nif_are_equivalent_sha1/1,
    erlang_and_nif_are_equivalent_sha224/1,
    erlang_and_nif_are_equivalent_sha256/1,
    erlang_and_nif_are_equivalent_sha384/1,
    erlang_and_nif_are_equivalent_sha512/1
]).
-export([
    test_vector_sha1_1/1,
    test_vector_sha1_2/1,
    test_vector_sha1_3/1,
    test_vector_sha1_4/1,
    test_vector_sha1_5/1,
    test_vector_sha256_1/1,
    test_vector_sha256_2/1,
    test_vector_sha256_3/1,
    test_vector_sha256_4/1,
    test_vector_sha256_5/1,
    test_vector_sha256_6/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        {group, equivalents},
        {group, test_vectors}
    ].

groups() ->
    [
        {equivalents, [parallel], [
            erlang_and_nif_are_equivalent_sha1,
            erlang_and_nif_are_equivalent_sha224,
            erlang_and_nif_are_equivalent_sha256,
            erlang_and_nif_are_equivalent_sha384,
            erlang_and_nif_are_equivalent_sha512
        ]},
        {test_vectors, [parallel], [
            test_vector_sha1_1,
            test_vector_sha1_2,
            test_vector_sha1_3,
            test_vector_sha1_4,
            test_vector_sha1_5,
            test_vector_sha256_1,
            test_vector_sha256_2,
            test_vector_sha256_3,
            test_vector_sha256_4,
            test_vector_sha256_5,
            test_vector_sha256_6
        ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    ok.

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

erlang_and_nif_are_equivalent_sha1(_Config) ->
    erlang_and_nif_are_equivalent_(sha).

erlang_and_nif_are_equivalent_sha224(_Config) ->
    erlang_and_nif_are_equivalent_(sha224).

erlang_and_nif_are_equivalent_sha256(_Config) ->
    erlang_and_nif_are_equivalent_(sha256).

erlang_and_nif_are_equivalent_sha384(_Config) ->
    erlang_and_nif_are_equivalent_(sha384).

erlang_and_nif_are_equivalent_sha512(_Config) ->
    erlang_and_nif_are_equivalent_(sha512).

erlang_and_nif_are_equivalent_(Sha) ->
    Prop = ?FORALL(
        {Pass, Salt, Count},
        {binary(), binary(), range(2, 20000)},
        fast_pbkdf2:pbkdf2(Sha, Pass, Salt, Count) =:=
            erl_pbkdf2:pbkdf2_oneblock(Sha, Pass, Salt, Count)
    ),
    ?assert(
        proper:quickcheck(Prop, [
            verbose,
            long_result,
            {numtests, 100},
            {start_size, 2},
            {max_size, 64}
        ])
    ).

%% Taken from the official RFC https://www.ietf.org/rfc/rfc6070.txt

test_vector_sha1_1(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 1, 20,
            base16:decode(<<"0c60c80f961f0e71f3a9b524af6012062fe037a6">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha, P, S, It, DkLen)).

test_vector_sha1_2(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 2, 20,
            base16:decode(<<"ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha, P, S, It, DkLen)).

test_vector_sha1_3(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 4096, 20,
            base16:decode(<<"4b007901b765489abead49d926f721d065a429c1">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha, P, S, It, DkLen)).

test_vector_sha1_4(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 16777216, 20,
            base16:decode(<<"eefe3d61cd4da4e4e9945b3d6ba2158c2634e984">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha, P, S, It, DkLen)).

test_vector_sha1_5(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"passwordPASSWORDpassword">>, <<"saltSALTsaltSALTsaltSALTsaltSALTsalt">>, 4096, 25,
            base16:decode(<<"3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha, P, S, It, DkLen)).

%% Taken from https://stackoverflow.com/a/5136918/8853275
test_vector_sha256_1(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 1, 32,
            base16:decode(<<"120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha256, P, S, It, DkLen)).

test_vector_sha256_2(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 2, 32,
            base16:decode(<<"ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha256, P, S, It, DkLen)).

test_vector_sha256_3(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 4096, 32,
            base16:decode(<<"c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha256, P, S, It, DkLen)).

test_vector_sha256_4(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"password">>, <<"salt">>, 16777216, 32,
            base16:decode(<<"cf81c66fe8cfc04d1f31ecb65dab4089f7f179e89b3b0bcb17ad10e3ac6eba46">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha256, P, S, It, DkLen)).

test_vector_sha256_5(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"passwordPASSWORDpassword">>, <<"saltSALTsaltSALTsaltSALTsaltSALTsalt">>, 4096, 40,
            base16:decode(
                <<"348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c4e2a1fb8dd53e1c635518c7dac47e9">>
            )},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha256, P, S, It, DkLen)).

test_vector_sha256_6(_Config) ->
    {P, S, It, DkLen, Result} =
        {<<"pass\0word">>, <<"sa\0lt">>, 4096, 16,
            base16:decode(<<"89b69d0516f829893c696226650a8687">>)},
    ?assertEqual(Result, fast_pbkdf2:pbkdf2(sha256, P, S, It, DkLen)).
