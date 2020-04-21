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

-module(jwtf_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

encode(Header0, Payload0) ->
    Header1 = b64url:encode(jiffy:encode(Header0)),
    Payload1 = b64url:encode(jiffy:encode(Payload0)),
    Sig = b64url:encode(<<"bad">>),
    <<Header1/binary, $., Payload1/binary, $., Sig/binary>>.

valid_header() ->
    {[{<<"typ">>, <<"JWT">>}, {<<"alg">>, <<"RS256">>}]}.

jwt_io_pubkey() ->
    PublicKeyPEM = <<"-----BEGIN PUBLIC KEY-----\n"
                  "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDdlatRjRjogo3WojgGH"
                  "FHYLugdUWAY9iR3fy4arWNA1KoS8kVw33cJibXr8bvwUAUparCwlvdbH6"
                  "dvEOfou0/gCFQsHUfQrSDv+MuSUMAe8jzKE4qW+jK+xQU9a03GUnKHkkl"
                  "e+Q0pX/g6jXZ7r1/xAK5Do2kQ+X5xK9cipRgEKwIDAQAB\n"
                  "-----END PUBLIC KEY-----\n">>,
    [PEMEntry] = public_key:pem_decode(PublicKeyPEM),
    public_key:pem_entry_decode(PEMEntry).


b64_badarg_test() ->
    Encoded = <<"0.0.0">>,
    ?assertEqual({error, {bad_request,badarg}},
        jwtf:decode(Encoded, [], nil)).


b64_bad_block_test() ->
    Encoded = <<" aGVsbG8. aGVsbG8. aGVsbG8">>,
    ?assertEqual({error, {bad_request,{bad_block,0}}},
        jwtf:decode(Encoded, [], nil)).


invalid_json_test() ->
    Encoded = <<"fQ.fQ.fQ">>,
    ?assertEqual({error, {bad_request,{1,invalid_json}}},
        jwtf:decode(Encoded, [], nil)).


truncated_json_test() ->
    Encoded = <<"ew.ew.ew">>,
    ?assertEqual({error, {bad_request,{2,truncated_json}}},
        jwtf:decode(Encoded, [], nil)).


missing_typ_test() ->
    Encoded = encode({[]}, []),
    ?assertEqual({error, {bad_request,<<"Missing typ header parameter">>}},
        jwtf:decode(Encoded, [typ], nil)).


invalid_typ_test() ->
    Encoded = encode({[{<<"typ">>, <<"NOPE">>}]}, []),
    ?assertEqual({error, {bad_request,<<"Invalid typ header parameter">>}},
        jwtf:decode(Encoded, [typ], nil)).


missing_alg_test() ->
    Encoded = encode({[]}, []),
    ?assertEqual({error, {bad_request,<<"Missing alg header parameter">>}},
        jwtf:decode(Encoded, [alg], nil)).


invalid_alg_test() ->
    Encoded = encode({[{<<"alg">>, <<"NOPE">>}]}, []),
    ?assertEqual({error, {bad_request,<<"Invalid alg header parameter">>}},
        jwtf:decode(Encoded, [alg], nil)).


missing_iss_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, {bad_request,<<"Missing iss claim">>}},
        jwtf:decode(Encoded, [{iss, right}], nil)).


invalid_iss_test() ->
    Encoded = encode(valid_header(), {[{<<"iss">>, <<"wrong">>}]}),
    ?assertEqual({error, {bad_request,<<"Invalid iss claim">>}},
        jwtf:decode(Encoded, [{iss, right}], nil)).


missing_iat_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, {bad_request,<<"Missing iat claim">>}},
        jwtf:decode(Encoded, [iat], nil)).


invalid_iat_test() ->
    Encoded = encode(valid_header(), {[{<<"iat">>, <<"hello">>}]}),
    ?assertEqual({error, {bad_request,<<"Invalid iat claim">>}},
        jwtf:decode(Encoded, [iat], nil)).


missing_nbf_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, {bad_request,<<"Missing nbf claim">>}},
        jwtf:decode(Encoded, [nbf], nil)).


invalid_nbf_test() ->
    Encoded = encode(valid_header(), {[{<<"nbf">>, 2 * now_seconds()}]}),
    ?assertEqual({error, {unauthorized, <<"nbf not in past">>}},
        jwtf:decode(Encoded, [nbf], nil)).


missing_exp_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, {bad_request, <<"Missing exp claim">>}},
        jwtf:decode(Encoded, [exp], nil)).


invalid_exp_test() ->
    Encoded = encode(valid_header(), {[{<<"exp">>, 0}]}),
    ?assertEqual({error, {unauthorized, <<"exp not in future">>}},
        jwtf:decode(Encoded, [exp], nil)).


missing_kid_test() ->
    Encoded = encode({[]}, {[]}),
    ?assertEqual({error, {bad_request, <<"Missing kid claim">>}},
        jwtf:decode(Encoded, [kid], nil)).


public_key_not_found_test() ->
    Encoded = encode(
        {[{<<"alg">>, <<"RS256">>}, {<<"kid">>, <<"1">>}]},
        {[]}),
    KS = fun(_, _) -> throw(not_found) end,
    Expected = {error, not_found},
    ?assertEqual(Expected, jwtf:decode(Encoded, [], KS)).


bad_rs256_sig_test() ->
    Encoded = encode(
        {[{<<"typ">>, <<"JWT">>}, {<<"alg">>, <<"RS256">>}]},
        {[]}),
    KS = fun(<<"RS256">>, undefined) -> jwt_io_pubkey() end,
    ?assertEqual({error, {bad_request, <<"Bad signature">>}},
        jwtf:decode(Encoded, [], KS)).


bad_hs256_sig_test() ->
    Encoded = encode(
        {[{<<"typ">>, <<"JWT">>}, {<<"alg">>, <<"HS256">>}]},
        {[]}),
    KS = fun(<<"HS256">>, undefined) -> <<"bad">> end,
    ?assertEqual({error, {bad_request, <<"Bad HMAC">>}},
        jwtf:decode(Encoded, [], KS)).


malformed_token_test() ->
    ?assertEqual({error, {bad_request, <<"Malformed token">>}},
        jwtf:decode(<<"a.b.c.d">>, [], nil)).

unknown_atom_check_test() ->
    ?assertError({unknown_checks, [foo, bar]},
        jwtf:decode(<<"a.b.c">>, [exp, foo, iss, bar], nil)).

unknown_binary_check_test() ->
    ?assertError({unknown_checks, [<<"bar">>]},
        jwtf:decode(<<"a.b.c">>, [exp, iss, <<"bar">>], nil)).

duplicate_check_test() ->
    ?assertError({duplicate_checks, [exp]},
        jwtf:decode(<<"a.b.c">>, [exp, exp], nil)).


%% jwt.io generated
hs256_test() ->
    EncodedToken = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6IjEyMzQ1Ni"
                     "J9.eyJpc3MiOiJodHRwczovL2Zvby5jb20iLCJpYXQiOjAsImV4cCI"
                     "6MTAwMDAwMDAwMDAwMDAsImtpZCI6ImJhciJ9.iS8AH11QHHlczkBn"
                     "Hl9X119BYLOZyZPllOVhSBZ4RZs">>,
    KS = fun(<<"HS256">>, <<"123456">>) -> <<"secret">> end,
    Checks = [{iss, <<"https://foo.com">>}, iat, exp, typ, alg, kid],
    ?assertMatch({ok, _}, catch jwtf:decode(EncodedToken, Checks, KS)).


%% pip install PyJWT
%% > import jwt
%% > jwt.encode({'foo':'bar'}, 'secret', algorithm='HS384')
hs384_test() ->
    EncodedToken = <<"eyJhbGciOiJIUzM4NCIsInR5cCI6IkpXVCJ9.eyJmb28iOiJiYXIif"
                     "Q.2quwghs6I56GM3j7ZQbn-ASZ53xdBqzPzTDHm_CtVec32LUy-Ezy"
                     "L3JjIe7WjL93">>,
    KS = fun(<<"HS384">>, _) -> <<"secret">> end,
    ?assertMatch({ok, {[{<<"foo">>,<<"bar">>}]}},
        catch jwtf:decode(EncodedToken, [], KS)).


%% pip install PyJWT
%% > import jwt
%% > jwt.encode({'foo':'bar'}, 'secret', algorithm='HS512')
hs512_test() ->
    EncodedToken = <<"eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJmb28iOiJiYX"
                     "IifQ.WePl7achkd0oGNB8XRF_LJwxlyiPZqpdNgdKpDboAjSTsW"
                     "q-aOGNynTp8TOv8KjonFym8vwFwppXOLoLXbkIaQ">>,
    KS = fun(<<"HS512">>, _) -> <<"secret">> end,
    ?assertMatch({ok, {[{<<"foo">>,<<"bar">>}]}},
        catch jwtf:decode(EncodedToken, [], KS)).


%% jwt.io generated
rs256_test() ->
    EncodedToken = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0N"
                     "TY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.Ek"
                     "N-DOsnsuRjRO6BxXemmJDm3HbxrbRzXglbN2S4sOkopdU4IsDxTI8j"
                     "O19W_A4K8ZPJijNLis4EZsHeY559a4DFOd50_OqgHGuERTqYZyuhtF"
                     "39yxJPAjUESwxk2J5k_4zM3O-vtd1Ghyo4IbqKKSy6J9mTniYJPenn"
                     "5-HIirE">>,

    Checks = [sig, alg],
    KS = fun(<<"RS256">>, undefined) -> jwt_io_pubkey() end,

    ExpectedPayload = {[
        {<<"sub">>, <<"1234567890">>},
        {<<"name">>, <<"John Doe">>},
        {<<"admin">>, true}
    ]},

    ?assertMatch({ok, ExpectedPayload}, jwtf:decode(EncodedToken, Checks, KS)).


encode_missing_alg_test() ->
    ?assertEqual({error, {bad_request, <<"Missing alg header parameter">>}},
        jwtf:encode({[]}, {[]}, <<"foo">>)).


encode_invalid_alg_test() ->
    ?assertEqual({error, {bad_request, <<"Invalid alg header parameter">>}},
        jwtf:encode({[{<<"alg">>, <<"BOGUS">>}]}, {[]}, <<"foo">>)).


encode_decode_test_() ->
    [{Alg, encode_decode(Alg)} || Alg <- jwtf:valid_algorithms()].


encode_decode(Alg) ->
    {EncodeKey, DecodeKey} = case jwtf:verification_algorithm(Alg) of
        {public_key, _Algorithm} ->
            create_keypair();
        {hmac, _Algorithm} ->
            Key = <<"a-super-secret-key">>,
            {Key, Key}
    end,
    Claims = claims(),
    {ok, Encoded} = jwtf:encode(header(Alg), Claims, EncodeKey),
    KS = fun(_, _) -> DecodeKey end,
    {ok, Decoded} = jwtf:decode(Encoded, [], KS),
    ?_assertMatch(Claims, Decoded).


header(Alg) ->
    {[
        {<<"typ">>, <<"JWT">>},
        {<<"alg">>, Alg},
        {<<"kid">>, <<"20170520-00:00:00">>}
    ]}.


claims() ->
    EpochSeconds = os:system_time(second),
    {[
        {<<"iat">>, EpochSeconds},
        {<<"exp">>, EpochSeconds + 3600}
    ]}.

create_keypair() ->
    %% https://tools.ietf.org/html/rfc7517#appendix-C
    N = decode(<<"t6Q8PWSi1dkJj9hTP8hNYFlvadM7DflW9mWepOJhJ66w7nyoK1gPNqFMSQRy"
        "O125Gp-TEkodhWr0iujjHVx7BcV0llS4w5ACGgPrcAd6ZcSR0-Iqom-QFcNP"
        "8Sjg086MwoqQU_LYywlAGZ21WSdS_PERyGFiNnj3QQlO8Yns5jCtLCRwLHL0"
        "Pb1fEv45AuRIuUfVcPySBWYnDyGxvjYGDSM-AqWS9zIQ2ZilgT-GqUmipg0X"
        "OC0Cc20rgLe2ymLHjpHciCKVAbY5-L32-lSeZO-Os6U15_aXrk9Gw8cPUaX1"
        "_I8sLGuSiVdt3C_Fn2PZ3Z8i744FPFGGcG1qs2Wz-Q">>),
    E = decode(<<"AQAB">>),
    D = decode(<<"GRtbIQmhOZtyszfgKdg4u_N-R_mZGU_9k7JQ_jn1DnfTuMdSNprTeaSTyWfS"
        "NkuaAwnOEbIQVy1IQbWVV25NY3ybc_IhUJtfri7bAXYEReWaCl3hdlPKXy9U"
        "vqPYGR0kIXTQRqns-dVJ7jahlI7LyckrpTmrM8dWBo4_PMaenNnPiQgO0xnu"
        "ToxutRZJfJvG4Ox4ka3GORQd9CsCZ2vsUDmsXOfUENOyMqADC6p1M3h33tsu"
        "rY15k9qMSpG9OX_IJAXmxzAh_tWiZOwk2K4yxH9tS3Lq1yX8C1EWmeRDkK2a"
        "hecG85-oLKQt5VEpWHKmjOi_gJSdSgqcN96X52esAQ">>),
    RSAPrivateKey = #'RSAPrivateKey'{
        modulus = N,
        publicExponent = E,
        privateExponent = D
    },
    RSAPublicKey = #'RSAPublicKey'{
        modulus = N,
        publicExponent = E
    },
    {RSAPrivateKey, RSAPublicKey}.


decode(Goop) ->
    crypto:bytes_to_integer(b64url:decode(Goop)).


now_seconds() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
