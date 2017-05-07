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

-module(jwtf).

-export([decode/3]).

decode(EncodedToken, Checks, KS) ->
    try
        [Header, Payload, Signature] = split(EncodedToken),
        validate(Header, Payload, Signature, Checks, KS),
        {ok, decode_json(Payload)}
    catch
        throw:Error ->
            Error
    end.


validate(Header0, Payload0, Signature, Checks, KS) ->
    Header1 = props(decode_json(Header0)),
    validate_header(Header1),

    Payload1 = props(decode_json(Payload0)),
    validate_payload(Payload1, Checks),

    Alg = prop(<<"alg">>, Header1),
    Key = key(Payload1, Checks, KS),
    verify(Alg, Header0, Payload0, Signature, Key).


validate_header(Props) ->
    case prop(<<"typ">>, Props) of
        <<"JWT">> ->
            ok;
        _ ->
            throw({error, invalid_typ})
    end,
    case prop(<<"alg">>, Props) of
        <<"RS256">> ->
            ok;
        <<"HS256">> ->
            ok;
        _ ->
            throw({error, invalid_alg})
    end.


%% Not all these fields have to be present, but if they _are_ present
%% they must be valid.
validate_payload(Props, Checks) ->
    validate_iss(Props, Checks),
    validate_iat(Props, Checks),
    validate_nbf(Props, Checks),
    validate_exp(Props, Checks).


validate_iss(Props, Checks) ->
    ExpectedISS = prop(iss, Checks),
    ActualISS = prop(<<"iss">>, Props),

    case {ExpectedISS, ActualISS} of
        {ISS, undefined} when ISS /= undefined ->
            throw({error, missing_iss});
        {ISS, ISS} ->
            ok;
        {_, _} ->
            throw({error, invalid_iss})
    end.


validate_iat(Props, Checks) ->
    Required = prop(iat, Checks),
    IAT = prop(<<"iat">>, Props),

    case {Required, IAT} of
        {undefined, undefined} ->
            ok;
        {true, undefined} ->
            throw({error, missing_iat});
        {true, IAT} ->
            assert_past(iat, IAT)
    end.


validate_nbf(Props, Checks) ->
    Required = prop(nbf, Checks),
    NBF = prop(<<"nbf">>, Props),

    case {Required, NBF} of
        {undefined, undefined} ->
            ok;
        {true, undefined} ->
            throw({error, missing_nbf});
        {true, IAT} ->
            assert_past(nbf, IAT)
    end.


validate_exp(Props, Checks) ->
    Required = prop(exp, Checks),
    EXP = prop(<<"exp">>, Props),

    case {Required, EXP} of
        {undefined, undefined} ->
            ok;
        {true, undefined} ->
            throw({error, missing_exp});
        {true, EXP} ->
            assert_future(exp, EXP)
    end.


key(Props, Checks, KS) ->
    Required = prop(kid, Checks),
    KID = prop(<<"kid">>, Props),
    case {Required, KID} of
        {undefined, undefined} ->
            KS(undefined);
        {true, undefined} ->
            throw({error, missing_kid});
        {true, KID} ->
            KS(KID)
    end.


verify(Alg, Header, Payload, Signature0, Key) ->
    Message = <<Header/binary, $., Payload/binary>>,
    Signature1 = b64url:decode(Signature0),
    case Alg of
        <<"RS256">> ->
            rs256_verify(Message, Signature1, Key);
        <<"HS256">> ->
            hs256_verify(Message, Signature1, Key)
    end.


rs256_verify(Message, Signature, PublicKey) ->
    case public_key:verify(Message, sha256, Signature, PublicKey) of
        true ->
            ok;
        false ->
            throw({error, bad_signature})
    end.


hs256_verify(Message, HMAC, SecretKey) ->
    case crypto:hmac(sha256, SecretKey, Message) of
        HMAC ->
            ok;
        _ ->
            throw({error, bad_hmac})
    end.


split(EncodedToken) ->
    case binary:split(EncodedToken, <<$.>>, [global]) of
        [_, _, _] = Split -> Split;
        _ -> throw({error, malformed_token})
    end.


decode_json(Encoded) ->
    case b64url:decode(Encoded) of
        {error, Reason} ->
            throw({error, Reason});
        Decoded ->
            jiffy:decode(Decoded)
    end.

props({Props}) ->
    Props;

props(_) ->
    throw({error, not_object}).


assert_past(Name, Time) ->
    case Time < now_seconds() of
        true ->
            ok;
        false ->
            throw({error, {Name, not_in_past}})
    end.

assert_future(Name, Time) ->
    case Time > now_seconds() of
        true ->
            ok;
        false ->
            throw({error, {Name, not_in_future}})
    end.


now_seconds() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.


prop(Prop, Props) ->
    proplists:get_value(Prop, Props).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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


invalid_typ_test() ->
    Encoded = encode({[{<<"typ">>, <<"NOPE">>}]}, []),
    ?assertEqual({error, invalid_typ}, decode(Encoded, [typ], nil)).


invalid_alg_test() ->
    Encoded = encode({[{<<"typ">>, <<"JWT">>}, {<<"alg">>, <<"NOPE">>}]}, []),
    ?assertEqual({error, invalid_alg}, decode(Encoded, [alg], nil)).


missing_iss_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, missing_iss}, decode(Encoded, [{iss, right}], nil)).


invalid_iss_test() ->
    Encoded = encode(valid_header(), {[{<<"iss">>, <<"wrong">>}]}),
    ?assertEqual({error, invalid_iss}, decode(Encoded, [{iss, right}], nil)).


missing_iat_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, missing_iat}, decode(Encoded, [iat], nil)).


invalid_iat_test() ->
    Encoded = encode(valid_header(), {[{<<"iat">>, 32503680000}]}),
    ?assertEqual({error, {iat,not_in_past}}, decode(Encoded, [iat], nil)).


missing_nbf_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, missing_nbf}, decode(Encoded, [nbf], nil)).


invalid_nbf_test() ->
    Encoded = encode(valid_header(), {[{<<"nbf">>, 32503680000}]}),
    ?assertEqual({error, {nbf,not_in_past}}, decode(Encoded, [nbf], nil)).


missing_exp_test() ->
    Encoded = encode(valid_header(), {[]}),
    ?assertEqual({error, missing_exp}, decode(Encoded, [exp], nil)).


invalid_exp_test() ->
    Encoded = encode(valid_header(), {[{<<"exp">>, 0}]}),
    ?assertEqual({error, {exp,not_in_future}}, decode(Encoded, [exp], nil)).


bad_rs256_sig_test() ->
    Encoded = encode(
        {[{<<"typ">>, <<"JWT">>}, {<<"alg">>, <<"RS256">>}]},
        {[]}),
    KS = fun(undefined) -> jwt_io_pubkey() end,
    ?assertEqual({error, bad_signature}, decode(Encoded, [], KS)).


bad_hs256_sig_test() ->
    Encoded = encode(
        {[{<<"typ">>, <<"JWT">>}, {<<"alg">>, <<"HS256">>}]},
        {[]}),
    KS = fun(undefined) -> <<"bad">> end,
    ?assertEqual({error, bad_hmac}, decode(Encoded, [], KS)).


malformed_token_test() ->
    ?assertEqual({error, malformed_token}, decode(<<"a.b.c.d">>, [], nil)).


hs256_test() ->
    EncodedToken = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJodHRwc"
                     "zovL2Zvby5jb20iLCJpYXQiOjAsImV4cCI6MTAwMDAwMDAwMDAwMDA"
                     "sImtpZCI6ImJhciJ9.lpOvEnYLdcujwo9RbhzXme6J-eQ1yfl782qq"
                     "crR6QYE">>,
    KS = fun(_) -> <<"secret">> end,
    Checks = [{iss, <<"https://foo.com">>}, iat, exp, kid, sig],
    ?assertMatch({ok, _}, decode(EncodedToken, Checks, KS)).


%% jwt.io example
rs256_test() ->
    EncodedToken = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0N"
                     "TY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.Ek"
                     "N-DOsnsuRjRO6BxXemmJDm3HbxrbRzXglbN2S4sOkopdU4IsDxTI8j"
                     "O19W_A4K8ZPJijNLis4EZsHeY559a4DFOd50_OqgHGuERTqYZyuhtF"
                     "39yxJPAjUESwxk2J5k_4zM3O-vtd1Ghyo4IbqKKSy6J9mTniYJPenn"
                     "5-HIirE">>,

    Checks = [sig],
    KS = fun(undefined) -> jwt_io_pubkey() end,

    ExpectedPayload = {[
        {<<"sub">>, <<"1234567890">>},
        {<<"name">>, <<"John Doe">>},
        {<<"admin">>, true}
    ]},

    ?assertMatch({ok, ExpectedPayload}, decode(EncodedToken, Checks, KS)).

-endif.



