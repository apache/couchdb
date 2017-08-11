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

% @doc
% This module decodes and validates JWT tokens. Almost all property
% checks are optional. If not checked, the presence or validity of the
% field is not verified. Signature check is mandatory, though.

-module(jwtf).

-export([
    encode/3,
    decode/3,
    valid_algorithms/0,
    verification_algorithm/1
]).

-define(ALGS, [
    {<<"RS256">>, {public_key, sha256}}, % RSA PKCS#1 signature with SHA-256
    {<<"RS384">>, {public_key, sha384}},
    {<<"RS512">>, {public_key, sha512}},
    {<<"ES256">>, {public_key, sha256}},
    {<<"ES384">>, {public_key, sha384}},
    {<<"ES512">>, {public_key, sha512}},
    {<<"HS256">>, {hmac, sha256}},
    {<<"HS384">>, {hmac, sha384}},
    {<<"HS512">>, {hmac, sha512}}]).


% @doc encode
% Encode the JSON Header and Claims using Key and Alg obtained from Header
-spec encode(term(), term(), term()) ->
    {ok, binary()} | no_return().
encode(Header = {HeaderProps}, Claims, Key) ->
    try
        Alg = case prop(<<"alg">>, HeaderProps) of
            undefined ->
                throw({bad_request, <<"Missing alg header parameter">>});
            Val ->
                Val
        end,
        EncodedHeader = b64url:encode(jiffy:encode(Header)),
        EncodedClaims = b64url:encode(jiffy:encode(Claims)),
        Message = <<EncodedHeader/binary, $., EncodedClaims/binary>>,
        SignatureOrMac = case verification_algorithm(Alg) of
            {public_key, Algorithm} ->
                public_key:sign(Message, Algorithm, Key);
            {hmac, Algorithm} ->
                crypto:hmac(Algorithm, Key, Message)
        end,
        EncodedSignatureOrMac = b64url:encode(SignatureOrMac),
        {ok, <<Message/binary, $., EncodedSignatureOrMac/binary>>}
    catch
        throw:Error ->
            {error, Error}
    end.


% @doc decode
% Decodes the supplied encoded token, checking
% for the attributes defined in Checks and calling
% the key store function to retrieve the key needed
% to verify the signature
decode(EncodedToken, Checks, KS) ->
    try
        [Header, Payload, Signature] = split(EncodedToken),
        validate(Header, Payload, Signature, Checks, KS),
        {ok, decode_json(Payload)}
    catch
        throw:Error ->
            {error, Error}
    end.


% @doc valid_algorithms
% Return a list of supported algorithms
-spec valid_algorithms() -> [binary()].
valid_algorithms() ->
    proplists:get_keys(?ALGS).


% @doc verification_algorithm
% Return {VerificationMethod, Algorithm} tuple for the specified Alg
-spec verification_algorithm(binary()) ->
    {atom(), atom()} | no_return().
verification_algorithm(Alg) ->
    case lists:keyfind(Alg, 1, ?ALGS) of
        {Alg, Val} ->
            Val;
        false ->
            throw({bad_request, <<"Invalid alg header parameter">>})
    end.


validate(Header0, Payload0, Signature, Checks, KS) ->
    Header1 = props(decode_json(Header0)),
    validate_header(Header1, Checks),

    Payload1 = props(decode_json(Payload0)),
    validate_payload(Payload1, Checks),

    Alg = prop(<<"alg">>, Header1),
    Key = key(Header1, Checks, KS),
    verify(Alg, Header0, Payload0, Signature, Key).


validate_header(Props, Checks) ->
    validate_typ(Props, Checks),
    validate_alg(Props, Checks).


validate_typ(Props, Checks) ->
    Required = prop(typ, Checks),
    TYP = prop(<<"typ">>, Props),
    case {Required, TYP} of
        {undefined, _} ->
            ok;
        {true, undefined} ->
            throw({bad_request, <<"Missing typ header parameter">>});
        {true, <<"JWT">>} ->
            ok;
        {true, _} ->
            throw({bad_request, <<"Invalid typ header parameter">>})
    end.


validate_alg(Props, Checks) ->
    Required = prop(alg, Checks),
    Alg = prop(<<"alg">>, Props),
    case {Required, Alg} of
        {undefined, _} ->
            ok;
        {true, undefined} ->
            throw({bad_request, <<"Missing alg header parameter">>});
        {true, Alg} ->
            case lists:member(Alg, valid_algorithms()) of
                true ->
                    ok;
                false ->
                    throw({bad_request, <<"Invalid alg header parameter">>})
            end
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
        {undefined, _} ->
            ok;
        {_ISS, undefined} ->
            throw({bad_request, <<"Missing iss claim">>});
        {ISS, ISS} ->
            ok;
        {_, _} ->
            throw({bad_request, <<"Invalid iss claim">>})
    end.


validate_iat(Props, Checks) ->
    Required = prop(iat, Checks),
    IAT = prop(<<"iat">>, Props),

    case {Required, IAT} of
        {undefined, _} ->
            ok;
        {true, undefined} ->
            throw({bad_request, <<"Missing iat claim">>});
        {true, IAT} when is_integer(IAT) ->
            ok;
        {true, _} ->
            throw({bad_request, <<"Invalid iat claim">>})
    end.


validate_nbf(Props, Checks) ->
    Required = prop(nbf, Checks),
    NBF = prop(<<"nbf">>, Props),

    case {Required, NBF} of
        {undefined, _} ->
            ok;
        {true, undefined} ->
            throw({bad_request, <<"Missing nbf claim">>});
        {true, IAT} ->
            assert_past(<<"nbf">>, IAT)
    end.


validate_exp(Props, Checks) ->
    Required = prop(exp, Checks),
    EXP = prop(<<"exp">>, Props),

    case {Required, EXP} of
        {undefined, _} ->
            ok;
        {true, undefined} ->
            throw({bad_request, <<"Missing exp claim">>});
        {true, EXP} ->
            assert_future(<<"exp">>, EXP)
    end.


key(Props, Checks, KS) ->
    Alg = prop(<<"alg">>, Props),
    Required = prop(kid, Checks),
    KID = prop(<<"kid">>, Props),
    case {Required, KID} of
        {true, undefined} ->
            throw({bad_request, <<"Missing kid claim">>});
        {_, KID} ->
            KS(Alg, KID)
    end.


verify(Alg, Header, Payload, SignatureOrMac0, Key) ->
    Message = <<Header/binary, $., Payload/binary>>,
    SignatureOrMac1 = b64url:decode(SignatureOrMac0),
    {VerificationMethod, Algorithm} = verification_algorithm(Alg),
    case VerificationMethod of
        public_key ->
            public_key_verify(Algorithm, Message, SignatureOrMac1, Key);
        hmac ->
            hmac_verify(Algorithm, Message, SignatureOrMac1, Key)
    end.


public_key_verify(Algorithm, Message, Signature, PublicKey) ->
    case public_key:verify(Message, Algorithm, Signature, PublicKey) of
        true ->
            ok;
        false ->
            throw({bad_request, <<"Bad signature">>})
    end.


hmac_verify(Algorithm, Message, HMAC, SecretKey) ->
    case crypto:hmac(Algorithm, SecretKey, Message) of
        HMAC ->
            ok;
        _ ->
            throw({bad_request, <<"Bad HMAC">>})
    end.


split(EncodedToken) ->
    case binary:split(EncodedToken, <<$.>>, [global]) of
        [_, _, _] = Split -> Split;
        _ -> throw({bad_request, <<"Malformed token">>})
    end.


decode_json(Encoded) ->
    case b64url:decode(Encoded) of
        {error, Reason} ->
            throw({bad_request, Reason});
        Decoded ->
            jiffy:decode(Decoded)
    end.

props({Props}) ->
    Props;

props(_) ->
    throw({bad_request, <<"Not an object">>}).


assert_past(Name, Time) ->
    case Time < now_seconds() of
        true ->
            ok;
        false ->
            throw({unauthorized, <<Name/binary, " not in past">>})
    end.

assert_future(Name, Time) ->
    case Time > now_seconds() of
        true ->
            ok;
        false ->
            throw({unauthorized, <<Name/binary, " not in future">>})
    end.


now_seconds() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.


prop(Prop, Props) ->
    proplists:get_value(Prop, Props).
