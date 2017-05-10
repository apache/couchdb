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
% This module fetches and parses JSON Web Key Sets (JWKS).

-module(jwks).

-export([
    get_keyset/1
]).

-include_lib("public_key/include/public_key.hrl").

get_keyset(Url) ->
    ReqHeaders = [],
    case ibrowse:send_req(Url, ReqHeaders, get) of
        {ok, "200", _RespHeaders, RespBody} ->
            {ok, parse_keyset(RespBody)};
        _Else ->
            {error, get_keyset_failed}
    end.


parse_keyset(Body) ->
    {Props} = jiffy:decode(Body),
    Keys = proplists:get_value(<<"keys">>, Props),
    lists:flatmap(fun parse_key/1, Keys).


parse_key({Props}) ->
    Alg = proplists:get_value(<<"alg">>, Props),
    Kty = proplists:get_value(<<"kty">>, Props),
    Kid = proplists:get_value(<<"kid">>, Props),
    case {Alg, Kty} of
        {<<"RS256">>, <<"RSA">>} ->
            E = proplists:get_value(<<"e">>, Props),
            N = proplists:get_value(<<"n">>, Props),
            [{{Kty, Kid}, #'RSAPublicKey'{
                modulus = decode_number(N),
                publicExponent =  decode_number(E)}}];
        {<<"ES256">>, <<"EC">>} ->
            Crv = proplists:get_value(<<"crv">>, Props),
            case Crv of
                <<"P-256">> ->
                    X = proplists:get_value(<<"x">>, Props),
                    Y = proplists:get_value(<<"y">>, Props),
                    Point = <<4:8,
                        (b64url:decode(X))/binary,
                        (b64url:decode(Y))/binary>>,
                    [{{Kty, Kid}, {
                        #'ECPoint'{point = Point},
                        {namedCurve,{1,2,840,10045,3,1,7}}
                    }}];
                _ ->
                    []
            end;
        _ ->
            []
    end.


decode_number(Base64) ->
    crypto:bytes_to_integer(b64url:decode(Base64)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

jwks_test() ->
    application:ensure_all_started(ibrowse),
    ?assertMatch({ok, _}, get_keyset("https://iam.eu-gb.bluemix.net/oidc/keys")).

rs_test() ->
    Ejson = {[
        {<<"kty">>, <<"RSA">>},
        {<<"n">>, <<"0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx"
        "4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMs"
        "tn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2"
        "QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbI"
        "SD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqb"
        "w0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw">>},
        {<<"e">>, <<"AQAB">>},
        {<<"alg">>, <<"RS256">>},
        {<<"kid">>, <<"2011-04-29">>}
    ]},
    ?assertMatch([{{<<"RSA">>, <<"2011-04-29">>}, {'RSAPublicKey', _, 65537}}],
        parse_key(Ejson)).


ec_test() ->
    PrivateKey = #'ECPrivateKey'{
        version = 1,
        parameters = {namedCurve,{1,2,840,10045,3,1,7}},
        privateKey = b64url:decode("870MB6gfuTJ4HtUnUvYMyJpr5eUZNP4Bk43bVdj3eAE"),
        publicKey = <<4:8,
            (b64url:decode("MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4"))/binary,
            (b64url:decode("4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM"))/binary>>},
    Ejson = {[
        {<<"kty">>, <<"EC">>},
        {<<"crv">>, <<"P-256">>},
        {<<"x">>, <<"MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4">>},
        {<<"y">>, <<"4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM">>},
        {<<"alg">>, <<"ES256">>},
        {<<"kid">>, <<"1">>}
    ]},
    ?assertMatch([{_Key, _Value}], parse_key(Ejson)),
    {_, ECPublicKey} = parse_key(Ejson),
    Msg = <<"foo">>,
    Sig = public_key:sign(Msg, sha256, PrivateKey),
    ?assert(public_key:verify(Msg, sha256, Sig, ECPublicKey)).

-endif.
