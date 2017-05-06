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
            throw({error, invalid_type})
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
            assert_past(iat, IAT)
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
    case crypto:verify(rsa, sha256, Message, Signature, PublicKey) of
        true ->
            ok;
        false ->
            throw({error, bad_signature})
    end.


hs256_verify(Message, HMAC, SecretKey) ->
    case crypto:hmac(sha256, SecretKey, Message) of
        HMAC ->
            ok;
        E ->
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

hs256_test() ->
    EncodedToken = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJodHRwc"
                     "zovL2Zvby5jb20iLCJpYXQiOjAsImV4cCI6MTAwMDAwMDAwMDAwMDA"
                     "sImtpZCI6ImJhciJ9.lpOvEnYLdcujwo9RbhzXme6J-eQ1yfl782qq"
                     "crR6QYE">>,
    KS = fun(_) -> <<"secret">> end,
    Checks = [{iss, <<"https://foo.com">>}, iat, exp, kid],
    ?assertMatch({ok, _}, decode(EncodedToken, Checks, KS)).

rs256_test() ->
    EncodedToken = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJodHRwc"
                     "zovL2Zvby5jb20iLCJpYXQiOjAsImV4cCI6MTAwMDAwMDAwMDAwMDA"
                     "sImtpZCI6ImJhciJ9.bi87-lkEeOblTb_5ZEh6FkmOSg3mC_kqu2xc"
                     "YJpJb3So29agyJkkidu3NF8R20x-Xi1wD6E8ACgfODsbdu5dbNRc-H"
                     "UaFUnvyBr-M94PXhSOvLduoXT2mg1tgD1s_n0QgmH0pP-aAINgotDi"
                     "UBuQ-pMD5hDIX2EYqAjwRcnVrno">>,

    PublicKey = <<"AQAB,3ZWrUY0Y6IKN1qI4BhxR2C7oHVFgGPYkd38uGq1jQNSqEvJFcN93CY"
                  "m16/G78FAFKWqwsJb3Wx+nbxDn6LtP4AhULB1H0K0g7/jLklDAHvI8yhOKl"
                  "voyvsUFPWtNxlJyh5JJXvkNKV/4Oo12e69f8QCuQ6NpEPl+cSvXIqUYBCs=">>,

    Checks = [{iss, <<"https://foo.com">>}, iat, exp, kid],
    KS = fun(<<"bar">>) -> PublicKey end,

    ?assertMatch({ok, _}, decode(EncodedToken, Checks, KS)).

-endif.



