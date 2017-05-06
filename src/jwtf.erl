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

-export([decode/1]).

-spec decode(EncodedToken :: binary()) ->
    {ok, DecodedToken :: term()} | {error, Reason :: term()}.
decode(EncodedToken) ->
    try
        [Header, Payload, Signature] = split(EncodedToken),
        validate(Header, Payload, Signature),
        {ok, decode_json(Payload)}
    catch
        throw:Error ->
            Error
    end.


validate(Header0, Payload0, Signature) ->
    Header1 = props(decode_json(Header0)),
    validate_header(Header1),

    Payload1 = props(decode_json(Payload0)),
    validate_payload(Payload1),

    PublicKey = public_key(Payload1),
    rs256_verify(Header0, Payload0, Signature, PublicKey).


validate_header(Props) ->
    case proplists:get_value(<<"typ">>, Props) of
        <<"JWT">> ->
            ok;
        _ ->
            throw({error, invalid_type})
    end,
    case proplists:get_value(<<"alg">>, Props) of
        <<"RS256">> ->
            ok;
        _ ->
            throw({error, invalid_alg})
    end.


validate_payload(Props) ->
    validate_iss(Props),
    validate_iat(Props),
    validate_exp(Props).


validate_iss(Props) ->
    ExpectedISS = list_to_binary(config:get("iam", "iss")),
    case proplists:get_value(<<"iss">>, Props) of
        undefined ->
            throw({error, missing_iss});
        ExpectedISS ->
            ok;
        _ ->
            throw({error, invalid_iss})
    end.


validate_iat(Props) ->
    case proplists:get_value(<<"iat">>, Props) of
        undefined ->
            throw({error, missing_iat});
        IAT ->
            assert_past(iat, IAT)
    end.


validate_exp(Props) ->
    case proplists:get_value(<<"exp">>, Props) of
        undefined ->
            throw({error, missing_exp});
        EXP ->
            assert_future(exp, EXP)
    end.


public_key(Props) ->
    KID = case proplists:get_value(<<"kid">>, Props) of
        undefined ->
            throw({error, missing_kid});
        List ->
             binary_to_list(List)
    end,
    case config:get("iam_rsa_public_keys", KID) of
        undefined ->
            throw({error, public_key_not_found});
        ExpMod ->
            [Exp, Mod] = re:split(ExpMod, ",", [{return, binary}]),
            [
                crypto:bytes_to_integer(base64:decode(Exp)),
                crypto:bytes_to_integer(base64:decode(Mod))
            ]
    end.


rs256_verify(Header, Payload, Signature, PublicKey) ->
    Message = <<Header/binary, $., Payload/binary>>,
    case crypto:verify(rsa, sha256, Message, Signature, PublicKey) of
        true ->
            ok;
        false ->
            throw({error, bad_signature})
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    ok = application:start(config),

    EncodedToken = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJodHRwczovL2Zvby5jb20iLCJpYXQiOjAsImV4cCI6MTAwMDAwMDAwMDAwMDAsImtpZCI6ImJhciJ9.bi87-lkEeOblTb_5ZEh6FkmOSg3mC_kqu2xcYJpJb3So29agyJkkidu3NF8R20x-Xi1wD6E8ACgfODsbdu5dbNRc-HUaFUnvyBr-M94PXhSOvLduoXT2mg1tgD1s_n0QgmH0pP-aAINgotDiUBuQ-pMD5hDIX2EYqAjwRcnVrno">>,

    PublicKey = "AQAB,3ZWrUY0Y6IKN1qI4BhxR2C7oHVFgGPYkd38uGq1jQNSqEvJFcN93CYm16/G78FAFKWqwsJb3Wx+nbxDn6LtP4AhULB1H0K0g7/jLklDAHvI8yhOKlvoyvsUFPWtNxlJyh5JJXvkNKV/4Oo12e69f8QCuQ6NpEPl+cSvXIqUYBCs=",

    config:set("iam", "iss", "https://foo.com"),
    config:set("iam_rsa_public_keys", "bar", PublicKey),

    ?assertEqual(nope, decode(EncodedToken)).

-endif.
