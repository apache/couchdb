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
    [parse_key(Key) || Key <- Keys].


parse_key({Props}) ->
    <<"RS256">> = proplists:get_value(<<"alg">>, Props),
    <<"RSA">> = proplists:get_value(<<"kty">>, Props),
    Kid = proplists:get_value(<<"kid">>, Props),
    E = proplists:get_value(<<"e">>, Props),
    N = proplists:get_value(<<"n">>, Props),
    {Kid, {'RSAPublicKey', decode_number(N), decode_number(E)}}.


decode_number(Base64) ->
    crypto:bytes_to_integer(b64url:decode(Base64)).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

jwks_test() ->
    application:ensure_all_started(ibrowse),
    ?assertMatch({ok, _}, get_keyset("https://iam.eu-gb.bluemix.net/oidc/keys")).

-endif.
