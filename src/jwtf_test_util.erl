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

-module(jwtf_test_util).

-export([
    create_private_key/0,
    create_keypair/0,
    to_public_key/1
]).

-include_lib("public_key/include/public_key.hrl").

-spec create_private_key() ->
    #'RSAPrivateKey'{} | no_return().
create_private_key() ->
    create_private_key("/tmp").


-spec create_keypair() ->
    {#'RSAPrivateKey'{}, #'RSAPublicKey'{}} | no_return().
create_keypair() ->
    PrivateKey = create_private_key(),
    {PrivateKey, to_public_key(PrivateKey)}.


-spec to_public_key(#'RSAPrivateKey'{}) ->
    #'RSAPublicKey'{}.
to_public_key(#'RSAPrivateKey'{} = PrivateKey) ->
    #'RSAPublicKey'{
        modulus = PrivateKey#'RSAPrivateKey'.modulus,
        publicExponent = PrivateKey#'RSAPrivateKey'.publicExponent}.


create_private_key(TmpDir) ->
    ok = verify_openssl(),
    Path = filename:join(TmpDir, timestamp() ++ "-rsa.key.der"),
    Bin = create_rsa_key(Path),
    public_key:der_decode('RSAPrivateKey', Bin).


verify_openssl() ->
    case os:cmd("openssl version") of
        "OpenSSL 1." ++ _Rest ->
            ok;
        _ ->
            throw({error, openssl_required})
    end.


timestamp() ->
    lists:concat([integer_to_list(N) || N <- tuple_to_list(os:timestamp())]).


create_rsa_key(Path) ->
    Cmd = "openssl genpkey -algorithm RSA -outform DER -out " ++ Path,
    Out = os:cmd(Cmd),
    %% Since os:cmd doesn't indicate if the command fails, we go to
    %% some length to ensure the output looks correct.
    ok = validate_genpkey_output(Out),
    {ok, Bin} = file:read_file(Path),
    ok = file:delete(Path),
    Bin.


validate_genpkey_output(Out) when is_list(Out) ->
    Length = length(Out),
    case re:run(Out, "[.+\n]+") of % should only contain period, plus, or nl
        {match, [{0, Length}]} ->
            ok;
        _ ->
            throw({error, {openssl_genpkey_failed, Out}})
    end.
