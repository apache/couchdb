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

-module(aegis_util).

-export([
    double/1,
    expand/1,
    pad/1,
    xorend/2
]).

%% @doc double
%% is the multiplication of S and 0...010 in the finite field
%% represented using the primitive polynomial
%% x<sup>128</sup> + x<sup>7</sup> + x<sup>2</sup> + x + 1.
%% @end
-spec double(Val :: binary()) -> binary().
double(<<0:1, Lo:127>>) ->
    <<(Lo bsl 1):128>>;
double(<<1:1, Lo:127>>) ->
    crypto:exor(<<(Lo bsl 1):128>>, <<16#87:128>>).

%% because SIV only uses half the bits of the input key
%% to encrypt and the other half for the authentication/IV
%% we expand our keys to 512 to ensure an overall security
%% threshold of 256.
expand(Key) when bit_size(Key) == 256 ->
    %% expansion technique from Bjoern Tackmann - IBM Zurich
    K0 = crypto:crypto_one_time(aes_256_ecb, Key, <<0:128>>, true),
    K1 = crypto:crypto_one_time(aes_256_ecb, Key, <<1:128>>, true),
    K2 = crypto:crypto_one_time(aes_256_ecb, Key, <<2:128>>, true),
    K3 = crypto:crypto_one_time(aes_256_ecb, Key, <<3:128>>, true),
    <<K0/binary, K1/binary, K2/binary, K3/binary>>.

%% @doc pad
%% indicates padding of string X, len(X) &lt; 128, out to 128 bits by
%% the concatenation of a single bit of 1 followed by as many 0 bits
%% as are necessary.
%% @end
-spec pad(binary()) -> binary().
pad(Val) when bit_size(Val) =< 128 ->
    Pad = 128 - bit_size(Val) - 1,
    <<Val/binary, 1:1, 0:Pad>>.

%% @doc xorend
%% where len(A) &gt;= len(B), means xoring a string B onto the end of
%% string A -- i.e., leftmost(A, len(A)-len(B)) || (rightmost(A,
%% len(B)) xor B).
%% @end
-spec xorend(binary(), binary()) -> binary().
xorend(A, B) when byte_size(A) >= byte_size(B) ->
    Diff = byte_size(A) - byte_size(B),
    <<Left:Diff/binary, Right/binary>> = A,
    Xor = crypto:exor(Right, B),
    <<Left/binary, Xor/binary>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

double_0_test() ->
    ?assertEqual(
        <<16#1c09bf5f83df7e080280b050b37e0e74:128>>,
        double(<<16#0e04dfafc1efbf040140582859bf073a:128>>)
    ).

double_1_test() ->
    ?assertEqual(
        <<16#dbe13bd0ed8c85dc9af179c99ddbf819:128>>,
        double(<<16#edf09de876c642ee4d78bce4ceedfc4f:128>>)
    ).

pad_test() ->
    ?assertEqual(
        <<16#112233445566778899aabbccddee8000:128>>,
        pad(<<16#112233445566778899aabbccddee:112>>)
    ).

-endif.
