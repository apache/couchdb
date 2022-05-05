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

-module(aegis_s2v).

-export([s2v/3]).

s2v(Key, [], <<>>) ->
    aegis_cmac:cmac(Key, <<1:128>>);
s2v(Key, AAD, PlainText) when length(AAD) < 127 ->
    s2v(Key, AAD, PlainText, aegis_cmac:cmac(Key, <<0:128>>)).

s2v(Key, [], PlainText, Acc) when bit_size(PlainText) >= 128 ->
    aegis_cmac:cmac(Key, aegis_util:xorend(PlainText, Acc));
s2v(Key, [], PlainText, Acc) ->
    aegis_cmac:cmac(
        Key,
        crypto:exor(aegis_util:double(Acc), aegis_util:pad(PlainText))
    );
s2v(Key, [H | T], PlainText, Acc0) ->
    Acc1 = crypto:exor(aegis_util:double(Acc0), aegis_cmac:cmac(Key, H)),
    s2v(Key, T, PlainText, Acc1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

s2v_0_test() ->
    ?assertEqual(
        <<16#85632d07c6e8f37f950acd320a2ecc93:128>>,
        s2v(
            <<16#fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0:128>>,
            [<<16#101112131415161718191a1b1c1d1e1f2021222324252627:192>>],
            <<16#112233445566778899aabbccddee:112>>
        )
    ).

%% for test coverage only. this value does not come from a test vector.
s2v_1_test() ->
    ?assertEqual(
        <<106, 56, 130, 35, 180, 192, 121, 7, 97, 30, 181, 248, 111, 114, 85, 151>>,
        s2v(<<0:128>>, [], <<>>)
    ).

-endif.
