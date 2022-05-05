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

-module(aegis).

-export([wrap_key/3, unwrap_key/3]).

wrap_key(KEK, AAD, DEK) when is_binary(KEK), is_list(AAD), is_binary(DEK) ->
    ExpandedKey = aegis_util:expand(KEK),
    {CipherText, CipherTag} =
        aegis_siv:block_encrypt(
            ExpandedKey,
            AAD,
            DEK
        ),
    <<CipherTag/binary, CipherText/binary>>.

unwrap_key(KEK, AAD, <<CipherTag:16/binary, CipherText/binary>>) when
    is_binary(KEK), is_list(AAD)
->
    ExpandedKey = aegis_util:expand(KEK),
    aegis_siv:block_decrypt(
        ExpandedKey,
        AAD,
        {CipherText, CipherTag}
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

aegis_test_() ->
    [
        ?_assertEqual(
            <<91, 78, 2, 43, 95, 157, 34, 252, 93, 35, 150, 141, 155, 139, 247, 136, 154, 203, 16,
                143, 196, 78, 93, 9, 189, 119, 22, 27, 60, 47, 186, 114, 70, 231, 113, 189, 36, 236,
                139, 153, 85, 58, 207, 165, 169, 70, 67, 61>>,
            wrap_key(<<0:256>>, [], <<1:256>>)
        ),
        ?_assertEqual(
            <<1:256>>,
            unwrap_key(
                <<0:256>>,
                [],
                <<91, 78, 2, 43, 95, 157, 34, 252, 93, 35, 150, 141, 155, 139, 247, 136, 154, 203,
                    16, 143, 196, 78, 93, 9, 189, 119, 22, 27, 60, 47, 186, 114, 70, 231, 113, 189,
                    36, 236, 139, 153, 85, 58, 207, 165, 169, 70, 67, 61>>
            )
        )
    ].

-endif.
