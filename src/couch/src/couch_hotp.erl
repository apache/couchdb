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

-module(couch_hotp).

-export([generate/4]).

generate(Alg, Key, Counter, OutputLen)
  when is_atom(Alg), is_binary(Key), is_integer(Counter), is_integer(OutputLen) ->
    Hmac = crypto:hmac(Alg, Key, <<Counter:64>>),
    Offset = binary:last(Hmac) band 16#f,
    Code =
        ((binary:at(Hmac, Offset) band 16#7f) bsl 24) +
        ((binary:at(Hmac, Offset + 1) band 16#ff) bsl 16) +
        ((binary:at(Hmac, Offset + 2) band 16#ff) bsl 8) +
        ((binary:at(Hmac, Offset + 3) band 16#ff)),
    case OutputLen of
        6 -> Code rem 1000000;
        7 -> Code rem 10000000;
        8 -> Code rem 100000000
    end.
