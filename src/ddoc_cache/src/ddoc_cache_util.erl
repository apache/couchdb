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

-module(ddoc_cache_util).


-export([
    new_uuid/0
]).


new_uuid() ->
    to_hex(crypto:strong_rand_bytes(16), []).


to_hex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdig(C1), hexdig(C2) | Acc]).


hexdig(C) when C >= 0, C =< 9 ->
    C + $0;
hexdig(C) when C >= 10, C =< 15 ->
    C + $A - 10.
