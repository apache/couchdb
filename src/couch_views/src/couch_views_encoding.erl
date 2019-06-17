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

-module(couch_views_encoding).


-export([
    encode/1,
    encode/2,
    decode/1
]).


-define(NULL, 0).
-define(FALSE, 1).
-define(TRUE, 2).
-define(NUMBER, 3).
-define(STRING, 4).
-define(LIST, 5).
-define(OBJECT, 6).


encode(X) ->
    encode(X, value).


encode(X, Type) when Type == key; Type == value ->
    erlfdb_tuple:pack(encode_int(X, Type)).


decode(Encoded) ->
    Val = erlfdb_tuple:unpack(Encoded),
    decode_int(Val).


encode_int(null, _Type) ->
    {?NULL};

encode_int(false, _Type) ->
    {?FALSE};

encode_int(true, _Type) ->
    {?TRUE};

encode_int(Num, key) when is_number(Num) ->
    {?NUMBER, float(Num)};

encode_int(Num, value) when is_number(Num) ->
    {?NUMBER, Num};

encode_int(Bin, key) when is_binary(Bin) ->
    {?STRING, couch_util:get_sort_key(Bin)};

encode_int(Bin, value) when is_binary(Bin) ->
    {?STRING, Bin};

encode_int(List, Type) when is_list(List) ->
    Encoded = lists:map(fun(Item) ->
        encode_int(Item, Type)
    end, List),
    {?LIST, list_to_tuple(Encoded)};

encode_int({Props}, Type) when is_list(Props) ->
    Encoded = lists:map(fun({K, V}) ->
        EK = encode_int(K, Type),
        EV = encode_int(V, Type),
        {EK, EV}
    end, Props),
    {?OBJECT, list_to_tuple(Encoded)}.


decode_int({?NULL}) ->
    null;

decode_int({?FALSE}) ->
    false;

decode_int({?TRUE}) ->
    true;

decode_int({?STRING, Bin}) ->
    Bin;

decode_int({?NUMBER, Num}) ->
    Num;

decode_int({?LIST, List}) ->
    lists:map(fun decode_int/1, tuple_to_list(List));

decode_int({?OBJECT, Object}) ->
    Props = lists:map(fun({EK, EV}) ->
        K = decode_int(EK),
        V = decode_int(EV),
        {K, V}
    end, tuple_to_list(Object)),
    {Props}.
