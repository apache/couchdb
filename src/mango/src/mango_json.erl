% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mango_json).

-export([
    min/0,
    max/0,
    cmp/2,
    cmp_raw/2,
    type/1,
    special/1,
    to_binary/1
]).

-define(MIN_VAL, mango_json_min).
-define(MAX_VAL, mango_json_max).

min() ->
    ?MIN_VAL.

max() ->
    ?MAX_VAL.

cmp(?MIN_VAL, ?MIN_VAL) ->
    0;
cmp(?MIN_VAL, _) ->
    -1;
cmp(_, ?MIN_VAL) ->
    1;
cmp(?MAX_VAL, ?MAX_VAL) ->
    0;
cmp(?MAX_VAL, _) ->
    1;
cmp(_, ?MAX_VAL) ->
    -1;
cmp(A, B) ->
    couch_ejson_compare:less(A, B).

cmp_raw(?MIN_VAL, ?MIN_VAL) ->
    0;
cmp_raw(?MIN_VAL, _) ->
    -1;
cmp_raw(_, ?MIN_VAL) ->
    1;
cmp_raw(?MAX_VAL, ?MAX_VAL) ->
    0;
cmp_raw(?MAX_VAL, _) ->
    1;
cmp_raw(_, ?MAX_VAL) ->
    -1;
cmp_raw(A, B) ->
    case A < B of
        true ->
            -1;
        false ->
            case A > B of
                true ->
                    1;
                false ->
                    0
            end
    end.

type(null) ->
    <<"null">>;
type(Bool) when is_boolean(Bool) ->
    <<"boolean">>;
type(Num) when is_number(Num) ->
    <<"number">>;
type(Str) when is_binary(Str) ->
    <<"string">>;
type({Props}) when is_list(Props) ->
    <<"object">>;
type(Vals) when is_list(Vals) ->
    <<"array">>.

special(?MIN_VAL) ->
    true;
special(?MAX_VAL) ->
    true;
special(_) ->
    false.

to_binary({Props}) ->
    Pred = fun({Key, Value}) ->
        {to_binary(Key), to_binary(Value)}
    end,
    {lists:map(Pred, Props)};
to_binary(Data) when is_list(Data) ->
    [to_binary(D) || D <- Data];
to_binary(null) ->
    null;
to_binary(true) ->
    true;
to_binary(false) ->
    false;
to_binary(Data) when is_atom(Data) ->
    list_to_binary(atom_to_list(Data));
to_binary(Data) when is_number(Data) ->
    Data;
to_binary(Data) when is_binary(Data) ->
    Data.
