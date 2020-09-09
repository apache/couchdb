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

-module(couch_rate_config).

% This parser supports only maps where key is atom and value
% is positive float or positive integer.

-include_lib("syntax_tools/include/merl.hrl").

-export([
    from_str/1,
    to_str/1
]).

from_str(String) ->
    parse_map(merl:quote(String)).


to_str(Map) when is_map(Map) ->
    StringArgs = maps:fold(fun(Key, Val, Acc) ->
        Acc ++ [atom_to_list(Key) ++ " => " ++ number_to_list(Val)]
    end, [], Map),
    "#{" ++ string:join(StringArgs, ", ") ++ "}".


number_to_list(Int) when is_integer(Int) ->
    integer_to_list(Int);

number_to_list(Float) when is_float(Float) ->
    float_to_list(Float).


parse_map(MapAST) ->
    erl_syntax:type(MapAST) == map_expr
        orelse fail("Only #{field => pos_integer() | float()} syntax is supported"),
    %% Parsing map manually, since merl does not support maps
    lists:foldl(fun(AST, Bindings) ->
        NameAST = erl_syntax:map_field_assoc_name(AST),
        erl_syntax:type(NameAST) == atom
            orelse fail("Only atoms are supported as field names"),
        Name = erl_syntax:atom_value(NameAST),
        ValueAST = erl_syntax:map_field_assoc_value(AST),
        Value = case erl_syntax:type(ValueAST) of
            integer ->
                erl_syntax:integer_value(ValueAST);
            float ->
                erl_syntax:float_value(ValueAST);
            _ ->
                fail("Only pos_integer() or float() alowed as values")
        end,
        Bindings#{Name => Value}
    end, #{}, erl_syntax:map_expr_fields(MapAST)).


fail(Msg) ->
    throw({error, Msg}).