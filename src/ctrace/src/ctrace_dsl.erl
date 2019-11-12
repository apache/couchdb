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

-module(ctrace_dsl).
-include_lib("syntax_tools/include/merl.hrl").

-export([
    compile/2,

    % Debug
    source/2
]).


-type ast() :: erl_syntax:syntaxTree().


-spec compile(OperationId :: string(), FilterDef :: string()) -> ok.
compile(OperationId, FilterDef) ->
    AST = parse_filter(OperationId, FilterDef),
    merl:compile_and_load(AST),
    ok.


-spec source(OperationId :: string(), FilterDef :: string()) -> string().
source(OperationId, FilterDef) ->
    AST = parse_filter(OperationId, FilterDef),
    Options = [{paper, 160}, {ribbon, 80}],
    erl_prettypr:format(erl_syntax:form_list(AST), Options).


-spec parse_filter(OperationId :: string(), FilterDef :: string()) -> [ast()].
parse_filter(OperationId, FilterDef) ->
    AST = merl:quote("match" ++ FilterDef ++ "."),
    case AST of
        ?Q("match(_@Args) when _@__@Guard -> _@Return.")
            when erl_syntax:type(Args) == map_expr ->
                validate_args(Args),
                validate_return(Return),
                generate(OperationId, Args, Guard, Return);
        ?Q("match(_@Args) when _@__@Guard -> _@@_.") ->
            fail("The only argument of the filter should be map");
        ?Q("match(_@@Args) when _@__@Guard -> _@@_.") ->
            fail("The arity of the filter function should be 1");
        _ ->
            fail("Unknown shape of a filter function")
    end.


-spec validate_args(MapAST :: ast()) -> ok.
validate_args(MapAST) ->
    %% Unfortunatelly merl doesn't seem to support maps
    %% so we had to do it manually
    lists:foldl(fun(AST, Bindings) ->
        erl_syntax:type(AST) == map_field_exact
            orelse fail("Only #{field := Var} syntax is supported in the header"),
        NameAST = erl_syntax:map_field_exact_name(AST),
        erl_syntax:type(NameAST) == atom
            orelse fail("Only atoms are supported as field names in the header"),
        Name = erl_syntax:atom_value(NameAST),
        VarAST = erl_syntax:map_field_exact_value(AST),
        erl_syntax:type(VarAST) == variable
            orelse fail("Only capitalized names are supported as matching variables in the header"),
        Var = erl_syntax:variable_name(VarAST),
        maps:is_key(Var, Bindings)
            andalso fail("'~s' variable is already in use", [Var]),
        Bindings#{Var => Name}
    end, #{}, erl_syntax:map_expr_fields(MapAST)).


-spec validate_return(Return :: [ast()]) -> ok.
validate_return(Return) ->
    case Return of
        ?Q("true") -> ok;
        ?Q("false") -> ok;
        ?Q("_@AST") when erl_syntax:type(AST) == float -> ok;
        _ ->
            fail("Unsupported return value '~s'", [erl_prettypr:format(Return)])
    end.


generate(OperationId, Args, Guard, Return) ->
    ModuleName = ctrace_config:filter_module_name(OperationId),
    Module = ?Q("-module('@ModuleName@')."),
    Export = ?Q("-export([match/1])."),
    Function = erl_syntax:function(merl:term(match), [
        ?Q("(_@Args) when _@__@Guard -> _@Return"),
        ?Q("(_) -> false")
    ]),
    lists:flatten([Module, Export, Function]).


fail(Msg) ->
    throw({error, Msg}).

fail(Msg, Args) ->
    throw({error, lists:flatten(io_lib:format(Msg, Args))}).