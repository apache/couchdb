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
    parse_rule/2,
    generate/2,
    compile/2,
    source/1,
    print_source/1
]).

-type ast()
    :: erl_syntax:syntaxTree().

-type bindings()
    :: map().

-type action()
    :: report
    | {sample, integer}
    | {sample, float}.

-type rule()
    :: #{
        name := atom(),
        args := ast(),
        conditions := ast(),
        bindings := bindings(),
        actions := [action()],
        source := string()
    }.

-spec parse_rule(
        FunName :: string(),
        String :: string()
    ) -> rule().

parse_rule(FunName, String) ->
    AST = merl:quote(FunName ++ String ++ "."),
    case AST of
        ?Q("'@Name'(_@Args) when _@__@Guard -> [_@@Actions].")
            when erl_syntax:type(Args) == map_expr ->
                #{
                    name => erl_syntax:atom_value(Name),
                    args => Args,
                    conditions => Guard,
                    bindings => bindings(Args),
                    actions => parse_actions(Actions),
                    source => String
                };
        ?Q("'@Name'(_@Args) when _@__@Guard -> _@@_.")
            when erl_syntax:type(Args) == map_expr ->
                fail("Function body should be a list of actions");
        ?Q("'@Name'(_@Args) when _@__@Guard -> _@@_.") ->
            fail("The only argument of the filter should be map");
        ?Q("'@Name'(_@@Args) when _@__@Guard -> _@@_.") ->
            fail("The arrity of the filter function should be 1");
        _ ->
            fail("Unknown shape of a filter function")
    end.

-spec bindings(
        MapAST :: ast()
    ) -> bindings().

bindings(MapAST) ->
    %% Unfortunatelly merl doesn't seem to support maps
    %% so we had to do it manually
    lists:foldl(fun(AST, Bindings) ->
        erl_syntax:type(AST) == map_field_exact
            orelse fail("only #{field := Var} syntax is supported in the header"),
        NameAST = erl_syntax:map_field_exact_name(AST),
        erl_syntax:type(NameAST) == atom
            orelse fail("only atoms are supported as field names in the header"),
        Name = erl_syntax:atom_value(NameAST),
        VarAST = erl_syntax:map_field_exact_value(AST),
        erl_syntax:type(VarAST) == variable
            orelse fail("only Capitalized names are supported as matching variables in the header"),
        Var = erl_syntax:variable_name(VarAST),
        maps:is_key(Var, Bindings)
            andalso fail(io_libs:format("'~s' variable is already in use", [Var])),
        Bindings#{Var => Name}
    end, #{}, erl_syntax:map_expr_fields(MapAST)).

-spec parse_actions(
        Actions :: [ast()]
    ) -> [action()].

parse_actions(Actions) ->
    lists:map(fun(ActionAST) ->
        parse_action(ActionAST)
    end, Actions).

-spec parse_action(
        Actions :: ast()
    ) -> action().

parse_action(ActionAST) ->
    case ActionAST of
        ?Q("report") ->
            report;
        ?Q("report_longer_than(_@AST)") when erl_syntax:type(AST) == integer ->
            {report_longer_than, erl_syntax:integer_value(AST)};
        ?Q("report_longer_than(_@AST)") ->
            fail("expecting `integer` in `report_longer_than` action");
        ?Q("sample(_@AST)") when erl_syntax:type(AST) == integer ->
            {sample, erl_syntax:integer_value(AST)};
        ?Q("sample(_@AST)") when erl_syntax:type(AST) == float ->
            {sample, erl_syntax:float_value(AST)};
        ?Q("sample(_@AST)") ->
            fail("expecting `integer | float` in `sample` action");
        _ ->
            fail(lists:flatten(io_lib:format(
                "unsuported action '~s'", [erl_prettypr:format(ActionAST)])))
    end.

-spec generate(
        ModuleName :: module(),
        Rules :: [rule()]
    ) -> [ast()].

generate(ModuleName, Rules) ->
    Module = ?Q("-module('@ModuleName@')."),
    Export = ?Q("-export([match/1])."),
    Ordered = order_rules(Rules),
    Functions = [
        erl_syntax:function(merl:term(match), [
            function_clause(Rule)
        || Rule <- Ordered] ++ [?Q("(_) -> false")])
    ],
    lists:flatten([Module, Export, Functions]).

-spec order_rules(
        [rule()]
    ) -> [rule()].

order_rules(Rules) ->
    lists:sort(fun(RuleA, RuleB) ->
        maps:get(name, RuleA) < maps:get(name, RuleB)
    end, Rules).

-spec source(
        Forms :: ast()
    ) -> string().

source(Forms) ->
    erl_prettypr:format(
        erl_syntax:form_list(Forms),
        [{paper,160},{ribbon,80}]).

-spec print_source(
        ast()
    ) -> ok.

print_source(Forms) ->
    io:format(source(Forms) ++ "~n", []).

-spec function_clause(
        rule()
    ) -> ast().

function_clause(Rule) ->
    #{
        args := Args,
        conditions := Guard,
        actions := Actions
    } = Rule,
    ActionsAST = actions(Actions),
    ?Q("(_@Args) when _@__@Guard -> _@ActionsAST").

-spec actions(
        [mfa()]
    ) -> ast().

actions(Actions) ->
    erl_syntax:list(lists:map(fun({Module, Function, Args}) ->
        %% keep in mind that implementation function would
        %% receive a list of arguments
        %% i.e. you would need to implement it as
        %% function_name([Arg1, Arg2]) -> fun(Span) -> true end).
        ?Q("'@Module@':'@Function@'(_@@Args@)")
    end, Actions)).

-spec compile(
        Module :: module(),
        Rules :: [mfa()]
    ) -> term().

compile(Module, Rules) ->
    AST = generate(Module, Rules),
    merl:compile_and_load(AST, [verbose]).

fail(Msg) ->
    throw({error, Msg}).

fail(Msg, Args) ->
    throw({error, lists:flatten(io_lib:format(Msg, Args))}).