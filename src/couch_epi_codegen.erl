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

-module(couch_epi_codegen).

-export([generate/2, scan/1, parse/1, function/1, format_term/1]).

generate(ModuleName, Forms) when is_atom(ModuleName) ->
    generate(atom_to_list(ModuleName), Forms);
generate(ModuleName, Forms0) ->
    Forms = scan("-module(" ++ ModuleName ++ ").") ++ Forms0,
    ASTForms = parse(Forms),
    {ok, Mod, Bin} = compile:forms(ASTForms, [verbose, report_errors]),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    ok.

scan(String) ->
    Exprs = [E || E <- re:split(String, "\\.\n", [{return, list}, trim])],
    FormsTokens = lists:foldl(fun(Expr, Acc) ->
        case erl_scan:string(Expr) of
            {ok, [], _} ->
                Acc;
            {ok, Tokens, _} ->
                [{Expr, fixup_terminator(Tokens)} | Acc]
        end
    end, [], Exprs),
    lists:reverse(FormsTokens).

parse(FormsTokens) ->
    ASTForms = lists:foldl(fun(Tokens, Forms) ->
        {ok, AST} = parse_form(Tokens),
        [AST | Forms]
    end, [], FormsTokens),
    lists:reverse(ASTForms).

format_term(Data) ->
    lists:flatten(io_lib:format("~w", [Data])).

parse_form(Tokens) ->
    {Expr, Forms} = split_expression(Tokens),
    case erl_parse:parse_form(Forms) of
        {ok, AST} -> {ok, AST};
        {error,{_,_, Reason}} ->
            %%Expr = [E || {E, _Form} <- Tokens],
            {error, Expr, Reason}
    end.

split_expression({Expr, Forms}) -> {Expr, Forms};
split_expression(Tokens) ->
    {Exprs, Forms} = lists:unzip(Tokens),
    {string:join(Exprs, "\n"), lists:append(Forms)}.

function(Clauses) ->
    [lists:flatten(Clauses)].

fixup_terminator(Tokens) ->
    case lists:last(Tokens) of
        {dot, _} -> Tokens;
        {';', _} -> Tokens;
        Token ->
            {line, Line} = erl_scan:token_info(Token, line),
            Tokens ++ [{dot, Line}]
    end.
