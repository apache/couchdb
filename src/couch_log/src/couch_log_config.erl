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
%
% Based on Bob Ippolitto's mochiglobal.erl

-module(couch_log_config).

-export([
    init/0,
    reconfigure/0,
    get/1
]).

-define(MOD_NAME, couch_log_config_dyn).
-define(ERL_FILE, "couch_log_config_dyn.erl").

-spec init() -> ok.
init() ->
    reconfigure().

-spec reconfigure() -> ok.
reconfigure() ->
    {ok, ?MOD_NAME, Bin} = compile:forms(forms(), [verbose, report_errors]),
    code:purge(?MOD_NAME),
    {module, ?MOD_NAME} = code:load_binary(?MOD_NAME, ?ERL_FILE, Bin),
    ok.

-spec get(atom()) -> term().
get(Key) ->
    ?MOD_NAME:get(Key).

-spec entries() -> [string()].
entries() ->
    [
        {level, "level", "info"},
        {level_int, "level", "info"},
        {report_level, "report_level", "info"},
        {max_message_size, "max_message_size", "16000"},
        {strip_last_msg, "strip_last_msg", "true"},
        {filter_fields, "filter_fields", "[pid, registered_name, error_info, messages]"}
    ].

-spec forms() -> [erl_syntax:syntaxTree()].
forms() ->
    GetFunClauses = lists:map(
        fun({FunKey, CfgKey, Default}) ->
            FunVal = transform(FunKey, config:get("log", CfgKey, Default)),
            Patterns = [erl_syntax:abstract(FunKey)],
            Bodies = [erl_syntax:abstract(FunVal)],
            erl_syntax:clause(Patterns, none, Bodies)
        end,
        entries()
    ),

    Statements = [
        % -module(?MOD_NAME)
        erl_syntax:attribute(
            erl_syntax:atom(module),
            [erl_syntax:atom(?MOD_NAME)]
        ),

        % -export([lookup/1]).
        erl_syntax:attribute(
            erl_syntax:atom(export),
            [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(
                        erl_syntax:atom(get),
                        erl_syntax:integer(1)
                    )
                ])
            ]
        ),

        % list(Key) -> Value.
        erl_syntax:function(erl_syntax:atom(get), GetFunClauses)
    ],
    [erl_syntax:revert(X) || X <- Statements].

transform(level, LevelStr) ->
    couch_log_util:level_to_atom(LevelStr);
transform(report_level, LevelStr) ->
    couch_log_util:level_to_atom(LevelStr);
transform(level_int, LevelStr) ->
    Level = couch_log_util:level_to_atom(LevelStr),
    couch_log_util:level_to_integer(Level);
transform(max_message_size, SizeStr) ->
    try list_to_integer(SizeStr) of
        Size -> Size
    catch
        _:_ ->
            16000
    end;
transform(strip_last_msg, "false") ->
    false;
transform(strip_last_msg, _) ->
    true;
transform(filter_fields, FieldsStr) ->
    Default = [pid, registered_name, error_info, messages],
    case parse_term(FieldsStr) of
        {ok, List} when is_list(List) ->
            case lists:all(fun erlang:is_atom/1, List) of
                true ->
                    List;
                false ->
                    Default
            end;
        _ ->
            Default
    end.

parse_term(List) ->
    {ok, Tokens, _} = erl_scan:string(List ++ "."),
    erl_parse:parse_term(Tokens).
