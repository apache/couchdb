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

-module(ctrace_config).

-export([
    get/1,
    parse_sampler/1,
    samplers/0,
    sampler/1,
    rules/1,
    is_enabled/0
]).

-type sampler() :: passage_sampler:sampler().

-type rule() :: ctrace_dsl:rule().

-spec get(
        OperationId :: atom() | binary()
    ) -> {error, Reason :: term()}
      | {ok, {Sampler :: sampler(), Rules :: [rule()]}}.

get(OperationId) ->
    case get_raw(OperationId) of
        undefined ->
            {error, not_found};
        {_, []} = Return ->
            Return;
        {Sampler, Rules} ->
            try
                {ok, {parse_sampler(Sampler), parse_rules(Rules)}}
            catch
                throw:{error, Reason} ->
                    {error, Reason}
            end
    end.

-spec samplers()
    -> [passage:trace_id()].

samplers() ->
    [K || {K, _V} <- config:get("tracing.samplers")].

-spec sampler(
        Operation :: string() | atom()
    ) -> undefined | string().

sampler(OperationName) when is_list(OperationName) ->
    config:get("tracing.samplers", OperationName);
sampler(OperationId) when is_atom(OperationId) ->
    sampler(atom_to_list(OperationId)).

-spec rules(
        Operation :: string() | atom()
    ) -> [{RuleId :: string(), RuleSource :: string()}].

rules(OperationName) when is_list(OperationName) ->
    config:get("tracing." ++ OperationName);
rules(OperationId) when is_atom(OperationId) ->
    rules(atom_to_list(OperationId)).

-spec is_enabled() -> boolean().

is_enabled() ->
    config:get_boolean("tracing", "enabled", false).

-spec get_raw(
        Operation :: string() | atom()
    ) -> undefined
      | {Sampler :: string(), [{RuleId :: string(), RuleSource :: string()}]}
      .

get_raw(OperationId) when is_atom(OperationId) ->
    get_raw(atom_to_list(OperationId));
get_raw(OperationName) when is_list(OperationName) ->
    case config:get("tracing.samplers", OperationName, undefined) of
        undefined ->
            undefined;
        Sampler ->
            {Sampler, rules(OperationName)}
    end.

-spec parse_sampler(
        Sampler :: binary() | string()
    ) -> passage_sampler:sampler().

parse_sampler(Binary) when is_binary(Binary) ->
    parse_sampler(binary_to_list(Binary));
parse_sampler("all") ->
    ctrace_sampler:all();
parse_sampler("null") ->
    ctrace_sampler:null();
parse_sampler(FloatStr) ->
    Help = "Cannot parse sampler. The only supported formats are: "
        " all | null | float(), got '~s'",
    try
        ctrace_sampler:probalistic(binary_to_float(FloatStr))
    catch
        _:_ ->
            throw({error, lists:flatten(io_lib:format(Help, [FloatStr]))})
    end.

-spec parse_rules(
        Rules :: [{RuleId :: string(), RuleSource :: string()}]
    ) -> [term()].

parse_rules(Rules) ->
    lists:map(fun({Name, Rule}) ->
        ctrace_dsl:parse_rule(Name, Rule)
    end, Rules).
