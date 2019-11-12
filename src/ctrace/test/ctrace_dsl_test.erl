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

-module(ctrace_dsl_test).


-include_lib("eunit/include/eunit.hrl").


simple_parse_and_compile_test() ->
    Filter = "(#{'http.method' := Method}) when Method == get -> 1.0",
    ctrace_dsl:compile("foo", Filter),
    ?assertEqual(1.0, run_filter("foo", #{'http.method' => get})),
    ?assertEqual(false, run_filter("foo", #{'httpd.method' => put})).


empty_map_test() ->
    Filter = "(#{}) -> true",
    ctrace_dsl:compile("foo", Filter),
    ?assertEqual(true, run_filter("foo", #{})),
    ?assertEqual(true, run_filter("foo", #{foo => bar})),
    ?assertEqual(false, run_filter("foo", nil)).


return_false_test() ->
    Filter = "(#{}) -> false",
    ctrace_dsl:compile("foo", Filter),
    ?assertEqual(false, run_filter("foo", #{})),
    ?assertEqual(false, run_filter("foo", nil)).


return_float_test() ->
    Filter = "(#{}) -> 0.2",
    ctrace_dsl:compile("foo", Filter),
    ?assertEqual(0.2, run_filter("foo", #{})),
    ?assertEqual(false, run_filter("foo", nil)).


bad_filter_body_is_list_test() ->
    Filter = "(#{}) -> []",
    Error = "Unsupported return value '[]'",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


bad_filter_body_has_calls_test() ->
    Filter = "(#{}) -> [module:function()]",
    Error = "Unsupported return value '[module:function()]'",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


bad_arg_list_too_few_test() ->
    Filter = "() -> true",
    Error = "The arity of the filter function should be 1",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


bad_arg_list_too_many_test() ->
    Filter = "(#{}, foo) -> true",
    Error = "The arity of the filter function should be 1",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


bad_arg_type_test() ->
    Filters = [
        "(atom) -> true",
        "([atom]) -> true",
        "(1) -> true",
        "(1.0) -> true"
    ],
    Error = "The only argument of the filter should be map",
    lists:foreach(fun(Filter) ->
        ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter))
    end, Filters).


bad_map_association_test() ->
    Filter = "(#{foo => Var}) -> true",
    Error = "Only #{field := Var} syntax is supported in the header",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


bad_field_variable_test() ->
    Filter = "(#{Var := Val}) -> false",
    Error = "Only atoms are supported as field names in the header",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


bad_field_match_test() ->
    Filter = "(#{foo := 2}) -> true",
    Error = "Only capitalized names are supported"
            " as matching variables in the header",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


repeated_variable_test() ->
    Filter = "(#{foo := Val, bar := Val}) -> true",
    Error = "'Val' variable is already in use",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


code_coverage1_test() ->
    Filter = "foo(#{}) -> bar",
    Error = "Unknown shape of a filter function",
    ?assertThrow({error, Error}, ctrace_dsl:compile("foo", Filter)).


code_coverage2_test() ->
    Filter = "(#{}) -> true",
    ?assertMatch([_ | _], ctrace_dsl:source("foo", Filter)).


run_filter(OperationId, Value) ->
    ModName = ctrace_config:filter_module_name(OperationId),
    ModName:match(Value).
