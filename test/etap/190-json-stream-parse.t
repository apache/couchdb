#!/usr/bin/env escript
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

main(_) ->
    test_util:init_code_path(),
    etap:plan(99),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag("Test died abnormally: ~p", [Other]),
            etap:bail("Bad return value.")
    end,
    ok.

test() ->
    crypto:start(),
    ok = test_raw_json_input(),
    ok = test_1_byte_data_function(),
    ok = test_multiple_bytes_data_function().


test_raw_json_input() ->
    etap:diag("Tests with raw JSON string as the input."),
    lists:foreach(
        fun({EJson, JsonString, Desc}) ->
            etap:is(
              equiv(EJson, json_stream_parse:to_ejson(JsonString)),
              true,
              Desc)
        end,
        cases()),
    ok.


test_1_byte_data_function() ->
    etap:diag("Tests with a 1 byte output data function as the input."),
    lists:foreach(
        fun({EJson, JsonString, Desc}) ->
            DataFun = fun() -> single_byte_data_fun(JsonString) end,
            etap:is(
              equiv(EJson, json_stream_parse:to_ejson(DataFun)),
              true,
              Desc)
        end,
        cases()),
    ok.


test_multiple_bytes_data_function() ->
    etap:diag("Tests with a multiple bytes output data function as the input."),
    lists:foreach(
        fun({EJson, JsonString, Desc}) ->
            DataFun = fun() -> multiple_bytes_data_fun(JsonString) end,
            etap:is(
              equiv(EJson, json_stream_parse:to_ejson(DataFun)),
              true,
              Desc)
        end,
        cases()),
    ok.


cases() ->
    [
        {1, "1", "integer numeric literial"},
        {3.1416, "3.14160", "float numeric literal"},  % text representation may truncate, trail zeroes
        {-1, "-1", "negative integer numeric literal"},
        {-3.1416, "-3.14160", "negative float numeric literal"},
        {12.0e10, "1.20000e+11", "float literal in scientific notation"},
        {1.234E+10, "1.23400e+10", "another float literal in scientific notation"},
        {-1.234E-10, "-1.23400e-10", "negative float literal in scientific notation"},
        {10.0, "1.0e+01", "yet another float literal in scientific notation"},
        {123.456, "1.23456E+2", "yet another float literal in scientific notation"},
        {10.0, "1e1", "yet another float literal in scientific notation"},
        {<<"foo">>, "\"foo\"", "string literal"},
        {<<"foo", 5, "bar">>, "\"foo\\u0005bar\"", "string literal with \\u0005"},
        {<<"">>, "\"\"", "empty string literal"},
        {<<"\n\n\n">>, "\"\\n\\n\\n\"", "only new lines literal"},
        {<<"\" \b\f\r\n\t\"">>, "\"\\\" \\b\\f\\r\\n\\t\\\"\"",
            "only white spaces string literal"},
        {null, "null", "null literal"},
        {true, "true", "true literal"},
        {false, "false", "false literal"},
        {<<"null">>, "\"null\"", "null string literal"},
        {<<"true">>, "\"true\"", "true string literal"},
        {<<"false">>, "\"false\"", "false string literal"},
        {{[]}, "{}", "empty object literal"},
        {{[{<<"foo">>, <<"bar">>}]}, "{\"foo\":\"bar\"}",
            "simple object literal"},
        {{[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]},
            "{\"foo\":\"bar\",\"baz\":123}", "another simple object literal"},
        {[], "[]", "empty array literal"},
        {[[]], "[[]]", "empty array literal inside a single element array literal"},
        {[1, <<"foo">>], "[1,\"foo\"]", "simple non-empty array literal"},
        {[1199344435545.0, 1], "[1199344435545.0,1]",
             "another simple non-empty array literal"},
        {[false, true, 321, null], "[false, true, 321, null]", "array of literals"},
        {{[{<<"foo">>, [123]}]}, "{\"foo\":[123]}",
             "object literal with an array valued property"},
        {{[{<<"foo">>, {[{<<"bar">>, true}]}}]},
            "{\"foo\":{\"bar\":true}}", "nested object literal"},
        {{[{<<"foo">>, []}, {<<"bar">>, {[{<<"baz">>, true}]}},
                {<<"alice">>, <<"bob">>}]},
            "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}",
            "complex object literal"},
        {[-123, <<"foo">>, {[{<<"bar">>, []}]}, null],
            "[-123,\"foo\",{\"bar\":[]},null]",
            "complex array literal"}
    ].


%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).
equiv({Props1}, {Props2}) ->
    equiv_object(Props1, Props2);
equiv(L1, L2) when is_list(L1), is_list(L2) ->
    equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) ->
    N1 == N2;
equiv(B1, B2) when is_binary(B1), is_binary(B2) ->
    B1 == B2;
equiv(true, true) ->
    true;
equiv(false, false) ->
    true;
equiv(null, null) ->
    true.


%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.
equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(
        fun({{K1, V1}, {K2, V2}}) ->
            equiv(K1, K2) andalso equiv(V1, V2)
        end,
        Pairs).


%% Recursively compare tuple elements for equivalence.
equiv_list([], []) ->
    true;
equiv_list([V1 | L1], [V2 | L2]) ->
    equiv(V1, V2) andalso equiv_list(L1, L2).


single_byte_data_fun([]) ->
    done;
single_byte_data_fun([H | T]) ->
    {<<H>>, fun() -> single_byte_data_fun(T) end}.


multiple_bytes_data_fun([]) ->
    done;
multiple_bytes_data_fun(L) ->
    N = crypto:rand_uniform(0, 7),
    {Part, Rest} = split(L, N),
    {list_to_binary(Part), fun() -> multiple_bytes_data_fun(Rest) end}.

split(L, N) when length(L) =< N ->
    {L, []};
split(L, N) ->
    take(N, L, []).

take(0, L, Acc) ->
    {lists:reverse(Acc), L};
take(N, [H|L], Acc) ->
    take(N - 1, L, [H | Acc]).
