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

-module(couch_ejson_compare_tests).

-define(MAX_UNICODE_STRING, <<255, 255, 255, 255>>).

% See mango_idx_view.hrl
-define(MAX_JSON_OBJ, {?MAX_UNICODE_STRING}).

-define(TEST_VALUES, [
    null,
    false,
    true,
    -2,
    -0.1,
    0,
    0.1,
    1,
    2,
    3.0,
    4,
    <<"a">>,
    <<"A">>,
    <<"aa">>,
    <<"b">>,
    <<"B">>,
    <<"ba">>,
    <<"bb">>,
    % Highest sorting unicode value
    % libicu >= 59 sorts it highest natively
    ?MAX_UNICODE_STRING,
    [<<"a">>],
    [<<"b">>],
    [<<"b">>, <<"c">>],
    [<<"b">>, <<"d">>],
    [<<"b">>, <<"d">>, <<"e">>],
    {[{<<"a">>, 1}]},
    {[{<<"a">>, 2}]},
    {[{<<"b">>, 1}]},
    {[{<<"b">>, 2}]},
    {[{<<"b">>, 2}, {<<"a">>, 1}]},
    {[{<<"b">>, 2}, {<<"c">>, 2}]}
]).

% Propery tests

-ifdef(WITH_PROPER).

-include_lib("couch/include/couch_eunit_proper.hrl").

property_test_() ->
    ?EUNIT_QUICKCHECK(60, 400).

% Properties

% The main, nif-based comparison, sorts the test values correctly
prop_nif_sorts_correctly() ->
    Positions = get_positions(?TEST_VALUES),
    ?FORALL(
        A,
        oneof(?TEST_VALUES),
        ?FORALL(B, oneof(?TEST_VALUES), begin
            expected_less(A, B, Positions) =:= less_nif(A, B)
        end)
    ).

% The erlang fallback comparison sorts the test values correctly
prop_erlang_sorts_correctly() ->
    Positions = get_positions(?TEST_VALUES),
    ?FORALL(
        A,
        oneof(?TEST_VALUES),
        ?FORALL(B, oneof(?TEST_VALUES), begin
            expected_less(A, B, Positions) =:= less_erl(A, B)
        end)
    ).

% Zero width unicode chars are ignored
prop_equivalent_unicode_values() ->
    ?FORALL({Prefix, Suffix}, {zero_width_list(), zero_width_list()}, begin
        Binary = unicode:characters_to_binary(Prefix ++ [$a] ++ Suffix),
        less(<<"a">>, Binary) =:= 0
    end).

% Every test value sorts less than the special ?MAX_JSON_OBJ
prop_test_values_are_less_than_max_json() ->
    ?FORALL(V, oneof(?TEST_VALUES), begin
        less(V, ?MAX_JSON_OBJ) =:= -1
    end).

% Any json value sorts less than the special ?MAX_JSON_OBJ
prop_any_json_is_less_than_max_json() ->
    ?FORALL(V, json(), begin
        less(V, ?MAX_JSON_OBJ) =:= -1
    end).

% ?MAX_JSON_OBJ (used in Mango) sorts the highest
prop_sort_key_test_values_less_than_max() ->
    ?FORALL(V, oneof(?TEST_VALUES), begin
        sort_key_cmp(V, ?MAX_JSON_OBJ) =:= -1
    end).

% Any json value sorts lower than ?MAX_JSON_OBJ
prop_sort_key_any_json_less_than_max() ->
    ?FORALL(V, json(), begin
        sort_key_cmp(V, ?MAX_JSON_OBJ) =:= -1
    end).

% In general, for any json, the nif collator matches the erlang collator
prop_nif_matches_erlang() ->
    ?FORALL(
        A,
        json(),
        ?FORALL(B, json(), begin
            less_nif(A, B) =:= less_erl(A, B)
        end)
    ).

% Check sort key orders json values same as as less
prop_sort_key_matches_less() ->
    ?FORALL(
        A,
        json(),
        ?FORALL(B, json(), begin
            sort_key_cmp(A, B) =:= less(A, B)
        end)
    ).

% Sorting a list with sort key is the same as sorting it with less
prop_sort_key_sorts_like_less() ->
    ?FORALL(L, list(json()), begin
        ByLess = lists:sort(fun(A, B) -> couch_ejson_compare:less(A, B) =< 0 end, L),
        BySortKey = lists:sort(
            fun(A, B) -> couch_ejson_compare:sort_key(A) =< couch_ejson_compare:sort_key(B) end, L
        ),
        ByLess =:= BySortKey
    end).

% Specifically check unicode strings. (The general idea here is we we'd like to
% spend our "randomizaton budget" exploring unicode strings more than various
% term shapes).
prop_sort_key_nif_matches_less() ->
    ?FORALL(
        A,
        sort_key_string(),
        ?FORALL(B, sort_key_string(), begin
            sort_key_nif_cmp(A, B) =:= less(A, B)
        end)
    ).

% Extra check that grouping works. Surround the value various zero-width
% characters and ensure sort string is the same as without them. In other words
% we'd group things like this together.
prop_sort_key_equivalent_strings() ->
    ?FORALL({Prefix, Suffix}, {zero_width_list(), zero_width_list()}, begin
        Binary = unicode:characters_to_binary(Prefix ++ [$a] ++ Suffix),
        SortKey = couch_ejson_compare:get_sort_key_nif(<<"a">>),
        SortKey =:= couch_ejson_compare:get_sort_key_nif(Binary)
    end).

% Generators

json() ->
    ?SIZED(Size, json(Size)).

json(0) ->
    oneof([
        null,
        true,
        false,
        json_number(),
        json_string(),
        [],
        {[]}
    ]);
json(Size) ->
    frequency([
        {1, null},
        {1, true},
        {1, false},
        {2, json_number()},
        {3, json_string()},
        {4, []},
        {4, {[]}},
        {5, ?LAZY(json_array(Size))},
        {5, ?LAZY(json_object(Size))}
    ]).

json_number() ->
    oneof([largeint(), int(), real()]).

json_string() ->
    utf8().

json_array(0) ->
    [];
json_array(Size) ->
    vector(Size div 2, json(Size div 2)).

json_object(0) ->
    {[]};
json_object(Size) ->
    {vector(Size div 2, {json_string(), json(Size div 2)})}.

zero_width_list() ->
    ?SIZED(Size, vector(Size, zero_width_chars())).

zero_width_chars() ->
    oneof([16#200B, 16#200C, 16#200D]).

% Besides handling json string we also handle ?MAX_UNICODE_STRING (the
% <<255,255,255,255>> to sorting values so make we mix that top sorting value
% into our values we pass into the ICU library. It should handle them as of ICU
% version >= 59
sort_key_string() ->
    oneof([json_string(), ?MAX_UNICODE_STRING]).

sort_key_cmp(A, B) ->
    term_cmp(couch_ejson_compare:sort_key(A), couch_ejson_compare:sort_key(B)).

sort_key_nif_cmp(A, B) ->
    term_cmp(couch_ejson_compare:get_sort_key_nif(A), couch_ejson_compare:get_sort_key_nif(B)).

% Helper to return the same shape as less/2
term_cmp(A, B) when A < B -> -1;
term_cmp(A, B) when A > B -> 1;
term_cmp(_, _) -> 0.

-else.

-include_lib("couch/include/couch_eunit.hrl").

-endif.

% Regular EUnit tests

get_icu_version_test() ->
    Ver = couch_ejson_compare:get_icu_version(),
    ?assertMatch({_, _, _, _}, Ver),
    {V1, V2, V3, V4} = Ver,
    ?assert(is_integer(V1) andalso V1 > 0),
    ?assert(is_integer(V2) andalso V2 >= 0),
    ?assert(is_integer(V3) andalso V3 >= 0),
    ?assert(is_integer(V4) andalso V4 >= 0).

get_uca_version_test() ->
    Ver = couch_ejson_compare:get_uca_version(),
    ?assertMatch({_, _, _, _}, Ver),
    {V1, V2, V3, V4} = Ver,
    ?assert(is_integer(V1) andalso V1 > 0),
    ?assert(is_integer(V2) andalso V2 >= 0),
    ?assert(is_integer(V3) andalso V3 >= 0),
    ?assert(is_integer(V4) andalso V4 >= 0).

get_collator_version_test() ->
    Ver = couch_ejson_compare:get_collator_version(),
    ?assertMatch({_, _, _, _}, Ver),
    {V1, V2, V3, V4} = Ver,
    ?assert(is_integer(V1) andalso V1 > 0),
    ?assert(is_integer(V2) andalso V2 >= 0),
    ?assert(is_integer(V3) andalso V3 >= 0),
    ?assert(is_integer(V4) andalso V4 >= 0).

max_depth_error_list_test() ->
    % NIF can handle terms with depth <= 9
    Nested9 = nest_list(<<"val">>, 9),
    ?assertEqual(0, less_nif(Nested9, Nested9)),

    % At depth >= 10 it will throw a max_depth_error
    Nested10 = nest_list(<<"val">>, 10),
    ?assertError(max_depth_error, less_nif(Nested10, Nested10)),

    % Then it should transparently jump to erlang land
    ?assertEqual(0, less(Nested10, Nested10)).

max_depth_error_obj_test() ->
    % NIF can handle terms with depth <= 9
    Nested9 = nest_obj(<<"k">>, <<"v">>, 9),
    ?assertEqual(0, less_nif(Nested9, Nested9)),

    % At depth >= 10 it will throw a max_depth_error
    Nested10 = nest_obj(<<"k">>, <<"v">>, 10),
    ?assertError(max_depth_error, less_nif(Nested10, Nested10)),

    % Then it should transparently jump to erlang land
    ?assertEqual(0, less(Nested10, Nested10)).

compare_strings_nif_test() ->
    ?assertEqual(-1, compare_strings(<<"a">>, <<"b">>)),
    ?assertEqual(0, compare_strings(<<"a">>, <<"a">>)),
    ?assertEqual(1, compare_strings(<<"b">>, <<"a">>)),

    LargeBin1 = <<<<"x">> || _ <- lists:seq(1, 1000000)>>,
    LargeBin2 = <<LargeBin1/binary, "x">>,
    ?assertEqual(-1, compare_strings(LargeBin1, LargeBin2)),
    ?assertEqual(1, compare_strings(LargeBin2, LargeBin1)),
    ?assertEqual(0, compare_strings(LargeBin1, LargeBin1)),

    ?assertError(badarg, compare_strings(42, <<"a">>)),
    ?assertError(badarg, compare_strings(<<"a">>, 42)),
    ?assertError(badarg, compare_strings(42, 42)).

% Here we test sort key can handle keys larger than the internal 256 stack
% buffer just so we can get some coverage there.
get_sort_key_nif_large_test() ->
    Small = binary:copy(<<"a">>, 16),
    Large = binary:copy(<<"a">>, 1000),
    SmallKey = couch_ejson_compare:get_sort_key_nif(Small),
    LargeKey = couch_ejson_compare:get_sort_key_nif(Large),

    ?assert(byte_size(SmallKey) =< 256),
    ?assert(byte_size(LargeKey) > 512),

    % Check the large heap path against less/2 just for belt and suspenders
    Larger = <<Large/binary, "b">>,
    ?assertEqual(-1, less(Large, Larger)),
    ?assert(LargeKey < couch_ejson_compare:get_sort_key_nif(Larger)),

    % Adding a lot of zero width junk to a large string still works
    ZeroWidth = binary:copy(<<16#E2, 16#80, 16#8B>>, 300),
    Equiv = <<Large/binary, ZeroWidth/binary>>,
    ?assertEqual(0, less(Large, Equiv)),
    ?assertEqual(LargeKey, couch_ejson_compare:get_sort_key_nif(Equiv)).

sort_key_max_json_obj_test() ->
    Max = couch_ejson_compare:sort_key(?MAX_JSON_OBJ),
    ?assertEqual(Max, couch_ejson_compare:sort_key(?MAX_JSON_OBJ)),
    lists:foreach(
        fun(V) -> ?assertEqual(-1, sort_key_cmp(V, ?MAX_JSON_OBJ)) end,
        [null, false, true, 42, <<"z">>, ?MAX_UNICODE_STRING, [1, 2], {[{<<"a">>, 1}]}]
    ).

% Helper functions

less(A, B) ->
    cmp_norm(couch_ejson_compare:less(A, B)).

less_nif(A, B) ->
    cmp_norm(couch_ejson_compare:less_nif(A, B)).

less_erl(A, B) ->
    cmp_norm(couch_ejson_compare:less_erl(A, B)).

compare_strings(A, B) ->
    couch_ejson_compare:compare_strings_nif(A, B).

nest_list(Val, 0) ->
    Val;
nest_list(Val, Depth) when is_integer(Depth), Depth > 0 ->
    [nest_list(Val, Depth - 1)].

nest_obj(K, V, 1) ->
    {[{K, V}]};
nest_obj(K, V, Depth) when is_integer(Depth), Depth > 1 ->
    {[{K, nest_obj(K, V, Depth - 1)}]}.

% Build a map of #{Val => PositionIndex} for the test values so that when any
% two are compared we can verify their position in the test list matches the
% compared result
get_positions(TestValues) ->
    lists:foldl(
        fun(Val, Acc) ->
            Acc#{Val => map_size(Acc)}
        end,
        #{},
        TestValues
    ).

% When two values are compared, check the test values positions index to ensure
% the order in the test value list matches the comparison result
expected_less(A, B, Positions) ->
    #{A := PosA, B := PosB} = Positions,
    if
        PosA =:= PosB -> 0;
        PosA < PosB -> -1;
        PosA > PosB -> 1
    end.

% Since collation functions can return magnitudes > 1, for example when
% comparing atoms A - B, we need to normalize the result to -1, 0, and 1.
cmp_norm(Cmp) when is_number(Cmp) ->
    if
        Cmp == 0 -> 0;
        Cmp < 0 -> -1;
        Cmp > 0 -> 1
    end.
