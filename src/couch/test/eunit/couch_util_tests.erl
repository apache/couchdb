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

-module(couch_util_tests).

-include_lib("couch/include/couch_eunit.hrl").

% For generating poisson distributed string lengths
% in the random unicode generation. This shoots
% for lengths centered around 24 characters. To
% change, replace this value with math:exp(-Length).
-define(POISSON_LIMIT, 3.775134544279098e-11).
-define(RANDOM_TEST_SIZE, 10000).

setup() ->
    %% We cannot start driver from here since it becomes bounded to eunit
    %% master process and the next couch_server_sup:start_link call will
    %% fail because server couldn't load driver since it already is.
    %%
    %% On other hand, we cannot unload driver here due to
    %% {error, not_loaded_by_this_process} while it is. Any ideas is welcome.
    %%
    Ctx = test_util:start_couch(),
    %% config:start_link(?CONFIG_CHAIN),
    %% {ok, _} = couch_drv:start_link(),
    Ctx.

teardown(Ctx) ->
    ok = test_util:stop_couch(Ctx),
    %% config:stop(),
    %% erl_ddll:unload_driver(couch_icu_driver),
    ok.


collation_test_() ->
    {
        "Collation tests",
        [
            {
                setup,
                fun setup/0, fun teardown/1,
                [
                    should_collate_ascii(),
                    should_collate_non_ascii()
                ]
            }
        ]
    }.

validate_callback_exists_test_() ->
    {
        "validate_callback_exists tests",
        [
            fun should_succeed_for_existent_cb/0,
            should_fail_for_missing_cb()
        ]
    }.

should_collate_ascii() ->
    ?_assertEqual(1, couch_util:collate(<<"foo">>, <<"bar">>)).

should_collate_non_ascii() ->
    ?_assertEqual(-1, couch_util:collate(<<"A">>, <<"aa">>)).

to_existed_atom_test() ->
    ?assert(couch_util:to_existing_atom(true)),
    ?assertMatch(foo, couch_util:to_existing_atom(<<"foo">>)),
    ?assertMatch(foobarbaz, couch_util:to_existing_atom("foobarbaz")).

implode_test() ->
    ?assertEqual([1, 38, 2, 38, 3], couch_util:implode([1, 2, 3], "&")).

trim_test() ->
    lists:map(fun(S) -> ?assertEqual("foo", couch_util:trim(S)) end,
              [" foo", "foo ", "\tfoo", " foo ", "foo\t", "foo\n", "\nfoo"]).

abs_pathname_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertEqual(Cwd ++ "/foo", couch_util:abs_pathname("./foo")).

flush_test() ->
    ?assertNot(couch_util:should_flush()),
    AcquireMem = fun() ->
        _IntsToAGazillion = lists:seq(1, 200000),
        _LotsOfData = lists:map(fun(_) -> <<"foobar">> end,
                                lists:seq(1, 500000)),
        _ = list_to_binary(_LotsOfData),

        %% Allocation 200K tuples puts us above the memory threshold
        %% Originally, there should be:
        %%      ?assertNot(should_flush())
        %% however, unlike for etap test, GC collects all allocated bits
        %% making this conditions fail. So we have to invert the condition
        %% since GC works, cleans the memory and everything is fine.
        ?assertNot(couch_util:should_flush())
    end,
    AcquireMem(),

    %% Checking to flush invokes GC
    ?assertNot(couch_util:should_flush()).

verify_test() ->
    ?assert(couch_util:verify("It4Vooya", "It4Vooya")),
    ?assertNot(couch_util:verify("It4VooyaX", "It4Vooya")),
    ?assert(couch_util:verify(<<"ahBase3r">>, <<"ahBase3r">>)),
    ?assertNot(couch_util:verify(<<"ahBase3rX">>, <<"ahBase3r">>)),
    ?assertNot(couch_util:verify(nil, <<"ahBase3r">>)).

find_in_binary_test_() ->
    Cases = [
        {<<"foo">>, <<"foobar">>, {exact, 0}},
        {<<"foo">>, <<"foofoo">>, {exact, 0}},
        {<<"foo">>, <<"barfoo">>, {exact, 3}},
        {<<"foo">>, <<"barfo">>, {partial, 3}},
        {<<"f">>, <<"fobarfff">>, {exact, 0}},
        {<<"f">>, <<"obarfff">>, {exact, 4}},
        {<<"f">>, <<"obarggf">>, {exact, 6}},
        {<<"f">>, <<"f">>, {exact, 0}},
        {<<"f">>, <<"g">>, not_found},
        {<<"foo">>, <<"f">>, {partial, 0}},
        {<<"foo">>, <<"g">>, not_found},
        {<<"foo">>, <<"">>, not_found},
        {<<"fofo">>, <<"foofo">>, {partial, 3}},
        {<<"foo">>, <<"gfobarfo">>, {partial, 6}},
        {<<"foo">>, <<"gfobarf">>, {partial, 6}},
        {<<"foo">>, <<"gfobar">>, not_found},
        {<<"fog">>, <<"gbarfogquiz">>, {exact, 4}},
        {<<"ggg">>, <<"ggg">>, {exact, 0}},
        {<<"ggg">>, <<"ggggg">>, {exact, 0}},
        {<<"ggg">>, <<"bggg">>, {exact, 1}},
        {<<"ggg">>, <<"bbgg">>, {partial, 2}},
        {<<"ggg">>, <<"bbbg">>, {partial, 3}},
        {<<"ggg">>, <<"bgbggbggg">>, {exact, 6}},
        {<<"ggg">>, <<"bgbggb">>, not_found}
    ],
    lists:map(
        fun({Needle, Haystack, Result}) ->
            Msg = lists:flatten(io_lib:format("Looking for ~s in ~s",
                                              [Needle, Haystack])),
            {Msg, ?_assertMatch(Result,
                                couch_util:find_in_binary(Needle, Haystack))}
        end, Cases).

should_succeed_for_existent_cb() ->
    ?_assert(couch_util:validate_callback_exists(lists, any, 2)).

should_fail_for_missing_cb() ->
    Cases = [
        {unknown_module, any, 1},
        {erlang, unknown_function, 1},
        {erlang, whereis, 100}
    ],
    lists:map(
        fun({M, F, A} = MFA) ->
            Name = lists:flatten(io_lib:format("~w:~w/~w", [M, F, A])),
            {Name, ?_assertThrow(
                {error, {undefined_callback, Name, MFA}},
                couch_util:validate_callback_exists(M, F, A))}
        end, Cases).

to_hex_test_() ->
    [
        ?_assertEqual("", couch_util:to_hex([])),
        ?_assertEqual("010203faff", couch_util:to_hex([1, 2, 3, 250, 255])),
        ?_assertEqual("", couch_util:to_hex(<<>>)),
        ?_assertEqual("010203faff", couch_util:to_hex(<<1, 2, 3, 250, 255>>))
    ].

json_decode_test_() ->
    [
        ?_assertEqual({[]}, couch_util:json_decode(<<"{}">>)),
        ?_assertEqual({[]}, couch_util:json_decode(<<"{}">>, [])),
        ?_assertEqual(#{}, couch_util:json_decode(<<"{}">>, [return_maps]))
    ].

sort_key_test_() ->
    {
        "Sort Key tests",
        [
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_get_sort_key/1,
                    fun test_get_sort_key_jiffy_string/1,
                    fun test_get_sort_key_fails_on_bad_input/1,
                    fun test_get_sort_key_longer_than_buffer/1,
                    fun test_sort_key_collation/1,
                    fun test_sort_key_list_sort/1
                ]
            }
        ]
    }.

test_get_sort_key(_) ->
    Strs = [
        <<"">>,
        <<"foo">>,
        <<"bar">>,
        <<"Bar">>,
        <<"baz">>,
        <<"BAZ">>,
        <<"quaz">>,
        <<"1234fdsa">>,
        <<"1234">>,
        <<"pizza">>
    ],
    Pairs = [{S1, S2} || S1 <- Strs, S2 <- Strs],
    lists:map(fun({S1, S2}) ->
        S1K = couch_util:get_sort_key(S1),
        S2K = couch_util:get_sort_key(S2),
        SortRes = sort_keys(S1K, S2K),
        Comment = list_to_binary(io_lib:format("strcmp(~p, ~p)", [S1, S2])),
        CollRes = couch_util:collate(S1, S2),
        {Comment, ?_assertEqual(SortRes, CollRes)}
    end, Pairs).

test_get_sort_key_jiffy_string(_) ->
    %% jiffy:decode does not null terminate strings
    %% so we use it here to test unterminated strings
    {[{S1,S2}]} = jiffy:decode(<<"{\"foo\": \"bar\"}">>),
    S1K = couch_util:get_sort_key(S1),
    S2K = couch_util:get_sort_key(S2),
    SortRes = sort_keys(S1K, S2K),
    CollRes = couch_util:collate(S1, S2),
    ?_assertEqual(SortRes, CollRes).

test_get_sort_key_fails_on_bad_input(_) ->
    %% generated with crypto:strong_rand_bytes
    %% contains invalid character, should error
    S = <<209,98,222,144,60,163,72,134,206,157>>,
    Res = couch_util:get_sort_key(S),
    ?_assertEqual(error, Res).

test_get_sort_key_longer_than_buffer(_) ->
    %% stack allocated buffer is 1024 units
    %% test resize logic with strings > 1024 char
    Extra = list_to_binary(["a" || _ <- lists:seq(1, 1200)]),
    ?_assert(is_binary(Extra)).

test_sort_key_collation(_) ->
    ?_test(begin
        lists:foreach(fun(_) ->
            K1 = random_unicode_binary(),
            SK1 = couch_util:get_sort_key(K1),

            K2 = random_unicode_binary(),
            SK2 = couch_util:get_sort_key(K2),

            % Probably kinda silly but whatevs
            ?assertEqual(couch_util:collate(K1, K1), sort_keys(SK1, SK1)),
            ?assertEqual(couch_util:collate(K2, K2), sort_keys(SK2, SK2)),

            ?assertEqual(couch_util:collate(K1, K2), sort_keys(SK1, SK2)),
            ?assertEqual(couch_util:collate(K2, K1), sort_keys(SK2, SK1))
        end, lists:seq(1, ?RANDOM_TEST_SIZE))
    end).

test_sort_key_list_sort(_) ->
    ?_test(begin
        RandomKeys = lists:map(fun(_) ->
            random_unicode_binary()
        end, lists:seq(1, ?RANDOM_TEST_SIZE)),

        CollationSorted = lists:sort(fun(A, B) ->
            couch_util:collate(A, B) =< 0
        end, RandomKeys),

        SortKeys = lists:map(fun(K) ->
            {couch_util:get_sort_key(K), K}
        end, RandomKeys),
        {_, SortKeySorted} = lists:unzip(lists:sort(SortKeys)),

        ?assertEqual(CollationSorted, SortKeySorted)
    end).

sort_keys(S1, S2) ->
    case S1 < S2 of
        true ->
            -1;
        false -> case S1 =:= S2 of
            true ->
                0;
            false ->
                1
        end
    end.

random_unicode_binary() ->
    Size = poisson_length(0, rand:uniform()),
    Chars = [random_unicode_char() || _ <- lists:seq(1, Size)],
    <<_/binary>> = unicode:characters_to_binary(Chars).

poisson_length(N, Acc) when Acc > ?POISSON_LIMIT ->
    poisson_length(N + 1, Acc * rand:uniform());
poisson_length(N, _) ->
    N.

random_unicode_char() ->
    BaseChar = rand:uniform(16#FFFD + 1) - 1,
    case BaseChar of
        BC when BC >= 16#D800, BC =< 16#DFFF ->
            % This range is reserved for surrogate pair
            % encodings.
            random_unicode_char();
        BC ->
            BC
    end.
