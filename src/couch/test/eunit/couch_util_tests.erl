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

validate_callback_exists_test_() ->
    {
        "validate_callback_exists tests",
        [
            fun should_succeed_for_existent_cb/0,
            should_fail_for_missing_cb()
        ]
    }.

to_existed_atom_test() ->
    ?assert(couch_util:to_existing_atom(true)),
    ?assertMatch(foo, couch_util:to_existing_atom(<<"foo">>)),
    ?assertMatch(foobarbaz, couch_util:to_existing_atom("foobarbaz")).

implode_test() ->
    ?assertEqual([1, 38, 2, 38, 3], couch_util:implode([1, 2, 3], "&")).

trim_test() ->
    lists:map(
        fun(S) -> ?assertEqual("foo", couch_util:trim(S)) end,
        [" foo", "foo ", "\tfoo", " foo ", "foo\t", "foo\n", "\nfoo"]
    ).

abs_pathname_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertEqual(Cwd ++ "/foo", couch_util:abs_pathname("./foo")).

flush_test() ->
    ?assertNot(couch_util:should_flush()),
    AcquireMem = fun() ->
        _IntsToAGazillion = lists:seq(1, 200000),
        _LotsOfData = lists:map(
            fun(_) -> <<"foobar">> end,
            lists:seq(1, 500000)
        ),
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
            Msg = lists:flatten(
                io_lib:format(
                    "Looking for ~s in ~s",
                    [Needle, Haystack]
                )
            ),
            {Msg,
                ?_assertMatch(
                    Result,
                    couch_util:find_in_binary(Needle, Haystack)
                )}
        end,
        Cases
    ).

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
            {Name,
                ?_assertThrow(
                    {error, {undefined_callback, Name, MFA}},
                    couch_util:validate_callback_exists(M, F, A)
                )}
        end,
        Cases
    ).

to_hex_test_() ->
    [
        ?_assertEqual("", couch_util:to_hex([])),
        ?_assertEqual(<<>>, couch_util:to_hex_bin(<<>>)),
        ?_assertEqual(<<"00">>, couch_util:to_hex_bin(<<0>>)),
        ?_assertEqual(<<"01">>, couch_util:to_hex_bin(<<1>>)),
        ?_assertEqual("010203faff", couch_util:to_hex([1, 2, 3, 250, 255])),
        ?_assertEqual(<<"010203faff">>, couch_util:to_hex_bin(<<1, 2, 3, 250, 255>>)),
        ?_assertEqual("", couch_util:to_hex(<<>>)),
        ?_assertEqual("010203faff", couch_util:to_hex(<<1, 2, 3, 250, 255>>))
    ].

to_hex_range_test() ->
    lists:foreach(
        fun(PrefixSize) ->
            lists:foreach(
                fun(I) ->
                    Prefix = list_to_binary(lists:duplicate(PrefixSize, 1)),
                    Bin = <<Prefix/binary, I:8/integer>>,
                    ?assertEqual(list_to_binary(to_hex_simple(Bin)), couch_util:to_hex_bin(Bin))
                end,
                lists:seq(0, 16#ff)
            )
        end,
        lists:seq(0, 8)
    ).

% Use previous implementation from couch_util for validation
%
to_hex_simple(<<Hi:4, Lo:4, Rest/binary>>) ->
    [nibble_to_hex(Hi), nibble_to_hex(Lo) | to_hex_simple(Rest)];
to_hex_simple(<<>>) ->
    [].

nibble_to_hex(0) -> $0;
nibble_to_hex(1) -> $1;
nibble_to_hex(2) -> $2;
nibble_to_hex(3) -> $3;
nibble_to_hex(4) -> $4;
nibble_to_hex(5) -> $5;
nibble_to_hex(6) -> $6;
nibble_to_hex(7) -> $7;
nibble_to_hex(8) -> $8;
nibble_to_hex(9) -> $9;
nibble_to_hex(10) -> $a;
nibble_to_hex(11) -> $b;
nibble_to_hex(12) -> $c;
nibble_to_hex(13) -> $d;
nibble_to_hex(14) -> $e;
nibble_to_hex(15) -> $f.

json_decode_test_() ->
    [
        ?_assertEqual({[]}, couch_util:json_decode(<<"{}">>)),
        ?_assertEqual({[]}, couch_util:json_decode(<<"{}">>, [])),
        ?_assertEqual(#{}, couch_util:json_decode(<<"{}">>, [return_maps]))
    ].
