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

-include("couch_eunit.hrl").


setup() ->
    %% We cannot start driver from here since it becomes bounded to eunit
    %% master process and the next couch_server_sup:start_link call will
    %% fail because server couldn't load driver since it already is.
    %%
    %% On other hand, we cannot unload driver here due to
    %% {error, not_loaded_by_this_process} while it is. Any ideas is welcome.
    %%
    couch_server_sup:start_link(?CONFIG_CHAIN),
    %% couch_config:start_link(?CONFIG_CHAIN),
    %% {ok, _} = couch_drv:start_link(),
    ok.

teardown(_) ->
    couch_server_sup:stop(),
    %% couch_config:stop(),
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
        _BigBin = list_to_binary(_LotsOfData),

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
