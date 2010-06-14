#!/usr/bin/env escript
%% -*- erlang -*-

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
    application:start(crypto),

    etap:plan(14),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    % to_existing_atom
    etap:is(true, couch_util:to_existing_atom(true), "An atom is an atom."),
    etap:is(foo, couch_util:to_existing_atom(<<"foo">>),
        "A binary foo is the atom foo."),
    etap:is(foobarbaz, couch_util:to_existing_atom("foobarbaz"),
        "A list of atoms is one munged atom."),

    % implode
    etap:is([1, 38, 2, 38, 3], couch_util:implode([1,2,3],"&"),
        "use & as separator in list."),

    % trim
    Strings = [" foo", "foo ", "\tfoo", " foo ", "foo\t", "foo\n", "\nfoo"],
    etap:ok(lists:all(fun(S) -> couch_util:trim(S) == "foo" end, Strings),
        "everything here trimmed should be foo."),

    % abs_pathname
    {ok, Cwd} = file:get_cwd(),
    etap:is(Cwd ++ "/foo", couch_util:abs_pathname("./foo"),
        "foo is in this directory."),

    % should_flush
    etap:ok(not couch_util:should_flush(),
        "Not using enough memory to flush."),
    AcquireMem = fun() ->
        IntsToAGazillion = lists:seq(1, 200000),
        LotsOfData = lists:map(
            fun(Int) -> {Int, <<"foobar">>} end,
        lists:seq(1, 500000)),
        etap:ok(couch_util:should_flush(),
            "Allocation 200K tuples puts us above the memory threshold.")
    end,
    AcquireMem(),

    etap:ok(not couch_util:should_flush(),
        "Checking to flush invokes GC."),

    % verify
    etap:is(true, couch_util:verify("It4Vooya", "It4Vooya"),
         "String comparison."),
    etap:is(false, couch_util:verify("It4VooyaX", "It4Vooya"),
         "String comparison (unequal lengths)."),
    etap:is(true, couch_util:verify(<<"ahBase3r">>, <<"ahBase3r">>),
        "Binary comparison."),
    etap:is(false, couch_util:verify(<<"ahBase3rX">>, <<"ahBase3r">>),
        "Binary comparison (unequal lengths)."),
    etap:is(false, couch_util:verify(nil, <<"ahBase3r">>),
        "Binary comparison with atom."),

    ok.
