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

    etap:plan(length(cases())),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.


test() ->
    lists:foreach(fun({Needle, Haystack, Result}) ->
        try
        Msg = io_lib:format("Looking for ~s in ~s", [Needle, Haystack]),
        etap:is(couch_util:find_in_binary(Needle, Haystack), Result, Msg)
        catch _T:_R ->
            etap:diag("~p", [{_T, _R}])
        end
    end, cases()),
    ok.


cases() ->
    [
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
    ].
