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
    etap:plan(4),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    Stemmed1 = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    Stemmed2 = [{2, {"1aa", "bar", []}}],

    etap:is(
        [],
        couch_key_tree:find_missing(TwoChildSibs, [{0,"1"}, {1,"1a"}]),
        "Look for missing keys."
    ),

    etap:is(
        [{0, "10"}, {100, "x"}],
        couch_key_tree:find_missing(
            TwoChildSibs,
            [{0,"1"}, {0, "10"}, {1,"1a"}, {100, "x"}]
        ),
        "Look for missing keys."
    ),

    etap:is(
        [{0, "1"}, {100, "x"}],
        couch_key_tree:find_missing(
            Stemmed1,
            [{0,"1"}, {1,"1a"}, {100, "x"}]
        ),
        "Look for missing keys."
    ),
    etap:is(
        [{0, "1"}, {1,"1a"}, {100, "x"}],
        couch_key_tree:find_missing(
            Stemmed2,
            [{0,"1"}, {1,"1a"}, {100, "x"}]
        ),
        "Look for missing keys."
    ),

    ok.
