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
    etap:plan(3),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    TwoChild = [{0, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]}]}}],
    Stemmed1 = [{1, {"1a", "bar", [{"1aa", "bar", []}]}}],
    Stemmed2 = [{2, {"1aa", "bar", []}}],

    etap:is(TwoChild, couch_key_tree:stem(TwoChild, 3),
        "Stemming more levels than what exists does nothing."),

    etap:is(Stemmed1, couch_key_tree:stem(TwoChild, 2),
        "Stemming with a depth of two returns the deepest two nodes."),

    etap:is(Stemmed2, couch_key_tree:stem(TwoChild, 1),
        "Stemming to a depth of one returns the deepest node."),

    ok.
