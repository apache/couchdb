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
    EmptyTree = [],
    One = [{0, {"1","foo",[]}}],
    TwoChildSibs = [{0, {"1","foo", [{"1a", "bar", []}, {"1b", "bar", []}]}}],
    Stemmed = [{2, {"1bb", "boo", []}}],

    etap:is(0, couch_key_tree:count_leafs(EmptyTree),
        "Empty trees have no leaves."),

    etap:is(1, couch_key_tree:count_leafs(One),
        "Single node trees have a single leaf."),

    etap:is(2, couch_key_tree:count_leafs(TwoChildSibs),
        "Two children siblings counted as two leaves."),

    etap:is(1, couch_key_tree:count_leafs(Stemmed),
        "Stemming does not affect leaf counting."),

    ok.
