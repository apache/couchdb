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

-module(couch_ejson_size_tests).

-include_lib("eunit/include/eunit.hrl").

% 4 byte utf8 encoding
-define(HWAIR, $\x{10348}).
% 3 byte utf8 encoding
-define(EURO, $\x{20ac}).
% 2 byte utf8 encoding
-define(CENT, $\x{a2}).

ejson_size_test_() ->
    [
        ?_assertEqual(R, couch_ejson_size:encoded_size(Input))
     || {R, Input} <- [
            {1, 1},
            {1, 1},
            {2, -1},
            {1, 9},
            {2, 10},
            {3, -10},
            {2, 11},
            {2, 99},
            {3, 100},
            {3, 999},
            {4, 1000},
            {4, 9999},
            {5, 10000},

            {3, 0.0},
            {3, 0.1},
            {3, 1.0},
            {4, -1.0},
            {3, 1.0e9},
            {4, 1.0e10},
            {5, 1.0e-10},
            {5, 1.0e-99},
            {6, 1.0e-100},
            {3, 1.0e-323},

            {2, arr_nested(0)},
            {22, arr_nested(10)},
            {2002, arr_nested(1000)},
            {9, obj_nested(0)},
            {69, obj_nested(10)},
            {6009, obj_nested(1000)},

            {4, null},
            {4, true},
            {5, false},

            {3, str(1, $x)},
            {4, str(1, ?CENT)},
            {5, str(1, ?EURO)},
            {6, str(1, ?HWAIR)},
            {3, str(1, $\x{1})},
            {12, str(10, $x)},
            {22, str(10, ?CENT)},
            {32, str(10, ?EURO)},
            {42, str(10, ?HWAIR)},
            {12, str(10, $\x{1})}
        ]
    ].

%% Helper functions

arr_nested(MaxDepth) ->
    arr_nested(MaxDepth, 0).

obj_nested(MaxDepth) ->
    obj_nested(MaxDepth, 0).

obj(N, K, V) ->
    {[{K, V} || _ <- lists:seq(1, N)]}.

str(N, C) ->
    unicode:characters_to_binary([C || _ <- lists:seq(1, N)]).

arr_nested(MaxDepth, MaxDepth) ->
    [];
arr_nested(MaxDepth, Depth) ->
    [arr_nested(MaxDepth, Depth + 1)].

obj_nested(MaxDepth, MaxDepth) ->
    obj(1, <<"k">>, <<"v">>);
obj_nested(MaxDepth, Depth) ->
    {[{<<"k">>, obj_nested(MaxDepth, Depth + 1)}]}.
