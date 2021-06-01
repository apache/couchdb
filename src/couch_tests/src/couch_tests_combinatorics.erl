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

-module(couch_tests_combinatorics).

-export([
    powerset/1,
    permutations/1,
    product/1,
    binary_combinations/1,
    n_combinations/2
]).

%% @doc powerset(Items)
%% Generate powerset for a given list of Items
%% By Hynek - Pichi - Vychodil
%% For example:
%%   1> powerset([foo, bar, baz]).
%%      [
%%          [foo],
%%          [foo,baz],
%%          [foo,bar,baz],
%%          [foo,bar],
%%          [bar],
%%          [bar,baz],
%%          [baz],
%%          []
%%      ]
-spec powerset(Elements :: list()) -> [list()].

powerset([]) ->
    [[]];
powerset([H | T]) ->
    PT = powerset(T),
    powerset(H, PT, PT).

powerset(_, [], Acc) ->
    Acc;
powerset(X, [H | T], Acc) ->
    powerset(X, T, [[X | H] | Acc]).

%% @doc permutations(Items)
%% Return all premutations of given list of Items.
%% from http://erlang.org/doc/programming_examples/list_comprehensions.html
%% For example:
%%   1> permutations([foo, bar, baz]).
%%      [
%%          [foo, bar, baz],
%%          [foo, baz, bar],
%%          [bar, foo, baz],
%%          [bar, baz, foo],
%%          [baz, foo, bar],
%%          [baz, bar, foo]
%%      ]
-spec permutations(Elements :: list()) -> [list()].

permutations([]) ->
    [[]];
permutations(L)  ->
    [[H | T] || H <- L, T <- permutations(L -- [H])].

%% @doc product({Items1, Items2, ..., ItemsN})
%% Return cartesian product of multiple sets represented as list of lists
%% From: http://stackoverflow.com/a/23886680
%% For example:
%%   1> product([[foo, bar], [1,2,3]]).
%%      [
%%        [foo, 1],
%%        [foo, 2],
%%        [foo, 3],
%%        [bar, 1],
%%        [bar, 2],
%%        [bar, 3]
%%      ]
-spec product(Elements :: list()) -> [list()].

product([H])   ->
    [[A] || A <- H];
product([H | T]) ->
    [[A | B] || A <- H, B <- product(T)].

%% @doc binary_combinations(NBits).
%% Generate all combinations of true and false for specified number of bits.
%% For example:
%%   1> binary_combinations(3).
%%      [
%%       [ false , false , false ],
%%       [ false , false , true  ],
%%       [ false , true  , false ],
%%       [ false , true  , true  ],
%%       [ true  , false , false ],
%%       [ true  , false , true  ],
%%       [ true  , true  , false ],
%%       [ true  , true  , true  ]
%%      ]
%%   2> length(binary_combinations(3))
%%     8
-spec binary_combinations(NBits :: pos_integer()) -> [list(boolean())].

binary_combinations(NBits) ->
    product(lists:duplicate(NBits, [true, false])).


%% @doc combinations(N, Items).
%% Generate all combinations by choosing N values from a given list of Items
%% in sorted order. Each combination is sorted and the entire table is sorted.
%% For example:
%%   1> couch_tests_combinatorics:n_combinations(2, [mon, tue, wed, thu, fri]).
%%      [
%%       [mon, tue],
%%       [mon, wed],
%%       [mon, thu],
%%       [mon, fri],
%%       [tue, wed],
%%       [tue, thu],
%%       [tue, fri],
%%       [wed, thu],
%%       [wed, fri],
%%       [thu, fri]
%%      ]
-spec n_combinations(Size :: pos_integer(), Elements :: list()) -> [list()].

n_combinations(0, _) ->
    [[]];
n_combinations(_, []) ->
    [];
n_combinations(N, [H | T]) ->
    [[H | L] || L <- n_combinations(N - 1, T)] ++ n_combinations(N, T).
