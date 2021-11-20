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

-module(smoosh_priority_queue).

-export([new/0, last_updated/2, is_key/2, in/4, in/5, out/1, size/1, info/1]).

-record(priority_queue, {
    dict = dict:new(),
    tree = gb_trees:empty()
}).

new() ->
    #priority_queue{}.

last_updated(Key, #priority_queue{dict = Dict}) ->
    case dict:find(Key, Dict) of
        {ok, {_Priority, {LastUpdatedMTime, _MInt}}} ->
            LastUpdatedMTime;
        error ->
            false
    end.

is_key(Key, #priority_queue{dict = Dict}) ->
    dict:is_key(Key, Dict).

in(Key, Value, Priority, Q) ->
    in(Key, Value, Priority, infinity, Q).

in(Key, Value, Priority, Capacity, #priority_queue{dict = Dict, tree = Tree}) ->
    Tree1 =
        case dict:find(Key, Dict) of
            {ok, TreeKey} ->
                gb_trees:delete_any(TreeKey, Tree);
            error ->
                Tree
        end,
    Now = {erlang:monotonic_time(), erlang:unique_integer([monotonic])},
    TreeKey1 = {Priority, Now},
    Tree2 = gb_trees:enter(TreeKey1, {Key, Value}, Tree1),
    Dict1 = dict:store(Key, TreeKey1, Dict),
    truncate(Capacity, #priority_queue{dict = Dict1, tree = Tree2}).

out(#priority_queue{dict = Dict, tree = Tree}) ->
    case gb_trees:is_empty(Tree) of
        true ->
            false;
        false ->
            {_, {Key, Value}, Tree1} = gb_trees:take_largest(Tree),
            Dict1 = dict:erase(Key, Dict),
            {Key, Value, #priority_queue{dict = Dict1, tree = Tree1}}
    end.

size(#priority_queue{tree = Tree}) ->
    gb_trees:size(Tree).

info(#priority_queue{tree = Tree} = Q) ->
    [
        {size, ?MODULE:size(Q)}
        | case gb_trees:is_empty(Tree) of
            true ->
                [];
            false ->
                {Min, _, _} = gb_trees:take_smallest(Tree),
                {Max, _, _} = gb_trees:take_largest(Tree),
                [{min, Min}, {max, Max}]
        end
    ].

truncate(infinity, Q) ->
    Q;
truncate(Capacity, Q) when Capacity > 0 ->
    truncate(Capacity, ?MODULE:size(Q), Q).

truncate(Capacity, Size, Q) when Size =< Capacity ->
    Q;
truncate(Capacity, Size, #priority_queue{dict = Dict, tree = Tree}) when Size > 0 ->
    {_, {Key, _}, Tree1} = gb_trees:take_smallest(Tree),
    Q1 = #priority_queue{dict = dict:erase(Key, Dict), tree = Tree1},
    truncate(Capacity, ?MODULE:size(Q1), Q1).
