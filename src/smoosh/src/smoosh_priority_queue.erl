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

-export([
    new/1,
    name/1,
    in/4,
    out/1,
    info/1,
    flush/1,
    to_map/1,
    from_map/3
]).

-record(priority_queue, {
    name,
    map,
    tree
}).

new(Name) ->
    #priority_queue{name = Name, map = #{}, tree = gb_trees:empty()}.

name(#priority_queue{name = Name}) ->
    Name.

flush(#priority_queue{} = Q) ->
    Q#priority_queue{map = #{}, tree = gb_trees:empty()}.

in(Key, Priority, Capacity, #priority_queue{map = Map, tree = Tree} = Q) ->
    case maps:find(Key, Map) of
        {ok, {Priority, _}} ->
            % Priority matches, keep everything as is. This might be the case
            % for upgrade channels, for instance, where priority is 1.
            Q;
        {ok, TreeKey} ->
            Tree1 = gb_trees:delete(TreeKey, Tree),
            insert(Key, Priority, Capacity, Q#priority_queue{tree = Tree1});
        error ->
            insert(Key, Priority, Capacity, Q)
    end.

out(#priority_queue{map = Map, tree = Tree} = Q) ->
    case gb_trees:is_empty(Tree) of
        true ->
            false;
        false ->
            {_, Key, Tree1} = gb_trees:take_largest(Tree),
            {Key, Q#priority_queue{map = maps:remove(Key, Map), tree = Tree1}}
    end.

qsize(#priority_queue{tree = Tree}) ->
    gb_trees:size(Tree).

info(#priority_queue{tree = Tree} = Q) ->
    case gb_trees:is_empty(Tree) of
        true ->
            #{size => qsize(Q), min => 0, max => 0};
        false ->
            {{Min, _}, _} = gb_trees:smallest(Tree),
            {{Max, _}, _} = gb_trees:largest(Tree),
            #{size => qsize(Q), min => Min, max => Max}
    end.

insert(Key, Priority, Capacity, #priority_queue{tree = Tree, map = Map} = Q) ->
    TreeKey = {Priority, make_ref()},
    Tree1 = gb_trees:insert(TreeKey, Key, Tree),
    truncate(Capacity, Q#priority_queue{map = Map#{Key => TreeKey}, tree = Tree1}).

truncate(Capacity, Q) when is_integer(Capacity), Capacity > 0 ->
    truncate(Capacity, qsize(Q), Q).

truncate(Capacity, Size, Q) when is_integer(Capacity), Size =< Capacity ->
    Q;
truncate(Capacity, Size, Q) when is_integer(Capacity), Size > 0 ->
    #priority_queue{map = Map, tree = Tree} = Q,
    {_, Key, Tree1} = gb_trees:take_smallest(Tree),
    Q1 = Q#priority_queue{map = maps:remove(Key, Map), tree = Tree1},
    truncate(Capacity, qsize(Q1), Q1).

% Serialize the queue to/from simple maps which look like #{Key => Priority}.
% The intent is for these to be used by the smoosh persistence facility.
%
to_map(#priority_queue{map = Map}) ->
    Fun = fun(_Key, {Priority, _Ref}) ->
        Priority
    end,
    maps:map(Fun, Map).

from_map(Name, Capacity, #{} = SerializedMap) ->
    Fun = fun(Key, Priority, Acc) ->
        insert(Key, Priority, Capacity, Acc)
    end,
    maps:fold(Fun, new(Name), SerializedMap).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

-define(K1, <<"db1">>).
-define(K2, {<<"db1">>, <<"design/_doc1">>}).
-define(K3, {index_cleanup, <<"db1">>}).
-define(P1, 1).
-define(P2, 2.4).
-define(P3, infinity).

basics_test() ->
    Q = new("foo"),
    ?assertMatch(#priority_queue{}, Q),
    ?assertEqual("foo", name(Q)),
    ?assertEqual(0, maps:get(size, info(Q))).

empty_test() ->
    Q = new("foo"),
    ?assertEqual(false, out(Q)),
    ?assertEqual(Q, truncate(1, Q)),
    ?assertEqual(Q, flush(Q)),
    ?assertEqual(#{}, to_map(Q)),
    ?assertEqual(Q, from_map("foo", 1, #{})).

one_element_test() ->
    Q0 = new("foo"),
    Q = in(?K1, ?P1, 1, Q0),
    ?assertMatch(#priority_queue{}, Q),
    ?assertEqual(#{max => 1, min => 1, size => 1}, info(Q)),
    ?assertEqual(Q, truncate(1, Q)),
    ?assertMatch({?K1, #priority_queue{}}, out(Q)),
    {?K1, Q2} = out(Q),
    ?assertEqual(Q2, Q0),
    ?assertEqual(#{?K1 => ?P1}, to_map(Q)),
    Q3 = from_map("foo", 1, to_map(Q)),
    ?assertEqual("foo", name(Q3)),
    ?assertEqual(#{max => ?P1, min => ?P1, size => 1}, info(Q3)),
    ?assertEqual(to_map(Q), to_map(Q3)),
    ?assertEqual(Q0, flush(Q)).

multiple_elements_basics_test() ->
    Q0 = new("foo"),
    Q1 = in(?K1, ?P1, 10, Q0),
    Q2 = in(?K2, ?P2, 10, Q1),
    Q3 = in(?K3, ?P3, 10, Q2),
    ?assertEqual(#{max => ?P3, min => ?P1, size => 3}, info(Q3)),
    ?assertEqual([?K3, ?K2, ?K1], drain(Q3)).

update_element_same_priority_test() ->
    Q0 = new("foo"),
    Q1 = in(?K1, ?P1, 10, Q0),
    ?assertEqual(Q1, in(?K1, ?P1, 10, Q1)).

update_element_new_priority_test() ->
    Q0 = new("foo"),
    Q1 = in(?K1, ?P1, 10, Q0),
    Q2 = in(?K2, ?P2, 10, Q1),
    Q3 = in(?K1, ?P3, 10, Q2),
    ?assertEqual(#{max => ?P3, min => ?P2, size => 2}, info(Q3)),
    ?assertEqual([?K1, ?K2], drain(Q3)).

capacity_test() ->
    Q0 = new("foo"),
    Q1 = in(?K1, ?P1, 2, Q0),
    % Capacity = 1, one element only remains
    ?assertEqual([?K2], drain(in(?K2, ?P2, 1, Q1))),
    % Capacity = 2, only top two elements remain
    Q2 = in(?K2, ?P2, 2, Q1),
    Q3 = in(?K3, ?P3, 2, Q2),
    ?assertEqual([?K3, ?K2], drain(Q3)).

a_lot_of_elements_test() ->
    N = 100000,
    KVs = lists:map(
        fun(I) ->
            P = rand:uniform(100),
            {{I, P}, P}
        end,
        lists:seq(1, N)
    ),
    Q = from_map("foo", N, maps:from_list(KVs)),
    ?assertMatch(N, maps:get(size, info(Q))),
    {_, Priorities} = lists:unzip(drain(Q)),
    ?assertEqual(lists:reverse(lists:sort(Priorities)), Priorities).

drain(Q) ->
    lists:reverse(drain(out(Q), [])).

drain(false, Acc) ->
    Acc;
drain({Key, Q}, Acc) ->
    drain(out(Q), [Key | Acc]).

-endif.
