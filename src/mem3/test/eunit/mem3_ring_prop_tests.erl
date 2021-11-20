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

-module(mem3_ring_prop_tests).

-ifdef(WITH_PROPER).

-include_lib("couch/include/couch_eunit_proper.hrl").

property_test_() ->
    ?EUNIT_QUICKCHECK(60).

% Properties

prop_get_ring_with_connected_intervals() ->
    ?FORALL(
        {Start, End},
        oneof(ranges()),
        ?FORALL(
            Intervals,
            g_connected_intervals(Start, End),
            mem3_util:get_ring(Intervals, Start, End) =:= lists:sort(Intervals)
        )
    ).

prop_get_ring_connected_plus_random_intervals() ->
    ?FORALL(
        {Intervals, Extra},
        {g_connected_intervals(1, 100), g_random_intervals(1, 100)},
        ?IMPLIES(
            sets:is_disjoint(endpoints(Intervals), endpoints(Extra)),
            begin
                AllInts = Intervals ++ Extra,
                Ring = mem3_util:get_ring(AllInts, 1, 100),
                Ring =:= lists:sort(Intervals)
            end
        )
    ).

prop_get_ring_connected_with_sub_intervals() ->
    ?FORALL(
        Intervals,
        g_connected_intervals(1, 100),
        ?FORALL(
            SubIntervals,
            g_subintervals(Intervals),
            begin
                AllInts = Intervals ++ SubIntervals,
                Ring = mem3_util:get_ring(AllInts, 1, 100),
                Ring =:= lists:sort(Intervals)
            end
        )
    ).

prop_get_ring_with_disconnected_intervals() ->
    ?FORALL(
        {Start, End},
        oneof(ranges()),
        ?FORALL(
            Intervals,
            g_disconnected_intervals(Start, End),
            mem3_util:get_ring(Intervals, Start, End) =:= []
        )
    ).

% Generators

ranges() ->
    [{1, 10}, {0, 2 bsl 31 - 1}, {2 bsl 31 - 10, 2 bsl 31 - 1}].

g_connected_intervals(Begin, End) ->
    ?SIZED(Size, g_connected_intervals(Begin, End, 5 * Size)).

g_connected_intervals(Begin, End, Split) when Begin =< End ->
    ?LET(
        N,
        choose(0, Split),
        begin
            if
                N == 0 ->
                    [{Begin, End}];
                N > 0 ->
                    Ns = lists:seq(1, N - 1),
                    Bs = lists:usort([rand_range(Begin, End) || _ <- Ns]),
                    Es = [B - 1 || B <- Bs],
                    shuffle(lists:zip([Begin] ++ Bs, Es ++ [End]))
            end
        end
    ).

g_non_trivial_connected_intervals(Begin, End, Split) ->
    ?SUCHTHAT(
        Connected,
        g_connected_intervals(Begin, End, Split),
        length(Connected) > 1
    ).

g_disconnected_intervals(Begin, End) ->
    ?SIZED(Size, g_disconnected_intervals(Begin, End, Size)).

g_disconnected_intervals(Begin, End, Split) when Begin =< End ->
    ?LET(
        Connected,
        g_non_trivial_connected_intervals(Begin, End, Split),
        begin
            I = rand:uniform(length(Connected)) - 1,
            {Before, [_ | After]} = lists:split(I, Connected),
            Before ++ After
        end
    ).

g_subintervals(Intervals) ->
    lists:foldl(fun(R, Acc) -> split_interval(R) ++ Acc end, [], Intervals).

split_interval({B, E}) when E - B >= 2 ->
    E1 = rand_range(B, E) - 1,
    B1 = E1 + 1,
    [{B, E1}, {B1, E}];
split_interval(_Range) ->
    [].

g_random_intervals(Start, End) ->
    ?LET(
        N,
        choose(1, 10),
        begin
            [
                begin
                    B = rand_range(Start, End),
                    E = rand_range(B, End),
                    {B, E}
                end
             || _ <- lists:seq(1, N)
            ]
        end
    ).

rand_range(B, B) ->
    B;
rand_range(B, E) ->
    B + rand:uniform(E - B).

shuffle(L) ->
    Tagged = [{rand:uniform(), X} || X <- L],
    [X || {_, X} <- lists:sort(Tagged)].

endpoints(Ranges) ->
    {Begins, Ends} = lists:unzip(Ranges),
    sets:from_list(Begins ++ Ends).

-endif.
