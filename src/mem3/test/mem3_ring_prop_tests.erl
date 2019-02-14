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


-include_lib("triq/include/triq.hrl").
-triq(eunit).

-compile(export_all).


% Properties

prop_get_ring_with_connected_intervals() ->
    ?FORALL({Start, End}, oneof(ranges()),
        ?FORALL(Intervals, g_connected_intervals(Start, End),
            mem3_util:get_ring(Intervals, Start, End) =/= []
        )
    ).


prop_get_ring_with_disconnected_intervals() ->
    ?FORALL({Start, End}, oneof(ranges()),
        ?FORALL(Intervals, g_disconnected_intervals(Start, End),
            mem3_util:get_ring(Intervals, Start, End) =:= []
        )
    ).


% Generators

ranges() ->
    [{0,1}, {1, 10}, {0, 2 bsl 31 - 1}, {2 bsl 31 - 10, 2 bsl 31 - 1}].


g_connected_intervals(Begin, End) ->
    ?SIZED(Size, g_connected_intervals(Begin, End, 10 * Size)).


g_connected_intervals(Begin, End, Split) when Begin =< End ->
    ?LET(N, choose(0, Split),
    begin
        if
            N == 0 ->
                [{Begin, End}];
            N > 0 ->
                Ns = lists:seq(1, N - 1),
                Bs = lists:usort([rand_range(Begin, End) || _ <- Ns]),
                Es = [B - 1 || B <- Bs],
                lists:zip([Begin] ++ Bs, Es ++ [End])
        end
    end).


g_non_trivial_connected_intervals(Begin, End, Split) ->
    ?SUCHTHAT(Connected, g_connected_intervals(Begin, End, Split),
        length(Connected) > 1).


g_disconnected_intervals(Begin, End) ->
    ?SIZED(Size, g_disconnected_intervals(Begin, End, Size)).


g_disconnected_intervals(Begin, End, Split) when Begin =< End ->
    ?LET(Connected, g_non_trivial_connected_intervals(Begin, End, Split),
    begin
        I = triq_rnd:uniform(length(Connected)) - 1,
        {Before, [_ | After]} = lists:split(I, Connected),
        Before ++ After
    end).


rand_range(B, E) ->
    B + triq_rnd:uniform(E - B).
