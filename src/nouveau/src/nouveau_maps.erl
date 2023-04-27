%%
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2021. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

%% backport of OTP 24 map functions not present in 23.

-module(nouveau_maps).

-export([merge_with/3, foreach/2]).

-if(?OTP_RELEASE >= 24).
merge_with(Combiner, Map1, Map2) ->
    maps:merge_with(Combiner, Map1, Map2).

foreach(Fun, MapOrIter) ->
    maps:foreach(Fun, MapOrIter).

-else.

merge_with(Combiner, Map1, Map2) when
    is_map(Map1),
    is_map(Map2),
    is_function(Combiner, 3)
->
    case map_size(Map1) > map_size(Map2) of
        true ->
            Iterator = maps:iterator(Map2),
            merge_with_1(
                maps:next(Iterator),
                Map1,
                Map2,
                Combiner
            );
        false ->
            Iterator = maps:iterator(Map1),
            merge_with_1(
                maps:next(Iterator),
                Map2,
                Map1,
                fun(K, V1, V2) -> Combiner(K, V2, V1) end
            )
    end;
merge_with(Combiner, Map1, Map2) ->
    error_with_info(
        error_type_merge_intersect(Map1, Map2, Combiner),
        [Combiner, Map1, Map2]
    ).

merge_with_1({K, V2, Iterator}, Map1, Map2, Combiner) ->
    case Map1 of
        #{K := V1} ->
            NewMap1 = Map1#{K := Combiner(K, V1, V2)},
            merge_with_1(maps:next(Iterator), NewMap1, Map2, Combiner);
        #{} ->
            merge_with_1(maps:next(Iterator), maps:put(K, V2, Map1), Map2, Combiner)
    end;
merge_with_1(none, Result, _, _) ->
    Result.

foreach(Fun, MapOrIter) when is_function(Fun, 2) ->
    Iter =
        if
            is_map(MapOrIter) -> maps:iterator(MapOrIter);
            true -> MapOrIter
        end,
    try maps:next(Iter) of
        Next ->
            foreach_1(Fun, Next)
    catch
        error:_ ->
            error_with_info({badmap, MapOrIter}, [Fun, MapOrIter])
    end;
foreach(Pred, Map) ->
    badarg_with_info([Pred, Map]).

foreach_1(Fun, {K, V, Iter}) ->
    Fun(K, V),
    foreach_1(Fun, maps:next(Iter));
foreach_1(_Fun, none) ->
    ok.

%% We must inline these functions so that the stacktrace points to
%% the correct function.
-compile({inline, [badarg_with_info/1, error_with_info/2]}).

badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_stdlib_errors}}]).

error_with_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => erl_stdlib_errors}}]).

error_type_two_maps(M1, M2) when is_map(M1) ->
    {badmap, M2};
error_type_two_maps(M1, _M2) ->
    {badmap, M1}.

error_type_merge_intersect(M1, M2, Combiner) when is_function(Combiner, 3) ->
    error_type_two_maps(M1, M2);
error_type_merge_intersect(_M1, _M2, _Combiner) ->
    badarg.

-endif.
