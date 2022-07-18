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

-module(mem3_reshard_validate).

-export([
    start_args/2,
    source/1,
    targets/2
]).

-include_lib("mem3/include/mem3.hrl").

-spec start_args(#shard{}, any()) -> ok | {error, term()}.
start_args(Source, Split) ->
    first_error([
        check_split(Split),
        check_range(Source, Split),
        check_node(Source),
        source(Source),
        check_shard_map(Source)
    ]).

-spec source(#shard{}) -> ok | {error, term()}.
source(#shard{name = Name}) ->
    case couch_server:exists(Name) of
        true ->
            ok;
        false ->
            {error, {source_shard_not_found, Name}}
    end.

-spec check_shard_map(#shard{}) -> ok | {error, term()}.
check_shard_map(#shard{name = Name}) ->
    DbName = mem3:dbname(Name),
    AllShards = mem3:shards(DbName),
    case mem3_util:calculate_max_n(AllShards) of
        N when is_integer(N), N >= 1 ->
            ok;
        N when is_integer(N), N < 1 ->
            {error, {not_enough_shard_copies, DbName}}
    end.

-spec targets(#shard{}, [#shard{}]) -> ok | {error, term()}.
targets(#shard{} = Source, Targets) ->
    first_error([
        target_ranges(Source, Targets)
    ]).

-spec check_split(any()) -> ok | {error, term()}.
check_split(Split) when is_integer(Split), Split > 1 ->
    ok;
check_split(Split) ->
    {error, {invalid_split_parameter, Split}}.

-spec check_range(#shard{}, any()) -> ok | {error, term()}.
check_range(#shard{range = Range = [B, E]}, Split) ->
    case (E + 1 - B) >= Split of
        true ->
            ok;
        false ->
            {error, {shard_range_cannot_be_split, Range, Split}}
    end.

-spec check_node(#shard{}) -> ok | {error, term()}.
check_node(#shard{node = undefined}) ->
    ok;
check_node(#shard{node = Node}) when Node =:= node() ->
    ok;
check_node(#shard{node = Node}) ->
    {error, {source_shard_node_is_not_current_node, Node}}.

-spec target_ranges(#shard{}, [#shard{}]) -> ok | {error, any()}.
target_ranges(#shard{range = [Begin, End]}, Targets) ->
    Ranges = [R || #shard{range = R} <- Targets],
    SortFun = fun([B1, _], [B2, _]) -> B1 =< B2 end,
    [First | RestRanges] = lists:sort(SortFun, Ranges),
    try
        TotalRange = lists:foldl(
            fun([B2, E2], [B1, E1]) ->
                case B2 =:= E1 + 1 of
                    true ->
                        ok;
                    false ->
                        throw({range_error, {B2, E1}})
                end,
                [B1, E2]
            end,
            First,
            RestRanges
        ),
        case [Begin, End] =:= TotalRange of
            true ->
                ok;
            false ->
                throw({range_error, {[Begin, End], TotalRange}})
        end
    catch
        throw:{range_error, Error} ->
            {error, {shard_range_error, Error}}
    end.

-spec first_error([ok | {error, term()}]) -> ok | {error, term()}.
first_error(Results) ->
    case [Res || Res <- Results, Res =/= ok] of
        [] ->
            ok;
        [FirstError | _] ->
            FirstError
    end.
