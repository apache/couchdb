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

% This module implements windowed base-2 histograms using Erlang counters [1].
%
% Base-2 histograms use power of 2 exponentially increasing bin widths. This
% allows capturing a range of values from microseconds to hours with a
% relatively small number of bins. The same principle is used when encoding
% floating point numbers [2]. In fact, our histograms rely on the ease of
% constructing and mainpululating binary representations of 64 bit floats in
% Erlang to do all of its heavy lifting.
%
% As a refresher, the standard (IEEE 754) 64 bit floating point representations
% looks something like:
%
%  sign  exponent    mantissa
%  [s64] [e63...e53] [m52...m1]
%  <-1-> <---11----> <---52--->
%
%
% The simplest scheme migth be to use the exponent to select the histogram bin
% and throw away the mantissa bits. However, in that case bin sizes end up
% growing a bit too fast and we lose resolution quickly. To increase the
% resolution, use a few most significant bit from the mantissa. For example,
% use 3 more bits mantissa bits for a total of 14 bits: [e63...e53] + [m52,
% m51, m50]:
%
%  sign  exponent    mantissa
%  [s64] [e63...e53] [m52, m51, m50, m49...m1]
%  <-1-> <-----------14------------> <--49--->
%        ^^^^^^^ bin index ^^^^^^^^^
%
% With Erlang's wonderful binary matching capabilities this becomes a
% one-liner:
%
%    <<0:1, BinIndex:14, _/bitstring>> = <<Val/float>>
%
% The internal implementation is a tuple of counter:new(?BIN_COUNT)
% elements. The tuple size is determined by the time window parameter when the
% histogram is created. After a histogram object is created, its Erlang term
% structure is static so it's suitable to be stored in a persistent term [3].
% The structure looks something like:
%
%  {
%     1          = [1, 2, ..., ?BIN_COUNT]
%     2          = [1, 2, ..., ?BIN_COUNT]
%     ...
%     TimeWindow = [1, 2, ..., ?BIN_COUNT]
%  }
%
% The representation can also be regarded as a table with rows as abstract time
% units: 1 = 1st second, 2 = 2nd second, etc, and the columns as histogram
% bins. The update/3 function takes a value and a current time as parameters.
% The time is used to select which row to update, and the value picks which
% bin to increment.
%
% In practice, the time window would be used as a circular buffer. The time
% parameter to the update/3 function might be the system monotonic clock time
% and then the histogram time index is computed as `Time rem TimeWindow`. So,
% as the monotonic time is advancing forward, the histogram time index will
% loop around. This comes with a minor annoynance of having to allocate a
% larger time window to accomodate some process which cleans stale (expired)
% histogram entries, possibly with some extra buffers to ensure the currently
% updated interval and the interval ready to be cleaned would not overlap.
%
% Reading a histogram can be done via the read/3 function. The function takes a
% start time and an interval. All the histogram entries in the interval will be
% summed together, bin by bin, and returned as a new counters object. This
% functionality can be used to gather and merge multiple histogram together.
%
% To get a stats summary across a time window use the stats/3 function. Just
% like the read/3 function, it takes a start time and a time interval over
% which to summarize the data.
%
% In addition to the new/1, update/3, read/3, stats/3 there are simple
% functions which default to using WindowSize = 1. The intent is they would be
% used when a simple histogram is needed without the time window functionality.
%
% [1] https://www.erlang.org/doc/man/counters.html
% [2] https://en.wikipedia.org/wiki/IEEE_754
% [3] https://www.erlang.org/doc/man/persistent_term.html

-module(couch_stats_histogram).

-export([
    new/0,
    new/1,

    update/2,
    update/3,

    stats/1,
    stats/3,

    read/1,
    read/3,

    clear/1,
    clear/3,

    bin_min/1,
    bin_max/1,
    bin_middle/1,

    calc_bin_offset/0,
    calc_bin_count/0,
    get_bin_boundaries/0
]).

% When updating constants comment out this directive, otherwise it might
% prevent module loading with the intermediate/new constants values.
%
-on_load(check_constants/0).

-define(FLOAT_SIZE, 64).

% 11 standard float64 exponent bits + 3 more extra msb mantissa bits
%
-define(INDEX_BITS, 14).

% Some practical min and max values to be able to have a fixed number of
% histogram bins. When used for timing typical units are milliseconds, so use 0.01
% msec as the minimum, and 4M msec (over 1 hour) for maximum.
%
-define(MIN_VAL, 0.01).
-define(MAX_VAL, 4000000.0).

% These are computed from previously defined constants. Recompute them with
% cal_bin_offset() and calc_bin_count(), respectively, if any of the constants
% above change.
%
-define(BIN_OFFSET, -8129).
-define(BIN_COUNT, 230).

% Public API

new() ->
    new(1).

new(TimeWindow) when is_integer(TimeWindow), TimeWindow >= 1 ->
    list_to_tuple([counter() || _ <- lists:seq(1, TimeWindow)]).

update(Ctx, Val) ->
    update(Ctx, 1, Val).

update(Ctx, Time, Val) when is_integer(Val) ->
    update(Ctx, Time, float(Val));
update(Ctx, Time, Val) when is_integer(Time), is_float(Val) ->
    Val1 = min(max(Val, ?MIN_VAL), ?MAX_VAL),
    Counter = hist_at(Ctx, Time),
    counters:add(Counter, bin_index(Val1), 1).

read(Ctx) ->
    read(Ctx, 1, 1).

read(Ctx, Time, Ticks) when is_integer(Time), is_integer(Ticks), Ticks >= 1 ->
    Ticks1 = min(tuple_size(Ctx), Ticks),
    read_fold(Ctx, Time, Ticks1, counter()).

stats(Ctx) ->
    stats(Ctx, 1, 1).

stats(Ctx, Time, Ticks) when is_integer(Time), is_integer(Ticks), Ticks >= 1 ->
    Counter = read(Ctx, Time, Ticks),
    couch_stats_math:summary(Counter, ?BIN_COUNT).

clear(Ctx) ->
    clear(Ctx, 1, 1).

clear(Ctx, Time, Ticks) when is_integer(Time), is_integer(Ticks), Ticks >= 1 ->
    Ticks1 = min(tuple_size(Ctx), Ticks),
    clear_fold(Ctx, Time, Ticks1).

% Utility functions

% Use this to recompute ?BIN_OFFSET if ?INDEX_BITS or MIN_VAL changes.
%
calc_bin_offset() ->
    <<0:1, I:?INDEX_BITS, _/bitstring>> = <<?MIN_VAL/float>>,
    % "1" because counter indices start at 1
    1 - I.

% Use this to recompute ?BIN_COUNT if ?INDEX_BITS, MIN_VAL, or MAX_VAL changes.
%
calc_bin_count() ->
    bin_index(?MAX_VAL).

get_bin_boundaries() ->
    [{bin_min(I), bin_max(I)} || I <- lists:seq(1, ?BIN_COUNT)].

% Private functions

% Called from -on_load() directive. Verify that our constants are sane
% if some are not updated module loading will throw an error.
%
check_constants() ->
    case is_float(?MIN_VAL) of
        true -> ok;
        false -> error({min_val_is_not_a_float, ?MIN_VAL})
    end,
    case ?MIN_VAL > 0.0 of
        true -> ok;
        false -> error({min_val_must_be_positive, ?MIN_VAL})
    end,
    case calc_bin_count() of
        ?BIN_COUNT -> ok;
        OtherBinCount -> error({bin_count_stale, ?BIN_COUNT, OtherBinCount})
    end,
    case calc_bin_offset() of
        ?BIN_OFFSET -> ok;
        OtherBinOffset -> error({bin_offset_stale, ?BIN_OFFSET, OtherBinOffset})
    end.

bin_index(Val) ->
    % Select the exponent bits plus a few most significant bits from
    % mantissa. ?BIN_OFFSET shifts the index into the range starting with 1
    % so we can index counter bins (those start with 1, just like tuples).
    <<0:1, BinIndex:?INDEX_BITS, _/bitstring>> = <<Val/float>>,
    BinIndex + ?BIN_OFFSET.

bin_min(Index) when is_integer(Index), Index >= 1, Index =< ?BIN_COUNT ->
    BiasedIndex = Index - ?BIN_OFFSET,
    % 1 is the sign bit
    BinBitSize = ?FLOAT_SIZE - 1 - ?INDEX_BITS,
    % Minimum value is the one with all the rest of mantissa bits set to 0
    <<Min/float>> = <<0:1, BiasedIndex:?INDEX_BITS, 0:BinBitSize>>,
    Min.

bin_max(Index) when is_integer(Index), Index >= 1, Index =< ?BIN_COUNT ->
    BiasedIndex = Index - ?BIN_OFFSET,
    % 1 is the sign bit
    BinBitSize = ?FLOAT_SIZE - 1 - ?INDEX_BITS,
    % For Max the intuition is we first construct a next highest power of two
    % value, by shifting left BinBitSize, then subtract 1. That sets all the
    % bits to 1. (for ex.:  1 bsl 4 = 1000, 1000 - 1 = 111)
    <<Max/float>> = <<0:1, BiasedIndex:?INDEX_BITS, ((1 bsl BinBitSize) - 1):BinBitSize>>,
    Max.

bin_middle(Index) when is_integer(Index), Index >= 1, Index =< ?BIN_COUNT ->
    BiasedIndex = Index - ?BIN_OFFSET,
    % 1 is the sign bit
    BinBitSize = ?FLOAT_SIZE - 1 - ?INDEX_BITS,
    % Shift left 1 bit less than we do in bin_max, which is effectively Max/2
    <<Mid/float>> = <<0:1, BiasedIndex:?INDEX_BITS, (1 bsl (BinBitSize - 1)):BinBitSize>>,
    Mid.

read_fold(_, _, 0, Acc) ->
    Acc;
read_fold(Counters, Time, Ticks, Acc) ->
    Acc1 = merge(Acc, hist_at(Counters, Time), ?BIN_COUNT),
    read_fold(Counters, Time + 1, Ticks - 1, Acc1).

clear_fold(_, _, 0) ->
    ok;
clear_fold(Counters, Time, Ticks) ->
    reset(hist_at(Counters, Time), ?BIN_COUNT),
    clear_fold(Counters, Time + 1, Ticks - 1).

merge(A, _, 0) ->
    A;
merge(A, B, I) when is_integer(I), I > 0 ->
    counters:add(A, I, counters:get(B, I)),
    merge(A, B, I - 1).

reset(_, 0) ->
    ok;
reset(Counters, I) when is_integer(I), I > 0 ->
    counters:put(Counters, I, 0),
    reset(Counters, I - 1).

counter() ->
    counters:new(?BIN_COUNT, [write_concurrency]).

hist_at(Counters, Time) when is_tuple(Counters), is_integer(Time) ->
    % Erlang monotonic time can be negative, so add a TimeWindow to it, to make
    % it positive again. Add +1 because counter indices start with 1 but X rem
    % Y returns values betweeen 0 and Y-1.
    TimeWindow = tuple_size(Counters),
    case Time rem TimeWindow of
        Idx when Idx < 0 -> element(Idx + TimeWindow + 1, Counters);
        Idx -> element(Idx + 1, Counters)
    end.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

basics_test() ->
    H = new(),
    ?assert(is_tuple(H)),
    ?assertEqual(1, tuple_size(H)),
    ?assertEqual(2, tuple_size(new(2))),
    ?assertEqual([], bins(H)),
    ?assertMatch([{_, _} | _], stats(H)),
    ?assertEqual(0, proplists:get_value(n, stats(H))),
    ?assertEqual(0, proplists:get_value(min, stats(H))),
    ?assertEqual(0, proplists:get_value(max, stats(H))).

update_test() ->
    H = new(),
    ?assertMatch(#{size := ?BIN_COUNT}, counters:info(read(H))),
    ?assertEqual([], bins(H)),
    update(H, 10.42),
    ?assertEqual([{81, 1}], bins(H)),
    ?assertEqual(81, bin_index(10.42)),
    % Update again to see how histogram gets bumped to 2
    update(H, 10.42),
    ?assertEqual([{81, 2}], bins(H)),
    ?assertEqual(2, proplists:get_value(n, stats(H))),
    ?assert(proplists:get_value(min, stats(H)) >= 10.0),
    ?assert(proplists:get_value(max, stats(H)) =< 11.0),
    clear(H),
    ?assertEqual([], bins(H)).

update_with_small_value_test() ->
    H = new(),
    % 0 is below the minimum value
    update(H, 0),
    ?assertEqual([{1, 1}], bins(H)),
    ?assertMatch([{_, _} | _], stats(H)),
    ?assertEqual(1, proplists:get_value(n, stats(H))),
    ?assert(bin_min(1) =< proplists:get_value(min, stats(H))),
    ?assert(proplists:get_value(max, stats(H)) =< bin_max(1)),
    clear(H),
    ?assertEqual([], bins(H)).

update_negative_time_index_test() ->
    H = new(3),
    update(H, -12, 0.42),
    update(H, -11, 4.2),
    update(H, -10, 4.2001),

    ?assertEqual(44, bin_index(0.42)),
    ?assertEqual(71, bin_index(4.2)),
    ?assertEqual(71, bin_index(4.2001)),

    ?assertEqual([{44, 1}], bins(H, -12, 1)),
    ?assertEqual([{71, 1}], bins(H, -11, 1)),
    ?assertEqual([{71, 1}], bins(H, -10, 1)),

    % Combine 1st and 2nd
    ?assertEqual([{44, 1}, {71, 1}], bins(H, -12, 2)),
    % Combine 2nd and 3rd
    ?assertEqual([{71, 2}], bins(H, -11, 2)),
    % Combine all three
    ?assertEqual([{44, 1}, {71, 2}], bins(H, -12, 3)),
    % Wrap around
    ?assertEqual([{44, 1}, {71, 2}], bins(H, -10, 3)),

    % Clear last two
    clear(H, -11, 2),
    ?assertEqual([{44, 1}], bins(H, -12, 3)),

    % Clear all
    clear(H, -12, 3),
    ?assertEqual([], bins(H, -12, 3)).

update_positive_time_index_test() ->
    H = new(3),
    update(H, 1, 0.42),
    update(H, 2, 4.2),
    update(H, 3, 4.2001),

    ?assertEqual([{44, 1}], bins(H, 1, 1)),
    ?assertEqual([{71, 1}], bins(H, 2, 1)),
    ?assertEqual([{71, 1}], bins(H, 3, 1)),

    % Combine 1st and 2nd
    ?assertEqual([{44, 1}, {71, 1}], bins(H, 1, 2)),
    % Combine 2nd and 3rd
    ?assertEqual([{71, 2}], bins(H, 2, 2)),
    % Combine all three
    ?assertEqual([{44, 1}, {71, 2}], bins(H, 1, 3)),
    % Wrap around
    ?assertEqual([{44, 1}, {71, 2}], bins(H, 3, 3)),

    % Clear last two
    clear(H, 2, 2),
    ?assertEqual([{44, 1}], bins(H, 1, 3)),

    % Clear all
    clear(H, 1, 3),
    ?assertEqual([], bins(H, 1, 3)).

update_negative_and_positive_time_index_test() ->
    H = new(3),
    update(H, -1, 0.42),
    update(H, 0, 4.2),
    update(H, 1, 4.2001),

    ?assertEqual([{44, 1}], bins(H, -1, 1)),
    ?assertEqual([{71, 1}], bins(H, 0, 1)),
    ?assertEqual([{71, 1}], bins(H, 1, 1)),

    % Combine 1st and 2nd
    ?assertEqual([{44, 1}, {71, 1}], bins(H, -1, 2)),
    % Combine 2nd and 3rd
    ?assertEqual([{71, 2}], bins(H, 0, 2)),
    % Combine all three
    ?assertEqual([{44, 1}, {71, 2}], bins(H, -1, 3)),
    % Wrap around
    ?assertEqual([{44, 1}, {71, 2}], bins(H, 1, 3)),

    % Clear all
    clear(H, -1, 3),
    ?assertEqual([], bins(H, -1, 3)).

update_with_large_value_test() ->
    H = new(),
    % Update with value > max
    [update(H, 1.0e300) || _ <- lists:seq(1, 1000)],
    ?assertEqual([{?BIN_COUNT, 1000}], bins(H)),
    clear(H),
    ?assertEqual([], bins(H)).

calculated_constants_test() ->
    ?assertEqual(?BIN_OFFSET, calc_bin_offset()),
    ?assertEqual(?BIN_COUNT, calc_bin_count()).

get_bin_boundaries_test() ->
    Boundaries = get_bin_boundaries(),
    ?assertEqual(?BIN_COUNT, length(Boundaries)),
    lists:foreach(
        fun({Min, Max}) ->
            ?assert(Min < Max)
        end,
        Boundaries
    ).

% Test utility functions

bins(H) ->
    bins(H, 1, 1).

bins(H, Time, Ticks) ->
    Counters = read(H, Time, Ticks),
    lists:foldl(
        fun(I, Acc) ->
            case counters:get(Counters, I) of
                C when C > 0 -> [{I, C} | Acc];
                _ -> Acc
            end
        end,
        [],
        lists:seq(?BIN_COUNT, 1, -1)
    ).

-endif.
