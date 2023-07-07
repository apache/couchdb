% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, eithe r express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_stats_math).

-export([
    summary/2
]).

% Stats are computed over two passes. First one, with acc1, gathers some
% basics, then the second pass, with acc2, computes additional, more complex,
% statistics.
%
-record(acc1, {
    % Non-zero bins = [{BinIndex, Counts}, ...]
    bins = [],
    n = 0,
    sum = 0,
    sum_log = 0,
    sum_inv = 0
}).

-record(acc2, {
    diff_2_sum = 0,
    diff_3_sum = 0,
    diff_4_sum = 0,
    % pXX = {Rank, PercentileValue}
    p50 = {0, 0},
    p75 = {0, 0},
    p90 = {0, 0},
    p95 = {0, 0},
    p99 = {0, 0},
    p999 = {0, 0}
}).

summary(Counter, BinCount) when is_tuple(Counter), is_integer(BinCount) ->
    calc_stats(pass1(Counter, BinCount)).

calc_stats(#acc1{n = 0}) ->
    % Can't do much with 0 items. Instead of inserting division checks for
    % 0 everywhere, just return a empty stats object
    n0_stats();
calc_stats(#acc1{n = N} = Stats) when is_integer(N), N > 0 ->
    #acc1{bins = Bins, sum = Sum, sum_log = SumLog, sum_inv = SumInv} = Stats,
    Mean = Sum / N,
    Acc2 = pass2(Bins, N, Mean),
    #acc2{
        diff_2_sum = Diff2Sum,
        diff_3_sum = Diff3Sum,
        diff_4_sum = Diff4Sum,
        p50 = {_, P50},
        p75 = {_, P75},
        p90 = {_, P90},
        p95 = {_, P95},
        p99 = {_, P99},
        p999 = {_, P999}
    } = Acc2,
    Variance = Diff2Sum / N,
    StdDev = math:sqrt(Variance),
    [
        {n, N},
        {min, couch_stats_histogram:bin_min(element(1, hd(Bins)))},
        {max, couch_stats_histogram:bin_max(element(1, lists:last(Bins)))},
        {arithmetic_mean, Mean},
        {geometric_mean, math:exp(SumLog / N)},
        {harmonic_mean, N / SumInv},
        {median, P50},
        {variance, Variance},
        {standard_deviation, StdDev},
        {skewness, skewness(N, Diff3Sum, StdDev)},
        {kurtosis, kurtosis(N, Diff4Sum, StdDev)},
        {percentile, [
            {50, P50},
            {75, P75},
            {90, P90},
            {95, P95},
            {99, P99},
            {999, P999}
        ]},
        % Emit for compatibility reasons
        {histogram, [{0, 0}]}
    ].

% Fist pass removes 0 bins and calculates some basics like sums, counts, sum
% logs. Some of these can only be used after the second pass over the data.
%
pass1(Counter, BinCount) ->
    lists:foldl(
        fun(Index, #acc1{} = Acc) ->
            case counters:get(Counter, Index) of
                Count when is_integer(Count), Count =< 0 ->
                    Acc;
                Count when is_integer(Count), Count > 0 ->
                    Val = couch_stats_histogram:bin_middle(Index),
                    Acc#acc1{
                        bins = [{Index, Count} | Acc#acc1.bins],
                        n = Acc#acc1.n + Count,
                        sum = Acc#acc1.sum + Count * Val,
                        sum_log = Acc#acc1.sum_log + Count * math:log(Val),
                        sum_inv = Acc#acc1.sum_inv + Count / Val
                    }
            end
        end,
        #acc1{},
        lists:seq(BinCount, 1, -1)
    ).

% Second statistics pass. This calculates the diff squared, cubed and 4th power
% sums. These are used later to get the 2nd, 3rd and 4th central momements to
% calcuate variance, skewness and kurtosis. In this pass we also calculate
% percentiles.
%
pass2(Bins, N, Mean) ->
    % Initialize each percentile's rank value as N * Q. During traversal each
    % corresponding rank will be decremeneted by the bin's count value until we
    % get to the bin where percentile rank < bin count value. After we
    % calculate a percentile (as a non-0 value), we ignore that percentile
    % entry and stop updating its rank.
    %
    Acc0 = #acc2{
        p50 = {N * 0.50, 0},
        p75 = {N * 0.75, 0},
        p90 = {N * 0.90, 0},
        p95 = {N * 0.95, 0},
        p99 = {N * 0.99, 0},
        p999 = {N * 0.999, 0}
    },
    lists:foldl(
        fun({Index, Count}, #acc2{} = Acc) ->
            Acc1 = percentiles(Acc, Index, Count),
            Diff = couch_stats_histogram:bin_middle(Index) - Mean,
            Diff2 = Diff * Diff,
            Diff3 = Diff2 * Diff,
            Diff4 = Diff2 * Diff2,
            Acc1#acc2{
                diff_2_sum = Acc1#acc2.diff_2_sum + Count * Diff2,
                diff_3_sum = Acc1#acc2.diff_3_sum + Count * Diff3,
                diff_4_sum = Acc1#acc2.diff_4_sum + Count * Diff4
            }
        end,
        Acc0,
        Bins
    ).

%% erlfmt-ignore
percentiles(#acc2{} = Acc1, Index, Count) ->
    Acc2 = Acc1#acc2{p50 = percentile(Acc1#acc2.p50, Index, Count)},
    Acc3 = Acc2#acc2{p75 = percentile(Acc2#acc2.p75, Index, Count)},
    Acc4 = Acc3#acc2{p90 = percentile(Acc3#acc2.p90, Index, Count)},
    Acc5 = Acc4#acc2{p95 = percentile(Acc4#acc2.p95, Index, Count)},
    Acc6 = Acc5#acc2{p99 = percentile(Acc5#acc2.p99, Index, Count)},
    Acc7 = Acc6#acc2{p999 = percentile(Acc6#acc2.p999, Index, Count)},
    Acc7.

percentile({Rank, 0}, Index, Count) when Rank < Count ->
    Min = couch_stats_histogram:bin_min(Index),
    Max = couch_stats_histogram:bin_max(Index),
    Width = Max - Min,
    % Count should not be 0, we already filtered out Count == 0 bins
    Frac = Rank / Count,
    % Do a bit of extra work to get a nicer interpolated percentile value. Frac
    % is the fractional part of the bin width based on the rank left-over. For
    % example, if the Count = 1000:
    %
    %   If Rank = 1, then Frac = 0.001 : We're closer to bin min
    %   If Rank = 500, then Frac = 0.5 : We're closer the middle
    %   If Rank = 950, then Frac = 0.95 : We're closer to bin max
    %
    Percentile = Min + Width * Frac,
    {Rank, Percentile};
percentile({Rank, 0}, _Index, Count) ->
    % Haven't reached our bin yet, reduce this percentile's rank by Count
    % amount and keep going.
    {Rank - Count, 0};
percentile({Rank, Percentile}, _, _) when is_number(Percentile), Percentile > 0 ->
    % Nothing left to do, we already calculated this percentile value.
    {Rank, Percentile}.

skewness(N, Diff3Sum, StdDev) ->
    % https://en.wikipedia.org/wiki/Skewness
    %   Skewness = M3 / StdDev^3
    %   M3 = mean(Diff3Sum) = Diff3Sum/N. (3rd central moment)
    case math:pow(StdDev, 3) of
        StdDev3 when StdDev3 < 1.0e-12 ->
            % If StdDev is too low avoid dividing by 0, assume skewness = 0
            0;
        StdDev3 ->
            M3 = Diff3Sum / N,
            M3 / StdDev3
    end.

kurtosis(N, Diff4Sum, StdDev) ->
    % http://en.wikipedia.org/wiki/Kurtosis
    %     Kurtosis = M4 / StdDev^4 - 3
    %     M4 = mean(Diff4Sum) = Diff4Sum/4. (4th central momement)
    %
    % Normal distribution kurtosis is 3 so we subtract 3 to get excess kurtosis
    % to show how it's different from a normal distribution.
    case math:pow(StdDev, 4) of
        StdDev4 when StdDev4 < 1.0e-12 ->
            0;
        StdDev4 ->
            M4 = Diff4Sum / N,
            M4 / StdDev4 - 3
    end.

n0_stats() ->
    [
        {n, 0},
        {min, 0},
        {max, 0},
        {arithmetic_mean, 0},
        {geometric_mean, 0},
        {harmonic_mean, 0},
        {median, 0},
        {variance, 0},
        {standard_deviation, 0},
        {skewness, 0},
        {kurtosis, 0},
        {percentile, [
            {50, 0},
            {75, 0},
            {90, 0},
            {95, 0},
            {99, 0},
            {999, 0}
        ]},
        {histogram, [{0, 0}]}
    ].

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

basic_test() ->
    H = couch_stats_histogram:new(),
    Vals = [0.05, 0.9, 0.7, 0.7, 10.1, 11, 100.5, 0.10, 13.5],
    [couch_stats_histogram:update(H, V) || V <- Vals],
    Stats = couch_stats_histogram:stats(H),
    Percentiles = prop(percentile, Stats),
    ?assertEqual(length(Vals), prop(n, Stats)),
    ?assert(flim(0.05, prop(min, Stats))),
    ?assert(flim(104, prop(max, Stats))),
    ?assert(flim(15.3, prop(arithmetic_mean, Stats))),
    ?assert(flim(1.9, prop(geometric_mean, Stats))),
    ?assert(flim(0.25, prop(harmonic_mean, Stats))),
    ?assert(flim(0.9, prop(median, Stats))),
    ?assert(flim(923, prop(variance, Stats))),
    ?assert(flim(30.4, prop(standard_deviation, Stats))),
    % Values are skewed toward the left so we should have a positive skew
    % https://en.wikipedia.org/wiki/Skewness
    ?assert(flim(2.3, prop(skewness, Stats))),
    % We have more extreme tail outliers compared to a normal distribution, so
    % excess kurtosis should be > 0. In stats-speak the distribution would
    % be "leptokurtic".
    ?assert(flim(3.7, prop(kurtosis, Stats))),
    ?assert(flim(0.9, prop(50, Percentiles))),
    ?assert(flim(11.7, prop(75, Percentiles))),
    ?assert(flim(97, prop(90, Percentiles))),
    ?assert(flim(100, prop(95, Percentiles))),
    ?assert(flim(103, prop(99, Percentiles))),
    ?assert(flim(104, prop(999, Percentiles))).

min_extreme_test() ->
    % All the values in the smallest bin
    H = couch_stats_histogram:new(),
    N = 1000000,
    [couch_stats_histogram:update(H, 0) || _ <- lists:seq(1, N)],
    Stats = couch_stats_histogram:stats(H),
    Percentiles = prop(percentile, Stats),
    ?assertEqual(N, prop(n, Stats)),
    ?assert(flim(0, prop(min, Stats))),
    ?assert(flim(0, prop(max, Stats))),
    ?assert(flim(0, prop(arithmetic_mean, Stats))),
    ?assert(flim(0, prop(geometric_mean, Stats))),
    ?assert(flim(0, prop(harmonic_mean, Stats))),
    ?assert(flim(0, prop(median, Stats))),
    ?assert(flim(0, prop(variance, Stats))),
    ?assert(flim(0, prop(standard_deviation, Stats))),
    ?assert(flim(0, prop(skewness, Stats))),
    ?assert(flim(0, prop(kurtosis, Stats))),
    ?assert(flim(0, prop(50, Percentiles))),
    ?assert(flim(0, prop(75, Percentiles))),
    ?assert(flim(0, prop(90, Percentiles))),
    ?assert(flim(0, prop(95, Percentiles))),
    ?assert(flim(0, prop(99, Percentiles))),
    ?assert(flim(0, prop(999, Percentiles))).

max_extreme_test() ->
    % All the values are in the largest bin
    H = couch_stats_histogram:new(),
    N = 1000000,
    % ?BIN_COUNT in couch_stats_histogram.erl
    HighestBin = 230,
    [couch_stats_histogram:update(H, 10000000) || _ <- lists:seq(1, N)],
    Stats = couch_stats_histogram:stats(H),
    Percentiles = prop(percentile, Stats),
    ?assertEqual(N, prop(n, Stats)),
    % Min would be the lower bound of the highest bin
    BinMin = couch_stats_histogram:bin_min(HighestBin),
    ?assert(flim(BinMin, prop(min, Stats))),
    % Max would be the highest bound of the highest bin
    BinMax = couch_stats_histogram:bin_max(HighestBin),
    ?assert(flim(BinMax, prop(max, Stats))),
    BinMid = couch_stats_histogram:bin_middle(HighestBin),
    ?assert(flim(BinMid, prop(arithmetic_mean, Stats))),
    ?assert(flim(BinMid, prop(geometric_mean, Stats))),
    ?assert(flim(BinMid, prop(harmonic_mean, Stats))),
    ?assert(flim(BinMid, prop(median, Stats))),
    ?assert(flim(0, prop(variance, Stats))),
    ?assert(flim(0, prop(standard_deviation, Stats))),
    ?assert(flim(0, prop(skewness, Stats))),
    ?assert(flim(0, prop(kurtosis, Stats))),
    ?assert(flim(BinMid, prop(50, Percentiles))),
    ?assert(flim(4128767, prop(75, Percentiles))),
    ?assert(flim(4168089, prop(90, Percentiles))),
    ?assert(flim(4181196, prop(95, Percentiles))),
    ?assert(flim(4191682, prop(99, Percentiles))),
    ?assert(flim(4194041, prop(999, Percentiles))).

normal_dist_test() ->
    H = couch_stats_histogram:new(),
    rand:seed(default, {1, 2, 3}),
    N = 1000000,
    Mean = 50,
    Var = 100,
    [couch_stats_histogram:update(H, rand:normal(Mean, Var)) || _ <- lists:seq(1, N)],
    Stats = couch_stats_histogram:stats(H),
    Percentiles = prop(percentile, Stats),
    ?assertEqual(N, prop(n, Stats)),
    ?assert(flim(3.7, prop(min, Stats))),
    ?assert(flim(104, prop(max, Stats))),
    ?assert(flim(Mean, prop(arithmetic_mean, Stats))),
    ?assert(flim(49, prop(geometric_mean, Stats))),
    ?assert(flim(48, prop(harmonic_mean, Stats))),
    % Median and mean of a normal distribution should be the same
    ?assert(flim(Mean, prop(median, Stats))),
    ?assert(flim(Var, prop(variance, Stats))),
    ?assert(flim(math:sqrt(Var), prop(standard_deviation, Stats))),
    % Skewness should be close to 0 as the distribution is symmetric
    ?assert(flim(0.0, prop(skewness, Stats))),
    % Excess kurtosis should be 0. In stats-speak normal distribution is
    % "mesokurtic".
    ?assert(flim(0.0, prop(kurtosis, Stats))),
    % P50 = Median = Mean
    ?assert(flim(Mean, prop(50, Percentiles))),
    ?assert(flim(56, prop(75, Percentiles))),
    ?assert(flim(63, prop(90, Percentiles))),
    ?assert(flim(68, prop(95, Percentiles))),
    ?assert(flim(74, prop(99, Percentiles))),
    ?assert(flim(82, prop(999, Percentiles))).

uniform_dist_test() ->
    H = couch_stats_histogram:new(),
    rand:seed(default, {1, 2, 3}),
    N = 1000000,
    % rand:uniform/1 returns values in [1,N], so subtract 1 to get values closer to 0
    RandFun = fun() -> rand:uniform(10000001) / 10 - 1 end,
    [couch_stats_histogram:update(H, RandFun()) || _ <- lists:seq(1, N)],
    Stats = couch_stats_histogram:stats(H),
    Percentiles = prop(percentile, Stats),
    ?assertEqual(N, prop(n, Stats)),
    ?assert(flim(0, prop(min, Stats))),
    ?assert(flim(1040000, prop(max, Stats))),
    ?assert(flim(500000, prop(arithmetic_mean, Stats))),
    ?assert(flim(368000, prop(geometric_mean, Stats))),
    ?assert(flim(8800, prop(harmonic_mean, Stats))),
    ?assert(flim(500000, prop(median, Stats))),
    % Variance and stddev should be large for a uniform distribution
    ?assert(flim(83.0e9, prop(variance, Stats))),
    ?assert(flim(290000, prop(standard_deviation, Stats))),
    % Skewness should be close to 0 as the distribution is symmetric
    ?assert(flim(0.0, prop(skewness, Stats))),
    % Uniform distribution would be platykurtic. Excess kurtosis should be
    % negative we'd have fewer extreme outliers (at the tails) than a normal
    % distribution might have.
    ?assert(flim(-1.2, prop(kurtosis, Stats))),
    ?assert(flim(500000, prop(50, Percentiles))),
    ?assert(flim(750000, prop(75, Percentiles))),
    ?assert(flim(900000, prop(90, Percentiles))),
    ?assert(flim(950000, prop(95, Percentiles))),
    ?assert(flim(1010000, prop(99, Percentiles))),
    ?assert(flim(1040000, prop(999, Percentiles))).

prop(Prop, KVs) ->
    proplists:get_value(Prop, KVs).

% Since we can't compare float exactly we use
% a tollerance of 5% and a minimum of 0.05
%
flim(X, Y) ->
    flim(X, Y, max(0.05, abs(X * 0.05))).

flim(X, Y, Tol) ->
    abs(X - Y) < Tol.

-endif.
