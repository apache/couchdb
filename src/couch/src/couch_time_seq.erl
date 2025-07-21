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

% This module implements exponentially decaying time intervals which map to
% database update sequences. The idea is be able to quickly determine the set
% of changes which occurred in a rough time interval. The closer to the last
% update time -- the higher the accuracy. For instance, going back a few days,
% it's only possible to target individual days. Then weeks, then months, then
% going back years can target only years.
%
% An example of the shape of the data structure might be:
%
%  +---------+  +---------+  +--------+
%  | seq=986 |  | seq=891 |  | seq=19 |
%  +---------+  +---------+  +--------+
%            ^            ^           ^
%            |            |           |
%          t=42          t=40        t=37
%
% The head, on the left, points to the newest (most recently updated) time bin.
% In this example it started at t=42. The last t=37 bin is the oldest time bin.
%
% If we're looking for sequences starting before t=41, we'd pick seq=891 and
% run the changes since=891. If we're looking for a sequence starting before
% t=37, we'd start with since=0. The main idea here is that we'd rather error
% on the side of returning too many rows than not enough.
%
% The number of bins is always less than or qual to ?MAX_BIN_COUNT. During
% updates, when we're forced to move to a new bin, the bins are rolled-up to
% make some room before adding the latest update.
%
% During the roll-up, multiple old bins might end up merging into a single with
% a larger width. For example the above bins might end up in a single bin. In
% the example above the old two bins might merge and the bins now might look
% like:
%
%  +---------+  +---------+
%  | seq=986 |  | seq=891 |
%  +---------+  +---------+
%            ^            ^
%            |            |
%          t=42          t=37
%
% If we're now looking for sequences staring before t=40, we'd pick seq=37.
%
% The roll-up algorithm can be summarized as:
%
%   - As a preparatory step: tag all the bins with their size, and reverse them
%     to put them in chronological order (oldest first).
%
%   - For each interval in ?INTERVALS:
%
%     * For each bin in the list of bins before (now - next interval):
%
%        + If lengths of two adjacent bins are =< interval, merge the newer
%          one into the older one.
%
%     * If we merges any bins for the given interval, return.
%
%     * If we didn't merge any bins, continue with a longer interval
%
%     * Once we get to just one interval remaining, and didn't succeed merging
%       anything using the interval schedule in ?INTERVALS, simply merge all
%       bins the oldest part of the interval. This is guaranteed to make more
%       room.
%

-module(couch_time_seq).

-feature(maybe_expr, enable).

-export([
    new/0,
    new/1,
    check/1,
    since/2,
    update/2,
    update/3,
    histogram/1
]).

-export_type([
    time_seq/0
]).

% How bin count = 60 was picked:
%
%  - With the ?INTERVALS schedule defined below ran 1 update per hour for 1M
%    updates starting at year 3000 and ending at year 3114 and obtained:
%      * Less than 10 years of spacing between years at the start: 3000, 3009, 3018 ...
%      * Ten individual latest days: 3114-01-20 -> 3114-01-30
%      * Seven individual latest months: 3113-07 -> 3114-01
%  - Compressed term_to_binary(TSeq) = 470B bytes
%  - Uncompressed term_to_binary(TSeq) = 920B
%  - RAM flat size erts_debug:flat_size(TSeq) * erlang:system_info(wordsize) = 2KB
%
-define(MAX_BIN_COUNT, 60).

-define(H, 3600).
-define(D, ?H * 24).
-define(M, ?D * 30).
-define(Y, ?D * 365).
%% erlfmt-ignore
-define(INTERVALS, [
    ?H * 3, ?H * 6, ?H * 12,
    ?D, ?D * 10,
    ?M, ?M * 6,
    ?Y, ?Y * 2, ?Y * 4, ?Y * 8, ?Y * 16
]).

% Version number so we could upgrade the data structure in the future.
%
-define(VER, 1).

% Users can set a minimum time boundary to avoid errors with broken clocks.
% Sometimes embedded systems get booted into 1970 and then get their time from
% NTP. Updates are ignored if we can tell they are obviously wrong. Use some
% recent time as a default min time value:
%
%   1752724800 = 2025-07-17T00:00:00
%
-define(DEFAULT_MIN_TIME, 1752724800).

-type unix_time() :: pos_integer().
-type bin_size() :: pos_integer().
-type update_seq() :: non_neg_integer().
-type bin() :: {unix_time(), bin_size(), update_seq()}.
-type time_seq() :: #{v := pos_integer(), bins := [bin()]}.

-spec new() -> time_seq().
new() ->
    #{v => ?VER, bins => []}.

-spec new(time_seq()) -> time_seq().
new(#{v := ?VER, bins := Bins} = Ctx) when is_list(Bins) ->
    % Future upgrade clauses to next version could go here. When upgrading make
    % sure to add a clause to check/1 to return true for the older version as
    % well. But the upgarde itself will happen in new/1.
    Ctx.

-spec check(time_seq()) -> boolean().
check(#{v := ?VER, bins := Bins}) when is_list(Bins) ->
    % On version upgrades add a clause to return true for the old version here
    % as well. The upgrade will happen in new/1, but the check/1 function
    % should accept (return true) for old versions.
    true;
check(_) ->
    false.

-spec update(time_seq(), update_seq()) -> time_seq().
update(#{v := ?VER} = Ctx, Seq) when is_integer(Seq), Seq >= 0 ->
    update(Ctx, os:system_time(second), Seq).

-spec update(time_seq(), unix_time(), update_seq()) -> time_seq().
update(#{v := ?VER} = Ctx, Time, Seq) when is_integer(Time), is_integer(Seq), Seq >= 0 ->
    #{bins := Bins} = Ctx,
    RoundTime = round_time(Time),
    case Time >= min_time() of
        true -> Ctx#{bins := update_bins(Bins, RoundTime, Seq)};
        false -> Ctx
    end.

% Retun a highest known sequence that comes before the given time. If the time
% falls on or before the oldest bin then return 0. This might be a sequence to
% use with a regular _changes?since=... call
%
-spec since(time_seq(), unix_time()) -> update_seq().
since(#{v := ?VER} = Ctx, Time) when is_integer(Time) ->
    #{bins := Bins} = Ctx,
    case lists:dropwhile(fun({T, _}) -> Time =< T end, Bins) of
        [] -> 0;
        [{_, Seq} | _] -> Seq
    end.

% Return a histogram of formatted time and number of sequence updates which
% happened during that interval. The result might look like:
%
%   [["2025-01-02T03:04:00Z", 42], ["2025-01-02T01:01:00Z", 1], ...]
%
% The nested list format is so it can be emitted as json
%
-spec histogram(time_seq()) -> [[binary() | update_seq()]].
histogram(#{v := ?VER, bins := Bins}) ->
    [[time_fmt(T), Seq] || {T, Seq} <- seq_histogram(Bins)].

%
% Private functions
%

update_bins(Bins, _Time, _Seq = 0) ->
    % Ignore sequence 0 updates
    Bins;
update_bins([], Time, Seq) ->
    % First update, must be non-0 sequence
    [{Time, Seq}];
update_bins([{TopT, TopSeq} | Rest], Time, Seq) when Time =:= TopT ->
    % Update current bin. Bump up the sequence if it increased.
    [{TopT, max(Seq, TopSeq)} | Rest];
update_bins([{TopT, _} | _] = Bins, Time, _) when Time < TopT ->
    % The bins are already at a later time, ignore.
    Bins;
update_bins([{TopT, TopSeq} | _] = Bins, Time, Seq) when Time > TopT ->
    % We're into another hour create new bin possibly roll up older ones
    [{Time, max(Seq, TopSeq)} | rollup(Time, Bins)].

rollup(_, Bins) when is_list(Bins), length(Bins) < ?MAX_BIN_COUNT ->
    Bins;
rollup(TimeNow, Bins) ->
    ReversedBins = add_lengths(TimeNow, Bins),
    RolledUpBins = do_rollup(ReversedBins, TimeNow, ?INTERVALS),
    % Remove the lengths and put them in the original order
    lists:reverse([{T, S} || {T, _L, S} <- RolledUpBins]).

do_rollup([_ | _] = Bins, _TimeNow, [_] = _Intervals) ->
    % We're down to the last, largest interval. If the nicer binning
    % didn't succeed just resample the oldest half
    {Before, After} = lists:split(length(Bins) div 2, Bins),
    % Interval = max(bin lengths) means we'll always succeed rolling up
    {_, IntervalLengths, _} = lists:unzip3(Before),
    Interval = lists:max(IntervalLengths),
    RolledUpBefore = merge(Before, Interval),
    RolledUpBefore ++ After;
do_rollup([_ | _] = Bins, TimeNow, [Interval, NextInterval | RestIntervals]) ->
    % Don't roll up bins after (TimeNow - NextInterval) as we'd like to have
    % have more precision closer to the present. For instance:
    %   - When we roll-up hours: stop before (now - 3 hours)
    %   - When we roll-up up 6 months: stop before (now - 1 year)
    %
    Thresh = TimeNow - NextInterval,
    {Before, After} = lists:splitwith(fun({T, _, _}) -> T =< Thresh end, Bins),
    case merge(Before, Interval) of
        Before ->
            % No change, try the next larger interval
            do_rollup(Bins, TimeNow, [NextInterval | RestIntervals]);
        RolledUpBefore ->
            % We made some room so we return
            RolledUpBefore ++ After
    end.

% Merge adjacent two bins if they are =< interval
%
merge(Bins, Int) ->
    merge(Bins, Int, []).

merge([], _Int, Acc) ->
    lists:reverse(Acc);
merge([_] = Bins, _Int, Acc) ->
    lists:reverse(Acc) ++ Bins;
merge([{T1, L1, S1}, {_, L2, S2} | Rest], Int, Acc) when L1 =< Int, L2 =< Int ->
    Merged = {T1, L1 + L2, max(S1, S2)},
    merge(Rest, Int, [Merged | Acc]);
merge([Bin | Rest], Int, Acc) ->
    merge(Rest, Int, [Bin | Acc]).

% Assign lengths to bins and put them in chronological order
%
add_lengths(TNow, [{T0, S0} | Rest]) ->
    lists:foldl(
        fun({T, S}, [{TPrev, _, _} | _] = Acc) ->
            [{T, TPrev - T, S} | Acc]
        end,
        [{T0, TNow - T0, S0}],
        Rest
    ).

seq_histogram(Bins) ->
    seq_histogram(Bins, []).

seq_histogram([], Acc) ->
    Acc;
seq_histogram([{T, S}], Acc) ->
    seq_histogram([], [{T, S} | Acc]);
seq_histogram([{T1, S1}, {T2, S2} | Rest], Acc) ->
    seq_histogram([{T2, S2} | Rest], [{T1, S1 - S2} | Acc]).

% Round times used for updates by the size of the first interval
%
round_time(T) when is_integer(T), T > 0 ->
    RoundBy = hd(?INTERVALS),
    (T div RoundBy) * RoundBy.

min_time() ->
    config:get_integer("couchdb", "time_seq_min_time", ?DEFAULT_MIN_TIME).

% Format time as YYYY-MM-DDTHH:MM:SSZ, it's accepted by RFC 3339 and an ISO 8601
% standandards.
%
time_fmt(Time) when is_integer(Time) ->
    list_to_binary(calendar:system_time_to_rfc3339(Time, [{offset, "Z"}])).
