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

-module(couch_time_seq_tests).

-ifdef(WITH_PROPER).
-include_lib("couch/include/couch_eunit_proper.hrl").
-else.
-include_lib("couch/include/couch_eunit.hrl").
-endif.

% Some bogus time in the far future to avoid reaching it and have that
% interfere with the tests
%
-define(TEST_TIME, "3000-01-01T00:00:00Z").

new_test() ->
    New = couch_time_seq:new(),
    Timestamp = couch_time_seq:timestamp(),
    TSeq1 = couch_time_seq:update(New, Timestamp, 1),
    ?assertMatch(#{v := 1, bins := [_ | _]}, couch_time_seq:new(TSeq1)).

min_time_limit_test() ->
    TSeq = couch_time_seq:new(),
    MinTime = couch_time_seq:update(TSeq, 1, 42),
    ?assertEqual(TSeq, MinTime).

update_first_test() ->
    TSeq = couch_time_seq:new(),
    TSeq1 = couch_time_seq:update(TSeq, test_time(), 1),
    ?assertNotEqual(TSeq, TSeq1),
    ?assertEqual(1, length(maps:get(bins, TSeq1))).

update_same_hour_test() ->
    TSeq = couch_time_seq:new(),
    T = test_time(),
    TSeq1 = couch_time_seq:update(TSeq, T, 1),
    TSeq2 = couch_time_seq:update(TSeq1, T, 2),
    #{bins := Bins} = TSeq2,
    ?assertMatch([{_, 1}], Bins),
    TSeq3 = couch_time_seq:update(TSeq2, T, 1),
    ?assertEqual(TSeq2, TSeq3).

stale_update_test() ->
    TSeq = couch_time_seq:new(),
    T = test_time(),
    TSeq1 = couch_time_seq:update(TSeq, T + hours(3), 42),
    TSeq2 = couch_time_seq:update(TSeq1, T, 43),
    ?assertEqual(TSeq1, TSeq2).

update_new_hour_test() ->
    TSeq = couch_time_seq:new(),
    TSeq1 = couch_time_seq:update(TSeq, test_time() + hours(3), 42),
    TSeq2 = couch_time_seq:update(TSeq1, test_time() + hours(6), 43),
    ?assertNotEqual(TSeq1, TSeq2),
    #{bins := Bins} = TSeq2,
    ?assertEqual(2, length(Bins)),
    ?assertMatch([{_, 43}, {_, 42}], Bins).

update_large_gap_test() ->
    TSeq = couch_time_seq:new(),
    T = test_time(),
    TSeq1 = couch_time_seq:update(TSeq, T + hours(1), 42),
    TSeq2 = couch_time_seq:update(TSeq1, T + years(30), 43),
    ?assertNotEqual(TSeq1, TSeq2),
    #{bins := Bins} = TSeq2,
    ?assertEqual(2, length(Bins)),
    ?assertMatch([{_, 43}, {_, 42}], Bins).

update_when_bins_are_full_test() ->
    TSeq = fill_bins(),
    #{bins := Bins} = TSeq,
    ?assertEqual(60, length(Bins)),
    #{bins := [{TopTime, TopSeq} | _]} = TSeq,
    TSeq1 = couch_time_seq:update(TSeq, TopTime + hours(1), TopSeq + 1),
    #{bins := Bins1} = TSeq1,
    ?assert(length(Bins1) =< 60),
    TSeq2 = couch_time_seq:update(TSeq1, TopTime + hours(2), TopSeq + 2),
    #{bins := Bins2} = TSeq2,
    ?assert(length(Bins2) =< 60),
    TSeq3 = couch_time_seq:update(TSeq2, TopTime + years(1), TopSeq + 3),
    #{bins := Bins3} = TSeq3,
    ?assert(length(Bins3) =< 60).

update_large_gap_when_bins_are_full_test() ->
    TSeq = fill_bins(),
    #{bins := Bins} = TSeq,
    ?assertEqual(60, length(Bins)),
    #{bins := [{TopTime, TopSeq} | _]} = TSeq,
    TSeq1 = couch_time_seq:update(TSeq, TopTime + years(999), TopSeq + 1),
    #{bins := Bins1} = TSeq1,
    {FirstTime, _} = lists:last(Bins1),
    ?assertEqual(test_time(), FirstTime),
    {LastTime, _} = hd(Bins1),
    ?assert(LastTime > test_time() + years(999)).

update_3_hours_8_times_test() ->
    TSeq = update_cnt(8, hours(3)),
    #{bins := Bins} = TSeq,
    ?assertEqual(8, length(Bins)),
    {FirstTime, _} = lists:last(Bins),
    ?assertEqual(test_time(), FirstTime),
    {LastTime, _} = hd(Bins),
    ?assertEqual(FirstTime + hours(21), LastTime),
    Hist = couch_time_seq:histogram(TSeq, 8),
    lists:foreach(fun([_T, N]) -> ?assertEqual(1, N) end, Hist).

update_twice_an_hour_for_a_day_test() ->
    TSeq = update_cnt(2 * 24, hours(1) div 2),
    #{bins := Bins} = TSeq,
    ?assertEqual(8, length(Bins)),
    {FirstTime, _} = lists:last(Bins),
    ?assertEqual(test_time(), FirstTime),
    {LastTime, _} = hd(Bins),
    ?assertEqual(FirstTime + hours(21), LastTime),
    Hist = couch_time_seq:histogram(TSeq, 2 * 24),
    lists:foreach(fun([_, N]) -> ?assertEqual(6, N) end, Hist).

update_once_an_hour_for_180_hours_test() ->
    TSeq = update_cnt(180, hours(1)),
    #{bins := Bins} = TSeq,
    ?assertEqual(60, length(Bins)),
    {FirstTime, _} = lists:last(Bins),
    ?assertEqual(test_time(), FirstTime),
    {LastTime, _} = hd(Bins),
    ?assertEqual(FirstTime + hours(177), LastTime),
    Hist = couch_time_seq:histogram(TSeq, 180),
    lists:foreach(fun([_, N]) -> ?assertEqual(3, N) end, Hist).

update_once_an_hour_for_183_hours_test() ->
    TSeq = update_cnt(183, hours(1)),
    #{bins := Bins} = TSeq,
    % We rolled up so the majority of bins were rolled up
    ?assertEqual(32, length(Bins)),
    {FirstTime, _} = lists:last(Bins),
    ?assertEqual(test_time(), FirstTime),
    {LastTime, _} = hd(Bins),
    ?assertEqual(FirstTime + hours(180), LastTime),
    Hist = couch_time_seq:histogram(TSeq, 183),
    % All the counts should be 3 through 6 only
    lists:foreach(fun([_, N]) -> ?assert(N >= 3 andalso N =< 6) end, Hist),
    [_, HistCntFirst] = hd(Hist),
    ?assertEqual(6, HistCntFirst),
    [_, HistCntLast] = lists:last(Hist),
    ?assertEqual(3, HistCntLast).

update_once_an_hour_for_100_000_hours_test() ->
    TSeq = update_cnt(100_000, 3600),
    #{bins := Bins} = TSeq,
    Hist = couch_time_seq:histogram(TSeq, 100_000),
    SumChanges = lists:sum([C || [_T, C] <- Hist]),
    ?assertEqual(100_000, SumChanges),
    {FirstTime, _} = lists:last(Bins),
    ?assertEqual(test_time(), FirstTime).

update_10_000_with_large_interval_test() ->
    Month = 3600 * 24 * 30,
    TSeq = update_cnt(10_000, Month),
    #{bins := Bins} = TSeq,
    Hist = couch_time_seq:histogram(TSeq, 10_000),
    SumChanges = lists:sum([C || [_T, C] <- Hist]),
    ?assertEqual(10_000, SumChanges),
    {FirstTime, _} = lists:last(Bins),
    ?assertEqual(test_time(), FirstTime).

since_test() ->
    T = test_time(),
    T3 = T + hours(3),
    T9 = T + hours(9),
    T15 = T + hours(15),
    TSeq0 = couch_time_seq:update(couch_time_seq:new(), T, 0),
    TSeq1 = couch_time_seq:update(TSeq0, T3, 42),
    TSeq2 = couch_time_seq:update(TSeq1, T9, 43),
    TSeq = couch_time_seq:update(TSeq2, T15, 44),

    % [{T15, 44}, {T9, 43}, {T3, 42}, {T, 0}]

    ?assertEqual(0, couch_time_seq:since(TSeq, 0)),

    ?assertEqual(0, couch_time_seq:since(TSeq, T - 1)),
    ?assertEqual(0, couch_time_seq:since(TSeq, T)),
    ?assertEqual(0, couch_time_seq:since(TSeq, T + 1)),

    ?assertEqual(0, couch_time_seq:since(TSeq, T3 - 1)),
    ?assertEqual(42, couch_time_seq:since(TSeq, T3)),
    ?assertEqual(42, couch_time_seq:since(TSeq, T3 + 1)),

    ?assertEqual(42, couch_time_seq:since(TSeq, T9 - 1)),
    ?assertEqual(43, couch_time_seq:since(TSeq, T9)),
    ?assertEqual(43, couch_time_seq:since(TSeq, T9 + 1)),

    ?assertEqual(43, couch_time_seq:since(TSeq, T15 - 1)),
    ?assertEqual(44, couch_time_seq:since(TSeq, T15)),
    ?assertEqual(44, couch_time_seq:since(TSeq, T15 + 1)),

    ?assertEqual(now, couch_time_seq:since(TSeq, T + hours(999))).

histogram_test() ->
    New = couch_time_seq:new(),
    ?assertEqual([], couch_time_seq:histogram(New, 0)),
    ?assertEqual([], couch_time_seq:histogram(New, 1)),
    T = test_time(),
    % Start with seq 42 as if we're upgrading a db which was created
    % before the time-seq feature. We don't know when those sequences were
    % created so expect them to appear in bin with Unix time = 0 (1970-01-01)
    TSeq0 = couch_time_seq:update(New, T, 42),
    ?assertEqual(
        [
            [<<"1970-01-01T00:00:00Z">>, 42],
            [<<"3000-01-01T00:00:00Z">>, 1]
        ],
        couch_time_seq:histogram(TSeq0, 43)
    ),

    TSeq = couch_time_seq:update(TSeq0, T + hours(3), 43),
    % The 1 is the jump from 42 to 43, then 3 is a jump from 43 to now (46)
    ?assertEqual(
        [
            [<<"1970-01-01T00:00:00Z">>, 42],
            [<<"3000-01-01T00:00:00Z">>, 1],
            [<<"3000-01-01T03:00:00Z">>, 3]
        ],
        couch_time_seq:histogram(TSeq, 46)
    ).

% Some test helper functions

test_time() ->
    calendar:rfc3339_to_system_time(?TEST_TIME).

hours(Hours) ->
    Hours * 3600.

years(Years) ->
    hours(1) * 24 * 356 * Years.

fill_bins() ->
    fill_bins(test_time(), 1, hours(1), couch_time_seq:new()).

fill_bins(Time, Seq, TimeInc, #{bins := Bins} = TSeq) ->
    case length(Bins) == 60 of
        true ->
            TSeq;
        false ->
            TSeq1 = couch_time_seq:update(TSeq, Time, Seq),
            Seq1 = Seq + 1,
            TimeInc1 =
                case TimeInc of
                    [From, To] -> From + rand:uniform(To - From) - 1;
                    _ -> TimeInc
                end,
            Time1 = Time + TimeInc1,
            fill_bins(Time1, Seq1, TimeInc, TSeq1)
    end.

update_cnt(N, TimeInc) ->
    update_cnt(N, test_time(), 0, TimeInc, couch_time_seq:new()).

update_cnt(0, _Time, _Seq, _TimeInc, TSeq) ->
    TSeq;
update_cnt(Cnt, Time, Seq, TimeInc, TSeq) ->
    TSeq1 = couch_time_seq:update(TSeq, Time, Seq),
    TimeInc1 =
        case TimeInc of
            [From, To] -> From + rand:uniform(To - From);
            _ -> TimeInc
        end,
    Time1 = Time + TimeInc1,
    Seq1 = Seq + 1,
    update_cnt(Cnt - 1, Time1, Seq1, TimeInc, TSeq1).

%
% Property tests
%

-ifdef(WITH_PROPER).

couch_time_property_test_() ->
    ?EUNIT_QUICKCHECK(60, 10000).
%
% Properties
%

prop_sorted_after_update() ->
    ?FORALL(
        TSeq,
        tseq_g(),
        begin
            #{bins := Bins} = TSeq,
            Bins =:= lists:reverse(lists:ukeysort(1, Bins))
        end
    ).

prop_sequences_are_non_decreasing_after_update() ->
    ?FORALL(
        TSeq,
        tseq_g(),
        begin
            #{bins := Bins} = TSeq,
            {_, Seqs} = lists:unzip(Bins),
            Seqs =:= lists:reverse(lists:sort(Seqs))
        end
    ).

prop_correct_size_after_update() ->
    ?FORALL(
        TSeq,
        tseq_g(),
        begin
            #{bins := Bins} = TSeq,
            60 >= length(Bins)
        end
    ).

prop_no_0_seq_bins_after_updates() ->
    ?FORALL(
        TSeq,
        tseq_g(),
        begin
            % Only the first bin may have a 0 sequence
            #{bins := Bins} = TSeq,
            case lists:reverse(Bins) of
                [] -> true;
                [{_, _}] -> true;
                [{_, 0} | Rest] -> [] =:= [B || {_, 0} = B <- Rest];
                [_ | _] -> [] =:= [B || {_, 0} = B <- Bins]
            end
        end
    ).

%
% Generators
%

hours_g() ->
    % 10 years worth of hours
    % -24 is to testing jumping backwards in time
    range(-24, 24 * 365 * 10).

seq_g() ->
    non_neg_integer().

tseq_g() ->
    ?LET(
        Updates,
        list({hours_g(), seq_g()}),
        lists:foldl(
            fun({Hours, Seq}, TSeq) ->
                couch_time_seq:update(TSeq, test_time() + hours(Hours), Seq)
            end,
            couch_time_seq:new(),
            Updates
        )
    ).

-endif.
