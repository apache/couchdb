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

-module(couch_scanner_util).

-export([
    new_scan_id/0,
    log/5,
    ejson_map/1,
    restart_tsec/0,
    schedule_time/5,
    load_regexes/1,
    compile_regexes/1,
    match_regexes/2,
    on_first_node/0,
    consistent_hash_nodes/1
]).

-define(SECOND, 1).
-define(MINUTE, 60 * ?SECOND).
-define(HOUR, 60 * ?MINUTE).
-define(DAY, 24 * ?HOUR).
-define(WEEK, 7 * ?DAY).
-define(MONTH, 30 * ?DAY).

new_scan_id() ->
    TSec = integer_to_binary(erlang:system_time(second)),
    Rand = string:lowercase(binary:encode_hex(crypto:strong_rand_bytes(6))),
    <<TSec/binary, "-", Rand/binary>>.

log(Level, Mod, Fmt, Args, #{} = Meta) when
    is_atom(Level), is_atom(Mod), is_list(Fmt), is_list(Args)
->
    {MFmt, MArgs} = log_format_meta(Mod, Meta),
    couch_log:Level(lists:flatten([MFmt, Fmt]), MArgs ++ Args).

ejson_map(Obj) ->
    jiffy:decode(jiffy:encode(Obj), [return_maps]).

restart_tsec() ->
    % Seconds since node started. To help run once after restart
    Native = erlang:system_info(start_time) + erlang:time_offset(),
    erlang:convert_time_unit(Native, native, second).

on_first_node() ->
    hd(mem3_util:live_nodes()) =:= node().

consistent_hash_nodes(Item) ->
    Nodes = mem3_util:live_nodes(),
    hd(mem3_util:rotate_list(Item, Nodes)) =:= node().

schedule_time(Now, Last, Restart, AfterCfg, RepeatCfg) when
    is_integer(Now), is_integer(Restart), is_integer(Last)
->
    RepeatPeriod = repeat_period(Now, Last, parse_repeat(RepeatCfg)),
    case {parse_after(AfterCfg), RepeatPeriod} of
        {undefined, undefined} when Last >= Restart ->
            % Run after restart, and already ran
            infinity;
        {undefined, undefined} when Last < Restart ->
            % Run after restart, but haven't run yet
            Now;
        {After, undefined} when is_integer(After), Last >= After ->
            % Run after an absolute timestamp, and already ran
            infinity;
        {After, undefined} when is_integer(After), Last < After ->
            % Run once, haven't run yet, schedule to run
            max(Now, After);
        {undefined, Period} ->
            % No after time, just period. Either need to wait
            % since last time it ran, or is actually ready to run
            max(Now, Last + Period);
        {After, Period} ->
            % Both after time set and a period. Wait for whichever
            % takes the longest
            lists:max([Now, After, Last + Period])
    end.

load_regexes(KVs) when is_list(KVs) ->
    lists:foldl(fun regex_fold/2, #{}, KVs).

compile_regexes(Regexes = #{}) ->
    Fun = fun(_PatId, PatVal) ->
        {ok, Regex} = re:compile(PatVal),
        Regex
    end,
    maps:map(Fun, Regexes).

match_regexes(Obj, #{} = Regexes) ->
    try
        match(Obj, Regexes)
    catch
        throw:{match, Id} ->
            {match, Id}
    end.

match(Str, #{} = Pats) when is_binary(Str) ->
    Fun = fun(PatId, PatVal) ->
        case re:run(Str, PatVal, [{capture, none}]) of
            match -> throw({match, PatId});
            nomatch -> nomatch
        end
    end,
    maps:foreach(Fun, Pats),
    nomatch;
match({Props}, #{} = Pats) when is_list(Props) ->
    match(Props, Pats);
match(#{} = Map, #{} = Pats) ->
    Fun = fun(K, V) ->
        nomatch = match(K, Pats),
        nomatch = match(V, Pats)
    end,
    maps:foreach(Fun, Map),
    nomatch;
match([], _Pats) ->
    nomatch;
match([{K, V} | Rest], #{} = Pats) ->
    nomatch = match(K, Pats),
    nomatch = match(V, Pats),
    match(Rest, Pats);
match([V | Rest], #{} = Pats) ->
    nomatch = match(V, Pats),
    match(Rest, Pats);
match(Num, _Pats) when is_number(Num) ->
    nomatch;
match(Atom, _Pats) when is_atom(Atom) ->
    nomatch.

regex_fold({K, V}, #{} = Acc) ->
    PatId = list_to_binary(K),
    PatVal = list_to_binary(V),
    try re:compile(PatVal) of
        {ok, _} -> Acc#{PatId => PatVal};
        _ -> Acc
    catch
        _Tag:_Err ->
            Acc
    end.

repeat_period(_Now, _Last, undefined) ->
    undefined;
repeat_period(Now, Last, {weekday, WeekdayNum}) ->
    {NowDate, {H, M, S}} = calendar:system_time_to_universal_time(Now, second),
    case abs(calendar:day_of_the_week(NowDate) - WeekdayNum) rem 7 of
        0 ->
            % It's today. Run only if it hasn't started yet
            DayStartUnixTSec = Now - H * 3600 + M * 60 + S,
            case Last > DayStartUnixTSec of
                true -> ?WEEK;
                false -> 0
            end;
        Days ->
            ?DAY * Days
    end;
repeat_period(_Now, _Last, Period) when is_integer(Period), Period > 0 ->
    Period.

parse_after(Time) when is_list(Time), length(Time) >= 7 ->
    case string:uppercase(Time) of
        "RESTART" ->
            undefined;
        [_, _, _, _, $-, _, _, $-, _, _] = T ->
            parse_rfc3339(T ++ "T00:00:00Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _] = T ->
            parse_rfc3339(T ++ ":00:00Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _, $:, _, _] = T ->
            parse_rfc3339(T ++ ":00Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _, $:, _, _, $:, _, _] = T ->
            parse_rfc3339(T ++ "Z");
        [_, _, _, _, $-, _, _, $-, _, _, $T, _, _, $:, _, _, $:, _, _ | _] = T ->
            parse_rfc3339(T);
        [_ | _] = T ->
            parse_unix(T)
    end;
parse_after(_) ->
    undefined.

parse_rfc3339(Time) ->
    try calendar:rfc3339_to_system_time(Time) of
        Sec when is_integer(Sec), Sec >= 0 ->
            Sec;
        Sec when is_integer(Sec) ->
            undefined
    catch
        error:_ ->
            undefined
    end.

parse_unix(Time) ->
    try list_to_integer(Time) of
        Sec when is_integer(Sec), Sec >= 0 ->
            Sec;
        Sec when is_integer(Sec) ->
            undefined
    catch
        error:badarg ->
            undefined
    end.

parse_repeat(Repeat) when is_list(Repeat) ->
    % Numbering follows https://www.erlang.org/doc/man/calendar#day_of_the_week-1
    case string:lowercase(Repeat) of
        "restart" -> undefined;
        "mon" ++ _ -> {weekday, 1};
        "tue" ++ _ -> {weekday, 2};
        "wed" ++ _ -> {weekday, 3};
        "thu" ++ _ -> {weekday, 4};
        "fri" ++ _ -> {weekday, 5};
        "sat" ++ _ -> {weekday, 6};
        "sun" ++ _ -> {weekday, 7};
        Val -> parse_non_weekday_period(Val)
    end.

parse_non_weekday_period(Period) ->
    case string:split(Period, "_") of
        [NumStr, UnitStr] ->
            Unit = parse_period_unit(UnitStr),
            % 10_hours -> ["10", "hours"] -> 10 * ?HOUR
            try {Unit, list_to_integer(NumStr)} of
                {undefined, _} ->
                    undefined;
                {_, Num} when is_integer(Num), Num > 0 ->
                    Num * Unit;
                {_, Num} when is_integer(Num) ->
                    undefined
            catch
                error:badarg ->
                    undefined
            end;
        _ ->
            undefined
    end.

parse_period_unit(Period) when is_list(Period) ->
    case Period of
        "sec" ++ _ -> ?SECOND;
        "min" ++ _ -> ?MINUTE;
        "hour" ++ _ -> ?HOUR;
        "day" ++ _ -> ?DAY;
        "week" ++ _ -> ?WEEK;
        "month" ++ _ -> ?MONTH;
        _ -> undefined
    end.

% Logging bits

log_format_meta(Mod, #{} = Meta) ->
    SId = {"s:~s ", maps:get(sid, Meta, undefined)},
    Fun = {"f:~s ", maps:get(fn, Meta, undefined)},
    Db = {"db:~s ", format_db(maps:get(db, Meta, undefined))},
    DDocId = {"ddoc:~s ", maps:get(ddoc, Meta, undefined)},
    DocId = {"doc:~s ", maps:get(doc, Meta, undefined)},
    Index = {"index:~s ", maps:get(index, Meta, undefined)},
    FmtArgs = [{"~s ", Mod}, SId, Fun, Db, DDocId, Index, DocId],
    lists:unzip([{Fmt, Arg} || {Fmt, Arg} <- FmtArgs, Arg /= undefined]).

format_db(undefined) ->
    undefined;
format_db(Db) when is_list(Db) ->
    format_db(list_to_binary(Db));
format_db(Db) when is_tuple(Db) ->
    format_db(couch_db:name(Db));
format_db(<<"shards/", _:8/binary, "-", _:8/binary, "/", Rest/binary>>) ->
    [Db, _] = binary:split(Rest, <<".">>),
    Db;
format_db(<<Db/binary>>) ->
    Db.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

parse_after_test() ->
    ?assertEqual(undefined, parse_after("restart")),
    ?assertEqual(undefined, parse_after("x")),
    ?assertEqual(0, parse_after("0000000")),
    ?assertEqual(undefined, parse_after("-0000001")),
    ?assertEqual(undefined, parse_after("foobarbaz10")),
    ?assertEqual(10000000, parse_after("10000000")),
    ?assertEqual(0, parse_after("1970-01-01T00:00:00")),
    ?assertEqual(1, parse_after("1970-01-01T00:00:01")),
    ?assertEqual(0, parse_after("1970-01-01T00:00:00Z")),
    ?assertEqual(1, parse_after("1970-01-01T00:00:01Z")),
    ?assertEqual(0, parse_after("1970-01-01T00:00")),
    ?assertEqual(0, parse_after("1970-01-01T00")),
    ?assertEqual(0, parse_after("1970-01-01")).

parse_repeat_test() ->
    ?assertEqual(undefined, parse_repeat("foo")),
    ?assertEqual(undefined, parse_repeat("ReStarT")),
    ?assertEqual({weekday, 1}, parse_repeat("mon")),
    ?assertEqual({weekday, 1}, parse_repeat("Monday")),
    ?assertEqual({weekday, 2}, parse_repeat("tuesday")),
    ?assertEqual({weekday, 3}, parse_repeat("wed")),
    ?assertEqual({weekday, 4}, parse_repeat("thurs.")),
    ?assertEqual({weekday, 5}, parse_repeat("Fri")),
    ?assertEqual({weekday, 6}, parse_repeat("sAt")),
    ?assertEqual({weekday, 7}, parse_repeat("sundays")),
    ?assertEqual(1, parse_repeat("1_sec")),
    ?assertEqual(1, parse_repeat("1_second")),
    ?assertEqual(1, parse_repeat("1_sec")),
    ?assertEqual(2, parse_repeat("2_sec")),
    ?assertEqual(3, parse_repeat("3_seconds")),
    ?assertEqual(60, parse_repeat("1_min")),
    ?assertEqual(2 * 60, parse_repeat("2_minutes")),
    ?assertEqual(60 * 60, parse_repeat("1_hour")),
    ?assertEqual(24 * 60 * 60, parse_repeat("1_day")),
    ?assertEqual(7 * 24 * 60 * 60, parse_repeat("1_week")),
    ?assertEqual(30 * 24 * 60 * 60, parse_repeat("1_month")).

repeat_period_test() ->
    %Fri, May 31, 2024 16:08:37
    Now = 1717171717,
    ?assertEqual(undefined, repeat_period(0, 42, undefined)),
    ?assertEqual(42, repeat_period(1, 2, 42)),
    ?assertEqual(0, repeat_period(Now, Now - 999999, {weekday, 5})),
    ?assertEqual(?WEEK, repeat_period(Now, Now - 1, {weekday, 5})),
    ?assertEqual(1 * ?DAY, repeat_period(Now, Now - 999999, {weekday, 6})).

regex_compile_test() ->
    KVs = [{"x", "a[d-f]"}, {"y", "**"}],
    Regexes = load_regexes(KVs),
    ?assertMatch(#{<<"x">> := <<"a[d-f]">>}, Regexes),
    Compiled = compile_regexes(Regexes),
    ?assertMatch(#{<<"x">> := Tup} when is_tuple(Tup), Compiled).

regex_match_test() ->
    KVs = [{"x", "abc"}, {"y", "^de"}, {"z", "k(l|m)$"}],
    Pats = compile_regexes(load_regexes(KVs)),
    ?assertEqual(nomatch, match_regexes(1, Pats)),
    ?assertEqual(nomatch, match_regexes(1.0, Pats)),
    ?assertEqual(nomatch, match_regexes(null, Pats)),
    ?assertEqual(nomatch, match_regexes(false, Pats)),
    ?assertEqual(nomatch, match_regexes(true, Pats)),
    ?assertEqual({match, <<"x">>}, match_regexes(<<"qabcr">>, Pats)),
    ?assertEqual({match, <<"y">>}, match_regexes(<<"dex">>, Pats)),
    ?assertEqual(nomatch, match_regexes(<<"xdef">>, Pats)),
    ?assertEqual({match, <<"z">>}, match_regexes(<<"qkl">>, Pats)),
    ?assertEqual(nomatch, match_regexes(<<"klx">>, Pats)),
    ?assertEqual({match, <<"x">>}, match_regexes([<<"g">>, <<"qabcq">>], Pats)),
    ?assertEqual({match, <<"x">>}, match_regexes({[{<<"a">>, <<"abc">>}]}, Pats)),
    ?assertEqual({match, <<"y">>}, match_regexes(#{a => <<"de">>}, Pats)).

log_format_test() ->
    ?assertEqual("mod db:x ", tlog(#{db => <<"x">>})),
    ?assertEqual("mod s:y f:z db:x ", tlog(#{db => "x", sid => y, fn => z})),
    Shard = <<"shards/80000000-ffffffff/db.1712291766">>,
    ?assertEqual("mod db:db ", tlog(#{db => Shard})).

tlog(Meta) ->
    {Fmt, Args} = log_format_meta(mod, Meta),
    lists:flatten(io_lib:format(lists:flatten(Fmt), lists:flatten(Args))).

node_picking_test_() ->
    {
        foreach,
        fun() -> test_util:start_couch([mem3]) end,
        fun(Ctx) -> test_util:stop_couch(Ctx) end,
        [
            ?TDEF_FE(t_first_node),
            ?TDEF_FE(t_consistent_hash_nodes)
        ]
    }.

t_first_node(_) ->
    ?assert(on_first_node()).

t_consistent_hash_nodes(_) ->
    ?assert(consistent_hash_nodes(<<"foo">>)).

-endif.
