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
-module(couch_uuids).
-include_lib("couch/include/couch_db.hrl").

-behaviour(gen_server).
-behaviour(config_listener).

-export([start/0, stop/0]).
-export([new/0, random/0]).
-export([v7_bin/0]).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

-define(DEFAULT_ALGORITHM, "uuid_v7").
-define(RELISTEN_DELAY, 5000).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

new() ->
    % Some algorithms can bypass the gen_server
    case config_algorithm() of
        "random" -> random();
        "uuid_v4" -> v4();
        "uuid_v7" -> v7();
        _ -> gen_server:call(?MODULE, create)
    end.

random() ->
    couch_util:to_hex_bin(crypto:strong_rand_bytes(16)).

init([]) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, state()}.

handle_call(create, _From, {utc_random, ClockSeq}) ->
    {UtcRandom, NewClockSeq} = utc_random(ClockSeq),
    {reply, UtcRandom, {utc_random, NewClockSeq}};
handle_call(create, _From, {utc_id, UtcIdSuffix, ClockSeq}) ->
    OsMicros = micros_since_epoch(),
    {UtcId, NewClockSeq} = utc_suffix(UtcIdSuffix, ClockSeq, OsMicros),
    {reply, UtcId, {utc_id, UtcIdSuffix, NewClockSeq}};
handle_call(create, _From, {sequential, Pref, Seq}) ->
    Result = ?l2b(Pref ++ io_lib:format("~6.16.0b", [Seq])),
    case Seq >= 16#fff000 of
        true ->
            {reply, Result, {sequential, new_prefix(), inc()}};
        _ ->
            {reply, Result, {sequential, Pref, Seq + inc()}}
    end.

handle_cast(change, _State) ->
    {noreply, state()};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_config_change("uuids", _, _, _, _) ->
    {ok, gen_server:cast(?MODULE, change)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    gen_server:cast(?MODULE, change),
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).

%% UUID Version 7
%% https://datatracker.ietf.org/doc/html/rfc9562#name-uuid-version-7
%%
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                           unix_ts_ms                          |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |          unix_ts_ms           |  ver  |       rand_a          |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |var|                        rand_b                             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                            rand_b                             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%
%% ver = 0111 = 7
%% var = 10 = 2
%%
v7_bin() ->
    MSec = os:system_time(millisecond),
    <<RandA:12, RandB:62, _:6>> = crypto:strong_rand_bytes(10),
    <<MSec:48, 7:4, RandA:12, 2:2, RandB:62>>.

%% UUID Version 4
%% https://www.rfc-editor.org/rfc/rfc9562#name-uuid-version-4
%%
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                           random_a                            |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |          random_a             |  ver  |       random_b        |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |var|                       random_c                            |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                           random_c                            |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%
%% ver = 0100 = 4
%% var = 10 = 2
%%
v4_bin() ->
    <<A:48, B:12, C:62, _:6>> = crypto:strong_rand_bytes(16),
    <<A:48, 4:4, B:12, 2:2, C:62>>.

v7() ->
    Bin = v7_bin(),
    Format = config:get("uuids", "format", "base_16"),
    uuid_format(Bin, Format).

v4() ->
    Bin = v4_bin(),
    Format = config:get("uuids", "format", "base_16"),
    uuid_format(Bin, Format).

uuid_format(<<_:128>> = Bin, "rfc9562") ->
    rfc9562_format(Bin);
uuid_format(<<_:128>> = Bin, "base_36") ->
    encode_base36(Bin);
uuid_format(<<_:128>> = Bin, "base_16") ->
    couch_util:to_hex_bin(Bin);
uuid_format(<<_:128>>, Other) when is_list(Other) ->
    error({unsupported_uuid_format, Other}).

% Opt for a fixed width represention
% 25 == length(integer_to_list(1 bsl 128 - 1, 36)).
%
encode_base36(<<Int:128>>) ->
    String = integer_to_list(Int, 36),
    Lower = string:to_lower(String),
    iolist_to_binary(io_lib:format("~25..0s", [Lower])).

rfc9562_format(<<_:128>> = Bin) ->
    Hex = couch_util:to_hex_bin(Bin),
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = Hex,
    <<A/binary, "-", B/binary, "-", C/binary, "-", D/binary, "-", E/binary>>.

new_prefix() ->
    couch_util:to_hex((crypto:strong_rand_bytes(13))).

inc() ->
    rand:uniform(16#ffd).

state() ->
    AlgoStr = config_algorithm(),
    case couch_util:to_existing_atom(AlgoStr) of
        random ->
            random;
        utc_random ->
            ClockSeq = micros_since_epoch(),
            {utc_random, ClockSeq};
        utc_id ->
            ClockSeq = micros_since_epoch(),
            UtcIdSuffix = config:get("uuids", "utc_id_suffix", ""),
            {utc_id, UtcIdSuffix, ClockSeq};
        sequential ->
            {sequential, new_prefix(), inc()};
        uuid_v7 ->
            uuid_v7;
        uuid_v4 ->
            uuid_v4;
        Unknown ->
            throw({unknown_uuid_algorithm, Unknown})
    end.

micros_since_epoch() ->
    os:system_time(microsecond).

utc_random(ClockSeq) ->
    Suffix = couch_util:to_hex(crypto:strong_rand_bytes(9)),
    utc_suffix(Suffix, ClockSeq, micros_since_epoch()).

utc_suffix(Suffix, ClockSeq, OsMicros) when is_integer(OsMicros) ->
    NewClockSeq =
        if
            OsMicros =< ClockSeq ->
                % Timestamp is lagging, use ClockSeq as Timestamp
                ClockSeq + 1;
            OsMicros > ClockSeq ->
                % Timestamp advanced, use it, and reset ClockSeq with it
                OsMicros
        end,
    Prefix = io_lib:format("~14.16.0b", [NewClockSeq]),
    {list_to_binary(Prefix ++ Suffix), NewClockSeq}.

config_algorithm() ->
    config:get("uuids", "algorithm", ?DEFAULT_ALGORITHM).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

utc_id_time_does_not_advance_test() ->
    % Timestamp didn't advance but local clock sequence should and new UUIds
    % should be generated
    ClockSeq0 = 345,
    {UtcId0, ClockSeq1} = utc_suffix("", ClockSeq0, 12),
    ?assert(is_binary(UtcId0)),
    ?assertEqual(ClockSeq0 + 1, ClockSeq1),
    {UtcId1, ClockSeq2} = utc_suffix("", ClockSeq1, ClockSeq0),
    ?assertNotEqual(UtcId0, UtcId1),
    ?assertEqual(ClockSeq1 + 1, ClockSeq2).

utc_id_time_advanced_test() ->
    % Timestamp advanced, a new UUID generated and also the last clock sequence
    % is updated to that timestamp.
    ClockSeq0 = 345,
    {UtcId0, ClockSeq1} = utc_suffix("", ClockSeq0, 12),
    ?assert(is_binary(UtcId0)),
    ?assertEqual(ClockSeq0 + 1, ClockSeq1),
    ClockSeq2 = 999,
    {UtcId1, ClockSeq3} = utc_suffix("", ClockSeq1, ClockSeq2),
    ?assert(is_binary(UtcId1)),
    ?assertNotEqual(UtcId0, UtcId1),
    ?assertEqual(ClockSeq2, ClockSeq3).

utc_random_test_time_does_not_advance_test() ->
    OsMicros = os:system_time(microsecond),
    ClockSeqFuture = OsMicros + 10_000_000,
    {UtcRandom, NextClockSeq} = utc_random(ClockSeqFuture),
    ?assert(is_binary(UtcRandom)),
    ?assertEqual(32, byte_size(UtcRandom)),
    ?assertEqual(ClockSeqFuture + 1, NextClockSeq).

utc_random_test_time_advance_test() ->
    ClockSeqPast = 111,
    {UtcRandom, NextClockSeq} = utc_random(ClockSeqPast),
    ?assert(is_binary(UtcRandom)),
    ?assertEqual(32, byte_size(UtcRandom)),
    ?assert(NextClockSeq > 1_000_000_000).

uuid_v7_test() ->
    Bin = v7_bin(),
    ?assertEqual(36, byte_size(uuid_format(Bin, "rfc9562"))),
    ?assertEqual(32, byte_size(uuid_format(Bin, "base_16"))),
    ?assertEqual(25, byte_size(uuid_format(Bin, "base_36"))),
    ?assertError({unsupported_uuid_format, "X"}, uuid_format(Bin, "X")),
    Fun1 = fun(_, Acc) -> sets:add_element(v7_bin(), Acc) end,
    Set1 = lists:foldl(Fun1, couch_util:new_set(), lists:seq(1, 10_000)),
    ?assertEqual(10_000, sets:size(Set1)).

uuid_v4_test() ->
    Bin = v4_bin(),
    ?assertEqual(36, byte_size(uuid_format(Bin, "rfc9562"))),
    ?assertEqual(32, byte_size(uuid_format(Bin, "base_16"))),
    ?assertEqual(25, byte_size(uuid_format(Bin, "base_36"))),
    ?assertError({unsupported_uuid_format, "X"}, uuid_format(Bin, "X")),
    Fun1 = fun(_, Acc) -> sets:add_element(v7_bin(), Acc) end,
    Set1 = lists:foldl(Fun1, couch_util:new_set(), lists:seq(1, 10_000)),
    ?assertEqual(10_000, sets:size(Set1)).

-endif.
