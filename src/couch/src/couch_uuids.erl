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
-vsn(3).
-behaviour(config_listener).

-export([start/0, stop/0]).
-export([new/0, random/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

-define(RELISTEN_DELAY, 5000).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

new() ->
    gen_server:call(?MODULE, create).

random() ->
    list_to_binary(couch_util:to_hex(crypto:strong_rand_bytes(16))).

init([]) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, state()}.

terminate(_Reason, _State) ->
    ok.

handle_call(create, _From, random) ->
    {reply, random(), random};
handle_call(create, _From, {utc_random, ClockSeq}) ->
    {UtcRandom, NewClockSeq} = utc_random(ClockSeq),
    {reply, UtcRandom, {utc_random, NewClockSeq}};
handle_call(create, _From, {utc_id, UtcIdSuffix, ClockSeq}) ->
    Now = os:timestamp(),
    {UtcId, NewClockSeq} = utc_suffix(UtcIdSuffix, ClockSeq, Now),
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

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_config_change("uuids", _, _, _, _) ->
    {ok, gen_server:cast(?MODULE, change)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    gen_server:cast(?MODULE, change),
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).

new_prefix() ->
    couch_util:to_hex((crypto:strong_rand_bytes(13))).

inc() ->
    crypto:rand_uniform(1, 16#ffe).

state() ->
    AlgoStr = config:get("uuids", "algorithm", "random"),
    case couch_util:to_existing_atom(AlgoStr) of
        random ->
            random;
        utc_random ->
            ClockSeq = micros_since_epoch(os:timestamp()),
            {utc_random, ClockSeq};
        utc_id ->
            ClockSeq = micros_since_epoch(os:timestamp()),
            UtcIdSuffix = config:get("uuids", "utc_id_suffix", ""),
            {utc_id, UtcIdSuffix, ClockSeq};
        sequential ->
            {sequential, new_prefix(), inc()};
        Unknown ->
            throw({unknown_uuid_algorithm, Unknown})
    end.

micros_since_epoch({_, _, Micro} = Now) ->
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    (Nowsecs - Then) * 1000000 + Micro.

utc_random(ClockSeq) ->
    Suffix = couch_util:to_hex(crypto:strong_rand_bytes(9)),
    utc_suffix(Suffix, ClockSeq, os:timestamp()).

utc_suffix(Suffix, ClockSeq, Now) ->
    OsMicros = micros_since_epoch(Now),
    NewClockSeq = if
        OsMicros =< ClockSeq ->
            % Timestamp is lagging, use ClockSeq as Timestamp
            ClockSeq + 1;
        OsMicros > ClockSeq ->
            % Timestamp advanced, use it, and reset ClockSeq with it
            OsMicros
    end,
    Prefix = io_lib:format("~14.16.0b", [NewClockSeq]),
    {list_to_binary(Prefix ++ Suffix), NewClockSeq}.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


utc_id_time_does_not_advance_test() ->
    % Timestamp didn't advance but local clock sequence should and new UUIds
    % should be generated
    Now = {0, 1, 2},
    ClockSeq0 = micros_since_epoch({3, 4, 5}),
    {UtcId0, ClockSeq1} = utc_suffix("", ClockSeq0, Now),
    ?assert(is_binary(UtcId0)),
    ?assertEqual(ClockSeq0 + 1, ClockSeq1),
    {UtcId1, ClockSeq2} = utc_suffix("", ClockSeq1, Now),
    ?assertNotEqual(UtcId0, UtcId1),
    ?assertEqual(ClockSeq1 + 1, ClockSeq2).


utc_id_time_advanced_test() ->
    % Timestamp advanced, a new UUID generated and also the last clock sequence
    % is updated to that timestamp.
    Now0 = {0, 1, 2},
    ClockSeq0 = micros_since_epoch({3, 4, 5}),
    {UtcId0, ClockSeq1} = utc_suffix("", ClockSeq0, Now0),
    ?assert(is_binary(UtcId0)),
    ?assertEqual(ClockSeq0 + 1, ClockSeq1),
    Now1 = {9, 9, 9},
    {UtcId1, ClockSeq2} = utc_suffix("", ClockSeq1, Now1),
    ?assert(is_binary(UtcId1)),
    ?assertNotEqual(UtcId0, UtcId1),
    ?assertEqual(micros_since_epoch(Now1), ClockSeq2).

utc_random_test_time_does_not_advance_test() ->
    {MSec, Sec, USec} = os:timestamp(),
    Future = {MSec + 10, Sec, USec},
    ClockSeqFuture = micros_since_epoch(Future),
    {UtcRandom, NextClockSeq} = utc_random(ClockSeqFuture),
    ?assert(is_binary(UtcRandom)),
    ?assertEqual(32, byte_size(UtcRandom)),
    ?assertEqual(ClockSeqFuture + 1, NextClockSeq).

utc_random_test_time_advance_test() ->
    ClockSeqPast = micros_since_epoch({1, 1, 1}),
    {UtcRandom, NextClockSeq} = utc_random(ClockSeqPast),
    ?assert(is_binary(UtcRandom)),
    ?assertEqual(32, byte_size(UtcRandom)),
    ?assert(NextClockSeq > micros_since_epoch({1000, 0, 0})).


-endif.
