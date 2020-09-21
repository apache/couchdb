% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_views_batch_impl).

-behavior(couch_views_batch).


-export([
    start/2,
    success/3,
    failure/2
]).


-include_lib("couch_mrview/include/couch_mrview.hrl").


-record(batch_st, {
    start_time,
    state,
    size,
    search_incr,
    sense_incr,
    max_tx_size_bytes,
    max_tx_time_msec,
    threshold_penalty
}).


-spec start(
        Mrst::#mrst{},
        State::term()
    ) -> {NewState::term(), BatchSize::pos_integer()}.
start(Mrst, undefined) ->
    St = #batch_st{
        state = search,
        size = get_config(batch_initial_size, "100"),
        search_incr = get_config(batch_search_increment, "500"),
        sense_incr = get_config(batch_sense_increment, "100"),
        max_tx_size_bytes = get_config(batch_max_tx_size_bytes, "9000000"),
        max_tx_time_msec = get_config(batch_max_tx_time_msec, "4500"),
        threshold_penalty = get_config(
                batch_threshold_penalty,
                "0.2",
                fun float_0_to_1/2
            )
    },
    start(Mrst, St);

start(_Mrst, #batch_st{size = Size} = St) ->
    NewSt = St#batch_st{
        start_time = erlang:monotonic_time()
    },
    {NewSt, Size}.


-spec success(
        Mrst::#mrst{},
        UpdateStats::couch_views_batch:update_stats(),
        State::term()
    ) -> NewState::term().
success(_Mrst, #{tx_size := TxSize}, #batch_st{} = St) ->
    #batch_st{
        start_time = StartTime,
        size = Size,
        state = State,
        search_incr = SearchIncr,
        sense_incr = SenseIncr,
        max_tx_size_bytes = MaxTxSize,
        max_tx_time_msec = MaxTxTime,
        threshold_penalty = ThresholdPenalty
    } = St,

    TxTimeNative = erlang:monotonic_time() - StartTime,
    TxTime = erlang:convert_time_unit(TxTimeNative, native, millisecond),

    {NewSize, NewState} = case TxSize > MaxTxSize orelse TxTime > MaxTxTime of
        true ->
            {round(Size * (1.0 - ThresholdPenalty)), sense};
        false when State == search ->
            {Size + SearchIncr, State};
        false when State == sense ->
            {Size + SenseIncr, State}
    end,

    St#batch_st{
        size = erlang:max(1, NewSize),
        state = NewState
    }.


-spec failure(Mrst::#mrst{}, State::term()) -> NewState::term().
failure(_Mrst, #batch_st{} = St) ->
    St#batch_st{
        size = erlang:max(1, St#batch_st.size div 2),
        state = sense
    }.


get_config(Key, Default) ->
    get_config(Key, Default, fun non_neg_integer/2).


get_config(Key, Default, Validator) ->
    StrVal = config:get("couch_views", atom_to_list(Key), Default),
    Validator(Key, StrVal).


non_neg_integer(Name, Str) ->
    try
        Val = list_to_integer(Str),
        true = Val > 0,
        Val
    catch _:_ ->
        erlang:error({invalid_non_neg_integer, {couch_views, Name, Str}})
    end.


float_0_to_1(Name, Str) ->
    Val = try
        list_to_float(Str)
    catch error:badarg ->
        erlang:error({invalid_float, {couch_views, Name, Str}})
    end,
    if Val >= 0.0 andalso Val =< 1.0 -> Val; true ->
        erlang:error({float_out_of_range, {couch_views, Name, Str}})
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


good_config_test() ->
    with_good_config(fun() ->
        {St, 1} = start(#mrst{}, undefined),
        ?assertMatch(
            #batch_st{
                state = search,
                size = 1,
                search_incr = 2,
                sense_incr = 3,
                max_tx_size_bytes = 4,
                max_tx_time_msec = 5,
                threshold_penalty = 0.6
            },
            St
        )
    end).


bad_config_test() ->
    Fields = [
        {batch_initial_size, invalid_non_neg_integer},
        {batch_search_increment, invalid_non_neg_integer},
        {batch_sense_increment, invalid_non_neg_integer},
        {batch_max_tx_size_bytes, invalid_non_neg_integer},
        {batch_max_tx_time_msec, invalid_non_neg_integer},
        {batch_threshold_penalty, invalid_float}
    ],
    lists:foreach(fun({Field, Error}) ->
        with_bad_config(atom_to_list(Field), fun() ->
            ?assertError(
                {Error, {couch_views, Field, _}},
                start(#mrst{}, undefined)
            )
        end)
    end, Fields).


float_range_test() ->
    with_bad_float_config("batch_threshold_penalty", fun() ->
        lists:foreach(fun(_) ->
            ?assertError(
                {float_out_of_range, {couch_views, batch_threshold_penalty, _}},
                start(#mrst{}, undefined)
            )
        end, lists:seq(1, 10))
    end).


with_good_config(Fun) ->
    meck:new(config),
    meck:expect(config, get, fun
        ("couch_views", "batch_initial_size", _) -> "1";
        ("couch_views", "batch_search_increment", _) -> "2";
        ("couch_views", "batch_sense_increment", _) -> "3";
        ("couch_views", "batch_max_tx_size_bytes", _) -> "4";
        ("couch_views", "batch_max_tx_time_msec", _) -> "5";
        ("couch_views", "batch_threshold_penalty", _) -> "0.6"
    end),
    try
        Fun()
    after
        meck:unload()
    end.


with_bad_config(FieldName, Fun) ->
    meck:new(config),
    meck:expect(config, get, fun("couch_views", Field, Default) ->
        case Field == FieldName of
            true ->
                case rand:uniform() < 0.5 of
                    true -> "foo";
                    false -> -10
                end;
            false ->
                Default
        end
    end),
    try
        Fun()
    after
        meck:unload()
    end.


with_bad_float_config(FieldName, Fun) ->
    meck:new(config),
    meck:expect(config, get, fun("couch_views", Field, Default) ->
        case Field == FieldName of
            true ->
                case rand:uniform() < 0.5 of
                    true -> "100.0";
                    false -> "-0.5"
                end;
            false ->
                Default
        end
    end),
    try
        Fun()
    after
        meck:unload()
    end.

-endif.
