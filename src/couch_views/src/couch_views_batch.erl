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

-module(couch_views_batch).


-export([
    start/0,
    success/2,
    failure/0
]).

-export([
    start/1,
    success/3,
    failure/1
]).


-callback start(State::term()) -> {NewState::term(), BatchSize::pos_integer()}.
-callback success(
            DocsRead::non_neg_integer(),
            TxSize::non_neg_integer(),
            State::term()
        ) -> NewState::term().
-callback failure(State::term()) -> NewState::term().


-define(DEFAULT_MOD, atom_to_list(?MODULE)).


-record(batch_st, {
    start_time,
    size,
    state = search,
    search_incr,
    sense_incr,
    max_tx_size,
    max_tx_time,
    threshold_penalty
}).


start() ->
    {Mod, State} = case load_state() of
        {M, S} ->
            {M, S};
        undefined ->
            ModStr = config:get("couch_views", "batch_module", ?DEFAULT_MOD),
            ModAtom = list_to_existing_atom(ModStr),
            {ModAtom, undefined}
    end,
    {NewState, BatchSize} = Mod:start(State),
    save_state(Mod, NewState),
    BatchSize.


success(DocsRead, TxSize) ->
    {Mod, State} = load_state(),
    NewState = Mod:success(DocsRead, TxSize, State),
    save_state(Mod, NewState),
    ok.


failure() ->
    {Mod, State} = load_state(),
    NewState = Mod:failure(State),
    save_state(Mod, NewState),
    ok.


-spec start(State::term()) -> {NewState::term(), BatchSize::pos_integer()}.
start(undefined) ->
    St = #batch_st{
        size = get_config("batch_initial_size", "100"),
        search_incr = get_config("batch_search_increment", "500"),
        sense_incr = get_config("batch_sense_increment", "100"),
        max_tx_size = get_config("batch_max_tx_size", "9000000"),
        max_tx_time = get_config("batch_max_tx_time", "4500"),
        threshold_penalty = get_config("batch_threshold_penalty", "0.2")
    },
    start(validate_opts(St));

start(#batch_st{size = Size} = St) ->
    NewSt = St#batch_st{
        start_time = erlang:monotonic_time()
    },
    {NewSt, Size}.


-spec success(
        DocsRead::non_neg_integer(),
        TxSize::non_neg_integer(),
        State::term()
    ) -> NewState::term().
success(_DocsRead, TxSize, #batch_st{} = St) ->
    #batch_st{
        start_time = StartTime,
        size = Size,
        state = State,
        search_incr = SearchIncr,
        sense_incr = SenseIncr,
        max_tx_size = MaxTxSize,
        max_tx_time = MaxTxTime,
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


-spec failure(State::term()) -> NewState::term().
failure(#batch_st{} = St) ->
    St#batch_st{
        size = erlang:max(1, St#batch_st.size div 2),
        state = sense
    }.


validate_opts(St) ->
    #batch_st{
        size = Size,
        search_incr = SearchIncr,
        sense_incr = SenseIncr,
        max_tx_size = MaxTxSize,
        max_tx_time = MaxTxTime,
        threshold_penalty = Penalty
    } = St,
    St#batch_st{
        size = non_neg_integer(Size, batch_initial_size),
        search_incr = non_neg_integer(SearchIncr, batch_search_increment),
        sense_incr = non_neg_integer(SenseIncr, batch_sense_increment),
        max_tx_size = non_neg_integer(MaxTxSize, batch_max_tx_size),
        max_tx_time = non_neg_integer(MaxTxTime, batch_max_tx_time),
        threshold_penalty = float_0_to_1(Penalty, batch_threshold_penalty)
    }.


get_config(Key, Default) ->
    config:get("couch_views", Key, Default).


non_neg_integer(Str, Name) ->
    try
        Val = list_to_integer(Str),
        true = Val > 0,
        Val
    catch _:_ ->
        erlang:error({invalid_non_neg_integer, {couch_views, Name, Str}})
    end.


float_0_to_1(Str, Name) ->
    Val = try
        list_to_float(Str)
    catch error:badarg ->
        erlang:error({invalid_float, {couch_views, Name, Str}})
    end,
    if Val >= 0.0 andalso Val =< 1.0 -> Val; true ->
        erlang:error({float_out_of_range, {couch_views, Name, Str}})
    end.


load_state() ->
    get(?MODULE).


save_state(Mod, Batch) ->
    put(?MODULE, {Mod, Batch}).
