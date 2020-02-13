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

-module(couch_rate_limiter).

%% This module implements an algorithm to control the consumption rate
%% parameters such as:
%%  - batch size
%%  - delay between batches
%% The components of the algorithm use
%%  - [ascending minima algorithm](http://web.archive.org/web/20120805114719/http://home.tiac.net/~cri/2001/slidingmin.html)
%%  - "Welford's method" of calculating average

-export([
    new/2,
    from_map/2,
    budget/2,
    delay/2,
    wait/2,
    in/3,
    success/3,
    failure/2,
    is_congestion/2,
    min_latency/2,
    format/2,
    to_map/2
]).

-type msec() :: non_neg_integer().

-define(STATE, ?MODULE).

%% This is the number below which the math would not work due to round errors
%% In particular the default values for thresholds would be equal
-define(MIN_TARGET, 36).

-define(record_to_keyval(Name, Record),
    lists:zip(record_info(fields, Name),
        tl(tuple_to_list(Record)))).

-define(map_to_record(RecordName, Map),
    element(1, lists:foldl(fun(Field, {Record, Idx}) ->
        {setelement(Idx, Record, maps:get(Field, Map, element(Idx, Record))), Idx + 1}
    end, {#RecordName{}, 2}, record_info(fields, RecordName)))).


-define(record_to_map(RecordName, Record),
    element(1, lists:foldl(fun(Field, {Map, Idx}) ->
        {
            maps:put(Field, element(Idx, Record), Map),
            Idx + 1
        }
    end, {#{}, 2}, record_info(fields, RecordName)))).

-record(?STATE, {
    window_size = 0 :: 0 | pos_integer(),
    timer = fun now_msec/0,
    size = 1 :: pos_integer(),
    epoch = 1 :: pos_integer(),
    minimums :: queue:queue() | undefined,
    start_ts = undefined,
    mean_reads = 0.0,
    mean_writes = 0.0,
    reads = 0,
    writes = 0,
    target = 4500,
    underload_threshold = 4275, %% target * 0.95
    overload_threshold = 4725,  %% target * 1.05
    delay_threshold = 4950, %% target * 1.10
    multiplicative_factor = 0.7,
    regular_delay = 100 :: timeout(),
    congested_delay = 5000 :: timeout(),
    initial_budget = 100,
    latency = 0
}).

-type state() :: #?STATE{}.


-spec new(couch_rate:id(), Opts :: map()) -> state().

new(_Id, #{sensitivity := S}) when S =< 0 ->
    error("expected SensitivityTimeWindow > 0");

new(_Id, #{target  := T}) when T < ?MIN_TARGET ->
    error("the target is too small");

new(_Id, #{budget := B, target  := T, window := W, sensitivity := S} = Opts) ->
    WinSize = W div S + 1,
    validate_arguments(?map_to_record(?STATE, maps:merge(#{
        minimums => queue:new(),
        window_size => WinSize,
        initial_budget => B,
        underload_threshold => round(T * 0.95),
        overload_threshold => round(T * 1.05),
        delay_threshold => round(T * 1.07)
    }, maps:without([budget, window, sensitivity], Opts)))).


-spec from_map(couch_rate:id(), map()) -> state().

from_map(_Id, Map) ->
    ?map_to_record(?STATE, Map).


-spec budget(couch_rate:id(), state()) ->
        {pos_integer(), state()}.

budget(Id, #?STATE{} = State) ->
    #?STATE{
        reads = R,
        writes = W,
        mean_writes = MW,
        mean_reads = MR,
        multiplicative_factor = MultiplicativeFactor,
        target = Target,
        initial_budget = InitialBudget,
        latency = Latency
    } = State,
    case pattern(Id, State) of
        optimal ->
            {max(1, round(MR)), State};
        failed ->
            %% decrease budget
            {max(1, round(R * MultiplicativeFactor)), State};
        overloaded ->
            %% decrease budget
            {max(1, round(R * MultiplicativeFactor)), State};
        underloaded ->
            ReadWriteRatio = min(1, MR / max(1, MW)),
            SingleWrite = Latency / W,
            EstimatedWrites = floor(Target / SingleWrite),
            {max(1, round(ReadWriteRatio * EstimatedWrites)), State};
        init ->
            {InitialBudget, State}
    end.

-spec delay(couch_rate:id(), state()) ->
        {pos_integer(), state()}.

delay(Id, #?STATE{} = State) ->
    #?STATE{
        regular_delay = RD,
        congested_delay = CD
    } = State,
    case pattern(Id, State) of
        failed ->
            {CD, State};
        _ ->
            {RD, State}
    end.


-spec wait(couch_rate:id(), state()) ->
    ok.

wait(Id, State) ->
    {Delay, _} = delay(Id, State),
    timer:sleep(Delay).


-spec in(couch_rate:id(), state(), Reads :: pos_integer()) ->
        {ok, state()}.

in(_Id, #?STATE{timer = TimerFun} = State, Reads) ->
    {ok, State#?STATE{
        reads = Reads,
        start_ts = TimerFun()
    }}.


-spec success(couch_rate:id(), state(), Writes :: pos_integer()) ->
        {ok, state()}.

success(_Id, #?STATE{start_ts = undefined} = State, _Writes) ->
    {ok, State};

success(_Id, #?STATE{} = State, Writes) ->
    #?STATE{
        start_ts = TS,
        timer = TimerFun,
        reads = Reads,
        mean_reads = MeanReads,
        mean_writes = MeanWrites,
        window_size = WinSize
    } = State,
    {ok, update_min(State#?STATE{
        writes = Writes,
        mean_writes = average(MeanWrites, WinSize, Writes),
        mean_reads = average(MeanReads, WinSize, Reads),
        latency = TimerFun() - TS
    })}.


-spec failure(couch_rate:id(), state()) -> {ok, state()}.

failure(_Id, #?STATE{start_ts = undefined} = State) ->
    {ok, State};

failure(_Id, #?STATE{} = State) ->
    #?STATE{
        timer = TimerFun,
        start_ts = TS
    } = State,
    {ok, update_min(State#?STATE{
        writes = 0,
        latency = TimerFun() - TS
    })}.


-spec is_congestion(couch_rate:id(), state()) -> boolean().

is_congestion(Id, #?STATE{} = State) ->
    case pattern(Id, State) of
        overloaded -> true;
        failed -> true;
        _ -> false
    end.


-spec format(couch_rate:id(), state()) -> [{Key :: atom(), Value :: term()}].

format(_Id, #?STATE{minimums = M} = State) ->
    Map = ?record_to_map(?STATE, State),
    Minimums = lists:map(fun({D, V}) ->
        [{value, V}, {death, D}]
    end, queue:to_list(M)),
    maps:to_list(maps:merge(Map, #{
        minimums => Minimums
    })).


-spec to_map(couch_rate:id(), state()) -> map().

to_map(_Id, #?STATE{} = State) ->
    ?record_to_map(?STATE, State).


-spec update_min(state()) -> state().

update_min(#?STATE{latency = ProcessingDelay} = Q0) ->
    Q1 = remove_greater_than(Q0, ProcessingDelay),
    Q2 = append(Q1, ProcessingDelay),
    maybe_remove_first(Q2).


-spec pattern(couch_rate:id(), state()) ->
        init
        | underloaded
        | overloaded
        | optimal
        | failed.

pattern(Id, #?STATE{} = State) ->
    #?STATE{
        underload_threshold = UnderloadThreshold,
        overload_threshold = OverloadThreshold,
        writes = W,
        mean_writes = MW
    } = State,
    case min_latency(Id, State) of
        MinRollingLatency when MinRollingLatency > OverloadThreshold ->
            overloaded;
        MinRollingLatency when MinRollingLatency > UnderloadThreshold ->
            optimal;
        MinRollingLatency when MinRollingLatency > 0 andalso W == 0 ->
            failed;
        MinRollingLatency when MinRollingLatency == 0 andalso MW == 0.0 ->
            init;
        _ ->
            underloaded
    end.


-spec min_latency(couch_rate:id(), state()) -> pos_integer() | 0.

min_latency(_Id, #?STATE{size = 1}) ->
    0;

min_latency(_Id, #?STATE{minimums = Minimums}) ->
    {value, {_, Min}} = head(Minimums),
    Min.


validate_arguments(#?STATE{timer = TimerFun})
        when not is_function(TimerFun, 0) ->
    error("expected `timer` to be an arity 0 function");

validate_arguments(#?STATE{window_size = WinSize})
        when WinSize < 1 ->
    error("expected `window_size` to be greater than 1");

validate_arguments(#?STATE{initial_budget = Budget})
        when Budget < 1 ->
    error("expected `initial_budget` to be greater than 1");

validate_arguments(#?STATE{overload_threshold = OT, target = T})
        when OT =< T ->
    error("expected `overload_threshold` to be greater than `target`");

validate_arguments(#?STATE{underload_threshold = UT, target = T})
        when UT >= T ->
    error("expected `underload_threshold` to be less than `target`");

validate_arguments(#?STATE{delay_threshold = DT, overload_threshold = OT})
        when DT =< OT ->
    error("expected `delay_threshold` to be greater than `overload_threshold`");

validate_arguments(#?STATE{multiplicative_factor = MF})
        when MF < 0 orelse MF > 1 ->
    error("expected `multiplicative_factor` to be in the (0, 1) range");

validate_arguments(#?STATE{} = State) ->
    State.


-spec remove_greater_than(state(), pos_integer()) -> state().

remove_greater_than(#?STATE{minimums = Minimums, size = S} = State, Value) ->
    case tail(Minimums) of
        {value, {_, T}} when Value =< T ->
            NewState = State#?STATE{minimums = tail_drop(Minimums), size = S - 1},
            remove_greater_than(NewState, Value);
        {value, _} ->
            State;
        empty ->
            State#?STATE{epoch = 1}
    end.


-spec append(state(), pos_integer()) -> state().

append(#?STATE{minimums = Minimums, epoch = E, window_size = S} = State, Value) ->
    Death = E + S,
    State#?STATE{
        minimums = tail_put(Minimums, {Death, Value}),
        epoch = E + 1,
        size = S + 1
    }.


-spec maybe_remove_first(state()) -> state().

maybe_remove_first(#?STATE{minimums = Minimums, epoch = E, size = S} = State) ->
    case head(Minimums) of
        {value, {E, _V}} ->
            State#?STATE{minimums = head_drop(Minimums), size = S - 1};
        _ ->
            State
    end.


% Donald Knuth’s Art of Computer Programming, Vol 2, page 232, 3rd
% Welford method
average(Avg, WindowSize, Value) ->
    Delta = Value - Avg,
    Avg + Delta / WindowSize.

%% The helper functions are added because queue module
%% naming conventions are weird
head(Q) -> queue:peek_r(Q).


head_drop(Q) -> queue:drop_r(Q).

tail(Q) -> queue:peek(Q).


tail_put(Q, V) -> queue:in_r(V, Q).


tail_drop(Q) -> queue:drop(Q).


-spec now_msec() -> msec().
now_msec() ->
    {Mega, Sec, Micro} = os:timestamp(),
    ((Mega * 1000000) + Sec) * 1000 + Micro div 1000.