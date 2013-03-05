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

-module(couch_stats_aggregator).
-behaviour(gen_server).

-export([start/0, start/1, stop/0]).
-export([all/0, all/1, get/1, get/2, get_json/1, get_json/2, collect_sample/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(aggregate, {
    description = <<"">>,
    seconds = 0,
    count = 0,
    current = null,
    sum = null,
    mean = null,
    variance = null,
    stddev = null,
    min = null,
    max = null,
    samples = []
}).


start() ->
    PrivDir = couch_util:priv_dir(),
    start(filename:join(PrivDir, "stat_descriptions.cfg")).
    
start(FileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [FileName], []).

stop() ->
    gen_server:cast(?MODULE, stop).

all() ->
    ?MODULE:all(0).
all(Time) when is_binary(Time) ->
    ?MODULE:all(list_to_integer(binary_to_list(Time)));
all(Time) when is_atom(Time) ->
    ?MODULE:all(list_to_integer(atom_to_list(Time)));
all(Time) when is_integer(Time) ->
    Aggs = ets:match(?MODULE, {{'$1', Time}, '$2'}),
    Stats = lists:map(fun([Key, Agg]) -> {Key, Agg} end, Aggs),
    case Stats of
        [] ->
            {[]};
        _ ->
            Ret = lists:foldl(fun({{Mod, Key}, Agg}, Acc) ->
                CurrKeys = case proplists:lookup(Mod, Acc) of
                    none -> [];
                    {Mod, {Keys}} -> Keys
                end,
                NewMod = {[{Key, to_json_term(Agg)} | CurrKeys]},
                [{Mod, NewMod} | proplists:delete(Mod, Acc)]
            end, [], Stats),
            {Ret}
    end.

get(Key) ->
    ?MODULE:get(Key, 0).
get(Key, Time) when is_binary(Time) ->
    ?MODULE:get(Key, list_to_integer(binary_to_list(Time)));
get(Key, Time) when is_atom(Time) ->
    ?MODULE:get(Key, list_to_integer(atom_to_list(Time)));
get(Key, Time) when is_integer(Time) ->
    case ets:lookup(?MODULE, {make_key(Key), Time}) of
        [] -> #aggregate{seconds=Time};
        [{_, Agg}] -> Agg
    end.

get_json(Key) ->
    get_json(Key, 0).
get_json(Key, Time) ->
    to_json_term(?MODULE:get(Key, Time)).

collect_sample() ->
    gen_server:call(?MODULE, collect_sample, infinity).


init(StatDescsFileName) ->
    % Create an aggregate entry for each {description, rate} pair.
    ets:new(?MODULE, [named_table, set, protected]),
    SampleStr = couch_config:get("stats", "samples", "[0]"),
    {ok, Samples} = couch_util:parse_term(SampleStr),
    {ok, Descs} = file:consult(StatDescsFileName),
    lists:foreach(fun({Sect, Key, Value}) ->
        lists:foreach(fun(Secs) ->
            Agg = #aggregate{
                description=list_to_binary(Value),
                seconds=Secs
            },
            ets:insert(?MODULE, {{{Sect, Key}, Secs}, Agg})
        end, Samples)
    end, Descs),
    
    Self = self(),
    ok = couch_config:register(
        fun("stats", _) -> exit(Self, config_change) end
    ),
    
    Rate = list_to_integer(couch_config:get("stats", "rate", "1000")),
    % TODO: Add timer_start to kernel start options.
    {ok, TRef} = timer:apply_after(Rate, ?MODULE, collect_sample, []),
    {ok, {TRef, Rate}}.
    
terminate(_Reason, {TRef, _Rate}) ->
    timer:cancel(TRef),
    ok.

handle_call(collect_sample, _, {OldTRef, SampleInterval}) ->
    timer:cancel(OldTRef),
    {ok, TRef} = timer:apply_after(SampleInterval, ?MODULE, collect_sample, []),
    % Gather new stats values to add.
    Incs = lists:map(fun({Key, Value}) ->
        {Key, {incremental, Value}}
    end, couch_stats_collector:all(incremental)),
    Abs = lists:map(fun({Key, Values}) ->
        couch_stats_collector:clear(Key),
        Values2 = case Values of
            X when is_list(X) -> X;
            Else -> [Else]
        end,
        {_, Mean} = lists:foldl(fun(Val, {Count, Curr}) ->
            {Count+1, Curr + (Val - Curr) / (Count+1)}
        end, {0, 0}, Values2),
        {Key, {absolute, Mean}}
    end, couch_stats_collector:all(absolute)),
    
    Values = Incs ++ Abs,
    Now = erlang:now(),
    lists:foreach(fun({{Key, Rate}, Agg}) ->
        NewAgg = case proplists:lookup(Key, Values) of
            none ->
                rem_values(Now, Agg);
            {Key, {Type, Value}} ->
                NewValue = new_value(Type, Value, Agg#aggregate.current),
                Agg2 = add_value(Now, NewValue, Agg),
                rem_values(Now, Agg2)
        end,
        ets:insert(?MODULE, {{Key, Rate}, NewAgg})
    end, ets:tab2list(?MODULE)),
    {reply, ok, {TRef, SampleInterval}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


new_value(incremental, Value, null) ->
    Value;
new_value(incremental, Value, Current) ->
    Value - Current;
new_value(absolute, Value, _Current) ->
    Value.

add_value(Time, Value, #aggregate{count=Count, seconds=Secs}=Agg) when Count < 1 ->
    Samples = case Secs of
        0 -> [];
        _ -> [{Time, Value}]
    end,
    Agg#aggregate{
        count=1,
        current=Value,
        sum=Value,
        mean=Value,
        variance=0.0,
        stddev=null,
        min=Value,
        max=Value,
        samples=Samples
    };
add_value(Time, Value, Agg) ->
    #aggregate{
        count=Count,
        current=Current,
        sum=Sum,
        mean=Mean,
        variance=Variance,
        samples=Samples
    } = Agg,
    
    NewCount = Count + 1,
    NewMean = Mean + (Value - Mean) / NewCount,
    NewVariance = Variance + (Value - Mean) * (Value - NewMean),
    StdDev = case NewCount > 1 of
        false -> null;
        _ -> math:sqrt(NewVariance / (NewCount - 1))
    end,
    Agg2 = Agg#aggregate{
        count=NewCount,
        current=Current + Value,
        sum=Sum + Value,
        mean=NewMean,
        variance=NewVariance,
        stddev=StdDev,
        min=lists:min([Agg#aggregate.min, Value]),
        max=lists:max([Agg#aggregate.max, Value])
    },
    case Agg2#aggregate.seconds of
        0 -> Agg2;
        _ -> Agg2#aggregate{samples=[{Time, Value} | Samples]}
    end.

rem_values(Time, Agg) ->
    Seconds = Agg#aggregate.seconds,
    Samples = Agg#aggregate.samples,
    Pred = fun({When, _Value}) ->
        timer:now_diff(Time, When) =< (Seconds * 1000000)
    end,
    {Keep, Remove} = lists:splitwith(Pred, Samples),
    Agg2 = lists:foldl(fun({_, Value}, Acc) ->
        rem_value(Value, Acc)
    end, Agg, Remove),
    Agg2#aggregate{samples=Keep}.

rem_value(_Value, #aggregate{count=Count, seconds=Secs}) when Count =< 1 ->
    #aggregate{seconds=Secs};
rem_value(Value, Agg) ->
    #aggregate{
        count=Count,
        sum=Sum,
        mean=Mean,
        variance=Variance
    } = Agg,

    OldMean = (Mean * Count - Value) / (Count - 1),
    OldVariance = Variance - (Value - OldMean) * (Value - Mean),
    OldCount = Count - 1,
    StdDev = case OldCount > 1 of
        false -> null;
        _ -> math:sqrt(clamp_value(OldVariance / (OldCount - 1)))
    end,
    Agg#aggregate{
        count=OldCount,
        sum=Sum-Value,
        mean=clamp_value(OldMean),
        variance=clamp_value(OldVariance),
        stddev=StdDev
    }.

to_json_term(Agg) ->
    {Min, Max} = case Agg#aggregate.seconds > 0 of
        false ->
            {Agg#aggregate.min, Agg#aggregate.max};
        _ ->
            case length(Agg#aggregate.samples) > 0 of
                true ->
                    Extract = fun({_Time, Value}) -> Value end,
                    Samples = lists:map(Extract, Agg#aggregate.samples),
                    {lists:min(Samples), lists:max(Samples)};
                _ ->
                    {null, null}
            end
    end,
    {[
        {description, Agg#aggregate.description},
        {current, round_value(Agg#aggregate.sum)},
        {sum, round_value(Agg#aggregate.sum)},
        {mean, round_value(Agg#aggregate.mean)},
        {stddev, round_value(Agg#aggregate.stddev)},
        {min, Min},
        {max, Max}
    ]}.

make_key({Mod, Val}) when is_integer(Val) ->
    {Mod, list_to_atom(integer_to_list(Val))};
make_key(Key) ->
    Key.

round_value(Val) when not is_number(Val) ->
    Val;
round_value(Val) when Val == 0 ->
    Val;
round_value(Val) ->
    erlang:round(Val * 1000.0) / 1000.0.

clamp_value(Val) when Val > 0.00000000000001 ->
    Val;
clamp_value(_) ->
    0.0.
