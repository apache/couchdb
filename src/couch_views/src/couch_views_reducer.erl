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


-module(couch_views_reducer).


-export([
    reduce/2,
    rereduce/3,
    rereduce_values/2,
    finalize/2
]).


-include_lib("couch/include/couch_db.hrl").


-define(SUMERROR, <<"The _sum function requires that map values be numbers, "
"arrays of numbers, or objects. Objects cannot be mixed with other "
"data structures. Objects can be arbitrarily nested, provided that the values "
"for all fields are themselves numbers, arrays of numbers, or objects.">>).


-define(STATERROR, <<"The _stats function requires that map values be numbers "
"or arrays of numbers, not '~p'">>).


-define(BUILTIN_SUM, <<"_sum", _/binary>>).
-define(BUILTIN_COUNT, <<"_count", _/binary>>).
-define(BUILTIN_STATS, <<"_stats", _/binary>>).
-define(BUILTIN_COUNT_DISTINCT, <<"_approx_count_distinct", _/binary>>).


-define(HYPER_PRECISION, 11).


reduce(?BUILTIN_SUM, Results) ->
    KVSize = ?term_size(Results),
    ReduceResults = lists:foldl(fun ({Key, Val}, Acc) ->
        case maps:is_key(Key, Acc) of
            true ->
                #{Key := Sum} = Acc,
                Sum1 = builtin_sum_rows([Val], Sum),
                Sum2 = check_sum_overflow(KVSize, ?term_size(Sum1), Sum1),
                Acc#{Key := Sum2};
            false ->
                Acc#{Key => Val}
        end
    end, #{}, Results),
    maps:to_list(ReduceResults);

reduce(?BUILTIN_COUNT, Results) ->
    ReduceResults = lists:foldl(fun ({Key, _}, Acc) ->
        case maps:is_key(Key, Acc) of
            true ->
                #{Key := Count0} = Acc,
                Count1 = builtin_sum_rows([1], Count0),
                Acc#{Key := Count1};
            false ->
                Acc#{Key => 1}
        end
    end, #{}, Results),
    maps:to_list(ReduceResults);

reduce(?BUILTIN_COUNT_DISTINCT, Results) ->
    ReduceResults = lists:foldl(fun ({Key, _Val}, Acc) ->
        EK = couch_views_encoding:encode(Key),
        Filter = case maps:is_key(Key, Acc) of
            true ->
                maps:get(Key, Acc);
            false ->
                hyper:new(?HYPER_PRECISION)
        end,
        Acc#{Key => hyper:insert(EK, Filter)}
    end, #{}, Results),
    maps:to_list(ReduceResults);

reduce(?BUILTIN_STATS, Results) ->
    ReduceResults = lists:foldl(fun ({Key, Val}, Acc) ->
        case maps:is_key(Key, Acc) of
            true ->
                #{Key := Stats} = Acc,
                ValStats = create_stats(Val),
                NewStats = aggregate_stats(Stats, ValStats),
                Acc#{Key := NewStats};
            false ->
                Acc#{Key => create_stats(Val)}
        end
    end, #{}, Results),
    maps:to_list(ReduceResults).


rereduce(_Reducer, [], _GroupLevel) ->
    no_kvs;

rereduce(_Reducer, Rows, GroupLevel) when length(Rows) == 1 ->
    {Key, Val} = hd(Rows),
    GroupKey = couch_views_util:group_level_key(Key, GroupLevel),
    [{GroupKey, Val}];

rereduce(Reducer, Rows, GroupLevel) ->
    ReReduceMap = lists:foldl(fun ({Key, Val}, Acc) ->
        GroupKey = couch_views_util:group_level_key(Key, GroupLevel),
        case maps:is_key(GroupKey, Acc) of
            true ->
                #{GroupKey := ValAcc} = Acc,
                ValAcc1 = rereduce_int(Reducer, Val, ValAcc),
                Acc#{GroupKey := ValAcc1};
            false ->
                Acc#{GroupKey => Val}
        end
    end, #{}, Rows),
    maps:to_list(ReReduceMap).


rereduce_values(Reducer, [Val1 | Vals]) ->
    lists:foldl(fun (Val, Acc) ->
        rereduce_int(Reducer, Val, Acc)
    end, Val1, Vals).


rereduce_int(?BUILTIN_SUM, Val, Acc) ->
    builtin_sum_rows([Val], Acc);

rereduce_int(?BUILTIN_COUNT, Val, Acc) ->
    builtin_sum_rows([Val], Acc);

rereduce_int(?BUILTIN_COUNT_DISTINCT, Filter, Acc) ->
    hyper:union([Acc, Filter]);

rereduce_int(?BUILTIN_STATS, Val, Acc) ->
    aggregate_stats(Val, Acc).


finalize(?BUILTIN_COUNT_DISTINCT, Reduction) ->
    true = hyper:is_hyper(Reduction),
    {ok, round(hyper:card(Reduction))};

finalize(_Reducer, Reduction) ->
    {ok, Reduction}.


builtin_sum_rows([], Acc) ->
    Acc;

builtin_sum_rows([Value | RestKVs], Acc) ->
    try sum_values(Value, Acc) of
        NewAcc ->
            builtin_sum_rows(RestKVs, NewAcc)
    catch
        throw:{builtin_reduce_error, Obj} ->
            Obj;
        throw:{invalid_value, Reason, Cause} ->
            {[{<<"error">>, <<"builtin_reduce_error">>},
                {<<"reason">>, Reason}, {<<"caused_by">>, Cause}]}
    end.


sum_values(Value, Acc) when is_number(Value), is_number(Acc) ->
    Acc + Value;

sum_values(Value, Acc) when is_list(Value), is_list(Acc) ->
    sum_arrays(Acc, Value);

sum_values(Value, Acc) when is_number(Value), is_list(Acc) ->
    sum_arrays(Acc, [Value]);

sum_values(Value, Acc) when is_list(Value), is_number(Acc) ->
    sum_arrays([Acc], Value);

sum_values({Props}, Acc) ->
    case lists:keyfind(<<"error">>, 1, Props) of
        {<<"error">>, <<"builtin_reduce_error">>} ->
            throw({builtin_reduce_error, {Props}});
        false ->
            ok
    end,
    case Acc of
        0 ->
            {Props};
        {AccProps} ->
            {sum_objects(lists:sort(Props), lists:sort(AccProps))}
    end;

sum_values(Else, _Acc) ->
    throw_sum_error(Else).


sum_objects([{K1, V1} | Rest1], [{K1, V2} | Rest2]) ->
    [{K1, sum_values(V1, V2)} | sum_objects(Rest1, Rest2)];

sum_objects([{K1, V1} | Rest1], [{K2, V2} | Rest2]) when K1 < K2 ->
    [{K1, V1} | sum_objects(Rest1, [{K2, V2} | Rest2])];

sum_objects([{K1, V1} | Rest1], [{K2, V2} | Rest2]) when K1 > K2 ->
    [{K2, V2} | sum_objects([{K1, V1} | Rest1], Rest2)];

sum_objects([], Rest) ->
    Rest;

sum_objects(Rest, []) ->
    Rest.


sum_arrays([], []) ->
    [];

sum_arrays([_ | _]=Xs, []) ->
    Xs;

sum_arrays([], [_ | _]=Ys) ->
    Ys;

sum_arrays([X | Xs], [Y | Ys]) when is_number(X), is_number(Y) ->
    [X + Y | sum_arrays(Xs, Ys)];

sum_arrays(Else, _) ->
    throw_sum_error(Else).


check_sum_overflow(InSize, OutSize, Sum) ->
    Overflowed = OutSize > 4906 andalso OutSize * 2 > InSize,
    case config:get("query_server_config", "reduce_limit", "true") of
        "true" when Overflowed ->
            Msg = log_sum_overflow(InSize, OutSize),
            {[
                {<<"error">>, <<"builtin_reduce_error">>},
                {<<"reason">>, Msg}
            ]};
        "log" when Overflowed ->
            log_sum_overflow(InSize, OutSize),
            Sum;
        _ ->
            Sum
    end.


log_sum_overflow(InSize, OutSize) ->
    Fmt = "Reduce output must shrink more rapidly: "
    "input size: ~b "
    "output size: ~b",
    Msg = iolist_to_binary(io_lib:format(Fmt, [InSize, OutSize])),
    couch_log:error(Msg, []),
    Msg.


throw_sum_error(Else) ->
    throw({invalid_value, ?SUMERROR, Else}).


create_stats(Vals) when is_list(Vals) ->
    [Val0 | Rest] = Vals,
    Acc0 = create_stats(Val0),
    lists:foldl(fun (Val, Acc) ->
        aggregate_stats(Val, Acc)
    end, Rest, Acc0);

create_stats(Val) when is_number(Val) ->
    #{
        <<"sum">> => Val,
        <<"count">> => 1,
        <<"min">> => Val,
        <<"max">> => Val,
        <<"sumsqr">> => Val * Val
    };

create_stats(Val) ->
    throw({invalid_value, iolist_to_binary(io_lib:format(?STATERROR, [Val]))}).

aggregate_stats(Stats1, Stats2) ->
    #{
        <<"sum">> := Sum1,
        <<"count">> := Count1,
        <<"min">> := Min1,
        <<"max">> := Max1,
        <<"sumsqr">> := SumSqr1
    } = Stats1,

    #{
        <<"sum">> := Sum2,
        <<"count">> := Count2,
        <<"min">> := Min2,
        <<"max">> := Max2,
        <<"sumsqr">> := SumSqr2
    } = Stats2,

    #{
        <<"sum">> => Sum1 + Sum2,
        <<"count">> => Count1 + Count2,
        <<"max">> => erlang:max(Max1, Max2),
        <<"min">> => erlang:min(Min1, Min2),
        <<"sumsqr">> => SumSqr1 + SumSqr2
    }.
