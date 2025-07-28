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

-module(couch_srt_query).

-feature(maybe_expr, enable).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_srt.hrl").

%% aggregate query api
-export([
    active/0,
    active/1,
    active_coordinators/0,
    active_coordinators/1,
    active_workers/0,
    active_workers/1,

    all/0,
    find_by_nonce/1,
    find_by_pid/1,
    find_by_pidref/1,
    find_workers_by_pidref/1,

    query_matcher/1,
    query_matcher/2,
    query_matcher_rows/1,
    query_matcher_rows/2,

    query/1,
    from/1,
    group_by/1,
    group_by/2,
    sort_by/1,
    sort_by/2,
    count_by/1,
    options/1,
    unlimited/0,
    with_limit/1,

    run/1,
    unsafe_run/1
]).

-export_type([
    query/0,
    query_expression/0,
    query_option/0
]).

-type aggregation_keys_fun() :: fun((Ele :: #rctx{}) -> aggregation_values() | aggregation_value()).
-type value_key_fun() :: fun((Ele :: #rctx{}) -> aggregation_values() | aggregation_value()).
-type count_key_fun() :: fun((A :: pos_integer(), B :: pos_integer()) -> pos_integer()).

-record(selector, {
    aggregation_keys = undefined ::
        rctx_field()
        | [rctx_field()]
        | undefined,
    value_key = undefined ::
        rctx_field()
        | undefined
}).

-record(unsafe_selector, {
    aggregation_keys = undefined ::
        aggregation_keys_fun()
        | rctx_field()
        | [rctx_field()]
        | undefined,
    value_key = undefined ::
        value_key_fun()
        | rctx_field()
        | undefined
}).

-record(query_options, {
    limit = undefined :: pos_integer() | unlimited | undefined,
    is_safe = undefined :: boolean() | undefined
}).

-type aggregation() :: group_by | sort_by | count_by.

-record(query, {
    matcher = undefined :: matcher_name() | all | undefined,
    selector = undefined :: #selector{} | #unsafe_selector{} | undefined,
    limit = undefined :: pos_integer() | unlimited | undefined,
    aggregation = undefined :: aggregation() | undefined,
    is_safe = true :: boolean()
}).

-record(from, {
    matcher = undefined :: matcher_name() | all | undefined,
    is_safe = undefined :: boolean() | undefined
}).

-opaque query() :: #query{}.
-opaque query_expression() ::
    #from{}
    | #query_options{}
    | #selector{}
    | #unsafe_selector{}
    | query_option()
    | {aggregation(), #selector{}}
    | {aggregation(), #unsafe_selector{}}.
-opaque query_option() ::
    {limit, pos_integer() | unlimited | undefined}.

%%
%% Aggregate query API
%%

active() ->
    active_int(all).

active_coordinators() ->
    active_int(coordinators).

active_workers() ->
    active_int(workers).

%% active_json() or active(json)?
active(json) ->
    to_json_list(active_int(all)).

active_coordinators(json) ->
    to_json_list(active_int(coordinators)).

active_workers(json) ->
    to_json_list(active_int(workers)).

active_int(coordinators) ->
    select_by_type(coordinators);
active_int(workers) ->
    select_by_type(workers);
active_int(all) ->
    select_by_type(all).

select_by_type(coordinators) ->
    ets:select(?CSRT_ETS, ets:fun2ms(fun(#rctx{type = #coordinator{}} = R) -> R end));
select_by_type(workers) ->
    ets:select(?CSRT_ETS, ets:fun2ms(fun(#rctx{type = #rpc_worker{}} = R) -> R end));
select_by_type(all) ->
    ets:tab2list(?CSRT_ETS).

find_by_nonce(Nonce) ->
    couch_srt_server:match_resource(#rctx{nonce = Nonce}).

find_by_pid(Pid) ->
    couch_srt_server:match_resource(#rctx{pid_ref = {Pid, '_'}}).

find_by_pidref(PidRef) ->
    couch_srt_server:match_resource(#rctx{pid_ref = PidRef}).

find_workers_by_pidref(PidRef) ->
    couch_srt_server:match_resource(#rctx{type = #rpc_worker{from = PidRef}}).

curry_field(Field) ->
    fun(Ele) -> couch_srt_entry:value(Field, Ele) end.

-spec group_by(Matcher, KeyFun, ValFun) ->
    {ok, aggregation_result()} | {limit, aggregation_result()}
when
    Matcher :: matcher(),
    KeyFun ::
        aggregation_keys_fun()
        | rctx_field()
        | [rctx_field()],
    ValFun ::
        value_key_fun()
        | rctx_field().
group_by(Matcher, KeyFun, ValFun) ->
    AggFun = fun erlang:'+'/2,
    group_by(Matcher, KeyFun, ValFun, AggFun).

-spec group_by(Matcher, KeyFun, ValFun, AggFun) ->
    {ok, aggregation_result()} | {limit, aggregation_result()}
when
    Matcher :: matcher(),
    KeyFun ::
        aggregation_keys_fun()
        | rctx_field()
        | [rctx_field()],
    ValFun ::
        value_key_fun()
        | rctx_field(),
    AggFun ::
        count_key_fun().
group_by(Matcher, KeyFun, ValFun, AggFun) ->
    group_by(Matcher, KeyFun, ValFun, AggFun, query_cardinality_limit()).

-spec all() ->
    matcher().

all() ->
    Spec = ets:fun2ms(fun(#rctx{} = R) -> R end),
    {Spec, ets:match_spec_compile(Spec)}.

%% eg: group_by(all(), username, docs_read).
%% eg: ^^ or: group_by(all(), [username, docs_read], ioq_calls).
%% eg: group_by(all(), [username, dbname, js_filter], docs_read).
%% eg: group_by(all(), [username, dbname, js_filter], ioq_calls).
%% eg: group_by(all(), [username, dbname, js_filter], get_kv_node).
-spec group_by(Matcher, KeyFun, ValFun, AggFun, Limit) ->
    {ok, aggregation_result()} | {limit, aggregation_result()}
when
    Matcher :: matcher(),
    KeyFun ::
        aggregation_keys_fun()
        | rctx_field()
        | [rctx_field()],
    ValFun ::
        value_key_fun()
        | rctx_field(),
    AggFun ::
        count_key_fun(),
    Limit :: pos_integer().

group_by(Matcher, KeyL, ValFun, AggFun, Limit) when is_list(KeyL) ->
    KeyFun = fun(Ele) -> list_to_tuple([couch_srt_entry:value(Key, Ele) || Key <- KeyL]) end,
    group_by(Matcher, KeyFun, ValFun, AggFun, Limit);
group_by(Matcher, Key, ValFun, AggFun, Limit) when is_atom(Key) ->
    group_by(Matcher, curry_field(Key), ValFun, AggFun, Limit);
group_by(Matcher, KeyFun, Val, AggFun, Limit) when is_atom(Val) ->
    group_by(Matcher, KeyFun, curry_field(Val), AggFun, Limit);
group_by(Matcher, KeyFun, ValFun, AggFun, Limit) ->
    %% This is a space versus speed tradeoff. Both query modes only filter
    %% through the table until `Limit` rows have been returned and both will
    %% utilize the compiled match_specs to do the testing, but
    %% `group_by_fold/5` will sequentially copy in every row and test it
    %% locally against the compiled match_spec using `ets:foldl/3`, whereas
    %% `group_by_select/5` does the filtering internally in the ETS NIF, by way
    %% of passing the uncompiled match_spec to `ets:select/3` to
    %% `ets:select/3`. The tradeoff here is that `ets:select` will copy `Limit`
    %% full `#rctx{}` records into this caller process, which we then aggregate
    %% over, as opposed to `ets:foldl` only sequentially loading a singular
    %% `#rctx{}` and extracting the relevant field value to aggregate on.
    %%
    %% The use of `query_by_fold` should only be needed if `Limit` is
    %% drastically increased, and even then, the efficiencies gained here with
    %% `query_by_fold` are shortlived until we can encode the values needed by
    %% `ValFun` into the match_spec return fields, at which point it becomes
    %% strictly worse.
    %%
    %% NOTE: This discrepancy of `ets:match_spec_run` taking a `match_spec()`
    %% vs `ets:select` taking a `comp_match_spec()` is why our CSRT `matcher()`
    %% type_spec funnels around both versions instead of just reference to the
    %% compiled spec stored by ETS internally.
    case config:get_boolean(?CSRT, "use_query_fold", false) of
        true ->
            group_by_fold(Matcher, KeyFun, ValFun, AggFun, Limit);
        false ->
            group_by_select(Matcher, KeyFun, ValFun, AggFun, Limit)
    end.

group_by_fold(Matcher, KeyFun, ValFun, AggFun, Limit) ->
    FoldFun = fun(Ele, Acc) ->
        case maps:size(Acc) =< Limit of
            true ->
                case ets_match(Ele, Matcher) of
                    true ->
                        Key = KeyFun(Ele),
                        Val = ValFun(Ele),
                        CurrVal = maps:get(Key, Acc, 0),
                        case AggFun(CurrVal, Val) of
                            0 ->
                                Acc;
                            NewVal ->
                                maps:put(Key, NewVal, Acc)
                        end;
                    false ->
                        Acc
                end;
            false ->
                throw({limit, Acc})
        end
    end,
    try
        {ok, ets:foldl(FoldFun, #{}, ?CSRT_ETS)}
    catch
        throw:{limit, Acc} ->
            {limit, Acc}
    end.

ets_match(Ele, {_, CMS}) ->
    ets:match_spec_run([Ele], CMS) =/= [].

group_by_select(Matcher, KeyFun, ValFun, AggFun, Limit) ->
    {Status, Rctxs} = group_by_select_rows(Matcher, Limit),
    %% If we hit `Status=limit` rows, still aggregate over what we found
    Aggregated = lists:foldl(fun(Rctx, Acc) ->
        Key = KeyFun(Rctx),
        Val = ValFun(Rctx),
        CurrVal = maps:get(Key, Acc, 0),
        case AggFun(CurrVal, Val) of
            0 ->
                Acc;
            NewVal ->
                maps:put(Key, NewVal, Acc)
        end
    end, #{}, Rctxs),
    {Status, Aggregated}.

group_by_select_rows(Matcher, Limit) ->
    try
        %% Use `ets:select/3` as this does the ets fold internally in a space
        %% efficient way that is still faster than the sequential traversal
        %% through the table. See the `ets:select/3` documentation for more
        %% info. We also use `ets:select/3` to pass the limit along, which
        %% results in ets effeciently traversing rows until `Limit` rows have
        %% been accumulated and returned.
        %% ets:select/* takes match_spec(), not  comp_match_spec()
        {MSpec, _CMSpec} = Matcher,
        case ets:select(?CSRT_ETS, MSpec, Limit) of
            %% Technically the {Rctxs, `continuation()`} here is an `opaque()`
            %% type, but we assume `'$end_of_table'` is a reasonable indication
            %% of no more rows. However, we fallback to checking the quantity
            %% returned in case this is ever no longer true.
            {Rctxs, '$end_of_table'} ->
                {ok, Rctxs};
            {Rctxs, _Continuation} ->
                %% Continuation is opaque, and there's no `is_more_rows` API to
                %% check to see if we actually limit the table Limit or we hit
                %% the edge case where exactly `Limit` rows were found.  The
                %% continuation can be passed back to `ets:select/1` to see if
                %% explicity returns `'$end_of_table'`, but if it did hit the
                %% `Limit`, we now wastefully fetch the next chunk of rows, so
                %% instead for now we assume that when the length of rows
                %% equals `Limit` that we hit the cap.  Note that this is only
                %% relevant because the API returning `'$end_of_table'` is not
                %% formally specified, but in theory this clause should not be
                %% hit.
                case length(Rctxs) >= Limit of
                    true ->
                        {limit, Rctxs};
                    false ->
                        {ok, Rctxs}
                end;
            %% Handle '$end_of_table'
            _ ->
                {ok, []}
        end
    catch
        _:_ ->
            {ok, []}
    end.

%%
%% Auxiliary functions to calculate topK
%%

-record(topK, {
    % we store ordered elements in ascending order
    seq = [] :: list({aggregation_key(), pos_integer()}),
    % we rely on erlang sorting order where `number < atom`
    min = infinite :: infinite | pos_integer(),
    max = 0 :: non_neg_integer(),
    size = 0 :: non_neg_integer(),
    % capacity cannot be less than 1
    capacity = 1 :: pos_integer()
}).

new_topK(K) when K >= 1 ->
    #topK{capacity = K}.

% when we are at capacity
% don't bother adding the value since it is less than what we already saw
update_topK(_Key, Value, #topK{size = S, capacity = S, min = Min} = Top) when Value < Min ->
    Top#topK{min = Value};
% when we are at capacity evict smallest value
update_topK(Key, Value, #topK{size = S, capacity = S, max = Max, seq = Seq} = Top) when
    Value > Max
->
    % capacity cannot be less than 1, so we can avoid handling the case when Seq is empty
    [_ | Truncated] = Seq,
    Top#topK{max = Value, seq = lists:keysort(2, [{Key, Value} | Truncated])};
% when we are at capacity and value is in between min and max evict smallest value
update_topK(Key, Value, #topK{size = S, capacity = S, seq = Seq} = Top) ->
    % capacity cannot be less than 1, so we can avoid handling the case when Seq is empty
    [_ | Truncated] = Seq,
    Top#topK{seq = lists:keysort(2, [{Key, Value} | Truncated])};
update_topK(Key, Value, #topK{size = S, min = Min, seq = Seq} = Top) when Value < Min ->
    Top#topK{size = S + 1, min = Value, seq = lists:keysort(2, [{Key, Value} | Seq])};
update_topK(Key, Value, #topK{size = S, max = Max, seq = Seq} = Top) when Value > Max ->
    Top#topK{size = S + 1, max = Value, seq = lists:keysort(2, [{Key, Value} | Seq])};
update_topK(Key, Value, #topK{size = S, seq = Seq} = Top) ->
    Top#topK{size = S + 1, seq = lists:keysort(2, [{Key, Value} | Seq])}.

get_topK(#topK{seq = S}) ->
    lists:reverse(S).

-spec topK(aggregation_result(), pos_integer()) ->
    ordered_result().
topK(Results, K) ->
    TopK = maps:fold(fun update_topK/3, new_topK(K), Results),
    get_topK(TopK).

%%
%% Query API functions
%%

%% @doc Specify the matcher to use for the query.
%% If atom 'all' is used then all entries would be in the scope of the query.
%% Also the use of 'all' makes the query 'unsafe'. Because it scans through all entries
%% and can return many matching rows.
%% Unsafe queries can only be run using 'unsafe_run/1'.
%% <code>
%% Q = query([
%%    ...
%%    from("docs_read")
%% ]),
%% </code>
%% @end
-spec from(MatcherName :: matcher_name() | all) ->
    {ok, #{from => matcher_name() | all, is_unsafe => boolean()}} | {error, any()}.
from(all) ->
    #from{matcher = all, is_safe = false};
from(MatcherName) ->
    case couch_srt_logger:get_matcher(MatcherName) of
        undefined ->
            {error, {unknown_matcher, MatcherName}};
        _ ->
            #from{matcher = MatcherName, is_safe = true}
    end.

%% @doc Construct 'options' query expression.
%% There are following types of expressions allowed in the query.
%%   <li>unlimited/0 @see unlimited/0 (cannot be used with 'with_limit/1')</li>
%%   <li>with_limit/1 @see with_limit/1 (cannot be used with 'unlimited/0')</li>
%% The order of expressions doesn't matter.
%% <code>
%% Q = query([
%%    ...
%%    options([
%%      ...
%%    ])
%% ]),
%% </code>
%% @end
-spec options([query_option()]) ->
    #query_options{} | {error, any()}.
options(Options) ->
    lists:foldl(
        fun
            (_, {error, _} = Error) ->
                Error;
            ({limit, unlimited}, Acc) ->
                Acc#query_options{limit = unlimited, is_safe = false};
            ({limit, Limit}, Acc) when is_integer(Limit) ->
                Acc#query_options{limit = Limit};
            ({error, _} = Error, _Acc) ->
                Error
        end,
        #query_options{is_safe = true},
        Options
    ).

%% @doc Enable unlimited number of results from the query.
%% The use of 'unlimited' makes the query 'unsafe'. Because it can return many matching rows.
%% Unsafe queries can only be run using 'unsafe_run/1'.
%% <code>
%% Q = query([
%%    ...
%%    options([
%%      unlimited()
%%    ])
%% ]),
%% </code>
%% @end
unlimited() ->
    {limit, unlimited}.

%% @doc Set limit on number of results returned from the query.
%% The construction of the query fail if the 'limit' is greater than
%% allowed for this cluster.
%% <code>
%% Q = query([
%%    ...
%%    options([
%%      with_limit(100)
%%    ])
%% ]),
%% </code>
%% @end
with_limit(Limit) when is_integer(Limit) ->
    case Limit =< query_limit() of
        true ->
            {limit, Limit};
        false ->
            {error, {beyond_limit, Limit}}
    end;
with_limit(Limit) ->
    {error, {invalid_limit, Limit}}.

%% @doc Request 'count_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    count_by(username)
%% ]),
%% </code>
%% @end
-spec count_by(AggregationKeys) ->
    {count_by, #selector{}} | {count_by, #unsafe_selector{}} | {error, any()}
when
    AggregationKeys ::
        aggregation_keys_fun()
        | binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()].
count_by(AggregationKeys) ->
    with_tag(select(AggregationKeys), count_by).

%% @doc Request 'sort_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    sort_by([username, dbname])
%% ]),
%% </code>
%% @end
-spec sort_by(AggregationKeys) ->
    {sort_by, #selector{}} | {sort_by, #unsafe_selector{}} | {error, any()}
when
    AggregationKeys ::
        aggregation_keys_fun()
        | binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()].
sort_by(AggregationKeys) ->
    with_tag(select(AggregationKeys), sort_by).

%% @doc Request 'sort_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    sort_by([username, dbname], ioq_calls)
%% ]),
%% </code>
%% @end
-spec sort_by(AggregationKeys, ValueKey) ->
    {sort_by, #selector{}} | {sort_by, #unsafe_selector{}} | {error, any()}
when
    AggregationKeys ::
        aggregation_keys_fun()
        | binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()],
    ValueKey ::
        value_key_fun()
        | binary()
        | rctx_field().
sort_by(AggregationKeys, ValueKey) ->
    with_tag(select(AggregationKeys, ValueKey), sort_by).

%% @doc Request 'group_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    group_by([username, dbname])
%% ]),
%% </code>
%% @end
-spec group_by(AggregationKeys) ->
    {group_by, #selector{}} | {group_by, #unsafe_selector{}} | {error, any()}
when
    AggregationKeys ::
        aggregation_keys_fun()
        | binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()].
group_by(AggregationKeys) ->
    with_tag(select(AggregationKeys), group_by).

%% @doc Request 'group_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    group_by([username, dbname], ioq_calls)
%% ]),
%% </code>
%% @end
-spec group_by(AggregationKeys, ValueKey) ->
    {group_by, #selector{}} | {group_by, #unsafe_selector{}} | {error, any()}
when
    AggregationKeys ::
        aggregation_keys_fun()
        | binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()],
    ValueKey ::
        value_key_fun()
        | binary()
        | rctx_field().
group_by(AggregationKeys, ValueKey) ->
    with_tag(select(AggregationKeys, ValueKey), group_by).

%% @doc Construct query from the expressions.
%% There are following types of expressions allowed in the query.
%%   <li>group_by/1 @see group_by/1</li>
%%   <li>group_by/2 @see group_by/1</li>
%%   <li>sort_by/1 @see sort_by/1</li>
%%   <li>count_by/1 @see count_by/1</li>
%%   <li>options/1 @see options/1</li>
%%   <li>from/1 @see from/1</li>
%% The order of expressions doesn't matter.
%% <code>
%% Q = query([
%%    from("docs_read"),
%%    group_by(username, dbname, ioq_calls),
%%    options([
%%       with_limit(10)
%%    ])
%% ]),
%% </code>
%% @end
query(Query) ->
    % start assuming safe query and turn to unsafe when we detect issues
    Acc = #query{is_safe = true},
    Result = lists:foldr(
        fun
            ({Aggregation, #unsafe_selector{} = Selector}, {E, #query{selector = undefined} = Q}) ->
                {E, Q#query{selector = Selector, is_safe = false, aggregation = Aggregation}};
            ({Aggregation, #unsafe_selector{}}, {E, Q}) ->
                {[{more_than_once, {select, Aggregation}} | E], Q};
            ({Aggregation, #selector{} = Selector}, {E, #query{selector = undefined} = Q}) ->
                {E, Q#query{selector = Selector, aggregation = Aggregation}};
            ({Aggregation, #selector{}}, {E, Q}) ->
                {[{more_than_once, {select, Aggregation}} | E], Q};
            (#query_options{is_safe = false, limit = Limit}, {E, #query{limit = undefined} = Q}) ->
                {E, Q#query{limit = Limit, is_safe = false}};
            (#query_options{limit = Limit}, {E, #query{limit = undefined} = Q}) ->
                {E, Q#query{limit = Limit}};
            (#query_options{}, {E, Q}) ->
                {[{more_than_once, options} | E], Q};
            (#from{matcher = Matcher, is_safe = false}, {E, #query{matcher = undefined} = Q}) ->
                {E, Q#query{matcher = Matcher, is_safe = false}};
            (#from{matcher = Matcher}, {E, #query{matcher = undefined} = Q}) ->
                {E, Q#query{matcher = Matcher}};
            (#from{}, {E, Q}) ->
                {[{more_than_once, from} | E], Q};
            ({error, Reason}, {E, Q}) ->
                {[Reason | E], Q}
        end,
        {[], Acc},
        Query
    ),
    case Result of
        {[], #query{} = Q} ->
            Q;
        {Errors, _} ->
            {error, Errors}
    end.

%% @doc Executes provided query. Only 'safe' queries can be executed using 'run'.
%% The query considered 'unsafe' if any of the conditions bellow are met:
%%   <li>Query uses 'unlimited/0'</li>
%%   <li>Query uses 'from(all)'</li>
%% <code>
%% Q = query([
%%    from("docs_read"),
%%    group_by(username, dbname, ioq_calls),
%%    options([
%%       with_limit(10)
%%    ])
%% ]),
%% run(Q)
%% </code>
%% @end
-spec run(#query{}) ->
    {ok, [{aggregation_key(), pos_integer()}]}
    | {limit, [{aggregation_key(), pos_integer()}]}.
run(#query{
    is_safe = true,
    matcher = MatcherName,
    selector = #selector{} = Selector,
    limit = Limit,
    aggregation = Aggregation
}) ->
    % we validated the presence of the matcher so this shouldn't fail
    {ok, Matcher} = get_matcher(MatcherName),
    case {Aggregation, Selector} of
        {count_by, #selector{aggregation_keys = AKey, value_key = undefined}} ->
            ValFun = fun(_) -> 1 end,
            {Result, Acc} = group_by(Matcher, AKey, ValFun),
            to_map({Result, topK(Acc, Limit)});
        {count_by, #selector{aggregation_keys = AKey, value_key = VKey}} ->
            {Result, Acc} = group_by(Matcher, AKey, VKey),
            to_map({Result, topK(Acc, Limit)});
        {sort_by, #selector{aggregation_keys = AKey, value_key = VKey}} ->
            {Result, Acc} = group_by(Matcher, AKey, VKey),
            {Result, topK(Acc, Limit)};
        {group_by, #selector{aggregation_keys = AKey, value_key = undefined}} ->
            ValFun = fun(_) -> 1 end,
            {Result, Acc} = group_by(Matcher, AKey, ValFun),
            to_map({Result, topK(Acc, Limit)});
        {group_by, #selector{aggregation_keys = AKey, value_key = VKey}} ->
            {Result, Acc} = group_by(Matcher, AKey, VKey),
            to_map({Result, topK(Acc, Limit)})
    end;
run(#query{}) ->
    {error,
        {unsafe_query, "Please use 'unsafe(Query)' instead if you really know what you are doing."}}.

%% @doc Executes provided query. This function is similar to 'run/1',
%% however it supports 'unsafe' queries. Be very careful using it.
%% Pay attention to cardinality of the result.
%% The query considered 'unsafe' if any of the conditions bellow are met:
%%   <li>Query uses 'unlimited/0'</li>
%%   <li>Query uses 'from(all)'</li>
%% <code>
%% Q = query([
%%    from("docs_read"),
%%    group_by(username, dbname, ioq_calls),
%%    options([
%%       with_limit(10)
%%    ])
%% ]),
%% unsafe_run(Q)
%% </code>
%% @end
-spec unsafe_run(#query{}) ->
    {ok, [{aggregation_key(), pos_integer()}]}
    | {limit, [{aggregation_key(), pos_integer()}]}.
unsafe_run(#query{selector = #unsafe_selector{} = Selector} = Query) ->
    %% mutate the record (since all fields stay the same)
    unsafe_run(Query#query{selector = setelement(1, Selector, selector)});
unsafe_run(#query{
    matcher = MatcherName,
    selector = #selector{} = Selector,
    limit = Limit,
    aggregation = Aggregation
}) ->
    Matcher = choose_matcher(MatcherName),
    case {Aggregation, Selector} of
        {count_by, #selector{aggregation_keys = AKey, value_key = undefined}} ->
            ValFun = fun(_) -> 1 end,
            to_map(maybe_apply_limit(group_by(Matcher, AKey, ValFun), Limit));
        {count_by, #selector{aggregation_keys = AKey, value_key = VKey}} ->
            to_map(maybe_apply_limit(group_by(Matcher, AKey, VKey), Limit));
        {sort_by, #selector{aggregation_keys = AKey, value_key = VKey}} ->
            maybe_apply_limit(group_by(Matcher, AKey, VKey), Limit);
        {group_by, #selector{aggregation_keys = AKey, value_key = undefined}} ->
            ValFun = fun(_) -> 1 end,
            to_map(maybe_apply_limit(group_by(Matcher, AKey, ValFun), Limit));
        {group_by, #selector{aggregation_keys = AKey, value_key = VKey}} ->
            to_map(maybe_apply_limit(group_by(Matcher, AKey, VKey), Limit))
    end.

%%
%% Query API auxiliary functions
%%

-spec select(AggregationKeys) ->
    #selector{} | #unsafe_selector{} | {error, any()}
when
    AggregationKeys ::
        aggregation_keys_fun() | binary() | rctx_field() | [binary()] | [rctx_field()].

select(AggregationKeys) ->
    maybe
        {ok, AKey} ?= parse_aggregation_keys(AggregationKeys),
        case is_safe_key(AKey) of
            true ->
                #selector{aggregation_keys = AKey};
            false ->
                #unsafe_selector{aggregation_keys = AKey}
        end
    end.

-spec select(AggregationKeys, ValueKey) ->
    {ok, #selector{} | #unsafe_selector{}} | {error, any()}
when
    AggregationKeys ::
        aggregation_keys_fun() | binary() | rctx_field() | [binary()] | [rctx_field()],
    ValueKey :: value_key_fun() | binary() | rctx_field().

select(AggregationKeys, ValueKey) ->
    maybe
        {ok, AKey} ?= parse_aggregation_keys(AggregationKeys),
        {ok, VKey} ?= parse_value_key(ValueKey),
        case is_safe_key(AKey) andalso is_safe_key(VKey) of
            true ->
                #selector{aggregation_keys = AKey, value_key = VKey};
            false ->
                #unsafe_selector{aggregation_keys = AKey, value_key = VKey}
        end
    end.

is_safe_key(Fun) when is_function(Fun) ->
    false;
is_safe_key(_) ->
    true.

parse_aggregation_keys(Fun) when is_function(Fun) ->
    validate_fun(Fun, key_fun);
parse_aggregation_keys(Keys) ->
    with_ok(parse_key(Keys)).

parse_value_key(Fun) when is_function(Fun) ->
    validate_fun(Fun, value_fun);
parse_value_key(Key) ->
    case parse_key(Key) of
        {error, _} = Error ->
            Error;
        Keys when is_list(Keys) ->
            {error, multiple_value_keys};
        K ->
            {ok, K}
    end.

with_tag({error, _} = Error, _) ->
    Error;
with_tag(Result, Tag) ->
    {Tag, Result}.

with_ok({error, _} = Error) ->
    Error;
with_ok(Result) ->
    {ok, Result}.

validate_fun(Fun, Tag) when is_function(Fun, 1) ->
    try Fun(#rctx{}) of
        _ ->
            {ok, Fun}
    catch
        _:_ ->
            {error, {invalid_fun, Tag}}
    end;
validate_fun(_Fun, Tag) ->
    {error, {invalid_fun, Tag}}.

choose_matcher(all) ->
    all();
choose_matcher(MatcherName) ->
    % we validated the presence of the matcher so this shouldn't fail
    {ok, Matcher} = get_matcher(MatcherName),
    Matcher.

-spec maybe_apply_limit(ResultsOrError, Limit) -> OrderedResultsOrError when
    ResultsOrError ::
        {ok, aggregation_result()}
        | {limit, aggregation_result()}
        | {error, any()},
    Limit :: unlimited | undefined | pos_integer(),
    OrderedResultsOrError ::
        {ok, ordered_result()}
        | {limit, ordered_result()}
        | {ok, aggregation_result()}
        | {limit, aggregation_result()}
        | {error, any()}.

maybe_apply_limit({Result, Results}, unlimited) ->
    {Result, Results};
maybe_apply_limit({Result, Results}, undefined) ->
    {Result, topK(Results, query_limit())};
maybe_apply_limit({Result, Results}, Limit) when is_integer(Limit) ->
    {Result, topK(Results, Limit)}.

-spec to_map(ResultsOrError) -> OrderedResultsOrError when
    ResultsOrError ::
        {ok, ordered_result() | aggregation_result()}
        | {limit, ordered_result() | aggregation_result()},
    OrderedResultsOrError ::
        {ok, aggregation_result()}
        | {limit, aggregation_result()}.
to_map({Result, Results}) when is_list(Results) ->
    {Result, maps:from_list(Results)};
to_map({Result, Results}) when is_map(Results) ->
    {Result, Results}.

-spec parse_key(Keys :: binary() | atom() | [binary()] | [atom()]) ->
    rctx_field()
    | [rctx_field()]
    | {error, Reason :: any()}.

parse_key([C | _] = Key) when is_integer(C) ->
    couch_srt_entry:key(Key);
parse_key(Keys) when is_list(Keys) ->
    parse_key(Keys, []);
parse_key(BinKey) when is_binary(BinKey) ->
    couch_srt_entry:key(BinKey);
parse_key(undefined) ->
    undefined;
parse_key(Key) when is_atom(Key) ->
    couch_srt_entry:key(Key).

parse_key([BinKey | Rest], Keys) ->
    case couch_srt_entry:key(BinKey) of
        {error, _} = Error ->
            Error;
        Key ->
            parse_key(Rest, [Key | Keys])
    end;
parse_key([], Keys) ->
    lists:reverse(Keys).

%%
%% Scanning with matchers
%%
-spec query_matcher(MatcherName :: string()) ->
    {ok, query_result()}
    | {error, any()}.
query_matcher(MatcherName) when is_list(MatcherName) ->
    query_matcher(MatcherName, query_limit()).

-spec query_matcher(MatcherName :: matcher_name(), Limit :: pos_integer()) ->
    {ok, query_result()}
    | {error, any()}.
query_matcher(MatcherName, Limit) when is_list(MatcherName) andalso is_integer(Limit) ->
    case get_matcher(MatcherName) of
        {ok, Matcher} ->
            query_matcher_rows(Matcher, Limit);
        Error ->
            Error
    end.

-spec query_matcher_rows(Matcher :: matcher()) ->
    {ok, query_result()}
    | {error, any()}.
query_matcher_rows(Matcher) ->
    query_matcher_rows(Matcher, query_limit()).

-spec query_matcher_rows(Matcher :: matcher(), Limit :: pos_integer()) ->
    {ok, query_result()}
    | {error, any()}.
query_matcher_rows({MSpec, _CompMSpec}, Limit) when
    is_list(MSpec) andalso is_integer(Limit) andalso Limit >= 1
->
    try
        %% ets:select/* takes match_spec(), not  comp_match_spec()
        %% use ets:select/3 to constrain to Limit rows, but we need to handle
        %% the continuation() style return type compared with ets:select/2.
        Rctxs =
            case ets:select(?CSRT_ETS, MSpec, Limit) of
                {Rctxs0, _Continuation} ->
                    Rctxs0;
                %% Handle '$end_of_table'
                _ ->
                    []
            end,
        {ok, to_json_list(Rctxs)}
    catch
        _:_ = Error ->
            {error, Error}
    end.

get_matcher(MatcherName) ->
    case couch_srt_logger:get_matcher(MatcherName) of
        undefined ->
            {error, {unknown_matcher, MatcherName}};
        Matcher ->
            {ok, Matcher}
    end.

%%
%% Auxiliary functions
%%
query_limit() ->
    config:get_integer(?CSRT, "query_limit", ?QUERY_LIMIT).

query_cardinality_limit() ->
    config:get_integer(?CSRT, "query_cardinality_limit", ?QUERY_CARDINALITY_LIMIT).

to_json_list(List) when is_list(List) ->
    lists:map(fun couch_srt_entry:to_json/1, List).
