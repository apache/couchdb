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

-module(csrt_query).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

%% aggregate query api
-export([
    active/0,
    active/1,
    active_coordinators/0,
    active_coordinators/1,
    active_workers/0,
    active_workers/1,
    all/0,
    count_by/1,
    count_by/2,
    find_by_nonce/1,
    find_by_pid/1,
    find_by_pidref/1,
    find_workers_by_pidref/1,
    group_by/3,
    group_by/4,
    query/1,
    query/2,
    query_matcher/1,
    query_matcher/2,
    query_matcher_rows/1,
    query_matcher_rows/2,
    sort_by/1,
    sort_by/2,
    sort_by/3
]).

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
    csrt_server:match_resource(#rctx{nonce = Nonce}).

find_by_pid(Pid) ->
    csrt_server:match_resource(#rctx{pid_ref = {Pid, '_'}}).

find_by_pidref(PidRef) ->
    csrt_server:match_resource(#rctx{pid_ref = PidRef}).

find_workers_by_pidref(PidRef) ->
    csrt_server:match_resource(#rctx{type = #rpc_worker{from = PidRef}}).

field(#rctx{pid_ref = Val}, pid_ref) -> Val;
%% NOTE: Pros and cons to doing these convert functions here
%% Ideally, this would be done later so as to prefer the core data structures
%% as long as possible, but we currently need the output of this function to
%% be jiffy:encode'able. The tricky bit is dynamically encoding the group_by
%% structure provided by the caller of *_by aggregator functions below.
%% For now, we just always return jiffy:encode'able data types.
field(#rctx{nonce = Val}, nonce) -> Val;
field(#rctx{type = Val}, type) -> csrt_util:convert_type(Val);
field(#rctx{dbname = Val}, dbname) -> Val;
field(#rctx{username = Val}, username) -> Val;
field(#rctx{db_open = Val}, db_open) -> Val;
field(#rctx{docs_read = Val}, docs_read) -> Val;
field(#rctx{docs_written = Val}, docs_written) -> Val;
field(#rctx{rows_read = Val}, rows_read) -> Val;
field(#rctx{changes_returned = Val}, changes_returned) -> Val;
field(#rctx{ioq_calls = Val}, ioq_calls) -> Val;
field(#rctx{js_filter = Val}, js_filter) -> Val;
field(#rctx{js_filtered_docs = Val}, js_filtered_docs) -> Val;
field(#rctx{get_kv_node = Val}, get_kv_node) -> Val;
field(#rctx{get_kp_node = Val}, get_kp_node) -> Val;
field(#rctx{started_at = Val}, started_at) -> Val;
field(#rctx{updated_at = Val}, updated_at) -> Val.

curry_field(Field) ->
    fun(Ele) -> field(Ele, Field) end.

count_by(KeyFun) ->
    csrt_query:count_by(all(), KeyFun).

count_by(Matcher, KeyFun) ->
    group_by(Matcher, KeyFun, fun(_) -> 1 end).

group_by(KeyFun, ValFun) ->
    csrt_query:group_by(all(), KeyFun, ValFun).

group_by(Matcher, KeyFun, ValFun) ->
    group_by(Matcher, KeyFun, ValFun, fun erlang:'+'/2).

group_by(Matcher, KeyFun, ValFun, AggFun) ->
    group_by(Matcher, KeyFun, ValFun, AggFun, ?QUERY_CARDINALITY_LIMIT).

-spec all() -> matcher().

all() ->
    Spec = ets:fun2ms(fun(#rctx{} = R) -> R end),
    {Spec, ets:match_spec_compile(Spec)}.

%% eg: group_by(all(), mfa, docs_read).
%% eg: group_by(all(), fun(#rctx{mfa=MFA,docs_read=DR}) -> {MFA, DR} end, ioq_calls).
%% eg: ^^ or: group_by(all(), [mfa, docs_read], ioq_calls).
%% eg: group_by(all(), [username, dbname, mfa], docs_read).
%% eg: group_by(all(), [username, dbname, mfa], ioq_calls).
%% eg: group_by(all(), [username, dbname, mfa], js_filters).
group_by(Matcher, KeyL, ValFun, AggFun, Limit) when is_list(KeyL) ->
    KeyFun = fun(Ele) -> list_to_tuple([field(Ele, Key) || Key <- KeyL]) end,
    group_by(Matcher, KeyFun, ValFun, AggFun, Limit);
group_by(Matcher, Key, ValFun, AggFun, Limit) when is_atom(Key) ->
    group_by(Matcher, curry_field(Key), ValFun, AggFun, Limit);
group_by(Matcher, KeyFun, Val, AggFun, Limit) when is_atom(Val) ->
    group_by(Matcher, KeyFun, curry_field(Val), AggFun, Limit);
group_by(Matcher, KeyFun, ValFun, AggFun, Limit) ->
    FoldFun = fun(Ele, Acc) ->
        case maps:size(Acc) =< Limit of
            true ->
                case maybe_match(Ele, Matcher) of
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
    catch throw:{limit, Acc} ->
        {limit, Acc}
    end.

maybe_match(_Ele, undefined) ->
    true;
maybe_match(Ele, {_, MS}) ->
    ets:match_spec_run([Ele], MS) =/= [].

%%
%% Auxiliary functions to calculate topK
%%

-record(topK, {
    % we store ordered elements in ascending order
    seq = [] :: list(pos_integer()),
    % we rely on erlang sorting order where `number < atom`
    min = infinite  :: infinite | pos_integer(),
    max = 0 :: pos_integer(),
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
update_topK(Key, Value, #topK{size = S, capacity = S, max = Max, seq = Seq} = Top) when Value > Max ->
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

topK(Results, K) ->
    TopK = maps:fold(fun update_topK/3, new_topK(K), Results),
    get_topK(TopK).

%% eg: sort_by([username, dbname, type], ioq_calls)
%% eg: sort_by([dbname, type], doc_reads)
sort_by(KeyFun) ->
    topK(count_by(KeyFun), 10).
sort_by(KeyFun, ValFun) ->
    {Result, Acc} = group_by(KeyFun, ValFun),
    {Result, topK(Acc, 10)}.
sort_by(KeyFun, ValFun, AggFun) ->
    {Result, Acc} = group_by(KeyFun, ValFun, AggFun),
    {Result, topK(Acc, 10)}.

to_json_list(List) when is_list(List) ->
    lists:map(fun csrt_util:to_json/1, List).

-spec query_matcher(MatcherName :: string()) -> {ok, query_result()}
    | {error, any()}.
query_matcher(MatcherName) when is_list(MatcherName)  ->
    query_matcher(MatcherName, query_limit()).

-spec query_matcher(MatcherName :: matcher_name(), Limit :: pos_integer()) -> {ok, query_result()}
    | {error, any()}.
query_matcher(MatcherName, Limit) when is_list(MatcherName) andalso is_integer(Limit) ->
    case csrt_logger:get_matcher(MatcherName) of
        undefined ->
            {error, {unknown_matcher, MatcherName}};
        Matcher ->
            query_matcher_rows(Matcher, Limit)
    end.

-spec query_matcher_rows(Matcher :: matcher()) -> {ok, query_result()}
    | {error, any()}.
query_matcher_rows(Matcher) ->
    query_matcher_rows(Matcher, query_limit()).

-spec query_matcher_rows(Matcher :: matcher(), Limit :: pos_integer()) -> {ok, query_result()}
    | {error, any()}.
query_matcher_rows({MSpec, _CompMSpec}, Limit) when is_list(MSpec) andalso is_integer(Limit) andalso Limit >= 1 ->
    try
        %% ets:select/* takes match_spec(), not  comp_match_spec()
        %% use ets:select/3 to constrain to Limit rows, but we need to handle
        %% the continuation() style return type compared with ets:select/2.
        Rctxs = case ets:select(?CSRT_ETS, MSpec, Limit) of
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

-spec query(Keys :: string() | [string()]) -> {ok, query_result()}
    | {error, any()}.
%% #{{<<"adm">>,<<"bench-yktbb3as46rzffea">>} => 2}
query(Keys) ->
    query(Keys, []).

-spec query(Keys :: string() | [string()], Options :: query_options()) -> {ok, query_result()}
    | {error, any()}.
%% #{{<<"adm">>,<<"bench-yktbb3as46rzffea">>} => 2}
query(_Keys, _Options) ->
    {error, todo}.

query_limit() ->
    config:get_integer(?CSRT, "query_limit", ?QUERY_LIMIT).
