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
    count_by/1,
    find_by_nonce/1,
    find_by_pid/1,
    find_by_pidref/1,
    find_workers_by_pidref/1,
    group_by/2,
    group_by/3,
    sorted/1,
    sorted_by/1,
    sorted_by/2,
    sorted_by/3
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
    ets:select(?MODULE, ets:fun2ms(fun(#rctx{type = #coordinator{}} = R) -> R end));
select_by_type(workers) ->
    ets:select(?MODULE, ets:fun2ms(fun(#rctx{type = #rpc_worker{}} = R) -> R end));
select_by_type(all) ->
    ets:tab2list(?MODULE).

find_by_nonce(Nonce) ->
    %%ets:match_object(?MODULE, ets:fun2ms(fun(#rctx{nonce = Nonce1} = R) when Nonce =:= Nonce1 -> R end)).
    [R || R <- ets:match_object(?MODULE, #rctx{nonce = Nonce})].

find_by_pid(Pid) ->
    %%[R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{pid_ref={Pid, '_'}, _ = '_'})].
    [R || R <- ets:match_object(?MODULE, #rctx{pid_ref = {Pid, '_'}})].

find_by_pidref(PidRef) ->
    %%[R || R <- ets:match_object(?MODULE, #rctx{pid_ref=PidRef, _ = '_'})].
    [R || R <- ets:match_object(?MODULE, #rctx{pid_ref = PidRef})].

find_workers_by_pidref(PidRef) ->
    %%[R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{type=#rpc_worker{from=PidRef}, _ = '_'})].
    [R || R <- ets:match_object(?MODULE, #rctx{type = #rpc_worker{from = PidRef}})].

field(#rctx{pid_ref = Val}, pid_ref) -> Val;
%% NOTE: Pros and cons to doing these convert functions here
%% Ideally, this would be done later so as to prefer the core data structures
%% as long as possible, but we currently need the output of this function to
%% be jiffy:encode'able. The tricky bit is dynamically encoding the group_by
%% structure provided by the caller of *_by aggregator functions below.
%% For now, we just always return jiffy:encode'able data types.
field(#rctx{nonce = Val}, nonce) -> Val;
%%field(#rctx{from=Val}, from) -> Val;
%% TODO: fix this, perhaps move it all to csrt_util?
field(#rctx{type = Val}, type) -> csrt_util:convert_type(Val);
field(#rctx{dbname = Val}, dbname) -> Val;
field(#rctx{username = Val}, username) -> Val;
%%field(#rctx{path=Val}, path) -> Val;
field(#rctx{db_open = Val}, db_open) -> Val;
field(#rctx{docs_read = Val}, docs_read) -> Val;
field(#rctx{rows_read = Val}, rows_read) -> Val;
field(#rctx{changes_processed = Val}, changes_processed) -> Val;
field(#rctx{changes_returned = Val}, changes_returned) -> Val;
field(#rctx{ioq_calls = Val}, ioq_calls) -> Val;
field(#rctx{io_bytes_read = Val}, io_bytes_read) -> Val;
field(#rctx{io_bytes_written = Val}, io_bytes_written) -> Val;
field(#rctx{js_evals = Val}, js_evals) -> Val;
field(#rctx{js_filter = Val}, js_filter) -> Val;
field(#rctx{js_filtered_docs = Val}, js_filtered_docs) -> Val;
field(#rctx{mango_eval_match = Val}, mango_eval_match) -> Val;
field(#rctx{get_kv_node = Val}, get_kv_node) -> Val;
field(#rctx{get_kp_node = Val}, get_kp_node) -> Val.

curry_field(Field) ->
    fun(Ele) -> field(Ele, Field) end.

count_by(KeyFun) ->
    group_by(KeyFun, fun(_) -> 1 end).

group_by(KeyFun, ValFun) ->
    group_by(KeyFun, ValFun, fun erlang:'+'/2).

%% eg: group_by(mfa, docs_read).
%% eg: group_by(fun(#rctx{mfa=MFA,docs_read=DR}) -> {MFA, DR} end, ioq_calls).
%% eg: ^^ or: group_by([mfa, docs_read], ioq_calls).
%% eg: group_by([username, dbname, mfa], docs_read).
%% eg: group_by([username, dbname, mfa], ioq_calls).
%% eg: group_by([username, dbname, mfa], js_filters).
group_by(KeyL, ValFun, AggFun) when is_list(KeyL) ->
    KeyFun = fun(Ele) -> list_to_tuple([field(Ele, Key) || Key <- KeyL]) end,
    group_by(KeyFun, ValFun, AggFun);
group_by(Key, ValFun, AggFun) when is_atom(Key) ->
    group_by(curry_field(Key), ValFun, AggFun);
group_by(KeyFun, Val, AggFun) when is_atom(Val) ->
    group_by(KeyFun, curry_field(Val), AggFun);
group_by(KeyFun, ValFun, AggFun) ->
    FoldFun = fun(Ele, Acc) ->
        Key = KeyFun(Ele),
        Val = ValFun(Ele),
        CurrVal = maps:get(Key, Acc, 0),
        NewVal = AggFun(CurrVal, Val),
        %% TODO: should we skip here? how to make this optional?
        case NewVal > 0 of
            true ->
                maps:put(Key, NewVal, Acc);
            false ->
                Acc
        end
    end,
    ets:foldl(FoldFun, #{}, ?MODULE).

%% Sorts largest first
sorted(Map) when is_map(Map) ->
    lists:sort(fun({_K1, A}, {_K2, B}) -> B < A end, maps:to_list(Map)).

shortened(L) ->
    lists:sublist(L, 10).

%% eg: sorted_by([username, dbname, mfa], ioq_calls)
%% eg: sorted_by([dbname, mfa], doc_reads)
sorted_by(KeyFun) -> shortened(sorted(count_by(KeyFun))).
sorted_by(KeyFun, ValFun) -> shortened(sorted(group_by(KeyFun, ValFun))).
sorted_by(KeyFun, ValFun, AggFun) -> shortened(sorted(group_by(KeyFun, ValFun, AggFun))).

to_json_list(List) when is_list(List) ->
    lists:map(fun csrt_util:to_json/1, List).
