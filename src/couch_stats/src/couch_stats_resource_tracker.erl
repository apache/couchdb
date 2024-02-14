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

-module(couch_stats_resource_tracker).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-export([
    inc/1, inc/2,
    maybe_inc/2,
    get_pid_ref/0,
    accumulate_delta/1
]).

-export([
    create_context/0, create_context/1, create_context/3,
    create_coordinator_context/1, create_coordinator_context/2,
    is_enabled/0,
    get_resource/0,
    get_resource/1,
    set_context_dbname/1,
    set_context_handler_fun/1,
    set_context_username/1,
    track/1,
    should_track/1
]).

-export([
    active/0,
    active_coordinators/0,
    active_workers/0,
    find_unmonitored/0
]).

-export([
    count_by/1,
    group_by/2,
    group_by/3,
    group_by/4,
    sorted/1,
    sorted_by/1,
    sorted_by/2,
    sorted_by/3,

    find_by_pid/1,

    unsafe_foldl/3,

    term_to_flat_json/1
]).

-export([
    make_delta/0
]).

%% Singular increment operations
-export([
    db_opened/0,
    doc_read/0,
    row_read/0,
    btree_fold/0,
    ioq_called/0,
    js_evaled/0,
    js_filtered/0,
    js_filtered_error/0,
    js_filtered_doc/0,
    mango_match_evaled/0,
    get_kv_node/0,
    get_kp_node/0
]).

%% Plural increment operations
-export([
    js_filtered_docs/1,
    io_bytes_read/1,
    io_bytes_written/1
]).

-export([
    field/2,
    curry_field/1
]).

-include_lib("couch/include/couch_db.hrl").

%% Use these for record upgrades over the wire and in ETS tables
%% TODO: alternatively, just delete these. Currently using a map
%% for shipping deltas over the wire, avoiding much of the
%% problem here. We'll likely still need to handle upgrades to
%% map format over time, so let's decide a course of action here.
-define(RCTX_V1, rctx_v1).
-define(RCTX, ?RCTX_V1).

-define(MANGO_EVAL_MATCH, mango_eval_match).
-define(DB_OPEN_DOC, docs_read).
-define(DB_OPEN, db_open).
-define(COUCH_SERVER_OPEN, db_open).
-define(COUCH_BT_FOLDS, btree_folds).
-define(COUCH_BT_GET_KP_NODE, get_kp_node).
-define(COUCH_BT_GET_KV_NODE, get_kv_node).
-define(COUCH_JS_FILTER, js_filter).
-define(COUCH_JS_FILTER_ERROR, js_filter_error).
-define(COUCH_JS_FILTERED_DOCS, js_filtered_docs).
-define(ROWS_READ, rows_read).

%% TODO: overlap between this and couch btree fold invocations
%% TODO: need some way to distinguish fols on views vs find vs all_docs
-define(FRPC_CHANGES_ROW, changes_processed).
-define(FRPC_CHANGES_RETURNED, changes_returned).
%%-define(FRPC_CHANGES_ROW, ?ROWS_READ).

%% Module pdict markers
-define(DELTA_TA, csrt_delta_ta).
-define(DELTA_TZ, csrt_delta_tz). %% T Zed instead of T0
-define(PID_REF, csrt_pid_ref). %% track local ID


-record(st, {
    eviction_delay = 10 * 1000, %% How many ms dead processes are visible
    scan_interval = 2048, %% How regularly to perfom scans
    tracking = #{} %% track active processes for eventual eviction
}).


%% TODO: switch to:
%% -record(?RCTX, {
-record(rctx, {
    %% Metadata
    started_at = tnow(),
    updated_at = tnow(),
    exited_at, %% TODO: do we need a final exit time and additional update times afterwards?
    pid_ref,
    mon_ref,
    mfa,
    nonce,
    from,
    type = unknown, %% unknown/background/system/rpc/coordinator/fabric_rpc/etc_rpc/etc
    state = alive,
    dbname,
    username,
    path,

    %% Stats counters
    db_open = 0,
    docs_read = 0,
    rows_read = 0,
    btree_folds = 0,
    changes_processed = 0,
    changes_returned = 0,
    ioq_calls = 0,
    io_bytes_read = 0,
    io_bytes_written = 0,
    js_evals = 0,
    js_filter = 0,
    js_filter_error = 0,
    js_filtered_docs = 0,
    mango_eval_match = 0,
    %% TODO: switch record definitions to be macro based, eg:
    %% ?COUCH_BT_GET_KP_NODE = 0,
    get_kv_node = 0,
    get_kp_node = 0
}).

%% monotonic time now in millisecionds
tnow() ->
    erlang:monotonic_time(millisecond).

is_enabled() ->
    config:get_boolean(?MODULE_STRING, "enabled", true).

db_opened() -> inc(db_opened).
doc_read() -> inc(docs_read).
row_read() -> inc(rows_read).
btree_fold() -> inc(?COUCH_BT_FOLDS).
%% TODO: do we need ioq_called and this access pattern?
ioq_called() -> is_enabled() andalso inc(ioq_calls).
js_evaled() -> inc(js_evals).
js_filtered() -> inc(js_filter).
js_filtered_error() -> inc(js_filter_error).
js_filtered_doc() -> inc(js_filtered_docs).
mango_match_evaled() -> inc(mango_eval_match).
get_kv_node() -> inc(get_kv_node).
get_kp_node() -> inc(get_kp_node).

js_filtered_docs(N) -> inc(js_filtered_docs, N).
io_bytes_read(N) -> inc(io_bytes_read, N).
io_bytes_written(N) -> inc(io_bytes_written, N).

inc(?DB_OPEN) ->
    inc(?DB_OPEN, 1);
inc(docs_read) ->
    inc(docs_read, 1);
inc(?ROWS_READ) ->
    inc(?ROWS_READ, 1);
inc(?FRPC_CHANGES_RETURNED) ->
    inc(?FRPC_CHANGES_RETURNED, 1);
inc(?COUCH_BT_FOLDS) ->
    inc(?COUCH_BT_FOLDS, 1);
inc(ioq_calls) ->
    inc(ioq_calls, 1);
inc(io_bytes_read) ->
    inc(io_bytes_read, 1);
inc(io_bytes_written) ->
    inc(io_bytes_written, 1);
inc(js_evals) ->
    inc(js_evals, 1);
inc(?COUCH_JS_FILTER) ->
    inc(?COUCH_JS_FILTER, 1);
inc(?COUCH_JS_FILTER_ERROR) ->
    inc(?COUCH_JS_FILTER_ERROR, 1);
inc(?COUCH_JS_FILTERED_DOCS) ->
    inc(?COUCH_JS_FILTERED_DOCS, 1);
inc(?MANGO_EVAL_MATCH) ->
    inc(?MANGO_EVAL_MATCH, 1);
inc(?COUCH_BT_GET_KP_NODE) ->
    inc(?COUCH_BT_GET_KP_NODE, 1);
inc(?COUCH_BT_GET_KV_NODE) ->
    inc(?COUCH_BT_GET_KV_NODE, 1);
inc(_) ->
    0.


inc(?DB_OPEN, N) ->
    update_counter(#rctx.?DB_OPEN, N);
inc(?ROWS_READ, N) ->
    update_counter(#rctx.?ROWS_READ, N);
inc(?FRPC_CHANGES_RETURNED, N) ->
    update_counter(#rctx.?FRPC_CHANGES_RETURNED, N);
inc(ioq_calls, N) ->
    update_counter(#rctx.ioq_calls, N);
inc(io_bytes_read, N) ->
    update_counter(#rctx.io_bytes_read, N);
inc(io_bytes_written, N) ->
    update_counter(#rctx.io_bytes_written, N);
inc(js_evals, N) ->
    update_counter(#rctx.js_evals, N);
inc(?COUCH_JS_FILTER, N) ->
    update_counter(#rctx.?COUCH_JS_FILTER, N);
inc(?COUCH_JS_FILTER_ERROR, N) ->
    update_counter(#rctx.?COUCH_JS_FILTER_ERROR, N);
inc(?COUCH_JS_FILTERED_DOCS, N) ->
    update_counter(#rctx.?COUCH_JS_FILTERED_DOCS, N);
inc(?MANGO_EVAL_MATCH, N) ->
    update_counter(#rctx.?MANGO_EVAL_MATCH, N);
inc(?DB_OPEN_DOC, N) ->
    update_counter(#rctx.?DB_OPEN_DOC, N);
inc(?FRPC_CHANGES_ROW, N) ->
    update_counter(#rctx.?ROWS_READ, N); %% TODO: rework double use of rows_read
inc(?COUCH_BT_GET_KP_NODE, N) ->
    update_counter(#rctx.?COUCH_BT_GET_KP_NODE, N);
inc(?COUCH_BT_GET_KV_NODE, N) ->
    update_counter(#rctx.?COUCH_BT_GET_KV_NODE, N);
inc(_, _) ->
    %% inc needs to allow unknown types to pass for accumulate_update to handle
    %% updates from nodes with newer data formats
    0.

maybe_inc([mango, evaluate_selector], Val) ->
    inc(?MANGO_EVAL_MATCH, Val);
maybe_inc([couchdb, database_reads], Val) ->
    inc(?DB_OPEN_DOC, Val);
maybe_inc([fabric_rpc, changes, processed], Val) ->
    inc(?FRPC_CHANGES_ROW, Val);
maybe_inc([fabric_rpc, changes, returned], Val) ->
    inc(?FRPC_CHANGES_RETURNED, Val);
maybe_inc([fabric_rpc, view, rows_read], Val) ->
    inc(?ROWS_READ, Val);
maybe_inc([couchdb, couch_server, open], Val) ->
    inc(?DB_OPEN, Val);
maybe_inc([couchdb, btree, folds], Val) ->
    inc(?COUCH_BT_FOLDS, Val);
maybe_inc([couchdb, btree, kp_node], Val) ->
    inc(?COUCH_BT_GET_KP_NODE, Val);
maybe_inc([couchdb, btree, kv_node], Val) ->
    inc(?COUCH_BT_GET_KV_NODE, Val);
maybe_inc([couchdb, query_server, js_filter_error], Val) ->
    inc(?COUCH_JS_FILTER_ERROR, Val);
maybe_inc([couchdb, query_server, js_filter], Val) ->
    inc(?COUCH_JS_FILTER, Val);
maybe_inc([couchdb, query_server, js_filtered_docs], Val) ->
    inc(?COUCH_JS_FILTERED_DOCS, Val);
maybe_inc(_Metric, _Val) ->
    %%io:format("SKIPPING MAYBE_INC METRIC[~p]: ~p~n", [Val, Metric]),
    0.


%% TODO: update stats_descriptions.cfg for relevant apps
should_track([fabric_rpc, all_docs, spawned]) ->
    is_enabled();
should_track([fabric_rpc, changes, spawned]) ->
    is_enabled();
should_track([fabric_rpc, changes, processed]) ->
    is_enabled();
should_track([fabric_rpc, changes, returned]) ->
    is_enabled();
should_track([fabric_rpc, map_view, spawned]) ->
    is_enabled();
should_track([fabric_rpc, reduce_view, spawned]) ->
    is_enabled();
should_track([fabric_rpc, get_all_security, spawned]) ->
    is_enabled();
should_track([fabric_rpc, open_doc, spawned]) ->
    is_enabled();
should_track([fabric_rpc, update_docs, spawned]) ->
    is_enabled();
should_track([fabric_rpc, open_shard, spawned]) ->
    is_enabled();
should_track([mango_cursor, view, all_docs]) ->
    is_enabled();
should_track([mango_cursor, view, idx]) ->
    is_enabled();
should_track(_Metric) ->
    %%io:format("SKIPPING METRIC: ~p~n", [Metric]),
    false.

accumulate_delta(Delta) when is_map(Delta) ->
    %% TODO: switch to creating a batch of updates to invoke a single
    %% update_counter rather than sequentially invoking it for each field
    is_enabled() andalso maps:foreach(fun inc/2, Delta);
accumulate_delta(undefined) ->
    ok;
accumulate_delta(Other) ->
    io:format("CSRT:ACC_DELTA UNKNOWN DELTA: ~p~n", [Other]).


update_counter(Field, Count) ->
    is_enabled() andalso update_counter(get_pid_ref(), Field, Count).


update_counter({_Pid,_Ref}=PidRef, Field, Count) ->
    %% TODO: mem3 crashes without catch, why do we lose the stats table?
    is_enabled() andalso catch ets:update_counter(?MODULE, PidRef, {Field, Count}, #rctx{pid_ref=PidRef}).


active() -> active_int(all).
active_coordinators() -> active_int(coordinators).
active_workers() -> active_int(workers).


active_int(coordinators) ->
    select_by_type(coordinators);
active_int(workers) ->
    select_by_type(workers);
active_int(all) ->
    lists:map(fun to_flat_json/1, ets:tab2list(?MODULE)).


select_by_type(coordinators) ->
    ets:select(couch_stats_resource_tracker,
        [{#rctx{type = {coordinator,'_','_'}, _ = '_'}, [], ['$_']}]);
select_by_type(workers) ->
    ets:select(couch_stats_resource_tracker,
        [{#rctx{type = {worker,'_','_'}, _ = '_'}, [], ['$_']}]);
select_by_type(all) ->
    lists:map(fun to_flat_json/1, ets:tab2list(?MODULE)).


field(#rctx{pid_ref=Val}, pid_ref) -> Val;
%% NOTE: Pros and cons to doing these convert functions here
%% Ideally, this would be done later so as to prefer the core data structures
%% as long as possible, but we currently need the output of this function to
%% be jiffy:encode'able. The tricky bit is dynamically encoding the group_by
%% structure provided by the caller of *_by aggregator functions below.
%% For now, we just always return jiffy:encode'able data types.
field(#rctx{mfa=Val}, mfa) -> convert_mfa(Val);
field(#rctx{nonce=Val}, nonce) -> Val;
field(#rctx{from=Val}, from) -> Val;
field(#rctx{type=Val}, type) -> convert_type(Val);
field(#rctx{state=Val}, state) -> Val;
field(#rctx{dbname=Val}, dbname) -> Val;
field(#rctx{username=Val}, username) -> Val;
field(#rctx{path=Val}, path) -> Val;
field(#rctx{db_open=Val}, db_open) -> Val;
field(#rctx{docs_read=Val}, docs_read) -> Val;
field(#rctx{rows_read=Val}, rows_read) -> Val;
field(#rctx{btree_folds=Val}, btree_folds) -> Val;
field(#rctx{changes_processed=Val}, changes_processed) -> Val;
field(#rctx{changes_returned=Val}, changes_returned) -> Val;
field(#rctx{ioq_calls=Val}, ioq_calls) -> Val;
field(#rctx{io_bytes_read=Val}, io_bytes_read) -> Val;
field(#rctx{io_bytes_written=Val}, io_bytes_written) -> Val;
field(#rctx{js_evals=Val}, js_evals) -> Val;
field(#rctx{js_filter=Val}, js_filter) -> Val;
field(#rctx{js_filter_error=Val}, js_filter_error) -> Val;
field(#rctx{js_filtered_docs=Val}, js_filtered_docs) -> Val;
field(#rctx{mango_eval_match=Val}, mango_eval_match) -> Val;
field(#rctx{get_kv_node=Val}, get_kv_node) -> Val;
field(#rctx{get_kp_node=Val}, get_kp_node) -> Val.


curry_field(Field) ->
    fun(Ele) -> field(Ele, Field) end.


count_by(KeyFun) ->
    group_by(KeyFun, fun(_) -> 1 end).


group_by(KeyFun, ValFun) ->
    group_by(KeyFun, ValFun, fun erlang:'+'/2).


group_by(KeyFun, ValFun, AggFun) ->
    group_by(KeyFun, ValFun, AggFun, fun ets:foldl/3).


%% eg: group_by(mfa, docs_read).
%% eg: group_by(fun(#rctx{mfa=MFA,docs_read=DR}) -> {MFA, DR} end, ioq_calls).
%% eg: ^^ or: group_by([mfa, docs_read], ioq_calls).
%% eg: group_by([username, dbname, mfa], docs_read).
%% eg: group_by([username, dbname, mfa], ioq_calls).
%% eg: group_by([username, dbname, mfa], js_filters).
group_by(KeyL, ValFun, AggFun, Fold) when is_list(KeyL) ->
    KeyFun = fun(Ele) -> list_to_tuple([field(Ele, Key) || Key <- KeyL]) end,
    group_by(KeyFun, ValFun, AggFun, Fold);
group_by(Key, ValFun, AggFun, Fold) when is_atom(Key) ->
    group_by(curry_field(Key), ValFun, AggFun, Fold);
group_by(KeyFun, Val, AggFun, Fold) when is_atom(Val) ->
    group_by(KeyFun, curry_field(Val), AggFun, Fold);
group_by(KeyFun, ValFun, AggFun, Fold) ->
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
    Fold(FoldFun, #{}, ?MODULE).


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


term_to_flat_json({shutdown, Reason0}) when is_atom(Reason0) ->
    Reason = atom_to_binary(Reason0),
    <<"shutdown: ", Reason/binary>>;
term_to_flat_json({type, Atom}) when is_atom(Atom) ->
    atom_to_binary(Atom);
term_to_flat_json({type, {coordinator, Verb0, Path0}}=_Type) ->
    Verb = atom_to_binary(Verb0),
    Path = list_to_binary(Path0),
    <<"coordinator:", Verb/binary, ":", Path/binary>>;
term_to_flat_json({type, {worker, M0, F0}}=_Type) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    <<"worker:", M/binary, ":", F/binary>>;
term_to_flat_json(Tuple) when is_tuple(Tuple) ->
    erlang:tuple_to_list(Tuple);
term_to_flat_json(Pid) when is_pid(Pid) ->
    ?l2b(pid_to_list(Pid));
term_to_flat_json(Ref) when is_reference(Ref) ->
    ?l2b(ref_to_list(Ref));
term_to_flat_json(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
term_to_flat_json(undefined) ->
    null;
term_to_flat_json(null) ->
    null;
term_to_flat_json(T) ->
    T.

to_flat_json(#rctx{}=Rctx) ->
    #rctx{
        updated_at = TP,
        started_at = TInit,
        pid_ref = {Pid0, Ref0},
        mfa = MFA0,
        nonce = Nonce0,
        from = From0,
        dbname = DbName,
        username = UserName,
        db_open = DbOpens,
        docs_read = DocsRead,
        rows_read = RowsRead,
        js_filter = JSFilters,
        js_filter_error = JSFilterErrors,
        js_filtered_docs = JSFilteredDocss,
        state = State0,
        type = Type,
        get_kp_node = KpNodes,
        get_kv_node = KvNodes,
        btree_folds = ChangesProcessed,
        changes_returned = ChangesReturned,
        ioq_calls = IoqCalls
    } = Rctx,
    Pid = term_to_flat_json(Pid0),
    Ref = term_to_flat_json(Ref0),
    PidRef = <<Pid/binary, ":", Ref/binary>>,
    MFA = case MFA0 of
        {M0, F0, A0} ->
            M = atom_to_binary(M0),
            F = atom_to_binary(F0),
            A = integer_to_binary(A0),
            <<M/binary, ":", F/binary, "/", A/binary>>;
        MFA0 when is_list(MFA0) ->
            MFA0;
        undefined ->
            null;
        OtherMFA ->
            throw({error, {unexpected, OtherMFA}})
    end,
    From = case From0 of
        {Parent0, ParentRef0} ->
            Parent = term_to_flat_json(Parent0),
            ParentRef = term_to_flat_json(ParentRef0),
            <<Parent/binary, ":", ParentRef/binary>>;
        undefined ->
            null
    end,
    State = case State0 of
        alive ->
            alive;
        {down, Reason0}  ->
            Reason = term_to_flat_json(Reason0),
            <<"down: ", Reason/binary>>
    end,
    Nonce = case Nonce0 of
        undefined ->
            null;
        Nonce0 ->
            list_to_binary(Nonce0)
    end,
    #{
        updated_at => TP,
        started_at => TInit,
        pid_ref => PidRef,
        mfa => MFA,
        nonce => Nonce,
        from => From,
        dbname => DbName,
        username => UserName,
        db_open => DbOpens,
        docs_read => DocsRead,
        js_filter => JSFilters,
        js_filter_error => JSFilterErrors,
        js_filtered_docs => JSFilteredDocss,
        rows_read => RowsRead,
        state => State,
        type => term_to_flat_json({type, Type}),
        kp_nodes => KpNodes,
        kv_nodes => KvNodes,
        btree_folds => ChangesProcessed,
        changes_returned => ChangesReturned,
        ioq_calls => IoqCalls
    }.


convert_mfa(MFA) when is_list(MFA)  ->
    list_to_binary(MFA);
convert_mfa({M0, F0, A0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    A = integer_to_binary(A0),
    <<M/binary, ":", F/binary, "/", A/binary>>;
convert_mfa(undefined) ->
    null.

convert_type(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
convert_type({coordinator, Verb0, Path0}) ->
    Verb = atom_to_binary(Verb0),
    Path = list_to_binary(Path0),
    <<"coordinator:", Verb/binary, ":", Path/binary>>;
convert_type({worker, M0, F0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    <<"worker:", M/binary, ":", F/binary>>.

get_pid_ref() ->
    case get(?PID_REF) of
        undefined ->
            Ref = make_ref(),
            set_pid_ref({self(), Ref});
        PidRef ->
            PidRef
    end.


create_context() ->
    is_enabled() andalso create_context(self()).


create_context(Pid) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            Ref = make_ref(),
            Rctx = make_record(Pid, Ref),
            track(Rctx),
            create_resource(Rctx),
            Rctx
    end.


create_resource(#rctx{} = Rctx) ->
    %% true = ets:insert(?MODULE, Rctx).
    catch ets:insert(?MODULE, Rctx).

%% add type to disnguish coordinator vs rpc_worker
create_context(From, {M,F,_A} = MFA, Nonce) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            PidRef = get_pid_ref(), %% this will instantiate a new PidRef
            %% TODO: extract user_ctx and db/shard from
            Rctx = #rctx{
                pid_ref = PidRef,
                from = From,
                mfa = MFA,
                type = {worker, M, F},
                nonce = Nonce
            },
            track(Rctx),
            erlang:put(?DELTA_TZ, Rctx),
            create_resource(Rctx),
            Rctx
    end.

create_coordinator_context(#httpd{path_parts=Parts} = Req) ->
    is_enabled() andalso create_coordinator_context(Req, io_lib:format("~p", [Parts])).

create_coordinator_context(#httpd{} = Req, Path) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            #httpd{
                method = Verb,
                nonce = Nonce
            } = Req,
            PidRef = get_pid_ref(), %% this will instantiate a new PidRef
            Rctx = #rctx{
                pid_ref = PidRef,
                %%type = {coordinator, Verb, Path},
                type = {coordinator, Verb, init},
                nonce = Nonce,
                path = list_to_binary([$/ | Path])
            },
            track(Rctx),
            erlang:put(?DELTA_TZ, Rctx),
            create_resource(Rctx),
            Rctx
    end.

set_context_dbname(DbName) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            catch case ets:update_element(?MODULE, get_pid_ref(), [{#rctx.dbname, DbName}]) of
                false ->
                    Stk = try throw(42) catch _:_:Stk0 -> Stk0 end,
                    io:format("UPDATING DBNAME[~p] FAILURE WITH CONTEXT: ~p AND STACK:~n~pFOO:: ~p~n~n", [DbName, get_resource(), Stk, process_info(self(), current_stacktrace)]),
                    timer:sleep(1000),
                    erlang:halt(kaboomz);
                true ->
                    true
            end
    end.

set_context_handler_fun(Fun) when is_function(Fun) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            FunName = erlang:fun_to_list(Fun),
            #rctx{type={coordinator, Verb, _}} = get_resource(),
            Update = [{#rctx.type, {coordinator, Verb, FunName}}],
            catch case ets:update_element(?MODULE, get_pid_ref(), Update) of
                false ->
                    Stk = try throw(42) catch _:_:Stk0 -> Stk0 end,
                    io:format("UPDATING HANDLER FUN[~p] FAILURE WITH CONTEXT: ~p AND STACK:~n~pFOO:: ~p~n~n", [FunName, get_resource(), Stk, process_info(self(), current_stacktrace)]),
                    timer:sleep(1000),
                    erlang:halt(kaboomz);
                true ->
                    true
            end
    end.

set_context_username(null) ->
    ok;
set_context_username(UserName) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            catch case ets:update_element(?MODULE, get_pid_ref(), [{#rctx.username, UserName}]) of
                false ->
                    Stk = try throw(42) catch _:_:Stk0 -> Stk0 end,
                    io:format("UPDATING USERNAME[~p] FAILURE WITH CONTEXT: ~p AND STACK:~n~pFOO:: ~p~n~n", [UserName, get_resource(), Stk, process_info(self(), current_stacktrace)]),
                    timer:sleep(1000),
                    erlang:halt(kaboomz);
                true ->
                    true
            end
    end.

track(#rctx{}=Rctx) ->
    %% TODO: should this block or not? If no, what cleans up zombies?
    %% gen_server:call(?MODULE, {track, PR}).
    gen_server:cast(?MODULE, {track, Rctx}).


make_delta() ->
    TA = case get(?DELTA_TA) of
        undefined ->
            %% Need to handle this better, can't just make a new T0 at T' as
            %% the timestamps will be identical causing a divide by zero error.
            %%
            %% Realistically need to ensure that all invocations of database
            %% operations sets T0 appropriately. Perhaps it's possible to do
            %% this is the couch_db:open chain, and then similarly, in
            %% couch_server, and uhhhh... couch_file, and...
            %%
            %% I think we need some type of approach for establishing a T0 that
            %% doesn't result in outrageous deltas. For now zero out the
            %% microseconds field, or subtract a second on the off chance that
            %% microseconds is zero. I'm not uptodate on the latest Erlang time
            %% libraries and don't remember how to easily get an
            %% `os:timestamp()` out of now() - 100ms or some such.
            %%
            %% I think it's unavoidable that we'll have some codepaths that do
            %% not properly instantiate the T0 at spawn resulting in needing to
            %% do some time of "time warp" or ignoring the timing collection
            %% entirely. Perhaps if we hoisted out the stats collection into
            %% the primary flow of the database and funnel that through all the
            %% function clauses we could then utilize Dialyzer to statically
            %% analyze and assert all code paths that invoke database
            %% operations have properly instantinated a T0 at the appropriate
            %% start time such that we don't have to "fudge" deltas with a
            %% missing start point, but we're a long ways from that happening
            %% so I feel it necessary to address the NULL start time.

            %% Track how often we fail to initiate T0 correctly
            %% Perhaps somewhat naughty we're incrementing stats from within
            %% couch_stats itself? Might need to handle this differently
            %% TODO: determine appropriate course of action here
            %% io:format("~n**********MISSING STARTING DELTA************~n~n", []),
            couch_stats:increment_counter(
                [couchdb, csrt, delta_missing_t0]),
                %%[couch_stats_resource_tracker, delta_missing_t0]),

            case erlang:get(?DELTA_TZ) of
                undefined ->
                    TA0 = make_delta_base(),
                    %% TODO: handline missing deltas, otherwise divide by zero
                    set_delta_a(TA0),
                    TA0;
                TA0 ->
                    TA0
            end;
        #rctx{} = TA0 ->
            TA0
    end,
    TB = get_resource(),
    Delta = make_delta(TA, TB),
    set_delta_a(TB),
    Delta.


make_delta(#rctx{}=TA, #rctx{}=TB) ->
    Delta = #{
        docs_read => TB#rctx.docs_read - TA#rctx.docs_read,
        js_filter => TB#rctx.js_filter - TA#rctx.js_filter,
        js_filter_error => TB#rctx.js_filter_error - TA#rctx.js_filter_error,
        js_filtered_docs => TB#rctx.js_filtered_docs - TA#rctx.js_filtered_docs,
        rows_read => TB#rctx.rows_read - TA#rctx.rows_read,
        changes_returned => TB#rctx.changes_returned - TA#rctx.changes_returned,
        btree_folds => TB#rctx.btree_folds - TA#rctx.btree_folds,
        get_kp_node => TB#rctx.get_kp_node - TA#rctx.get_kp_node,
        get_kv_node => TB#rctx.get_kv_node - TA#rctx.get_kv_node,
        db_open => TB#rctx.db_open - TA#rctx.db_open,
        ioq_calls => TB#rctx.ioq_calls - TA#rctx.ioq_calls,
        dt => TB#rctx.updated_at - TA#rctx.updated_at
    },
    %% TODO: reevaluate this decision
    %% Only return non zero (and also positive) delta fields
    maps:filter(fun(_K,V) -> V > 0 end, Delta);
make_delta(_, #rctx{}) ->
    #{error => missing_beg_rctx};
make_delta(#rctx{}, _) ->
    #{error => missing_fin_rctx}.

make_delta_base() ->
    Ref = make_ref(),
    %% TODO: extract user_ctx and db/shard from request
    Now = tnow(),
    #rctx{
        pid_ref = {self(), Ref},
        started_at = Now - 100, %% give us 100ms rewind time for missing T0
        updated_at = Now
    }.

set_delta_a(TA) ->
    erlang:put(?DELTA_TA, TA).

set_pid_ref(PidRef) ->
    erlang:put(?PID_REF, PidRef),
    PidRef.

get_resource() ->
    get_resource(get_pid_ref()).

get_resource(PidRef) ->
    catch case ets:lookup(?MODULE, PidRef) of
        [#rctx{}=TP] ->
            TP;
        [] ->
            undefined
    end.

make_record(Pid, Ref) ->
    #rctx{pid_ref = {Pid, Ref}}.


find_unmonitored() ->
    %% TODO: only need PidRef here, replace with a select that does that...
    [PR || #rctx{pid_ref=PR} <- ets:match_object(?MODULE, #rctx{mon_ref=undefined, _ = '_'})].


find_by_pid(Pid) ->
    [R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{pid_ref={Pid, '_'}, _ = '_'})].


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE, [
        named_table,
        public,
        {decentralized_counters, true}, %% TODO: test impact of this
        {write_concurrency, true},
        {read_concurrency, true},
        {keypos, #rctx.pid_ref}
    ]),
    St = #st{},
    case is_enabled() of
        false ->
            ok;
        true ->
            _TimerRef = erlang:send_after(St#st.scan_interval, self(), scan)
    end,
    {ok, St}.

handle_call(fetch, _from, #st{} = St) ->
    {reply, {ok, St}, St};
handle_call({track, _}=Msg, _From, St) ->
    {noreply, St1} = handle_cast(Msg, St),
    {reply, ok, St1};
handle_call(Msg, _From, St) ->
    {stop, {unknown_call, Msg}, error, St}.

handle_cast({track, #rctx{pid_ref=PidRef}}, #st{tracking=AT0} = St0) ->
    AT = maybe_track(PidRef, AT0),
    {noreply, St0#st{tracking=AT}};
handle_cast(Msg, St) ->
    {stop, {unknown_cast, Msg}, St}.

handle_info(scan, #st{tracking=AT0} = St0) ->
    Unmonitored = find_unmonitored(),
    AT = maybe_track(Unmonitored, AT0),
    _TimerRef = erlang:send_after(St0#st.scan_interval, self(), scan),
    {noreply, St0#st{tracking=AT}};
handle_info({'DOWN', MonRef, _Type, DPid, Reason0}, #st{tracking=AT0} = St0) ->
    %% io:format("CSRT:HI(~p)~n", [{'DOWN', MonRef, Type, DPid, Reason}]),
    St = case maps:get(MonRef, AT0, undefined) of
        undefined ->
            io:format("ERROR: UNEXPECTED MISSING MONITOR IN TRACKING TABLE: {~p, ~p}~n", [MonRef, DPid]),
            St0;
        {RPid, _Ref} = PidRef ->
            if
                RPid =:= DPid -> ok;
                true -> erlang:halt(io_lib:format("CSRT:HI PID MISMATCH ABORT: ~p =/= ~p~n", [DPid, RPid]))
            end,
            %% remove double bookkeeping
            AT = maps:remove(MonRef, maps:remove(PidRef, AT0)),
            %% TODO: Assert Pid matches Object
            %% update process state in live table
            %% TODO: decide whether we want the true match to crash this process on failure
            %% true = ets:update_element(?MODULE, PidRef,
            Reason = case Reason0 of
                {shutdown, Shutdown0} ->
                    Shutdown = atom_to_binary(Shutdown0),
                    <<"shutdown: ", Shutdown/binary>>;
                Reason0 ->
                    Reason0
            end,
            ets:update_element(?MODULE, PidRef,
                [{#rctx.state, {down, Reason}}, {#rctx.updated_at, tnow()}]),
            log_process_lifetime_report(PidRef),
            %% Delay eviction to allow human visibility on short lived pids
            erlang:send_after(St0#st.eviction_delay, self(), {evict, PidRef}),
            St0#st{tracking=AT}
    end,
    {noreply, St};
handle_info({evict, {_Pid, _Ref}=PidRef}, #st{}=St) ->
    ets:delete(?MODULE, PidRef),
    {noreply, St};
handle_info(Msg, St) ->
    {stop, {unknown_info, Msg}, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


maybe_track([], AT) ->
    AT;
maybe_track(PidRef, AT) when is_tuple(PidRef) ->
    maybe_track([PidRef], AT);
maybe_track([{Pid,_Ref} = PidRef | PidRefs], AT) ->
    AT1 = case maps:is_key(PidRef, AT) of
        true -> %% noop, we're already tracking this PidRef
            AT;
        false -> %% setup new monitor and double bookkeep refs
            Mon = erlang:monitor(process, Pid),
            %% TODO: decide whether we want the true match to crash this process on failure
            %%true = ets:update_element(?MODULE, PidRef, [{#rctx.mon_ref, Mon}]),
            ets:update_element(?MODULE, PidRef, [{#rctx.mon_ref, Mon}]),
            maps:put(Mon, PidRef, maps:put(PidRef, Mon, AT))
    end,
    maybe_track(PidRefs, AT1).

log_process_lifetime_report(PidRef) ->
    %% More safely assert this can't ever be undefined
    #rctx{} = Rctx = get_resource(PidRef),
    %% TODO: catch error out of here, report crashes on depth>1 json
    %%io:format("CSRT RCTX: ~p~n", [to_flat_json(Rctx)]),
    case is_enabled() andalso should_log(Rctx) of
        true ->
            couch_log:report("csrt-pid-usage-lifetime", to_flat_json(Rctx));
        false ->
            ok
    end.


logging_enabled() ->
    case conf_get("log_pid_usage_report", "coordinator") of
        "coordinator" ->
            coordinator;
        "true" ->
            true;
        _ ->
            false
    end.


should_log(#rctx{}=Rctx) ->
    should_log(Rctx, logging_enabled()).


should_log(#rctx{}, true) ->
    true;
should_log(#rctx{}, false) ->
    false;
should_log(#rctx{type = {coordinator, _, _}}, coordinator) ->
    true;
should_log(#rctx{type = {worker, fabric_rpc, FName}}, _) ->
    case conf_get("log_fabric_rpc") of
        "true" ->
            true;
        undefined ->
            false;
        Name ->
            Name =:= atom_to_list(FName)
    end;
should_log(#rctx{}, _) ->
    false.


conf_get(Key) ->
    conf_get(Key, undefined).


conf_get(Key, Default) ->
    config:get(?MODULE_STRING, Key, Default).


%% Reimplementation of: https://github.com/erlang/otp/blob/b2ee4fc9a0b81a139dad2033e9b2bfc178146886/lib/stdlib/src/ets.erl#L633-L658
%% with wrapping of ets:safe_fixtable/2 removed
unsafe_foldl(F, Accu, T) ->
    First = ets:first(T),
    do_foldl(F, Accu, First, T).

do_foldl(F, Accu0, Key, T) ->
    case Key of
        '$end_of_table' ->
            Accu0;
        _ ->
            do_foldl(F,
                lists:foldl(F, Accu0, ets:lookup(T, Key)),
                ets:next(T, Key), T)
    end.
