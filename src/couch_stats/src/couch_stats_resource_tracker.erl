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

%% PidRef API
-export([
    get_pid_ref/0,
    set_pid_ref/1,
    create_pid_ref/0,
    close_pid_ref/0, close_pid_ref/1
]).

%% Context API
-export([
    create_resource/1,
    create_context/5,
    create_coordinator_context/2,
    create_worker_context/3,
    destroy_context/0, destroy_context/1,

    get_resource/0, get_resource/1,

    set_context_dbname/1, set_context_dbname/2,
    set_context_handler_fun/1, set_context_handler_fun/2,
    set_context_username/1, set_context_username/2
]).

%% stats collection api
-export([
    is_enabled/0,

    inc/1, inc/2,
    maybe_inc/2,
    accumulate_delta/1,
    make_delta/0,

    ioq_called/0,

    should_track/1
]).

%% aggregate query api
-export([
    active/0, active/1,
    active_coordinators/0, active_coordinators/1,
    active_workers/0, active_workers/1,

    count_by/1,
    group_by/2, group_by/3,
    sorted/1,
    sorted_by/1, sorted_by/2, sorted_by/3,

    find_by_pid/1,
    find_by_pidref/1,
    find_by_nonce/1,
    find_workers_by_pidref/1
]).

%% Process lifetime reporting api
-export([
    log_process_lifetime_report/1,
    is_logging_enabled/0,
    logging_enabled/0,
    should_log/1, should_log/2,
    tracker/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% Module pdict markers
-define(DELTA_TA, csrt_delta_ta).
-define(DELTA_TZ, csrt_delta_tz). %% T Zed instead of T0
-define(PID_REF, csrt_pid_ref). %% track local ID
-define(TRACKER_PID, csrt_tracker). %% tracker pid

-define(MANGO_EVAL_MATCH, mango_eval_match).
-define(DB_OPEN_DOC, docs_read).
-define(DB_OPEN, db_open).
-define(COUCH_SERVER_OPEN, db_open).
-define(COUCH_BT_GET_KP_NODE, get_kp_node).
-define(COUCH_BT_GET_KV_NODE, get_kv_node).
-define(COUCH_BT_WRITE_KP_NODE, write_kp_node).
-define(COUCH_BT_WRITE_KV_NODE, write_kv_node).
-define(COUCH_JS_FILTER, js_filter).
-define(COUCH_JS_FILTERED_DOCS, js_filtered_docs).
-define(IOQ_CALLS, ioq_calls).
-define(ROWS_READ, rows_read).

%% TODO: overlap between this and couch btree fold invocations
%% TODO: need some way to distinguish fols on views vs find vs all_docs
-define(FRPC_CHANGES_ROW, changes_processed).
-define(FRPC_CHANGES_RETURNED, changes_returned).

-record(st, {}).

-record(rctx, {
    %% Metadata
    started_at = tnow(),
    updated_at = tnow(),
    pid_ref,
    mfa,
    nonce,
    from,
    type = unknown, %% unknown/background/system/rpc/coordinator/fabric_rpc/etc_rpc/etc
    dbname,
    username,
    path,

    %% Stats counters
    db_open = 0,
    docs_read = 0,
    rows_read = 0,
    changes_processed = 0,
    changes_returned = 0,
    ioq_calls = 0,
    io_bytes_read = 0,
    io_bytes_written = 0,
    js_evals = 0,
    js_filter = 0,
    js_filtered_docs = 0,
    mango_eval_match = 0,
    %% TODO: switch record definitions to be macro based, eg:
    %% ?COUCH_BT_GET_KP_NODE = 0,
    get_kv_node = 0,
    get_kp_node = 0,
    write_kv_node = 0,
    write_kp_node = 0
}).

%%
%% Public API
%%

%%
%% PidRef operations
%%

get_pid_ref() ->
    get(?PID_REF).

set_pid_ref(PidRef) ->
    erlang:put(?PID_REF, PidRef),
    PidRef.

create_pid_ref() ->
    case get_pid_ref() of
        undefined ->
            ok;
        PidRef0 ->
            %% TODO: what to do when it already exists?
            throw({epidexist, PidRef0}),
            close_pid_ref(PidRef0)
    end,
    PidRef = {self(), make_ref()},
    set_pid_ref(PidRef),
    PidRef.

close_pid_ref() ->
    close_pid_ref(get_pid_ref()).

%%close_pid_ref(undefined) ->
%%    undefined;
close_pid_ref(_PidRef) ->
    erase(?PID_REF).

get_resource() ->
    get_resource(get_pid_ref()).

get_resource(undefined) ->
    undefined;
get_resource(PidRef) ->
    catch case ets:lookup(?MODULE, PidRef) of
        [#rctx{}=Rctx] ->
            Rctx;
        [] ->
            undefined
    end.

%% monotonic time now in millisecionds
tnow() ->
    erlang:monotonic_time(millisecond).

is_enabled() ->
    config:get_boolean(?MODULE_STRING, "enabled", true).

%%
%% Aggregate query API
%%

active() -> active_int(all).
active_coordinators() -> active_int(coordinators).
active_workers() -> active_int(workers).

%% active_json() or active(json)?
active(json) -> to_json_list(active_int(all)).
active_coordinators(json) -> to_json_list(active_int(coordinators)).
active_workers(json) -> to_json_list(active_int(workers)).


active_int(coordinators) ->
    select_by_type(coordinators);
active_int(workers) ->
    select_by_type(workers);
active_int(all) ->
    select_by_type(all).


select_by_type(coordinators) ->
    ets:select(?MODULE, ets:fun2ms(fun(#rctx{type = {coordinator, _, _}} = R) -> R end));
select_by_type(workers) ->
    ets:select(?MODULE, ets:fun2ms(fun(#rctx{type = {worker, _, _}} = R) -> R end));
select_by_type(all) ->
    ets:tab2list(?MODULE).

find_by_nonce(Nonce) ->
    ets:match_object(?MODULE, ets:fun2ms(fun(#rctx{nonce = Nonce1} = R) when Nonce =:= Nonce1 -> R end)).

find_by_pid(Pid) ->
    [R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{pid_ref={Pid, '_'}, _ = '_'})].

find_by_pidref(PidRef) ->
    [R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{pid_ref=PidRef, _ = '_'})].

find_workers_by_pidref(PidRef) ->
    [R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{from=PidRef, _ = '_'})].

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
field(#rctx{dbname=Val}, dbname) -> Val;
field(#rctx{username=Val}, username) -> Val;
field(#rctx{path=Val}, path) -> Val;
field(#rctx{db_open=Val}, db_open) -> Val;
field(#rctx{docs_read=Val}, docs_read) -> Val;
field(#rctx{rows_read=Val}, rows_read) -> Val;
field(#rctx{changes_processed=Val}, changes_processed) -> Val;
field(#rctx{changes_returned=Val}, changes_returned) -> Val;
field(#rctx{ioq_calls=Val}, ioq_calls) -> Val;
field(#rctx{io_bytes_read=Val}, io_bytes_read) -> Val;
field(#rctx{io_bytes_written=Val}, io_bytes_written) -> Val;
field(#rctx{js_evals=Val}, js_evals) -> Val;
field(#rctx{js_filter=Val}, js_filter) -> Val;
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

%%
%% Conversion API for outputting JSON
%%

convert_mfa(MFA) when is_list(MFA)  ->
    list_to_binary(MFA);
convert_mfa({M0, F0, A0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    A = integer_to_binary(A0),
    <<M/binary, ":", F/binary, "/", A/binary>>;
convert_mfa(null) ->
    null;
convert_mfa(undefined) ->
    null.

convert_type(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
convert_type({coordinator, Verb0, Atom0}) when is_atom(Atom0) ->
    Verb = atom_to_binary(Verb0),
    Atom = atom_to_binary(Atom0),
    <<"coordinator:", Verb/binary, ":", Atom/binary>>;
convert_type({coordinator, Verb0, Path0}) ->
    Verb = atom_to_binary(Verb0),
    Path = list_to_binary(Path0),
    <<"coordinator:", Verb/binary, ":", Path/binary>>;
convert_type({worker, M0, F0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    <<"worker:", M/binary, ":", F/binary>>;
convert_type(null) ->
    null;
convert_type(undefined) ->
    null.

convert_path(undefined) ->
    null;
convert_path(Path) when is_binary(Path) ->
    Path.

convert_pidref({Parent0, ParentRef0}) ->
    Parent = convert_pid(Parent0),
    ParentRef = convert_ref(ParentRef0),
    <<Parent/binary, ":", ParentRef/binary>>;
convert_pidref(null) ->
    null;
convert_pidref(undefined) ->
    null.

convert_pid(Pid) when is_pid(Pid) ->
    ?l2b(pid_to_list(Pid)).

convert_ref(Ref) when is_reference(Ref) ->
    ?l2b(ref_to_list(Ref)).

to_json(#rctx{}=Rctx) ->
    #rctx{
        updated_at = TP,
        started_at = TInit,
        pid_ref = PidRef,
        mfa = MFA,
        nonce = Nonce,
        from = From,
        dbname = DbName,
        path = Path,
        username = UserName,
        db_open = DbOpens,
        docs_read = DocsRead,
        rows_read = RowsRead,
        js_filter = JSFilters,
        js_filtered_docs = JSFilteredDocss,
        type = Type,
        get_kp_node = GetKpNodes,
        get_kv_node = GetKvNodes,
        %%write_kp_node = WriteKpNodes,
        %%write_kv_node = WriteKvNodes,
        changes_returned = ChangesReturned,
        ioq_calls = IoqCalls
    } = Rctx,

    #{
        updated_at => TP,
        started_at => TInit,
        pid_ref => convert_pidref(PidRef),
        mfa => convert_mfa(MFA),
        nonce => Nonce,
        from => convert_pidref(From),
        dbname => DbName,
        path => convert_path(Path),
        username => UserName,
        db_open => DbOpens,
        docs_read => DocsRead,
        js_filter => JSFilters,
        js_filtered_docs => JSFilteredDocss,
        rows_read => RowsRead,
        type => convert_type(Type),
        get_kp_nodes => GetKpNodes,
        get_kv_nodes => GetKvNodes,
        %%write_kp_nodes => WriteKpNodes,
        %%write_kv_nodes => WriteKvNodes,
        changes_returned => ChangesReturned,
        ioq_calls => IoqCalls
    }.

%%
%% Context lifecycle API
%%

create_resource(#rctx{} = Rctx) ->
    catch ets:insert(?MODULE, Rctx).

create_worker_context(From, {M,F,_A} = MFA, Nonce) ->
    case is_enabled() of
        true ->
            create_context(MFA, {worker, M, F}, null, From, Nonce);
        false ->
            false
    end.

create_coordinator_context(#httpd{} = Req, Path0) ->
    case is_enabled() of
        true ->
            #httpd{
                method = Verb,
                nonce = Nonce
                %%path_parts = Parts
            } = Req,
            %%Path = list_to_binary([$/ | io_lib:format("~p", [Parts])]),
            Path = list_to_binary([$/ | Path0]),
            Type = {coordinator, Verb, init},
            create_context(null, Type, Path, null, Nonce);
        false ->
            false
    end.

create_context(MFA, Type, Path, From, Nonce) ->
    PidRef = create_pid_ref(),
    Rctx = #rctx{
        from = From,
        pid_ref = PidRef,
        mfa = MFA,
        nonce = Nonce,
        path = Path,
        type = Type
    },
    erlang:put(?DELTA_TZ, Rctx),
    create_resource(Rctx),
    track(Rctx),
    PidRef.

set_context_dbname(DbName) ->
    set_context_dbname(DbName, get_pid_ref()).

set_context_dbname(_, undefined) ->
    ok;
set_context_dbname(DbName, PidRef) ->
    is_enabled() andalso update_element(PidRef, [{#rctx.dbname, DbName}]).

set_context_handler_fun(Fun) when is_function(Fun) ->
    set_context_handler_fun(Fun, get_pid_ref()).
set_context_handler_fun(_, undefined) ->
    ok;
set_context_handler_fun(Fun, PidRef) when is_function(Fun) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            FunName = erlang:fun_to_list(Fun),
            #rctx{type={coordinator, Verb, _}} = get_resource(),
            Update = [{#rctx.type, {coordinator, Verb, FunName}}],
            update_element(PidRef, Update)
    end.

set_context_username(null) ->
    ok;
set_context_username(undefined) ->
    ok;
set_context_username(User) ->
    set_context_username(User, get_pid_ref()).

set_context_username(null, _) ->
    ok;
set_context_username(_, undefined) ->
    ok;
set_context_username(#httpd{user_ctx = Ctx}, PidRef) ->
    set_context_username(Ctx, PidRef);
set_context_username(#user_ctx{name = Name}, PidRef) ->
    set_context_username(Name, PidRef);
set_context_username(UserName, PidRef) ->
    is_enabled() andalso update_element(PidRef, [{#rctx.username, UserName}]).

destroy_context() ->
    destroy_context(get_pid_ref()).

destroy_context(undefined) ->
    ok;
destroy_context({_, _} = PidRef) ->
    stop_tracker(get_tracker()),
    close_pid_ref(PidRef),
    ok.

%% Stat collection API

inc(Key) ->
    inc(Key, 1).

%% TODO: inc(io_bytes_read, N) ->
%% TODO: inc(io_bytes_written, N) ->
%% TODO: inc(js_evals, N) ->
inc(?DB_OPEN, N) ->
    update_counter(#rctx.?DB_OPEN, N);
inc(?ROWS_READ, N) ->
    update_counter(#rctx.?ROWS_READ, N);
inc(?FRPC_CHANGES_RETURNED, N) ->
    update_counter(#rctx.?FRPC_CHANGES_RETURNED, N);
inc(?IOQ_CALLS, N) ->
    update_counter(#rctx.?IOQ_CALLS, N);
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
maybe_inc([couchdb, btree, get_node, kp_node], Val) ->
    inc(?COUCH_BT_GET_KP_NODE, Val);
maybe_inc([couchdb, btree, get_node, kv_node], Val) ->
    inc(?COUCH_BT_GET_KV_NODE, Val);
%% The write_node logic won't pickup writes as none of the RPC
%% processes actually perform the write operation
%% TODO: bubble up induced work from other processes
maybe_inc([couchdb, btree, write_node, kp_node], Val) ->
    inc(?COUCH_BT_WRITE_KP_NODE, Val);
maybe_inc([couchdb, btree, write_node, kv_node], Val) ->
    inc(?COUCH_BT_WRITE_KV_NODE, Val);
maybe_inc([couchdb, query_server, calls, ddoc_filter], Val) ->
    inc(?COUCH_JS_FILTER, Val);
maybe_inc([couchdb, query_server, volume, ddoc_filter], Val) ->
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

ioq_called() ->
    inc(ioq_calls).

accumulate_delta(Delta) when is_map(Delta) ->
    %% TODO: switch to creating a batch of updates to invoke a single
    %% update_counter rather than sequentially invoking it for each field
    is_enabled() andalso maps:foreach(fun inc/2, Delta);
accumulate_delta(undefined) ->
    ok.

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
        js_filtered_docs => TB#rctx.js_filtered_docs - TA#rctx.js_filtered_docs,
        rows_read => TB#rctx.rows_read - TA#rctx.rows_read,
        changes_returned => TB#rctx.changes_returned - TA#rctx.changes_returned,
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

%% TODO: what to do when PidRef=undefined?
make_delta_base(PidRef) ->
    %% TODO: extract user_ctx and db/shard from request
    Now = tnow(),
    #rctx{
        pid_ref = PidRef,
        %% TODO: confirm this subtraction works
        started_at = Now - 100, %% give us 100ms rewind time for missing T0
        updated_at = Now
    }.

make_delta_base() ->
    make_delta_base(get_pid_ref()).

set_delta_a(TA) ->
    erlang:put(?DELTA_TA, TA).

update_counter(Field, Count) ->
    is_enabled() andalso update_counter(get_pid_ref(), Field, Count).

update_counter(undefined, _Field, _Count) ->
    ok;
update_counter({_Pid,_Ref}=PidRef, Field, Count) ->
    %% TODO: mem3 crashes without catch, why do we lose the stats table?
    is_enabled() andalso catch ets:update_counter(?MODULE, PidRef, {Field, Count}, #rctx{pid_ref=PidRef}).

update_element(undefined, _Update) ->
    ok;
update_element({_Pid,_Ref}=PidRef, Update) ->
    %% TODO: should we take any action when the update fails?
    is_enabled() andalso catch ets:update_element(?MODULE, PidRef, Update).

%% Process lifetime logging api

track(#rctx{pid_ref=PidRef}) ->
    case get_tracker() of
        undefined ->
            Pid = spawn(?MODULE, tracker, [PidRef]),
            put_tracker(Pid),
            Pid;
        Pid when is_pid(Pid) ->
            Pid
    end.

tracker({Pid, _Ref}=PidRef) ->
    MonRef = erlang:monitor(process, Pid),
    receive
        stop ->
            %% TODO: do we need cleanup here?
            log_process_lifetime_report(PidRef),
            catch evict(PidRef),
            demonitor(MonRef),
            ok;
        {'DOWN', MonRef, _Type, _0DPid, _Reason0} ->
            destroy_context(PidRef),
            %% TODO: should we pass reason to log_process_lifetime_report?
            %% Reason = case Reason0 of
            %%     {shutdown, Shutdown0} ->
            %%         Shutdown = atom_to_binary(Shutdown0),
            %%         <<"shutdown: ", Shutdown/binary>>;
            %%     Reason0 ->
            %%         Reason0
            %% end,
            %% TODO: should we send the induced work delta to the coordinator?
            log_process_lifetime_report(PidRef),
            catch evict(PidRef)
    end.

log_process_lifetime_report(PidRef) ->
    case is_enabled() andalso is_logging_enabled() of
        true ->
            Rctx = get_resource(PidRef),
            case should_log(Rctx) of
               true ->
                    couch_log:report("csrt-pid-usage-lifetime", to_json(Rctx));
                _ ->
                    ok
            end;
        false ->
            ok
    end.

is_logging_enabled() ->
    logging_enabled() =/= false.

logging_enabled() ->
    case conf_get("log_pid_usage_report", "coordinator") of
        "coordinator" ->
            coordinator;
        "true" ->
            true;
        _ ->
            false
    end.

should_log(undefined) ->
    false;
should_log(#rctx{}=Rctx) ->
    should_log(Rctx, logging_enabled()).

should_log(undefined, _) ->
    false;
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

%%
%% gen_server callbacks
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE, [
        named_table,
        public,
        {decentralized_counters, true},
        {write_concurrency, true},
        {read_concurrency, true},
        {keypos, #rctx.pid_ref}
    ]),
    {ok, #st{}}.

handle_call(fetch, _from, #st{} = St) ->
    {reply, {ok, St}, St};
handle_call({call_search, _}, _From, St) ->
    %% TODO: provide isolated search queries here
    {reply, ok, St};
handle_call(Msg, _From, St) ->
    {stop, {unknown_call, Msg}, St}.

handle_cast(Msg, St) ->
    {stop, {unknown_cast, Msg}, St}.

handle_info(Msg, St) ->
    {stop, {unknown_info, Msg}, St}.

terminate(_Reason, _St) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%
%% private functions
%%

conf_get(Key) ->
    conf_get(Key, undefined).


conf_get(Key, Default) ->
    config:get(?MODULE_STRING, Key, Default).

to_json_list(List) when is_list(List) ->
    lists:map(fun to_json/1, List).

%%
%% Process lifetime logging api
%%

get_tracker() ->
    get(?TRACKER_PID).

put_tracker(Pid) when is_pid(Pid) ->
    put(?TRACKER_PID, Pid).

evict(PidRef) ->
    ets:delete(?MODULE, PidRef).

stop_tracker(undefined) ->
    ok;
stop_tracker(Pid) when is_pid(Pid) ->
    Pid ! stop.

