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

%% TODO: delete these exports after csrt_logger experiment concluded
-export([
    new/0,
    tnow/0
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2
]).

%% PidRef API
-export([
    close_pid_ref/0,
    close_pid_ref/1,
    create_pid_ref/0,
    get_pid_ref/0,
    set_pid_ref/1
]).

%% Context API
-export([
    create_context/2,
    create_coordinator_context/2,
    create_resource/1,
    create_worker_context/3,
    destroy_context/0,
    destroy_context/1,
    get_resource/0,
    get_resource/1,
    set_context_dbname/1,
    set_context_dbname/2,
    set_context_handler_fun/1,
    set_context_handler_fun/2,
    set_context_handler_fun/3,
    set_context_username/1,
    set_context_username/2
]).

%% stats collection api
-export([
    accumulate_delta/1,
    inc/1,
    inc/2,
    ioq_called/0,
    is_enabled/0,
    make_delta/0,
    maybe_inc/2,
    should_track/1
]).

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

%% Process lifetime reporting api
-export([
    is_logging_enabled/0,
    log_process_lifetime_report/1,
    logging_enabled/0,
    should_log/1,
    should_log/2,
    tracker/1,
    to_json/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

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

-define(STATS_TO_KEYS, #{
    [mango, evaluate_selector] => ?MANGO_EVAL_MATCH,
    [couchdb, database_reads] => ?DB_OPEN_DOC,
    [fabric_rpc, changes, processed] => ?FRPC_CHANGES_ROW,
    [fabric_rpc, changes, returned] => ?FRPC_CHANGES_RETURNED,
    [fabric_rpc, view, rows_read] => ?ROWS_READ,
    [couchdb, couch_server, open] => ?DB_OPEN,
    [couchdb, btree, get_node, kp_node] => ?COUCH_BT_GET_KP_NODE,
    [couchdb, btree, get_node, kv_node] => ?COUCH_BT_GET_KV_NODE,
    [couchdb, btree, write_node, kp_node] => ?COUCH_BT_WRITE_KP_NODE,
    [couchdb, btree, write_node, kv_node] => ?COUCH_BT_WRITE_KV_NODE,
    [couchdb, query_server, calls, ddoc_filter] => ?COUCH_JS_FILTER,
    [couchdb, query_server, volume, ddoc_filter] => ?COUCH_JS_FILTERED_DOCS
}).

-define(KEYS_TO_FIELDS, #{
    ?DB_OPEN => #rctx.?DB_OPEN,
    ?ROWS_READ => #rctx.?ROWS_READ,
    ?FRPC_CHANGES_RETURNED => #rctx.?FRPC_CHANGES_RETURNED,
    ?IOQ_CALLS => #rctx.?IOQ_CALLS,
    ?COUCH_JS_FILTER => #rctx.?COUCH_JS_FILTER,
    ?COUCH_JS_FILTERED_DOCS => #rctx.?COUCH_JS_FILTERED_DOCS,
    ?MANGO_EVAL_MATCH => #rctx.?MANGO_EVAL_MATCH,
    ?DB_OPEN_DOC => #rctx.?DB_OPEN_DOC,
    ?FRPC_CHANGES_ROW => #rctx.?ROWS_READ, %% TODO: rework double use of rows_read
    ?COUCH_BT_GET_KP_NODE => #rctx.?COUCH_BT_GET_KP_NODE,
    ?COUCH_BT_GET_KV_NODE => #rctx.?COUCH_BT_GET_KV_NODE,
    ?COUCH_BT_WRITE_KP_NODE => #rctx.?COUCH_BT_WRITE_KP_NODE,
    ?COUCH_BT_WRITE_KV_NODE => #rctx.?COUCH_BT_WRITE_KV_NODE
}).

-record(st, {}).

%%
%% Public API
%%

%%
%% PidRef operations
%%

new() -> #rctx{}.

get_pid_ref() ->
    get(?PID_REF).

set_pid_ref(PidRef) ->
    erlang:put(?PID_REF, PidRef),
    PidRef.

create_pid_ref() ->
    {self(), make_ref()}.

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
    ets:match_object(?MODULE, ets:fun2ms(fun(#rctx{nonce = Nonce1} = R) when Nonce =:= Nonce1 -> R end)).

find_by_pid(Pid) ->
    [R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{pid_ref={Pid, '_'}, _ = '_'})].

find_by_pidref(PidRef) ->
    [R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{pid_ref=PidRef, _ = '_'})].

find_workers_by_pidref(PidRef) ->
    [R || #rctx{} = R <- ets:match_object(?MODULE, #rctx{type=#rpc_worker{from=PidRef}, _ = '_'})].

field(#rctx{pid_ref=Val}, pid_ref) -> Val;
%% NOTE: Pros and cons to doing these convert functions here
%% Ideally, this would be done later so as to prefer the core data structures
%% as long as possible, but we currently need the output of this function to
%% be jiffy:encode'able. The tricky bit is dynamically encoding the group_by
%% structure provided by the caller of *_by aggregator functions below.
%% For now, we just always return jiffy:encode'able data types.
field(#rctx{nonce=Val}, nonce) -> Val;
%%field(#rctx{from=Val}, from) -> Val;
field(#rctx{type=Val}, type) -> convert_type(Val);
field(#rctx{dbname=Val}, dbname) -> Val;
field(#rctx{username=Val}, username) -> Val;
%%field(#rctx{path=Val}, path) -> Val;
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

convert_type(#coordinator{method=Verb0, path=Path, mod=M0, func=F0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    Verb = atom_to_binary(Verb0),
    <<"coordinator-{", M/binary, ":", F/binary, "}:", Verb/binary, ":", Path/binary>>;
convert_type(#rpc_worker{mod=M0, func=F0, from=From0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    From = convert_pidref(From0),
    <<"rpc_worker-{", From/binary, "}:", M/binary, ":", F/binary>>;
convert_type(undefined) ->
    null.

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
    #{
        updated_at => Rctx#rctx.updated_at,
        started_at => Rctx#rctx.started_at,
        pid_ref => convert_pidref(Rctx#rctx.pid_ref),
        nonce => Rctx#rctx.nonce,
        dbname => Rctx#rctx.dbname,
        username => Rctx#rctx.username,
        db_open => Rctx#rctx.db_open,
        docs_read => Rctx#rctx.docs_read,
        js_filter => Rctx#rctx.js_filter,
        js_filtered_docs => Rctx#rctx.js_filtered_docs,
        rows_read => Rctx#rctx.rows_read,
        type => convert_type(Rctx#rctx.type),
        get_kp_node => Rctx#rctx.get_kp_node,
        get_kv_node => Rctx#rctx.get_kv_node,
        write_kp_node => Rctx#rctx.write_kp_node,
        write_kv_node => Rctx#rctx.write_kv_node,
        changes_returned => Rctx#rctx.changes_returned,
        ioq_calls => Rctx#rctx.ioq_calls
    }.

%%
%% Context lifecycle API
%%

create_resource(#rctx{} = Rctx) ->
    catch ets:insert(?MODULE, Rctx).

create_worker_context(From, {M,F,_A}, Nonce) ->
    case is_enabled() of
        true ->
            Type = #rpc_worker{from=From, mod=M, func=F},
            create_context(Type, Nonce);
        false ->
            false
    end.

create_coordinator_context(#httpd{method=Verb, nonce=Nonce}, Path0) ->
    case is_enabled() of
        true ->
            Path = list_to_binary([$/ | Path0]),
            Type = #coordinator{method=Verb, path=Path},
            create_context(Type, Nonce);
        false ->
            false
    end.

create_context(Type, Nonce) ->
    Rctx = new_context(Type, Nonce),
    set_pid_ref(Rctx#rctx.pid_ref),
    erlang:put(?DELTA_TZ, Rctx),
    create_resource(Rctx),
    track(Rctx),
    Rctx#rctx.pid_ref.

%% Might be useful to export this but the internal worker types aren't exported
new_context(Type, Nonce) ->
    #rctx{
       nonce = Nonce,
       pid_ref = create_pid_ref(),
       type = Type
    }.

set_context_dbname(DbName) ->
    set_context_dbname(DbName, get_pid_ref()).

set_context_dbname(_, undefined) ->
    ok;
set_context_dbname(DbName, PidRef) ->
    is_enabled() andalso update_element(PidRef, [{#rctx.dbname, DbName}]).

set_context_handler_fun(Fun) when is_function(Fun) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            FProps = erlang:fun_info(Fun),
            Mod = proplists:get_value(module, FProps),
            Func = proplists:get_value(name, FProps),
            set_context_handler_fun(Mod, Func)
    end.

set_context_handler_fun(Mod, Func) when is_atom(Mod) andalso is_atom(Func) ->
    set_context_handler_fun(Mod, Func, get_pid_ref()).

set_context_handler_fun(_, _, undefined) ->
    ok;
set_context_handler_fun(Mod, Func, PidRef) when is_atom(Mod) andalso is_atom(Func) ->
    case is_enabled() of
        false ->
            ok;
        true ->
            #rctx{type=#coordinator{}=Coordinator} = get_resource(),
            Update = [{#rctx.type, Coordinator#coordinator{mod=Mod, func=Func}}],
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

inc(Key, N) ->
    case maps:is_key(Key, ?KEYS_TO_FIELDS) of
        true ->
            update_counter(maps:get(Key, ?KEYS_TO_FIELDS), N);
        false ->
            0
    end.


maybe_inc(Stat, Val) ->
    case maps:is_key(Stat, ?STATS_TO_KEYS) of
        true ->
            inc(maps:get(Stat, ?STATS_TO_KEYS), Val);
        false ->
            0
    end.

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
            ok;
        {'DOWN', MonRef, _Type, _0DPid, _Reason0} ->
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
            case conf_get("logger_type", "csrt_logger") of
                "csrt_logger" ->
                    csrt_logger:maybe_report(PidRef);
                _ ->
                    Rctx = get_resource(PidRef),
                    case should_log(Rctx) of
                       true ->
                            couch_log:report("csrt-pid-usage-lifetime", to_json(Rctx));
                        _ ->
                            ok
                    end
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
should_log(#rctx{type = #coordinator{}}, coordinator) ->
    true;
should_log(#rctx{type = #rpc_worker{mod=fabric_rpc, func=FName}}, _) ->
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

handle_call({call_search, _}, _From, St) ->
    %% TODO: provide isolated search queries here
    {reply, ok, St};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State, 0}.

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


-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_stats_resource_tracker_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_static_map_translations)
        ]
    }.

setup() ->
    test_util:start_couch().

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_static_map_translations(_) ->
    ?assert(lists:all(fun(E) -> maps:is_key(E, ?KEYS_TO_FIELDS) end, maps:values(?STATS_TO_KEYS))),
    %% TODO: properly handle ioq_calls field
    ?assertEqual(lists:sort(maps:values(?STATS_TO_KEYS)), lists:delete(ioq_calls, lists:sort(maps:keys(?KEYS_TO_FIELDS)))).

-endif.
