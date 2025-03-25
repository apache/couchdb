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

-module(csrt_util).

-export([
    is_enabled/0,
    is_enabled_init_p/0,
    get_pid_ref/0,
    get_pid_ref/1,
    set_pid_ref/1,
    should_track_init_p/2,
    tnow/0,
    tutc/0,
    tutc/1
]).

%% JSON Conversion API
-export([
    convert_type/1,
    convert_pidref/1,
    convert_pid/1,
    convert_ref/1,
    to_json/1
]).

%% Delta API
-export([
    add_delta/2,
    extract_delta/1,
    get_delta/1,
    get_delta_a/0,
    get_delta_zero/0,
    maybe_add_delta/1,
    maybe_add_delta/2,
    make_delta/1,
    make_dt/2,
    make_dt/3,
    rctx_delta/2,
    set_delta_a/1,
    set_delta_zero/1
]).

%% Extra niceties and testing facilities
-export([
    set_fabric_init_p/2,
    set_fabric_init_p/3,
    map_to_rctx/1,
    field/2
]).


-include_lib("couch_stats_resource_tracker.hrl").

-spec is_enabled() -> boolean().
is_enabled() ->
    config:get_boolean(?CSRT, "enabled", true).

-spec is_enabled_init_p() -> boolean().
is_enabled_init_p() ->
    config:get_boolean(?CSRT_INIT_P, "enabled", true).

-spec should_track_init_p(Mod :: atom(), Func :: atom()) -> boolean().
should_track_init_p(fabric_rpc, Func) ->
    config:get_boolean(?CSRT_INIT_P, fabric_conf_key(Func), false);
should_track_init_p(_Mod, _Func) ->
    false.

%% Monotnonic time now in native format using time forward only event tracking
-spec tnow() -> integer().
tnow() ->
    erlang:monotonic_time().

%% Get current system time in UTC RFC 3339 format
-spec tutc() -> calendar:rfc3339_string().
tutc() ->
    tutc(tnow()).

%% Convert a integer system time in milliseconds into UTC RFC 3339 format
-spec tutc(Time :: integer()) -> calendar:rfc3339_string().
tutc(Time0) when is_integer(Time0) ->
    Unit = millisecond,
    Time1 = Time0 + erlang:time_offset(),
    Time = erlang:convert_time_unit(Time1, native, Unit),
    calendar:system_time_to_rfc3339(Time, [{unit, Unit}, {offset, "z"}]).

%% Returns dt (delta time) in microseconds
%% @equiv make_dt(A, B, microsecond)
-spec make_dt(A, B) -> pos_integer() when
    A :: integer(),
    B :: integer().
make_dt(A, B) ->
    make_dt(A, B, microsecond).

%% Returns monotonic dt (delta time) in specified time_unit()
-spec make_dt(A, B, Unit) -> pos_integer() when
    A :: integer(),
    B :: integer(),
    Unit :: erlang:time_unit().
make_dt(A, A, _Unit) when is_integer(A) ->
    %% Handle edge case when monotonic_time()'s are equal
    %% Always return a non zero value so we don't divide by zero
    %% This always returns 1, independent of unit, as that's the smallest
    %% possible positive integer value delta.
    1;
make_dt(A, B, Unit) when is_integer(A) andalso is_integer(B) andalso B > A ->
    A1 = erlang:convert_time_unit(A, native, Unit),
    B1 = erlang:convert_time_unit(B, native, Unit),
    B1 - A1.

%%
%% Conversion API for outputting JSON
%%

-spec convert_type(T) -> binary() | null when
    T :: #coordinator{} | #rpc_worker{} | undefined.
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

-spec convert_pidref(PidRef) -> binary() | null when
    PidRef :: {A :: pid(), B :: reference()} | undefined.
convert_pidref({Parent0, ParentRef0}) ->
    Parent = convert_pid(Parent0),
    ParentRef = convert_ref(ParentRef0),
    <<Parent/binary, ":", ParentRef/binary>>;
%%convert_pidref(null) ->
%%    null;
convert_pidref(undefined) ->
    null.

-spec convert_pid(Pid :: pid()) -> binary().
convert_pid(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).

-spec convert_ref(Ref :: reference()) -> binary().
convert_ref(Ref) when is_reference(Ref) ->
    list_to_binary(ref_to_list(Ref)).

-spec to_json(Rctx :: rctx()) -> map().
to_json(#rctx{}=Rctx) ->
    #{
        updated_at => tutc(Rctx#rctx.updated_at),
        started_at => tutc(Rctx#rctx.started_at),
        pid_ref => convert_pidref(Rctx#rctx.pid_ref),
        nonce => Rctx#rctx.nonce,
        dbname => Rctx#rctx.dbname,
        username => Rctx#rctx.username,
        db_open => Rctx#rctx.db_open,
        docs_read => Rctx#rctx.docs_read,
        docs_written => Rctx#rctx.docs_written,
        js_filter => Rctx#rctx.js_filter,
        js_filtered_docs => Rctx#rctx.js_filtered_docs,
        rows_read => Rctx#rctx.rows_read,
        type => convert_type(Rctx#rctx.type),
        get_kp_node => Rctx#rctx.get_kp_node,
        get_kv_node => Rctx#rctx.get_kv_node,
        write_kp_node => Rctx#rctx.write_kp_node,
        write_kv_node => Rctx#rctx.write_kv_node,
        changes_returned => Rctx#rctx.changes_returned,
        changes_processed => Rctx#rctx.changes_processed,
        ioq_calls => Rctx#rctx.ioq_calls
    }.

%% NOTE: this does not do the inverse of to_json, should it conver types?
-spec map_to_rctx(Map :: map()) -> rctx().
map_to_rctx(Map) ->
    maps:fold(fun map_to_rctx_field/3, #rctx{}, Map).

-spec map_to_rctx_field(Field :: rctx_field(), Val :: any(), Rctx :: rctx()) -> rctx().
map_to_rctx_field(updated_at, Val, Rctx) ->
    Rctx#rctx{updated_at = Val};
map_to_rctx_field(started_at, Val, Rctx) ->
    Rctx#rctx{started_at = Val};
map_to_rctx_field(pid_ref, Val, Rctx) ->
    Rctx#rctx{pid_ref = Val};
map_to_rctx_field(nonce, Val, Rctx) ->
    Rctx#rctx{nonce = Val};
map_to_rctx_field(dbname, Val, Rctx) ->
    Rctx#rctx{dbname = Val};
map_to_rctx_field(username, Val, Rctx) ->
    Rctx#rctx{username = Val};
map_to_rctx_field(db_open, Val, Rctx) ->
    Rctx#rctx{db_open = Val};
map_to_rctx_field(docs_read, Val, Rctx) ->
    Rctx#rctx{docs_read = Val};
map_to_rctx_field(docs_written, Val, Rctx) ->
    Rctx#rctx{docs_written = Val};
map_to_rctx_field(js_filter, Val, Rctx) ->
    Rctx#rctx{js_filter = Val};
map_to_rctx_field(js_filtered_docs, Val, Rctx) ->
    Rctx#rctx{js_filtered_docs = Val};
map_to_rctx_field(rows_read, Val, Rctx) ->
    Rctx#rctx{rows_read = Val};
map_to_rctx_field(type, Val, Rctx) ->
    Rctx#rctx{type = Val};
map_to_rctx_field(get_kp_node, Val, Rctx) ->
    Rctx#rctx{get_kp_node = Val};
map_to_rctx_field(get_kv_node, Val, Rctx) ->
    Rctx#rctx{get_kv_node = Val};
map_to_rctx_field(write_kp_node, Val, Rctx) ->
    Rctx#rctx{write_kp_node = Val};
map_to_rctx_field(write_kv_node, Val, Rctx) ->
    Rctx#rctx{write_kv_node = Val};
map_to_rctx_field(changes_returned, Val, Rctx) ->
    Rctx#rctx{changes_returned = Val};
map_to_rctx_field(changes_processed, Val, Rctx) ->
    Rctx#rctx{changes_processed = Val};
map_to_rctx_field(ioq_calls, Val, Rctx) ->
    Rctx#rctx{ioq_calls = Val}.

-spec field(Field :: rctx_field(), Rctx :: rctx()) -> any().
field(updated_at, #rctx{updated_at = Val}) ->
    Val;
field(started_at, #rctx{started_at = Val}) ->
    Val;
field(pid_ref, #rctx{pid_ref = Val}) ->
    Val;
field(nonce, #rctx{nonce = Val}) ->
    Val;
field(dbname, #rctx{dbname = Val}) ->
    Val;
field(username, #rctx{username = Val}) ->
    Val;
field(db_open, #rctx{db_open = Val}) ->
    Val;
field(docs_read, #rctx{docs_read = Val}) ->
    Val;
field(docs_written, #rctx{docs_written = Val}) ->
    Val;
field(js_filter, #rctx{js_filter = Val}) ->
    Val;
field(js_filtered_docs, #rctx{js_filtered_docs = Val}) ->
    Val;
field(rows_read, #rctx{rows_read = Val}) ->
    Val;
field(type, #rctx{type = Val}) ->
    Val;
field(get_kp_node, #rctx{get_kp_node = Val}) ->
    Val;
field(get_kv_node, #rctx{get_kv_node = Val}) ->
    Val;
field(changes_returned, #rctx{changes_returned = Val}) ->
    Val;
field(changes_processed, #rctx{changes_processed = Val}) ->
    Val;
field(ioq_calls, #rctx{ioq_calls = Val}) ->
    Val.

add_delta({A}, Delta) -> {A, Delta};
add_delta({A, B}, Delta) -> {A, B, Delta};
add_delta({A, B, C}, Delta) -> {A, B, C, Delta};
add_delta({A, B, C, D}, Delta) -> {A, B, C, D, Delta};
add_delta({A, B, C, D, E}, Delta) -> {A, B, C, D, E, Delta};
add_delta({A, B, C, D, E, F}, Delta) -> {A, B, C, D, E, F, Delta};
add_delta({A, B, C, D, E, F, G}, Delta) -> {A, B, C, D, E, F, G, Delta};
add_delta(T, _Delta) -> T.

extract_delta({A, {delta, Delta}}) -> {{A}, Delta};
extract_delta({A, B, {delta, Delta}}) -> {{A, B}, Delta};
extract_delta({A, B, C, {delta, Delta}}) -> {{A, B, C}, Delta};
extract_delta({A, B, C, D, {delta, Delta}}) -> {{A, B, C, D}, Delta};
extract_delta({A, B, C, D, E, {delta, Delta}}) -> {{A, B, C, D, E}, Delta};
extract_delta({A, B, C, D, E, F, {delta, Delta}}) -> {{A, B, C, D, E, F}, Delta};
extract_delta({A, B, C, D, E, F, G, {delta, Delta}}) -> {{A, B, C, D, E, F, G}, Delta};
extract_delta(T) -> {T, undefined}.

-spec get_delta(PidRef :: maybe_pid_ref()) -> tagged_delta().
get_delta(PidRef) ->
    {delta, make_delta(PidRef)}.

maybe_add_delta(T) ->
    case is_enabled() of
        false ->
            T;
        true ->
            maybe_add_delta_int(T, get_delta(get_pid_ref()))
    end.

%% Allow for externally provided Delta in error handling scenarios
%% eg in cases like rexi_server:notify_caller/3
maybe_add_delta(T, Delta) ->
    case is_enabled() of
        false ->
            T;
        true ->
            maybe_add_delta_int(T, Delta)
    end.

maybe_add_delta_int(T, undefined) ->
    T;
maybe_add_delta_int(T, Delta) when is_map(Delta) ->
    maybe_add_delta_int(T, {delta, Delta});
maybe_add_delta_int(T, {delta, _} = Delta) ->
    add_delta(T, Delta).

-spec make_delta(PidRef :: maybe_pid_ref()) -> maybe_delta().
make_delta(undefined) ->
    undefined;
make_delta(PidRef) ->
    TA = get_delta_a(),
    TB = csrt_server:get_resource(PidRef),
    Delta = rctx_delta(TA, TB),
    set_delta_a(TB),
    Delta.

-spec rctx_delta(TA :: Rctx, TB :: Rctx) -> map().
rctx_delta(#rctx{}=TA, #rctx{}=TB) ->
    Delta = #{
        docs_read => TB#rctx.docs_read - TA#rctx.docs_read,
        docs_written => TB#rctx.docs_written - TA#rctx.docs_written,
        js_filter => TB#rctx.js_filter - TA#rctx.js_filter,
        js_filtered_docs => TB#rctx.js_filtered_docs - TA#rctx.js_filtered_docs,
        rows_read => TB#rctx.rows_read - TA#rctx.rows_read,
        changes_returned => TB#rctx.changes_returned - TA#rctx.changes_returned,
        changes_processed => TB#rctx.changes_processed - TA#rctx.changes_processed,
        get_kp_node => TB#rctx.get_kp_node - TA#rctx.get_kp_node,
        get_kv_node => TB#rctx.get_kv_node - TA#rctx.get_kv_node,
        db_open => TB#rctx.db_open - TA#rctx.db_open,
        ioq_calls => TB#rctx.ioq_calls - TA#rctx.ioq_calls,
        dt => make_dt(TA#rctx.updated_at, TB#rctx.updated_at)
    },
    %% TODO: reevaluate this decision
    %% Only return non zero (and also positive) delta fields
    %% NOTE: this can result in Delta's of the form #{dt => 1}
    maps:filter(fun(_K,V) -> V > 0 end, Delta);
rctx_delta(_, _) ->
    undefined.

-spec get_delta_a() -> maybe_rctx().
get_delta_a() ->
    erlang:get(?DELTA_TA).

-spec get_delta_zero() -> maybe_rctx().
get_delta_zero() ->
    erlang:get(?DELTA_TZ).

-spec set_delta_a(TA :: rctx()) -> maybe_rctx().
set_delta_a(TA) ->
    erlang:put(?DELTA_TA, TA).

-spec set_delta_zero(TZ :: rctx()) -> maybe_rctx().
set_delta_zero(TZ) ->
    erlang:put(?DELTA_TZ, TZ).

-spec get_pid_ref() -> maybe_pid_ref().
get_pid_ref() ->
    get(?PID_REF).

-spec get_pid_ref(Rctx :: rctx()) -> pid_ref().
get_pid_ref(#rctx{pid_ref=PidRef}) ->
    PidRef;
get_pid_ref(R) ->
    throw({unexpected, R}).

-spec set_pid_ref(PidRef :: pid_ref()) -> pid_ref().
set_pid_ref(PidRef) ->
    erlang:put(?PID_REF, PidRef),
    PidRef.

%% @equiv set_fabric_init_p(Func, Enabled, true).
-spec set_fabric_init_p(Func :: atom(), Enabled :: boolean()) -> ok.
set_fabric_init_p(Func, Enabled) ->
    set_fabric_init_p(Func, Enabled, true).

%% Expose Persist for use in test cases outside this module
-spec set_fabric_init_p(Func, Enabled, Persist) -> ok when
        Func :: atom(), Enabled :: boolean(), Persist :: boolean().
set_fabric_init_p(Func, Enabled, Persist) ->
    Key = fabric_conf_key(Func),
    ok = config:set_boolean(?CSRT_INIT_P, Key, Enabled, Persist).

-spec fabric_conf_key(Key :: atom()) -> string().
fabric_conf_key(Key) ->
    %% Double underscore to separate Mod and Func
    "fabric_rpc__" ++ atom_to_list(Key).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_stats_resource_tracker_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_should_track_init_p),
            ?TDEF_FE(t_should_track_init_p_empty),
            ?TDEF_FE(t_should_track_init_p_disabled),
            ?TDEF_FE(t_should_not_track_init_p)
        ]
    }.

setup() ->
    test_util:start_couch().

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_should_track_init_p(_) ->
    enable_init_p(),
    [?assert(should_track_init_p(M, F), {M, F}) || [M, F] <- base_metrics()].

t_should_track_init_p_empty(_) ->
    config:set(?CSRT_INIT_P, "enabled", "true", false),
    [?assert(should_track_init_p(M, F) =:= false, {M, F}) || [M, F] <- base_metrics()].

t_should_track_init_p_disabled(_) ->
    config:set(?CSRT_INIT_P, "enabled", "false", false),
    [?assert(should_track_init_p(M, F) =:= false, {M, F}) || [M, F] <- base_metrics()].

t_should_not_track_init_p(_) ->
    enable_init_p(),
    Metrics = [
        [couch_db, name],
        [couch_db, get_after_doc_read_fun],
        [couch_db, open],
        [fabric_rpc, get_purge_seq]
    ],
    [?assert(should_track_init_p(M, F) =:= false, {M, F}) || [M, F] <- Metrics].

enable_init_p() ->
    enable_init_p(base_metrics()).

enable_init_p(Metrics) ->
    config:set(?CSRT_INIT_P, "enabled", "true", false),
    [set_fabric_init_p(F, true, false) || [_, F] <- Metrics].

base_metrics() ->
    [
        [fabric_rpc, all_docs],
        [fabric_rpc, changes],
        [fabric_rpc, map_view],
        [fabric_rpc, reduce_view],
        [fabric_rpc, get_all_security],
        [fabric_rpc, open_doc],
        [fabric_rpc, update_docs],
        [fabric_rpc, open_shard]
    ].

-endif.
