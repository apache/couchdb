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
    conf_get/1,
    conf_get/2,
    get_pid_ref/0,
    get_pid_ref/1,
    set_pid_ref/1,
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

-include_lib("couch_stats_resource_tracker.hrl").

-spec is_enabled() -> boolean().
is_enabled() ->
    config:get_boolean(?CSRT, "enabled", true).

-spec conf_get(Key :: list()) -> list().
conf_get(Key) ->
    conf_get(Key, "undefined").

-spec conf_get(Key :: list(), Default :: list()) -> list().
conf_get(Key, Default) ->
    config:get(?MODULE_STRING, Key, Default).

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
        ioq_calls => Rctx#rctx.ioq_calls
    }.

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

