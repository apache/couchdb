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
    is_enabled_reporting/0,
    is_enabled_rpc_reporting/0,
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
    convert_string/1,
    to_json/1
]).

%% Delta API
-export([
    add_delta/2,
    extract_delta/1,
    get_delta/1,
    get_delta_a/0,
    get_updated_at/0,
    maybe_add_delta/1,
    maybe_add_delta/2,
    make_delta/1,
    make_dt/2,
    make_dt/3,
    rctx_delta/2,
    put_delta_a/1,
    put_updated_at/1
]).

%% Extra niceties and testing facilities
-export([
    set_fabric_init_p/2,
    set_fabric_init_p/3,
    rctx_record_info/0
]).

-include_lib("couch_stats_resource_tracker.hrl").

-ifdef(TEST).
-spec is_enabled() -> boolean().
is_enabled() ->
    %% randomly enable CSRT during testing to handle unexpected failures
    case config:get_boolean(?CSRT, "randomize_testing", true) of
        true ->
            rand:uniform(100) > 80;
        false ->
            config:get_boolean(?CSRT, "enable", true)
    end.
-else.
-spec is_enabled() -> boolean().
is_enabled() ->
    %% TODO: toggle back to false before merging
    config:get_boolean(?CSRT, "enable", true).
-endif.

-spec is_enabled_init_p() -> boolean().
is_enabled_init_p() ->
    %% TODO: toggle back to false before merging
    config:get_boolean(?CSRT, "enable_init_p", true).

-spec should_track_init_p(Mod :: atom(), Func :: atom()) -> boolean().
should_track_init_p(fabric_rpc, Func) ->
    is_enabled_init_p() andalso config:get_boolean(?CSRT_INIT_P, fabric_conf_key(Func), false);
should_track_init_p(_Mod, _Func) ->
    false.

%% Toggle to disable all reporting
-spec is_enabled_reporting() -> boolean().
is_enabled_reporting() ->
    %% TODO: toggle back to false before merging
    config:get_boolean(?CSRT, "enable_reporting", true).

%% Toggle to disable all reporting from #rpc_worker{} types, eg only log
%% #coordinator{} types. This is a bit of a kludge that would be better served
%% by a dynamic match spec generator, but this provides a know for disabling
%% any rpc worker logs, even if they hit the normal logging Threshold's.
-spec is_enabled_rpc_reporting() -> boolean().
is_enabled_rpc_reporting() ->
    config:get_boolean(?CSRT, "enable_rpc_reporting", false).

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
    case erlang:convert_time_unit(abs(B - A), native, Unit) of
        Delta when Delta > 0 ->
            Delta;
        _ ->
            %% Handle case where Delta is smaller than a whole Unit, eg:
            %% Unit = millisecond,
            %% (node1@127.0.0.1)2> erlang:convert_time_unit(423, native, Unit).
            %% 0
            1
    end.

%%
%% Conversion API for outputting JSON
%%

-spec convert_type(T) -> binary() | null when
    T :: #coordinator{} | #rpc_worker{} | undefined.
convert_type(#coordinator{method = Verb0, path = Path, mod = M0, func = F0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    Verb = atom_to_binary(Verb0),
    <<"coordinator-{", M/binary, ":", F/binary, "}:", Verb/binary, ":", Path/binary>>;
convert_type(#rpc_worker{mod = M0, func = F0, from = From0}) ->
    M = atom_to_binary(M0),
    F = atom_to_binary(F0),
    %% Technically From is a PidRef data type from Pid, but different Ref for fabric
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

-spec convert_string(Str :: string() | binary() | undefined) -> binary() | null.
convert_string(undefined) ->
    null;
convert_string(Str) when is_list(Str) ->
    list_to_binary(Str);
convert_string(Bin) when is_binary(Bin) ->
    Bin.

-spec to_json(Rctx :: rctx()) -> map().
to_json(#rctx{} = Rctx) ->
    #{
        updated_at => convert_string(tutc(Rctx#rctx.updated_at)),
        started_at => convert_string(tutc(Rctx#rctx.started_at)),
        pid_ref => convert_pidref(Rctx#rctx.pid_ref),
        nonce => convert_string(Rctx#rctx.nonce),
        dbname => convert_string(Rctx#rctx.dbname),
        username => convert_string(Rctx#rctx.username),
        db_open => Rctx#rctx.db_open,
        docs_read => Rctx#rctx.docs_read,
        docs_written => Rctx#rctx.docs_written,
        js_filter => Rctx#rctx.js_filter,
        js_filtered_docs => Rctx#rctx.js_filtered_docs,
        rows_read => Rctx#rctx.rows_read,
        type => convert_type(Rctx#rctx.type),
        get_kp_node => Rctx#rctx.get_kp_node,
        get_kv_node => Rctx#rctx.get_kv_node,
        %% "Example to extend CSRT"
        %% write_kp_node => Rctx#rctx.write_kp_node,
        %% write_kv_node => Rctx#rctx.write_kv_node,
        changes_returned => Rctx#rctx.changes_returned,
        ioq_calls => Rctx#rctx.ioq_calls
    }.

-spec add_delta(T :: term(), Delta :: maybe_delta()) -> term_delta().
add_delta(T, undefined) ->
    T;
add_delta(T, Delta) when is_map(Delta) ->
    add_delta_int(T, {delta, Delta}).

-spec add_delta_int(T :: term(), Delta :: tagged_delta()) -> term_delta().
add_delta_int(T, {delta, _} = Delta) ->
    {T, Delta}.

-spec extract_delta(T :: term_delta()) -> {term(), maybe_delta()}.
extract_delta({Msg, {delta, Delta}}) ->
    {Msg, Delta};
extract_delta(Msg) ->
    {Msg, undefined}.

-spec get_delta(PidRef :: maybe_pid_ref()) -> tagged_delta().
get_delta(PidRef) ->
    {delta, make_delta(PidRef)}.

-spec maybe_add_delta(T :: term()) -> term_delta().
maybe_add_delta(T) ->
    case is_enabled() of
        false ->
            T;
        true ->
            maybe_add_delta_int(T, get_delta(get_pid_ref()))
    end.

%% Allow for externally provided Delta in error handling scenarios
%% eg in cases like rexi_server:notify_caller/3
-spec maybe_add_delta(T :: term(), Delta :: maybe_delta()) -> term_delta().
maybe_add_delta(T, undefined) ->
    T;
maybe_add_delta(T, Delta0) when is_map(Delta0) ->
    case is_enabled() of
        false ->
            T;
        true ->
            Delta = {delta, Delta0},
            maybe_add_delta_int(T, Delta)
    end.

-spec maybe_add_delta_int(T :: term(), Delta :: tagged_delta()) -> term_delta().
maybe_add_delta_int(T, {delta, undefined}) ->
    T;
maybe_add_delta_int(T, {delta, _} = Delta) ->
    add_delta_int(T, Delta).

-spec make_delta(PidRef :: maybe_pid_ref()) -> maybe_delta().
make_delta(undefined) ->
    undefined;
make_delta(PidRef) ->
    TA = get_delta_a(),
    TB = csrt_server:get_resource(PidRef),
    Delta = rctx_delta(TA, TB),
    put_delta_a(TB),
    Delta.

-spec rctx_delta(TA :: Rctx, TB :: Rctx) -> map().
rctx_delta(#rctx{} = TA, #rctx{} = TB) ->
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
        %% "Example to extend CSRT"
        %% write_kp_node => TB#rctx.write_kp_node - TA#rctx.write_kp_node,
        %% write_kv_node => TB#rctx.write_kv_node - TA#rctx.write_kv_node,
        dt => make_dt(TA#rctx.updated_at, TB#rctx.updated_at)
    },
    %% TODO: reevaluate this decision
    %% Only return non zero (and also positive) delta fields
    %% NOTE: this can result in Delta's of the form #{dt => 1}
    maps:filter(fun(_K, V) -> V > 0 end, Delta);
rctx_delta(_, _) ->
    undefined.

-spec get_delta_a() -> maybe_rctx().
get_delta_a() ->
    erlang:get(?DELTA_TA).

-spec put_delta_a(TA :: rctx()) -> maybe_rctx().
put_delta_a(TA) ->
    erlang:put(?DELTA_TA, TA).

-spec get_updated_at() -> maybe_integer().
get_updated_at() ->
    erlang:get(?LAST_UPDATED).

-spec put_updated_at(Updated :: rctx() | integer()) -> maybe_integer().
put_updated_at(#rctx{updated_at = Updated}) ->
    put_updated_at(Updated);
put_updated_at(Updated) when is_integer(Updated) ->
    erlang:put(?LAST_UPDATED, Updated).

-spec get_pid_ref() -> maybe_pid_ref().
get_pid_ref() ->
    get(?PID_REF).

-spec get_pid_ref(Rctx :: rctx()) -> maybe_pid_ref().
get_pid_ref(#rctx{pid_ref = PidRef}) ->
    PidRef;
get_pid_ref(_) ->
    undefined.

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
            ?TDEF_FE(t_should_not_track_init_p_empty),
            ?TDEF_FE(t_should_not_track_init_p_empty_and_disabled),
            ?TDEF_FE(t_should_not_track_init_p_disabled),
            ?TDEF_FE(t_should_not_track_init_p),
            ?TDEF_FE(t_should_extract_fields_properly)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch(),
    config:set_boolean(?CSRT, "randomize_testing", false, false),
    Ctx.

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_should_track_init_p(_) ->
    enable_init_p(),
    [?assert(should_track_init_p(M, F), {M, F}) || [M, F] <- base_metrics()].

t_should_not_track_init_p_empty(_) ->
    disable_init_p_metrics(),
    enable_init_p([]),
    [?assert(should_track_init_p(M, F) =:= false, {M, F}) || [M, F] <- base_metrics()].

t_should_not_track_init_p_empty_and_disabled(_) ->
    disable_init_p(),
    [?assert(should_track_init_p(M, F) =:= false, {M, F}) || [M, F] <- base_metrics()].

t_should_not_track_init_p_disabled(_) ->
    enable_init_p_metrics(),
    disable_init_p(),
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

t_should_extract_fields_properly(_) ->
    Rctx = #rctx{},
    #{fields := Fields} = csrt_entry:record_info(),
    %% csrt_entry:value/2 throws on invalid fields, assert that the function succeeded
    TestField = fun(Field) ->
        try
            csrt_entry:value(Field, Rctx),
            true
        catch
            _:_ -> false
        end
    end,
    [?assert(TestField(Field)) || Field <- Fields].

enable_init_p() ->
    enable_init_p(base_metrics()).

enable_init_p(Metrics) ->
    config:set(?CSRT, "enable_init_p", "true", false),
    enable_init_p_metrics(Metrics).

enable_init_p_metrics() ->
    enable_init_p(base_metrics()).

enable_init_p_metrics(Metrics) ->
    [set_fabric_init_p(F, true, false) || [_, F] <- Metrics].

disable_init_p() ->
    disable_init_p(base_metrics()).

disable_init_p(Metrics) ->
    config:set(?CSRT, "enable_init_p", "false", false),
    disable_init_p_metrics(Metrics).

disable_init_p_metrics() ->
    disable_init_p_metrics(base_metrics()).

disable_init_p_metrics(Metrics) ->
    [set_fabric_init_p(F, false, false) || [_, F] <- Metrics].

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
