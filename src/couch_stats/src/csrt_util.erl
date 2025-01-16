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
    make_dt/2,
    make_dt/3,
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

