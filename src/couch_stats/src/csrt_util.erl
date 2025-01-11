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
    tnow/0
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

is_enabled() ->
    config:get_boolean(?CSRT, "enabled", true).


conf_get(Key) ->
    conf_get(Key, undefined).


conf_get(Key, Default) ->
    config:get(?MODULE_STRING, Key, Default).

%% monotonic time now in millisecionds
tnow() ->
    erlang:monotonic_time(millisecond).

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
    list_to_binary(pid_to_list(Pid)).

convert_ref(Ref) when is_reference(Ref) ->
    list_to_binary(ref_to_list(Ref)).

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

