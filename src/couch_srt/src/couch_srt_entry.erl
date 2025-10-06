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

-module(couch_srt_entry).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_srt.hrl").

-export([
    value/2,
    key/1,
    from_map/1,
    record_info/0
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

-spec value(rctx_field(), #rctx{}) -> any().

value(pid_ref, #rctx{pid_ref = Val}) -> Val;
value(nonce, #rctx{nonce = Val}) -> Val;
value(type, #rctx{type = Val}) -> convert_type(Val);
value(dbname, #rctx{dbname = Val}) -> Val;
value(username, #rctx{username = Val}) -> Val;
value(db_open, #rctx{db_open = Val}) -> Val;
value(docs_read, #rctx{docs_read = Val}) -> Val;
value(docs_written, #rctx{docs_written = Val}) -> Val;
value(rows_read, #rctx{rows_read = Val}) -> Val;
value(changes_returned, #rctx{changes_returned = Val}) -> Val;
value(ioq_calls, #rctx{ioq_calls = Val}) -> Val;
value(js_filter, #rctx{js_filter = Val}) -> Val;
value(js_filtered_docs, #rctx{js_filtered_docs = Val}) -> Val;
value(get_kv_node, #rctx{get_kv_node = Val}) -> Val;
value(get_kp_node, #rctx{get_kp_node = Val}) -> Val;
value(started_at, #rctx{started_at = Val}) -> Val;
value(updated_at, #rctx{updated_at = Val}) -> Val.

-spec key(BinKey :: binary() | string() | atom()) ->
    Key ::
        rctx_field()
        | {error, Reason :: any()}.

key(Key) when is_atom(Key) ->
    key_from_atom(Key);
key(Key) when is_binary(Key) ->
    key_from_binary(Key);
key(Key) when is_list(Key) ->
    case key_from_binary(list_to_binary(Key)) of
        {error, {invalid_key, _Key}} ->
            {error, {invalid_key, Key}};
        Res ->
            Res
    end;
key(Other) ->
    key_error(Other).

key_from_atom(pid_ref) -> pid_ref;
key_from_atom(nonce) -> nonce;
key_from_atom(type) -> type;
key_from_atom(dbname) -> dbname;
key_from_atom(username) -> username;
key_from_atom(db_open) -> db_open;
key_from_atom(docs_read) -> docs_read;
key_from_atom(rows_read) -> rows_read;
key_from_atom(changes_returned) -> changes_returned;
key_from_atom(ioq_calls) -> ioq_calls;
key_from_atom(js_filter) -> js_filter;
key_from_atom(js_filtered_docs) -> js_filtered_docs;
key_from_atom(get_kv_node) -> get_kv_node;
key_from_atom(get_kp_node) -> get_kp_node;
key_from_atom(Other) -> key_error(Other).

key_from_binary(<<"pid_ref">>) -> pid_ref;
key_from_binary(<<"nonce">>) -> nonce;
key_from_binary(<<"type">>) -> type;
key_from_binary(<<"dbname">>) -> dbname;
key_from_binary(<<"username">>) -> username;
key_from_binary(<<"db_open">>) -> db_open;
key_from_binary(<<"docs_read">>) -> docs_read;
key_from_binary(<<"rows_read">>) -> rows_read;
key_from_binary(<<"changes_returned">>) -> changes_returned;
key_from_binary(<<"ioq_calls">>) -> ioq_calls;
key_from_binary(<<"js_filter">>) -> js_filter;
key_from_binary(<<"js_filtered_docs">>) -> js_filtered_docs;
key_from_binary(<<"get_kv_node">>) -> get_kv_node;
key_from_binary(<<"get_kp_node">>) -> get_kp_node;
key_from_binary(Other) -> key_error(Other).

key_error(Key) ->
    {error, {invalid_key, Key}}.

-spec from_map(Map :: map()) -> rctx().

from_map(Map) ->
    maps:fold(fun set_field/3, #rctx{}, Map).

-spec set_field(Field :: rctx_field(), Val :: any(), Rctx :: rctx()) -> rctx().
set_field(updated_at, Val, Rctx) ->
    Rctx#rctx{updated_at = Val};
set_field(started_at, Val, Rctx) ->
    Rctx#rctx{started_at = Val};
set_field(pid_ref, Val, Rctx) ->
    Rctx#rctx{pid_ref = Val};
set_field(nonce, Val, Rctx) ->
    Rctx#rctx{nonce = Val};
set_field(dbname, Val, Rctx) ->
    Rctx#rctx{dbname = Val};
set_field(username, Val, Rctx) ->
    Rctx#rctx{username = Val};
set_field(db_open, Val, Rctx) ->
    Rctx#rctx{db_open = Val};
set_field(docs_read, Val, Rctx) ->
    Rctx#rctx{docs_read = Val};
set_field(docs_written, Val, Rctx) ->
    Rctx#rctx{docs_written = Val};
set_field(js_filter, Val, Rctx) ->
    Rctx#rctx{js_filter = Val};
set_field(js_filtered_docs, Val, Rctx) ->
    Rctx#rctx{js_filtered_docs = Val};
set_field(rows_read, Val, Rctx) ->
    Rctx#rctx{rows_read = Val};
set_field(type, Val, Rctx) ->
    Rctx#rctx{type = Val};
set_field(get_kp_node, Val, Rctx) ->
    Rctx#rctx{get_kp_node = Val};
set_field(get_kv_node, Val, Rctx) ->
    Rctx#rctx{get_kv_node = Val};
%% "Example to extend CSRT"
%% set_field(write_kp_node, Val, Rctx) ->
%%     Rctx#rctx{write_kp_node = Val};
%% set_field(write_kv_node, Val, Rctx) ->
%%     Rctx#rctx{write_kv_node = Val};
set_field(changes_returned, Val, Rctx) ->
    Rctx#rctx{changes_returned = Val};
set_field(ioq_calls, Val, Rctx) ->
    Rctx#rctx{ioq_calls = Val};
set_field(_, _, Rctx) ->
    %% Unknown key, could throw but just move on
    Rctx.

-spec record_info() ->
    #{
        fields => [rctx_field()],
        size => pos_integer(),
        field_idx => #{rctx_field() => pos_integer()}
    }.
record_info() ->
    Fields = record_info(fields, rctx),
    Size = record_info(size, rctx),
    Idx = maps:from_list(lists:zip(Fields, lists:seq(1, length(Fields)))),
    #{
        fields => Fields,
        field_idx => Idx,
        size => Size
    }.

-spec to_json(Rctx :: rctx()) -> map().
to_json(#rctx{} = Rctx) ->
    #{
        updated_at => convert_string(couch_srt_util:tutc(Rctx#rctx.updated_at)),
        started_at => convert_string(couch_srt_util:tutc(Rctx#rctx.started_at)),
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
