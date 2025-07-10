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

-module(csrt_entry).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

-export([
    value/2,
    key/1,
    from_map/1
]).

-spec value(#rctx{}, rctx_field()) -> any().

value(#rctx{pid_ref = Val}, pid_ref) -> Val;
value(#rctx{nonce = Val}, nonce) -> Val;
value(#rctx{type = Val}, type) -> csrt_util:convert_type(Val);
value(#rctx{dbname = Val}, dbname) -> Val;
value(#rctx{username = Val}, username) -> Val;
value(#rctx{db_open = Val}, db_open) -> Val;
value(#rctx{docs_read = Val}, docs_read) -> Val;
value(#rctx{docs_written = Val}, docs_written) -> Val;
value(#rctx{rows_read = Val}, rows_read) -> Val;
value(#rctx{changes_returned = Val}, changes_returned) -> Val;
value(#rctx{ioq_calls = Val}, ioq_calls) -> Val;
value(#rctx{js_filter = Val}, js_filter) -> Val;
value(#rctx{js_filtered_docs = Val}, js_filtered_docs) -> Val;
value(#rctx{get_kv_node = Val}, get_kv_node) -> Val;
value(#rctx{get_kp_node = Val}, get_kp_node) -> Val;
value(#rctx{started_at = Val}, started_at) -> Val;
value(#rctx{updated_at = Val}, updated_at) -> Val.

-spec key(BinKey :: binary() | string() | atom()) -> Key :: rctx_field()
    | throw({bad_request, Reason :: binary()}).

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
