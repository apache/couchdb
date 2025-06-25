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
    key/1
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
    key_from_binary(list_to_binary(Key));
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
    Other = iolist_to_binary(io_lib:format("~s", Key)),
    throw({bad_request, <<"Invalid key '", Other/binary, "'">>}).

