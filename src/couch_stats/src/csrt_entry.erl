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

-spec key(BinKey :: binary() | string()) -> Key :: rctx_field()
    | throw({bad_request, Reason :: binary()}).

key(<<"pid_ref">>) -> pid_ref;
key(<<"nonce">>) -> nonce;
key(<<"type">>) -> type;
key(<<"dbname">>) -> dbname;
key(<<"username">>) -> username;
key(<<"db_open">>) -> db_open;
key(<<"docs_read">>) -> docs_read;
key(<<"rows_read">>) -> rows_read;
key(<<"changes_returned">>) -> changes_returned;
key(<<"ioq_calls">>) -> ioq_calls;
key(<<"js_filter">>) -> js_filter;
key(<<"js_filtered_docs">>) -> js_filtered_docs;
key(<<"get_kv_node">>) -> get_kv_node;
key(<<"get_kp_node">>) -> get_kp_node;
key(Other) when is_binary(Other) -> throw({bad_request, <<"Invalid key '", Other/binary, "'">>}).
