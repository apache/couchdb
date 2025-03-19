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

-define(CSRT, "csrt").
-define(CSRT_INIT_P, "csrt.init_p").

%% CSRT pdict markers
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
-define(DOCS_WRITTEN, docs_written).
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
    %% NOTE: these stats are not local to the RPC worker, need forwarding
    [couchdb, query_server, calls, ddoc_filter] => ?COUCH_JS_FILTER,
    [couchdb, query_server, volume, ddoc_filter] => ?COUCH_JS_FILTERED_DOCS
}).

-define(KEYS_TO_FIELDS, #{
    ?DB_OPEN => #rctx.?DB_OPEN,
    ?ROWS_READ => #rctx.?ROWS_READ,
    ?FRPC_CHANGES_RETURNED => #rctx.?FRPC_CHANGES_RETURNED,
    ?DOCS_WRITTEN => #rctx.?DOCS_WRITTEN,
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

-type pid_ref() :: {pid(), reference()}.
-type maybe_pid_ref() :: pid_ref() | undefined.
-type maybe_pid() :: pid() | undefined.

-record(rpc_worker, {
    mod :: atom()  | '_',
    func :: atom()  | '_',
    from :: pid_ref() | '_'
}).

-record(coordinator, {
    mod :: atom()  | '_',
    func :: atom()  | '_',
    method :: atom() | '_',
    path :: binary() | '_'
}).

-type coordinator() :: #coordinator{}.
-type rpc_worker() :: #rpc_worker{}.
-type rctx_type() :: coordinator() | rpc_worker().

-record(rctx, {
    %% Metadata
    started_at = csrt_util:tnow(),
    updated_at = csrt_util:tnow(),
    pid_ref :: maybe_pid_ref() | {'_', '_'},
    nonce,
    type :: rctx_type() | undefined | '_',
    dbname,
    username,

    %% Stats counters
    db_open = 0,
    docs_read = 0 :: non_neg_integer(),
    docs_written = 0 :: non_neg_integer(),
    rows_read = 0 :: non_neg_integer(),
    changes_processed = 0 :: non_neg_integer(),
    changes_returned = 0 :: non_neg_integer(),
    ioq_calls = 0 :: non_neg_integer(),
    io_bytes_read = 0 :: non_neg_integer(),
    io_bytes_written = 0 :: non_neg_integer(),
    js_evals = 0 :: non_neg_integer(),
    js_filter = 0 :: non_neg_integer(),
    js_filtered_docs = 0 :: non_neg_integer(),
    mango_eval_match = 0 :: non_neg_integer(),
    %% TODO: switch record definitions to be macro based, eg:
    %% ?COUCH_BT_GET_KP_NODE = 0 :: non_neg_integer(),
    get_kv_node = 0 :: non_neg_integer(),
    get_kp_node = 0 :: non_neg_integer(),
    write_kv_node = 0 :: non_neg_integer(),
    write_kp_node = 0 :: non_neg_integer()
}).

-type rctx_field() ::
    started_at
        | updated_at
        | pid_ref
        | nonce
        | type
        | dbname
        | username
        | db_open
        | docs_read
        | docs_written
        | rows_read
        | changes_processed
        | changes_returned
        | ioq_calls
        | io_bytes_read
        | io_bytes_written
        | js_evals
        | js_filter
        | js_filtered_docs
        | mango_eval_match
        | get_kv_node
        | get_kp_node
        | write_kv_node
        | write_kp_node.

-type coordinator_rctx() :: #rctx{type :: coordinator()}.
-type rpc_worker_rctx() :: #rctx{type :: rpc_worker()}.
-type rctx() :: #rctx{} | coordinator_rctx() | rpc_worker_rctx().
-type maybe_rctx() :: rctx() | undefined.

%% TODO: solidify nonce type
-type nonce() :: any().
-type dbname() :: iodata().
-type username() :: iodata().

-type delta() :: map().
-type maybe_delta() :: delta() | undefined.
-type tagged_delta() :: {delta, maybe_delta()}.

-type matchers() :: #{atom() => {ets:match_spec(), ets:comp_match_spec()}}.
-type maybe_matchers() :: matchers() | undefined.
