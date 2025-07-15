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
-define(CSRT_ETS, csrt_server).

%% CSRT pdict markers
-define(DELTA_TA, {csrt, delta_ta}).
-define(LAST_UPDATED, {csrt, last_updated}).
-define(PID_REF, {csrt, pid_ref}). %% track local ID
-define(TRACKER_PID, {csrt, tracker}). %% tracker pid

%% Stats fields
-define(DB_OPEN_DOC, docs_read).
-define(DB_OPEN, db_open).
-define(COUCH_SERVER_OPEN, db_open).
-define(COUCH_BT_GET_KP_NODE, get_kp_node).
-define(COUCH_BT_GET_KV_NODE, get_kv_node).
%% "Example to extend CSRT"
%%-define(COUCH_BT_WRITE_KP_NODE, write_kp_node).
%%-define(COUCH_BT_WRITE_KV_NODE, write_kv_node).
-define(COUCH_JS_FILTER, js_filter).
-define(COUCH_JS_FILTERED_DOCS, js_filtered_docs).
-define(IOQ_CALLS, ioq_calls).
-define(DOCS_WRITTEN, docs_written).
-define(ROWS_READ, rows_read).
-define(FRPC_CHANGES_RETURNED, changes_returned).

%% csrt_logger matcher keys
-define(MATCHERS_KEY, {csrt_logger, all_csrt_matchers}).
-define(CSRT_MATCHERS_ENABLED, "csrt_logger.matchers_enabled").
-define(CSRT_MATCHERS_THRESHOLD, "csrt_logger.matchers_threshold").
-define(CSRT_MATCHERS_DBNAMES, "csrt_logger.dbnames_io").

%% matcher query magnitude default limitations
-define(QUERY_CARDINALITY_LIMIT, 10_000).
-define(QUERY_LIMIT, 100).

%% Mapping of couch_stat metric names to #rctx{} field names.
%% These are used for fields that we inc a counter on.
-define(STATS_TO_KEYS, #{
    [couchdb, database_reads] => ?DB_OPEN_DOC,
    %% Double on ?ROWS_READ for changes_processed as we only need the one
    %% field, as opposed to needing both metrics to distinguish changes
    %% workloads and view/_all_docs.
    [fabric_rpc, changes, processed] => ?ROWS_READ,
    [fabric_rpc, changes, returned] => ?FRPC_CHANGES_RETURNED,
    [fabric_rpc, view, rows_read] => ?ROWS_READ,
    [couchdb, couch_server, open] => ?DB_OPEN,
    [couchdb, btree, get_node, kp_node] => ?COUCH_BT_GET_KP_NODE,
    [couchdb, btree, get_node, kv_node] => ?COUCH_BT_GET_KV_NODE

    %% NOTE: these stats are not local to the RPC worker, need forwarding
    %% "Example to extend CSRT"
    %% [couchdb, btree, write_node, kp_node] => ?COUCH_BT_WRITE_KP_NODE,
    %% [couchdb, btree, write_node, kv_node] => ?COUCH_BT_WRITE_KV_NODE,
    %% [couchdb, query_server, calls, ddoc_filter] => ?COUCH_JS_FILTER
}).

%% Mapping of stat field names to their corresponding record entries.
%% This only includes integer fields valid for ets:update_counter
-define(STAT_KEYS_TO_FIELDS, #{
    ?DB_OPEN => #rctx.?DB_OPEN,
    ?ROWS_READ => #rctx.?ROWS_READ,
    ?FRPC_CHANGES_RETURNED => #rctx.?FRPC_CHANGES_RETURNED,
    ?DOCS_WRITTEN => #rctx.?DOCS_WRITTEN,
    ?IOQ_CALLS => #rctx.?IOQ_CALLS,
    ?COUCH_JS_FILTER => #rctx.?COUCH_JS_FILTER,
    ?COUCH_JS_FILTERED_DOCS => #rctx.?COUCH_JS_FILTERED_DOCS,
    ?DB_OPEN_DOC => #rctx.?DB_OPEN_DOC,
    ?COUCH_BT_GET_KP_NODE => #rctx.?COUCH_BT_GET_KP_NODE,
    ?COUCH_BT_GET_KV_NODE => #rctx.?COUCH_BT_GET_KV_NODE
    %% "Example to extend CSRT"
    %% ?COUCH_BT_WRITE_KP_NODE => #rctx.?COUCH_BT_WRITE_KP_NODE,
    %% ?COUCH_BT_WRITE_KV_NODE => #rctx.?COUCH_BT_WRITE_KV_NODE
}).

-type throw(_Reason) :: no_return().

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
    started_at = csrt_util:tnow() :: integer() | '_',
    %% NOTE: updated_at must be after started_at to preserve time congruity
    updated_at = csrt_util:tnow() :: integer() | '_',
    pid_ref :: maybe_pid_ref() | {'_', '_'} | '_',
    nonce :: nonce() | undefined | '_',
    type :: rctx_type() | undefined | '_',
    dbname :: dbname() | undefined | '_',
    username :: username() | undefined | '_',

    %% Stats Counters
    db_open = 0 :: non_neg_integer() | '_',
    docs_read = 0 :: non_neg_integer() | '_',
    docs_written = 0 :: non_neg_integer() | '_',
    rows_read = 0 :: non_neg_integer() | '_',
    changes_returned = 0 :: non_neg_integer() | '_',
    ioq_calls = 0 :: non_neg_integer() | '_',
    js_filter = 0 :: non_neg_integer() | '_',
    js_filtered_docs = 0 :: non_neg_integer() | '_',
    get_kv_node = 0 :: non_neg_integer() | '_',
    get_kp_node = 0 :: non_neg_integer() | '_'
    %% "Example to extend CSRT"
    %%write_kv_node = 0 :: non_neg_integer() | '_',
    %%write_kp_node = 0 :: non_neg_integer() | '_'
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
        | changes_returned
        | ioq_calls
        | js_filter
        | js_filtered_docs
        | get_kv_node
        | get_kp_node.
        %% "Example to extend CSRT"
        %%| write_kv_node
        %%| write_kp_node.


-type coordinator_rctx() :: #rctx{type :: coordinator()}.
-type rpc_worker_rctx() :: #rctx{type :: rpc_worker()}.
-type rctx() :: #rctx{} | coordinator_rctx() | rpc_worker_rctx().
-type rctxs() :: [#rctx{}] | [].
-type maybe_rctx() :: rctx() | undefined.

%% TODO: solidify nonce type and ideally move to couch_db.hrl
-type nonce() :: any().
-type dbname() :: iodata().
-type username() :: iodata().

-type delta() :: map().
-type maybe_delta() :: delta() | undefined.
-type tagged_delta() :: {delta, maybe_delta()}.
-type term_delta() :: term() | {term(), tagged_delta()}.

-type matcher_name() :: string().
-type matcher() :: {ets:match_spec(), ets:comp_match_spec()}.
-type matchers() :: #{matcher_name() => matcher()} | #{}.
-type matcher_matches() :: #{matcher_name() => rctxs()} | #{}.
-type maybe_matcher() :: matcher() | undefined.
-type maybe_matchers() :: matchers() | undefined.

-type maybe_integer() :: integer() | undefined.
%% This is a little awkward to type, it's a list of ets:update_counter UpdateOp's
%% where ets types the updates as `UpdateOp = {Pos, Incr}`. We can do better than
%% that because we know `Pos` is the #rctx record field index, a non_neg_integer(),
%% and similarly, we know Incr is from `csrt_util:make_dt`, which is returns at
%% least one. Ideally, we'd specify the `Pos` type sufficiently to be one of the
%% valid #rctx record field names, however, a clean solution is not obvious.
-type counter_updates_list() :: [{non_neg_integer(), pos_integer()}] | [].

-type tuple_of_field_values() :: tuple().
-type tuple_of_field_names() :: tuple().

-type query_options() :: #{limit => pos_integer()}.
-type aggregation_key() :: tuple_of_field_names().
-type aggregation_values() :: tuple_of_field_values().
-type aggregation_result() :: #{aggregation_key() => non_neg_integer()}.
-type ordered_result() :: [{aggregation_key(), non_neg_integer()}].
-type query_result() :: aggregation_result() | ordered_result().
-type json_spec(_Spec) :: term().
