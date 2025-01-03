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


-record(rpc_worker, {
    mod :: atom()  | '_',
    func :: atom()  | '_',
    from :: {pid(), reference()} | '_'
}).

-record(coordinator, {
    mod :: atom()  | '_',
    func :: atom()  | '_',
    method :: atom() | '_',
    path :: binary() | '_'
}).

-record(rctx, {
    %% Metadata
    started_at = ?MODULE:tnow(),
    updated_at = ?MODULE:tnow(),
    pid_ref,
    nonce,
    type, %% #coordinator{}/#rpc_worker{}/#replication_worker{}/#compaction_worker
    dbname,
    username,

    %% Stats counters
    db_open = 0,
    docs_read = 0,
    rows_read = 0,
    changes_processed = 0,
    changes_returned = 0,
    ioq_calls = 0,
    io_bytes_read = 0,
    io_bytes_written = 0,
    js_evals = 0,
    js_filter = 0,
    js_filtered_docs = 0,
    mango_eval_match = 0,
    %% TODO: switch record definitions to be macro based, eg:
    %% ?COUCH_BT_GET_KP_NODE = 0,
    get_kv_node = 0,
    get_kp_node = 0,
    write_kv_node = 0,
    write_kp_node = 0
}).
