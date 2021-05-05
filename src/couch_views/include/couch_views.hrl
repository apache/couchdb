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

% Current implementation version
-define(CURRENT_VIEW_IMPL_VERSION, 1).

% Index info/data subspaces
-define(VIEW_INFO, 0).
-define(VIEW_DATA, 1).
-define(VIEW_TREES, 3).

% Index info keys
-define(VIEW_UPDATE_SEQ, 0).
-define(VIEW_ROW_COUNT, 1).
-define(VIEW_KV_SIZE, 2).
-define(VIEW_BUILD_STATUS, 3).
-define(VIEW_CREATION_VS, 4).
-define(VIEW_IMPL_VERSION, 5).

% Data keys
-define(VIEW_ID_RANGE, 0).
-define(VIEW_MAP_RANGE, 1).

% Tree keys
-define(VIEW_ID_TREE, 0).
-define(VIEW_ROW_TREES, 1).

% jobs api
-define(INDEX_JOB_TYPE, <<"views">>).

% indexing progress
-define(INDEX_BUILDING, <<"building">>).
-define(INDEX_READY, <<"ready">>).

% Views/db marker to indicate that the current (latest) FDB GRV version should
% be used. Use `null` so it can can be round-tripped through json serialization
% with couch_jobs.
-define(VIEW_CURRENT_VSN, null).


-record(mrst, {
    sig=nil,
    fd=nil,
    fd_monitor,
    db_name,
    idx_name,
    language,
    design_opts=[],
    partitioned=false,
    lib,
    views,
    id_btree=nil,
    update_seq=0,
    purge_seq=0,
    first_build,
    partial_resp_pid,
    doc_acc,
    doc_queue,
    write_queue,
    qserver=nil
}).


-record(mrview, {
    id_num,
    update_seq=0,
    purge_seq=0,
    map_names=[],
    reduce_funs=[],
    def,
    btree=nil,
    options=[]
}).


-define(MAX_VIEW_LIMIT, 16#10000000).

-record(mrargs, {
    view_type,
    reduce,

    preflight_fun,

    start_key,
    start_key_docid,
    end_key,
    end_key_docid,
    keys,

    direction = fwd,
    limit = ?MAX_VIEW_LIMIT,
    skip = 0,
    group_level = 0,
    group = undefined,
    stable = false,
    update = true,
    multi_get = false,
    inclusive_end = true,
    include_docs = false,
    doc_options = [],
    update_seq=false,
    conflicts,
    callback,
    sorted = true,
    extra = [],
    page_size = undefined,
    bookmark=nil
}).

-record(vacc, {
    db,
    req,
    resp,
    prepend,
    etag,
    should_close = false,
    buffer = [],
    bufsize = 0,
    threshold = 1490,
    row_sent = false,
    meta_sent = false,
    paginated = false,
    meta = #{}
}).


-record(view_row, {
    key,
    id,
    value,
    doc
}).
