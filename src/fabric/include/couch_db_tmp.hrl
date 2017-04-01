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

-define(LOCAL_DOC_PREFIX, "_local/").
-define(DESIGN_DOC_PREFIX0, "_design").
-define(DESIGN_DOC_PREFIX, "_design/").

-define(MIN_STR, <<"">>).
-define(MAX_STR, <<255>>). % illegal utf string

-define(JSON_ENCODE(V), couch_util:json_encode(V)).
-define(JSON_DECODE(V), couch_util:json_decode(V)).

-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).

-define(DEFAULT_ATTACHMENT_CONTENT_TYPE, <<"application/octet-stream">>).

-define(LOG_DEBUG(Format, Args), couch_log:debug(Format, Args)).
-define(LOG_INFO(Format, Args), couch_log:notice(Format, Args)).
-define(LOG_ERROR(Format, Args), couch_log:error(Format, Args)).

-record(rev_info,
    {
    rev,
    seq = 0,
    deleted = false,
    body_sp = nil % stream pointer
    }).

-record(doc_info,
    {
    id = <<"">>,
    high_seq = 0,
    revs = [] % rev_info
    }).

-record(full_doc_info,
    {id = <<"">>,
    update_seq = 0,
    deleted = false,
    data_size = 0,
    rev_tree = []
    }).

-record(httpd,
    {mochi_req,
    peer,
    method,
    path_parts,
    db_url_handlers,
    user_ctx,
    req_body = undefined,
    design_url_handlers,
    auth,
    default_fun,
    url_handlers
    }).


-record(doc,
    {
    id = <<"">>,
    revs = {0, []},

    % the json body object.
    body = {[]},

    atts = [], % attachments

    deleted = false,

    % key/value tuple of meta information, provided when using special options:
    % couch_db:open_doc(Db, Id, Options).
    meta = []
    }).


-record(att,
    {
    name,
    type,
    att_len,
    disk_len, % length of the attachment in its identity form
              % (that is, without a content encoding applied to it)
              % differs from att_len when encoding /= identity
    md5= <<>>,
    revpos=0,
    data,
    encoding=identity % currently supported values are:
                      %     identity, gzip
                      % additional values to support in the future:
                      %     deflate, compress
    }).


-record(user_ctx,
    {
    name=null,
    roles=[],
    handler
    }).

% This should be updated anytime a header change happens that requires more
% than filling in new defaults.
%
% As long the changes are limited to new header fields (with inline
% defaults) added to the end of the record, then there is no need to increment
% the disk revision number.
%
% if the disk revision is incremented, then new upgrade logic will need to be
% added to couch_db_updater:init_db.

-define(LATEST_DISK_VERSION, 5).

-record(db_header,
    {disk_version = ?LATEST_DISK_VERSION,
     update_seq = 0,
     unused = 0,
     id_tree_state = nil,
     seq_tree_state = nil,
     local_tree_state = nil,
     purge_seq = 0,
     purged_docs = nil,
     security_ptr = nil,
     revs_limit = 1000
    }).

-record(db,
    {main_pid = nil,
    update_pid = nil,
    compactor_pid = nil,
    instance_start_time, % number of microsecs since jan 1 1970 as a binary string
    fd,
    fd_monitor,
    header = #db_header{},
    committed_update_seq,
    id_tree,
    seq_tree,
    local_tree,
    update_seq,
    name,
    filepath,
    validate_doc_funs = undefined,
    security = [],
    security_ptr = nil,
    user_ctx = #user_ctx{},
    waiting_delayed_commit = nil,
    revs_limit = 1000,
    fsync_options = [],
    is_sys_db = false
    }).


-record(view_query_args, {
    start_key,
    end_key,
    start_docid = ?MIN_STR,
    end_docid = ?MAX_STR,

    direction = fwd,
    inclusive_end=true, % aka a closed-interval

    limit = 10000000000, % Huge number to simplify logic
    skip = 0,

    group_level = 0,

    view_type = nil,
    include_docs = false,
    stale = false,
    multi_get = false,
    callback = nil,
    list = nil,
    keys = nil,
    sorted = true,
    extra = []
}).

-record(view_fold_helper_funs, {
    reduce_count,
    passed_end,
    start_response,
    send_row
}).

-record(reduce_fold_helper_funs, {
    start_response,
    send_row
}).

-record(extern_resp_args, {
    code = 200,
    stop = false,
    data = <<>>,
    ctype = "application/json",
    headers = [],
    json = nil
}).

-record(group, {
    sig=nil,
    dbname,
    fd=nil,
    name,
    def_lang,
    design_options=[],
    views,
    id_btree=nil,
    current_seq=0,
    purge_seq=0,
    query_server=nil,
    waiting_delayed_commit=nil,
    atts=[]
    }).

-record(view,
    {id_num,
    map_names=[],
    def,
    btree=nil,
    reduce_funs=[],
    dbcopies=[],
    options=[]
    }).

-record(index_header,
    {seq=0,
    purge_seq=0,
    id_btree_state=nil,
    view_states=nil
    }).

-record(http_db, {
    url,
    auth = [],
    resource = "",
    headers = [
        {"User-Agent", "CouchDB/"++couch:version()},
        {"Accept", "application/json"},
        {"Accept-Encoding", "gzip"}
    ],
    qs = [],
    method = get,
    body = nil,
    options = [
        {response_format,binary},
        {inactivity_timeout, 30000}
    ],
    retries = 10,
    pause = 500,
    conn = nil
}).

% small value used in revision trees to indicate the revision isn't stored
-define(REV_MISSING, []).

-record(changes_args, {
    feed = "normal",
    dir = fwd,
    since = "0",
    limit = 1000000000000000,
    style = main_only,
    heartbeat,
    timeout,
    filter,
    include_docs = false
}).

-record(proc, {
    pid,
    lang,
    client = nil,
    ddoc_keys = [],
    prompt_fun,
    set_timeout_fun,
    stop_fun,
    data_fun
}).

-record(leaf, {
    deleted,
    ptr,
    seq,
    size = 0,
    atts = []
}).
