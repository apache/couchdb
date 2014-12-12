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
-define(DEFAULT_COMPRESSION, snappy).

-define(MIN_STR, <<"">>).
-define(MAX_STR, <<255>>). % illegal utf string

-define(REWRITE_COUNT, couch_rewrite_count).

-define(JSON_ENCODE(V), couch_util:json_encode(V)).
-define(JSON_DECODE(V), couch_util:json_decode(V)).

-define(IS_OLD_RECORD(V, R), (tuple_size(V) /= tuple_size(R))).

-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).
-define(i2b(V), couch_util:integer_to_boolean(V)).
-define(b2i(V), couch_util:boolean_to_integer(V)).
-define(term_to_bin(T), term_to_binary(T, [{minor_version, 1}])).
-define(term_size(T),
    try
        erlang:external_size(T)
    catch _:_ ->
        byte_size(?term_to_bin(T))
    end).

-define(DEFAULT_ATTACHMENT_CONTENT_TYPE, <<"application/octet-stream">>).

-define(ADMIN_USER, #user_ctx{roles=[<<"_admin">>]}).
-define(ADMIN_CTX, {user_ctx, ?ADMIN_USER}).

-define(SYSTEM_DATABASES, [
    <<"_dbs">>,
    <<"_global_changes">>,
    <<"_metadata">>,
    <<"_nodes">>,
    <<"_replicator">>,
    <<"_users">>
]).


-type branch() :: {Key::term(), Value::term(), Tree::term()}.
-type path() :: {Start::pos_integer(), branch()}.

-record(rev_info, {
    rev,
    seq = 0,
    deleted = false,
    body_sp = nil % stream pointer
}).

-record(doc_info, {
    id = <<"">>,
    high_seq = 0,
    revs = [] % rev_info
}).

-record(size_info, {
    active = 0,
    external = 0
}).

-record(full_doc_info, {
    id = <<"">>,
    update_seq = 0,
    deleted = false,
    rev_tree = [],
    sizes = #size_info{}
}).

-record(httpd, {
    mochi_req,
    peer,
    method,
    requested_path_parts,
    path_parts,
    db_url_handlers,
    user_ctx,
    req_body = undefined,
    design_url_handlers,
    auth,
    default_fun,
    url_handlers,
    authentication_handlers = [],
    absolute_uri,
    auth_module,
    begin_ts,
    original_method,
    nonce,
    cors_config,
    qs
}).


-record(doc, {
    id = <<"">>,
    revs = {0, []},

    % the json body object.
    body = {[]},

    atts = [] :: [couch_att:att()], % attachments

    deleted = false,

    % key/value tuple of meta information, provided when using special options:
    % couch_db:open_doc(Db, Id, Options).
    meta = []
}).


-record(user_ctx, {
    name=null,
    roles=[],
    handler
}).

-record(db, {
    main_pid = nil,
    compactor_pid = nil,
    instance_start_time, % number of microsecs since jan 1 1970 as a binary string
    fd,
    fd_monitor,
    header = couch_db_header:new(),
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
    options = [],
    compression,
    before_doc_update = nil, % nil | fun(Doc, Db) -> NewDoc
    after_doc_read = nil    % nil | fun(Doc, Db) -> NewDoc
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

-record(index_header, {
    seq=0,
    purge_seq=0,
    id_btree_state=nil,
    view_states=nil
}).

% small value used in revision trees to indicate the revision isn't stored
-define(REV_MISSING, []).

-record(changes_args, {
    feed = "normal",
    dir = fwd,
    since = 0,
    limit = 1000000000000000,
    style = main_only,
    heartbeat,
    timeout,
    filter = "",
    filter_fun,
    filter_args = [],
    include_docs = false,
    doc_options = [],
    conflicts = false,
    db_open_options = []
}).

-record(btree, {
    fd,
    root,
    extract_kv,
    assemble_kv,
    less,
    reduce = nil,
    compression = ?DEFAULT_COMPRESSION
}).

-record(proc, {
    pid,
    lang,
    client = nil,
    ddoc_keys = [],
    prompt_fun,
    set_timeout_fun,
    stop_fun
}).

-record(leaf,  {
    deleted,
    ptr,
    seq,
    sizes = #size_info{},
    atts = []
}).


-type doc() :: #doc{}.
-type ddoc() :: #doc{}.
-type user_ctx() :: #user_ctx{}.
-type sec_props() :: [tuple()].
-type sec_obj() :: {sec_props()}.

