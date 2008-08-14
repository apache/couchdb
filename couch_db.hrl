% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-define(LOCAL_DOC_PREFIX, "_local/").
-define(DESIGN_DOC_PREFIX0, "_design").
-define(DESIGN_DOC_PREFIX, "_design/").

-define(DEFAULT_ATTACHMENT_CONTENT_TYPE, "application/octet-stream").
        
-define(LOG_DEBUG(Format, Args),
    case couch_log:debug_on() of
        true -> error_logger:info_report(couch_debug, {Format, Args});
        false -> ok
    end).

-define(LOG_INFO(Format, Args),
    case couch_log:info_on() of
        true -> error_logger:info_report(couch_info, {Format, Args});
        false -> ok
    end).

-define(LOG_ERROR(Format, Args),
    error_logger:info_report(couch_error, {Format, Args})).

-record(doc_info,
    {
    id = "",
    rev = "",
    update_seq = 0,
    summary_pointer = nil,
    conflict_revs = [],
    deleted_conflict_revs = [],
    deleted = false
    }).

-record(full_doc_info,
    {id = "",
    update_seq = 0,
    deleted = false,
    rev_tree = []
    }).

-record(doc,
    {
    id = "",
    revs = [],

    % the json body object.
    body = {obj, []},

    % each attachment contains:
    %    {data, Type, <<binary>>}
    % or:
    %    {pointer, Type, {FileHandle, StreamPointer, Length}}
    attachments = [],

    deleted = false,

    % key/value tuple of meta information, provided when using special options:
    % couch_db:open_doc(Db, Id, Options).
    meta = []
    }).
    
    



-record(db_header,
    {write_version = 0,
     update_seq = 0,
     summary_stream_state = nil,
     fulldocinfo_by_id_btree_state = nil,
     docinfo_by_seq_btree_state = nil,
     local_docs_btree_state = nil,
     doc_count=0,
     doc_del_count=0
    }).

-record(db,
    {main_pid=nil,
    update_pid=nil,
    compactor_pid=nil,
    fd,
    header = #db_header{},
    summary_stream,
    fulldocinfo_by_id_btree,
    docinfo_by_seq_btree,
    local_docs_btree,
    update_seq,
    doc_count,
    doc_del_count,
    name,
    filepath
    }).
    
    

% small value used in revision trees to indicate the revision isn't stored
-define(REV_MISSING, []).
