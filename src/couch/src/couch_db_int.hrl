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


-record(new_pse_db, {
    vsn,
    name,
    filepath,

    engine = {couch_bt_engine, undefined},

    main_pid = nil,
    compactor_pid = nil,

    committed_update_seq,

    instance_start_time, % number of microsecs since jan 1 1970 as a binary string

    user_ctx = #user_ctx{},
    security = [],
    validate_doc_funs = undefined,

    before_doc_update = nil, % nil | fun(Doc, Db) -> NewDoc
    after_doc_read = nil,    % nil | fun(Doc, Db) -> NewDoc

    waiting_delayed_commit = nil,

    options = [],
    compression
}).


-define(NEW_PSE_DB, {
    db,
    _, % Version
    _, % Name
    _, % FilePath
    _, % Engine
    _, % MainPid
    _, % CompactorPid
    _, % CommittedUpdateSeq
    _, % InstanceStartTime
    _, % UserCtx
    _, % Security
    _, % ValidateDocFuns
    _, % BeforeDocUpdate
    _, % AfterDocRead
    _, % WaitingDelayedCommit
    _, % Options
    _  % Compression
}).


-define(PSE_DB_NAME(Db), element(3, Db)).
-define(PSE_DB_MAIN_PID(Db), element(6, Db)).
-define(PSE_DB_USER_CTX(Db), element(10, Db)).
-define(PSE_DB_SECURITY(Db), element(11, Db)).
