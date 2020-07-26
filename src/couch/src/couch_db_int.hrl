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
    vsn = 1,
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

    % feature removed in 3.x, but field kept to avoid changing db record size
    % and breaking rolling cluster upgrade
    waiting_delayed_commit_deprecated,

    options = [],
    compression,
    access = false
}).


-define(OLD_DB_REC, {
    db,
    _, % MainPid
    _, % CompactorPid
    _, % InstanceStartTime
    _, % Fd
    _, % FdMonitor
    _, % Header
    _, % CommittedUpdateSeq
    _, % IdTree
    _, % SeqTree
    _, % LocalTree
    _, % UpdateSeq
    _, % Name
    _, % FilePath
    _, % ValidateDocFuns
    _, % Security
    _, % SecurityPtr
    _, % UserCtx
    _, % WaitingDelayedCommit
    _, % RevsLimit
    _, % FsyncOptions
    _, % Options
    _, % Compression
    _, % BeforeDocUpdate
    _  % AfterDocRead
}).


-define(OLD_DB_NAME(Db), element(2, Db)).
-define(OLD_DB_MAIN_PID(Db), element(13, Db)).
-define(OLD_DB_USER_CTX(Db), element(18, Db)).
-define(OLD_DB_SECURITY(Db), element(16, Db)).
