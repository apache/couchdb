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

-define(REP_ID_VERSION, 4).

% Some fields from the replication doc
-define(SOURCE, <<"source">>).
-define(TARGET, <<"target">>).
-define(CREATE_TARGET, <<"create_target">>).
-define(DOC_IDS, <<"doc_ids">>).
-define(SELECTOR, <<"selector">>).
-define(FILTER, <<"filter">>).
-define(QUERY_PARAMS, <<"query_params">>).
-define(URL, <<"url">>).
-define(AUTH, <<"auth">>).
-define(HEADERS, <<"headers">>).
-define(PROXY, <<"proxy">>).
-define(SOURCE_PROXY, <<"source_proxy">>).
-define(TARGET_PROXY, <<"target_proxy">>).

-define(REPLICATION_STATE, <<"_replication_state">>).
-define(REPLICATION_STATS, <<"_replication_stats">>).
-define(REPLICATION_ID, <<"_replication_id">>).
-define(REPLICATION_STATE_TIME, <<"_replication_state_time">>).
-define(REPLICATION_STATE_REASON, <<"_replication_state_reason">>).

% Replication states
-define(ST_ERROR, <<"error">>).
-define(ST_COMPLETED, <<"completed">>).
-define(ST_RUNNING, <<"running">>).
-define(ST_FAILED, <<"failed">>).
-define(ST_PENDING, <<"pending">>).
-define(ST_CRASHING, <<"crashing">>).

% Some fields from a rep object
-define(REP_ID, <<"rep_id">>).
-define(BASE_ID, <<"base_id">>).
-define(DB_NAME, <<"db_name">>).
-define(DB_UUID, <<"db_uuid">>).
-define(DOC_ID, <<"doc_id">>).
-define(REP_USER, <<"rep_user">>).
-define(START_TIME, <<"start_time">>).
-define(OPTIONS, <<"options">>).

% Fields for couch job data objects
-define(REP, <<"rep">>).
-define(REP_PARSE_ERROR, <<"rep_parse_error">>).
-define(REP_STATS, <<"rep_stats">>).
-define(STATE, <<"state">>).
-define(STATE_INFO, <<"state_info">>).
-define(DOC_STATE, <<"doc_state">>).
-define(ERROR_COUNT, <<"error_count">>).
-define(LAST_UPDATED, <<"last_updated">>).
-define(LAST_START, <<"last_start">>).
-define(LAST_ERROR, <<"last_error">>).
-define(JOB_HISTORY, <<"job_history">>).
-define(CHECKPOINT_HISTORY, <<"checkpoint_history">>).
-define(REP_NODE, <<"node">>).
-define(REP_PID, <<"pid">>).

% Job history tags
-define(HIST_TYPE, <<"type">>).
-define(HIST_TIMESTAMP, <<"timestamp">>).
-define(HIST_REASON, <<"reason">>).
-define(HIST_ADDED, <<"added">>).
-define(HIST_STARTED, <<"started">>).
-define(HIST_STOPPED, <<"stopped">>).
-define(HIST_PENDING, <<"pending">>).
-define(HIST_CRASHED, <<"crashed">>).

-define(REP_DB_NAME, <<"_replicator">>).

% Can be used as a guard
-define(IS_REP_DB(X), (X =:= ?REP_DB_NAME orelse
    binary_part(X, {byte_size(X), -12}) =:= <<"/_replicator">>)).


-type rep_id() :: binary().
-type job_id() :: binary().
-type user_name() :: binary() | null.
-type db_doc_id() :: {binary(), binary() | '_'}.
-type seconds() :: non_neg_integer().
