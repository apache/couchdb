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

-include_lib("mem3/include/mem3.hrl").


-type range_pos() :: non_neg_integer().
-type split() :: pos_integer().  % also power of 2
-type job_id() :: binary() | undefined.
-type job_type() :: split.
-type time_sec() :: non_neg_integer().

-type shard_split_main_state() ::
    running |
    stopped.

-type job_state() ::
    new |
    running |
    stopped |
    failed |
    completed.

-type split_state() ::
    new |
    initial_copy |
    topoff1 |
    build_indices |
    topoff2 |
    copy_local_docs |
    update_shardmap |
    wait_source_close |
    topoff3 |
    source_delete |
    completed.


-record(job, {
    id :: job_id() | '$1' | '_',
    type :: job_type(),
    job_state :: job_state(),
    split_state :: split_state(),
    state_info = [] :: [{atom(), any()}],
    source :: #shard{},
    targets :: [#shard{}],
    history = [] :: [{atom(), time_sec()}],
    start_time = 0 :: non_neg_integer(),
    update_time = 0 :: non_neg_integer(),
    node :: node(),
    pid :: undefined | pid() | '$1' | '_',
    ref :: undefined | reference() | '_',
    manager :: undefined | pid(),
    workers = [] :: [pid()],
    retries = 0 :: non_neg_integer()
}).

-record(state, {
    state :: shard_split_main_state(),
    state_info :: [],
    update_time :: non_neg_integer(),
    job_prefix :: binary(),
    state_id :: binary(),
    db_monitor :: pid(),
    node :: node()
}).


-define(SPLIT_STATES, [
    new,
    initial_copy,
    topoff1,
    build_indices,
    topoff2,
    copy_local_docs,
    update_shardmap,
    wait_source_close,
    topoff3,
    source_delete,
    completed
]).
