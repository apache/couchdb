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

-define(DEFAULT_PRIORITY, 1.0).
-define(BAD_MAGIC_NUM, -12341234).

%% Dispatch Strategies
-define(DISPATCH_RANDOM, "random").
-define(DISPATCH_FD_HASH, "fd_hash").
-define(DISPATCH_SINGLE_SERVER, "single_server").
-define(DISPATCH_SERVER_PER_SCHEDULER, "server_per_scheduler").

%% Config Categories
-define(SHARD_CLASS_SEPARATOR, "||").
-define(IOQ2_CONFIG, "ioq2").
-define(IOQ2_BYPASS_CONFIG, "ioq2.bypass").
-define(IOQ2_SHARDS_CONFIG, "ioq2.shards").
-define(IOQ2_USERS_CONFIG, "ioq2.users").
-define(IOQ2_CLASSES_CONFIG, "ioq2.classes").


-define(DEFAULT_CLASS_PRIORITIES, [
    {customer, 1.0},
    {internal_repl, 0.001},
    {view_compact, 0.0001},
    {db_compact, 0.0001},
    {low, 0.0001},
    {db_meta, 1.0},

    {db_update, 1.0},
    {view_update, 1.0},
    {other, 1.0},
    {interactive, 1.0}
]).


-record(ioq_request, {
    fd,
    msg,
    key,
    init_priority = 1.0,
    fin_priority,
    ref,
    from,
    t0,
    tsub,
    shard,
    user,
    db,
    class,
    ddoc
}).


-record(ioq_file, {
    fd,
    ioq,
    tab,
    parallel=false,
    id_ref
}).


-type io_priority() :: db_compact
    | db_update
    | interactive
    | internal_repl
    | other
    | customer
    | db_meta
    | low.
-type view_io_priority() :: view_compact
    | view_update.
-type dbcopy_string() :: string(). %% "dbcopy"
-type dbname() :: binary() | dbcopy_string().
-type group_id() :: any().
-type io_dimensions() :: {io_priority(), dbname()}
    | {view_io_priority(), dbname(), group_id()}.
-type ioq_request() :: #ioq_request{}.

