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

-define(REP_ID_VERSION, 3).

-record(rep, {
    id :: rep_id() | '_' | 'undefined',
    source :: any() | '_',
    target :: any() | '_',
    options :: [_] | '_',
    user_ctx :: any() | '_',
    type = db :: atom() | '_',
    view = nil :: any() | '_',
    doc_id :: any() | '_',
    db_name = null :: null | binary() | '_',
    start_time = {0, 0, 0} :: erlang:timestamp() | '_'
}).

-type rep_id() :: {string(), string()}.
-type db_doc_id() :: {binary(), binary() | '_'}.
-type seconds() :: non_neg_integer().
-type rep_start_result() ::
    {ok, rep_id()} |
    ignore |
    {temporary_error, binary()} |
    {permanent_failure, binary()}.


-record(doc_worker_result, {
    id :: db_doc_id(),
    wref :: reference(),
    result :: rep_start_result()
}).
