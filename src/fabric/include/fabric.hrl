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

-record(collector, {
    db_name=nil,
    ddoc_name=nil,
    view_name=nil,
    query_args,
    callback,
    counters,
    buffer_size,
    blocked = [],
    total_rows = 0,
    offset = 0,
    rows = [],
    skip,
    limit,
    keys,
    os_proc,
    reducer,
    collation,
    lang,
    sorted,
    user_acc,
    update_seq
}).

-record(view_row, {key, id, value, doc, worker}).

-type row_property_key() :: id | key | value | doc | worker.
-type row_properties() :: [{row_property_key(), any()}].

-type view_row_map() ::
	#{
	  id => term(),
	  key => term(),
	  value => term(),
	  doc => term(),
	  worker => term()
	 }.

-type view_row() :: #view_row{} | {view_row, view_row_map()}.
