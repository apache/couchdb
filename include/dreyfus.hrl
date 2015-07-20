% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-record(index, {
    current_seq=0,
    dbname,
    ddoc_id,
    analyzer,
    def,
    def_lang,
    name,
    sig=nil
}).

-record(grouping, {
    by=nil,
    groups=[],
    offset=0,
    limit=10,
    sort=relevance,
    new_api=true
}).

-record(index_query_args, {
    q,
    limit=25,
    stale=false,
    include_docs=false,
    bookmark=nil,
    sort=relevance,
    grouping=#grouping{},
    stable=false,
    counts=nil,
    ranges=nil,
    drilldown=[],
    include_fields=nil,
    highlight_fields=nil,
    highlight_pre_tag = <<"<em>">>,
    highlight_post_tag = <<"</em>">>,
    highlight_number=1,
    highlight_size=0,
    raw_bookmark=false
}).

-record(sortable, {
    order, % sort order
    shard, % originating shard
    item   % the item itself
}).

% Our local representation of top_docs, not equal to wire format.
-record(top_docs, {
    update_seq,
    total_hits,
    hits,
    counts,
    ranges
}).

%% These must match the case classes in clouseau.
-record(hit, {
    order,
    fields
}).
