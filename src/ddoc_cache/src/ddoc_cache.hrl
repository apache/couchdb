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

-type dbname() :: iodata().
-type docid() :: iodata().
-type doc_hash() :: <<_:128>>.
-type revision() :: {pos_integer(), doc_hash()}.

-define(CACHE, ddoc_cache_entries).
-define(LRU, ddoc_cache_lru).
-define(REFRESH_TIMEOUT, 67000).
-define(SHUTDOWN_TIMEOUT, 1000).

-record(entry, {
    key,
    val,
    pid
}).

-record(opener, {
    key,
    pid,
    clients
}).


-ifdef(TEST).
-define(EVENT(Name, Arg), ddoc_cache_ev:event(Name, Arg)).
-else.
-define(EVENT(Name, Arg), ignore).
-endif.
