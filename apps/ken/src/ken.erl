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

-module(ken).

-export([add/1]).
-export([remove/1]).
-export([add_all_shards/1]).

% Add a database shard to be indexed.
add(DbName) ->
    ken_server:add(DbName).

% Remove all pending jobs for a database shard.
remove(DbName) ->
    ken_server:remove(DbName).

% Add all shards for a database to be indexed.
add_all_shards(DbName) ->
    ken_server:add_all_shards(DbName).
