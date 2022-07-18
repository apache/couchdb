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

-module(ddoc_cache_entry_custom).

-export([
    dbname/1,
    ddocid/1,
    recover/1,
    insert/2
]).

dbname({DbName, _}) ->
    DbName.

ddocid(_) ->
    no_ddocid.

recover({DbName, Mod}) ->
    Mod:recover(DbName).

insert(_, _) ->
    ok.
