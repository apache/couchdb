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

-module(ddoc_cache_entry_ddocid_rev).

-export([
    dbname/1,
    ddocid/1,
    recover/1,
    insert/2
]).

-include_lib("couch/include/couch_db.hrl").

dbname({DbName, _, _}) ->
    DbName.

ddocid({_, DDocId, _}) ->
    DDocId.

recover({DbName, DDocId, Rev}) ->
    Opts = [ejson_body, ?ADMIN_CTX, ddoc_cache],
    {ok, [Resp]} = fabric:open_revs(DbName, DDocId, [Rev], Opts),
    Resp.

insert({DbName, DDocId, _Rev}, {ok, #doc{} = DDoc}) ->
    Key = {ddoc_cache_entry_ddocid, {DbName, DDocId}},
    spawn(fun() -> ddoc_cache_lru:insert(Key, DDoc) end);
insert(_, _) ->
    ok.
