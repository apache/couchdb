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

-module(ddoc_cache).


-export([
    open_doc/2,
    open_doc/3,
    open_validation_funs/1,
    open_custom/2,
    refresh/2,

    %% deprecated
    open/2
]).


open_doc(DbName, DocId) ->
    Key = {ddoc_cache_entry_ddocid, {DbName, DocId}},
    ddoc_cache_lru:open(Key).


open_doc(DbName, DocId, RevId) ->
    Key = {ddoc_cache_entry_ddocid_rev, {DbName, DocId, RevId}},
    ddoc_cache_lru:open(Key).


open_validation_funs(DbName) ->
    Key = {ddoc_cache_entry_validation_funs, DbName},
    ddoc_cache_lru:open(Key).


open_custom(DbName, Mod) ->
    Key = {ddoc_cache_entry_custom, {DbName, Mod}},
    ddoc_cache_lru:open(Key).


refresh(ShardDbName, DDocIds) when is_list(DDocIds) ->
    DbName = mem3:dbname(ShardDbName),
    ddoc_cache_lru:refresh(DbName, DDocIds).


open(DbName, validation_funs) ->
    open_validation_funs(DbName);
open(DbName, Module) when is_atom(Module) ->
    open_custom(DbName, Module);
open(DbName, <<"_design/", _/binary>>=DDocId) when is_binary(DbName) ->
    open_doc(DbName, DDocId);
open(DbName, DDocId) when is_binary(DDocId) ->
    open_doc(DbName, <<"_design/", DDocId/binary>>).
