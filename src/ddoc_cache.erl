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
    start/0,
    stop/0
]).

-export([
    open_doc/2,
    open_doc/3,
    open_validation_funs/1,
    evict/2,

    %% deprecated
    open/2
]).

start() ->
    application:start(ddoc_cache).

stop() ->
    application:stop(ddoc_cache).

open_doc(DbName, DocId) ->
    Key = {DbName, DocId, '_'},
    case ddoc_cache_opener:match_newest(Key) of
        {ok, _} = Resp ->
            Resp;
        missing ->
            ddoc_cache_opener:open_doc(DbName, DocId);
        recover ->
            ddoc_cache_opener:recover_doc(DbName, DocId)
    end.

open_doc(DbName, DocId, RevId) ->
    Key = {DbName, DocId, RevId},
    case ddoc_cache_opener:lookup(Key) of
        {ok, _} = Resp ->
            Resp;
        missing ->
            ddoc_cache_opener:open_doc(DbName, DocId, RevId);
        recover ->
            ddoc_cache_opener:recover_doc(DbName, DocId, RevId)
    end.

open_validation_funs(DbName) ->
    Key = {DbName, validation_funs},
    case ddoc_cache_opener:lookup(Key) of
        {ok, _} = Resp ->
            Resp;
        missing ->
            ddoc_cache_opener:open_validation_funs(DbName);
        recover ->
            ddoc_cache_opener:recover_validation_funs(DbName)
    end.

evict(ShardDbName, DDocIds) ->
    DbName = mem3:dbname(ShardDbName),
    ddoc_cache_opener:evict_docs(DbName, DDocIds).

open(DbName, validation_funs) ->
    open_validation_funs(DbName);
open(DbName, <<"_design/", _/binary>>=DDocId) when is_binary(DbName) ->
    open_doc(DbName, DDocId);
open(DbName, DDocId) when is_binary(DDocId) ->
    open_doc(DbName, <<"_design/", DDocId/binary>>).
