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


-module(couch_replicator_fabric2_plugin).


-export([
    after_db_create/2,
    after_db_delete/2,
    after_doc_write/6
]).


after_db_create(DbName, DbUUID) ->
    couch_replicator:after_db_create(DbName, DbUUID),
    [DbName, DbUUID].


after_db_delete(DbName, DbUUID) ->
    couch_replicator:after_db_delete(DbName, DbUUID),
    [DbName, DbUUID].


after_doc_write(Db, Doc, Winner, OldWinner, RevId, Seq)->
    couch_replicator:after_doc_write(Db, Doc, Winner, OldWinner, RevId, Seq),
    [Db, Doc, Winner, OldWinner, RevId, Seq].
