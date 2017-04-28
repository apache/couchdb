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

-module(couch_replicator_manager).

% TODO: This is a temporary proxy module to external calls (outside replicator)
%  to other replicator modules. This is done to avoid juggling multiple repos
% during development.

% NV: TODO: These functions were moved to couch_replicator_docs
% but it is still called from fabric_doc_update. Keep it here for now
% later, update fabric to call couch_replicator_docs instead
-export([before_doc_update/2, after_doc_read/2]).


before_doc_update(Doc, Db) ->
    couch_replicator_docs:before_doc_update(Doc, Db).

after_doc_read(Doc, Db) ->
    couch_replicator_docs:after_doc_read(Doc, Db).
