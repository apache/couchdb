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


-module(couch_views_fabric2_plugin).


-export([
    after_doc_write/6
]).


after_doc_write(Db, Doc, NewWinner, OldWinner, NewRevId, Seq)->
    couch_views_updater:index(Db, Doc, NewWinner, OldWinner, NewRevId, Seq),
    [Db, Doc, NewWinner, OldWinner, NewRevId, Seq].
