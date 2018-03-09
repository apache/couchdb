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

-module(couch_index_plugin_couch_db).

-export([
    before_copy_purge_info/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


before_copy_purge_info(DbName) ->
    {ok, DDocs} = design_docs(DbName),
    lists:map(fun(DDoc) ->
        JsonDDoc = couch_doc:from_json_obj(DDoc),
        couch_mrview_index:maybe_create_local_purge_doc(DbName, JsonDDoc)
    end, DDocs).

%% Internal functions

design_docs(DbName) ->
    try
        case fabric:design_docs(mem3:dbname(DbName)) of
            {error, {maintenance_mode, _, _Node}} ->
                {ok, []};
            Else ->
                Else
        end
    catch error:database_does_not_exist ->
        {ok, []}
    end.
