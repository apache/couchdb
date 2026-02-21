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

-module(nouveau_plugin_couch_db).

-export([
    before_doc_update/3,
    is_valid_purge_client/2,
    on_compact/2
]).

-include("nouveau.hrl").
-include_lib("couch/include/couch_db.hrl").

%% New index definitions get an explicit lucene version property, if missing.
before_doc_update(
    #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>, revs = {0, []}} = Doc,
    Db,
    ?INTERACTIVE_EDIT = UpdateType
) ->
    #doc{body = {Fields}} = Doc,
    case couch_util:get_value(<<"nouveau">>, Fields) of
        {Indexes} when is_list(Indexes) ->
            [add_versions_to_doc(Doc), Db, UpdateType];
        _ ->
            [Doc, Db, UpdateType]
    end;
before_doc_update(Doc, Db, UpdateType) ->
    [Doc, Db, UpdateType].

add_versions_to_doc(#doc{} = Doc) ->
    #doc{body = {Fields0}} = Doc,
    {Indexes0} = couch_util:get_value(<<"nouveau">>, Fields0),
    Indexes1 = lists:map(fun add_version_to_index/1, Indexes0),
    Fields1 = couch_util:set_value(<<"nouveau">>, Fields0, {Indexes1}),
    Doc#doc{body = {Fields1}}.

add_version_to_index({IndexName, {Index}}) ->
    case couch_util:get_value(<<"lucene_version">>, Index) of
        undefined ->
            {IndexName,
                {couch_util:set_value(<<"lucene_version">>, Index, ?TARGET_LUCENE_VERSION)}};
        _ ->
            {IndexName, {Index}}
    end.

is_valid_purge_client(DbName, Props) ->
    nouveau_util:verify_index_exists(DbName, Props).

on_compact(DbName, DDocs) ->
    nouveau_util:ensure_local_purge_docs(DbName, DDocs).
