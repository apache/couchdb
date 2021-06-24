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
-module(couch_views_ddoc).


-export([
    get_interactive_list/1,
    get_mango_list/1,
    is_interactive/1
]).


-include_lib("couch/include/couch_db.hrl").


% TODO: build a ddoc cache that checks the md_version
get_interactive_list(Db) ->
    DDocs = fabric2_db:get_design_docs(Db),
    lists:filter(fun is_interactive/1, DDocs).


get_mango_list(Db) ->
    DDocs = fabric2_db:get_design_docs(Db),
    lists:filter(fun (DDoc) ->
        {Props} = couch_doc:to_json_obj(DDoc, []),
        fabric2_util:get_value(<<"language">>, Props) == <<"query">>
    end, DDocs).


is_interactive(#doc{} = DDoc) ->
    {Props} = couch_doc:to_json_obj(DDoc, []),
    {Opts} = fabric2_util:get_value(<<"options">>, Props, {[]}),
    fabric2_util:get_value(<<"interactive">>, Opts, false).
