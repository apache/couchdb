% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_fabric_cleanup).

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([go/1]).

go(DbName) ->
    {ok, DesignDocs} = fabric:design_docs(DbName),
    ActiveSigs = lists:usort(lists:flatmap(fun active_sigs/1,
        [couch_doc:from_json_obj(DD) || DD <- DesignDocs])),
    clouseau_rpc:cleanup(DbName, ActiveSigs),
    ok.

active_sigs(#doc{body={Fields}}=Doc) ->
    {RawIndexes} = couch_util:get_value(<<"indexes">>, Fields, {[]}),
    {IndexNames, _} = lists:unzip(RawIndexes),
    [begin
         {ok, Index} = dreyfus_index:design_doc_to_index(Doc, IndexName),
         Index#index.sig
     end || IndexName <- IndexNames].
