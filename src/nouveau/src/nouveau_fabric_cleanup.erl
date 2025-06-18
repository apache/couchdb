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

-module(nouveau_fabric_cleanup).

-include_lib("couch/include/couch_db.hrl").

-include("nouveau.hrl").
-include_lib("mem3/include/mem3.hrl").

-export([go/1]).

go(DbName) ->
    DesignDocs =
        case fabric:design_docs(DbName) of
            {ok, DDocs} when is_list(DDocs) ->
                DDocs;
            Else ->
                couch_log:debug("Invalid design docs: ~p~n", [Else]),
                []
        end,
    ActiveSigs =
        lists:usort(
            lists:flatmap(
                fun(Doc) -> active_sigs(DbName, Doc) end,
                [couch_doc:from_json_obj(DD) || DD <- DesignDocs]
            )
        ),
    Shards = mem3:shards(DbName),
    fabric_drop_seq:cleanup_peer_checkpoint_docs(DbName, <<"nouveau">>, ActiveSigs),
    lists:foreach(
        fun(Shard) ->
            rexi:cast(Shard#shard.node, {nouveau_rpc, cleanup, [Shard#shard.name, ActiveSigs]})
        end,
        Shards
    ).

active_sigs(DbName, #doc{} = Doc) ->
    Indexes = nouveau_util:design_doc_to_indexes(DbName, Doc),
    lists:map(fun(Index) -> Index#index.sig end, Indexes).
