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

-module(fabric_drop_seq).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").

-export([go/1]).

go(DbName) ->
    MrArgs = #mrargs{
        view_type = map,
        include_docs = true,
        extra = [
            {include_system, true},
            {namespace, <<"_local">>}
        ]
    },
    {ok, LocalDocs} = fabric:all_docs(DbName, fun all_docs_cb/2, [], MrArgs),
%    1. find lowest update seq per {node,uuid} pair in all checkpoints excluding mem3_sync
%    2. use mem3_sync docs to find lowest update seq on any other {node,uuids}
%    3. issue rpc to matching shards and ask them to set drop seq if uuid matches

    lists:foldl(fun parse_replication_doc/2, [], LocalDocs).

all_docs_cb({row, Row}, Acc0) ->
    Acc1 =
        case lists:keyfind(doc, 1, Row) of
            {doc, Doc} ->
                [couch_doc:from_json_obj(Doc) | Acc0];
            false ->
                Acc0
        end,
    {ok, Acc1};
all_docs_cb(_Else, Acc) ->
    {ok, Acc}.

parse_replication_doc(#doc{} = Doc, Acc) ->
    {Props} = Doc#doc.body,
    ReplicationIdVersion = couch_util:get_value(<<"replication_id_version">>, Props),
    case ReplicationIdVersion of
        ?REP_ID_VERSION ->
            SourceLastSeq = couch_util:get_value(<<"source_last_seq">>, Props),
            [decode_seq(SourceLastSeq) | Acc];
        _Else ->
            Acc
    end.

parse_shard_sync_doc(#doc{id = <<"_local/shard-sync-", _/binary>>} = Doc, Acc) ->
    {Props} = Doc#doc.body,
    History = couch_util:get_value(<<"history">>, Props),
    [History | Acc];
parse_shard_sync_doc(_Doc, Acc) ->
    Acc.


decode_seq(OpaqueSeq) ->
    Decoded = fabric_view_changes:decode_seq(OpaqueSeq),
    lists:foldl(
        fun
            ({_Node, Range, {Seq, Uuid, Node}}, Acc) ->
                Acc#{{Node, Range, Uuid} => Seq};
            ({_Node, _Range, _Seq}, Acc) ->
                Acc
        end,
        #{},
        Decoded
    ).

