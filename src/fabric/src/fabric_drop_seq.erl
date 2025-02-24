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
    %    1. find lowest update seq per {node,uuid} pair in all peer checkpoints excluding mem3_sync
    %    2. use mem3_sync docs to find lowest update seq on any other {node,uuids}
    %    3. issue rpc to matching shards and ask them to set drop seq if uuid matches

    PeerCheckpoints = lists:foldl(fun parse_peer_doc/2, #{}, LocalDocs),
    ShardSyncHistory = lists:foldl(fun parse_shard_sync_doc/2, [], LocalDocs),
    {PeerCheckpoints, ShardSyncHistory}.

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

parse_peer_doc(#doc{} = Doc, Acc) ->
    {Props} = Doc#doc.body,
    Type = couch_util:get_value(<<"type">>, Props),
    case Type of
        <<"_peer_checkpoint">> ->
            UpdateSeq = couch_util:get_value(<<"update_seq">>, Props),
            maps:merge_with(fun combine_peers/3, decode_seq(UpdateSeq), Acc);
        _Else ->
            Acc
    end.

parse_shard_sync_doc(#doc{id = <<"_local/shard-sync-", _/binary>>} = Doc, Acc) ->
    {Props} = Doc#doc.body,
    {[{_Node, History}]} = couch_util:get_value(<<"history">>, Props),
    L = [
         {couch_util:get_value(<<"source_node">>, Item),
          couch_util:get_value(<<"source_uuid">>, Item),
          couch_util:get_value(<<"source_seq">>, Item),
          couch_util:get_value(<<"target_node">>, Item),
          couch_util:get_value(<<"target_uuid">>, Item),
          couch_util:get_value(<<"target_seq">>, Item)
         } || {Item} <- History],
    L ++ Acc;
parse_shard_sync_doc(_Doc, Acc) ->
    Acc.

decode_seq(OpaqueSeq) ->
    Decoded = fabric_view_changes:decode_seq(OpaqueSeq),
    lists:foldl(
        fun
            ({_Node, _Range, {Seq, Uuid, Node}}, Acc) ->
                Acc#{{Node, Uuid} => Seq};
            ({_Node, _Range, _Seq}, Acc) ->
                Acc
        end,
        #{},
        Decoded
    ).

combine_peers(_Key, Val1, Val2) when is_integer(Val1), is_integer(Val2) ->
    min(Val1, Val2).
