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

-module(mem3_shard_split_dbdoc).


-export([
    update_shard_map/1,
    update_shard_map_rpc/3
]).


-include_lib("couch/include/couch_db.hrl").
-include("mem3_shard_split.hrl").


update_shard_map(#job{source = Source, targets = Targets} = Job) ->
    couch_log:info("~p : replicating dbs to local node", [?MODULE]),
    case mem3_util:replicate_dbs_from_all_nodes(600000) of
        ok ->
            ok;
        Error ->
            error(Error)
    end,
    #shard{name = SourceName} = Source,
    DocId = mem3:dbname(SourceName),
    OldBody = case mem3_util:open_db_doc(DocId) of
        {ok, #doc{body = DocBody}} ->
            DocBody;
        OpenErr ->
            error({shard_doc_open_error, OpenErr})
    end,
    Node = hd(mem3_util:live_nodes()),
    Body = update_shard_props(OldBody, Source, Targets),
    JobStr = mem3_shard_split_job:jobfmt(Job),
    LogArgs1 = [?MODULE, JobStr, Node, OldBody, Body],
    couch_log:notice("~p : ~s node:~p doc ~p -> ~p", LogArgs1),
    case mem3_rpc:update_shard_map(Node, DocId, OldBody, Body, 600000) of
        {ok, Res} ->
            LogArgs2 = [?MODULE, JobStr, Node, Res],
            couch_log:notice("~p : ~s node:~p updated ~p", LogArgs2),
            UntilSec = mem3_shard_split:now_sec() + 600,
            case wait_source_removed(Source, 5, UntilSec) of
                true ->
                    ok;
                false ->
                    error({shard_update_did_not_propagate, Source})
            end;
        UpdateErr ->
            ErrArgs = [?MODULE, JobStr, Node, UpdateErr],
            couch_log:error("~p : ~s node:~p error:~p", ErrArgs),
            error({shard_doc_update_error, UpdateErr})
    end.


update_shard_map_rpc(DocId, OldBody, {[_ | _]} = Body) ->
    RepRes = mem3_util:replicate_dbs_from_all_nodes(600000),
    CurrNode = hd(mem3_util:live_nodes()),
    % Make sure we are still the coordinator
    case {node() == CurrNode, RepRes} of
        {true, ok} ->
            case mem3_util:open_db_doc(DocId) of
                {ok, Doc} ->
                    replicate_changes(write_shard_doc(Doc, OldBody, Body));
                Error ->
                    Error
            end;
       {false, _} ->
            {error, {coordinator_changed, node(), CurrNode}};
       {_, {error, RepError}} ->
            {error, RepError}
    end.


% Private

wait_source_removed(#shard{name = Name} = Source, SleepSec, UntilSec) ->
    case check_source_removed(Source) of
        true ->
            true;
        false ->
            case mem3_shard_split:now_sec() < UntilSec of
                true ->
                    LogMsg = "~p : Waiting for shard ~p removal confirmation",
                    couch_log:notice(LogMsg, [?MODULE, Name]),
                    timer:sleep(SleepSec * 1000),
                    wait_source_removed(Source, SleepSec, UntilSec);
                false ->
                    false
            end
    end.

check_source_removed(#shard{name = Name}) ->
    DbName = mem3:dbname(Name),
    Live = mem3_util:live_nodes(),
    ShardNodes = [N || #shard{node = N} <- mem3:shards(DbName)],
    Nodes = lists:usort([N || N <- ShardNodes, lists:member(N, Live)]),
    {Responses, _} = rpc:multicall(Nodes, mem3, shards, [DbName]),
    Shards = lists:usort(lists:flatten(Responses)),
    case [S || S = #shard{name = S, node = N} <- Shards, S =:= Name,
            N =:= node()] of
        [] ->  true;
        [_ | _] -> false
    end.


update_shard_props({Props0}, #shard{} = Source, [#shard{} | _] = Targets) ->
    {ByNode0} =  couch_util:get_value(<<"by_node">>, Props0, {[]}),
    ByNodeKV = {<<"by_node">>, {update_by_node(ByNode0, Source, Targets)}},
    Props1 = lists:keyreplace(<<"by_node">>, 1, Props0, ByNodeKV),

    {ByRange0} = couch_util:get_value(<<"by_range">>, Props1, {[]}),
    ByRangeKV = {<<"by_range">>, {update_by_range(ByRange0, Source, Targets)}},
    Props2 = lists:keyreplace(<<"by_range">>, 1, Props1, ByRangeKV),

    Changelog = couch_util:get_value(<<"changelog">>, Props2, []),
    {Node, Range} = {node_key(Source), range_key(Source)},
    ChangelogEntry = [[<<"split">>, Range, length(Targets), Node]],
    ChangelogKV = {<<"changelog">>,  Changelog ++ ChangelogEntry},
    Props3 = lists:keyreplace(<<"changelog">>, 1, Props2, ChangelogKV),

    {Props3}.


update_by_node(ByNode, #shard{} = Source, [#shard{} | _] = Targets) ->
    {NodeKey, SKey} = {node_key(Source), range_key(Source)},
    {_, Ranges} = lists:keyfind(NodeKey, 1, ByNode),
    Ranges1 = Ranges -- [SKey],
    Ranges2 = Ranges1 ++ [range_key(T) || T <- Targets],
    lists:keyreplace(NodeKey, 1, ByNode, {NodeKey, lists:sort(Ranges2)}).


update_by_range(ByRange, Source, Targets) ->
    ByRange1 = remove_node_from_source(ByRange, Source),
    lists:foldl(fun add_node_to_target_foldl/2, ByRange1, Targets).


remove_node_from_source(ByRange, Source) ->
    {NodeKey, SKey} = {node_key(Source), range_key(Source)},
    {_, SourceNodes} = lists:keyfind(SKey, 1, ByRange),
    % Double check that source had node to begin with
    case lists:member(NodeKey, SourceNodes) of
        true ->
            ok;
        false ->
            error({source_shard_missing_node, NodeKey, SourceNodes})
    end,
    SourceNodes1 = SourceNodes -- [NodeKey],
    case SourceNodes1 of
        [] ->
            % If last node deleted, remove entry
            lists:keydelete(SKey, 1, ByRange);
        _ ->
            lists:keyreplace(SKey, 1, ByRange, {SKey, SourceNodes1})
    end.


add_node_to_target_foldl(#shard{} = Target, ByRange) ->
    {NodeKey, TKey} = {node_key(Target), range_key(Target)},
    case lists:keyfind(TKey, 1, ByRange) of
        {_, Nodes} ->
            % Double check that target does not have node already
            case lists:member(NodeKey, Nodes) of
                false ->
                    ok;
                true ->
                    error({target_shard_already_has_node, NodeKey, Nodes})
            end,
            Nodes1 = lists:sort([NodeKey | Nodes]),
            lists:keyreplace(TKey, 1, ByRange, {TKey, Nodes1});
        false ->
            % fabric_db_create:make_document/3 says they should be sorted
            lists:sort([{TKey, [NodeKey]} | ByRange])
    end.


node_key(#shard{node = Node}) ->
    couch_util:to_binary(Node).

range_key(#shard{range = [B, E]}) ->
    BHex = couch_util:to_hex(<<B:32/integer>>),
    EHex = couch_util:to_hex(<<E:32/integer>>),
    list_to_binary([BHex, "-", EHex]).


write_shard_doc(#doc{deleted = true, id = DocId}, _, _) ->
    {error, {shard_doc_deleted, DocId}};

write_shard_doc(#doc{body = CurrOldBody} = Doc, OldBody, Body) ->
    % Check when writing that the document body looks exactly like
    % the version the modifications were based off to start with
    SameBody = couch_util:ejsort(CurrOldBody) == couch_util:ejsort(OldBody),
    case SameBody of
        true ->
            DbName = ?l2b(config:get("mem3", "shards_db", "_dbs")),
            UpdatedDoc = Doc#doc{body = Body},
            couch_util:with_db(DbName, fun(Db) ->
                try
                    couch_db:update_doc(Db, UpdatedDoc, [])
                catch
                    conflict ->
                        {error, {conflict, CurrOldBody, UpdatedDoc}}
                end
            end);
        false ->
            {error, {conflict, CurrOldBody, OldBody}}
    end.


replicate_changes({ok, Res}) ->
    couch_log:notice("~p : shard doc written, start replication", [?MODULE]),
    case mem3_util:replicate_dbs_to_all_nodes(300000) of
        ok ->
            couch_log:notice("~p : dbs replicated successfully", [?MODULE]),
            {ok, Res};
        {error, Error} ->
            couch_log:error("~p : dbs replication error: ~p", [?MODULE, Error]),
            {error, Error}
    end;

replicate_changes(Error) ->
    Error.
