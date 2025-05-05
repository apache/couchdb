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

-module(mem3_reshard_dbdoc).

-export([
    update_shard_map/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("mem3_reshard.hrl").

update_shard_map(#job{source = Source, target = Target} = Job) ->
    DocId = mem3:dbname(Source#shard.name),
    JobStr = mem3_reshard_job:jobfmt(Job),
    LogMsg1 = "~p : ~p calling update_shard_map",
    couch_log:notice(LogMsg1, [?MODULE, JobStr]),
    try
        case mem3:get_db_doc(DocId) of
            {ok, #doc{} = Doc} ->
                #doc{body = Body} = Doc,
                NewBody = update_shard_props(Body, Source, Target),
                NewDoc = Doc#doc{body = NewBody},
                case mem3:update_db_doc(NewDoc) of
                    {ok, _} ->
                        ok;
                    {error, UpdateError} ->
                        exit(UpdateError)
                end,
                LogMsg2 = "~p : ~p update_shard_map returned",
                couch_log:notice(LogMsg2, [?MODULE, JobStr]),
                TimeoutMSec = shard_update_timeout_msec(),
                UntilSec = mem3_reshard:now_sec() + (TimeoutMSec div 1000),
                case wait_source_removed(Source, 5, UntilSec) of
                    true ->
                        ok;
                    false ->
                        exit(shard_update_did_not_propagate)
                end;
            Error ->
                exit(Error)
        end
    catch
        _:Err ->
            exit(Err)
    end.

update_shard_props({Props0}, #shard{} = Source, [#shard{} | _] = Targets) ->
    {ByNode0} = couch_util:get_value(<<"by_node">>, Props0, {[]}),
    ByNodeKV = {<<"by_node">>, {update_by_node(ByNode0, Source, Targets)}},
    Props1 = lists:keyreplace(<<"by_node">>, 1, Props0, ByNodeKV),

    {ByRange0} = couch_util:get_value(<<"by_range">>, Props1, {[]}),
    ByRangeKV = {<<"by_range">>, {update_by_range(ByRange0, Source, Targets)}},
    Props2 = lists:keyreplace(<<"by_range">>, 1, Props1, ByRangeKV),

    Changelog = couch_util:get_value(<<"changelog">>, Props2, []),
    {Node, Range} = {node_key(Source), range_key(Source)},
    TRanges = [range_key(T) || T <- Targets],
    ChangelogEntry = [[<<"split">>, Range, TRanges, Node]],
    ChangelogKV = {<<"changelog">>, Changelog ++ ChangelogEntry},
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
            throw({source_shard_missing_node, NodeKey, SourceNodes})
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
                    throw({target_shard_already_has_node, NodeKey, Nodes})
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
    mem3_util:range_to_hex([B, E]).

shard_update_timeout_msec() ->
    config:get_integer("reshard", "shard_update_timeout_msec", 300000).

wait_source_removed(#shard{name = Name} = Source, SleepSec, UntilSec) ->
    case check_source_removed(Source) of
        true ->
            true;
        false ->
            case mem3_reshard:now_sec() < UntilSec of
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
    SourcePresent = [
        S
     || S = #shard{name = S, node = N} <- Shards,
        S =:= Name,
        N =:= node()
    ],
    case SourcePresent of
        [] -> true;
        [_ | _] -> false
    end.
