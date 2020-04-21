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

-behaviour(gen_server).

-export([
    update_shard_map/1,

    start_link/0,

    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include_lib("couch/include/couch_db.hrl").
-include("mem3_reshard.hrl").


-spec update_shard_map(#job{}) -> no_return | ok.
update_shard_map(#job{source = Source, target = Target} = Job) ->
    Node = hd(mem3_util:live_nodes()),
    JobStr = mem3_reshard_job:jobfmt(Job),
    LogMsg1 = "~p : ~p calling update_shard_map node:~p",
    couch_log:notice(LogMsg1, [?MODULE, JobStr, Node]),
    ServerRef = {?MODULE, Node},
    CallArg = {update_shard_map, Source, Target},
    TimeoutMSec = shard_update_timeout_msec(),
    try
        case gen_server:call(ServerRef, CallArg, TimeoutMSec) of
            {ok, _} -> ok;
            {error, CallError} -> throw({error, CallError})
        end
    catch
        _:Err ->
            exit(Err)
    end,
    LogMsg2 = "~p : ~p update_shard_map on node:~p returned",
    couch_log:notice(LogMsg2, [?MODULE, JobStr, Node]),
    UntilSec = mem3_reshard:now_sec() + (TimeoutMSec div 1000),
    case wait_source_removed(Source, 5, UntilSec) of
        true -> ok;
        false -> exit(shard_update_did_not_propagate)
    end.


-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    couch_log:notice("~p start init()", [?MODULE]),
    {ok, nil}.


terminate(_Reason, _State) ->
    ok.


handle_call({update_shard_map, Source, Target}, _From, State) ->
    Res = try
        update_shard_map(Source, Target)
    catch
        throw:{error, Error} ->
            {error, Error}
    end,
    {reply, Res, State};

handle_call(Call, From, State) ->
    couch_log:error("~p unknown call ~p from: ~p", [?MODULE, Call, From]),
    {noreply, State}.


handle_cast(Cast, State) ->
    couch_log:error("~p unexpected cast ~p", [?MODULE, Cast]),
    {noreply, State}.


handle_info(Info, State) ->
    couch_log:error("~p unexpected info ~p", [?MODULE, Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private

update_shard_map(Source, Target) ->
    ok = validate_coordinator(),
    ok = replicate_from_all_nodes(shard_update_timeout_msec()),
    DocId = mem3:dbname(Source#shard.name),
    OldDoc = case mem3_util:open_db_doc(DocId) of
        {ok, #doc{deleted = true}} ->
            throw({error, missing_source});
        {ok, #doc{} = Doc} ->
            Doc;
        {not_found, deleted} ->
            throw({error, missing_source});
        OpenErr ->
            throw({error, {shard_doc_open_error, OpenErr}})
    end,
    #doc{body = OldBody} = OldDoc,
    NewBody = update_shard_props(OldBody, Source, Target),
    {ok, _}  = write_shard_doc(OldDoc, NewBody),
    ok = replicate_to_all_nodes(shard_update_timeout_msec()),
    {ok, NewBody}.


validate_coordinator() ->
    case hd(mem3_util:live_nodes()) =:= node() of
        true -> ok;
        false -> throw({error, coordinator_changed})
    end.


replicate_from_all_nodes(TimeoutMSec) ->
    case mem3_util:replicate_dbs_from_all_nodes(TimeoutMSec) of
        ok -> ok;
        Error -> throw({error, Error})
    end.


replicate_to_all_nodes(TimeoutMSec) ->
    case mem3_util:replicate_dbs_to_all_nodes(TimeoutMSec) of
        ok -> ok;
        Error -> throw({error, Error})
    end.


write_shard_doc(#doc{id = Id} = Doc, Body) ->
    UpdatedDoc = Doc#doc{body = Body},
    couch_util:with_db(mem3_sync:shards_db(), fun(Db) ->
        try
            {ok, _} = couch_db:update_doc(Db, UpdatedDoc, [])
        catch
            conflict ->
                throw({error, {conflict, Id, Doc#doc.body, UpdatedDoc}})
        end
    end).


update_shard_props({Props0}, #shard{} = Source, [#shard{} | _] = Targets) ->
    {ByNode0} =  couch_util:get_value(<<"by_node">>, Props0, {[]}),
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
    BHex = couch_util:to_hex(<<B:32/integer>>),
    EHex = couch_util:to_hex(<<E:32/integer>>),
    list_to_binary([BHex, "-", EHex]).


shard_update_timeout_msec() ->
    config:get_integer("reshard", "shard_upate_timeout_msec", 300000).


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
    SourcePresent = [S || S = #shard{name = S, node = N} <- Shards, S =:= Name,
        N =:= node()],
    case SourcePresent of
        [] ->  true;
        [_ | _] -> false
    end.
