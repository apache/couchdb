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

-module(fabric_doc_purge).

-export([go/3]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

go(_, [], _) ->
    {ok, []};
go(DbName, AllIdsRevs, Opts) ->
    % tag each purge request with UUId
    {AllUUIDs, AllUUIDsIdsRevs, DocCount} = tag_docs(AllIdsRevs),

    Options = lists:delete(all_or_nothing, Opts),
    % Counters -> [{Worker, UUIDs}]
    {Counters, Workers} = dict:fold(fun(Shard, UUIDsIdsRevs, {Cs,Ws}) ->
        UUIDs = [UUID || {UUID, _Id, _Revs} <-UUIDsIdsRevs],
        #shard{name=Name, node=Node} = Shard,
        Ref = rexi:cast(Node,
            {fabric_rpc, purge_docs, [Name, UUIDsIdsRevs, Options]}),
        Worker = Shard#shard{ref=Ref},
        {[{Worker, UUIDs}|Cs], [Worker|Ws]}
    end, {[], []}, group_idrevs_by_shard(DbName, AllUUIDsIdsRevs)),

    RexiMon = fabric_util:create_monitors(Workers),
    W = couch_util:get_value(w, Options, integer_to_list(mem3:quorum(DbName))),
    Acc = {length(Workers), DocCount, list_to_integer(W), Counters, dict:new()},
    Timeout = fabric_util:request_timeout(),
    try rexi_utils:recv(Workers, #shard.ref,
        fun handle_message/3, Acc, infinity, Timeout) of
    {ok, {Health, Results}} when Health =:= ok; Health =:= accepted ->
        % Results-> [{UUID, {ok, Revs}}]
        {Health, [R || R <-
            couch_util:reorder_results(AllUUIDs, Results)]};
    {timeout, Acc1} ->
        {_, _, W1, Counters1, DocReplDict0} = Acc1,
        {DefunctWorkers, _} = lists:unzip(Counters1),
        fabric_util:log_timeout(DefunctWorkers, "purge_docs"),
        DocReplDict = lists:foldl(fun({_W, Docs}, Dict) ->
            Replies = [{error, timeout} || _D <- Docs],
            append_purge_replies(Docs, Replies, Dict)
        end, DocReplDict0, Counters1),
        {Health, _, Resp} = dict:fold(
            fun force_reply/3, {ok, W1, []}, DocReplDict),
        case Health of
            error -> timeout;
            _ -> {Health, [R || R <-
                couch_util:reorder_results(AllUUIDs, Resp)]}

        end;
    Else ->
        Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Worker, Acc0) ->
    {_, DocCount, W, Counters, DocsDict0} = Acc0,
    {FailCounters, NewCounters} = lists:partition(fun({#shard{node=N}, _}) ->
        N == NodeRef
    end, Counters),
    % fill DocsDict with error messages for relevant Docs
    DocsDict = lists:foldl(fun({_W, Docs}, CDocsDict) ->
        Replies = [{error, internal_server_error} || _D <- Docs],
        append_purge_replies(Docs, Replies, CDocsDict)
    end, DocsDict0, FailCounters),
    skip_message({length(NewCounters), DocCount, W, NewCounters, DocsDict});
handle_message({rexi_EXIT, _}, Worker, Acc0) ->
    {WC, DocCount , W, Counters, DocsDict0} = Acc0,
    % fill DocsDict with error messages for relevant Docs
    {value, {_W, Docs}, NewCounters} = lists:keytake(Worker, 1, Counters),
    Replies = [{error, internal_server_error} || _D <- Docs],
    DocsDict = append_purge_replies(Docs, Replies, DocsDict0),
    skip_message({WC-1, DocCount, W, NewCounters, DocsDict});
handle_message({ok, Replies0}, Worker, Acc0) ->
    {WCount, DocCount, W, Counters, DocsDict0} = Acc0,
    {value, {_W, Docs}, NewCounters} = lists:keytake(Worker, 1, Counters),
    DocsDict = append_purge_replies(Docs, Replies0, DocsDict0),
    case {WCount, dict:size(DocsDict)} of
    {1, _} ->
        % last message has arrived, we need to conclude things
        {Health, W, Replies} = dict:fold(fun force_reply/3, {ok, W, []},
           DocsDict),
        {stop, {Health, Replies}};
    {_, DocCount} ->
        % we've got at least one reply for each document, let's take a look
        case dict:fold(fun maybe_reply/3, {stop,W,[]}, DocsDict) of
        continue ->
            {ok, {WCount - 1, DocCount, W, NewCounters, DocsDict}};
        {stop, W, Replies} ->
            {stop, {ok, Replies}}
        end;
    _ ->
        {ok, {WCount - 1, DocCount, W, NewCounters, DocsDict}}
    end;
handle_message({error, purged_docs_limit_exceeded}=Error, Worker, Acc0) ->
    {WC, DocCount , W, Counters, DocsDict0} = Acc0,
    % fill DocsDict with error messages for relevant Docs
    {value, {_W, Docs}, NewCounters} = lists:keytake(Worker, 1, Counters),
    Replies = [Error || _D <- Docs],
    DocsDict = append_purge_replies(Docs, Replies, DocsDict0),
    skip_message({WC-1, DocCount, W, NewCounters, DocsDict});
handle_message({bad_request, Msg}, _, _) ->
    throw({bad_request, Msg}).


tag_docs(AllIdsRevs) ->
    {UUIDs, UUIDsIdsRevs, DocCount} = lists:foldl(fun(
        {Id, Revs}, {UAcc, UIRAcc, C}) ->
        UUID = couch_uuids:new(),
        {[UUID|UAcc], [{UUID, Id, Revs}|UIRAcc], C+1}
    end, {[], [], 0}, AllIdsRevs),
    {lists:reverse(UUIDs), lists:reverse(UUIDsIdsRevs), DocCount}.


force_reply(Doc, Replies, {Health, W, Acc}) ->
    case update_quorum_met(W, Replies) of
    {true, FinalReply} ->
        {Health, W, [{Doc, FinalReply} | Acc]};
    false ->
        case [Reply || {ok, Reply} <- Replies] of
        [] ->
            UReplies = lists:usort(Replies),
            case UReplies of
                [{error, internal_server_error}] ->
                    {error, W, [{Doc, {error, internal_server_error}} | Acc]};
                [{error, timeout}] ->
                    {error, W, [{Doc, {error, timeout}} | Acc]};
                [FirstReply|[]] ->
                    % check if all errors are identical, if so inherit health
                    {Health, W, [{Doc, FirstReply} | Acc]};
                _ ->
                    {error, W, [{Doc, UReplies} | Acc]}
             end;
        AcceptedReplies0 ->
            NewHealth = case Health of ok -> accepted; _ -> Health end,
            AcceptedReplies = lists:usort(lists:flatten(AcceptedReplies0)),
            {NewHealth, W, [{Doc, {accepted, AcceptedReplies}} | Acc]}
        end
    end.


maybe_reply(_, _, continue) ->
    % we didn't meet quorum for all docs, so we're fast-forwarding the fold
    continue;
maybe_reply(Doc, Replies, {stop, W, Acc}) ->
    case update_quorum_met(W, Replies) of
    {true, Reply} ->
        {stop, W, [{Doc, Reply} | Acc]};
    false ->
        continue
    end.

update_quorum_met(W, Replies) ->
    OkReplies = lists:foldl(fun(Reply, PrevsAcc) ->
        case Reply of
            {ok, PurgedRevs} -> [PurgedRevs | PrevsAcc];
            _ -> PrevsAcc
        end
    end, [], Replies),
    if length(OkReplies) < W -> false; true ->
        % make a union of PurgedRevs
        FinalReply = {ok, lists:usort(lists:flatten(OkReplies))},
        {true, FinalReply}
    end.


group_idrevs_by_shard(DbName, UUIDsIdsRevs) ->
    lists:foldl(fun({_UUID, Id, _Revs} = UUIDIdRevs, D0) ->
        lists:foldl(fun(Shard, D1) ->
            dict:append(Shard, UUIDIdRevs, D1)
        end, D0, mem3:shards(DbName, Id))
    end, dict:new(), UUIDsIdsRevs).


append_purge_replies([], [], DocReplyDict) ->
    DocReplyDict;
append_purge_replies([Doc|Rest1], [Reply|Rest2], Dict0) ->
    append_purge_replies(Rest1, Rest2, dict:append(Doc, Reply, Dict0)).


skip_message({0, _, W, _, DocsDict}) ->
    {Health, W, Reply} = dict:fold(fun force_reply/3, {ok, W, []}, DocsDict),
    {stop, {Health, Reply}};
skip_message(Acc0) ->
    {ok, Acc0}.


% eunits
doc_purge_ok_test() ->
    meck:new(couch_log),
    meck:expect(couch_log, warning, fun(_,_) -> ok end),
    meck:expect(couch_log, notice, fun(_,_) -> ok end),

    Revs1 = [{1, <<"rev11">>}], UUID1 = <<"3de03c5f4c2cd34cc515a9d1ea000abd">>,
    UUIDIdRevs1 = {UUID1, <<"id1">>, Revs1},
    Revs2 = [{1, <<"rev12">>}], UUID2 = <<"4de03c5f4c2cd34cc515a9d1ea000abc">>,
    UUIDIdRevs2 = {UUID2, <<"id2">>, Revs2},
    UUIDsIDdsRevs = [UUIDIdRevs1, UUIDIdRevs2],
    Shards =
        mem3_util:create_partition_map("foo",3,1,["node1","node2","node3"]),
    Counters = dict:to_list(
        group_idrevs_by_shard_hack(<<"foo">>, Shards, UUIDsIDdsRevs)),
    DocsDict = dict:new(),

    % ***test for W = 2
    AccW2 = {length(Shards), length(UUIDsIDdsRevs), list_to_integer("2"),
        Counters, DocsDict},
    {ok, {WaitingCountW2_1,_,_,_,_} = AccW2_1} =
        handle_message({ok,[{ok, Revs1}, {ok, Revs2}]}, hd(Shards), AccW2),
    ?assertEqual(2, WaitingCountW2_1),
    {stop, FinalReplyW2 } =
        handle_message({ok, [{ok, Revs1}, {ok, Revs2}]},
            lists:nth(2,Shards), AccW2_1),
    ?assertEqual(
        {ok, [{UUID1, {ok, Revs1}}, {UUID2, {ok, Revs2}}]},
        FinalReplyW2
    ),

    % ***test for W = 3
    AccW3 = {length(Shards), length(UUIDsIDdsRevs), list_to_integer("3"),
        Counters, DocsDict},
    {ok, {WaitingCountW3_1,_,_,_,_} = AccW3_1} =
        handle_message({ok, [{ok, Revs1}, {ok, Revs2}]}, hd(Shards), AccW3),
    ?assertEqual(2, WaitingCountW3_1),
    {ok, {WaitingCountW3_2,_,_,_,_} = AccW3_2} =
        handle_message({ok,[{ok, Revs1}, {ok, Revs2}]},
            lists:nth(2,Shards), AccW3_1),
    ?assertEqual(1, WaitingCountW3_2),
    {stop, FinalReplyW3 } =
        handle_message({ok, [{ok, Revs1}, {ok, Revs2}]},
            lists:nth(3,Shards), AccW3_2),
    ?assertEqual(
        {ok, [{UUID1, {ok, Revs1}}, {UUID2, {ok, Revs2}}]},
        FinalReplyW3
    ),

    % *** test rexi_exit on 1 node
    Acc0 = {length(Shards), length(UUIDsIDdsRevs), list_to_integer("2"),
        Counters, DocsDict},
    {ok, {WaitingCount1,_,_,_,_} = Acc1} =
        handle_message({ok, [{ok, Revs1}, {ok, Revs2}]}, hd(Shards), Acc0),
    ?assertEqual(2, WaitingCount1),
    {ok, {WaitingCount2,_,_,_,_} = Acc2} =
        handle_message({rexi_EXIT, nil}, lists:nth(2,Shards), Acc1),
    ?assertEqual(1, WaitingCount2),
    {stop, Reply} =
        handle_message({ok, [{ok, Revs1}, {ok, Revs2}]},
            lists:nth(3,Shards), Acc2),
    ?assertEqual(
        {ok,[{UUID1, {ok, Revs1}}, {UUID2, {ok, Revs2}}]},
        Reply
    ),

    % *** test {error, purge_during_compaction_exceeded_limit} on all nodes
    % *** still should return ok reply for the request
    ErrPDCEL = {error, purge_during_compaction_exceeded_limit},
    Acc20 = {length(Shards), length(UUIDsIDdsRevs), list_to_integer("3"),
        Counters, DocsDict},
    {ok, {WaitingCount21,_,_,_,_} = Acc21} =
        handle_message({ok, [ErrPDCEL, ErrPDCEL]}, hd(Shards), Acc20),
    ?assertEqual(2, WaitingCount21),
    {ok, {WaitingCount22,_,_,_,_} = Acc22} =
        handle_message({ok, [ErrPDCEL, ErrPDCEL]}, lists:nth(2,Shards), Acc21),
    ?assertEqual(1, WaitingCount22),
    {stop, Reply2 } =
        handle_message({ok, [ErrPDCEL, ErrPDCEL]}, lists:nth(3,Shards), Acc22),
    ?assertEqual(
        {ok, [{UUID1, ErrPDCEL}, {UUID2, ErrPDCEL}]},
        Reply2
    ),

    % *** test {error, purged_docs_limit_exceeded} on all nodes
    % *** still should return ok reply for the request
    ErrPDLE = {error, purged_docs_limit_exceeded},
    Acc30 = {length(Shards), length(UUIDsIDdsRevs), list_to_integer("3"),
        Counters, DocsDict},
    {ok, {WaitingCount31,_,_,_,_} = Acc31} =
        handle_message({ok, [ErrPDLE, ErrPDLE]}, hd(Shards), Acc30),
    ?assertEqual(2, WaitingCount31),
    {ok, {WaitingCount32,_,_,_,_} = Acc32} =
        handle_message({ok, [ErrPDLE, ErrPDLE]}, lists:nth(2,Shards), Acc31),
    ?assertEqual(1, WaitingCount32),
    {stop, Reply3 } =
        handle_message({ok, [ErrPDLE, ErrPDLE]},lists:nth(3,Shards), Acc32),
    ?assertEqual(
        {ok, [{UUID1, ErrPDLE}, {UUID2, ErrPDLE}]},
        Reply3
    ),
    meck:unload(couch_log).


doc_purge_accepted_test() ->
    meck:new(couch_log),
    meck:expect(couch_log, warning, fun(_,_) -> ok end),
    meck:expect(couch_log, notice, fun(_,_) -> ok end),

    Revs1 = [{1, <<"rev11">>}], UUID1 = <<"3de03c5f4c2cd34cc515a9d1ea000abd">>,
    UUIDIdRevs1 = {UUID1, <<"id1">>, Revs1},
    Revs2 = [{1, <<"rev12">>}], UUID2 = <<"4de03c5f4c2cd34cc515a9d1ea000abc">>,
    UUIDIdRevs2 = {UUID2, <<"id2">>, Revs2},
    UUIDsIDdsRevs = [UUIDIdRevs1, UUIDIdRevs2],
    Shards =
        mem3_util:create_partition_map("foo",3,1,["node1","node2","node3"]),
    Counters = dict:to_list(
        group_idrevs_by_shard_hack(<<"foo">>, Shards, UUIDsIDdsRevs)),
    DocsDict = dict:new(),

    % *** test rexi_exit on 2 nodes
    Acc0 = {length(Shards), length(UUIDsIDdsRevs), list_to_integer("2"),
        Counters, DocsDict},
    {ok, {WaitingCount1,_,_,_,_} = Acc1} =
        handle_message({ok, [{ok, Revs1}, {ok, Revs2}]}, hd(Shards), Acc0),
    ?assertEqual(2, WaitingCount1),
    {ok, {WaitingCount2,_,_,_,_} = Acc2} =
        handle_message({rexi_EXIT, nil}, lists:nth(2, Shards), Acc1),
    ?assertEqual(1, WaitingCount2),
    {stop, Reply} =
        handle_message({rexi_EXIT, nil}, lists:nth(3, Shards), Acc2),
    ?assertEqual(
        {accepted, [{UUID1, {accepted, Revs1}}, {UUID2, {accepted, Revs2}}]},
        Reply
    ),
    meck:unload(couch_log).


doc_purge_error_test() ->
    meck:new(couch_log),
    meck:expect(couch_log, warning, fun(_,_) -> ok end),
    meck:expect(couch_log, notice, fun(_,_) -> ok end),

    Revs1 = [{1, <<"rev11">>}], UUID1 = <<"3de03c5f4c2cd34cc515a9d1ea000abd">>,
    UUIDIdRevs1 = {UUID1, <<"id1">>, Revs1},
    Revs2 = [{1, <<"rev12">>}], UUID2 = <<"4de03c5f4c2cd34cc515a9d1ea000abc">>,
    UUIDIdRevs2 = {UUID2, <<"id2">>, Revs2},
    UUIDsIDdsRevs = [UUIDIdRevs1, UUIDIdRevs2],
    Shards =
        mem3_util:create_partition_map("foo",3,1,["node1","node2","node3"]),
    Counters = dict:to_list(
        group_idrevs_by_shard_hack(<<"foo">>, Shards, UUIDsIDdsRevs)),
    DocsDict = dict:new(),

    % *** test rexi_exit on all 3 nodes
    Acc0 = {length(Shards), length(UUIDsIDdsRevs), list_to_integer("2"),
        Counters, DocsDict},
    {ok, {WaitingCount1,_,_,_,_} = Acc1} =
        handle_message({rexi_EXIT, nil}, hd(Shards), Acc0),
    ?assertEqual(2, WaitingCount1),
    {ok, {WaitingCount2,_,_,_,_} = Acc2} =
        handle_message({rexi_EXIT, nil}, lists:nth(2,Shards), Acc1),
    ?assertEqual(1, WaitingCount2),
    {stop, Reply} =
        handle_message({rexi_EXIT, nil}, lists:nth(3,Shards), Acc2),
    ?assertEqual(
        {error, [{UUID1, {error, internal_server_error}},
            {UUID2, {error, internal_server_error}}]},
        Reply
    ),

    % ***test w quorum > # shards, which should fail immediately
    Shards2 = mem3_util:create_partition_map("foo",1,1,["node1"]),
    Counters2 = dict:to_list(
        group_idrevs_by_shard_hack(<<"foo">>, Shards2, UUIDsIDdsRevs)),
    AccW4 = {length(Shards2), length(UUIDsIDdsRevs), list_to_integer("2"),
        Counters2, DocsDict},
    Bool =
        case handle_message({ok, [{ok, Revs1}, {ok, Revs2}]},
                hd(Shards), AccW4) of
            {stop, _Reply} ->
                true;
            _ -> false
        end,
    ?assertEqual(true, Bool),

    % *** test Docs with no replies should end up as {error, internal_server_error}
    SA1 = #shard{node = a, range = [1]},
    SA2 = #shard{node = a, range = [2]},
    SB1 = #shard{node = b, range = [1]},
    SB2 = #shard{node = b, range = [2]},
    Counters3 = [{SA1,[UUID1]}, {SB1,[UUID1]},
        {SA2,[UUID2]}, {SB2,[UUID2]}],
    Acc30 = {length(Counters3), length(UUIDsIDdsRevs), 2, Counters3, DocsDict},
    {ok, Acc31} = handle_message({ok, [{ok, Revs1}]}, SA1, Acc30),
    {ok, Acc32} = handle_message({rexi_EXIT, nil}, SB1, Acc31),
    {ok, Acc33} = handle_message({rexi_EXIT, nil}, SA2, Acc32),
    {stop, Acc34} = handle_message({rexi_EXIT, nil}, SB2, Acc33),
    ?assertEqual(
        {error, [{UUID1, {accepted, Revs1}},
            {UUID2, {error, internal_server_error}}]},
        Acc34
    ),
    meck:unload(couch_log).


% needed for testing to avoid having to start the mem3 application
group_idrevs_by_shard_hack(_DbName, Shards, UUIDsIdsRevs) ->
    lists:foldl(fun({UUID, _Id, _Revs}, Dict0) ->
        lists:foldl(fun(Shard, Dict1) ->
            dict:append(Shard, UUID, Dict1)
        end, Dict0, Shards)
    end, dict:new(), UUIDsIdsRevs).
