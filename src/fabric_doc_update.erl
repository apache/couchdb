% Copyright 2010 Cloudant
%
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

-module(fabric_doc_update).

-export([go/3]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

go(_, [], _) ->
    {ok, []};
go(DbName, AllDocs, Opts) ->
    validate_atomic_update(DbName, AllDocs, lists:member(all_or_nothing, Opts)),
    Options = lists:delete(all_or_nothing, Opts),
    GroupedDocs = lists:map(fun({#shard{name=Name, node=Node} = Shard, Docs}) ->
        Ref = rexi:cast(Node, {fabric_rpc, update_docs, [Name, Docs, Options]}),
        {Shard#shard{ref=Ref}, Docs}
    end, group_docs_by_shard(DbName, AllDocs)),
    {Workers, _} = lists:unzip(GroupedDocs),
    RexiMon = fabric_util:create_monitors(Workers),
    W = couch_util:get_value(w, Options, integer_to_list(mem3:quorum(DbName))),
    Acc0 = {length(Workers), length(AllDocs), list_to_integer(W), GroupedDocs,
        dict:from_list([{Doc,[]} || Doc <- AllDocs])},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, {Health, Results}} when Health =:= ok; Health =:= accepted ->
        {Health, [R || R <- couch_util:reorder_results(AllDocs, Results), R =/= noreply]};
    {timeout, Acc} ->
        {_, _, W1, _, DocReplDict} = Acc,
        {Health, _, Resp} = dict:fold(fun force_reply/3, {ok, W1, []},
            DocReplDict),
        {Health, [R || R <- couch_util:reorder_results(AllDocs, Resp), R =/= noreply]};
    Else ->
        Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Worker, Acc0) ->
    {_, LenDocs, W, GroupedDocs, DocReplyDict} = Acc0,
    NewGrpDocs = [X || {#shard{node=N}, _} = X <- GroupedDocs, N =/= NodeRef],
    skip_message({length(NewGrpDocs), LenDocs, W, NewGrpDocs, DocReplyDict});

handle_message({rexi_EXIT, _}, Worker, Acc0) ->
    {WC,LenDocs,W,GrpDocs,DocReplyDict} = Acc0,
    NewGrpDocs = lists:keydelete(Worker,1,GrpDocs),
    skip_message({WC-1,LenDocs,W,NewGrpDocs,DocReplyDict});
handle_message(internal_server_error, Worker, Acc0) ->
    % happens when we fail to load validation functions in an RPC worker
    {WC,LenDocs,W,GrpDocs,DocReplyDict} = Acc0,
    NewGrpDocs = lists:keydelete(Worker,1,GrpDocs),
    skip_message({WC-1,LenDocs,W,NewGrpDocs,DocReplyDict});
handle_message({ok, Replies}, Worker, Acc0) ->
    {WaitingCount, DocCount, W, GroupedDocs, DocReplyDict0} = Acc0,
    {value, {_, Docs}, NewGrpDocs} = lists:keytake(Worker, 1, GroupedDocs),
    DocReplyDict = append_update_replies(Docs, Replies, DocReplyDict0),
    case {WaitingCount, dict:size(DocReplyDict)} of
    {1, _} ->
        % last message has arrived, we need to conclude things
        {Health, W, Reply} = dict:fold(fun force_reply/3, {ok, W, []},
           DocReplyDict),
        {stop, {Health, Reply}};
    {_, DocCount} ->
        % we've got at least one reply for each document, let's take a look
        case dict:fold(fun maybe_reply/3, {stop,W,[]}, DocReplyDict) of
        continue ->
            {ok, {WaitingCount - 1, DocCount, W, NewGrpDocs, DocReplyDict}};
        {stop, W, FinalReplies} ->
            {stop, {ok, FinalReplies}}
        end
    end;
handle_message({missing_stub, Stub}, _, _) ->
    throw({missing_stub, Stub});
handle_message({not_found, no_db_file} = X, Worker, Acc0) ->
    {_, _, _, GroupedDocs, _} = Acc0,
    Docs = couch_util:get_value(Worker, GroupedDocs),
    handle_message({ok, [X || _D <- Docs]}, Worker, Acc0).

force_reply(Doc, [], {_, W, Acc}) ->
    {error, W, [{Doc, {error, internal_server_error}} | Acc]};
force_reply(Doc, [FirstReply|_] = Replies, {Health, W, Acc}) ->
    case update_quorum_met(W, Replies) of
    {true, Reply} ->
        {Health, W, [{Doc,Reply} | Acc]};
    false ->
        twig:log(warn, "write quorum (~p) failed for ~s", [W, Doc#doc.id]),
        case [Reply || {ok, Reply} <- Replies] of
        [] ->
            % check if all errors are identical, if so inherit health
            case lists:all(fun(E) -> E =:= FirstReply end, Replies) of
            true ->
                {Health, W, [{Doc, FirstReply} | Acc]};
            false ->
                {error, W, [{Doc, FirstReply} | Acc]}
            end;
        [AcceptedRev | _] ->
            NewHealth = case Health of ok -> accepted; _ -> Health end,
            {NewHealth, W, [{Doc, {accepted,AcceptedRev}} | Acc]}
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
    Counters = lists:foldl(fun(R,D) -> orddict:update_counter(R,1,D) end,
        orddict:new(), Replies),
    case lists:dropwhile(fun({_, Count}) -> Count < W end, Counters) of
    [] ->
        false;
    [{FinalReply, _} | _] ->
        {true, FinalReply}
    end.

-spec group_docs_by_shard(binary(), [#doc{}]) -> [{#shard{}, [#doc{}]}].
group_docs_by_shard(DbName, Docs) ->
    dict:to_list(lists:foldl(fun(#doc{id=Id} = Doc, D0) ->
        lists:foldl(fun(Shard, D1) ->
            dict:append(Shard, Doc, D1)
        end, D0, mem3:shards(DbName,Id))
    end, dict:new(), Docs)).

append_update_replies([], [], DocReplyDict) ->
    DocReplyDict;
append_update_replies([Doc|Rest], [], Dict0) ->
    % icky, if replicated_changes only errors show up in result
    append_update_replies(Rest, [], dict:append(Doc, noreply, Dict0));
append_update_replies([Doc|Rest1], [Reply|Rest2], Dict0) ->
    % TODO what if the same document shows up twice in one update_docs call?
    append_update_replies(Rest1, Rest2, dict:append(Doc, Reply, Dict0)).

skip_message({0, _, W, _, DocReplyDict}) ->
    {Health, W, Reply} = dict:fold(fun force_reply/3, {ok, W, []}, DocReplyDict),
    {stop, {Health, Reply}};
skip_message(Acc0) ->
    {ok, Acc0}.

validate_atomic_update(_, _, false) ->
    ok;
validate_atomic_update(_DbName, AllDocs, true) ->
    % TODO actually perform the validation.  This requires some hackery, we need
    % to basically extract the prep_and_validate_updates function from couch_db
    % and only run that, without actually writing in case of a success.
    Error = {not_implemented, <<"all_or_nothing is not supported yet">>},
    PreCommitFailures = lists:map(fun(#doc{id=Id, revs = {Pos,Revs}}) ->
        case Revs of [] -> RevId = <<>>; [RevId|_] -> ok end,
        {{Id, {Pos, RevId}}, Error}
    end, AllDocs),
    throw({aborted, PreCommitFailures}).

% eunits
doc_update1_test() ->
    Doc1 = #doc{revs = {1,[<<"foo">>]}},
    Doc2 = #doc{revs = {1,[<<"bar">>]}},
    Docs = [Doc1],
    Docs2 = [Doc2, Doc1],
    Dict = dict:from_list([{Doc,[]} || Doc <- Docs]),
    Dict2 = dict:from_list([{Doc,[]} || Doc <- Docs2]),

    Shards =
        mem3_util:create_partition_map("foo",3,1,["node1","node2","node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>,Shards,Docs),


    % test for W = 2
    AccW2 = {length(Shards), length(Docs), list_to_integer("2"), GroupedDocs,
        Dict},

    {ok,{WaitingCountW2_1,_,_,_,_}=AccW2_1} =
        handle_message({ok, [{ok, Doc1}]},hd(Shards),AccW2),
    ?assertEqual(WaitingCountW2_1,2),
    {stop, FinalReplyW2 } =
        handle_message({ok, [{ok, Doc1}]},lists:nth(2,Shards),AccW2_1),
    ?assertEqual({ok, [{Doc1, {ok,Doc1}}]},FinalReplyW2),

    % test for W = 3
    AccW3 = {length(Shards), length(Docs), list_to_integer("3"), GroupedDocs,
        Dict},

    {ok,{WaitingCountW3_1,_,_,_,_}=AccW3_1} =
        handle_message({ok, [{ok, Doc1}]},hd(Shards),AccW3),
    ?assertEqual(WaitingCountW3_1,2),

    {ok,{WaitingCountW3_2,_,_,_,_}=AccW3_2} =
        handle_message({ok, [{ok, Doc1}]},lists:nth(2,Shards),AccW3_1),
    ?assertEqual(WaitingCountW3_2,1),

    {stop, FinalReplyW3 } =
        handle_message({ok, [{ok, Doc1}]},lists:nth(3,Shards),AccW3_2),
    ?assertEqual({ok, [{Doc1, {ok,Doc1}}]},FinalReplyW3),

    % test w quorum > # shards, which should fail immediately

    Shards2 = mem3_util:create_partition_map("foo",1,1,["node1"]),
    GroupedDocs2 = group_docs_by_shard_hack(<<"foo">>,Shards2,Docs),

    AccW4 =
        {length(Shards2), length(Docs), list_to_integer("2"), GroupedDocs2, Dict},
    Bool =
    case handle_message({ok, [{ok, Doc1}]},hd(Shards2),AccW4) of
        {stop, _Reply} ->
            true;
        _ -> false
    end,
    ?assertEqual(Bool,true),

    % Docs with no replies should end up as {error, internal_server_error}
    SA1 = #shard{node=a, range=1},
    SB1 = #shard{node=b, range=1},
    SA2 = #shard{node=a, range=2},
    SB2 = #shard{node=b, range=2},
    GroupedDocs3 = [{SA1,[Doc1]}, {SB1,[Doc1]}, {SA2,[Doc2]}, {SB2,[Doc2]}],
    StW5_0 = {length(GroupedDocs3), length(Docs2), 2, GroupedDocs3, Dict2},
    {ok, StW5_1} = handle_message({ok, [{ok, "A"}]}, SA1, StW5_0),
    {ok, StW5_2} = handle_message({rexi_EXIT, nil}, SB1, StW5_1),
    {ok, StW5_3} = handle_message({rexi_EXIT, nil}, SA2, StW5_2),
    {stop, ReplyW5} = handle_message({rexi_EXIT, nil}, SB2, StW5_3),
    ?assertEqual(
        {error, [{Doc1,{accepted,"A"}},{Doc2,{error,internal_server_error}}]},
        ReplyW5
    ).


doc_update2_test() ->
    Doc1 = #doc{revs = {1,[<<"foo">>]}},
    Doc2 = #doc{revs = {1,[<<"bar">>]}},
    Docs = [Doc2, Doc1],
    Shards =
        mem3_util:create_partition_map("foo",3,1,["node1","node2","node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>,Shards,Docs),
    Acc0 = {length(Shards), length(Docs), list_to_integer("2"), GroupedDocs,
        dict:from_list([{Doc,[]} || Doc <- Docs])},

    {ok,{WaitingCount1,_,_,_,_}=Acc1} =
        handle_message({ok, [{ok, Doc1},{ok, Doc2}]},hd(Shards),Acc0),
    ?assertEqual(WaitingCount1,2),

    {ok,{WaitingCount2,_,_,_,_}=Acc2} =
        handle_message({rexi_EXIT, 1},lists:nth(2,Shards),Acc1),
    ?assertEqual(WaitingCount2,1),

    {stop, Reply} =
        handle_message({rexi_EXIT, 1},lists:nth(3,Shards),Acc2),

    ?assertEqual({accepted, [{Doc1,{accepted,Doc2}}, {Doc2,{accepted,Doc1}}]},
        Reply).

doc_update3_test() ->
    Doc1 = #doc{revs = {1,[<<"foo">>]}},
    Doc2 = #doc{revs = {1,[<<"bar">>]}},
    Docs = [Doc2, Doc1],
    Shards =
        mem3_util:create_partition_map("foo",3,1,["node1","node2","node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>,Shards,Docs),
    Acc0 = {length(Shards), length(Docs), list_to_integer("2"), GroupedDocs,
        dict:from_list([{Doc,[]} || Doc <- Docs])},

    {ok,{WaitingCount1,_,_,_,_}=Acc1} =
        handle_message({ok, [{ok, Doc1},{ok, Doc2}]},hd(Shards),Acc0),
    ?assertEqual(WaitingCount1,2),

    {ok,{WaitingCount2,_,_,_,_}=Acc2} =
        handle_message({rexi_EXIT, 1},lists:nth(2,Shards),Acc1),
    ?assertEqual(WaitingCount2,1),

    {stop, Reply} =
        handle_message({ok, [{ok, Doc1},{ok, Doc2}]},lists:nth(3,Shards),Acc2),

    ?assertEqual({ok, [{Doc1, {ok, Doc2}},{Doc2, {ok,Doc1}}]},Reply).

% needed for testing to avoid having to start the mem3 application
group_docs_by_shard_hack(_DbName, Shards, Docs) ->
    dict:to_list(lists:foldl(fun(#doc{id=_Id} = Doc, D0) ->
        lists:foldl(fun(Shard, D1) ->
            dict:append(Shard, Doc, D1)
        end, D0, Shards)
    end, dict:new(), Docs)).
