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
    W = couch_util:get_value(w, Options, couch_config:get("cluster","w","2")),
    Acc0 = {length(Workers), length(AllDocs), list_to_integer(W), GroupedDocs,
        dict:from_list([{Doc,[]} || Doc <- AllDocs])},
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, Results} ->
        Reordered = couch_util:reorder_results(AllDocs, Results),
        {ok, [R || R <- Reordered, R =/= noreply]};
    Else ->
        Else
    end.

handle_message({rexi_DOWN, _, _, _}, _Worker, Acc0) ->
    skip_message(Acc0);
handle_message({rexi_EXIT, _}, _Worker, Acc0) ->
    {WaitingCount, _, W, _, DocReplyDict} = Acc0,
    if WaitingCount =:= 1 ->
        {W, Reply} = dict:fold(fun force_reply/3, {W,[]}, DocReplyDict),
        {stop, Reply};
    true ->
        {ok, setelement(1, Acc0, WaitingCount-1)}
    end;
handle_message({ok, Replies}, Worker, Acc0) ->
    {WaitingCount, DocCount, W, GroupedDocs, DocReplyDict0} = Acc0,
    Docs = couch_util:get_value(Worker, GroupedDocs),
    DocReplyDict = append_update_replies(Docs, Replies, DocReplyDict0),
    case {WaitingCount, dict:size(DocReplyDict)} of
    {1, _} ->
        % last message has arrived, we need to conclude things
        {W, Reply} = dict:fold(fun force_reply/3, {W,[]}, DocReplyDict),
        {stop, Reply};
    {_, DocCount} ->
        % we've got at least one reply for each document, let's take a look
        case dict:fold(fun maybe_reply/3, {stop,W,[]}, DocReplyDict) of
        continue ->
            {ok, {WaitingCount - 1, DocCount, W, GroupedDocs, DocReplyDict}};
        {stop, W, FinalReplies} ->
            {stop, FinalReplies}
        end;
    {_, N} when N < DocCount ->
        % no point in trying to finalize anything yet
        {ok, {WaitingCount - 1, DocCount, W, GroupedDocs, DocReplyDict}}
    end;
handle_message({missing_stub, Stub}, _, _) ->
    throw({missing_stub, Stub});
handle_message({not_found, no_db_file} = X, Worker, Acc0) ->
    {_, _, _, GroupedDocs, _} = Acc0,
    Docs = couch_util:get_value(Worker, GroupedDocs),
    handle_message({ok, [X || _D <- Docs]}, Worker, Acc0).

force_reply(Doc, [], {W, Acc}) ->
    {W, [{Doc, {error, internal_server_error}} | Acc]};
force_reply(Doc, [FirstReply|_] = Replies, {W, Acc}) ->
    case update_quorum_met(W, Replies) of
    {true, Reply} ->
        {W, [{Doc,Reply} | Acc]};
    false ->
        ?LOG_ERROR("write quorum (~p) failed, reply ~p", [W, FirstReply]),
        % TODO make a smarter choice than just picking the first reply
        {W, [{Doc,FirstReply} | Acc]}
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

skip_message(Acc0) ->
    % TODO fix this
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
