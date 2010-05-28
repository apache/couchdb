-module(fabric_update_docs).
-export([go/3]).
-include("fabric.hrl").

go(DbName, AllDocs, Options) ->
    GroupedDocs = lists:map(fun({#shard{name=Name, node=Node} = Shard, Docs}) ->
        Ref = rexi:cast(Node, {fabric_rpc, update_docs, [Name, Docs, Options]}),
        {Shard#shard{ref=Ref}, Docs}
    end, group_docs_by_shard(DbName, AllDocs)),
    {Workers, _} = lists:unzip(GroupedDocs),
    Acc0 = {length(Workers), length(AllDocs), couch_util:get_value(w, Options, 1),
        GroupedDocs, dict:new()},
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, Results} ->
        {ok, couch_util:reorder_results(AllDocs, Results)};
    Else ->
        Else
    end.

handle_message(_Worker, {rexi_DOWN, _, _, _}, Acc0) ->
    skip_message(Acc0);
handle_message(_Worker, {rexi_EXIT, _}, Acc0) ->
    skip_message(Acc0);
handle_message(Worker, {ok, Replies}, Acc0) ->
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
    end.

force_reply(Doc, Replies, {W, Acc}) ->
    % TODO make a real decision here
    case Replies of
    [] ->
        {W, [{Doc, {error, internal_server_error}} | Acc]};
    [Reply| _] ->
        {W, [{Doc, Reply} | Acc]}
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
    % TODO make a real decision here
    case length(Replies) >= W of
    true ->
        {true, hd(Replies)};
    false ->
        false
    end.

-spec group_docs_by_shard(binary(), [#doc{}]) -> [{#shard{}, [#doc{}]}].
group_docs_by_shard(DbName, Docs) ->
    dict:to_list(lists:foldl(fun(#doc{id=Id} = Doc, D0) ->
        lists:foldl(fun(Shard, D1) ->
            dict:append(Shard, Doc, D1)
        end, D0, partitions:for_key(DbName,Id))
    end, dict:new(), Docs)).

append_update_replies([], [], DocReplyDict) ->
    DocReplyDict;
append_update_replies([Doc|Rest1], [Reply|Rest2], Dict0) ->
    % TODO what if the same document shows up twice in one update_docs call?
    append_update_replies(Rest1, Rest2, dict:append(Doc, Reply, Dict0)).

skip_message(Acc0) ->
    % TODO fix this
    {ok, Acc0}.

