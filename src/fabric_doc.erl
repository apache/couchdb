-module(fabric_doc).
-export([open_doc/3, open_revs/4, get_missing_revs/2, update_docs/3]).

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").

open_doc(DbName, Id, Options) ->
    Workers = fabric_util:submit_jobs(partitions:for_key(DbName,Id), open_doc,
        [Id, Options]),
    SuppressDeletedDoc = not lists:member(deleted, Options),
    Acc0 = {length(Workers), couch_util:get_value(r, Options, 1), []},
    case fabric_util:recv(Workers, #shard.ref, fun handle_open_doc/3, Acc0) of
    {ok, {ok, #doc{deleted=true}}} when SuppressDeletedDoc ->
        {not_found, deleted};
    {ok, Else} ->
        Else;
    Error ->
        Error
    end.

open_revs(DbName, Id, Revs, Options) ->
    Workers = fabric_util:submit_jobs(partitions:for_key(DbName,Id), open_revs,
        [Id, Revs, Options]),
    Acc0 = {length(Workers), couch_util:get_value(r, Options, 1), []},
    fabric_util:recv(Workers, #shard.ref, fun handle_open_revs/3, Acc0).

update_docs(DbName, AllDocs, Options) ->
    GroupedDocs = lists:map(fun({#shard{name=Name, node=Node} = Shard, Docs}) ->
        Ref = rexi:cast(Node, {fabric_rpc, update_docs, [Name, Docs, Options]}),
        {Shard#shard{ref=Ref}, Docs}
    end, group_docs_by_shard(DbName, AllDocs)),
    {Workers, _} = lists:unzip(GroupedDocs),
    Acc0 = {length(Workers), length(AllDocs), couch_util:get_value(w, Options, 1),
        GroupedDocs, dict:new()},
    case fabric_util:recv(Workers, #shard.ref, fun handle_update_docs/3, Acc0) of
    {ok, Results} ->
        {ok, couch_util:reorder_results(AllDocs, Results)};
    Else ->
        Else
    end.

get_missing_revs(_DbName, _IdsRevs) ->
    ok.

handle_open_doc(_Worker, {rexi_DOWN, _, _, _}, Acc0) ->
    skip_message(Acc0);
handle_open_doc(_Worker, {rexi_EXIT, _Reason}, Acc0) ->
    skip_message(Acc0);
handle_open_doc(_Worker, Reply, {WaitingCount, R, Replies}) ->
    case merge_read_reply(make_key(Reply), Reply, Replies) of
    {_, KeyCount} when KeyCount =:= R ->
        {stop, Reply};
    {NewReplies, KeyCount} when KeyCount < R ->
        if WaitingCount =:= 1 ->
            % last message arrived, but still no quorum
            repair_read_quorum_failure(NewReplies);
        true ->
            {ok, {WaitingCount-1, R, NewReplies}}
        end
    end.

handle_open_revs(_Worker, {rexi_DOWN, _, _, _}, Acc0) ->
    skip_message(Acc0);
handle_open_revs(_Worker, {rexi_EXIT, _}, Acc0) ->
    skip_message(Acc0);
handle_open_revs(_Worker, _Reply, {_WaitingCount, _R, _Replies}) ->
    {stop, not_implemented}.

handle_update_docs(_Worker, {rexi_DOWN, _, _, _}, Acc0) ->
    skip_message(Acc0);
handle_update_docs(_Worker, {rexi_EXIT, _}, Acc0) ->
    skip_message(Acc0);
handle_update_docs(Worker, {ok, Replies}, Acc0) ->
    {WaitingCount, DocCount, W, GroupedDocs, DocReplyDict0} = Acc0,
    Docs = couch_util:get_value(Worker, GroupedDocs),
    DocReplyDict = append_update_replies(Docs, Replies, DocReplyDict0),
    case {WaitingCount, dict:size(DocReplyDict)} of
    {1, _} ->
        % last message has arrived, we need to conclude things
        {W, Reply} = dict:fold(fun force_update_reply/3, {W,[]}, DocReplyDict),
        {stop, Reply};
    {_, DocCount} ->
        % we've got at least one reply for each document, let's take a look
        case dict:fold(fun maybe_update_reply/3, {stop,W,[]}, DocReplyDict) of
        continue ->
            {ok, {WaitingCount - 1, DocCount, W, GroupedDocs, DocReplyDict}};
        {stop, W, FinalReplies} ->
            {stop, FinalReplies}
        end;
    {_, N} when N < DocCount ->
        % no point in trying to finalize anything yet
        {ok, {WaitingCount - 1, DocCount, W, GroupedDocs, DocReplyDict}}
    end.

force_update_reply(Doc, Replies, {W, Acc}) ->
    % TODO make a real decision here
    case Replies of
    [] ->
        {W, [{Doc, {error, internal_server_error}} | Acc]};
    [Reply| _] ->
        {W, [{Doc, Reply} | Acc]}
    end.

maybe_update_reply(_, _, continue) ->
    % we didn't meet quorum for all docs, so we're fast-forwarding the fold
    continue;
maybe_update_reply(Doc, Replies, {stop, W, Acc}) ->
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

skip_message({1, _R, Replies}) ->
    repair_read_quorum_failure(Replies);
skip_message({WaitingCount, R, Replies}) ->
    {ok, {WaitingCount-1, R, Replies}}.

merge_read_reply(Key, Reply, Replies) ->
    case lists:keyfind(Key, 1, Replies) of
    false ->
        {[{Key, Reply, 1} | Replies], 1};
    {Key, _, N} ->
        {lists:keyreplace(Key, 1, Replies, {Key, Reply, N+1}), N+1}
    end.

append_update_replies([], [], DocReplyDict) ->
    DocReplyDict;
append_update_replies([Doc|Rest1], [Reply|Rest2], Dict0) ->
    % TODO what if the same document shows up twice in one update_docs call?
    append_update_replies(Rest1, Rest2, dict:append(Doc, Reply, Dict0)).

make_key({ok, #doc{id=Id, revs=Revs}}) ->
    {Id, Revs};
make_key({not_found, missing}) ->
    {not_found, missing}.

repair_read_quorum_failure(Replies) ->
    case [Doc || {_Key, {ok, Doc}, _Count} <- Replies] of
    [] ->
        {stop, {not_found, missing}};
    [Doc|_] ->
        % TODO merge docs to find the winner as determined by replication
        {stop, {ok, Doc}}
    end.
