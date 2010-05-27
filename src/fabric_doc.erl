-module(fabric_doc).
-export([open_doc/3, open_doc_revs/4, get_missing_revs/2, update_docs/3]).

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").

open_doc(DbName, DocId, Opts) ->
    Shards = partitions:for_key(DbName, DocId),
    Workers = fabric_util:submit_jobs(Shards, open_doc, [DocId, [deleted|Opts]]),
    Acc0 = {length(Workers), couch_util:get_value(r, Opts, 1), []},
    ?LOG_INFO("Workers ~p Acc0 ~p", [Workers, Acc0]),
    SuppressDeletedDoc = not lists:member(deleted, Opts),
    case fabric_util:recv(Workers, #shard.ref, fun handle_open_doc/3, Acc0) of
    {ok, #doc{deleted=true}} when SuppressDeletedDoc ->
        {not_found, deleted};
    Else ->
        Else
    end.

open_doc_revs(DbName, DocId, Revs, Options) ->
    ok.

update_docs(DbName, Docs, Options) ->
    ok.

get_missing_revs(DbName, IdsRevs) ->
    ok.

handle_open_doc(_Worker, {rexi_DOWN, _, _, _}, {WaitingCount, R, Replies}) ->
    if WaitingCount =:= 1 ->
        repair_read_quorum_failure(Replies);
    true ->
        {ok, {WaitingCount-1, R, Replies}}
    end;
handle_open_doc(_Worker, {rexi_EXIT, Reason}, {WaitingCount, R, Replies}) ->
    ?LOG_ERROR("open_doc rexi_EXIT ~p", [Reason]),
    if WaitingCount =:= 1 ->
        repair_read_quorum_failure(Replies);
    true ->
        {ok, {WaitingCount-1, R, Replies}}
    end;
handle_open_doc(_Worker, Reply, {WaitingCount, R, Replies}) ->
    ?LOG_INFO("got ~p when ~p ~p ~p", [Reply, WaitingCount, R, Replies]),
    case merge_replies(make_key(Reply), Reply, Replies) of
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

merge_replies(Key, Reply, Replies) ->
    case lists:keyfind(Key, 1, Replies) of
    false ->
        {[{Key, Reply, 1} | Replies], 1};
    {Key, _, N} ->
        {lists:keyreplace(Key, 1, Replies, {Key, Reply, N+1}), N+1}
    end.

make_key({ok, #doc{id=Id, revs=Revs}}) ->
    {Id, Revs};
make_key({not_found, missing}) ->
    {not_found, missing}.

repair_read_quorum_failure(Replies) ->
    case [Doc || {ok, Doc} <- Replies] of
    [] ->
        {stop, {not_found, missing}};
    [Doc|Rest] ->
        % TODO merge docs to find the winner as determined by replication
        {stop, {ok, Doc}}
    end.
