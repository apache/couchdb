-module(fabric_doc_open).

-export([go/3]).

-include("fabric.hrl").

go(DbName, Id, Options) ->
    Workers = fabric_util:submit_jobs(mem3:shards(DbName,Id), open_doc,
        [Id, Options]),
    SuppressDeletedDoc = not lists:member(deleted, Options),
    Acc0 = {length(Workers), couch_util:get_value(r, Options, 1), []},
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, {ok, #doc{deleted=true}}} when SuppressDeletedDoc ->
        {not_found, deleted};
    {ok, Else} ->
        Else;
    Error ->
        Error
    end.

handle_message({rexi_DOWN, _, _, _}, _Worker, Acc0) ->
    skip_message(Acc0);
handle_message({rexi_EXIT, _Reason}, _Worker, Acc0) ->
    skip_message(Acc0);
handle_message(Reply, _Worker, {WaitingCount, R, Replies}) ->
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

make_key({ok, #doc{id=Id, revs=Revs}}) ->
    {Id, Revs};
make_key(Else) ->
    Else.

repair_read_quorum_failure(Replies) ->
    case [Doc || {_Key, {ok, Doc}, _Count} <- Replies] of
    [] ->
        {stop, {not_found, missing}};
    [Doc|_] ->
        % TODO merge docs to find the winner as determined by replication
        {stop, {ok, Doc}}
    end.