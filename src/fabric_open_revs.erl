-module(fabric_open_revs).
-export([go/4]).
-include("fabric.hrl").

go(DbName, Id, Revs, Options) ->
    Workers = fabric_util:submit_jobs(partitions:for_key(DbName,Id), open_revs,
        [Id, Revs, Options]),
    Acc0 = {length(Workers), couch_util:get_value(r, Options, 1), []},
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, {ok, Reply}} ->
        {ok, Reply};
    Else ->
        Else
    end.

handle_message(_Worker, {rexi_DOWN, _, _, _}, Acc0) ->
    skip_message(Acc0);
handle_message(_Worker, {rexi_EXIT, _}, Acc0) ->
    skip_message(Acc0);
handle_message(_Worker, Reply, {WaitingCount, R, Replies}) ->
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

    