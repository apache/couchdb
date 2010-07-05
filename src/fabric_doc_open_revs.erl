-module(fabric_doc_open_revs).

-export([go/4]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

go(DbName, Id, Revs, Options) ->
    Workers = fabric_util:submit_jobs(mem3:shards(DbName,Id), open_revs,
        [Id, Revs, Options]),
    R = couch_util:get_value(r, Options, couch_config:get("cluster","r","2")),
    Acc0 = {length(Workers), list_to_integer(R), []},
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, {ok, Reply}} ->
        {ok, Reply};
    Else ->
        Else
    end.

handle_message({rexi_DOWN, _, _, _}, _Worker, Acc0) ->
    skip_message(Acc0);
handle_message({rexi_EXIT, _}, _Worker, Acc0) ->
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

    