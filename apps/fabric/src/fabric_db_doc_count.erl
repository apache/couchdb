-module(fabric_db_doc_count).

-export([go/1]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

go(DbName) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_doc_count, []),
    Acc0 = {fabric_dict:init(Workers, nil), 0},
    fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0).

handle_message({ok, Count}, Shard, {Counters, Acc}) ->
    case fabric_dict:lookup_element(Shard, Counters) of
    undefined ->
        % already heard from someone else in this range
        {ok, {Counters, Acc}};
    nil ->
        C1 = fabric_dict:store(Shard, ok, Counters),
        C2 = fabric_view:remove_overlapping_shards(Shard, C1),
        case fabric_dict:any(nil, C2) of
        true ->
            {ok, {C2, Count+Acc}};
        false ->
            {stop, Count+Acc}
        end
    end;
handle_message(_, _, Acc) ->
    {ok, Acc}.

