-module(fabric_db_delete).
-export([go/2]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

go(DbName, Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, delete_db, [Options, DbName]),
    Acc0 = fabric_dict:init(Workers, nil),
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, ok} ->
        ok;
    {ok, not_found} ->
        erlang:error(database_does_not_exist);
    Error ->
        Error
    end.

handle_message(Msg, Shard, Counters) ->
    C1 = fabric_dict:store(Shard, Msg, Counters),
    case fabric_dict:any(nil, C1) of
    true ->
        {ok, C1};
    false ->
        final_answer(C1)
    end.

final_answer(Counters) ->
    Successes = [X || {_, M} = X <- Counters, M == ok orelse M == not_found],
    case fabric_view:is_progress_possible(Successes) of
    true ->
        case lists:keymember(ok, 2, Successes) of
        true ->
            {stop, ok};
        false ->
            {stop, not_found}
        end;
    false ->
        {error, internal_server_error}
    end.
