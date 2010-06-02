-module(fabric_doc_count).

-export([go/1]).

-include("fabric.hrl").

go(DbName) ->
    Shards = partitions:all_parts(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_doc_count, []),
    Acc0 = {length(Workers), [{Beg,nil} || #shard{range=[Beg,_]} <- Workers]},
    fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0).

handle_message({ok, Count}, #shard{range=[Beg,_]}, {N, Infos0}) ->
    case couch_util:get_value(Beg, Infos0) of
    nil ->
        Infos = lists:keyreplace(Beg, 1, Infos0, {Beg, Count}),
        case is_complete(Infos) of
        true ->
            {stop, lists:sum([C || {_, C} <- Infos])};
        false ->
            if N > 1 ->
                {ok, {N-1, Infos}};
            true ->
                report_error(Infos),
                {error, missing_shards}
            end
        end;
    _ ->
        {ok, {N-1, Infos0}}
    end;
handle_message(_, _, {1, Infos}) ->
    report_error(Infos),
    {error, missing_shards};
handle_message(_Other, _, {N, Infos0}) ->
    {ok, {N-1, Infos0}}.

report_error(Infos) ->
    MissingShards = [S || {S,nil} <- Infos],
    ?LOG_ERROR("doc_count error, missing shards: ~p", [MissingShards]).

is_complete(List) ->
    not lists:keymember(nil, 2, List).
