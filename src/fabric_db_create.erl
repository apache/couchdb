-module(fabric_db_create).
-export([go/2]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

%% @doc Create a new database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
go(DbName, Options) ->
    Shards = mem3:choose_shards(DbName, Options),
    Doc = make_document(Shards),
    Workers = fabric_util:submit_jobs(Shards, create_db, [Options, Doc]),
    Acc0 = fabric_dict:init(Workers, nil),
    case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
    {ok, _} ->
        ok;
    Else ->
        Else
    end.

handle_message(Msg, Shard, Counters) ->
    C1 = fabric_dict:store(Shard, Msg, Counters),
    case fabric_dict:any(nil, C1) of
    true ->
        {ok, C1};
    false ->
        final_answer(C1)
    end.

make_document([#shard{dbname=DbName}|_] = Shards) ->
    {RawOut, ByNodeOut, ByRangeOut} =
    lists:foldl(fun(#shard{node=N, range=[B,E]}, {Raw, ByNode, ByRange}) ->
        Range = ?l2b([couch_util:to_hex(<<B:32/integer>>), "-",
            couch_util:to_hex(<<E:32/integer>>)]),
        Node = couch_util:to_binary(N),
        {[[<<"add">>, Range, Node] | Raw], orddict:append(Node, Range, ByNode),
            orddict:append(Range, Node, ByRange)}
    end, {[], [], []}, Shards),
    #doc{id=DbName, body = {[
        {<<"changelog">>, lists:sort(RawOut)},
        {<<"by_node">>, {[{K,lists:sort(V)} || {K,V} <- ByNodeOut]}},
        {<<"by_range">>, {[{K,lists:sort(V)} || {K,V} <- ByRangeOut]}}
    ]}}.

final_answer(Counters) ->
    Successes = [X || {_, M} = X <- Counters, M == ok orelse M == file_exists],
    case fabric_view:is_progress_possible(Successes) of
    true ->
        case lists:keymember(file_exists, 2, Successes) of
        true ->
            {error, file_exists};
        false ->
            {stop, ok}
        end;
    false ->
        {error, internal_server_error}
    end.
