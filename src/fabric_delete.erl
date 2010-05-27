-module(fabric_delete).
-author('Brad Anderson <brad@cloudant.com>').

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").

%% api
-export([delete_db/2]).


%% =====================
%%   api
%% =====================

%% @doc Delete a database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
-spec delete_db(binary(), list()) -> {ok, #db{}} | {error, any()}.
delete_db(DbName, Options) ->
    Parts = partitions:all_parts(DbName),
    RefPartMap = send_calls(DbName, Options, Parts),
    Acc0 = {false, length(RefPartMap)},
    case fabric_util:receive_loop(
        RefPartMap, 1, fun handle_delete_msg/3, Acc0, 5000, infinity) of
    {ok, _Results} ->
        delete_fullmap(DbName),
        ok;
    Error -> Error
    end.


%% =====================
%%   internal
%% =====================

%% @doc delete the partitions on all appropriate nodes (rexi calls)
-spec send_calls(binary(), list(), fullmap()) -> [{reference(), part()}].
send_calls(DbName, Options, Parts) ->
    lists:map(fun(#part{node=Node, b=Beg} = Part) ->
        ShardName = showroom_utils:shard_name(Beg, DbName),
        Ref = rexi:async_server_call({couch_server, Node},
                                     {delete, ShardName, Options}),
        {Ref, Part}
    end, Parts).

handle_delete_msg(_, not_found, _) ->
    {error, not_found};
handle_delete_msg(_, {rexi_EXIT, _Reason}, {Complete, N, Parts}) ->
    {ok, {Complete, N-1, Parts}};
handle_delete_msg(_, {rexi_DOWN, _, _, _}, {Complete, _N, _Parts}) ->
    if
        Complete -> {stop, ok};
        true -> {error, delete_db_fubar}
    end;
handle_delete_msg(_, _, {true, 1, _Acc}) ->
    {stop, ok};
handle_delete_msg({_, #part{b=Beg}}, {ok, _}, {false, 1, PartResults0}) ->
    PartResults = lists:keyreplace(Beg, 1, PartResults0, {Beg, true}),
    case is_complete(PartResults) of
    true -> {stop, ok};
    false -> {error, delete_db_fubar}
    end;
handle_delete_msg(_RefPart, {ok, _}, {true, N, Parts}) ->
    {ok, {true, N-1, Parts}};
handle_delete_msg({_Ref, #part{b=Beg}}, {ok, _}, {false, Rem, PartResults0}) ->
    PartResults = lists:keyreplace(Beg, 1, PartResults0, {Beg, true}),
    {ok, {is_complete(PartResults), Rem-1, PartResults}}.

is_complete(List) ->
    lists:all(fun({_,Bool}) -> Bool end, List).

delete_fullmap(DbName) ->
    case couch_db:open(<<"dbs">>, []) of
    {ok, Db} ->
        couch_api:open_doc(Db, DbName, nil, []),
        couch_api:update_doc(Db, DbName, {[{<<"_deleted">>,true}]});
    Error -> Error
    end.
