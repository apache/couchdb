-module(fabric_db_delete).
-author(brad@cloudant.com).

-export([delete_db/2]).

-include("fabric.hrl").

%% @doc Delete a database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
-spec delete_db(binary(), list()) -> {ok, #db{}} | {error, any()}.
delete_db(DbName, Options) ->
    Fullmap = partitions:all_parts(DbName),
    RefPartMap = send_delete_calls(Fullmap, Options),
    Acc0 = {not_found, length(RefPartMap)},
    case fabric_util:receive_loop(
        RefPartMap, 1, fun handle_delete_msg/3, Acc0) of
    {ok, _Results} ->
        delete_fullmap(DbName),
        ok;
    Error -> Error
    end.

%%
%% internal
%%

%% @doc delete the partitions on all appropriate nodes (rexi calls)
-spec send_delete_calls(fullmap(), list()) -> [{reference(), part()}].
send_delete_calls(Parts, Options) ->
    lists:map(fun(#shard{node=Node, name=ShardName} = Part) ->
        Ref = rexi:async_server_call({couch_server, Node},
                                     {delete, ShardName, Options}),
        {Ref, Part}
    end, Parts).

handle_delete_msg(ok, _, {_, 1}) ->
    {stop, ok};
handle_delete_msg(not_found, _, {Acc, 1}) ->
    {stop, Acc};
handle_delete_msg({rexi_EXIT, _Reason}, _, {Acc, N}) ->
    % TODO is this the appropriate action to take, or should we abort?
    {ok, {Acc, N-1}};
handle_delete_msg({rexi_DOWN, _, _, _}, _, _Acc) ->
    {error, delete_db_fubar};
handle_delete_msg(not_found, _, {Acc, N}) ->
    {ok, {Acc, N-1}};
handle_delete_msg(ok, _, {_, N}) ->
    {ok, {ok, N-1}}.

delete_fullmap(DbName) ->
    case couch_db:open(<<"dbs">>, []) of
    {ok, Db} ->
        {ok, Doc} = couch_api:open_doc(Db, DbName, nil, []),
        couch_api:update_doc(Db, Doc#doc{deleted=true});
    Error -> Error
    end.
