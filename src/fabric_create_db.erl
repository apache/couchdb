-module(fabric_create_db).
-author(brad@cloudant.com).

-export([create_db/2]).

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").


%% @doc Create a new database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
-spec create_db(binary(), list()) -> {ok, #db{}} | {error, any()}.
create_db(DbName, Options) ->
    Fullmap = partitions:fullmap(DbName, Options),
    {ok, FullNodes} = mem3:fullnodes(),
    RefPartMap = send_create_calls(Fullmap, Options),
    Acc0 = {false, length(RefPartMap), lists:usort([ {Beg, false} ||
         {_,#shard{range=[Beg,_]}} <- RefPartMap])},
    Result = case fabric_util:receive_loop(
        RefPartMap, 1, fun handle_create_msg/3, Acc0) of
    {ok, _Results} -> ok;
    Error -> Error
    end,
    % always install partition map, even w/ errors, so delete is possible
    partitions:install_fullmap(DbName, Fullmap, FullNodes, Options),
    Result.

%%
%% internal
%%

%% @doc create the partitions on all appropriate nodes (rexi calls)
-spec send_create_calls(fullmap(), list()) -> [{reference(), part()}].
send_create_calls(Fullmap, Options) ->
    lists:map(fun(#shard{node=Node, name=ShardName} = Part) ->
        Ref = rexi:async_server_call({couch_server, Node},
                                     {create, ShardName, Options}),
        {Ref, Part}
    end, Fullmap).

%% @doc handle create messages from shards
handle_create_msg(_, file_exists, _) ->
    {error, file_exists};
handle_create_msg(_, {rexi_EXIT, _Reason}, {Complete, N, Parts}) ->
    {ok, {Complete, N-1, Parts}};
handle_create_msg(_, {rexi_DOWN, _, _, _}, {Complete, _N, _Parts}) ->
    if
        Complete -> {stop, ok};
        true -> {error, create_db_fubar}
    end;
handle_create_msg(_, _, {true, 1, _Acc}) ->
    {stop, ok};
handle_create_msg({_, #shard{range=[Beg,_]}}, {ok, _}, {false, 1, PartResults0}) ->
    PartResults = lists:keyreplace(Beg, 1, PartResults0, {Beg, true}),
    case is_complete(PartResults) of
    true -> {stop, ok};
    false -> {error, create_db_fubar}
    end;
handle_create_msg(_RefPart, {ok, _}, {true, N, Parts}) ->
    {ok, {true, N-1, Parts}};
handle_create_msg({_Ref, #shard{range=[Beg,_]}}, {ok, _}, {false, Rem, PartResults0}) ->
    PartResults = lists:keyreplace(Beg, 1, PartResults0, {Beg, true}),
    {ok, {is_complete(PartResults), Rem-1, PartResults}}.

is_complete(List) ->
    lists:all(fun({_,Bool}) -> Bool end, List).
