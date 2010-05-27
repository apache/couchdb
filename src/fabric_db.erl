-module(fabric_db).
-author('Brad Anderson <brad@cloudant.com>').
-author('Adam Kocoloski <adam@cloudant.com>').

-export([all_databases/1, get_db_info/2, create_db/2, delete_db/2]).

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").

%% @doc gets all databases in the cluster.
-spec all_databases(binary() | []) -> [binary()].
all_databases([]) ->
    Dbs = ets:foldl(fun(#shard{dbname=DbName}, AccIn) ->
        new_acc(DbName, AccIn)
    end, [], partitions),
    {ok, Dbs};
all_databases(Customer) ->
    ?debugFmt("~nCustomer: ~p~n", [Customer]),
    Dbs = ets:foldl(fun(#shard{dbname=DbName}, AccIn) ->
        DbNameStr = ?b2l(DbName),
        case string:str(DbNameStr, Customer) of
        1 ->
            new_acc(DbNameStr, AccIn);
        _ -> AccIn
        end
    end, [], dbs_cache),
    {ok, Dbs}.

%% @doc get database information tuple
get_db_info(DbName, Customer) ->
    Name = cloudant_db_name(Customer, DbName),
    Shards = partitions:all_parts(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_db_info, []),
    Acc0 = {false, length(Workers), lists:usort([ {Beg, nil} ||
        #shard{range=[Beg,_]} <- Workers])},
    case fabric_util:recv(Workers, #shard.ref, fun handle_info_msg/3, Acc0) of
    {ok, ShardInfos} ->
        {ok, process_infos(ShardInfos, [{db_name, Name}])};
    Error -> Error
    end.

%% @doc Create a new database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
-spec create_db(binary(), list()) -> {ok, #db{}} | {error, any()}.
create_db(DbName, Options) ->
    Fullmap = partitions:fullmap(DbName, Options),
    {ok, FullNodes} = mem3:fullnodes(),
    RefPartMap = send_create_calls(DbName, Options, Fullmap),
    Acc0 = {false, length(RefPartMap), lists:usort([ {Beg, false} ||
         {_,#shard{range=[Beg,_]}} <- RefPartMap])},
    case fabric_util:receive_loop(
        RefPartMap, 1, fun handle_create_msg/3, Acc0, 5000, infinity) of
    {ok, _Results} ->
        partitions:install_fullmap(DbName, Fullmap, FullNodes, Options),
        ok;
    Error -> Error
    end.

%% @doc Delete a database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
-spec delete_db(binary(), list()) -> {ok, #db{}} | {error, any()}.
delete_db(DbName, Options) ->
    Parts = partitions:all_parts(DbName),
    RefPartMap = send_delete_calls(DbName, Options, Parts),
    Acc0 = {true, length(RefPartMap)},
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

new_acc(DbName, Acc) ->
    case lists:member(DbName, Acc) of
    true -> Acc;
    _ ->[DbName | Acc]
    end.

handle_info_msg(_, _, {true, _, Infos0}) ->
    {stop, Infos0};
handle_info_msg(_, _, {false, 1, Infos0}) ->
    MissingShards = lists:reverse(lists:foldl(fun
        ({S,nil}, Acc) -> [S|Acc];
        (_, Acc) -> Acc
    end, [], Infos0)),
    ?LOG_ERROR("get_db_info error, missing shards: ~p", [MissingShards]),
    {error, get_db_info};
handle_info_msg(#shard{range=[Beg,_]}, {ok, Info}, {false, N, Infos0}) ->
    case couch_util:get_value(Beg, Infos0) of
    nil ->
        Infos = lists:keyreplace(Beg, 1, Infos0, {Beg, Info}),
        case is_complete(info, Infos) of
        true -> {ok, {true, N-1, Infos}};
        false -> {ok, {false, N-1, Infos}}
        end;
    _ ->
        {ok, {false, N-1, Infos0}}
    end;
handle_info_msg(_, _Other, {Complete, N, Infos0}) ->
    {ok, {Complete, N-1, Infos0}}.

is_complete(info, List) ->
    not lists:any(fun({_,Info}) -> Info =:= nil end, List);
is_complete(create, List) ->
    lists:all(fun({_,Bool}) -> Bool end, List).

cloudant_db_name(Customer, FullName) ->
    case Customer of
    "" -> FullName;
    Name -> re:replace(FullName, [Name,"/"], "", [{return, binary}])
    end.

%% Loop through Tasks on the flattened Infos and get the aggregated result
process_infos(Infos, Initial) ->
    Tasks = [
        {doc_count, fun sum/2, 0},
        {doc_del_count, fun sum/2, 0},
        {update_seq, fun max/2, 1},
        {purge_seq, fun sum/2, 0},
        {compact_running, fun bool/2, 0},
        {disk_size, fun sum/2, 0},
        {instance_start_time, fun(_, _) -> <<"0">> end, 0},
        {disk_format_version, fun max/2, 0}],

    Infos1 = lists:flatten(Infos),

    Result = lists:map(fun({Type, Fun, Default}) ->
        {Type, process_info(Type, Fun, Default, Infos1)}
    end, Tasks),
    lists:flatten([Initial, Result]).

 process_info(Type, Fun, Default, List) ->
     lists:foldl(fun(V, AccIn) -> Fun(V, AccIn) end, Default,
         proplists:get_all_values(Type, List)).

sum(New, Existing) ->
    New + Existing.

bool(New, Existing) ->
    New andalso Existing.

max(New, Existing) ->
    case New > Existing of
        true  -> New;
        false -> Existing
    end.


%% @doc create the partitions on all appropriate nodes (rexi calls)
-spec send_create_calls(binary(), list(), fullmap()) -> [{reference(), part()}].
send_create_calls(DbName, Options, Fullmap) ->
    lists:map(fun(#shard{node=Node, range=[Beg,_]} = Part) ->
        ShardName = showroom_utils:shard_name(Beg, DbName),
        Ref = rexi:async_server_call({couch_server, Node},
                                     {create, ShardName, Options}),
        {Ref, Part}
    end, Fullmap).

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
    case is_complete(create, PartResults) of
    true -> {stop, ok};
    false -> {error, create_db_fubar}
    end;
handle_create_msg(_RefPart, {ok, _}, {true, N, Parts}) ->
    {ok, {true, N-1, Parts}};
handle_create_msg({_Ref, #shard{range=[Beg,_]}}, {ok, _}, {false, Rem, PartResults0}) ->
    PartResults = lists:keyreplace(Beg, 1, PartResults0, {Beg, true}),
    {ok, {is_complete(create, PartResults), Rem-1, PartResults}}.


%% @doc delete the partitions on all appropriate nodes (rexi calls)
-spec send_delete_calls(binary(), list(), fullmap()) -> [{reference(), part()}].
send_delete_calls(DbName, Options, Parts) ->
    lists:map(fun(#shard{node=Node, range=[Beg,_]} = Part) ->
        ShardName = showroom_utils:shard_name(Beg, DbName),
        Ref = rexi:async_server_call({couch_server, Node},
                                     {delete, ShardName, Options}),
        {Ref, Part}
    end, Parts).

handle_delete_msg(_, not_found, {NotFound, N}) ->
    {ok, {NotFound, N-1}};
handle_delete_msg(_, {rexi_EXIT, _Reason}, {NotFound, N}) ->
    {ok, {NotFound, N-1}};
handle_delete_msg(_, {rexi_DOWN, _, _, _}, _Acc) ->
    {error, delete_db_fubar};
handle_delete_msg(_, _, {NotFound, 1}) ->
    if
    NotFound -> {stop, not_found};
    true -> {stop, ok}
    end;
handle_delete_msg(_, ok, {_NotFound, N}) ->
    {ok, {false, N-1}}.

delete_fullmap(DbName) ->
    case couch_db:open(<<"dbs">>, []) of
    {ok, Db} ->
        {ok, Doc} = couch_api:open_doc(Db, DbName, nil, []),
        couch_api:update_doc(Db, Doc#doc{deleted=true});
    Error -> Error
    end.
