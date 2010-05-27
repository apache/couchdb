-module(fabric_info).

-export([all_databases/1, get_db_info/2]).

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
    Parts = partitions:all_parts(DbName),
    RefPartMap = send_info_calls(DbName, Parts),
    Acc0 = {false, length(RefPartMap), lists:usort([ {Beg, nil} ||
         {_,#shard{range=[Beg,_]}} <- RefPartMap])},
    case fabric_util:receive_loop(
        RefPartMap, 1, fun handle_info_msg/3, Acc0, 5000, infinity) of
    {ok, ShardInfos} ->
        {ok, process_infos(ShardInfos, [{db_name, Name}])};
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

send_info_calls(DbName, Parts) ->
    lists:map(fun(#shard{node=Node, range=[Beg,_]} = Part) ->
        ShardName = showroom_utils:shard_name(Beg, DbName),
        Ref = rexi:cast(Node, {fabric_rpc, get_db_info, ShardName}),
        {Ref, Part}
    end, Parts).

handle_info_msg(_, _, {true, _, Infos0}) ->
    {stop, Infos0};
handle_info_msg(_, _, {false, 1, Infos0}) ->
    MissingShards = lists:keyfind(nil, 2, Infos0),
    ?LOG_ERROR("get_db_info error, missing shards: ~p", [MissingShards]),
    {error, get_db_info};
handle_info_msg({_,#shard{range=[Beg,_]}}, {ok, Info}, {false, N, Infos0}) ->
    case couch_util:get_value(Beg, Infos0) of
    nil ->
        Infos = lists:keyreplace(Beg, 1, Infos0, {Beg, Info}),
        case is_complete(Infos) of
        true -> {ok, {true, N-1, Infos}};
        false -> {ok, {false, N-1, Infos}}
        end;
    _ ->
        {ok, {false, N-1, Infos0}}
    end;
handle_info_msg(_, _Other, {Complete, N, Infos0}) ->
    {ok, {Complete, N-1, Infos0}}.


is_complete(List) ->
    not lists:any(fun({_,nil}) -> true end, List).

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
         couch_util:get_all_values(Type, List)).

sum(New, Existing) ->
    New + Existing.

bool(New, Existing) ->
    New andalso Existing.

max(New, Existing) ->
    case New > Existing of
        true  -> New;
        false -> Existing
    end.
