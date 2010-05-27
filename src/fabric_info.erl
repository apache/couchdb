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
get_db_info(DbName, _Customer) ->
    Parts = partitions:all_parts(DbName),
    RefPartMap = send_info_calls(DbName, Parts),
    {ok, Results} = fabric_rpc:receive_loop(RefPartMap, 5000, fun info_loop/3),
    InfoList = Results,
    %     process_infos(ShardInfos, [{db_name, Name}]);
    {ok, InfoList}.

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
        Ref = rexi:cast(Node, {rexi_rpc, get_db_info, ShardName}),
        {Ref, Part}
    end, Parts).

%% @doc create_db receive loop
%%      Acc is either an accumulation of responses, or if we've received all
%%      responses, it's {ok, Responses}
-spec info_loop([ref_part_map()], tref(), beg_acc()) ->
    beg_acc() | {ok, beg_acc()}.
info_loop(_,_,{ok, Acc}) -> {ok, Acc};
info_loop(RefPartMap, TimeoutRef, AccIn) ->
    receive
    {Ref, {ok, Info}} when is_reference(Ref) ->
        %AccOut = check_all_parts(Ref, RefPartMap, AccIn, ok),
        %info_loop(RefPartMap, TimeoutRef, AccOut);
            ok;
    {Ref, Reply} when is_reference(Ref) ->
        %AccOut = check_all_parts(Ref, RefPartMap, AccIn, Reply),
        %info_loop(RefPartMap, TimeoutRef, AccOut);
            ok;
    {timeout, TimeoutRef} ->
        {error, timeout}
    end.


cloudant_db_name(Customer, FullName) ->
    case Customer of
    "" ->
        FullName;
    Name ->
        re:replace(FullName, [Name,"/"], "", [{return, binary}])
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
