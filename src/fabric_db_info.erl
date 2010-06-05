-module(fabric_db_info).
-author(brad@cloudant.com).

-export([get_db_info/2]).

-include("fabric.hrl").

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


%% =====================
%%   internal
%% =====================

handle_info_msg(_, _, {true, _, Infos0}) ->
    {stop, Infos0};
handle_info_msg(_, _, {false, 1, Infos0}) ->
    MissingShards = lists:reverse(lists:foldl(fun
        ({S,nil}, Acc) -> [S|Acc];
        (_, Acc) -> Acc
    end, [], Infos0)),
    ?LOG_ERROR("get_db_info error, missing shards: ~p", [MissingShards]),
    {error, get_db_info};
handle_info_msg({ok, Info}, #shard{range=[Beg,_]}, {false, N, Infos0}) ->
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
handle_info_msg(_Other, _, {Complete, N, Infos0}) ->
    {ok, {Complete, N-1, Infos0}}.

is_complete(List) ->
    not lists:any(fun({_,Info}) -> Info =:= nil end, List).

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
