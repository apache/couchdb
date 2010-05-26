-module(fabric_create).
-author('Brad Anderson <brad@cloudant.com>').

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").

%% api
-export([create_db/2]).


%% =====================
%%   api
%% =====================

%% @doc Create a new database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
-spec create_db(binary(), list()) -> {ok, #db{}} | {error, any()}.
create_db(DbName, Options) ->
    Fullmap = partitions:fullmap(DbName, Options),
    {ok, FullNodes} = mem3:fullnodes(),
    RefPartMap = send_create_calls(DbName, Options, Fullmap),
    {ok, Results} = fabric_rpc:receive_loop(RefPartMap, 5000,
                                            fun create_db_loop/3),
    case create_results(Results, RefPartMap) of
    ok ->
        partitions:install_fullmap(DbName, Fullmap, FullNodes, Options),
        {ok, #db{name=DbName}};
    Other -> {error, Other}
    end.


%% =====================
%%   internal
%% =====================

%% @doc create the partitions on all appropriate nodes (rexi calls)
-spec send_create_calls(binary(), list(), fullmap()) -> [{reference(), part()}].
send_create_calls(DbName, Options, Fullmap) ->
    lists:map(fun(#part{node=Node, b=Beg} = Part) ->
        ShardName = showroom_utils:shard_name(Beg, DbName),
        Ref = rexi:async_server_call({couch_server, Node},
                                     {create, ShardName, Options}),
        {Ref, Part}
    end, Fullmap).

%% @doc create_db receive loop
%%      Acc is either an accumulation of responses, or if we've received all
%%      responses, it's {ok, Responses}
-spec create_db_loop([ref_part_map()], tref(), beg_acc()) ->
    beg_acc() | {ok, beg_acc()}.
create_db_loop(_,_,{ok, Acc}) -> {ok, Acc};
create_db_loop(RefPartMap, TimeoutRef, AccIn) ->
    receive
    {Ref, {ok, MainPid}} when is_reference(Ref) ->
        % for dev only, close the Fd      TODO: remove me
        gen_server:call({couch_server, node(MainPid)}, {force_close, MainPid}),

        AccOut = check_all_parts(Ref, RefPartMap, AccIn, ok),
        create_db_loop(RefPartMap, TimeoutRef, AccOut);
    {Ref, Reply} when is_reference(Ref) ->
        AccOut = check_all_parts(Ref, RefPartMap, AccIn, Reply),
        create_db_loop(RefPartMap, TimeoutRef, AccOut);
    {timeout, TimeoutRef} ->
        {error, timeout}
    end.

%% @doc check the results (beginning of each partition range) of the create
%%      replies.  If we have a good reply from each partition, return ok
-spec create_results(beg_acc(), [ref_part_map()]) -> ok | create_quorum_error.
create_results(Results, RefPartMap) ->
    ResultBegParts = create_result(Results, []),
    DistinctBegParts = distinct_parts(RefPartMap),
    if
        ResultBegParts =:= DistinctBegParts -> ok;
        true ->
            ?debugFmt("~nResultBegParts: ~p~nDistinctBegParts: ~p~n",
                      [ResultBegParts, DistinctBegParts]),
            create_quorum_error
    end.

-spec create_result(beg_acc(), [part()]) -> [part()] | file_exists.
create_result([], Acc) ->
    lists:usort(Acc);
create_result([{#part{b=Beg}, ok}|Rest], Acc) ->
    create_result(Rest, [Beg|Acc]);
create_result([{_, {error, file_exists}}|_Rest], _Acc) ->
    {error, file_exists}; % if any replies were file_exists, return that
create_result([{_, Result}|Rest], Acc) ->
    showroom_log:message(error, "create_db error: ~p", [Result]),
    create_result(Rest, Acc).

check_all_parts(Ref, RefPartMap, Acc, Reply) ->
    case couch_util:get_value(Ref, RefPartMap) of
    #part{} = Part ->
        case lists:keyfind(Part, 1, Acc) of
        true -> Acc; % already present... that's odd
        _ ->
            NewAcc = [{Part, Reply} | Acc],
            case length(NewAcc) >= length(RefPartMap) of
            true -> {ok, NewAcc};
            _ -> NewAcc
            end
        end;
    _ -> Acc % ignore a non-matching Ref
    end.

distinct_parts(RefPartMap) ->
    {_Refs, Parts} = lists:unzip(RefPartMap),
    BegParts = lists:map(fun(#part{b=Beg}) -> Beg end, Parts),
    lists:usort(BegParts).
