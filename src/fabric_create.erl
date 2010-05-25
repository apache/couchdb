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
    RefNodePart = send_create_calls(DbName, Options, Fullmap),
    {ok, Results} = create_db_loop(RefNodePart),
    case create_results(Results, RefNodePart) of
    ok ->
        partitions:install_fullmap(DbName, Fullmap, FullNodes, Options),
        {ok, #db{name=DbName}};
    Other -> {error, Other}
    end.


%% =====================
%%   internal
%% =====================

%% @doc create the partitions on all appropriate nodes (rexi calls)
-spec send_create_calls(binary(), list(), [mem_node()]) -> [{reference(), np()}].
send_create_calls(DbName, Options, Fullmap) ->
    lists:map(fun({Node, Part}) ->
        ShardName = showroom_utils:shard_name(Part, DbName),
        Ref = rexi:async_server_call({couch_server, Node},
                                     {create, ShardName, Options}),
        {Ref, {Node, Part}}
    end, Fullmap).

%% @doc set up the receive loop with an overall timeout
-spec create_db_loop([ref_node_part()]) -> {ok, np_acc()}.
create_db_loop(RefNodePart) ->
    TimeoutRef = erlang:make_ref(),
    {ok, TRef} = timer:send_after(5000, {timeout, TimeoutRef}),
    Results = create_db_loop(RefNodePart, TimeoutRef, []),
    timer:cancel(TRef),
    Results.

%% @doc create_db receive loop
%%      Acc is either an accumulation of responses, or if we've received all
%%      responses, it's {ok, Responses}
-spec create_db_loop([ref_node_part()], tref(), np_acc()) ->
    np_acc() | {ok, np_acc()}.
create_db_loop(_,_,{ok, Acc}) -> {ok, Acc};
create_db_loop(RefNodePart, TimeoutRef, AccIn) ->
    receive
    {Ref, {ok, MainPid}} when is_reference(Ref) ->
        % for dev only, close the Fd      TODO: remove me
        gen_server:call({couch_server, node(MainPid)}, {force_close, MainPid}),

        AccOut = check_all_parts(Ref, RefNodePart, AccIn, ok),
        create_db_loop(RefNodePart, TimeoutRef, AccOut);
    {Ref, Reply} when is_reference(Ref) ->
        AccOut = check_all_parts(Ref, RefNodePart, AccIn, Reply),
        create_db_loop(RefNodePart, TimeoutRef, AccOut);
    {timeout, TimeoutRef} ->
        {error, timeout}
    end.

%% @doc check the results of the create replies
%%      If we have a good reply from each partition, return ok
-spec create_results(np_acc(), [ref_node_part()]) -> ok | create_quorum_error.
create_results(Results, RefNodePart) ->
    ResultParts = create_result(Results, []),
    DistinctParts = distinct_parts(RefNodePart),
    if
        ResultParts =:= DistinctParts -> ok;
        true ->
            ?debugFmt("~nResultParts: ~p~nDistinctParts: ~p~n",
                      [ResultParts, DistinctParts]),
            create_quorum_error
    end.

-spec create_result(np_acc(), [np()]) -> [np()] | file_exists.
create_result([], Acc) ->
    lists:usort(Acc);
create_result([{{_N,P}, ok}|Rest], Acc) ->
    create_result(Rest, [P|Acc]);
create_result([{_NP, {error, file_exists}}|_Rest], _Acc) ->
    {error, file_exists}; % if any replies were file_exists, return that
create_result([{{_N,_P}, Result}|Rest], Acc) ->
    showroom_log:message(error, "create_db error: ~p", [Result]),
    create_result(Rest, Acc).

check_all_parts(Ref, RefNodePart, Acc, Reply) ->
    case couch_util:get_value(Ref, RefNodePart) of
    {Node, Part} ->
        case lists:keyfind({Node, Part}, 1, Acc) of
        true -> Acc; % already present... that's odd
        _ ->
            NewAcc = [{{Node, Part}, Reply} | Acc],
            case length(NewAcc) >= length(RefNodePart) of
            true -> {ok, NewAcc};
            _ -> NewAcc
            end
        end;
    _ -> Acc % ignore a non-matching Ref
    end.

%% @doc check that we have a good reply from each partition.
%%      If we do, return {ok, Acc}, if we don't, return Acc of partitions
%%      Three 'case' statements and one 'if', a personal best.  fml
%% @end
% check_distinct_parts(Ref, RefNodePart, Acc, Msg) ->
%     Parts = distinct_parts(RefNodePart),
%     case couch_util:get_value(Ref, RefNodePart) of
%     {Node, Part} ->
%         case lists:member(Part, Acc) of
%         true -> Acc;
%         _ ->
%             case Msg of
%             ok ->
%                 NewAcc = lists:usort([Part|Acc]),
%                 if
%                     Parts =:= NewAcc -> {ok, NewAcc};
%                     true -> NewAcc
%                 end;
%             _ ->
%                 Hex = showroom_utils:int_to_hexstr(Part),
%                 showroom_log:message(error,
%                     "create_db reply error: ~p from ~p ~p", [Msg, Node, Hex]),
%                 Acc
%             end
%         end;
%     _ -> Acc % ignore a non-matching Ref
%     end.

distinct_parts(RefNodePart) ->
    {_Refs, NPs} = lists:unzip(RefNodePart),
    {_Nodes, Parts} = lists:unzip(NPs),
    lists:usort(Parts).
