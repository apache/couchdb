-module(fabric_delete).
-author('Brad Anderson <brad@cloudant.com>').

-include("../../couch/src/couch_db.hrl").
-include("../../dynomite/include/membership.hrl").

%% api
-export([delete_db/2]).


%% =====================
%%   api
%% =====================

%% @doc Delete a new database, and all its partition files across the cluster
%%      Options is proplist with user_ctx, n, q
-spec delete_db(binary(), list()) -> {ok, #db{}} | {error, any()}.
delete_db(DbName, Options) ->
    Fullmap = partitions:fullmap(DbName, Options),
    RefNodePart = send_delete_calls(DbName, Options, Fullmap),
    {ok, Results} = delete_db_loop(RefNodePart),
    delete_results(Results, RefNodePart).


%delete_db(DbName, Options) ->
%    ResolveFun = fun(_Good) -> true end,
%    case cluster_ops:all_parts({dynomite_couch_api,delete_db,[DbName, Options]},
%                               d, true, ResolveFun) of
%    {ok, true} -> ok;
%    [{error, d_quorum_not_met}, {good, _Good}, {bad, Bad}] ->
%        showroom_utils:first_bad(Bad);
%    [{error, Error}, {good, _Good}, {bad, Bad}] ->
%        {Error, showroom_utils:first_bad(Bad)};
%    Other ->
%        ?debugFmt("~nOther: ~p~n", [Other]),
%        Other
%    end.

%% =====================
%%   internal
%% =====================

%% @doc delete the partitions on all appropriate nodes (rexi calls)
-spec send_delete_calls(binary(), list(), [mem_node()]) -> [{reference(), np()}].
send_delete_calls(DbName, Options, Fullmap) ->
    lists:map(fun({Node, Part}) ->
        ShardName = showroom_utils:shard_name(Part, DbName),
        Ref = rexi:async_server_call({couch_server, Node},
                                     {delete, ShardName, Options}),
        {Ref, {Node, Part}}
    end, Fullmap).

%% @doc set up the receive loop with an overall timeout
-spec delete_db_loop([ref_node_part()]) -> {ok, np_acc()}.
delete_db_loop(RefNodePart) ->
    TimeoutRef = erlang:make_ref(),
    {ok, TRef} = timer:send_after(5000, {timeout, TimeoutRef}),
    Results = delete_db_loop(RefNodePart, TimeoutRef, []),
    timer:cancel(TRef),
    Results.

%% @doc delete_db receive loop
%%      Acc is either an accumulation of responses, or if we've received all
%%      responses, it's {ok, Responses}
-spec delete_db_loop([ref_node_part()], tref(), np_acc()) ->
    np_acc() | {ok, np_acc()}.
delete_db_loop(_,_,{ok, Acc}) -> {ok, Acc};
delete_db_loop(RefNodePart, TimeoutRef, AccIn) ->
    receive
    {Ref, {ok, deleted}} when is_reference(Ref) ->
        AccOut = check_all_parts(Ref, RefNodePart, AccIn, ok),
        delete_db_loop(RefNodePart, TimeoutRef, AccOut);
    {Ref, Reply} when is_reference(Ref) ->
        AccOut = check_all_parts(Ref, RefNodePart, AccIn, Reply),
        delete_db_loop(RefNodePart, TimeoutRef, AccOut);
    {timeout, TimeoutRef} ->
        {error, timeout}
    end.

%% @doc check the results of the delete replies
%%      If we have a good reply from all partitions, return ok
-spec delete_results(np_acc(), [ref_node_part()]) ->
    ok | {error, delete_quorum_error}.
delete_results(Results, RefNodePart) ->
    ResultNPs = delete_result(Results, []),
    AllNPs = all_nodes_parts(RefNodePart),
    if
        ResultNPs =:= AllNPs -> ok;
        true -> {error, delete_quorum_error}
    end.

-spec delete_result(np_acc(), [np()]) -> [np()] | file_exists.
delete_result([], Acc) ->
    lists:sort(Acc);
delete_result([{NP, ok}|Rest], Acc) ->
    delete_result(Rest, [NP|Acc]);
delete_result([{_NP, {error, file_exists}}|_Rest], _Acc) ->
    {error, file_exists}; % if any replies were file_exists, return that
delete_result([{{_N,_P}, Result}|Rest], Acc) ->
    ?LOG_ERROR("delete_db error: ~p", [Result]),
    delete_result(Rest, Acc).

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
%                     "delete_db reply error: ~p from ~p ~p", [Msg, Node, Hex]),
%                 Acc
%             end
%         end;
%     _ -> Acc % ignore a non-matching Ref
%     end.

all_nodes_parts(RefNodePart) ->
    {_Refs, NPs} = lists:unzip(RefNodePart),
    lists:sort(NPs).
