%%%-------------------------------------------------------------------
%%% File:      cluster_ops.erl
%%% @author    Brad Anderson <brad@cloudant.com> [http://cloudant.com]
%%% @copyright 2009 Brad Anderson
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-07-21 by Brad Anderson
%%%-------------------------------------------------------------------
-module(cluster_ops).
-author('brad@cloudant.com').

%% API
-export([key_lookup/3, key_lookup/5,
  all_parts/4,
  some_parts/4, some_parts/5,
  quorum_from_each_part/3]).

-include("../include/common.hrl").
-include("../include/config.hrl").

-include("../include/profile.hrl").


%%====================================================================
%% API
%%====================================================================

%% @doc Get to the proper shard on N nodes by key lookup
%%
%%      This fun uses quorum constants from config
key_lookup(Key, {M,F,A}, Access) ->
    N = list_to_integer(couch_config:get("cluster", "n", "3")),
    key_lookup(Key, {M,F,A}, Access, get_const(Access), N).


%% @doc Get to the proper shard on N nodes by key lookup
%%
%%      This fun uses a provided quorum constant, possibly from request,
%%      possibly from config
key_lookup(Key, {M,F,A}, Access, Const, N) ->
    NodeParts = membership2:nodeparts_for_key(Key),
    {ResolveFun, NotFoundFun} = case Access of
    r -> {fun resolve_read/1, fun resolve_not_found/2};
    w -> {fun resolve_write/1, fun(_,_) -> {false, notused, []} end}
    end,
    MapFun = fun({Node,Part}) ->
        try
            rpc:call(Node, M, F, [[Part | A]])
        catch Class:Exception ->
            {error, Class, Exception}
        end
    end,
    {GoodReplies, Bad} = pcall(MapFun, NodeParts, N),
    if length(Bad) > 0 -> ?LOG_DEBUG("~nBad: ~p~n", [Bad]); true -> ok end,
    Good = lists:map(fun strip_ok/1, GoodReplies),
    final_key_lookup(Good, Bad, N, Const, ResolveFun, NotFoundFun, Access).


%% @doc Do op on all shards (and maybe even replication partners)
all_parts({M,F,A}, Access, AndPartners, ResolveFun) ->
    NodePartList = membership2:all_nodes_parts(AndPartners),
    MapFun = fun({Node, Part}) ->
        try
            rpc:call(Node, M, F, [[Part | A]])
        catch Class:Exception ->
            {error, Class, Exception}
        end
    end,
    Replies = ?PMAP(MapFun, NodePartList),
    {Good, Bad} = lists:partition(fun valid/1, Replies),
    final_all_parts(Good, Bad, length(NodePartList), ResolveFun, Access).


%% @doc Do op on some shards, depending on list of keys sent in.
%%
%%      This fun uses quorum constants from config
some_parts(KeyFun, SeqsKVPairs, {M,F,A}, Access) ->
    some_parts(KeyFun, SeqsKVPairs, {M,F,A}, Access, get_const(Access)).


%% @doc Do op on some shards, depending on list of keys sent in.
%%
%%      This fun uses a provided quorum constant, possibly from request,
%%      possibly from config
some_parts(KeyFun, SeqsKVPairs, {M,F,A}, _Access, Const) ->
    TaskFun = fun({{Node,Part}, Values}) ->
        try
            rpc:call(Node, M, F, [[Part | [Values | A]]])
        catch Class:Exception ->
            {error, Class, Exception}
        end
    end,

    % get tasks per node that are part / values for that partition
    DistTasks = get_dist_tasks(KeyFun, SeqsKVPairs),

    % With the distributed tasklist in hand, do the tasks per partition.
    % For each partition, do the work on all nodes/parts.
    TaskReplies = ?PMAP(TaskFun, DistTasks),
    {GoodReplies, Bad} = lists:partition(fun valid/1, TaskReplies),
    if length(Bad) > 0 -> ?LOG_DEBUG("~nBad: ~p~n", [Bad]); true -> ok end,
    Good = lists:map(fun strip_ok/1, GoodReplies),
    final_some_parts(Good, Bad, Const).


quorum_from_each_part({M,F,A}, Access, ResolveFun) ->
    Const = get_const(Access),
    {_, Parts} = lists:unzip(membership2:partitions()),
    PartsMapFun = fun(Part) ->
        Nodes = membership2:nodes_for_part(Part),
        NodesMapFun = fun(Node) -> rpc:call(Node, M, F, [[Part | A]]) end,
        {GoodReplies,BadReplies} = pcall(NodesMapFun, Nodes, Const),
        Good1 = lists:map(fun strip_ok/1, GoodReplies),
        Bad1 = case length(Good1) >= Const of
        true -> [];
        false -> BadReplies
        end,
        {Good1,Bad1}
    end,
    Results1 = ?PMAP(PartsMapFun, Parts),
    {Good,Bad} = lists:foldl(fun({G,B}, {GAcc,BAcc}) ->
        {lists:append(G,GAcc),lists:append(B,BAcc)}
    end, {[],[]}, Results1),
    if length(Bad) > 0 -> ?LOG_DEBUG("~nBad: ~p~n", [Bad]); true -> ok end,
    final_quorum_from_each_part(Good, Bad, length(Parts), ResolveFun, Access).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

final_key_lookup(Good, Bad, N, Const, ResolveFun, NotFoundFun, Access) ->
  {NotFound, Return, Reasons} = NotFoundFun(Bad, Const),
  if
    length(Good) >= Const -> {ok, ResolveFun(Good)};
    NotFound -> {ok, Return, Reasons};
    true -> error_message(Good, Bad, N, Const, Access)
  end.


final_all_parts(Good, Bad, Total, ResolveFun, Access) ->
  case length(Good) =:= Total of
    true -> {ok, ResolveFun(Good)};
    _ -> error_message(Good, Bad, Total, Total, Access)
  end.


final_some_parts(Good, _Bad, Const) ->
  Good1 = lists:flatten(Good),
  {Seqs, _} = lists:unzip(Good1),
  {ResG,ResB} =
    lists:foldl(
      fun(Seq, {AccG,AccB}) ->
          Vals = proplists:get_all_values(Seq, Good1),
          case length(Vals) >= Const of
            true -> {[{Seq, Vals}|AccG],AccB};
            _ -> {AccG, [{Seq, Vals}|AccB]}
          end
      end, {[],[]}, lists:usort(Seqs)),
  case length(ResB) of
    0 -> {ok, ResG};
    _ -> {error, ResB}
  end.


final_quorum_from_each_part(Good, Bad, Total, ResolveFun, Access) ->
    case length(Good) =:= Total of
    true -> {ok, ResolveFun(Good)};
    _ -> error_message(Good, Bad, Total, Total, Access)
    end.


resolve_read([First|Responses]) ->
  case First of
    not_found -> not_found;
    _ -> lists:foldr(fun vector_clock:resolve/2, First, Responses)
  end.


resolve_write([First|Responses]) ->
  case First of
    not_found -> not_found;
    _ -> lists:foldr(fun vector_clock:resolve/2, First, Responses)
  end.


resolve_not_found(Bad, R) ->
  {NotFoundCnt, DeletedCnt, OtherReasons} =
    lists:foldl(fun({Error,Reason}, {NotFoundAcc, DeletedAcc, ReasonAcc}) ->
      case {Error,Reason} of
        {not_found, {_Clock, [missing|_Rest]}} ->
          {NotFoundAcc+1, DeletedAcc, ReasonAcc};
        {not_found, {_Clock, [deleted|_Rest]}} ->
          {NotFoundAcc, DeletedAcc+1, ReasonAcc};
        _ ->
          {NotFoundAcc, DeletedAcc, [Reason|ReasonAcc]}
      end
    end, {0, 0, []}, Bad),
  % TODO: is the comparison to R good here, or should it be N-R?
  if
    NotFoundCnt >= R -> {true, {not_found, missing}, OtherReasons};
    DeletedCnt >= R -> {true, {not_found, deleted}, OtherReasons};
    true -> {false, other, OtherReasons}
  end.


error_message(Good, Bad, N, T, Access) ->
  Msg = list_to_atom(lists:concat([atom_to_list(Access), "_quorum_not_met"])),
  ?LOG_ERROR("~p~nSuccess on ~p of ~p servers. Needed ~p. Errors: ~w"
             , [Msg, length(Good), N, T, Bad]),
  [{error, Msg}, {good, Good}, {bad, Bad}].


pcall(MapFun, Servers, Const) ->
  Replies = lib_misc:pmap(MapFun, Servers, Const),
  lists:partition(fun valid/1, Replies).


valid({ok, _}) -> true;
valid(ok) -> true;
valid(_) -> false.


strip_ok({ok, Val}) -> Val;
strip_ok(Val) -> Val.


%% @spec get_dist_tasks(KeyFun::function(), KVPairs::list()) ->
%%           [{{Node::node(), Part::integer()}, SeqVals}]
%%       Type     - ordered | ??
%%       SeqVals   - [{Seq, Val}]
%% @doc builds a distributed task list of nodes with a list of shard/values.
%%      This looks like a dict structure
%%      but is a list so we can use ?PMAP with the results
%% @end
get_dist_tasks(KeyFun, SeqsKVPairs) ->
    NPSV = lists:flatmap(fun({_,KVPair} = Elem) ->
        [{NP, Elem} || NP <- membership2:nodeparts_for_key(KeyFun(KVPair))]
    end, SeqsKVPairs),
    group_by_key(NPSV).

group_by_key([]) ->
    [];
group_by_key(List) ->
    [{FirstK,FirstV} | Rest] = lists:keysort(1,List),
    Acc0 = {FirstK, [FirstV], []},
    FoldFun = fun({K,V}, {K,Vs,Acc}) ->
        {K, [V|Vs], Acc};
    ({NewKey,V}, {OldKey,Vs,Acc}) ->
        {NewKey, [V], [{OldKey,Vs}|Acc]}
    end,
    {LastK, LastVs, Acc} = lists:foldl(FoldFun, Acc0, Rest),
    [{LastK, LastVs} | Acc].

get_const(r) ->
    list_to_integer(couch_config:get("cluster", "r", "2"));
get_const(w) ->
    list_to_integer(couch_config:get("cluster", "w", "2"));
get_const(r1) ->
    1;
get_const(Other) ->
    throw({bad_access_term, Other}).
