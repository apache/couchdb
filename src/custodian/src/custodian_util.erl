% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(custodian_util).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([summary/0, report/0]).
-export([ensure_dbs_exists/0]).

% Old design doc which should be cleaned up
-define(CUSTODIAN_ID, <<"_design/custodian">>).

-record(state, {live, safe, callback, db, acc}).

%% public functions.

summary() ->
    Dict0 = dict:from_list(
        [{conflicted, 0}] ++
            [{{live, N}, 0} || N <- lists:seq(0, cluster_n() - 1)] ++
            [{{safe, N}, 0} || N <- lists:seq(0, cluster_n() - 1)]
    ),
    Fun = fun
        (_Id, _Range, {conflicted, _N}, Dict) ->
            dict:update_counter(conflicted, 1, Dict);
        (_Id, _Range, Item, Dict) ->
            dict:update_counter(Item, 1, Dict)
    end,
    dict:to_list(fold_dbs(Dict0, Fun)).

report() ->
    Fun = fun
        (Id, _Range, {conflicted, N}, Acc) ->
            [{Id, {conflicted, N}} | Acc];
        (Id, Range, Item, Acc) ->
            [{Id, Range, Item} | Acc]
    end,
    fold_dbs([], Fun).

ensure_dbs_exists() ->
    DbName = mem3_sync:shards_db(),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    ensure_custodian_ddoc_is_deleted(Db),
    {ok, Db}.

%% private functions.

fold_dbs(Acc, Fun) ->
    Safe = maybe_redirect([node() | nodes()]),
    Live = Safe -- maintenance_nodes(Safe),
    {ok, Db} = ensure_dbs_exists(),
    try
        State0 = #state{live = Live, safe = Safe, callback = Fun, db = Db, acc = Acc},
        {ok, State1} = couch_db:fold_docs(Db, fun fold_dbs1/2, State0, []),
        State1#state.acc
    after
        couch_db:close(Db)
    end.

fold_dbs1(#full_doc_info{id = <<"_design/", _/binary>>}, Acc) ->
    {ok, Acc};
fold_dbs1(#full_doc_info{deleted = true}, Acc) ->
    {ok, Acc};
fold_dbs1(#full_doc_info{id = Id} = FDI, State) ->
    InternalAcc =
        case count_conflicts(FDI) of
            0 ->
                State#state.acc;
            ConflictCount ->
                (State#state.callback)(Id, null, {conflicted, ConflictCount}, State#state.acc)
        end,
    fold_dbs(Id, load_shards(State#state.db, FDI), State#state{acc = InternalAcc}).

fold_dbs(Id, Shards, State) ->
    IsSafe = fun(#shard{node = N}) -> lists:member(N, State#state.safe) end,
    IsLive = fun(#shard{node = N}) -> lists:member(N, State#state.live) end,
    LiveShards = lists:filter(IsLive, Shards),
    SafeShards = lists:filter(IsSafe, Shards),
    TargetN = mem3_util:calculate_max_n(Shards),
    Acc0 = State#state.acc,
    Acc1 =
        case mem3_util:calculate_max_n(LiveShards) of
            LiveN when LiveN < TargetN ->
                LiveRanges = get_range_counts(LiveN, LiveShards, Shards),
                lists:foldl(
                    fun({Range, N}, FAcc) ->
                        (State#state.callback)(Id, Range, {live, N}, FAcc)
                    end,
                    Acc0,
                    LiveRanges
                );
            _ ->
                Acc0
        end,
    Acc2 =
        case mem3_util:calculate_max_n(SafeShards) of
            SafeN when SafeN < TargetN ->
                SafeRanges = get_range_counts(SafeN, SafeShards, Shards),
                lists:foldl(
                    fun({Range, N}, FAcc) ->
                        (State#state.callback)(Id, Range, {safe, N}, FAcc)
                    end,
                    Acc1,
                    SafeRanges
                );
            _ ->
                Acc1
        end,
    {ok, State#state{acc = Acc2}}.

get_range_counts(MaxN, Shards, AllShards) ->
    Ranges = ranges(Shards),
    AllRanges = ranges(AllShards),

    % Get a list of ranges that were used to fill the MaxN rings. Also return
    % whatever was left (not part of the rings).
    {UnusedRanges, UsedRanges} = get_n_rings(MaxN, Ranges, []),

    % All the ranges that participated in filling the N rings will get
    % their number of copies set to MaxN.
    UsedCounts = update_counts(UsedRanges, #{}, 1, fun(_) -> MaxN end),

    % Add ranges that were present but didn't get picked in the rings
    PresentCounts = update_counts(UnusedRanges, UsedCounts, 1, fun(N) ->
        max(N + 1, MaxN)
    end),

    % Handle shards that are not present at all. Mark these ranges as missing.
    Missing = [R || R <- AllRanges, not lists:member(R, Ranges)],
    RangeCounts = update_counts(Missing, PresentCounts, 0, fun(_) -> 0 end),

    % Report only shards with counts =< MaxN
    RangeCounts1 = maps:filter(fun(_, N) -> N =< MaxN end, RangeCounts),
    lists:sort(maps:to_list(RangeCounts1)).

update_counts(Ranges, Acc0, Init, UpdateFun) ->
    lists:foldl(
        fun({B, E}, Acc) ->
            maps:update_with({B, E}, UpdateFun, Init, Acc)
        end,
        Acc0,
        Ranges
    ).

ranges(Shards) ->
    lists:map(
        fun(S) ->
            [B, E] = mem3:range(S),
            {B, E}
        end,
        Shards
    ).

get_n_rings(N, Ranges, Rings) when N =< 0 ->
    {Ranges, Rings};
get_n_rings(N, Ranges, Rings) ->
    Ring = mem3_util:get_ring(Ranges),
    get_n_rings(N - 1, Ranges -- Ring, Rings ++ Ring).

cluster_n() ->
    config:get_integer("cluster", "n", 3).

maintenance_nodes(Nodes) ->
    {Modes, _} = rpc:multicall(Nodes, config, get, ["couchdb", "maintenance_mode"]),
    [N || {N, Mode} <- lists:zip(Nodes, Modes), Mode =:= "true"].

load_shards(Db, #full_doc_info{id = Id} = FDI) ->
    case couch_db:open_doc(Db, FDI, [ejson_body]) of
        {ok, #doc{body = {Props}}} ->
            mem3_util:build_shards(Id, Props);
        {not_found, _} ->
            erlang:error(database_does_not_exist, ?b2l(Id))
    end.

maybe_redirect(Nodes) ->
    maybe_redirect(Nodes, []).

maybe_redirect([], Acc) ->
    Acc;
maybe_redirect([Node | Rest], Acc) ->
    case config:get("mem3.redirects", atom_to_list(Node)) of
        undefined ->
            maybe_redirect(Rest, [Node | Acc]);
        Redirect ->
            maybe_redirect(Rest, [list_to_atom(Redirect) | Acc])
    end.

count_conflicts(#full_doc_info{rev_tree = T}) ->
    Leafs = [1 || {#leaf{deleted = false}, _} <- couch_key_tree:get_all_leafs(T)],
    length(Leafs) - 1.

% Ensure the design doc which was added 3.2.0 is deleted as we switched to using a BDU
% function instead. After a few releases this function could be removed as well
%
ensure_custodian_ddoc_is_deleted(Db) ->
    case couch_db:open_doc(Db, ?CUSTODIAN_ID, [ejson_body]) of
        {not_found, _Reason} ->
            ok;
        {ok, Doc} ->
            DeletedDoc = Doc#doc{deleted = true, body = {[]}},
            try couch_db:update_doc(Db, DeletedDoc, [?ADMIN_CTX]) of
                {ok, _} ->
                    LogMsg = "~p : deleted custodian ddoc ~s",
                    couch_log:notice(LogMsg, [?MODULE, ?CUSTODIAN_ID]),
                    ok
            catch
                conflict ->
                    {ok, NewDb} = couch_db:reopen(Db),
                    ensure_custodian_ddoc_is_deleted(NewDb)
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_range_counts_test_() ->
    [
        ?_assertEqual(Res, get_range_counts(N, Shards, AllShards))
     || {N, Shards, AllShards, Res} <- [
            % No shards are present. There is a full range shard that would
            % fit. Report that range as missing.
            {0, [], [full()], [{{0, ?RING_END}, 0}]},

            % Can't complete the ring. But would complete it if had the
            % {2, ?RING_END} interval available.
            {0, [sh(0, 1)], [sh(0, 1), sh(2, ?RING_END)], [{{2, ?RING_END}, 0}]},

            % Can complete the ring only 1 time. Report that range as the
            % one available with a count of 1
            {1, [full()], [full(), full()], [{{0, ?RING_END}, 1}]},

            % Can complete the ring only 1 time with a full range shard, but
            % there is also {2, ?RING_END} that would complete another the
            % the ring as well if {0, 1} was present.
            {1, [sh(2, ?RING_END), full()], [sh(0, 1), sh(2, ?RING_END), full()], [
                {{0, 1}, 0},
                {{0, ?RING_END}, 1},
                {{2, ?RING_END}, 1}
            ]},

            % Can complete the ring 2 times [{0, 2},{3, ?RING_END)] and full(),
            % and there is remnant of a 5, 9 range that would comlete the ring
            % as well if {0, 4} and {10, ?RING_END} were present. So report
            {2, [sh(0, 2), sh(3, ?RING_END), sh(5, 9), full()],
                [
                    sh(0, 2),
                    sh(
                        3,
                        ?RING_END
                    ),
                    full(),
                    sh(0, 4),
                    sh(5, 9),
                    sh(10, ?RING_END)
                ],
                [
                    {{0, 2}, 1},
                    {{0, 4}, 0},
                    {{0, ?RING_END}, 1},
                    {{3, ?RING_END}, 1},
                    {{5, 9}, 1},
                    {{10, ?RING_END}, 0}
                ]}
        ]
    ].

full() ->
    #shard{range = [0, ?RING_END]}.

sh(B, E) ->
    #shard{range = [B, E]}.

-endif.
