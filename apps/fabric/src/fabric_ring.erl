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

-module(fabric_ring).

-export([
    is_progress_possible/1,
    is_progress_possible/2,
    get_shard_replacements/2,
    node_down/3,
    node_down/4,
    handle_error/3,
    handle_error/4,
    handle_response/4,
    handle_response/5
]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

-type fabric_dict() :: [{#shard{}, any()}].
-type ring_opts() :: [atom() | tuple()].

%% @doc looks for a fully covered keyrange in the list of counters
-spec is_progress_possible(fabric_dict()) -> boolean().
is_progress_possible(Counters) ->
    is_progress_possible(Counters, []).

%% @doc looks for a fully covered keyrange in the list of counters
%% This version take ring option to configure how progress will
%% be checked. By default, [], checks that the full ring is covered.
-spec is_progress_possible(fabric_dict(), ring_opts()) -> boolean().
is_progress_possible(Counters, RingOpts) ->
    is_progress_possible(Counters, [], 0, ?RING_END, RingOpts).

-spec get_shard_replacements(binary(), [#shard{}]) -> [#shard{}].
get_shard_replacements(DbName, UsedShards0) ->
    % We only want to generate a replacements list from shards
    % that aren't already used.
    AllLiveShards = mem3:live_shards(DbName, [node() | nodes()]),
    UsedShards = [S#shard{ref = undefined} || S <- UsedShards0],
    get_shard_replacements_int(AllLiveShards -- UsedShards, UsedShards).

-spec node_down(node(), fabric_dict(), fabric_dict()) ->
    {ok, fabric_dict()} | error.
node_down(Node, Workers, Responses) ->
    node_down(Node, Workers, Responses, []).

-spec node_down(node(), fabric_dict(), fabric_dict(), ring_opts()) ->
    {ok, fabric_dict()} | error.
node_down(Node, Workers, Responses, RingOpts) ->
    {B, E} = range_bounds(Workers, Responses),
    Workers1 = fabric_dict:filter(
        fun(#shard{node = N}, _) ->
            N =/= Node
        end,
        Workers
    ),
    case is_progress_possible(Workers1, Responses, B, E, RingOpts) of
        true -> {ok, Workers1};
        false -> error
    end.

-spec handle_error(#shard{}, fabric_dict(), fabric_dict()) ->
    {ok, fabric_dict()} | error.
handle_error(Shard, Workers, Responses) ->
    handle_error(Shard, Workers, Responses, []).

-spec handle_error(#shard{}, fabric_dict(), fabric_dict(), ring_opts()) ->
    {ok, fabric_dict()} | error.
handle_error(Shard, Workers, Responses, RingOpts) ->
    {B, E} = range_bounds(Workers, Responses),
    Workers1 = fabric_dict:erase(Shard, Workers),
    case is_progress_possible(Workers1, Responses, B, E, RingOpts) of
        true -> {ok, Workers1};
        false -> error
    end.

-spec handle_response(#shard{}, any(), fabric_dict(), fabric_dict()) ->
    {ok, {fabric_dict(), fabric_dict()}} | {stop, fabric_dict()}.
handle_response(Shard, Response, Workers, Responses) ->
    handle_response(Shard, Response, Workers, Responses, []).

-spec handle_response(
    #shard{},
    any(),
    fabric_dict(),
    fabric_dict(),
    ring_opts()
) ->
    {ok, {fabric_dict(), fabric_dict()}} | {stop, fabric_dict()}.
handle_response(Shard, Response, Workers, Responses, RingOpts) ->
    handle_response(
        Shard,
        Response,
        Workers,
        Responses,
        RingOpts,
        fun stop_workers/1
    ).

% Worker response handler. Gets reponses from shard and puts them in the list
% until they complete a full ring. Then kill unused responses and remaining
% workers.
%
% How a ring "completes" is driven by RingOpts:
%
%  * When RingOpts is [] (the default case) responses must form a "clean"
%    ring, where all copies at the start of the range and end of the range must
%    have the same boundary values.
%
%  * When RingOpts is [{any, [#shard{}]}] responses are accepted from any of
%    the provided list of shards. This type of ring might be used when querying
%    a partitioned database. As soon as a result from any of the shards
%    arrives, result collection stops.
%
%  * When RingOpts is [all], responses are accepted until all the shards return
%  results
%
handle_response(Shard, Response, Workers, Responses, RingOpts, CleanupCb) ->
    Workers1 = fabric_dict:erase(Shard, Workers),
    case RingOpts of
        [] ->
            #shard{range = [B, E]} = Shard,
            Responses1 = [{{B, E}, Shard, Response} | Responses],
            handle_response_ring(Workers1, Responses1, CleanupCb);
        [{any, Any}] ->
            handle_response_any(Shard, Response, Workers1, Any, CleanupCb);
        [all] ->
            Responses1 = [{Shard, Response} | Responses],
            handle_response_all(Workers1, Responses1)
    end.

handle_response_ring(Workers, Responses, CleanupCb) ->
    {MinB, MaxE} = range_bounds(Workers, Responses),
    Ranges = lists:map(fun({R, _, _}) -> R end, Responses),
    case mem3_util:get_ring(Ranges, MinB, MaxE) of
        [] ->
            {ok, {Workers, Responses}};
        Ring ->
            % Return one response per range in the ring. The
            % response list is reversed before sorting so that the
            % first shard copy to reply is first. We use keysort
            % because it is documented as being stable so that
            % we keep the relative order of duplicate shards
            SortedResponses = lists:keysort(1, lists:reverse(Responses)),
            UsedResponses = get_responses(Ring, SortedResponses),
            % Kill all the remaining workers as well as the redunant responses
            stop_unused_workers(Workers, Responses, UsedResponses, CleanupCb),
            {stop, fabric_dict:from_list(UsedResponses)}
    end.

handle_response_any(Shard, Response, Workers, Any, CleanupCb) ->
    case lists:member(Shard#shard{ref = undefined}, Any) of
        true ->
            stop_unused_workers(Workers, [], [], CleanupCb),
            {stop, fabric_dict:from_list([{Shard, Response}])};
        false ->
            {ok, {Workers, []}}
    end.

handle_response_all(Workers, Responses) ->
    case fabric_dict:size(Workers) =:= 0 of
        true ->
            {stop, fabric_dict:from_list(Responses)};
        false ->
            {ok, {Workers, Responses}}
    end.

% Check if workers still waiting and the already received responses could
% still form a continous range. The range won't always be the full ring, and
% the bounds are computed based on the minimum and maximum interval beginning
% and ends.
%
% There is also a special case where even if the ring cannot be formed, but
% there is an overlap between all the shards, then it's considered that
% progress can still be made. This is essentially to allow for split
% partitioned shards where one shard copy on a node was split the set of ranges
% might look like: 00-ff, 00-ff, 07-ff. Even if both 00-ff workers exit,
% progress can still be made with the remaining 07-ff copy.
%
-spec is_progress_possible(
    fabric_dict(),
    [{any(), #shard{}, any()}],
    non_neg_integer(),
    non_neg_integer(),
    ring_opts()
) -> boolean().
is_progress_possible([], [], _, _, _) ->
    false;
is_progress_possible(Counters, Responses, MinB, MaxE, []) ->
    ResponseRanges = lists:map(fun({{B, E}, _, _}) -> {B, E} end, Responses),
    Ranges = fabric_util:worker_ranges(Counters) ++ ResponseRanges,
    mem3_util:get_ring(Ranges, MinB, MaxE) =/= [];
is_progress_possible(Counters, _Responses, _, _, [all]) ->
    fabric_dict:size(Counters) > 0;
is_progress_possible(Counters, Responses, _, _, [{any, AnyShards}]) ->
    InAny = fun(S) -> lists:member(S#shard{ref = undefined}, AnyShards) end,
    case fabric_dict:filter(fun(S, _) -> InAny(S) end, Counters) of
        [] ->
            case lists:filter(fun({_, S, _}) -> InAny(S) end, Responses) of
                [] -> false;
                [_ | _] -> true
            end;
        [_ | _] ->
            true
    end.

get_shard_replacements_int(UnusedShards, UsedShards) ->
    % If we have more than one copy of a range then we don't
    % want to try and add a replacement to any copy.
    RangeCounts = lists:foldl(
        fun(#shard{range = R}, Acc) ->
            dict:update_counter(R, 1, Acc)
        end,
        dict:new(),
        UsedShards
    ),

    % For each seq shard range with a count of 1, find any
    % possible replacements from the unused shards. The
    % replacement list is keyed by range.
    lists:foldl(
        fun(#shard{range = [B, E] = Range}, Acc) ->
            case dict:find(Range, RangeCounts) of
                {ok, 1} ->
                    Repls = mem3_util:non_overlapping_shards(UnusedShards, B, E),
                    % Only keep non-empty lists of replacements
                    if
                        Repls == [] -> Acc;
                        true -> [{Range, Repls} | Acc]
                    end;
                _ ->
                    Acc
            end
        end,
        [],
        UsedShards
    ).

range_bounds(Workers, Responses) ->
    RespRanges = lists:map(fun({R, _, _}) -> R end, Responses),
    Ranges = fabric_util:worker_ranges(Workers) ++ RespRanges,
    {Bs, Es} = lists:unzip(Ranges),
    {lists:min(Bs), lists:max(Es)}.

get_responses([], _) ->
    [];
get_responses([Range | Ranges], [{Range, Shard, Value} | Resps]) ->
    [{Shard, Value} | get_responses(Ranges, Resps)];
get_responses(Ranges, [_DupeRangeResp | Resps]) ->
    get_responses(Ranges, Resps).

stop_unused_workers(_, _, _, undefined) ->
    ok;
stop_unused_workers(Workers, AllResponses, UsedResponses, CleanupCb) ->
    WorkerShards = [S || {S, _} <- Workers],
    Used = [S || {S, _} <- UsedResponses],
    Unused = [S || {_, S, _} <- AllResponses, not lists:member(S, Used)],
    CleanupCb(WorkerShards ++ Unused).

stop_workers(Shards) when is_list(Shards) ->
    rexi:kill_all([{Node, Ref} || #shard{node = Node, ref = Ref} <- Shards]).

% Unit tests

is_progress_possible_full_range_test() ->
    % a base case
    ?assertEqual(false, is_progress_possible([], [], 0, 0, [])),
    T1 = [[0, ?RING_END]],
    ?assertEqual(true, is_progress_possible(mk_cnts(T1))),
    T2 = [[0, 10], [11, 20], [21, ?RING_END]],
    ?assertEqual(true, is_progress_possible(mk_cnts(T2))),
    % gap
    T3 = [[0, 10], [12, ?RING_END]],
    ?assertEqual(false, is_progress_possible(mk_cnts(T3))),
    % outside range
    T4 = [[1, 10], [11, 20], [21, ?RING_END]],
    ?assertEqual(false, is_progress_possible(mk_cnts(T4))),
    % outside range
    T5 = [[0, 10], [11, 20], [21, ?RING_END + 1]],
    ?assertEqual(false, is_progress_possible(mk_cnts(T5))),
    % possible progress but with backtracking
    T6 = [[0, 10], [11, 20], [0, 5], [6, 21], [21, ?RING_END]],
    ?assertEqual(true, is_progress_possible(mk_cnts(T6))),
    % not possible, overlap is not exact
    T7 = [[0, 10], [13, 20], [21, ?RING_END], [9, 12]],
    ?assertEqual(false, is_progress_possible(mk_cnts(T7))).

is_progress_possible_with_responses_test() ->
    C1 = mk_cnts([[0, ?RING_END]]),
    ?assertEqual(true, is_progress_possible(C1, [], 0, ?RING_END, [])),
    % check for gaps
    C2 = mk_cnts([[5, 6], [7, 8]]),
    ?assertEqual(true, is_progress_possible(C2, [], 5, 8, [])),
    ?assertEqual(false, is_progress_possible(C2, [], 4, 8, [])),
    ?assertEqual(false, is_progress_possible(C2, [], 5, 7, [])),
    ?assertEqual(false, is_progress_possible(C2, [], 4, 9, [])),
    % check for uneven shard range copies
    C3 = mk_cnts([[2, 5], [2, 10]]),
    ?assertEqual(true, is_progress_possible(C3, [], 2, 10, [])),
    ?assertEqual(false, is_progress_possible(C3, [], 2, 11, [])),
    ?assertEqual(false, is_progress_possible(C3, [], 3, 10, [])),
    % they overlap but still not a proper ring
    C4 = mk_cnts([[2, 4], [3, 7], [6, 10]]),
    ?assertEqual(false, is_progress_possible(C4, [], 2, 10, [])),
    % some of the ranges are in responses
    RS1 = mk_resps([{"n1", 7, 8, 42}]),
    C5 = mk_cnts([[5, 6]]),
    ?assertEqual(true, is_progress_possible(C5, RS1, 5, 8, [])),
    ?assertEqual(false, is_progress_possible([], RS1, 5, 8, [])),
    ?assertEqual(true, is_progress_possible([], RS1, 7, 8, [])).

is_progress_possible_with_ring_opts_any_test() ->
    Opts = [{any, [mk_shard("n1", [0, 5]), mk_shard("n2", [3, 10])]}],
    C1 = [{mk_shard("n1", [0, ?RING_END]), nil}],
    RS1 = mk_resps([{"n1", 3, 10, 42}]),
    ?assertEqual(false, is_progress_possible(C1, [], 0, ?RING_END, Opts)),
    ?assertEqual(false, is_progress_possible([], [], 0, ?RING_END, Opts)),
    ?assertEqual(false, is_progress_possible([], RS1, 0, ?RING_END, Opts)),
    % explicitly accept only the shard specified in the ring options
    ?assertEqual(false, is_progress_possible([], RS1, 3, 10, [{any, []}])),
    % need to match the node exactly
    ?assertEqual(false, is_progress_possible([], RS1, 3, 10, Opts)),
    RS2 = mk_resps([{"n2", 3, 10, 42}]),
    ?assertEqual(true, is_progress_possible([], RS2, 3, 10, Opts)),
    % assert that counters can fill the ring not just the response
    C2 = [{mk_shard("n1", [0, 5]), nil}],
    ?assertEqual(true, is_progress_possible(C2, [], 0, ?RING_END, Opts)).

is_progress_possible_with_ring_opts_all_test() ->
    C1 = [{mk_shard("n1", [0, ?RING_END]), nil}],
    ?assertEqual(true, is_progress_possible(C1, [], 0, ?RING_END, [all])),
    ?assertEqual(false, is_progress_possible([], [], 0, ?RING_END, [all])).

get_shard_replacements_test() ->
    Unused = [
        mk_shard(N, [B, E])
     || {N, B, E} <- [
            {"n1", 11, 20},
            {"n1", 21, ?RING_END},
            {"n2", 0, 4},
            {"n2", 5, 10},
            {"n2", 11, 20},
            {"n3", 0, 21, ?RING_END}
        ]
    ],
    Used = [
        mk_shard(N, [B, E])
     || {N, B, E} <- [
            {"n2", 21, ?RING_END},
            {"n3", 0, 10},
            {"n3", 11, 20}
        ]
    ],
    Res = lists:sort(get_shard_replacements_int(Unused, Used)),
    % Notice that [0, 10] range can be replaces by spawning the
    % [0, 4] and [5, 10] workers on n1
    Expect = [
        {[0, 10], [mk_shard("n2", [0, 4]), mk_shard("n2", [5, 10])]},
        {[11, 20], [mk_shard("n1", [11, 20]), mk_shard("n2", [11, 20])]},
        {[21, ?RING_END], [mk_shard("n1", [21, ?RING_END])]}
    ],
    ?assertEqual(Expect, Res).

handle_response_basic_test() ->
    Shard1 = mk_shard("n1", [0, 1]),
    Shard2 = mk_shard("n1", [2, ?RING_END]),

    Workers1 = fabric_dict:init([Shard1, Shard2], nil),

    Result1 = handle_response(Shard1, 42, Workers1, [], [], undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, Responses1}} = Result1,
    ?assertEqual(fabric_dict:erase(Shard1, Workers1), Workers2),
    ?assertEqual([{{0, 1}, Shard1, 42}], Responses1),

    Result2 = handle_response(Shard2, 43, Workers2, Responses1, [], undefined),
    ?assertEqual({stop, [{Shard1, 42}, {Shard2, 43}]}, Result2).

handle_response_incomplete_ring_test() ->
    Shard1 = mk_shard("n1", [0, 1]),
    Shard2 = mk_shard("n1", [2, 10]),

    Workers1 = fabric_dict:init([Shard1, Shard2], nil),

    Result1 = handle_response(Shard1, 42, Workers1, [], [], undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, Responses1}} = Result1,
    ?assertEqual(fabric_dict:erase(Shard1, Workers1), Workers2),
    ?assertEqual([{{0, 1}, Shard1, 42}], Responses1),

    Result2 = handle_response(Shard2, 43, Workers2, Responses1, [], undefined),
    ?assertEqual({stop, [{Shard1, 42}, {Shard2, 43}]}, Result2).

handle_response_multiple_copies_test() ->
    Shard1 = mk_shard("n1", [0, 1]),
    Shard2 = mk_shard("n2", [0, 1]),
    Shard3 = mk_shard("n1", [2, ?RING_END]),

    Workers1 = fabric_dict:init([Shard1, Shard2, Shard3], nil),

    Result1 = handle_response(Shard1, 42, Workers1, [], [], undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, Responses1}} = Result1,

    Result2 = handle_response(Shard2, 43, Workers2, Responses1, [], undefined),
    ?assertMatch({ok, {_, _}}, Result2),
    {ok, {Workers3, Responses2}} = Result2,

    Result3 = handle_response(Shard3, 44, Workers3, Responses2, [], undefined),
    % Use the value (42) to distinguish between [0, 1] copies. In reality
    % they should have the same value but here we need to assert that copy
    % that responded first is included in the ring.
    ?assertEqual({stop, [{Shard1, 42}, {Shard3, 44}]}, Result3).

handle_response_backtracking_test() ->
    Shard1 = mk_shard("n1", [0, 5]),
    Shard2 = mk_shard("n1", [10, ?RING_END]),
    Shard3 = mk_shard("n2", [2, ?RING_END]),
    Shard4 = mk_shard("n3", [0, 1]),

    Workers1 = fabric_dict:init([Shard1, Shard2, Shard3, Shard4], nil),

    Result1 = handle_response(Shard1, 42, Workers1, [], [], undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, Responses1}} = Result1,

    Result2 = handle_response(Shard2, 43, Workers2, Responses1, [], undefined),
    ?assertMatch({ok, {_, _}}, Result2),
    {ok, {Workers3, Responses2}} = Result2,

    Result3 = handle_response(Shard3, 44, Workers3, Responses2, [], undefined),
    ?assertMatch({ok, {_, _}}, Result3),
    {ok, {Workers4, Responses3}} = Result3,

    Result4 = handle_response(Shard4, 45, Workers4, Responses3, [], undefined),
    ?assertEqual({stop, [{Shard3, 44}, {Shard4, 45}]}, Result4).

handle_response_ring_opts_any_test() ->
    Shard1 = mk_shard("n1", [0, 5]),
    Shard2 = mk_shard("n2", [0, 1]),
    Shard3 = mk_shard("n3", [0, 1]),

    Opts = [{any, [mk_shard("n3", [0, 1])]}],

    ShardList = [Shard1, Shard2, Shard3],
    WithRefs = [S#shard{ref = make_ref()} || S <- ShardList],
    Workers1 = fabric_dict:init(WithRefs, nil),

    Result1 = handle_response(Shard1, 42, Workers1, [], Opts, undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, []}} = Result1,

    % Still waiting because the node doesn't match
    Result2 = handle_response(Shard2, 43, Workers2, [], Opts, undefined),
    ?assertMatch({ok, {_, _}}, Result2),
    {ok, {Workers3, []}} = Result2,

    Result3 = handle_response(Shard3, 44, Workers3, [], Opts, undefined),
    ?assertEqual({stop, [{Shard3, 44}]}, Result3).

handle_response_ring_opts_all_test() ->
    Shard1 = mk_shard("n1", [0, 5]),
    Shard2 = mk_shard("n2", [0, 1]),
    Shard3 = mk_shard("n3", [0, 1]),

    ShardList = [Shard1, Shard2, Shard3],
    [W1, W2, W3] = WithRefs = [S#shard{ref = make_ref()} || S <- ShardList],
    Workers1 = fabric_dict:init(WithRefs, nil),

    Result1 = handle_response(W1, 42, Workers1, [], [all], undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, _}} = Result1,

    % Even though n2 and n3 cover the same range, with 'all' option we wait for
    % all workers to return.
    Result2 = handle_response(W2, 43, Workers2, [], [all], undefined),
    ?assertMatch({ok, {_, _}}, Result2),
    {ok, {Workers3, _}} = Result2,

    % Stop only after all the shards respond
    Result3 = handle_response(W3, 44, Workers3, [], [all], undefined),
    ?assertMatch({stop, [_ | _]}, Result3).

handle_error_test() ->
    Shard1 = mk_shard("n1", [0, 5]),
    Shard2 = mk_shard("n1", [10, ?RING_END]),
    Shard3 = mk_shard("n2", [2, ?RING_END]),
    Shard4 = mk_shard("n3", [0, 1]),

    Workers1 = fabric_dict:init([Shard1, Shard2, Shard3, Shard4], nil),

    Result1 = handle_response(Shard1, 42, Workers1, [], [], undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, Responses1}} = Result1,

    Result2 = handle_error(Shard2, Workers2, Responses1),
    ?assertMatch({ok, _}, Result2),
    {ok, Workers3} = Result2,
    ?assertEqual(fabric_dict:erase(Shard2, Workers2), Workers3),

    Result3 = handle_response(Shard3, 44, Workers3, Responses1, [], undefined),
    ?assertMatch({ok, {_, _}}, Result3),
    {ok, {Workers4, Responses3}} = Result3,
    ?assertEqual(error, handle_error(Shard4, Workers4, Responses3)).

node_down_test() ->
    Shard1 = mk_shard("n1", [0, 5]),
    Shard2 = mk_shard("n1", [10, ?RING_END]),
    Shard3 = mk_shard("n2", [2, ?RING_END]),
    Shard4 = mk_shard("n3", [0, 1]),

    Workers1 = fabric_dict:init([Shard1, Shard2, Shard3, Shard4], nil),

    Result1 = handle_response(Shard1, 42, Workers1, [], [], undefined),
    ?assertMatch({ok, {_, _}}, Result1),
    {ok, {Workers2, Responses1}} = Result1,

    Result2 = handle_response(Shard2, 43, Workers2, Responses1, [], undefined),
    ?assertMatch({ok, {_, _}}, Result2),
    {ok, {Workers3, Responses2}} = Result2,

    Result3 = node_down(n1, Workers3, Responses2),
    ?assertMatch({ok, _}, Result3),
    {ok, Workers4} = Result3,
    ?assertEqual([{Shard3, nil}, {Shard4, nil}], Workers4),

    Result4 = handle_response(Shard3, 44, Workers4, Responses2, [], undefined),
    ?assertMatch({ok, {_, _}}, Result4),
    {ok, {Workers5, Responses3}} = Result4,

    % Note: Shard3 was already processed, it's ok if n2 went down after
    ?assertEqual({ok, [{Shard4, nil}]}, node_down(n2, Workers5, Responses3)),

    ?assertEqual(error, node_down(n3, Workers5, Responses3)).

mk_cnts(Ranges) ->
    Shards = lists:map(fun mk_shard/1, Ranges),
    fabric_dict:init([S#shard{ref = make_ref()} || S <- Shards], nil).

mk_resps(RangeNameVals) ->
    [{{B, E}, mk_shard(Name, [B, E]), V} || {Name, B, E, V} <- RangeNameVals].

mk_shard([B, E]) when is_integer(B), is_integer(E) ->
    #shard{range = [B, E]}.

mk_shard(Name, Range) ->
    Node = list_to_atom(Name),
    BName = list_to_binary(Name),
    #shard{name = BName, node = Node, range = Range}.
