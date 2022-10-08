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

-module(fabric_open_revs).

-export([
    go/3
]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(req, {
    idrevs,
    wcnt = 0,
    rcnt = 0,
    responses = []
}).

-record(st, {
    r,
    args,
    reqs,
    workers
}).

go(_DbName, [], _Options) ->
    {ok, []};
go(DbName, IdsRevsOpts, Options) ->
    St = init_state(DbName, IdsRevsOpts, Options),
    WShards = maps:keys(St#st.workers),
    RexiMon = fabric_util:create_monitors(WShards),
    try fabric_util:recv(WShards, #shard.ref, fun handle_message/3, St) of
        {timeout, #st{workers = #{} = Workers1}} ->
            stop_workers(Workers1),
            fabric_util:log_timeout(maps:keys(Workers1), "open_revs"),
            {error, timeout};
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message([_ | _] = Resps, Worker, #st{} = St) ->
    #st{workers = Workers, reqs = Reqs, r = R} = St,
    {ArgsRefs, Workers1} = maps:take(Worker, Workers),
    ArgsResps = lists:zip(ArgsRefs, Resps),
    Reqs1 = lists:foldl(fun responses_fold/2, Reqs, ArgsResps),
    case not r_met(Reqs1, R) andalso have_viable_workers(Workers1) of
        true ->
            {ok, St#st{workers = Workers1, reqs = Reqs1}};
        false ->
            stop_workers(Workers1),
            {stop, finalize(St#st.args, Reqs1)}
    end;
handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Worker, #st{} = St) ->
    #st{workers = Workers, reqs = Reqs} = St,
    FilterFun = fun(#shard{node = N}) -> N =:= NodeRef end,
    DeadKeys = lists:filter(FilterFun, maps:keys(Workers)),
    Workers1 = maps:without(DeadKeys, Workers),
    DeadWorkers = maps:with(DeadKeys, Workers),
    FoldFun = fun(_, ArgRefs, Acc) -> update_wcnt(Acc, ArgRefs, -1) end,
    Reqs1 = maps:fold(FoldFun, Reqs, DeadWorkers),
    Error = {error, {nodedown, <<"progress not possible">>}},
    handle_error(Error, St#st{workers = Workers1, reqs = Reqs1});
handle_message({rexi_EXIT, Reason}, Worker, #st{} = St) ->
    handle_message(Reason, Worker, St);
handle_message({error, Reason}, Worker, #st{} = St) ->
    handle_message(Reason, Worker, St);
handle_message(Reason, Worker, #st{} = St) ->
    #st{workers = Workers, reqs = Reqs} = St,
    {DeadArgRefs, Workers1} = maps:take(Worker, Workers),
    Reqs1 = update_wcnt(Reqs, DeadArgRefs, -1),
    handle_error(Reason, St#st{workers = Workers1, reqs = Reqs1}).

init_state(DbName, IdsRevsOpts, Options) ->
    DefaultR = integer_to_list(mem3:quorum(DbName)),
    R = list_to_integer(couch_util:get_value(r, Options, DefaultR)),
    {ArgRefs, Reqs0} = build_req_map(IdsRevsOpts),
    ShardMap = build_worker_map(DbName, Reqs0),
    {Workers, Reqs} = spawn_workers(Reqs0, ShardMap, Options),
    #st{r = R, args = ArgRefs, reqs = Reqs, workers = Workers}.

responses_fold({ArgRef, NewResp}, #{} = Reqs) ->
    #{ArgRef := Req} = Reqs,
    #req{rcnt = R, wcnt = W, responses = Resps} = Req,
    Resps1 = merge_responses(Resps, NewResp),
    % If responses don't match or are "not found", don't bump rcnt so we can
    % wait for more workers.
    OldLen = length(Resps),
    NewLen = length(Resps1),
    NewR =
        case {any_not_found(NewResp), OldLen} of
            {true, _} -> R;
            {_, 0} -> R + 1;
            {_, L} when L == NewLen -> R + 1;
            {_, L} when L < NewLen -> R
        end,
    Reqs#{
        ArgRef => Req#req{
            rcnt = NewR,
            wcnt = W - 1,
            responses = Resps1
        }
    }.

handle_error(Error, #st{workers = Workers, reqs = Reqs} = St) ->
    case success_possible(Reqs) of
        true ->
            case have_viable_workers(Workers) of
                true ->
                    {ok, St};
                false ->
                    % Don't have a choice, have to stop
                    {stop, finalize(St#st.args, Reqs)}
            end;
        false ->
            stop_workers(Workers),
            {error, Error}
    end.

% De-duplicate identical responses as we go along
%
merge_responses(Responses, Response) ->
    Fun = fun(A, B) -> sort_key(A) =< sort_key(B) end,
    lists:umerge(Fun, Responses, lists:usort(Fun, Response)).

% Assuming docs in most cases will be identical, so check their revs and other
% metadata and avoid checking their body, which can be quite a bit a larger,
% just to figure they are actually the same.
%
sort_key({ok, #doc{id = Id, revs = Revs, deleted = Deleted}}) ->
    {Revs, Deleted, Id};
sort_key(NotFound) ->
    NotFound.

% Build a #{ArgRef => #req{}} map. ArgRef references the initial {{Id, Revs},
% Opts} tuples and the #req{} is a record keeping track of response for just
% that {Id, Revs} pair.
%
build_req_map(IdsRevsOpts) ->
    Fun = fun(IdRevsOpts, Acc) ->
        ArgRef = make_ref(),
        {ArgRef, Acc#{ArgRef => #req{idrevs = IdRevsOpts}}}
    end,
    lists:mapfoldr(Fun, #{}, IdsRevsOpts).

% Build a #{#shard{} => [ArgRef1, ArgRef2, ...]} map. This structure will be
% used for launching workers and then matching up response with the original
% args.
%
build_worker_map(DbName, #{} = Reqs) ->
    FoldReqsFun = fun(ArgRef, #req{idrevs = IdRevs}, WAcc) ->
        {{DocId, _}, _} = IdRevs,
        FoldShardsFun = fun(Shard, WAccInner) ->
            UpdateFun = fun(ArgRefs) -> [ArgRef | ArgRefs] end,
            maps:update_with(Shard, UpdateFun, [ArgRef], WAccInner)
        end,
        lists:foldl(FoldShardsFun, WAcc, mem3:shards(DbName, DocId))
    end,
    maps:fold(FoldReqsFun, #{}, Reqs).

spawn_workers(#{} = Reqs, #{} = ShardMap, Options) ->
    Fun = fun(Shard, ArgRefs, {WAcc, ReqsAcc}) ->
        Worker = rexi_cast(Shard, ArgRefs, ReqsAcc, Options),
        WAcc1 = WAcc#{Worker => ArgRefs},
        ReqsAcc1 = update_wcnt(ReqsAcc, ArgRefs, 1),
        {WAcc1, ReqsAcc1}
    end,
    maps:fold(Fun, {#{}, Reqs}, ShardMap).

% Spawn a worker and return an updated #shard{} with worker ref
% Args are fetched from the Reqs map based on the ArgRef tag
%
rexi_cast(#shard{} = Shard, ArgRefs, #{} = Reqs, Options) ->
    Fun = fun(ArgRef) when is_reference(ArgRef) ->
        #{ArgRef := #req{idrevs = IdRevs}} = Reqs,
        IdRevs
    end,
    Args = lists:map(Fun, ArgRefs),
    RexiArgs = {fabric_rpc, open_revs, [Shard#shard.name, Args, Options]},
    WRef = rexi:cast(Shard#shard.node, RexiArgs),
    Shard#shard{ref = WRef}.

% Update worker count for each of the #req{} records. Value may be positive
% or negative, which could be used to decrement worker counts
%
update_wcnt(#{} = Reqs, ArgRefs, Val) when is_integer(Val) ->
    Fun = fun(Ref, Acc) ->
        #{Ref := #req{wcnt = W} = Req} = Acc,
        Acc#{Ref => Req#req{wcnt = W + Val}}
    end,
    lists:foldl(Fun, Reqs, ArgRefs).

% Return true if we still have any outstanding workers we could wait on
%
have_viable_workers(#{} = Workers) ->
    map_size(Workers) > 0.

% We can still return success if we either have some waiting workers, or at
% least one response already for each {Id, Revs} pair.
%
success_possible(#{} = Reqs) ->
    Fun = fun(_, #req{wcnt = W, rcnt = R}, Acc) -> Acc andalso W + R > 0 end,
    maps:fold(Fun, true, Reqs).

r_met(#{} = Reqs, ExpectedR) ->
    Fun = fun(_, #req{rcnt = R}, Acc) -> min(R, Acc) end,
    maps:fold(Fun, ExpectedR, Reqs) >= ExpectedR.

finalize(ArgRefs, #{} = Reqs) ->
    Fun = fun(Ref) ->
        #{Ref := #req{responses = Resps}} = Reqs,
        finalize_req(Resps)
    end,
    lists:map(Fun, ArgRefs).

finalize_req(DocRevs) ->
    Paths = lists:map(fun rev_to_path/1, DocRevs),
    RevTree = couch_key_tree:multi_merge([], Paths),
    TreeLeafs = couch_key_tree:get_all_leafs(RevTree),
    lists:map(fun path_to_reply/1, TreeLeafs).

path_to_reply({?REV_MISSING, {Pos, [Rev]}}) ->
    {{not_found, missing}, {Pos, Rev}};
path_to_reply({#doc{} = Doc, _}) ->
    {ok, Doc}.

any_not_found([]) ->
    true;
any_not_found([_ | _] = Revs) ->
    Fun = fun
        ({{not_found, missing}, {_, _}}) -> true;
        (_) -> false
    end,
    lists:any(Fun, Revs).

rev_to_path({ok, #doc{} = Doc}) ->
    couch_doc:to_path(Doc);
rev_to_path({{not_found, missing}, {Pos, Rev}}) ->
    {Pos, {Rev, ?REV_MISSING, []}}.

stop_workers(#{} = Workers) ->
    Fun = fun(#shard{node = Node, ref = Ref}) -> {Node, Ref} end,
    NodeRefs = lists:map(Fun, maps:keys(Workers)),
    rexi:kill_all(NodeRefs).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

setup_all() ->
    config:start_link([]),
    meck:new([fabric, couch_stats, couch_log]),
    meck:new(rexi, [passthrough]),
    meck:new(mem3, [passthrough]),
    meck:new(fabric_util, [passthrough]),
    meck:expect(fabric, update_docs, fun(_, _, _) -> {ok, nil} end),
    meck:expect(mem3, quorum, fun(_) -> 2 end),
    meck:expect(mem3, shards, fun(<<"db">>, <<"a">>) ->
        [#shard{node = 'n1'}, #shard{node = 'n2'}, #shard{node = 'n3'}]
    end),
    meck:expect(couch_stats, increment_counter, fun(_) -> ok end),
    meck:expect(couch_log, notice, fun(_, _) -> ok end),
    meck:expect(rexi, cast, fun(_, _) -> make_ref() end),
    meck:expect(rexi, kill_all, fun(_) -> ok end).

teardown_all(_) ->
    meck:unload(),
    config:stop().

setup() ->
    meck:reset([
        couch_log,
        mem3,
        rexi,
        couch_stats,
        fabric,
        fabric_util
    ]).

teardown(_) ->
    ok.

st0() ->
    IdRevsOpts = [{{<<"a">>, all}, []}],
    init_state(<<"db">>, IdRevsOpts, []).

foo1() -> {ok, #doc{revs = {1, [<<"foo">>]}}}.
foo2() -> {ok, #doc{revs = {2, [<<"foo2">>, <<"foo">>]}}}.
foo2stemmed() -> {ok, #doc{revs = {2, [<<"foo2">>]}}}.
bar1() -> {ok, #doc{revs = {1, [<<"bar">>]}}}.
bazNF() -> {{not_found, missing}, {1, <<"baz">>}}.
baz1() -> {ok, #doc{revs = {1, [<<"baz">>]}}}.

open_revs_quorum_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_empty_request_gets_an_empty_result),
                ?TDEF_FE(t_initial_state),
                ?TDEF_FE(t_basic_response),
                ?TDEF_FE(t_finish_quorum),
                ?TDEF_FE(t_no_quorum_on_different_responses),
                ?TDEF_FE(t_no_quorum_on_not_found),
                ?TDEF_FE(t_done_on_third),
                ?TDEF_FE(t_all_different_responses),
                ?TDEF_FE(t_ancestors_merge_correctly),
                ?TDEF_FE(t_stemmed_merge_correctly),
                ?TDEF_FE(t_not_found_counted_as_descendant),
                ?TDEF_FE(t_all_not_found),
                ?TDEF_FE(t_rev_not_found_returned),
                ?TDEF_FE(t_rexi_errors_progress),
                ?TDEF_FE(t_generic_errors_progress),
                ?TDEF_FE(t_failure_on_all_errors)
            ]
        }
    }.

t_empty_request_gets_an_empty_result(_) ->
    ?assertEqual({ok, []}, go(<<"foo">>, [], [])).

t_initial_state(_) ->
    % Smoke test that we have setup our state correctly
    S0 = st0(),
    ?assertMatch(#st{args = [_], r = 2, reqs = #{}, workers = #{}}, S0),
    #st{args = ArgsRefs, reqs = Reqs, workers = Workers} = S0,
    % The args refs list is the reqs keys
    ?assertEqual(lists:sort(ArgsRefs), lists:sort(maps:keys(Reqs))),
    % Each worker's args is from the args refs list and there are no args which
    % which haven't been assigned to at least one worker.
    SpawnedArgs = lists:flatten(maps:values(Workers)),
    ?assertEqual(lists:sort(ArgsRefs), lists:usort(SpawnedArgs)),
    maps:map(
        fun(_, #req{} = Req) ->
            % 3 workers were spawned
            ?assertEqual(3, Req#req.wcnt),
            % no workers have returned yet
            ?assertEqual(0, Req#req.rcnt),
            % responses is an empty list (since no workers returned yet)
            ?assertEqual([], Req#req.responses)
        end,
        Reqs
    ).

t_basic_response(_) ->
    S0 = #st{workers = Workers0} = st0(),
    [W1 | _] = lists:sort(maps:keys(Workers0)),
    Res = handle_message([[foo1(), bar1()]], W1, S0),
    ?assertMatch({ok, #st{}}, Res),
    {ok, #st{reqs = Reqs, workers = Workers1}} = Res,
    ?assertEqual(2, map_size(Workers1)),
    ?assertNot(maps:is_key(W1, Workers1)),
    ?assertEqual(1, map_size(Reqs)),
    [#req{wcnt = W, rcnt = R}] = maps:values(Reqs),
    ?assertEqual(2, W),
    ?assertEqual(1, R).

t_finish_quorum(_) ->
    % Two messages with the same revisions means we're done
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2 | _] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[foo1(), bar1()]], W1, S0),
    Res = handle_message([[bar1(), foo1()]], W2, S1),
    % Got the same revisions as previous message, so we're done
    ?assertEqual({stop, [[foo1(), bar1()]]}, Res).

t_no_quorum_on_different_responses(_) ->
    % Got different revisions, so we're waiting for more workers.
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[foo1(), bar1()]], W1, S0),
    Res1 = handle_message([[foo2(), bar1()]], W2, S1),
    ?assertMatch({ok, #st{}}, Res1),
    {ok, S2} = Res1,
    % Now we have both case were all workers have returned and also the quorum
    % was reached.
    Res2 = handle_message([[foo1(), bar1()]], W3, S2),
    ?assertEqual({stop, [[foo2(), bar1()]]}, Res2).

t_no_quorum_on_not_found(_) ->
    % Got a [] (not found), so wait for more workers
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[foo1(), bar1()]], W1, S0),
    {ok, S2} = handle_message([[]], W2, S1),
    Res2 = handle_message([[foo2(), bar1()]], W3, S2),
    ?assertEqual({stop, [[foo2(), bar1()]]}, Res2).

t_done_on_third(_) ->
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[]], W1, S0),
    {ok, S2} = handle_message([[foo2(), bar1()]], W2, S1),
    ?assertEqual({stop, [[foo2(), bar1()]]}, handle_message([[]], W3, S2)).

t_all_different_responses(_) ->
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[bar1()]], W1, S0),
    {ok, S2} = handle_message([[foo2(), bar1()]], W2, S1),
    Res = handle_message([[foo1(), bazNF()]], W3, S2),
    Expect = [[foo2(), bazNF(), bar1()]],
    ?assertEqual({stop, Expect}, Res).

t_ancestors_merge_correctly(_) ->
    % Ancestors are merged as internal nodes correctly
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[foo1()]], W1, S0),
    {ok, S2} = handle_message([[foo2()]], W2, S1),
    ?assertEqual({stop, [[foo2()]]}, handle_message([[]], W3, S2)).

t_stemmed_merge_correctly(_) ->
    % Ancestors are merged as internal nodes correctly. That includes stemmed
    % revisions.
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[foo2()]], W1, S0),
    {ok, S2} = handle_message([[foo2stemmed()]], W2, S1),
    ?assertEqual({stop, [[foo2(), bar1()]]}, handle_message([[bar1()]], W3, S2)).

t_not_found_counted_as_descendant(_) ->
    % not_found counted as a descendant when merged with found rev of at the
    % same level
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[bar1(), bazNF()]], W1, S0),
    {ok, S2} = handle_message([[bar1(), baz1()]], W2, S1),
    {stop, [Revs]} = handle_message([[foo1()]], W3, S2),
    ?assertEqual([foo1(), baz1(), bar1()], Revs).

t_all_not_found(_) ->
    % All not found responses
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[]], W1, S0),
    {ok, S2} = handle_message([[]], W2, S1),
    ?assertEqual({stop, [[]]}, handle_message([[]], W3, S2)).

t_rev_not_found_returned(_) ->
    % If a specific rev is not found that is returned
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[bazNF()]], W1, S0),
    {ok, S2} = handle_message([[bazNF()]], W2, S1),
    ?assertEqual({stop, [[bazNF()]]}, handle_message([[bazNF()]], W3, S2)).

t_rexi_errors_progress(_) ->
    % Got two rexi errors and one good result
    S0 = #st{workers = Workers0} = st0(),
    [W1, _, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message([[foo1()]], W1, S0),
    {ok, S2} = handle_message({rexi_DOWN, nodedown, {x, n2}, y}, z, S1),
    ?assertEqual({stop, [[foo1()]]}, handle_message({rexi_EXIT, w}, W3, S2)).

t_generic_errors_progress(_) ->
    % Got two generic errors but can still succeed
    S0 = #st{workers = Workers0} = st0(),
    [W1, W2, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message({error, x}, W1, S0),
    {ok, S2} = handle_message([[bar1()]], W2, S1),
    ?assertEqual({stop, [[bar1()]]}, handle_message(z, W3, S2)).

t_failure_on_all_errors(_) ->
    S0 = #st{workers = Workers0} = st0(),
    [W1, _, W3] = lists:sort(maps:keys(Workers0)),
    {ok, S1} = handle_message({error, k}, W1, S0),
    {ok, S2} = handle_message({rexi_DOWN, nodedown, {x, n2}, y}, z, S1),
    ?assertEqual({error, e}, handle_message({rexi_EXIT, e}, W3, S2)).

-endif.
