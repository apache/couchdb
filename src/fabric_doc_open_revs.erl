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

-module(fabric_doc_open_revs).

-export([go/4]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
    dbname,
    worker_count,
    workers,
    reply_count = 0,
    r,
    revs,
    latest,
    replies = [],
    repair = false
}).

go(DbName, Id, Revs, Options) ->
    Workers = fabric_util:submit_jobs(mem3:shards(DbName,Id), open_revs,
        [Id, Revs, Options]),
    R = couch_util:get_value(r, Options, integer_to_list(mem3:quorum(DbName))),
    State = #state{
        dbname = DbName,
        worker_count = length(Workers),
        workers = Workers,
        r = list_to_integer(R),
        revs = Revs,
        latest = lists:member(latest, Options),
        replies = []
    },
    RexiMon = fabric_util:create_monitors(Workers),
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, State) of
    {ok, {ok, Reply}} ->
        {ok, filter_reply(Reply)};
    {timeout, #state{workers=DefunctWorkers}} ->
        fabric_util:log_timeout(DefunctWorkers, "open_revs"),
        {error, timeout};
    Else ->
        Else
    after
        rexi_monitor:stop(RexiMon)
    end.


handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Worker, #state{workers=Workers}=State) ->
    NewState = State#state{
        workers = lists:keydelete(NodeRef, #shard.node, Workers)
    },
    handle_message({ok, []}, nil, NewState);

handle_message({rexi_EXIT, _}, Worker, #state{workers=Workers}=State) ->
    NewState = State#state{
        workers = lists:delete(Worker, Workers)
    },
    handle_message({ok, []}, nil, NewState);

handle_message({ok, RawReplies}, Worker, State) ->
    #state{
        dbname = DbName,
        reply_count = ReplyCount,
        worker_count = WorkerCount,
        workers = Workers,
        replies = PrevReplies,
        r = R,
        revs = Revs,
        latest = Latest,
        repair = InRepair
    } = State,

    IsTree = Revs == all orelse Latest,

    {NewReplies, QuorumMet, Repair} = case IsTree of
        true ->
            {NewReplies0, AllInternal, Repair0} =
                    tree_replies(PrevReplies, tree_sort(RawReplies)),
            NumLeafs = couch_key_tree:count_leafs(PrevReplies),
            SameNumRevs = length(RawReplies) == NumLeafs,
            QMet = AllInternal andalso SameNumRevs andalso ReplyCount + 1 >= R,
            {NewReplies0, QMet, Repair0};
        false ->
            {NewReplies0, MinCount} = dict_replies(PrevReplies, RawReplies),
            {NewReplies0, MinCount >= R, false}
    end,

    Complete = (ReplyCount =:= (WorkerCount - 1)),

    case QuorumMet orelse Complete of
        true ->
            fabric_util:cleanup(lists:delete(Worker, Workers)),
            maybe_read_repair(
                    DbName,
                    IsTree,
                    NewReplies,
                    ReplyCount + 1,
                    InRepair orelse Repair
                ),
            {stop, format_reply(IsTree, NewReplies)};
        false ->
            {ok, State#state{
                replies = NewReplies,
                reply_count = ReplyCount + 1,
                workers = lists:delete(Worker, Workers),
                repair = InRepair orelse Repair
            }}
    end.


tree_replies(RevTree, []) ->
    {RevTree, true, false};

tree_replies(RevTree0, [{ok, Doc} | Rest]) ->
    {RevTree1, Done, Repair} = tree_replies(RevTree0, Rest),
    Path = couch_doc:to_path(Doc),
    case couch_key_tree:merge(RevTree1, Path) of
        {RevTree2, internal_node} ->
            {RevTree2, Done, Repair};
        {RevTree2, new_leaf} ->
            {RevTree2, Done, true};
        {RevTree2, _} ->
            {RevTree2, false, true}
    end;

tree_replies(RevTree0, [{{not_found, missing}, {Pos, Rev}} | Rest]) ->
    {RevTree1, Done, Repair} = tree_replies(RevTree0, Rest),
    Node = {Rev, ?REV_MISSING, []},
    Path = {Pos, Node},
    case couch_key_tree:merge(RevTree1, Path) of
        {RevTree2, internal_node} ->
            {RevTree2, Done, true};
        {RevTree2, _} ->
            {RevTree2, false, Repair}
    end.


tree_sort(Replies) ->
    SortFun = fun(A, B) -> sort_key(A) =< sort_key(B) end,
    lists:sort(SortFun, Replies).


sort_key({ok, #doc{revs = {Pos, [Rev | _]}}}) ->
    {Pos, Rev};
sort_key({{not_found, _}, {Pos, Rev}}) ->
    {Pos, Rev}.


dict_replies(Dict, []) ->
    case [Count || {_Key, {_Reply, Count}} <- Dict] of
        [] -> {Dict, 0};
        Counts -> {Dict, lists:min(Counts)}
    end;

dict_replies(Dict, [Reply | Rest]) ->
    NewDict = fabric_util:update_counter(Reply, 1, Dict),
    dict_replies(NewDict, Rest).


maybe_read_repair(Db, IsTree, Replies, ReplyCount, DoRepair) ->
    Docs = case IsTree of
        true -> tree_repair_docs(Replies, DoRepair);
        false -> dict_repair_docs(Replies, ReplyCount)
    end,
    case Docs of
        [] ->
            ok;
        _ ->
            erlang:spawn(fun() -> read_repair(Db, Docs) end)
    end.


tree_repair_docs(_Replies, false) ->
    [];

tree_repair_docs(Replies, true) ->
    Leafs = couch_key_tree:get_all_leafs(Replies),
    [Doc || {Doc, {_Pos, _}} <- Leafs, is_record(Doc, doc)].


dict_repair_docs(Replies, ReplyCount) ->
    NeedsRepair = lists:any(fun({_, {_, C}}) -> C < ReplyCount end, Replies),
    if not NeedsRepair -> []; true ->
        [Doc || {_, {{ok, Doc}, _}} <- Replies]
    end.


read_repair(Db, Docs) ->
    Res = fabric:update_docs(Db, Docs, [replicated_changes, ?ADMIN_CTX]),
    case Res of
        {ok, []} ->
            couch_stats:increment_counter([fabric, read_repairs, success]);
        _ ->
            couch_stats:increment_counter([fabric, read_repairs, failure]),
            [#doc{id = Id} | _] = Docs,
            couch_log:notice("read_repair ~s ~s ~p", [Db, Id, Res])
    end.


format_reply(true, Replies) ->
    tree_format_replies(Replies);

format_reply(false, Replies) ->
    dict_format_replies(Replies).


tree_format_replies(RevTree) ->
    Leafs = couch_key_tree:get_all_leafs(RevTree),
    lists:sort(lists:map(fun(Reply) ->
        case Reply of
            {?REV_MISSING, {Pos, [Rev]}} ->
                {{not_found, missing}, {Pos, Rev}};
            {Doc, _} when is_record(Doc, doc) ->
                {ok, Doc}
        end
    end, Leafs)).


dict_format_replies(Dict) ->
    lists:sort([Reply || {_, {Reply, _}} <- Dict]).

filter_reply(Replies) ->
    case [{ok, Doc} || {ok, Doc} <- Replies] of
        [] -> Replies;
        Filtered -> Filtered
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    config:start_link([]),
    meck:new([fabric, couch_stats, couch_log]),
    meck:expect(fabric, update_docs, fun(_, _, _) -> {ok, nil} end),
    meck:expect(couch_stats, increment_counter, fun(_) -> ok end),
    meck:expect(couch_log, notice, fun(_, _) -> ok end).


teardown(_) ->
    (catch meck:unload([fabric, couch_stats, couch_log])),
    config:stop().


state0(Revs, Latest) ->
    #state{
        worker_count = 3,
        workers = [w1, w2, w3],
        r = 2,
        revs = Revs,
        latest = Latest
    }.


revs() -> [{1,<<"foo">>}, {1,<<"bar">>}, {1,<<"baz">>}].


foo1() -> {ok, #doc{revs = {1, [<<"foo">>]}}}.
foo2() -> {ok, #doc{revs = {2, [<<"foo2">>, <<"foo">>]}}}.
fooNF() -> {{not_found, missing}, {1,<<"foo">>}}.
bar1() -> {ok, #doc{revs = {1, [<<"bar">>]}}}.
barNF() -> {{not_found, missing}, {1,<<"bar">>}}.
bazNF() -> {{not_found, missing}, {1,<<"baz">>}}.
baz1() -> {ok, #doc{revs = {1, [<<"baz">>]}}}.



open_doc_revs_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            check_empty_response_not_quorum(),
            check_basic_response(),
            check_finish_quorum(),
            check_finish_quorum_newer(),
            check_no_quorum_on_second(),
            check_done_on_third(),
            check_specific_revs_first_msg(),
            check_revs_done_on_agreement(),
            check_latest_true(),
            check_ancestor_counted_in_quorum(),
            check_not_found_counts_for_descendant(),
            check_worker_error_skipped(),
            check_not_found_replies_are_removed_when_doc_found(),
            check_not_found_returned_when_doc_not_found()
        ]
    }.


% Tests for revs=all


check_empty_response_not_quorum() ->
    % Simple smoke test that we don't think we're
    % done with a first empty response
    ?_assertMatch(
        {ok, #state{workers = [w2, w3]}},
        handle_message({ok, []}, w1, state0(all, false))
    ).


check_basic_response() ->
    % Check that we've handle a response
    ?_assertMatch(
        {ok, #state{reply_count = 1, workers = [w2, w3]}},
        handle_message({ok, [foo1(), bar1()]}, w1, state0(all, false))
    ).


check_finish_quorum() ->
    % Two messages with the same revisions means we're done
    ?_test(begin
        S0 = state0(all, false),
        {ok, S1} = handle_message({ok, [foo1(), bar1()]}, w1, S0),
        Expect = {stop, [bar1(), foo1()]},
        ?assertEqual(Expect, handle_message({ok, [foo1(), bar1()]}, w2, S1))
    end).


check_finish_quorum_newer() ->
    % We count a descendant of a revision for quorum so
    % foo1 should count for foo2 which means we're finished.
    % We also validate that read_repair was triggered.
    ?_test(begin
        S0 = state0(all, false),
        {ok, S1} = handle_message({ok, [foo1(), bar1()]}, w1, S0),
        Expect = {stop, [bar1(), foo2()]},
        ok = meck:reset(fabric),
        ?assertEqual(Expect, handle_message({ok, [foo2(), bar1()]}, w2, S1)),
        ok = meck:wait(fabric, update_docs, '_', 5000),
        ?assertMatch(
            [{_, {fabric, update_docs, [_, _, _]}, _}],
            meck:history(fabric)
        )
    end).


check_no_quorum_on_second() ->
    % Quorum not yet met for the foo revision so we
    % would wait for w3
    ?_test(begin
        S0 = state0(all, false),
        {ok, S1} = handle_message({ok, [foo1(), bar1()]}, w1, S0),
        ?assertMatch(
            {ok, #state{workers = [w3]}},
            handle_message({ok, [bar1()]}, w2, S1)
        )
    end).


check_done_on_third() ->
    % The third message of three means we're done no matter
    % what. Every revision seen in this pattern should be
    % included.
    ?_test(begin
        S0 = state0(all, false),
        {ok, S1} = handle_message({ok, [foo1(), bar1()]}, w1, S0),
        {ok, S2} = handle_message({ok, [bar1()]}, w2, S1),
        Expect = {stop, [bar1(), foo1()]},
        ?assertEqual(Expect, handle_message({ok, [bar1()]}, w3, S2))
    end).


% Tests for a specific list of revs


check_specific_revs_first_msg() ->
    ?_test(begin
        S0 = state0(revs(), false),
        ?assertMatch(
            {ok, #state{reply_count = 1, workers = [w2, w3]}},
            handle_message({ok, [foo1(), bar1(), bazNF()]}, w1, S0)
        )
    end).


check_revs_done_on_agreement() ->
    ?_test(begin
        S0 = state0(revs(), false),
        Msg = {ok, [foo1(), bar1(), bazNF()]},
        {ok, S1} = handle_message(Msg, w1, S0),
        Expect = {stop, [bar1(), foo1(), bazNF()]},
        ?assertEqual(Expect, handle_message(Msg, w2, S1))
    end).


check_latest_true() ->
    ?_test(begin
        S0 = state0(revs(), true),
        Msg1 = {ok, [foo2(), bar1(), bazNF()]},
        Msg2 = {ok, [foo2(), bar1(), bazNF()]},
        {ok, S1} = handle_message(Msg1, w1, S0),
        Expect = {stop, [bar1(), foo2(), bazNF()]},
        ?assertEqual(Expect, handle_message(Msg2, w2, S1))
    end).


check_ancestor_counted_in_quorum() ->
    ?_test(begin
        S0 = state0(revs(), true),
        Msg1 = {ok, [foo1(), bar1(), bazNF()]},
        Msg2 = {ok, [foo2(), bar1(), bazNF()]},
        Expect = {stop, [bar1(), foo2(), bazNF()]},

        % Older first
        {ok, S1} = handle_message(Msg1, w1, S0),
        ?assertEqual(Expect, handle_message(Msg2, w2, S1)),

        % Newer first
        {ok, S2} = handle_message(Msg2, w2, S0),
        ?assertEqual(Expect, handle_message(Msg1, w1, S2))
    end).


check_not_found_counts_for_descendant() ->
    ?_test(begin
        S0 = state0(revs(), true),
        Msg1 = {ok, [foo1(), bar1(), bazNF()]},
        Msg2 = {ok, [foo1(), bar1(), baz1()]},
        Expect = {stop, [bar1(), baz1(), foo1()]},

        % not_found first
        {ok, S1} = handle_message(Msg1, w1, S0),
        ?assertEqual(Expect, handle_message(Msg2, w2, S1)),

        % not_found second
        {ok, S2} = handle_message(Msg2, w2, S0),
        ?assertEqual(Expect, handle_message(Msg1, w1, S2))
    end).


check_worker_error_skipped() ->
    ?_test(begin
        S0 = state0(revs(), true),
        Msg1 = {ok, [foo1(), bar1(), baz1()]},
        Msg2 = {rexi_EXIT, reason},
        Msg3 = {ok, [foo1(), bar1(), baz1()]},
        Expect = {stop, [bar1(), baz1(), foo1()]},

        {ok, S1} = handle_message(Msg1, w1, S0),
        {ok, S2} = handle_message(Msg2, w2, S1),
        ?assertEqual(Expect, handle_message(Msg3, w2, S2))
    end).

check_not_found_replies_are_removed_when_doc_found() ->
    ?_test(begin
        Replies = [foo1(), bar1(), bazNF()],
        Expect = [foo1(), bar1()],
        ?assertEqual(Expect, filter_reply(Replies))
    end).

check_not_found_returned_when_doc_not_found() ->
    ?_test(begin
        Replies = [fooNF(), barNF(), bazNF()],
        Expect = [fooNF(), barNF(), bazNF()],
        ?assertEqual(Expect, filter_reply(Replies))
    end).


-endif.
