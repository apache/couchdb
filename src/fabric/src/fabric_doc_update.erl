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

-module(fabric_doc_update).

-export([go/3]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(acc, {
    waiting_count,
    doc_count,
    w,
    grouped_docs,
    reply,
    dbname,
    update_options = [],
    started = [],
    conflicts = [],
    serialize_worker_startup = true
}).

go(_, [], _) ->
    {ok, []};
go(DbName, AllDocs0, Opts) ->
    AllDocs1 = before_doc_update(DbName, AllDocs0, Opts),
    AllDocs = tag_docs(AllDocs1),
    validate_atomic_update(DbName, AllDocs, lists:member(all_or_nothing, Opts)),
    Options = lists:delete(all_or_nothing, Opts),
    GroupedDocs = lists:map(
        fun({#shard{} = Shard, Docs}) ->
            {Shard#shard{ref = make_ref()}, Docs}
        end,
        group_docs_by_shard(DbName, AllDocs)
    ),
    {Workers, _} = lists:unzip(GroupedDocs),
    RexiMon = fabric_util:create_monitors(Workers),
    Acc0 = #acc{
        waiting_count = length(Workers),
        doc_count = length(AllDocs),
        w = fabric_util:w_from_opts(DbName, Options),
        grouped_docs = GroupedDocs,
        reply = dict:new(),
        dbname = DbName,
        update_options = Options,
        serialize_worker_startup = serialize_worker_startup(Options)
    },
    Timeout = fabric_util:request_timeout(),
    Acc1 = start_workers_strategy(Acc0),
    try rexi_utils:recv(Workers, #shard.ref, fun handle_message/3, Acc1, infinity, Timeout) of
        {ok, {Health, Results}} when
            Health =:= ok; Health =:= accepted; Health =:= error
        ->
            ensure_all_responses(Health, AllDocs, Results);
        {timeout, Acc} ->
            #acc{
                w = W1,
                grouped_docs = GroupedDocs1,
                reply = DocReplDict,
                serialize_worker_startup = SWS
            } = Acc,
            {DefunctWorkers, _} = lists:unzip(GroupedDocs1),
            fabric_util:log_timeout(DefunctWorkers, "update_docs"),
            {Health, _, _, Resp} = dict:fold(
                fun force_reply/3,
                {ok, W1, SWS, []},
                DocReplDict
            ),
            ensure_all_responses(Health, AllDocs, Resp);
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Worker, #acc{} = Acc0) ->
    #acc{grouped_docs = GroupedDocs} = Acc0,
    NewGrpDocs = [X || {#shard{node = N}, _} = X <- GroupedDocs, N =/= NodeRef],
    skip_message(
        start_workers(Acc0#acc{waiting_count = length(NewGrpDocs), grouped_docs = NewGrpDocs})
    );
handle_message({rexi_EXIT, _}, Worker, #acc{} = Acc0) ->
    #acc{waiting_count = WC, grouped_docs = GrpDocs} = Acc0,
    NewGrpDocs = lists:keydelete(Worker, 1, GrpDocs),
    skip_message(start_workers(Acc0#acc{waiting_count = WC - 1, grouped_docs = NewGrpDocs}));
handle_message({error, all_dbs_active}, Worker, #acc{} = Acc0) ->
    % treat it like rexi_EXIT, the hope at least one copy will return successfully
    #acc{waiting_count = WC, grouped_docs = GrpDocs} = Acc0,
    NewGrpDocs = lists:keydelete(Worker, 1, GrpDocs),
    skip_message(start_workers(Acc0#acc{waiting_count = WC - 1, grouped_docs = NewGrpDocs}));
handle_message(internal_server_error, Worker, #acc{} = Acc0) ->
    % happens when we fail to load validation functions in an RPC worker
    #acc{waiting_count = WC, grouped_docs = GrpDocs} = Acc0,
    NewGrpDocs = lists:keydelete(Worker, 1, GrpDocs),
    skip_message(start_workers(Acc0#acc{waiting_count = WC - 1, grouped_docs = NewGrpDocs}));
handle_message(attachment_chunk_received, _Worker, #acc{} = Acc0) ->
    {ok, Acc0};
handle_message({ok, Replies}, Worker, #acc{} = Acc0) ->
    #acc{
        waiting_count = WaitingCount,
        doc_count = DocCount,
        w = W,
        grouped_docs = GroupedDocs,
        reply = DocReplyDict0,
        conflicts = Conflicts0,
        serialize_worker_startup = SWS
    } = Acc0,
    {value, {_, Docs}, NewGrpDocs} = lists:keytake(Worker, 1, GroupedDocs),
    Conflicts = collect_conflicts(Docs, Replies, Conflicts0, SWS),
    DocReplyDict = append_update_replies(Docs, Replies, DocReplyDict0),
    Acc1 = Acc0#acc{conflicts = Conflicts},
    case {WaitingCount, dict:size(DocReplyDict)} of
        {1, _} ->
            % last message has arrived, we need to conclude things
            {Health, W, _, Reply} = dict:fold(
                fun force_reply/3,
                {ok, W, SWS, []},
                DocReplyDict
            ),
            start_remaining_workers(Acc1#acc{grouped_docs = NewGrpDocs}),
            {stop, {Health, Reply}};
        {_, DocCount} ->
            % we've got at least one reply for each document, let's take a look
            case dict:fold(fun maybe_reply/3, {stop, W, SWS, []}, DocReplyDict) of
                continue ->
                    {ok,
                        start_workers(Acc1#acc{
                            waiting_count = WaitingCount - 1,
                            grouped_docs = NewGrpDocs,
                            reply = DocReplyDict
                        })};
                {stop, W, _, FinalReplies} ->
                    start_remaining_workers(Acc1#acc{grouped_docs = NewGrpDocs}),
                    {stop, {ok, FinalReplies}}
            end;
        _ ->
            {ok,
                start_workers(Acc1#acc{
                    waiting_count = WaitingCount - 1,
                    grouped_docs = NewGrpDocs,
                    reply = DocReplyDict
                })}
    end;
handle_message({missing_stub, Stub}, _, _) ->
    throw({missing_stub, Stub});
handle_message({not_found, no_db_file} = X, Worker, #acc{} = Acc0) ->
    #acc{grouped_docs = GroupedDocs} = Acc0,
    Docs = couch_util:get_value(Worker, GroupedDocs),
    handle_message({ok, [X || _D <- Docs]}, Worker, Acc0);
handle_message({bad_request, Msg}, _, _) ->
    throw({bad_request, Msg});
handle_message({bad_request, Error, Reason}, _, _) ->
    throw({bad_request, Error, Reason});
handle_message({forbidden, Msg}, _, _) ->
    throw({forbidden, Msg});
handle_message({request_entity_too_large, Entity}, _, _) ->
    throw({request_entity_too_large, Entity}).

before_doc_update(DbName, Docs, Opts) ->
    % Use the same pattern as in couch_db:validate_doc_update/3. If the document was already
    % checked during the interactive edit we don't want to spend time in the internal replicator
    % revalidating everything.
    UpdateType =
        case get(io_priority) of
            {internal_repl, _} ->
                ?REPLICATED_CHANGES;
            _ ->
                ?INTERACTIVE_EDIT
        end,
    case {fabric_util:is_replicator_db(DbName), fabric_util:is_users_db(DbName)} of
        {true, _} ->
            %% cluster db is expensive to create so we only do it if we have to
            Db = fabric_util:open_cluster_db(DbName, Opts),
            [
                couch_replicator_docs:before_doc_update(Doc, Db, UpdateType)
             || Doc <- Docs
            ];
        {_, true} ->
            %% cluster db is expensive to create so we only do it if we have to
            Db = fabric_util:open_cluster_db(DbName, Opts),
            [
                couch_users_db:before_doc_update(Doc, Db, UpdateType)
             || Doc <- Docs
            ];
        _ ->
            Docs
    end.

tag_docs([]) ->
    [];
tag_docs([#doc{meta = Meta} = Doc | Rest]) ->
    [Doc#doc{meta = [{ref, make_ref()} | Meta]} | tag_docs(Rest)].

untag_docs([]) ->
    [];
untag_docs([#doc{} = Doc | Rest]) ->
    [untag_doc(Doc) | untag_docs(Rest)].

untag_doc(#doc{} = Doc) ->
    Doc#doc{meta = lists:keydelete(ref, 1, Doc#doc.meta)}.

force_reply(Doc, [], {_, W, SWS, Acc}) ->
    {error, W, SWS, [{Doc, {error, internal_server_error}} | Acc]};
force_reply(Doc, [FirstReply | _] = Replies, {Health, W, SWS, Acc}) ->
    case update_quorum_met(W, Replies, SWS) of
        {true, Reply} ->
            % corner case new_edits:false and vdu: [noreply, forbidden, noreply]
            case check_forbidden_msg(Replies) of
                {forbidden, ForbiddenReply} ->
                    {Health, W, SWS, [{Doc, ForbiddenReply} | Acc]};
                false ->
                    {Health, W, SWS, [{Doc, Reply} | Acc]}
            end;
        false ->
            case [Reply || {ok, Reply} <- Replies] of
                [] ->
                    % check if all errors are identical, if so inherit health
                    case lists:all(fun(E) -> E =:= FirstReply end, Replies) of
                        true ->
                            CounterKey = [fabric, doc_update, errors],
                            couch_stats:increment_counter(CounterKey),
                            {Health, W, SWS, [{Doc, FirstReply} | Acc]};
                        false ->
                            CounterKey = [fabric, doc_update, mismatched_errors],
                            couch_stats:increment_counter(CounterKey),
                            case check_forbidden_msg(Replies) of
                                {forbidden, ForbiddenReply} ->
                                    {Health, W, SWS, [{Doc, ForbiddenReply} | Acc]};
                                false ->
                                    {error, W, SWS, [{Doc, FirstReply} | Acc]}
                            end
                    end;
                [AcceptedRev | _] ->
                    CounterKey = [fabric, doc_update, write_quorum_errors],
                    couch_stats:increment_counter(CounterKey),
                    NewHealth =
                        case Health of
                            ok -> accepted;
                            _ -> Health
                        end,
                    {NewHealth, W, SWS, [{Doc, {accepted, AcceptedRev}} | Acc]}
            end
    end.

maybe_reply(_, _, continue) ->
    % we didn't meet quorum for all docs, so we're fast-forwarding the fold
    continue;
maybe_reply(Doc, Replies, {stop, W, SWS, Acc}) ->
    case update_quorum_met(W, Replies, SWS) of
        {true, Reply} ->
            {stop, W, SWS, [{Doc, Reply} | Acc]};
        false ->
            continue
    end.

% this ensures that we got some response for all documents being updated
ensure_all_responses(Health, AllDocs, Resp) ->
    Results = [
        R
     || R <- couch_util:reorder_results(
            AllDocs,
            Resp,
            {error, internal_server_error}
        ),
        R =/= noreply
    ],
    case lists:member({error, internal_server_error}, Results) of
        true ->
            {error, Results};
        false ->
            {Health, Results}
    end.

% This is a corner case where
% 1) revision tree for the document are out of sync across nodes
% 2) update on one node extends the revision tree
% 3) VDU forbids the document
% 4) remaining nodes do not extend revision tree, so noreply is returned
% If at at least one node forbids the update, and all other replies
% are noreply, then we reject the update
check_forbidden_msg(Replies) ->
    Pred = fun
        ({_, {forbidden, _}}) ->
            true;
        (_) ->
            false
    end,
    case lists:partition(Pred, Replies) of
        {[], _} ->
            false;
        {[ForbiddenReply = {_, {forbidden, _}} | _], RemReplies} ->
            case lists:all(fun(E) -> E =:= noreply end, RemReplies) of
                true ->
                    {forbidden, ForbiddenReply};
                false ->
                    false
            end
    end.

update_quorum_met(W, Replies, SWS) ->
    Counters = lists:foldl(
        fun(R, D) -> orddict:update_counter(R, 1, D) end,
        orddict:new(),
        Replies
    ),
    GoodReplies = lists:filter(fun(C) -> good_reply(C, SWS) end, Counters),
    case lists:dropwhile(quorum_pred(W, SWS), GoodReplies) of
        [] ->
            false;
        [{FinalReply, _} | _] ->
            {true, FinalReply}
    end.

% With a conflict in sws we stop the quorum early
quorum_pred(W, SWS) when is_boolean(SWS) ->
    fun
        ({conflict, _}) when SWS -> false;
        ({_, Count}) -> Count < W
    end.

good_reply({{ok, _}, _}, _) ->
    true;
good_reply({noreply, _}, _) ->
    true;
good_reply({conflict, _}, SWS) ->
    SWS;
good_reply(_, _) ->
    false.

-spec group_docs_by_shard(binary(), [#doc{}]) -> [{#shard{}, [#doc{}]}].
group_docs_by_shard(DbName, Docs) ->
    dict:to_list(
        lists:foldl(
            fun(#doc{id = Id} = Doc, D0) ->
                lists:foldl(
                    fun(Shard, D1) ->
                        dict:append(Shard, Doc, D1)
                    end,
                    D0,
                    begin
                        case mem3:shards(DbName, Id) of
                            [] ->
                                [];
                            [#shard{range = Range} | _] = Shards ->
                                mem3_util:rotate_list({Range, DbName}, Shards)
                        end
                    end
                )
            end,
            dict:new(),
            Docs
        )
    ).

append_update_replies([], [], DocReplyDict) ->
    DocReplyDict;
append_update_replies([Doc | Rest], [], Dict0) ->
    % icky, if replicated_changes only errors show up in result
    append_update_replies(Rest, [], dict:append(Doc, noreply, Dict0));
append_update_replies([Doc | Rest1], [Reply | Rest2], Dict0) ->
    append_update_replies(Rest1, Rest2, dict:append(Doc, Reply, Dict0)).

skip_message(#acc{waiting_count = 0} = Acc) ->
    #acc{w = W, reply = DocReplyDict, serialize_worker_startup = SWS} = Acc,
    {Health, W, _, Reply} = dict:fold(fun force_reply/3, {ok, W, SWS, []}, DocReplyDict),
    {stop, {Health, Reply}};
skip_message(#acc{} = Acc0) ->
    {ok, Acc0}.

validate_atomic_update(_, _, false) ->
    ok;
validate_atomic_update(_DbName, AllDocs, true) ->
    % TODO actually perform the validation.  This requires some hackery, we need
    % to basically extract the prep_and_validate_updates function from couch_db
    % and only run that, without actually writing in case of a success.
    Error = {not_implemented, <<"all_or_nothing is not supported">>},
    PreCommitFailures = lists:map(
        fun(#doc{id = Id, revs = {Pos, Revs}}) ->
            case Revs of
                [] -> RevId = <<>>;
                [RevId | _] -> ok
            end,
            {{Id, {Pos, RevId}}, Error}
        end,
        AllDocs
    ),
    throw({aborted, PreCommitFailures}).

% replicated changes are always in parallel
serialize_worker_startup(Options) ->
    case proplists:get_value(?REPLICATED_CHANGES, Options) of
        true -> false;
        _ -> config:get_boolean("fabric", "serialize_worker_startup", true)
    end.

start_workers_strategy(#acc{serialize_worker_startup = true} = Acc) ->
    start_workers(Acc);
start_workers_strategy(#acc{serialize_worker_startup = false} = Acc) ->
    start_remaining_workers(Acc).

% Start one worker per range per invocation of this function
start_workers(#acc{} = Acc) ->
    Ranges = lists:usort([W#shard.range || {W, _} <- Acc#acc.grouped_docs]),
    lists:foldl(
        fun(Range, AccIn) ->
            [{Worker, Docs} | _] = [
                {W, D}
             || {W, D} <- Acc#acc.grouped_docs, W#shard.range == Range
            ],
            start_worker(Worker, Docs, AccIn)
        end,
        Acc,
        Ranges
    ).

start_remaining_workers(#acc{} = Acc) ->
    lists:foldl(
        fun({Worker, Docs}, AccIn) ->
            start_worker(Worker, Docs, AccIn)
        end,
        Acc,
        Acc#acc.grouped_docs
    ).

start_worker(#shard{ref = Ref} = Worker, Docs0, #acc{} = Acc) when is_reference(Ref) ->
    #shard{name = Name, node = Node} = Worker,
    #acc{update_options = UpdateOptions, conflicts = Conflicts} = Acc,
    case lists:member(Ref, Acc#acc.started) of
        true ->
            Acc;
        false ->
            % If a doc is a settled conflict for a range, skip it
            Docs = filter_conflicts(Docs0, Conflicts),
            Ref = rexi:cast_ref(
                Ref, Node, {fabric_rpc, update_docs, [Name, untag_docs(Docs), UpdateOptions]}
            ),
            % We need to save what we just cast so we can match up results exactly
            NewGrouped = lists:keystore(Worker, 1, Acc#acc.grouped_docs, {Worker, Docs}),
            Acc#acc{
                started = [Ref | Acc#acc.started],
                grouped_docs = NewGrouped
            }
    end;
start_worker(#shard{}, _Docs, #acc{} = Acc) ->
    %% for unit tests below.
    Acc.

% With sws=false conflicts act like normal values and conflicts list stays
% empty (filter_conflicts/2 is a no-op)
collect_conflicts(_Docs, _Replies, Conflicts, false) ->
    Conflicts;
collect_conflicts(Docs, Replies, Conflicts, true) when length(Docs) == length(Replies) ->
    [untag_doc(D) || {D, conflict} <- lists:zip(Docs, Replies)] ++ Conflicts;
collect_conflicts(_Docs, _Replies, Conflicts, true) ->
    % Replicated changes return no replies by default
    Conflicts.

filter_conflicts(Docs, []) ->
    Docs;
filter_conflicts(Docs, Conflicts) ->
    [D || D <- Docs, not lists:member(untag_doc(D), Conflicts)].

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

setup_all() ->
    meck:new([couch_log, couch_stats]),
    meck:expect(couch_log, warning, fun(_, _) -> ok end),
    meck:expect(couch_stats, increment_counter, fun(_) -> ok end),
    meck:new(rexi, [passthrough]),
    meck:expect(rexi, cast_ref, fun(Ref, _Node, _Msg) -> Ref end).

teardown_all(_) ->
    meck:unload().

doc_update_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        [
            fun doc_update1/0,
            fun doc_update2/0,
            fun doc_update3/0,
            fun early_termination_on_conflict_1/0,
            fun early_termination_on_conflict_2/0,
            fun handle_all_dbs_active/0,
            fun handle_two_all_dbs_actives/0,
            fun one_forbid/0,
            fun two_forbid/0,
            fun extend_tree_forbid/0,
            fun other_errors_one_forbid/0,
            fun one_error_two_forbid/0,
            fun one_success_two_forbid/0,
            fun worker_before_doc_update_forbidden/0,
            fun handle_bad_request/0,
            fun filter_conflicts_drops_seen_docs/0,
            fun parallel_in_flight_after_conflict/0,
            fun serial_filters_conflicts_at_cast/0,
            fun sws_false_mode_conflict_not_final/0,
            fun sws_false_mode_ok_can_outvote_conflict/0
        ]
    }.

% eunits
doc_update1() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1],
    Docs2 = [Doc1, Doc2],
    Dict = dict:from_list([{Doc, []} || Doc <- Docs]),
    Dict2 = dict:from_list([{Doc, []} || Doc <- Docs2]),

    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),

    % test for W = 2
    AccW2 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = Dict
    },

    {ok, #acc{waiting_count = WaitingCountW2_1} = AccW2_1} =
        handle_message({ok, [{ok, Doc1}]}, hd(Shards), AccW2),
    ?assertEqual(WaitingCountW2_1, 2),
    {stop, FinalReplyW2} =
        handle_message({ok, [{ok, Doc1}]}, lists:nth(2, Shards), AccW2_1),
    ?assertEqual({ok, [{Doc1, {ok, Doc1}}]}, FinalReplyW2),

    % test for W = 3
    AccW3 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 3,
        grouped_docs = GroupedDocs,
        reply = Dict
    },

    {ok, #acc{waiting_count = WaitingCountW3_1} = AccW3_1} =
        handle_message({ok, [{ok, Doc1}]}, hd(Shards), AccW3),
    ?assertEqual(WaitingCountW3_1, 2),

    {ok, #acc{waiting_count = WaitingCountW3_2} = AccW3_2} =
        handle_message({ok, [{ok, Doc1}]}, lists:nth(2, Shards), AccW3_1),
    ?assertEqual(WaitingCountW3_2, 1),

    {stop, FinalReplyW3} =
        handle_message({ok, [{ok, Doc1}]}, lists:nth(3, Shards), AccW3_2),
    ?assertEqual({ok, [{Doc1, {ok, Doc1}}]}, FinalReplyW3),

    % test w quorum > # shards, which should fail immediately

    Shards2 = mem3_util:create_partition_map("foo", 1, 1, ["node1"]),
    GroupedDocs2 = group_docs_by_shard_hack(<<"foo">>, Shards2, Docs),

    AccW4 =
        #acc{
            waiting_count = length(Shards2),
            doc_count = length(Docs),
            w = 2,
            grouped_docs = GroupedDocs2,
            reply = Dict
        },
    Bool =
        case handle_message({ok, [{ok, Doc1}]}, hd(Shards2), AccW4) of
            {stop, _Reply} ->
                true;
            _ ->
                false
        end,
    ?assertEqual(Bool, true),

    % Docs with no replies should end up as {error, internal_server_error}
    SA1 = #shard{node = a, range = 1},
    SB1 = #shard{node = b, range = 1},
    SA2 = #shard{node = a, range = 2},
    SB2 = #shard{node = b, range = 2},
    GroupedDocs3 = [{SA1, [Doc1]}, {SB1, [Doc1]}, {SA2, [Doc2]}, {SB2, [Doc2]}],
    StW5_0 = #acc{
        waiting_count = length(GroupedDocs3),
        doc_count = length(Docs2),
        w = 2,
        grouped_docs = GroupedDocs3,
        reply = Dict2
    },
    {ok, StW5_1} = handle_message({ok, [{ok, "A"}]}, SA1, StW5_0),
    {ok, StW5_2} = handle_message({rexi_EXIT, nil}, SB1, StW5_1),
    {ok, StW5_3} = handle_message({rexi_EXIT, nil}, SA2, StW5_2),
    {stop, ReplyW5} = handle_message({rexi_EXIT, nil}, SB2, StW5_3),
    ?assertEqual(
        {error, [{Doc1, {accepted, "A"}}, {Doc2, {error, internal_server_error}}]},
        ReplyW5
    ).

doc_update2() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message({rexi_EXIT, 1}, lists:nth(2, Shards), Acc1),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message({rexi_EXIT, 1}, lists:nth(3, Shards), Acc2),

    ?assertEqual(
        {accepted, [{Doc1, {accepted, Doc1}}, {Doc2, {accepted, Doc2}}]},
        Reply
    ).

doc_update3() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message({rexi_EXIT, 1}, lists:nth(2, Shards), Acc1),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, lists:nth(3, Shards), Acc2),

    ?assertEqual({ok, [{Doc1, {ok, Doc1}}, {Doc2, {ok, Doc2}}]}, Reply).

early_termination_on_conflict_1() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    [S1 | _] =
        Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },
    ?assertEqual(
        {stop, {ok, [{Doc1, conflict}, {Doc2, conflict}]}},
        handle_message({ok, [conflict, conflict]}, S1, Acc0)
    ).

early_termination_on_conflict_2() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    [S1, S2 | _] =
        Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 3,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },
    {ok, #acc{} = Acc1} =
        handle_message({ok, [internal_server_error, internal_server_error]}, S1, Acc0),
    ?assertEqual(
        {stop, {ok, [{Doc1, conflict}, {Doc2, conflict}]}},
        handle_message({ok, [conflict, conflict]}, S2, Acc1)
    ).

handle_all_dbs_active() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message({error, all_dbs_active}, lists:nth(2, Shards), Acc1),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, lists:nth(3, Shards), Acc2),

    ?assertEqual({ok, [{Doc1, {ok, Doc1}}, {Doc2, {ok, Doc2}}]}, Reply).

handle_two_all_dbs_actives() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message({error, all_dbs_active}, lists:nth(2, Shards), Acc1),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message({error, all_dbs_active}, lists:nth(3, Shards), Acc2),

    ?assertEqual(
        {accepted, [{Doc1, {accepted, Doc1}}, {Doc2, {accepted, Doc2}}]},
        Reply
    ).

one_forbid() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),

    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, noreply]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, lists:nth(2, Shards), Acc1
        ),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message({ok, [{ok, Doc1}, noreply]}, lists:nth(3, Shards), Acc2),

    ?assertEqual(
        {ok, [
            {Doc1, {ok, Doc1}},
            {Doc2, {Doc2, {forbidden, <<"not allowed">>}}}
        ]},
        Reply
    ).

two_forbid() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),

    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, noreply]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, lists:nth(2, Shards), Acc1
        ),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, lists:nth(3, Shards), Acc2
        ),

    ?assertEqual(
        {ok, [
            {Doc1, {ok, Doc1}},
            {Doc2, {Doc2, {forbidden, <<"not allowed">>}}}
        ]},
        Reply
    ).

% This should actually never happen, because an `{ok, Doc}` message means that the revision
% tree is extended and so the VDU should forbid the document.
% Leaving this test here to make sure quorum rules still apply.
extend_tree_forbid() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),

    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, lists:nth(2, Shards), Acc1
        ),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, lists:nth(3, Shards), Acc2),

    ?assertEqual({ok, [{Doc1, {ok, Doc1}}, {Doc2, {ok, Doc2}}]}, Reply).

other_errors_one_forbid() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),

    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message({ok, [{ok, Doc1}, {Doc2, {error, <<"foo">>}}]}, hd(Shards), Acc0),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message({ok, [{ok, Doc1}, {Doc2, {error, <<"bar">>}}]}, lists:nth(2, Shards), Acc1),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, lists:nth(3, Shards), Acc2
        ),
    ?assertEqual({error, [{Doc1, {ok, Doc1}}, {Doc2, {Doc2, {error, <<"foo">>}}}]}, Reply).

one_error_two_forbid() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),

    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, hd(Shards), Acc0
        ),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message({ok, [{ok, Doc1}, {Doc2, {error, <<"foo">>}}]}, lists:nth(2, Shards), Acc1),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, lists:nth(3, Shards), Acc2
        ),
    ?assertEqual(
        {error, [{Doc1, {ok, Doc1}}, {Doc2, {Doc2, {forbidden, <<"not allowed">>}}}]}, Reply
    ).

one_success_two_forbid() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),

    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },

    {ok, #acc{waiting_count = WaitingCount1} = Acc1} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, hd(Shards), Acc0
        ),
    ?assertEqual(WaitingCount1, 2),

    {ok, #acc{waiting_count = WaitingCount2} = Acc2} =
        handle_message({ok, [{ok, Doc1}, {Doc2, {ok, Doc2}}]}, lists:nth(2, Shards), Acc1),
    ?assertEqual(WaitingCount2, 1),

    {stop, Reply} =
        handle_message(
            {ok, [{ok, Doc1}, {Doc2, {forbidden, <<"not allowed">>}}]}, lists:nth(3, Shards), Acc2
        ),
    ?assertEqual(
        {error, [{Doc1, {ok, Doc1}}, {Doc2, {Doc2, {forbidden, <<"not allowed">>}}}]}, Reply
    ).

worker_before_doc_update_forbidden() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Docs = [Doc1],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },
    ?assertThrow({forbidden, <<"msg">>}, handle_message({forbidden, <<"msg">>}, hd(Shards), Acc)).

handle_bad_request() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Docs = [Doc1],
    Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{Doc, []} || Doc <- Docs])
    },
    ?assertThrow(
        {bad_request, msg},
        handle_message({bad_request, msg}, hd(Shards), Acc)
    ),
    ?assertThrow(
        {bad_request, err, reason},
        handle_message({bad_request, err, reason}, hd(Shards), Acc)
    ).

filter_conflicts_drops_seen_docs() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    [Tagged1, Tagged2] = tag_docs([Doc1, Doc2]),
    ?assertEqual([Doc1, Doc2], filter_conflicts([Doc1, Doc2], [])),
    ?assertEqual([Doc2], filter_conflicts([Doc1, Doc2], [Doc1])),
    ?assertEqual([], filter_conflicts([Doc1, Doc2], [Doc1, Doc2])),
    % Match against untagged
    ?assertEqual([Tagged2], filter_conflicts([Tagged1, Tagged2], [Doc1])).

% In flight parallel mode, conflict reply from one worker shouldn't touch
% another in-flight workers's grouped_docs entry, otherwise we wouldn't be able
% to match replies exactly
parallel_in_flight_after_conflict() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    [S1, S2 | _] =
        Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{D, []} || D <- Docs])
    },

    % S1 returns Doc1=conflict / Doc2=ok. Doc1 should be added to conflicts. S2
    % entry should be left alone
    {ok, #acc{conflicts = [Doc1]} = Acc1} =
        handle_message({ok, [conflict, {ok, Doc2}]}, S1, Acc0),
    {S2, S2Docs} = lists:keyfind(S2, 1, Acc1#acc.grouped_docs),
    ?assertEqual([Doc1, Doc2], S2Docs),

    %% S2 reply comes back, and we want lengths to match exactly
    {stop, {ok, Reply}} =
        handle_message({ok, [conflict, {ok, Doc2}]}, S2, Acc1),
    ?assertEqual(
        lists:sort([{Doc1, conflict}, {Doc2, {ok, Doc2}}]),
        lists:sort(Reply)
    ).

% In serial mode, a worker started after a peer's conflict response must be
% cast with conflicted doc filtred out and its grouped_docs updated to that
% filtered list. We want cast and reply pairs to always stay in sync
serial_filters_conflicts_at_cast() ->
    Doc1 = #doc{id = <<"a">>, revs = {1, [<<"foo">>]}},
    Doc2 = #doc{id = <<"b">>, revs = {1, [<<"bar">>]}},
    [Tagged1, Tagged2] = tag_docs([Doc1, Doc2]),
    Ref = make_ref(),
    Worker = #shard{
        node = node1,
        name = <<"db/shard">>,
        ref = Ref,
        range = [0, 100]
    },
    Acc0 = #acc{
        conflicts = [Doc1],
        grouped_docs = [{Worker, [Tagged1, Tagged2]}],
        update_options = []
    },

    Self = self(),
    meck:expect(rexi, cast_ref, fun(R, _Node, Msg) ->
        Self ! {cast, R, Msg},
        R
    end),
    Acc1 = start_worker(Worker, [Tagged1, Tagged2], Acc0),
    receive
        {cast, Ref, {fabric_rpc, update_docs, [<<"db/shard">>, CastDocs, []]}} ->
            ?assertEqual([Doc2], CastDocs)
    end,

    ?assert(lists:member(Ref, Acc1#acc.started)),
    {Worker, Stored} = lists:keyfind(Worker, 1, Acc1#acc.grouped_docs),
    ?assertEqual([Tagged2], Stored).

sws_false_mode_conflict_not_final() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    [S1, S2 | _] =
        Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{D, []} || D <- Docs]),
        serialize_worker_startup = false
    },

    % S1 conflict on Doc1, ok on Doc2. Conflicts list should be []
    % Doc1 has no good replies and Doc2 has an ok (1 < W=2) and we
    % should be continuing (not stopping).
    {ok, #acc{conflicts = [], waiting_count = 2} = Acc1} =
        handle_message({ok, [conflict, {ok, Doc2}]}, S1, Acc0),

    % S2 reports the same. Doc1 still has no good replies, quorum
    % not met, so we keep waiting
    {ok, _Acc2} = handle_message({ok, [conflict, {ok, Doc2}]}, S2, Acc1).

sws_false_mode_ok_can_outvote_conflict() ->
    Doc1 = #doc{revs = {1, [<<"foo">>]}},
    Doc2 = #doc{revs = {1, [<<"bar">>]}},
    Docs = [Doc1, Doc2],
    [S1, S2, S3] =
        Shards =
        mem3_util:create_partition_map("foo", 3, 1, ["node1", "node2", "node3"]),
    GroupedDocs = group_docs_by_shard_hack(<<"foo">>, Shards, Docs),
    Acc0 = #acc{
        waiting_count = length(Shards),
        doc_count = length(Docs),
        w = 2,
        grouped_docs = GroupedDocs,
        reply = dict:from_list([{D, []} || D <- Docs]),
        serialize_worker_startup = false
    },
    % With enough peers returning ok for both docs, resolve doc1 as ok
    % even if one reported aconflict.
    {ok, Acc1} = handle_message({ok, [conflict, {ok, Doc2}]}, S1, Acc0),
    {ok, Acc2} = handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, S2, Acc1),
    {stop, {ok, Reply}} = handle_message({ok, [{ok, Doc1}, {ok, Doc2}]}, S3, Acc2),
    ?assertEqual(
        lists:sort([{Doc1, {ok, Doc1}}, {Doc2, {ok, Doc2}}]),
        lists:sort(Reply)
    ).

% needed for testing to avoid having to start the mem3 application
group_docs_by_shard_hack(_DbName, Shards, Docs) ->
    dict:to_list(
        lists:foldl(
            fun(#doc{id = _Id} = Doc, D0) ->
                lists:foldl(
                    fun(Shard, D1) ->
                        dict:append(Shard, Doc, D1)
                    end,
                    D0,
                    Shards
                )
            end,
            dict:new(),
            Docs
        )
    ).

-endif.
