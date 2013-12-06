% Copyright 2013 Cloudant
%
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

-module(mem3_rpc).


-export([
    get_missing_revs/4,
    update_docs/4,
    load_checkpoint/4,
    save_checkpoint/6
]).

% Private RPC callbacks
-export([
    load_checkpoint_rpc/3,
    save_checkpoint_rpc/5
]).


-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(CTX, #user_ctx{roles = [<<"_admin">>]}).


get_missing_revs(Node, DbName, IdsRevs, Options) ->
    rexi_call(Node, {fabric_rpc, get_missing_revs, [DbName, IdsRevs, Options]}).


update_docs(Node, DbName, Docs, Options) ->
    rexi_call(Node, {fabric_rpc, update_docs, [DbName, Docs, Options]}).


load_checkpoint(Node, DbName, SourceNode, SourceUUID) ->
    Args = [DbName, SourceNode, SourceUUID],
    rexi_call(Node, {mem3_rpc, load_checkpoint_rpc, Args}).


save_checkpoint(Node, DbName, DocId, Seq, Entry, History) ->
    Args = [DbName, DocId, Seq, Entry, History],
    rexi_call(Node, {mem3_rpc, save_checkpoint_rpc, Args}).


load_checkpoint_rpc(DbName, SourceNode, SourceUUID) ->
    erlang:put(io_priority, {internal_repl, DbName}),
    case couch_db:open_int(DbName, [{user_ctx, ?CTX}]) of
    {ok, Db} ->
        TargetUUID = couch_db:get_uuid(Db),
        NewId = mem3_rep:make_local_id(SourceUUID, TargetUUID),
        case couch_db:open_doc(Db, NewId, []) of
        {ok, Doc} ->
            rexi:reply({ok, {NewId, Doc}});
        {not_found, _} ->
            OldId = mem3_rep:make_local_id(SourceNode, node()),
            case couch_db:open_doc(Db, OldId, []) of
            {ok, Doc} ->
                rexi:reply({ok, {NewId, Doc}});
            {not_found, _} ->
                rexi:reply({ok, {NewId, #doc{id = NewId}}})
            end
        end;
    Error ->
        rexi:reply(Error)
    end.


save_checkpoint_rpc(DbName, Id, SourceSeq, NewEntry0, History0) ->
    erlang:put(io_priority, {internal_repl, DbName}),
    case couch_db:open_int(DbName, [{user_ctx, ?CTX}]) of
        {ok, #db{update_seq = TargetSeq} = Db} ->
            NewEntry = {[
                {<<"target_node">>, atom_to_binary(node(), utf8)},
                {<<"target_uuid">>, couch_db:get_uuid(Db)},
                {<<"target_seq">>, TargetSeq}
            ] ++ NewEntry0},
            Body = {[
                {<<"seq">>, SourceSeq},
                {<<"target_uuid">>, couch_db:get_uuid(Db)},
                {<<"history">>, add_checkpoint(NewEntry, History0)}
            ]},
            Doc = #doc{id = Id, body = Body},
            rexi:reply(try couch_db:update_doc(Db, Doc, []) of
                {ok, _} ->
                    {ok, Body};
                Else ->
                    {error, Else}
            catch
                Exception ->
                    Exception;
                error:Reason ->
                    {error, Reason}
            end);
        Error ->
            rexi:reply(Error)
    end.


%% @doc This adds a new update sequence checkpoint to the replication
%%      history. Checkpoints are keyed by the source node so that we
%%      aren't mixing history between source shard moves.
add_checkpoint({Props}, {History}) ->
    % Extract the source and target seqs for reference
    SourceSeq = couch_util:get_value(<<"source_seq">>, Props),
    TargetSeq = couch_util:get_value(<<"target_seq">>, Props),

    % Get the history relevant to the source node.
    SourceNode = couch_util:get_value(<<"source_node">>, Props),
    SourceHistory = couch_util:get_value(SourceNode, History, []),

    % If either the source or target shard has been truncated
    % we need to filter out any history that was stored for
    % any larger update seq than we're currently recording.
    FilteredHistory = filter_history(SourceSeq, TargetSeq, SourceHistory),

    % Insert the new entry into the history and trim the history
    % to keep an exponentially increasing delta between checkpoints.
    % We do this by defining logical buckets of exponentially
    % increasing size and then keep the smallest and largest values
    % in each bucket. We keep both min and max points so that
    % we don't end up with empty buckets as new points are added.
    %
    % NB: We're guaranteed to keep the newest entry passed to this
    % function because we filter out all larger update sequences
    % which means it is guaranteed to be the smallest value in the
    % first bucket with a delta of 0.
    WithNewEntry = [{Props} | FilteredHistory],

    % Tag each entry with the bucket id
    BucketTagged = lists:map(fun({Entry}) ->
        EntrySourceSeq = couch_util:get_value(<<"source_seq">>, Entry),
        BucketTag = case SourceSeq - EntrySourceSeq of
            0 ->
                0;
            N when N > 0 ->
                % This is int(log2(SourceSeq - EntrySourceSeq))
                trunc(math:log(N) / math:log(2))
        end,
        {BucketTag, SourceSeq - EntrySourceSeq, {Entry}}
    end, WithNewEntry),

    % Find the min/max entries for each bucket
    Buckets = lists:foldl(fun({Bucket, Delta, Entry}, BucketAcc) ->
        {MinEntry, MaxEntry} = case dict:find(Bucket, BucketAcc) of
            {ok, Value} -> Value;
            error -> {nil, nil}
        end,
        NewMin = case MinEntry of
            {MinDelta, _} when Delta < MinDelta ->
                {Delta, Entry};
            nil ->
                {Delta, Entry};
            _ ->
                MinEntry
        end,
        NewMax = case MaxEntry of
            {MaxDelta, _} when Delta > MaxDelta ->
                {Delta, Entry};
            nil ->
                {Delta, Entry};
            _ ->
                MaxEntry
        end,
        dict:store(Bucket, {NewMin, NewMax}, BucketAcc)
    end, dict:new(), BucketTagged),

    % Turn our bucket dict back into a list sorted by increasing
    % deltas (which corresponds to decreasing source_seq values).
    NewSourceHistory = lists:flatmap(fun({_Bucket, {Min, Max}}) ->
        % If there's a single point in a bucket its both the min
        % and max entry so we account for that here.
        if Min == Max ->
            [element(2, Min)];
        true ->
            [element(2, Min), element(2, Max)]
        end
    end, lists:sort(dict:to_list(Buckets))),

    % Finally update the source node history and we're done.
    NodeRemoved = lists:keydelete(SourceNode, 1, History),
    {[{SourceNode, NewSourceHistory} | NodeRemoved]}.


filter_history(SourceSeqThresh, TargetSeqThresh, History) ->
    SourceFilter = fun({Entry}) ->
        SourceSeq = couch_util:get_value(<<"source_seq">>, Entry),
        SourceSeq < SourceSeqThresh
    end,
    TargetFilter = fun({Entry}) ->
        TargetSeq = couch_util:get_value(<<"target_seq">>, Entry),
        TargetSeq < TargetSeqThresh
    end,
    SourceFiltered = lists:filter(SourceFilter, History),
    lists:filter(TargetFilter, SourceFiltered).


rexi_call(Node, MFA) ->
    Mon = rexi_monitor:start([rexi_utils:server_pid(Node)]),
    Ref = rexi:cast(Node, self(), MFA, [sync]),
    try
        receive {Ref, {ok, Reply}} ->
            Reply;
        {Ref, Error} ->
            erlang:error(Error);
        {rexi_DOWN, Mon, _, Reason} ->
            erlang:error({rexi_DOWN, {Node, Reason}})
        after 600000 ->
            erlang:error(timeout)
        end
    after
        rexi_monitor:stop(Mon)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-define(SNODE, <<"src@localhost">>).
-define(TNODE, <<"tgt@localhost">>).
-define(SNODE_KV, {<<"source_node">>, ?SNODE}).
-define(TNODE_KV, {<<"target_node">>, ?TNODE}).
-define(SSEQ, <<"source_seq">>).
-define(TSEQ, <<"target_seq">>).
-define(ENTRY(S, T), {[?SNODE_KV, {?SSEQ, S}, ?TNODE_KV, {?TSEQ, T}]}).


filter_history_data() ->
    [
        ?ENTRY(13, 15),
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ].


filter_history_remove_none_test() ->
    ?assertEqual(filter_history(20, 20, filter_history_data()), [
        ?ENTRY(13, 15),
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]).


filter_history_remove_all_test() ->
    ?assertEqual(filter_history(1, 1, filter_history_data()), []).


filter_history_remove_equal_test() ->
    ?assertEqual(filter_history(10, 10, filter_history_data()), [
        ?ENTRY(2, 3)
    ]),
    ?assertEqual(filter_history(11, 9, filter_history_data()), [
        ?ENTRY(2, 3)
    ]).


filter_history_remove_for_source_and_target_test() ->
    ?assertEqual(filter_history(11, 20, filter_history_data()), [
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]),
    ?assertEqual(filter_history(14, 14, filter_history_data()), [
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]).


filter_history_remove_for_both_test() ->
    ?assertEqual(filter_history(11, 11, filter_history_data()), [
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]).


filter_history_remove_for_both_again_test() ->
    ?assertEqual(filter_history(3, 4, filter_history_data()), [
        ?ENTRY(2, 3)
    ]).


add_first_checkpoint_test() ->
    History = {[]},
    ?assertEqual(add_checkpoint(?ENTRY(2, 3), History), {[
        {?SNODE, [
            ?ENTRY(2, 3)
        ]}
    ]}).


add_first_checkpoint_to_empty_test() ->
    History = {[{?SNODE, []}]},
    ?assertEqual(add_checkpoint(?ENTRY(2, 3), History), {[
        {?SNODE, [
            ?ENTRY(2, 3)
        ]}
    ]}).


add_second_checkpoint_test() ->
    History = {[{?SNODE, [?ENTRY(2, 3)]}]},
    ?assertEqual(add_checkpoint(?ENTRY(10, 9), History), {[
        {?SNODE, [
            ?ENTRY(10, 9),
            ?ENTRY(2, 3)
        ]}
    ]}).


add_third_checkpoint_test() ->
    History = {[{?SNODE, [
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]}]},
    ?assertEqual(add_checkpoint(?ENTRY(11, 10), History), {[
        {?SNODE, [
            ?ENTRY(11, 10),
            ?ENTRY(10, 9),
            ?ENTRY(2, 3)
        ]}
    ]}).


add_fourth_checkpoint_test() ->
    History = {[{?SNODE, [
        ?ENTRY(11, 10),
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]}]},
    ?assertEqual(add_checkpoint(?ENTRY(12, 13), History), {[
        {?SNODE, [
            ?ENTRY(12, 13),
            ?ENTRY(11, 10),
            ?ENTRY(10, 9),
            ?ENTRY(2, 3)
        ]}
    ]}).


add_checkpoint_with_replacement_test() ->
    History = {[{?SNODE, [
        ?ENTRY(12, 13),
        ?ENTRY(11, 10),
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]}]},
    % Picking a source_seq of 16 to force 10, 11, and 12
    % into the same bucket to show we drop the 11 entry.
    ?assertEqual(add_checkpoint(?ENTRY(16, 16), History), {[
        {?SNODE, [
            ?ENTRY(16, 16),
            ?ENTRY(12, 13),
            ?ENTRY(10, 9),
            ?ENTRY(2, 3)
        ]}
    ]}).

add_checkpoint_drops_redundant_checkpoints_test() ->
    % I've added comments showing the bucket ID based
    % on the ?ENTRY passed to add_checkpoint
    History = {[{?SNODE, [
        ?ENTRY(15, 15), % Bucket 0
        ?ENTRY(14, 14), % Bucket 1
        ?ENTRY(13, 13), % Bucket 1
        ?ENTRY(12, 12), % Bucket 2
        ?ENTRY(11, 11), % Bucket 2
        ?ENTRY(10, 10), % Bucket 2
        ?ENTRY(9, 9),   % Bucket 2
        ?ENTRY(8, 8),   % Bucket 3
        ?ENTRY(7, 7),   % Bucket 3
        ?ENTRY(6, 6),   % Bucket 3
        ?ENTRY(5, 5),   % Bucket 3
        ?ENTRY(4, 4),   % Bucket 3
        ?ENTRY(3, 3),   % Bucket 3
        ?ENTRY(2, 2),   % Bucket 3
        ?ENTRY(1, 1)    % Bucket 3
    ]}]},
    ?assertEqual(add_checkpoint(?ENTRY(16, 16), History), {[
        {?SNODE, [
            ?ENTRY(16, 16), % Bucket 0
            ?ENTRY(15, 15), % Bucket 0
            ?ENTRY(14, 14), % Bucket 1
            ?ENTRY(13, 13), % Bucket 1
            ?ENTRY(12, 12), % Bucket 2
            ?ENTRY(9, 9),   % Bucket 2
            ?ENTRY(8, 8),   % Bucket 3
            ?ENTRY(1, 1)    % Bucket 3
        ]}
    ]}).


add_checkpoint_show_not_always_a_drop_test() ->
    % Depending on the edge conditions of buckets we
    % may not always drop values when adding new
    % checkpoints. In this case 12 stays because there's
    % no longer a value for 10 or 11.
    %
    % I've added comments showing the bucket ID based
    % on the ?ENTRY passed to add_checkpoint
    History = {[{?SNODE, [
        ?ENTRY(16, 16), % Bucket 0
        ?ENTRY(15, 15), % Bucket 1
        ?ENTRY(14, 14), % Bucket 1
        ?ENTRY(13, 13), % Bucket 2
        ?ENTRY(12, 12), % Bucket 2
        ?ENTRY(9, 9),   % Bucket 3
        ?ENTRY(8, 8),   % Bucket 3
        ?ENTRY(1, 1)    % Bucket 4
    ]}]},
    ?assertEqual(add_checkpoint(?ENTRY(17, 17), History), {[
        {?SNODE, [
            ?ENTRY(17, 17), % Bucket 0
            ?ENTRY(16, 16), % Bucket 0
            ?ENTRY(15, 15), % Bucket 1
            ?ENTRY(14, 14), % Bucket 1
            ?ENTRY(13, 13), % Bucket 2
            ?ENTRY(12, 12), % Bucket 2
            ?ENTRY(9, 9),   % Bucket 3
            ?ENTRY(8, 8),   % Bucket 3
            ?ENTRY(1, 1)    % Bucket 4
        ]}
    ]}).


add_checkpoint_big_jump_show_lots_drop_test() ->
    % I've added comments showing the bucket ID based
    % on the ?ENTRY passed to add_checkpoint
    History = {[{?SNODE, [
        ?ENTRY(16, 16), % Bucket 4
        ?ENTRY(15, 15), % Bucket 4
        ?ENTRY(14, 14), % Bucket 4
        ?ENTRY(13, 13), % Bucket 4
        ?ENTRY(12, 12), % Bucket 4
        ?ENTRY(9, 9),   % Bucket 4
        ?ENTRY(8, 8),   % Bucket 4
        ?ENTRY(1, 1)    % Bucket 4
    ]}]},
    ?assertEqual(add_checkpoint(?ENTRY(32, 32), History), {[
        {?SNODE, [
            ?ENTRY(32, 32), % Bucket 0
            ?ENTRY(16, 16), % Bucket 4
            ?ENTRY(1, 1)    % Bucket 4
        ]}
    ]}).


add_checkpoint_show_filter_history_test() ->
    History = {[{?SNODE, [
        ?ENTRY(16, 16),
        ?ENTRY(15, 15),
        ?ENTRY(14, 14),
        ?ENTRY(13, 13),
        ?ENTRY(12, 12),
        ?ENTRY(9, 9),
        ?ENTRY(8, 8),
        ?ENTRY(1, 1)
    ]}]},
    % Drop for both
    ?assertEqual(add_checkpoint(?ENTRY(10, 10), History), {[
        {?SNODE, [
            ?ENTRY(10, 10),
            ?ENTRY(9, 9),
            ?ENTRY(8, 8),
            ?ENTRY(1, 1)
        ]}
    ]}),
    % Drop four source
    ?assertEqual(add_checkpoint(?ENTRY(10, 200), History), {[
        {?SNODE, [
            ?ENTRY(10, 200),
            ?ENTRY(9, 9),
            ?ENTRY(8, 8),
            ?ENTRY(1, 1)
        ]}
    ]}),
    % Drop for target. Obviously a source_seq of 200
    % will end up droping the 8 entry.
    ?assertEqual(add_checkpoint(?ENTRY(200, 10), History), {[
        {?SNODE, [
            ?ENTRY(200, 10),
            ?ENTRY(9, 9),
            ?ENTRY(1, 1)
        ]}
    ]}).


add_checkpoint_from_other_node_test() ->
    History = {[{<<"not_the_source">>, [
        ?ENTRY(12, 13),
        ?ENTRY(11, 10),
        ?ENTRY(10, 9),
        ?ENTRY(2, 3)
    ]}]},
    % No filtering
    ?assertEqual(add_checkpoint(?ENTRY(1, 1), History), {[
        {?SNODE, [
            ?ENTRY(1, 1)
        ]},
        {<<"not_the_source">>, [
            ?ENTRY(12, 13),
            ?ENTRY(11, 10),
            ?ENTRY(10, 9),
            ?ENTRY(2, 3)
        ]}
    ]}),
    % No dropping
    ?assertEqual(add_checkpoint(?ENTRY(200, 200), History), {[
        {?SNODE, [
            ?ENTRY(200, 200)
        ]},
        {<<"not_the_source">>, [
            ?ENTRY(12, 13),
            ?ENTRY(11, 10),
            ?ENTRY(10, 9),
            ?ENTRY(2, 3)
        ]}
    ]}).


-endif.
