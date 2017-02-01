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
    find_common_seq/4,
    get_missing_revs/4,
    update_docs/4,
    load_checkpoint/4,
    save_checkpoint/6
]).

% Private RPC callbacks
-export([
    find_common_seq_rpc/3,
    load_checkpoint_rpc/3,
    save_checkpoint_rpc/5
]).


-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").


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


find_common_seq(Node, DbName, SourceUUID, SourceEpochs) ->
    Args = [DbName, SourceUUID, SourceEpochs],
    rexi_call(Node, {mem3_rpc, find_common_seq_rpc, Args}).


load_checkpoint_rpc(DbName, SourceNode, SourceUUID) ->
    erlang:put(io_priority, {internal_repl, DbName}),
    case get_or_create_db(DbName, [?ADMIN_CTX]) of
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
    case get_or_create_db(DbName, [?ADMIN_CTX]) of
        {ok, Db} ->
            NewEntry = {[
                {<<"target_node">>, atom_to_binary(node(), utf8)},
                {<<"target_uuid">>, couch_db:get_uuid(Db)},
                {<<"target_seq">>, couch_db:get_update_seq(Db)}
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

find_common_seq_rpc(DbName, SourceUUID, SourceEpochs) ->
    erlang:put(io_priority, {internal_repl, DbName}),
    case get_or_create_db(DbName, [?ADMIN_CTX]) of
    {ok, Db} ->
        case couch_db:get_uuid(Db) of
        SourceUUID ->
            TargetEpochs = couch_db:get_epochs(Db),
            Seq = compare_epochs(SourceEpochs, TargetEpochs),
            rexi:reply({ok, Seq});
        _Else ->
            rexi:reply({ok, 0})
        end;
    Error ->
        rexi:reply(Error)
    end.


%% @doc Return the sequence where two files with the same UUID diverged.
compare_epochs(SourceEpochs, TargetEpochs) ->
    compare_rev_epochs(
        lists:reverse(SourceEpochs),
        lists:reverse(TargetEpochs)
    ).


compare_rev_epochs([{Node, Seq} | SourceRest], [{Node, Seq} | TargetRest]) ->
    % Common history, fast-forward
    compare_epochs(SourceRest, TargetRest);
compare_rev_epochs([], [{_, TargetSeq} | _]) ->
    % Source has not moved, start from seq just before the target took over
    TargetSeq - 1;
compare_rev_epochs([{_, SourceSeq} | _], []) ->
    % Target has not moved, start from seq where source diverged
    SourceSeq;
compare_rev_epochs([{_, SourceSeq} | _], [{_, TargetSeq} | _]) ->
    % The source was moved to a new location independently, take the minimum
    erlang:min(SourceSeq, TargetSeq) - 1.


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

    % Re-bucket our history based on the most recent source
    % sequence. This is where we drop old checkpoints to
    % maintain the exponential distribution.
    {_, RebucketedHistory} = rebucket(FilteredHistory, SourceSeq, 0),
    NewSourceHistory = [{Props} | RebucketedHistory],

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


%% @doc This function adjusts our history to maintain a
%% history of checkpoints that follow an exponentially
%% increasing age from the most recent checkpoint.
%%
%% The terms newest and oldest used in these comments
%% refers to the (NewSeq - CurSeq) difference where smaller
%% values are considered newer.
%%
%% It works by assigning each entry to a bucket and keeping
%% the newest and oldest entry in each bucket. Keeping
%% both the newest and oldest means that we won't end up
%% with empty buckets as checkpoints are promoted to new
%% buckets.
%%
%% The return value of this function is a two-tuple of the
%% form `{BucketId, History}` where BucketId is the id of
%% the bucket for the first entry in History. This is used
%% when recursing to detect the oldest value in a given
%% bucket.
%%
%% This function expects the provided history to be sorted
%% in descending order of source_seq values.
rebucket([], _NewSeq, Bucket) ->
    {Bucket+1, []};
rebucket([{Entry} | RestHistory], NewSeq, Bucket) ->
    CurSeq = couch_util:get_value(<<"source_seq">>, Entry),
    case find_bucket(NewSeq, CurSeq, Bucket) of
        Bucket ->
            % This entry is in an existing bucket which means
            % we will only keep it if its the oldest value
            % in the bucket. To detect this we rebucket the
            % rest of the list and only include Entry if the
            % rest of the list is in a bigger bucket.
            case rebucket(RestHistory, NewSeq, Bucket) of
                {Bucket, NewHistory} ->
                    % There's another entry in this bucket so we drop the
                    % current entry.
                    {Bucket, NewHistory};
                {NextBucket, NewHistory} when NextBucket > Bucket ->
                    % The rest of the history was rebucketed into a larger
                    % bucket so this is the oldest entry in the current
                    % bucket.
                    {Bucket, [{Entry} | NewHistory]}
            end;
        NextBucket when NextBucket > Bucket ->
            % This entry is the newest in NextBucket so we add it
            % to our history and continue rebucketing.
            {_, NewHistory} = rebucket(RestHistory, NewSeq, NextBucket),
            {NextBucket, [{Entry} | NewHistory]}
    end.


%% @doc Find the bucket id for the given sequence pair.
find_bucket(NewSeq, CurSeq, Bucket) ->
    % The +1 constant in this comparison is a bit subtle. The
    % reason for it is to make sure that the first entry in
    % the history is guaranteed to have a BucketId of 1. This
    % also relies on never having a duplicated update
    % sequence so adding 1 here guarantees a difference >= 2.
    if (NewSeq - CurSeq + 1) > (2 bsl Bucket) ->
        find_bucket(NewSeq, CurSeq, Bucket+1);
    true ->
        Bucket
    end.


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


get_or_create_db(DbName, Options) ->
    couch_db:open_int(DbName, [{create_if_missing, true} | Options]).


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
