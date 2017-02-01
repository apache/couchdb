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

-module(couch_mrview_compactor).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([compact/3, swap_compacted/2, remove_compacted/1]).

-record(acc, {
   btree = nil,
   last_id = nil,
   kvs = [],
   kvs_size = 0,
   changes = 0,
   total_changes
}).

-define(DEFAULT_RECOMPACT_RETRY_COUNT, 3).

compact(_Db, State, Opts) ->
    case lists:member(recompact, Opts) of
        false -> compact(State);
        true -> recompact(State)
    end.

compact(State) ->
    #mrst{
        db_name=DbName,
        idx_name=IdxName,
        sig=Sig,
        update_seq=Seq,
        id_btree=IdBtree,
        log_btree=LogBtree,
        seq_indexed=SeqIndexed,
        keyseq_indexed=KeySeqIndexed,
        views=Views
    } = State,
    erlang:put(io_priority, {view_compact, DbName, IdxName}),

    {EmptyState, NumDocIds} = couch_util:with_db(DbName, fun(Db) ->
        CompactFName = couch_mrview_util:compaction_file(DbName, Sig),
        {ok, Fd} = couch_mrview_util:open_file(CompactFName),
        ESt = couch_mrview_util:reset_index(Db, Fd, State),

        {ok, Count} = couch_db:get_doc_count(Db),

        {ESt, Count}
    end),

    #mrst{
        id_btree = EmptyIdBtree,
        log_btree = EmptyLogBtree,
        views = EmptyViews
    } = EmptyState,

    TotalChanges0 = case SeqIndexed orelse KeySeqIndexed of
        true -> NumDocIds * 2;
        _ -> NumDocIds
    end,

    TotalChanges = lists:foldl(
        fun(View, Acc) ->
            {ok, Kvs} = couch_mrview_util:get_row_count(View),
            case SeqIndexed orelse KeySeqIndexed of
                true ->
                    {ok, SKvs} = couch_mrview_util:get_view_changes_count(View),
                    Acc + Kvs + SKvs;
                false ->
                    Acc + Kvs
            end
        end,
        TotalChanges0, Views),

    couch_task_status:add_task([
        {type, view_compaction},
        {database, DbName},
        {design_document, IdxName},
        {progress, 0}
    ]),

    BufferSize0 = config:get(
        "view_compaction", "keyvalue_buffer_size", "2097152"
    ),
    BufferSize = list_to_integer(BufferSize0),

    FoldFun = fun({DocId, ViewIdKeys} = KV, Acc) ->
        #acc{btree = Bt, kvs = Kvs, kvs_size = KvsSize} = Acc,
        NewKvs = case Kvs of
            [{DocId, OldViewIdKeys} | Rest] ->
                couch_log:error("Dupes of ~s in ~s ~s",
                                [DocId, DbName, IdxName]),
                [{DocId, ViewIdKeys ++ OldViewIdKeys} | Rest];
            _ ->
                [KV | Kvs]
        end,
        KvsSize2 = KvsSize + ?term_size(KV),
        case KvsSize2 >= BufferSize of
            true ->
                {ok, Bt2} = couch_btree:add(Bt, lists:reverse(NewKvs)),
                Acc2 = update_task(Acc, length(NewKvs)),
                {ok, Acc2#acc{
                    btree = Bt2, kvs = [], kvs_size = 0, last_id = DocId}};
            _ ->
                {ok, Acc#acc{
                    kvs = NewKvs, kvs_size = KvsSize2, last_id = DocId}}
        end
    end,

    InitAcc = #acc{total_changes = TotalChanges, btree = EmptyIdBtree},
    {ok, _, FinalAcc} = couch_btree:foldl(IdBtree, FoldFun, InitAcc),
    #acc{btree = Bt3, kvs = Uncopied} = FinalAcc,
    {ok, NewIdBtree} = couch_btree:add(Bt3, lists:reverse(Uncopied)),
    FinalAcc2 = update_task(FinalAcc, length(Uncopied)),


    {NewLogBtree, FinalAcc3} = case SeqIndexed of
        true ->
            compact_log(LogBtree, BufferSize,
                        FinalAcc2#acc{kvs=[],
                                      kvs_size=0,
                                      btree=EmptyLogBtree});
        _ ->
            {nil, FinalAcc2}
    end,

    {NewViews, _} = lists:mapfoldl(fun({View, EmptyView}, Acc) ->
        compact_view(View, EmptyView, BufferSize, Acc)
    end, FinalAcc3, lists:zip(Views, EmptyViews)),

    unlink(EmptyState#mrst.fd),
    {ok, EmptyState#mrst{
        id_btree=NewIdBtree,
        log_btree=NewLogBtree,
        views=NewViews,
        update_seq=Seq
    }}.


recompact(State) ->
    recompact(State, recompact_retry_count()).

recompact(#mrst{db_name=DbName, idx_name=IdxName}, 0) ->
    erlang:error({exceeded_recompact_retry_count,
        [{db_name, DbName}, {idx_name, IdxName}]});

recompact(State, RetryCount) ->
    Self = self(),
    link(State#mrst.fd),
    {Pid, Ref} = erlang:spawn_monitor(fun() ->
        couch_index_updater:update(Self, couch_mrview_index, State)
    end),
    recompact_loop(Pid, Ref, State, RetryCount).

recompact_loop(Pid, Ref, State, RetryCount) ->
    receive
        {'$gen_cast', {new_state, State2}} ->
            % We've made progress so reset RetryCount
            recompact_loop(Pid, Ref, State2, recompact_retry_count());
        {'DOWN', Ref, _, _, {updated, Pid, State2}} ->
            unlink(State#mrst.fd),
            {ok, State2};
        {'DOWN', Ref, _, _, Reason} ->
            unlink(State#mrst.fd),
            couch_log:warning("Error during recompaction: ~r", [Reason]),
            recompact(State, RetryCount - 1)
    end.

recompact_retry_count() ->
    config:get_integer(
        "view_compaction",
        "recompact_retry_count",
        ?DEFAULT_RECOMPACT_RETRY_COUNT
    ).


compact_log(LogBtree, BufferSize, Acc0) ->
    FoldFun = fun(KV, Acc) ->
        #acc{btree = Bt, kvs = Kvs, kvs_size = KvsSize} = Acc,
        KvsSize2 = KvsSize + ?term_size(KV),
        case KvsSize2 >= BufferSize of
            true ->
                {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV | Kvs])),
                Acc2 = update_task(Acc, 1 + length(Kvs)),
                {ok, Acc2#acc{
                    btree = Bt2, kvs = [], kvs_size = 0}};
            _ ->
                {ok, Acc#acc{
                    kvs = [KV | Kvs], kvs_size = KvsSize2}}
        end
    end,

    {ok, _, FinalAcc} = couch_btree:foldl(LogBtree, FoldFun, Acc0),
    #acc{btree = Bt3, kvs = Uncopied} = FinalAcc,
    {ok, NewLogBtree} = couch_btree:add(Bt3, lists:reverse(Uncopied)),
    FinalAcc2 = update_task(FinalAcc, length(Uncopied)),
    {NewLogBtree, FinalAcc2}.

%% @spec compact_view(View, EmptyView, Retry, Acc) -> {CompactView, NewAcc}
compact_view(#mrview{id_num=VID}=View, EmptyView, BufferSize, Acc0) ->

    {NewBt, Acc1} = compact_view_btree(View#mrview.btree,
                                       EmptyView#mrview.btree,
                                       VID, BufferSize, Acc0),

    %% are we indexing changes by sequences?
    {NewSeqBt, Acc2} = case View#mrview.seq_indexed of
        true ->
            compact_view_btree(View#mrview.seq_btree,
                               EmptyView#mrview.seq_btree,
                               VID, BufferSize, Acc1);
        _ ->
            {nil, Acc1}
    end,
    {NewKeyBySeqBt, FinalAcc} = case View#mrview.keyseq_indexed of
        true ->
            compact_view_btree(View#mrview.key_byseq_btree,
                               EmptyView#mrview.key_byseq_btree,
                               VID, BufferSize, Acc2);
        _ ->
            {nil, Acc2}
    end,

    {EmptyView#mrview{btree=NewBt,
                      seq_btree=NewSeqBt,
                      key_byseq_btree=NewKeyBySeqBt}, FinalAcc}.

compact_view_btree(Btree, EmptyBtree, VID, BufferSize, Acc0) ->
    Fun = fun(KV, #acc{btree = Bt, kvs = Kvs, kvs_size = KvsSize} = Acc) ->
        KvsSize2 = KvsSize + ?term_size(KV),
        if KvsSize2 >= BufferSize ->
            {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV | Kvs])),
            Acc2 = update_task(VID, Acc, 1 + length(Kvs)),
            {ok, Acc2#acc{btree = Bt2, kvs = [], kvs_size = 0}};
        true ->
            {ok, Acc#acc{kvs = [KV | Kvs], kvs_size = KvsSize2}}
        end
    end,

    InitAcc = Acc0#acc{kvs = [], kvs_size = 0, btree = EmptyBtree},
    {ok, _, FinalAcc} = couch_btree:foldl(Btree, Fun, InitAcc),
    #acc{btree = Bt3, kvs = Uncopied} = FinalAcc,
    {ok, NewBt} = couch_btree:add(Bt3, lists:reverse(Uncopied)),
    FinalAcc2 = update_task(VID, FinalAcc, length(Uncopied)),
    {NewBt, FinalAcc2}.

update_task(Acc, ChangesInc) ->
    update_task(null, Acc, ChangesInc).


update_task(VID, #acc{changes=Changes, total_changes=Total}=Acc, ChangesInc) ->
    Phase = if is_integer(VID) -> view; true -> ids end,
    Changes2 = Changes + ChangesInc,
    Progress = if Total == 0 -> 0; true -> (Changes2 * 100) div Total end,
    couch_task_status:update([
        {phase, Phase},
        {view, VID},
        {changes_done, Changes2},
        {total_changes, Total},
        {progress, Progress}
    ]),
    Acc#acc{changes = Changes2}.


swap_compacted(OldState, NewState) ->
    #mrst{
        sig=Sig,
        db_name=DbName
    } = NewState,

    link(NewState#mrst.fd),
    Ref = erlang:monitor(process, NewState#mrst.fd),

    RootDir = couch_index_util:root_dir(),
    IndexFName = couch_mrview_util:index_file(DbName, Sig),
    CompactFName = couch_mrview_util:compaction_file(DbName, Sig),
    ok = couch_file:delete(RootDir, IndexFName),
    ok = file:rename(CompactFName, IndexFName),

    unlink(OldState#mrst.fd),
    erlang:demonitor(OldState#mrst.fd_monitor, [flush]),
    
    {ok, NewState#mrst{fd_monitor=Ref}}.


remove_compacted(#mrst{sig = Sig, db_name = DbName} = State) ->
    RootDir = couch_index_util:root_dir(),
    CompactFName = couch_mrview_util:compaction_file(DbName, Sig),
    ok = couch_file:delete(RootDir, CompactFName),
    {ok, State}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

recompact_test_() ->
    [
        recompact_success_after_progress(),
        recompact_exceeded_retry_count()
    ].

recompact_success_after_progress() ->
    ?_test(begin
        ok = meck:expect(couch_index_updater, update, fun
            (Pid, _, #mrst{update_seq=0} = State) ->
                Pid ! {'$gen_cast', {new_state, State#mrst{update_seq = 1}}},
                timer:sleep(100),
                exit({updated, self(), State#mrst{update_seq = 2}})
        end),
        try
            State = #mrst{fd=self(), update_seq=0},
            ?assertEqual({ok, State#mrst{update_seq = 2}}, recompact(State))
        after
            meck:unload(couch_index_updater)
        end
    end).

recompact_exceeded_retry_count() ->
    ?_test(begin
        ok = meck:expect(couch_index_updater, update,
            fun(_, _, _) ->
                exit(error)
        end),
        ok = meck:expect(couch_log, warning, fun(_, _) -> ok end),
        try
            State = #mrst{fd=self(), db_name=foo, idx_name=bar},
            ExpectedError = {exceeded_recompact_retry_count,
                [{db_name, foo}, {idx_name, bar}]},
                ?assertError(ExpectedError, recompact(State))
        after
            meck:unload(couch_log),
            meck:unload(couch_index_updater)
        end
    end).

-endif.
