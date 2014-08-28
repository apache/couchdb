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

-module(couch_mrview_updater).

-export([start_update/3, purge/4, process_doc/3, finish_update/1]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(REM_VAL,  {[{<<"_removed">>, true}]}).


start_update(Partial, State, NumChanges) ->
    QueueOpts = [{max_size, 100000}, {max_items, 500}],
    {ok, DocQueue} = couch_work_queue:new(QueueOpts),
    {ok, WriteQueue} = couch_work_queue:new(QueueOpts),

    InitState = State#mrst{
        first_build=State#mrst.update_seq==0,
        partial_resp_pid=Partial,
        doc_acc=[],
        doc_queue=DocQueue,
        write_queue=WriteQueue
    },

    Self = self(),
    MapFun = fun() ->
        couch_task_status:add_task([
            {type, indexer},
            {database, State#mrst.db_name},
            {design_document, State#mrst.idx_name},
            {progress, 0},
            {changes_done, 0},
            {total_changes, NumChanges}
        ]),
        couch_task_status:set_update_frequency(500),
        map_docs(Self, InitState)
    end,
    WriteFun = fun() -> write_results(Self, InitState) end,

    spawn_link(MapFun),
    spawn_link(WriteFun),

    {ok, InitState}.


purge(_Db, PurgeSeq, PurgedIdRevs, State) ->
    #mrst{
        id_btree=IdBtree,
        log_btree=LogBtree,
        views=Views
    } = State,

    Ids = [Id || {Id, _Revs} <- PurgedIdRevs],
    {ok, Lookups, LLookups, LogBtree2, IdBtree2} = case LogBtree of
        nil ->
            {ok, L, Bt} = couch_btree:query_modify(IdBtree, Ids, [], Ids),
            {ok, L, [], nil, Bt};
        _ ->
            {ok, L, Bt} = couch_btree:query_modify(IdBtree, Ids, [], Ids),
            {ok, LL, LBt} = couch_btree:query_modify(LogBtree, Ids, [], Ids),
            {ok, L, LL, LBt, Bt}
    end,

    MakeDictFun = fun
        ({ok, {DocId, ViewNumRowKeys}}, DictAcc) ->
            FoldFun = fun
                ({ViewNum, {Key, Seq, _Op}}, DictAcc2) ->
                    dict:append(ViewNum, {Key, Seq, DocId}, DictAcc2);
                ({ViewNum, RowKey}, DictAcc2) ->
                    dict:append(ViewNum, {RowKey, DocId}, DictAcc2)
            end,
            lists:foldl(FoldFun, DictAcc, ViewNumRowKeys);
        ({not_found, _}, DictAcc) ->
            DictAcc
    end,
    KeysToRemove = lists:foldl(MakeDictFun, dict:new(), Lookups),
    SeqsToRemove = lists:foldl(MakeDictFun, dict:new(), LLookups),

    RemKeysFun = fun(#mrview{id_num=ViewId}=View) ->
        #mrview{seq_indexed=SIndexed, keyseq_indexed=KSIndexed} = View,
        ToRem = couch_util:dict_find(ViewId, KeysToRemove, []),
        {ok, VBtree2} = couch_btree:add_remove(View#mrview.btree, [], ToRem),
        NewPurgeSeq = case VBtree2 =/= View#mrview.btree of
            true -> PurgeSeq;
            _ -> View#mrview.purge_seq
        end,
        {SeqBtree3, KeyBySeqBtree3} = if SIndexed orelse KSIndexed ->
            SToRem = couch_util:dict_find(ViewId, SeqsToRemove, []),
            {ok, SeqBtree2} = if SIndexed ->
                SKs = [{Seq, Key} || {Key, Seq, _} <- SToRem],
                couch_btree:add_remove(View#mrview.seq_btree,
                                       [], SKs);
            true ->
                {ok, nil}
            end,
            {ok, KeyBySeqBtree2} = if KSIndexed ->
                KSs = [{[Seq, Key], DocId} || {Key, Seq, DocId} <- SToRem],
                couch_btree:add_remove(View#mrview.key_byseq_btree,
                                       [], KSs);
            true ->
                {ok, nil}
            end,
            {SeqBtree2, KeyBySeqBtree2};
        true ->
            {nil, nil}
        end,

        View#mrview{btree=VBtree2,
                    seq_btree=SeqBtree3,
                    key_byseq_btree=KeyBySeqBtree3,
                    purge_seq=NewPurgeSeq}

    end,

    Views2 = lists:map(RemKeysFun, Views),
    {ok, State#mrst{
        id_btree=IdBtree2,
        log_btree=LogBtree2,
        views=Views2,
        purge_seq=PurgeSeq
    }}.


process_doc(Doc, Seq, #mrst{doc_acc=Acc}=State) when length(Acc) > 100 ->
    couch_work_queue:queue(State#mrst.doc_queue, lists:reverse(Acc)),
    process_doc(Doc, Seq, State#mrst{doc_acc=[]});
process_doc(nil, Seq, #mrst{doc_acc=Acc}=State) ->
    {ok, State#mrst{doc_acc=[{nil, Seq, nil, nil} | Acc]}};
process_doc(#doc{id=Id, deleted=true}=Doc, Seq, #mrst{doc_acc=Acc}=State) ->
    {RevPos, [Rev | _]} = Doc#doc.revs,
    {ok, State#mrst{doc_acc=[{Id, Seq, {RevPos, Rev}, deleted} | Acc]}};
process_doc(#doc{id=Id}=Doc, Seq, #mrst{doc_acc=Acc}=State) ->
    {RevPos, [Rev | _]} = Doc#doc.revs,
    {ok, State#mrst{doc_acc=[{Id, Seq, {RevPos, Rev}, Doc} | Acc]}}.


finish_update(#mrst{doc_acc=Acc}=State) ->
    if Acc /= [] ->
        couch_work_queue:queue(State#mrst.doc_queue, Acc);
        true -> ok
    end,
    couch_work_queue:close(State#mrst.doc_queue),
    receive
        {new_state, NewState} ->
            {ok, NewState#mrst{
                first_build=undefined,
                partial_resp_pid=undefined,
                doc_acc=undefined,
                doc_queue=undefined,
                write_queue=undefined,
                qserver=nil
            }}
    end.


map_docs(Parent, State0) ->
    case couch_work_queue:dequeue(State0#mrst.doc_queue) of
        closed ->
            couch_query_servers:stop_doc_map(State0#mrst.qserver),
            couch_work_queue:close(State0#mrst.write_queue);
        {ok, Dequeued} ->
            % Run all the non deleted docs through the view engine and
            % then pass the results on to the writer process.
            State1 = case State0#mrst.qserver of
                nil -> start_query_server(State0);
                _ -> State0
            end,
            QServer = State1#mrst.qserver,
            DocFun = fun
                ({nil, Seq, _, _}, {SeqAcc, Results}) ->
                    {erlang:max(Seq, SeqAcc), Results};
                ({Id, Seq, Rev, deleted}, {SeqAcc, Results}) ->
                    {erlang:max(Seq, SeqAcc), [{Id, Seq, Rev, []} | Results]};
                ({Id, Seq, Rev, Doc}, {SeqAcc, Results}) ->
                    couch_stats:increment_counter([couchdb, mrview, map_docs],
                                                  1),
                    {ok, Res} = couch_query_servers:map_doc_raw(QServer, Doc),
                    {erlang:max(Seq, SeqAcc), [{Id, Seq, Rev, Res} | Results]}
            end,
            FoldFun = fun(Docs, Acc) ->
                update_task(length(Docs)),
                lists:foldl(DocFun, Acc, Docs)
            end,
            Results = lists:foldl(FoldFun, {0, []}, Dequeued),
            couch_work_queue:queue(State1#mrst.write_queue, Results),
            map_docs(Parent, State1)
    end.


write_results(Parent, State) ->
    case accumulate_writes(State, State#mrst.write_queue, nil) of
        stop ->
            Parent ! {new_state, State};
        {Go, {Seq, ViewKVs, DocIdKeys, Log}} ->
            NewState = write_kvs(State, Seq, ViewKVs, DocIdKeys, Log),
            if Go == stop ->
                Parent ! {new_state, NewState};
            true ->
                send_partial(NewState#mrst.partial_resp_pid, NewState),
                write_results(Parent, NewState)
            end
    end.


start_query_server(State) ->
    #mrst{
        language=Language,
        lib=Lib,
        views=Views
    } = State,
    Defs = [View#mrview.def || View <- Views],
    {ok, QServer} = couch_query_servers:start_doc_map(Language, Defs, Lib),
    State#mrst{qserver=QServer}.


accumulate_writes(State, W, Acc0) ->
    {Seq, ViewKVs, DocIdKVs, Log} = case Acc0 of
        nil -> {0, [{V#mrview.id_num, {[], []}} || V <- State#mrst.views], [], dict:new()};
        _ -> Acc0
    end,
    case couch_work_queue:dequeue(W) of
        closed when Seq == 0 ->
            stop;
        closed ->
            {stop, {Seq, ViewKVs, DocIdKVs, Log}};
        {ok, Info} ->
            {_, _, NewIds, _} = Acc = merge_results(Info, Seq, ViewKVs, DocIdKVs, Log),
            case accumulate_more(length(NewIds)) of
                true -> accumulate_writes(State, W, Acc);
                false -> {ok, Acc}
            end
    end.


accumulate_more(NumDocIds) ->
    % check if we have enough items now
    MinItems = config:get("view_updater", "min_writer_items", "100"),
    MinSize = config:get("view_updater", "min_writer_size", "16777216"),
    {memory, CurrMem} = process_info(self(), memory),
    NumDocIds < list_to_integer(MinItems)
        andalso CurrMem < list_to_integer(MinSize).


merge_results([], SeqAcc, ViewKVs, DocIdKeys, Log) ->
    {SeqAcc, ViewKVs, DocIdKeys, Log};
merge_results([{Seq, Results} | Rest], SeqAcc, ViewKVs, DocIdKeys, Log) ->
    Fun = fun(RawResults, {VKV, DIK, Log2}) ->
        merge_results(RawResults, VKV, DIK, Log2)
    end,
    {ViewKVs1, DocIdKeys1, Log1} = lists:foldl(Fun, {ViewKVs, DocIdKeys, Log},
                                               Results),
    merge_results(Rest, erlang:max(Seq, SeqAcc), ViewKVs1, DocIdKeys1,
                  Log1).


merge_results({DocId, _Seq, Rev, []}, ViewKVs, DocIdKeys, Log) ->
    {ViewKVs, [{DocId, Rev, []} | DocIdKeys], dict:store(DocId, [], Log)};
merge_results({DocId, Seq, Rev, RawResults}, ViewKVs, DocIdKeys, Log) ->
    JsonResults = couch_query_servers:raw_to_ejson(RawResults),
    Results = [[list_to_tuple(Res) || Res <- FunRs] || FunRs <- JsonResults],
    {ViewKVs1, ViewIdKeys, Log1} = insert_results(DocId, Seq, Rev, Results, ViewKVs, [],
                                            [], Log),
    {ViewKVs1, [ViewIdKeys | DocIdKeys], Log1}.


insert_results(DocId, _Seq, _Rev, [], [], ViewKVs, ViewIdKeys, Log) ->
    {lists:reverse(ViewKVs), {DocId, ViewIdKeys}, Log};
insert_results(DocId, Seq, Rev, [KVs | RKVs], [{Id, {VKVs, SKVs}} | RVKVs], VKVAcc,
               VIdKeys, Log) ->
    CombineDupesFun = fun
        ({Key, Val}, {[{Key, {dups, Vals}} | Rest], IdKeys, Log2}) ->
            {[{Key, {dups, [Val | Vals]}} | Rest], IdKeys, Log2};
        ({Key, Val1}, {[{Key, Val2} | Rest], IdKeys, Log2}) ->
            {[{Key, {dups, [Val1, Val2]}} | Rest], IdKeys, Log2};
        ({Key, Value}, {Rest, IdKeys, Log2}) ->
            {[{Key, Value} | Rest], [{Id, Key} | IdKeys],
             dict:append(DocId, {Id, {Key, Seq, add}}, Log2)}
    end,
    InitAcc = {[], VIdKeys, Log},
    couch_stats:increment_counter([couchdb, mrview, emits], length(KVs)),
    {Duped, VIdKeys0, Log1} = lists:foldl(CombineDupesFun, InitAcc,
                                          lists:sort(KVs)),
    FinalKVs = [{{Key, DocId}, Val} || {Key, Val} <- Duped] ++ VKVs,
    FinalSKVs = [{{Seq, Key}, {DocId, Val, Rev}} || {Key, Val} <- Duped] ++ SKVs,
    insert_results(DocId, Seq, Rev, RKVs, RVKVs,
                  [{Id, {FinalKVs, FinalSKVs}} | VKVAcc], VIdKeys0, Log1).


write_kvs(State, UpdateSeq, ViewKVs, DocIdKeys, Log) ->
    #mrst{
        id_btree=IdBtree,
        log_btree=LogBtree,
        first_build=FirstBuild
    } = State,

    {ok, ToRemove, IdBtree2} = update_id_btree(IdBtree, DocIdKeys, FirstBuild),
    ToRemByView = collapse_rem_keys(ToRemove, dict:new()),

    {ok, SeqsToAdd, SeqsToRemove, LogBtree2} = case LogBtree of
        nil -> {ok, undefined, undefined, nil};
        _ -> update_log(LogBtree, Log, UpdateSeq, FirstBuild)
    end,

    UpdateView = fun(#mrview{id_num=ViewId}=View, {ViewId, {KVs, SKVs}}) ->
        #mrview{seq_indexed=SIndexed, keyseq_indexed=KSIndexed} = View,
        ToRem = couch_util:dict_find(ViewId, ToRemByView, []),
        {ok, VBtree2} = couch_btree:add_remove(View#mrview.btree, KVs, ToRem),
        NewUpdateSeq = case VBtree2 =/= View#mrview.btree of
            true -> UpdateSeq;
            _ -> View#mrview.update_seq
        end,

        %% store the view changes.
        {SeqBtree3, KeyBySeqBtree3} = if SIndexed orelse KSIndexed ->
            SToRem = couch_util:dict_find(ViewId, SeqsToRemove, []),
            SToAdd = couch_util:dict_find(ViewId, SeqsToAdd, []),
            SKVs1 = SKVs ++ SToAdd,

            {ok, SeqBtree2} = if SIndexed ->
                RemSKs = [{Seq, Key} || {Key, Seq, _} <- SToRem],
                couch_btree:add_remove(View#mrview.seq_btree,
                                       SKVs1, RemSKs);
            true ->
                {ok, nil}
            end,
            {ok, KeyBySeqBtree2} = if KSIndexed ->
                RemKSs = [{[Key, Seq], DocId} || {Key, Seq, DocId} <- SToRem],
                couch_btree:add_remove(View#mrview.key_byseq_btree,
                                       couch_mrview_util:to_key_seq(SKVs1),
                                       RemKSs);
            true ->
                {ok, nil}
            end,
            {SeqBtree2, KeyBySeqBtree2};
        true ->
            {nil, nil}
        end,
        View#mrview{btree=VBtree2,
                    seq_btree=SeqBtree3,
                    key_byseq_btree=KeyBySeqBtree3,
                    update_seq=NewUpdateSeq}
    end,

    State#mrst{
        views=lists:zipwith(UpdateView, State#mrst.views, ViewKVs),
        update_seq=UpdateSeq,
        id_btree=IdBtree2,
        log_btree=LogBtree2
    }.


update_id_btree(Btree, DocIdKeys, true) ->
    ToAdd = [{Id, DIKeys} || {Id, DIKeys} <- DocIdKeys, DIKeys /= []],
    couch_btree:query_modify(Btree, [], ToAdd, []);
update_id_btree(Btree, DocIdKeys, _) ->
    ToFind = [Id || {Id, _} <- DocIdKeys],
    ToAdd = [{Id, DIKeys} || {Id, DIKeys} <- DocIdKeys, DIKeys /= []],
    ToRem = [Id || {Id, DIKeys} <- DocIdKeys, DIKeys == []],
    couch_btree:query_modify(Btree, ToFind, ToAdd, ToRem).

update_log(Btree, Log, _UpdatedSeq, true) ->
    ToAdd = lists:filter(fun({_, Keys}) -> Keys =/= [] end, dict:to_list(Log)),
    {ok, LogBtree2} = couch_btree:add_remove(Btree, ToAdd, []),
    {ok, dict:new(), dict:new(), LogBtree2};
update_log(Btree, Log, UpdatedSeq, _) ->
    %% build list of updated keys and Id
    {ToLook, Updated} = dict:fold(fun(Id, DIKeys, {IdsAcc, KeysAcc}) ->
        KeysAcc1 = lists:foldl(fun({ViewId, {Key, _Seq, _Op}}, KeysAcc2) ->
            [{Id, ViewId, Key} | KeysAcc2]
        end, KeysAcc, DIKeys),
        {[Id | IdsAcc], KeysAcc1}
    end, {[], []}, Log),

    MapFun = fun({ok, KV}) -> [KV]; (not_found) -> [] end,
    KVsToLook = lists:flatmap(MapFun, couch_btree:lookup(Btree, ToLook)),
    {Log1, AddAcc, DelAcc} = lists:foldl(fun({DocId, VIdKeys}, Acc) ->
        lists:foldl(fun({ViewId, {Key, Seq, Op}}, {Log4, AddAcc4, DelAcc4}) ->
            case lists:member({DocId, ViewId, Key}, Updated) of
                true ->
                    % the log is updated, deleted old record from the view
                    DelAcc5 = dict:append(ViewId, {Key, Seq, DocId}, DelAcc4),
                    {Log4, AddAcc4, DelAcc5};
                false when Op /= del ->
                    % an update operation has been logged for this key. We must
                    % now record it as deleted in the log, remove the old
                    % record in the view and update the view with a removed
                    % record.
                    LogValue = {ViewId, {Key, UpdatedSeq, del}},
                    Log5 = dict:append(DocId, LogValue, Log4),
                    DelAcc5 = dict:append(ViewId, {Key, Seq, DocId}, DelAcc4),
                    AddValue = {{UpdatedSeq, Key}, {DocId, ?REM_VAL}},
                    AddAcc5 = dict:append(ViewId, AddValue, AddAcc4),
                    {Log5, AddAcc5, DelAcc5};
                false ->
                    % the key has already been registered in the view as
                    % deleted, make sure to add it to the new log.
                    Log5 = dict:append(DocId, {ViewId, {Key, Seq, del}}, Log4),
                    {Log5, AddAcc4, DelAcc4}
            end
        end, Acc, VIdKeys)
    end, {Log, dict:new(), dict:new()}, KVsToLook),

    ToAdd = [{Id, DIKeys} || {Id, DIKeys} <- dict:to_list(Log1), DIKeys /= []],
    % store the new logs
    {ok, LogBtree2} = couch_btree:add_remove(Btree, ToAdd, []),
    {ok, AddAcc, DelAcc, LogBtree2}.

collapse_rem_keys([], Acc) ->
    Acc;
collapse_rem_keys([{ok, {DocId, ViewIdKeys}} | Rest], Acc) ->
    NewAcc = lists:foldl(fun({ViewId, Key}, Acc2) ->
        dict:append(ViewId, {Key, DocId}, Acc2)
    end, Acc, ViewIdKeys),
    collapse_rem_keys(Rest, NewAcc);
collapse_rem_keys([{not_found, _} | Rest], Acc) ->
    collapse_rem_keys(Rest, Acc).


send_partial(Pid, State) when is_pid(Pid) ->
    gen_server:cast(Pid, {new_state, State});
send_partial(_, _) ->
    ok.


update_task(NumChanges) ->
    [Changes, Total] = couch_task_status:get([changes_done, total_changes]),
    Changes2 = Changes + NumChanges,
    Progress = case Total of
        0 ->
            % updater restart after compaction finishes
            0;
        _ ->
            (Changes2 * 100) div Total
    end,
    couch_task_status:update([{progress, Progress}, {changes_done, Changes2}]).
