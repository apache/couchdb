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

-include("couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


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
        views=Views
    } = State,

    Ids = [Id || {Id, _Revs} <- PurgedIdRevs],
    {ok, Lookups, IdBtree2} = couch_btree:query_modify(IdBtree, Ids, [], Ids),

    MakeDictFun = fun
        ({ok, {DocId, ViewNumRowKeys}}, DictAcc) ->
            FoldFun = fun({ViewNum, RowKey}, DictAcc2) ->
                dict:append(ViewNum, {RowKey, DocId}, DictAcc2)
            end,
            lists:foldl(FoldFun, DictAcc, ViewNumRowKeys);
        ({not_found, _}, DictAcc) ->
            DictAcc
    end,
    KeysToRemove = lists:foldl(MakeDictFun, dict:new(), Lookups),

    RemKeysFun = fun(#mrview{id_num=Num, btree=Btree}=View) ->
        case dict:find(Num, KeysToRemove) of
            {ok, RemKeys} ->
                {ok, Btree2} = couch_btree:add_remove(Btree, [], RemKeys),
                NewPurgeSeq = case Btree2 /= Btree of
                    true -> PurgeSeq;
                    _ -> View#mrview.purge_seq
                end,
                View#mrview{btree=Btree2, purge_seq=NewPurgeSeq};
            error ->
                View
        end
    end,

    Views2 = lists:map(RemKeysFun, Views),
    {ok, State#mrst{
        id_btree=IdBtree2,
        views=Views2,
        purge_seq=PurgeSeq
    }}.


process_doc(Doc, Seq, #mrst{doc_acc=Acc}=State) when length(Acc) > 100 ->
    couch_work_queue:queue(State#mrst.doc_queue, lists:reverse(Acc)),
    process_doc(Doc, Seq, State#mrst{doc_acc=[]});
process_doc(nil, Seq, #mrst{doc_acc=Acc}=State) ->
    {ok, State#mrst{doc_acc=[{nil, Seq, nil} | Acc]}};
process_doc(#doc{id=Id, deleted=true}, Seq, #mrst{doc_acc=Acc}=State) ->
    {ok, State#mrst{doc_acc=[{Id, Seq, deleted} | Acc]}};
process_doc(#doc{id=Id}=Doc, Seq, #mrst{doc_acc=Acc}=State) ->
    {ok, State#mrst{doc_acc=[{Id, Seq, Doc} | Acc]}}.


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
                ({nil, Seq, _}, {SeqAcc, Results}) ->
                    {erlang:max(Seq, SeqAcc), Results};
                ({Id, Seq, deleted}, {SeqAcc, Results}) ->
                    {erlang:max(Seq, SeqAcc), [{Id, []} | Results]};
                ({Id, Seq, Doc}, {SeqAcc, Results}) ->
                    {ok, Res} = couch_query_servers:map_doc_raw(QServer, Doc),
                    {erlang:max(Seq, SeqAcc), [{Id, Res} | Results]}
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
    case couch_work_queue:dequeue(State#mrst.write_queue) of
        closed ->
            Parent ! {new_state, State};
        {ok, Info} ->
            EmptyKVs = [{V#mrview.id_num, []} || V <- State#mrst.views],
            {Seq, ViewKVs, DocIdKeys} = merge_results(Info, 0, EmptyKVs, []),
            NewState = write_kvs(State, Seq, ViewKVs, DocIdKeys),
            send_partial(NewState#mrst.partial_resp_pid, NewState),
            write_results(Parent, NewState)
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


merge_results([], SeqAcc, ViewKVs, DocIdKeys) ->
    {SeqAcc, ViewKVs, DocIdKeys};
merge_results([{Seq, Results} | Rest], SeqAcc, ViewKVs, DocIdKeys) ->
    Fun = fun(RawResults, {VKV, DIK}) ->
        merge_results(RawResults, VKV, DIK)
    end,
    {ViewKVs1, DocIdKeys1} = lists:foldl(Fun, {ViewKVs, DocIdKeys}, Results),
    merge_results(Rest, erlang:max(Seq, SeqAcc), ViewKVs1, DocIdKeys1).


merge_results({DocId, []}, ViewKVs, DocIdKeys) ->
    {ViewKVs, [{DocId, []} | DocIdKeys]};
merge_results({DocId, RawResults}, ViewKVs, DocIdKeys) ->
    JsonResults = couch_query_servers:raw_to_ejson(RawResults),
    Results = [[list_to_tuple(Res) || Res <- FunRs] || FunRs <- JsonResults],
    {ViewKVs1, ViewIdKeys} = insert_results(DocId, Results, ViewKVs, [], []),
    {ViewKVs1, [ViewIdKeys | DocIdKeys]}.


insert_results(DocId, [], [], ViewKVs, ViewIdKeys) ->
    {lists:reverse(ViewKVs), {DocId, ViewIdKeys}};
insert_results(DocId, [KVs | RKVs], [{Id, VKVs} | RVKVs], VKVAcc, VIdKeys) ->
    CombineDupesFun = fun
        ({Key, Val}, {[{Key, {dups, Vals}} | Rest], IdKeys}) ->
            {[{Key, {dups, [Val | Vals]}} | Rest], IdKeys};
        ({Key, Val1}, {[{Key, Val2} | Rest], IdKeys}) ->
            {[{Key, {dups, [Val1, Val2]}} | Rest], IdKeys};
        ({Key, _}=KV, {Rest, IdKeys}) ->
            {[KV | Rest], [{Id, Key} | IdKeys]}
    end,
    InitAcc = {[], VIdKeys},
    {Duped, VIdKeys0} = lists:foldl(CombineDupesFun, InitAcc, lists:sort(KVs)),
    FinalKVs = [{{Key, DocId}, Val} || {Key, Val} <- Duped] ++ VKVs,
    insert_results(DocId, RKVs, RVKVs, [{Id, FinalKVs} | VKVAcc], VIdKeys0).


write_kvs(State, UpdateSeq, ViewKVs, DocIdKeys) ->
    #mrst{
        id_btree=IdBtree,
        first_build=FirstBuild
    } = State,

    {ok, ToRemove, IdBtree2} = update_id_btree(IdBtree, DocIdKeys, FirstBuild),
    ToRemByView = collapse_rem_keys(ToRemove, dict:new()),

    UpdateView = fun(#mrview{id_num=ViewId}=View, {ViewId, KVs}) ->
        ToRem = couch_util:dict_find(ViewId, ToRemByView, []),
        {ok, VBtree2} = couch_btree:add_remove(View#mrview.btree, KVs, ToRem),
        NewUpdateSeq = case VBtree2 =/= View#mrview.btree of
            true -> UpdateSeq;
            _ -> View#mrview.update_seq
        end,
        View#mrview{btree=VBtree2, update_seq=NewUpdateSeq}
    end,

    State#mrst{
        views=lists:zipwith(UpdateView, State#mrst.views, ViewKVs),
        update_seq=UpdateSeq,
        id_btree=IdBtree2
    }.


update_id_btree(Btree, DocIdKeys, true) ->
    ToAdd = [{Id, DIKeys} || {Id, DIKeys} <- DocIdKeys, DIKeys /= []],
    couch_btree:query_modify(Btree, [], ToAdd, []);
update_id_btree(Btree, DocIdKeys, _) ->
    ToFind = [Id || {Id, _} <- DocIdKeys],
    ToAdd = [{Id, DIKeys} || {Id, DIKeys} <- DocIdKeys, DIKeys /= []],
    ToRem = [Id || {Id, DIKeys} <- DocIdKeys, DIKeys == []],
    couch_btree:query_modify(Btree, ToFind, ToAdd, ToRem).


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
