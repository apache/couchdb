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

-export([start_update/4, purge/4, process_doc/3, finish_update/1]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(REM_VAL, removed).

start_update(Partial, State, NumChanges, NumChangesDone) ->
    MaxSize = config:get_integer("view_updater", "queue_memory_cap", 100000),
    MaxItems = config:get_integer("view_updater", "queue_item_cap", 500),
    QueueOpts = [{max_size, MaxSize}, {max_items, MaxItems}],
    {ok, DocQueue} = couch_work_queue:new(QueueOpts),
    {ok, WriteQueue} = couch_work_queue:new(QueueOpts),
    InitState = State#mrst{
        first_build = State#mrst.update_seq == 0,
        partial_resp_pid = Partial,
        doc_acc = [],
        doc_queue = DocQueue,
        write_queue = WriteQueue
    },

    Self = self(),

    MapFun = fun() ->
        erlang:put(
            io_priority,
            {view_update, State#mrst.db_name, State#mrst.idx_name}
        ),
        Progress =
            case NumChanges of
                0 -> 0;
                _ -> (NumChangesDone * 100) div NumChanges
            end,
        couch_task_status:add_task([
            {indexer_pid, ?l2b(pid_to_list(Partial))},
            {type, indexer},
            {database, State#mrst.db_name},
            {design_document, State#mrst.idx_name},
            {progress, Progress},
            {changes_done, NumChangesDone},
            {total_changes, NumChanges}
        ]),
        couch_task_status:set_update_frequency(500),
        map_docs(Self, InitState)
    end,
    WriteFun = fun() ->
        erlang:put(
            io_priority,
            {view_update, State#mrst.db_name, State#mrst.idx_name}
        ),
        write_results(Self, InitState)
    end,
    spawn_link(MapFun),
    spawn_link(WriteFun),

    {ok, InitState}.

purge(_Db, PurgeSeq, PurgedIdRevs, State) ->
    #mrst{
        id_btree = IdBtree,
        views = Views,
        partitioned = Partitioned
    } = State,

    Ids = [Id || {Id, _Revs} <- PurgedIdRevs],
    {ok, Lookups, IdBtree2} = couch_btree:query_modify(IdBtree, Ids, [], Ids),

    MakeDictFun = fun
        ({ok, {DocId, ViewNumRowKeys}}, DictAcc) ->
            FoldFun = fun
                ({ViewNum, {Key, Seq, _Op}}, DictAcc2) ->
                    dict:append(ViewNum, {Key, Seq, DocId}, DictAcc2);
                ({ViewNum, RowKey0}, DictAcc2) ->
                    RowKey =
                        if
                            not Partitioned ->
                                RowKey0;
                            true ->
                                [{RK, _}] = inject_partition([{RowKey0, DocId}]),
                                RK
                        end,
                    dict:append(ViewNum, {RowKey, DocId}, DictAcc2)
            end,
            lists:foldl(FoldFun, DictAcc, ViewNumRowKeys);
        ({not_found, _}, DictAcc) ->
            DictAcc
    end,
    KeysToRemove = lists:foldl(MakeDictFun, dict:new(), Lookups),

    RemKeysFun = fun(#mrview{id_num = ViewId} = View) ->
        ToRem = couch_util:dict_find(ViewId, KeysToRemove, []),
        {ok, VBtree2} = couch_btree:add_remove(View#mrview.btree, [], ToRem),
        NewPurgeSeq =
            case VBtree2 =/= View#mrview.btree of
                true -> PurgeSeq;
                _ -> View#mrview.purge_seq
            end,
        View#mrview{btree = VBtree2, purge_seq = NewPurgeSeq}
    end,

    Views2 = lists:map(RemKeysFun, Views),
    {ok, State#mrst{
        id_btree = IdBtree2,
        views = Views2,
        purge_seq = PurgeSeq
    }}.

process_doc(Doc, Seq, #mrst{doc_acc = Acc} = State) when length(Acc) > 100 ->
    couch_work_queue:queue(State#mrst.doc_queue, lists:reverse(Acc)),
    process_doc(Doc, Seq, State#mrst{doc_acc = []});
process_doc(nil, Seq, #mrst{doc_acc = Acc} = State) ->
    {ok, State#mrst{doc_acc = [{nil, Seq, nil} | Acc]}};
% TODO: re-evaluate why this is commented out
% process_doc(#doc{id=Id, deleted=true}, Seq, #mrst{doc_acc=Acc}=State) ->
%     {ok, State#mrst{doc_acc=[{Id, Seq, deleted} | Acc]}};
process_doc(#doc{id = Id} = Doc, Seq, #mrst{doc_acc = Acc} = State) ->
    {ok, State#mrst{doc_acc = [{Id, Seq, Doc} | Acc]}}.

finish_update(#mrst{doc_acc = Acc} = State) ->
    if
        Acc /= [] ->
            couch_work_queue:queue(State#mrst.doc_queue, Acc);
        true ->
            ok
    end,
    couch_work_queue:close(State#mrst.doc_queue),
    receive
        {new_state, NewState} ->
            {ok, NewState#mrst{
                first_build = undefined,
                partial_resp_pid = undefined,
                doc_acc = undefined,
                doc_queue = undefined,
                write_queue = undefined,
                qserver = nil
            }}
    end.

make_deleted_body({Props}, Meta, Seq) ->
    BodySp = couch_util:get_value(body_sp, Meta),
    Result = [{<<"_seq">>, Seq}, {<<"_body_sp">>, BodySp}],
    case couch_util:get_value(<<"_access">>, Props) of
        undefined -> Result;
        Access -> [{<<"_access">>, Access} | Result]
    end.

map_docs(Parent, #mrst{db_name = DbName, idx_name = IdxName} = State0) ->
    erlang:put(io_priority, {view_update, DbName, IdxName}),
    case couch_work_queue:dequeue(State0#mrst.doc_queue) of
        closed ->
            couch_query_servers:stop_doc_map(State0#mrst.qserver),
            couch_work_queue:close(State0#mrst.write_queue);
        {ok, Dequeued} ->
            % Run all the non deleted docs through the view engine and
            % then pass the results on to the writer process.
            State1 =
                case State0#mrst.qserver of
                    nil -> start_query_server(State0);
                    _ -> State0
                end,
            QServer = State1#mrst.qserver,
            DocFun = fun
                ({nil, Seq, _}, {SeqAcc, Results}) ->
                    {erlang:max(Seq, SeqAcc), Results};
               ({Id, Seq, Rev, #doc{deleted=true, body=Body, meta=Meta}}, {SeqAcc, Results}) ->
                   % _access needs deleted docs
                   case IdxName of
                       <<"_design/_access">> ->
                           % splice in seq
                           {Start, Rev1} = Rev,
                           Doc = #doc{
                               id = Id,
                               revs = {Start, [Rev1]},
                               body = {make_deleted_body(Body, Meta, Seq)}, %% todo: only keep _access and add _seq
                               deleted = true
                           },
                           {ok, Res} = couch_query_servers:map_doc_raw(QServer, Doc),
                           {erlang:max(Seq, SeqAcc), [{Id, Seq, Rev, Res} | Results]};
                       _Else ->
                           {erlang:max(Seq, SeqAcc), [{Id, Seq, Rev, []} | Results]}
                       end;
                ({Id, Seq, Doc}, {SeqAcc, Results}) ->
                    couch_stats:increment_counter([couchdb, mrview, map_doc]),
                    % IdxName: ~p, Doc: ~p~n~n", [IdxName, Doc]),
                    Doc0 = case IdxName of
                        <<"_design/_access">> ->
                            % splice in seq
                            {Props} = Doc#doc.body,
                            BodySp = couch_util:get_value(body_sp, Doc#doc.meta),
                            Doc#doc{
                                body = {Props++[{<<"_seq">>, Seq}, {<<"_body_sp">>, BodySp}]}
                            };
                        _Else ->
                            Doc
                        end,
                    {ok, Res} = couch_query_servers:map_doc_raw(QServer, Doc0),
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

write_results(Parent, #mrst{} = State) ->
    case accumulate_writes(State, State#mrst.write_queue, nil) of
        stop ->
            Parent ! {new_state, State};
        {Go, {Seq, ViewKVs, DocIdKeys}} ->
            NewState = write_kvs(State, Seq, ViewKVs, DocIdKeys),
            if
                Go == stop ->
                    Parent ! {new_state, NewState};
                true ->
                    send_partial(NewState#mrst.partial_resp_pid, NewState),
                    write_results(Parent, NewState)
            end
    end.

start_query_server(State) ->
    #mrst{
        language = Language,
        lib = Lib,
        views = Views
    } = State,
    Defs = [View#mrview.def || View <- Views],
    {ok, QServer} = couch_query_servers:start_doc_map(Language, Defs, Lib),
    State#mrst{qserver = QServer}.

accumulate_writes(State, W, Acc0) ->
    {Seq, ViewKVs, DocIdKVs} =
        case Acc0 of
            nil -> {0, [{V#mrview.id_num, []} || V <- State#mrst.views], []};
            _ -> Acc0
        end,
    case couch_work_queue:dequeue(W) of
        closed when Seq == 0 ->
            stop;
        closed ->
            {stop, {Seq, ViewKVs, DocIdKVs}};
        {ok, Info} ->
            {_, _, NewIds} = Acc = merge_results(Info, Seq, ViewKVs, DocIdKVs),
            case accumulate_more(length(NewIds), Acc) of
                true -> accumulate_writes(State, W, Acc);
                false -> {ok, Acc}
            end
    end.

accumulate_more(NumDocIds, Acc) ->
    % check if we have enough items now
    MinItems = config:get("view_updater", "min_writer_items", "100"),
    MinSize = config:get("view_updater", "min_writer_size", "16777216"),
    CurrMem = ?term_size(Acc),
    NumDocIds < list_to_integer(MinItems) andalso
        CurrMem < list_to_integer(MinSize).

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
    case lists:flatten(Results) of
        [] ->
            {ViewKVs, [{DocId, []} | DocIdKeys]};
        _ ->
            {ViewKVs1, ViewIdKeys} = insert_results(DocId, Results, ViewKVs, [], []),
            {ViewKVs1, [ViewIdKeys | DocIdKeys]}
    end.

insert_results(DocId, [], [], ViewKVs, ViewIdKeys) ->
    {lists:reverse(ViewKVs), {DocId, ViewIdKeys}};
insert_results(DocId, [KVs | RKVs], [{Id, VKVs} | RVKVs], VKVAcc, VIdKeys) ->
    CombineDupesFun = fun
        ({Key, Val}, {[{Key, {dups, Vals}} | Rest], IdKeys}) ->
            {[{Key, {dups, [Val | Vals]}} | Rest], IdKeys};
        ({Key, Val1}, {[{Key, Val2} | Rest], IdKeys}) ->
            {[{Key, {dups, [Val1, Val2]}} | Rest], IdKeys};
        ({Key, Value}, {Rest, IdKeys}) ->
            {[{Key, Value} | Rest], [{Id, Key} | IdKeys]}
    end,
    InitAcc = {[], VIdKeys},
    couch_stats:increment_counter([couchdb, mrview, emits], length(KVs)),
    {Duped, VIdKeys0} = lists:foldl(
        CombineDupesFun,
        InitAcc,
        lists:sort(KVs)
    ),
    FinalKVs = [{{Key, DocId}, Val} || {Key, Val} <- Duped] ++ VKVs,
    insert_results(DocId, RKVs, RVKVs, [{Id, FinalKVs} | VKVAcc], VIdKeys0).

write_kvs(State, UpdateSeq, ViewKVs, DocIdKeys) ->
    #mrst{
        id_btree = IdBtree,
        first_build = FirstBuild,
        partitioned = Partitioned
    } = State,

    {ok, ToRemove, IdBtree2} = update_id_btree(IdBtree, DocIdKeys, FirstBuild),
    ToRemByView = collapse_rem_keys(ToRemove, dict:new()),

    UpdateView = fun(#mrview{id_num = ViewId} = View, {ViewId, KVs0}) ->
        ToRem0 = couch_util:dict_find(ViewId, ToRemByView, []),
        {KVs, ToRem} =
            case Partitioned of
                true ->
                    KVs1 = inject_partition(KVs0),
                    ToRem1 = inject_partition(ToRem0),
                    {KVs1, ToRem1};
                false ->
                    {KVs0, ToRem0}
            end,
        {ok, VBtree2} = couch_btree:add_remove(View#mrview.btree, KVs, ToRem),
        NewUpdateSeq =
            case VBtree2 =/= View#mrview.btree of
                true -> UpdateSeq;
                _ -> View#mrview.update_seq
            end,

        View2 = View#mrview{btree = VBtree2, update_seq = NewUpdateSeq},
        maybe_notify(State, View2, KVs, ToRem),
        View2
    end,

    State#mrst{
        views = lists:zipwith(UpdateView, State#mrst.views, ViewKVs),
        update_seq = UpdateSeq,
        id_btree = IdBtree2
    }.

inject_partition(Rows) ->
    lists:map(
        fun
            ({{Key, DocId}, Value}) ->
                % Adding a row to the view
                {Partition, _} = couch_partition:extract(DocId),
                {{{p, Partition, Key}, DocId}, Value};
            ({Key, DocId}) ->
                % Removing a row based on values in id_tree
                {Partition, _} = couch_partition:extract(DocId),
                {{p, Partition, Key}, DocId}
        end,
        Rows
    ).

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
    NewAcc = lists:foldl(
        fun({ViewId, Key}, Acc2) ->
            dict:append(ViewId, {Key, DocId}, Acc2)
        end,
        Acc,
        ViewIdKeys
    ),
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
    Progress =
        case Total of
            0 ->
                % updater restart after compaction finishes
                0;
            _ ->
                (Changes2 * 100) div Total
        end,
    couch_task_status:update([{progress, Progress}, {changes_done, Changes2}]).

maybe_notify(State, View, KVs, ToRem) ->
    Updated = fun() ->
        [Key || {{Key, _}, _} <- KVs]
    end,
    Removed = fun() ->
        [Key || {Key, _DocId} <- ToRem]
    end,
    couch_index_plugin:index_update(State, View, Updated, Removed).
