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

-export([compact/3, swap_compacted/2]).

-record(acc, {
   btree = nil,
   last_id = nil,
   kvs = [],
   kvs_size = 0,
   changes = 0,
   total_changes
}).


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
        views=Views
    } = State,

    EmptyState = couch_util:with_db(DbName, fun(Db) ->
        CompactFName = couch_mrview_util:compaction_file(DbName, Sig),
        {ok, Fd} = couch_mrview_util:open_file(CompactFName),
        couch_mrview_util:reset_index(Db, Fd, State)
    end),

    #mrst{
        id_btree = EmptyIdBtree,
        views = EmptyViews
    } = EmptyState,

    {ok, Count} = couch_btree:full_reduce(IdBtree),
    TotalChanges = lists:foldl(
        fun(View, Acc) ->
            {ok, Kvs} = couch_mrview_util:get_row_count(View),
            Acc + Kvs
        end,
        Count, Views),
    couch_task_status:add_task([
        {type, view_compaction},
        {database, DbName},
        {design_document, IdxName},
        {progress, 0}
    ]),

    BufferSize0 = couch_config:get(
        "view_compaction", "keyvalue_buffer_size", "2097152"
    ),
    BufferSize = list_to_integer(BufferSize0),

    FoldFun = fun({DocId, ViewIdKeys} = KV, Acc) ->
        #acc{btree = Bt, kvs = Kvs, kvs_size = KvsSize} = Acc,
        NewKvs = case Kvs of
            [{DocId, OldViewIdKeys} | Rest] ->
                ?LOG_ERROR("Dupes of ~s in ~s ~s", [DocId, DbName, IdxName]),
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

    {NewViews, _} = lists:mapfoldl(fun({View, EmptyView}, Acc) ->
        compact_view(View, EmptyView, BufferSize, Acc)
    end, FinalAcc2, lists:zip(Views, EmptyViews)),

    unlink(EmptyState#mrst.fd),
    {ok, EmptyState#mrst{
        id_btree=NewIdBtree,
        views=NewViews,
        update_seq=Seq
    }}.


recompact(State) ->
    link(State#mrst.fd),
    {Pid, Ref} = erlang:spawn_monitor(fun() ->
        couch_index_updater:update(couch_mrview_index, State)
    end),
    receive
        {'DOWN', Ref, _, _, {updated, Pid, State2}} ->
            unlink(State#mrst.fd),
            {ok, State2}
    end.


%% @spec compact_view(View, EmptyView, Retry, Acc) -> {CompactView, NewAcc}
compact_view(#mrview{id_num=VID}=View, EmptyView, BufferSize, Acc0) ->
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

    InitAcc = Acc0#acc{kvs = [], kvs_size = 0, btree = EmptyView#mrview.btree},
    {ok, _, FinalAcc} = couch_btree:foldl(View#mrview.btree, Fun, InitAcc),
    #acc{btree = Bt3, kvs = Uncopied} = FinalAcc,
    {ok, NewBt} = couch_btree:add(Bt3, lists:reverse(Uncopied)),
    FinalAcc2 = update_task(VID, FinalAcc, length(Uncopied)),
    {EmptyView#mrview{btree=NewBt}, FinalAcc2}.


update_task(Acc, ChangesInc) ->
    update_task(null, Acc, ChangesInc).


update_task(VID, #acc{changes=Changes, total_changes=Total}=Acc, ChangesInc) ->
    Changes2 = Changes + ChangesInc,
    couch_task_status:update([
        {view, VID},
        {changes_done, Changes2},
        {total_changes, Total},
        {progress, (Changes2 * 100) div Total}
    ]),
    Acc#acc{changes = Changes2}.


swap_compacted(OldState, NewState) ->
    #mrst{
        sig=Sig,
        db_name=DbName
    } = NewState,

    link(NewState#mrst.fd),

    RootDir = couch_index_util:root_dir(),
    IndexFName = couch_mrview_util:index_file(DbName, Sig),
    CompactFName = couch_mrview_util:compaction_file(DbName, Sig),
    ok = couch_file:delete(RootDir, IndexFName),
    ok = file:rename(CompactFName, IndexFName),

    unlink(OldState#mrst.fd),
    erlang:demonitor(OldState#mrst.fd_monitor, [flush]),
    
    {ok, NewState}.
