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

-include("couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([compact/2, swap_compacted/2]).


compact(State, Opts) ->
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
    TaskName = <<DbName/binary, ":", IdxName/binary>>,
    couch_task_status:add_task(<<"View Group Compaction">>, TaskName, <<"">>),

    BufferSize0 = couch_config:get(
        "view_compaction", "keyvalue_buffer_size", "2097152"
    ),
    BufferSize = list_to_integer(BufferSize0),

    FoldFun = fun({DocId, _} = KV, {Bt, Acc, AccSize, Copied, LastId}) ->
        if DocId =:= LastId ->
            % COUCHDB-999 regression test
            ?LOG_ERROR("Duplicate docid `~s` detected in view group `~s`"
                ++ ", database `~s` - This view needs to be rebuilt.",
                [DocId, IdxName, DbName]
            ),
            exit({view_duplicate_id, DocId});
        true -> ok end,
        AccSize2 = AccSize + ?term_size(KV),
        case AccSize2 >= BufferSize of
            true ->
                {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV|Acc])),
                couch_task_status:update("Copied ~p of ~p Ids (~p%)",
                    [Copied, Count, (Copied * 100) div Count]),
                {ok, {Bt2, [], 0, Copied+1+length(Acc), DocId}};
            _ ->
                {ok, {Bt, [KV | Acc], AccSize2, Copied, DocId}}
        end
    end,

    InitAcc = {EmptyIdBtree, [], 0, 0, nil},
    {ok, _, FinalAcc} = couch_btree:foldl(IdBtree, FoldFun, InitAcc),
    {Bt3, Uncopied, _, _, _} = FinalAcc,
    {ok, NewIdBtree} = couch_btree:add(Bt3, lists:reverse(Uncopied)),

    NewViews = lists:map(fun({View, EmptyView}) ->
        compact_view(View, EmptyView, BufferSize)
    end, lists:zip(Views, EmptyViews)),

    unlink(EmptyState#mrst.fd),
    {ok, EmptyState#mrst{
        id_btree=NewIdBtree,
        views=NewViews,
        update_seq=Seq
    }}.


recompact(State) ->
    #mrst{
        db_name=DbName,
        idx_name=IdxName,
        update_seq=UpdateSeq
    } = State,
    link(State#mrst.fd),
    ?LOG_INFO("Recompacting index ~s ~s at ~p", [DbName, IdxName, UpdateSeq]),
    {_Pid, Ref} = erlang:spawn_monitor(fun() ->
        couch_index_updater:update(couch_mrview_index, State)
    end),
    receive
        {'DOWN', Ref, _, _, {updated, State2}} ->
            unlink(State#mrst.fd),
            {ok, State2}
    end.


%% @spec compact_view(View, EmptyView, Retry) -> CompactView
compact_view(View, EmptyView, BufferSize) ->
    {ok, Count} = couch_mrview_util:get_row_count(View),
    Fun = fun(KV, {Bt, Acc, AccSize, Copied}) ->
        AccSize2 = AccSize + ?term_size(KV),
        if AccSize2 >= BufferSize ->
            {ok, Bt2} = couch_btree:add(Bt, lists:reverse([KV|Acc])),
            couch_task_status:update("View #~p: copied ~p of ~p KVs (~p%)",
                [View#mrview.id_num, Copied, Count, (Copied*100) div Count]),
            {ok, {Bt2, [], 0, Copied + 1 + length(Acc)}};
        true ->
            {ok, {Bt, [KV|Acc], AccSize2, Copied}}
        end
    end,

    InitAcc = {EmptyView#mrview.btree, [], 0, 0},
    {ok, _, FinalAcc} = couch_btree:foldl(View#mrview.btree, Fun, InitAcc),
    {Bt3, Uncopied, _, _} = FinalAcc,
    {ok, NewBt} = couch_btree:add(Bt3, lists:reverse(Uncopied)),

    EmptyView#mrview{btree=NewBt}.


swap_compacted(OldState, NewState) ->
    #mrst{
        sig=Sig,
        db_name=DbName,
        idx_name=IdxName
    } = NewState,
    ?LOG_INFO("View index compaction complete for ~s ~s", [DbName, IdxName]),

    link(NewState#mrst.fd),

    RootDir = couch_index_util:root_dir(),
    IndexFName = couch_mrview_util:index_file(DbName, Sig),
    CompactFName = couch_mrview_util:compaction_file(DbName, Sig),
    couch_file:close(OldState#mrst.fd),
    ok = couch_file:delete(RootDir, IndexFName),
    ok = file:rename(CompactFName, IndexFName),

    unlink(OldState#mrst.fd),
    
    {ok, NewState}.


