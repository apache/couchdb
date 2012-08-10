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

-module(couch_view_updater).

-export([update/3]).

-include("couch_db.hrl").

-spec update(_, #group{}, Dbname::binary()) -> no_return().

update(Owner, Group, DbName) when is_binary(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, [
        % allow reading docs from system dbs
        {user_ctx, #user_ctx{roles=[<<"_admin">>]}
    }]),
    try
        update(Owner, Group, Db)
    after
        couch_db:close(Db)
    end;

update(Owner, Group, #db{name = DbName} = Db) ->
    #group{
        name = GroupName,
        current_seq = Seq,
        purge_seq = PurgeSeq
    } = Group,

    DbPurgeSeq = couch_db:get_purge_seq(Db),
    if DbPurgeSeq == PurgeSeq ->
        ok;
    DbPurgeSeq == PurgeSeq + 1 ->
        ok;
    true ->
        exit(reset)
    end,
    {ok, MapQueue} = couch_work_queue:new(
        [{max_size, 100000}, {max_items, 500}]),
    {ok, WriteQueue} = couch_work_queue:new(
        [{max_size, 100000}, {max_items, 500}]),
    Self = self(),
    spawn_link(fun() ->
        do_maps(add_query_server(Group), MapQueue, WriteQueue)
    end),
    TotalChanges = couch_db:count_changes_since(Db, Seq),
    spawn_link(fun() ->
        couch_task_status:add_task([
            {type, indexer},
            {database, DbName},
            {design_document, GroupName},
            {progress, 0},
            {changes_done, 0},
            {total_changes, TotalChanges}
        ]),
        couch_task_status:set_update_frequency(500),
        Group2 =
        if DbPurgeSeq == PurgeSeq + 1 ->
            purge_index(Group, Db);
        true ->
            Group
        end,
        ViewEmptyKVs = [{View, []} || View <- Group2#group.views],
        do_writes(Self, Owner, Group2, WriteQueue, Seq == 0, ViewEmptyKVs)
    end),
    % compute on all docs modified since we last computed.
    #group{ design_options = DesignOptions } = Group,
    IncludeDesign = couch_util:get_value(<<"include_design">>,
        DesignOptions, false),
    LocalSeq = couch_util:get_value(<<"local_seq">>, DesignOptions, false),
    DocOpts =
    case LocalSeq of
    true -> [conflicts, deleted_conflicts, local_seq];
    _ -> [conflicts, deleted_conflicts]
    end,
    {ok, _, _}
        = couch_db:enum_docs_since(
            Db,
            Seq,
            fun(DocInfo, _, Acc) ->
                load_doc(Db, DocInfo, MapQueue, DocOpts, IncludeDesign),
                {ok, Acc}
            end,
            ok, []),
    couch_work_queue:close(MapQueue),
    receive {new_group, NewGroup0} ->
        NewGroup = NewGroup0#group{
            current_seq=couch_db:get_update_seq(Db)
        },
        gen_server:cast(Owner, {self(), new_group, NewGroup})
    end.


add_query_server(#group{query_server = nil} = Group) ->
    {ok, Qs} = couch_query_servers:start_doc_map(
        Group#group.def_lang,
        [View#view.def || View <- Group#group.views],
        Group#group.lib),
    Group#group{query_server = Qs};
add_query_server(Group) ->
    Group.


purge_index(#group{views=Views, id_btree=IdBtree}=Group, Db) ->
    {ok, PurgedIdsRevs} = couch_db:get_last_purged(Db),
    Ids = [Id || {Id, _Revs} <- PurgedIdsRevs],
    {ok, Lookups, IdBtree2} = couch_btree:query_modify(IdBtree, Ids, [], Ids),

    % now populate the dictionary with all the keys to delete
    ViewKeysToRemoveDict = lists:foldl(
        fun({ok,{DocId,ViewNumRowKeys}}, ViewDictAcc) ->
            lists:foldl(
                fun({ViewNum, RowKey}, ViewDictAcc2) ->
                    dict:append(ViewNum, {RowKey, DocId}, ViewDictAcc2)
                end, ViewDictAcc, ViewNumRowKeys);
        ({not_found, _}, ViewDictAcc) ->
            ViewDictAcc
        end, dict:new(), Lookups),

    % Now remove the values from the btrees
    PurgeSeq = couch_db:get_purge_seq(Db),
    Views2 = lists:map(
        fun(#view{id_num=Num,btree=Btree}=View) ->
            case dict:find(Num, ViewKeysToRemoveDict) of
            {ok, RemoveKeys} ->
                {ok, ViewBtree2} = couch_btree:add_remove(Btree, [], RemoveKeys),
                case ViewBtree2 =/= Btree of
                    true ->
                        View#view{btree=ViewBtree2, purge_seq=PurgeSeq};
                    _ ->
                        View#view{btree=ViewBtree2}
                end;
            error -> % no keys to remove in this view
                View
            end
        end, Views),
    Group#group{id_btree=IdBtree2,
            views=Views2,
            purge_seq=PurgeSeq}.


load_doc(Db, DocInfo, MapQueue, DocOpts, IncludeDesign) ->
    #doc_info{id=DocId, high_seq=Seq, revs=[#rev_info{deleted=Deleted}|_]} = DocInfo,
    case {IncludeDesign, DocId} of
    {false, <<?DESIGN_DOC_PREFIX, _/binary>>} -> % we skip design docs
        ok;
    _ ->
        if Deleted ->
            couch_work_queue:queue(MapQueue, {Seq, #doc{id=DocId, deleted=true}});
        true ->
            {ok, Doc} = couch_db:open_doc_int(Db, DocInfo, DocOpts),
            couch_work_queue:queue(MapQueue, {Seq, Doc})
        end
    end.

do_maps(#group{query_server = Qs} = Group, MapQueue, WriteQueue) ->
    case couch_work_queue:dequeue(MapQueue) of
    closed ->
        couch_work_queue:close(WriteQueue),
        couch_query_servers:stop_doc_map(Group#group.query_server);
    {ok, Queue} ->
        Items = lists:foldr(
            fun({Seq, #doc{id = Id, deleted = true}}, Acc) ->
                Item = {Seq, Id, []},
                [Item | Acc];
            ({Seq, #doc{id = Id, deleted = false} = Doc}, Acc) ->
                {ok, Result} = couch_query_servers:map_doc_raw(Qs, Doc),
                Item = {Seq, Id, Result},
                [Item | Acc]
            end,
            [], Queue),
        ok = couch_work_queue:queue(WriteQueue, Items),
        do_maps(Group, MapQueue, WriteQueue)
    end.

do_writes(Parent, Owner, Group, WriteQueue, InitialBuild, ViewEmptyKVs) ->
    case couch_work_queue:dequeue(WriteQueue) of
    closed ->
        Parent ! {new_group, Group};
    {ok, Queue0} ->
        Queue = lists:flatten(Queue0),
        {ViewKVs, DocIdViewIdKeys} = lists:foldr(
            fun({_Seq, Id, []}, {ViewKVsAcc, DocIdViewIdKeysAcc}) ->
                {ViewKVsAcc, [{Id, []} | DocIdViewIdKeysAcc]};
            ({_Seq, Id, RawQueryResults}, {ViewKVsAcc, DocIdViewIdKeysAcc}) ->
                QueryResults = [
                    [list_to_tuple(FunResult) || FunResult <- FunRs] || FunRs <-
                        couch_query_servers:raw_to_ejson(RawQueryResults)
                ],
                {NewViewKVs, NewViewIdKeys} = view_insert_doc_query_results(
                        Id, QueryResults, ViewKVsAcc, [], []),
                {NewViewKVs, [{Id, NewViewIdKeys} | DocIdViewIdKeysAcc]}
            end,
            {ViewEmptyKVs, []}, Queue),
        {NewSeq, _, _} = lists:last(Queue),
        Group2 = write_changes(
            Group, ViewKVs, DocIdViewIdKeys, NewSeq, InitialBuild),
        case Owner of
        nil ->
            ok;
        _ ->
            ok = gen_server:cast(Owner, {partial_update, Parent, Group2})
        end,
        update_task(length(Queue)),
        do_writes(Parent, Owner, Group2, WriteQueue, InitialBuild, ViewEmptyKVs)
    end.


view_insert_doc_query_results(_DocId, [], [], ViewKVsAcc, ViewIdKeysAcc) ->
    {lists:reverse(ViewKVsAcc), lists:reverse(ViewIdKeysAcc)};
view_insert_doc_query_results(DocId, [ResultKVs | RestResults],
        [{View, KVs} | RestViewKVs], ViewKVsAcc, ViewIdKeysAcc) ->
    % Take any identical keys and combine the values
    {NewKVs, NewViewIdKeys} = lists:foldl(
        fun({Key, Val}, {[{{Key, _DocId} = Kd, PrevVal} | AccRest], AccVid}) ->
            AccKv2 = case PrevVal of
            {dups, Dups} ->
                [{Kd, {dups, [Val | Dups]}} | AccRest];
            _ ->
                [{Kd, {dups, [Val, PrevVal]}} | AccRest]
            end,
            {AccKv2, [{View#view.id_num, Key} | AccVid]};
        ({Key, Val}, {AccKv, AccVid}) ->
            {[{{Key, DocId}, Val} | AccKv], [{View#view.id_num, Key} | AccVid]}
        end,
        {[], []}, lists:sort(ResultKVs)),
    NewViewKVsAcc = [{View, NewKVs ++ KVs} | ViewKVsAcc],
    NewViewIdKeysAcc = NewViewIdKeys ++ ViewIdKeysAcc,
    view_insert_doc_query_results(
        DocId, RestResults, RestViewKVs, NewViewKVsAcc, NewViewIdKeysAcc).


write_changes(Group, ViewKeyValuesToAdd, DocIdViewIdKeys, NewSeq, InitialBuild) ->
    #group{id_btree=IdBtree} = Group,

    AddDocIdViewIdKeys = [{DocId, ViewIdKeys} || {DocId, ViewIdKeys} <- DocIdViewIdKeys, ViewIdKeys /= []],
    if InitialBuild ->
        RemoveDocIds = [],
        LookupDocIds = [];
    true ->
        RemoveDocIds = [DocId || {DocId, ViewIdKeys} <- DocIdViewIdKeys, ViewIdKeys == []],
        LookupDocIds = [DocId || {DocId, _ViewIdKeys} <- DocIdViewIdKeys]
    end,
    {ok, LookupResults, IdBtree2}
        = couch_btree:query_modify(IdBtree, LookupDocIds, AddDocIdViewIdKeys, RemoveDocIds),
    KeysToRemoveByView = lists:foldl(
        fun(LookupResult, KeysToRemoveByViewAcc) ->
            case LookupResult of
            {ok, {DocId, ViewIdKeys}} ->
                lists:foldl(
                    fun({ViewId, Key}, KeysToRemoveByViewAcc2) ->
                        dict:append(ViewId, {Key, DocId}, KeysToRemoveByViewAcc2)
                    end,
                    KeysToRemoveByViewAcc, ViewIdKeys);
            {not_found, _} ->
                KeysToRemoveByViewAcc
            end
        end,
        dict:new(), LookupResults),
    Views2 = lists:zipwith(fun(View, {_View, AddKeyValues}) ->
            KeysToRemove = couch_util:dict_find(View#view.id_num, KeysToRemoveByView, []),
            {ok, ViewBtree2} = couch_btree:add_remove(View#view.btree, AddKeyValues, KeysToRemove),
            case ViewBtree2 =/= View#view.btree of
                true ->
                    View#view{btree=ViewBtree2, update_seq=NewSeq};
                _ ->
                    View#view{btree=ViewBtree2}
            end
        end,    Group#group.views, ViewKeyValuesToAdd),
    Group#group{views=Views2, current_seq=NewSeq, id_btree=IdBtree2}.

update_task(NumChanges) ->
    [Changes, Total] = couch_task_status:get([changes_done, total_changes]),
    Changes2 = Changes + NumChanges,
    Progress = (Changes2 * 100) div Total,
    couch_task_status:update([{progress, Progress}, {changes_done, Changes2}]).
