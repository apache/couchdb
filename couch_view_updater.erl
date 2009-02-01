% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_view_updater).

-export([update/1]).

-include("couch_db.hrl").

update(#group{db=#db{name=DbName}=Db,name=GroupName,current_seq=Seq,purge_seq=PurgeSeq}=Group) ->
    couch_task_status:add_task(<<"View Group Indexer">>, <<DbName/binary," ",GroupName/binary>>, <<"Starting index update">>),
    
    DbPurgeSeq = couch_db:get_purge_seq(Db),
    Group2 =
    if DbPurgeSeq == PurgeSeq ->
        Group;
    DbPurgeSeq == PurgeSeq + 1 ->
        couch_task_status:update(<<"Removing purged entries from view index.">>),
        purge_index(Group);
    true ->
        couch_task_status:update(<<"Resetting view index due to lost purge entries.">>),
        exit(reset)
    end,
    
    ViewEmptyKVs = [{View, []} || View <- Group2#group.views],
    % compute on all docs modified since we last computed.
    TotalChanges = couch_db:count_changes_since(Db, Seq),
    % update status every half second
    couch_task_status:set_update_frequency(500),
    {ok, {_,{UncomputedDocs, Group3, ViewKVsToAdd, DocIdViewIdKeys}}}
        = couch_db:enum_docs_since(
            Db,
            Seq,
            fun(DocInfo, _, {ChangesProcessed, Acc}) ->
                couch_task_status:update("Processed ~p of ~p changes (~p%)",
                        [ChangesProcessed, TotalChanges, (ChangesProcessed*100) div TotalChanges]),
                {ok, {ChangesProcessed+1, process_doc(Db, DocInfo, Acc)}}
            end,
            {0, {[], Group2, ViewEmptyKVs, []}}
            ),
    couch_task_status:set_update_frequency(0),
    couch_task_status:update("Finishing."),
    {Group4, Results} = view_compute(Group3, UncomputedDocs),
    {ViewKVsToAdd2, DocIdViewIdKeys2} = view_insert_query_results(
            UncomputedDocs, Results, ViewKVsToAdd, DocIdViewIdKeys),
    couch_query_servers:stop_doc_map(Group4#group.query_server),
    NewSeq = couch_db:get_update_seq(Db),
    {ok, Group5} = write_changes(Group4, ViewKVsToAdd2, DocIdViewIdKeys2,
                NewSeq),
    exit({new_group, Group5#group{query_server=nil}}).


purge_index(#group{db=Db, views=Views, id_btree=IdBtree}=Group) ->
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
    Views2 = lists:map(
        fun(#view{id_num=Num,btree=Btree}=View) ->
            case dict:find(Num, ViewKeysToRemoveDict) of
            {ok, RemoveKeys} ->
                {ok, Btree2} = couch_btree:add_remove(Btree, [], RemoveKeys),
                View#view{btree=Btree2};
            error -> % no keys to remove in this view
                View
            end
        end, Views),
    Group#group{id_btree=IdBtree2,
            views=Views2,
            purge_seq=couch_db:get_purge_seq(Db)}.

process_doc(Db, DocInfo, {Docs, #group{sig=Sig,name=GroupId,design_options=DesignOptions}=Group, ViewKVs,
        DocIdViewIdKeys}) ->
    % This fun computes once for each document        
    #doc_info{id=DocId, deleted=Deleted} = DocInfo,
    IncludeDesign = proplists:get_value(<<"include_design">>, 
        DesignOptions, false),
    case {IncludeDesign, DocId} of
    {_, GroupId} ->
        % uh oh. this is the design doc with our definitions. See if
        % anything in the definition changed.
        case couch_db:open_doc(Db, DocInfo, [conflicts, deleted_conflicts]) of
        {ok, Doc} ->
            case couch_view_group:design_doc_to_view_group(Doc) of
            #group{sig=Sig} ->
                % The same md5 signature, keep on computing
                case IncludeDesign of
                true ->
                    {[Doc | Docs], Group, ViewKVs, DocIdViewIdKeys};
                _ ->
                    {Docs, Group, ViewKVs, DocIdViewIdKeys}
                end;
            _ ->
                exit(reset)
            end;
        {not_found, deleted} ->
            exit(reset)
        end;
    {false, <<?DESIGN_DOC_PREFIX, _/binary>>} -> % we skip design docs
        {Docs, Group, ViewKVs, DocIdViewIdKeys};
    _ ->
        {Docs2, DocIdViewIdKeys2} =
        if Deleted ->
            {Docs, [{DocId, []} | DocIdViewIdKeys]};
        true ->
            {ok, Doc} = couch_db:open_doc(Db, DocInfo, 
                [conflicts, deleted_conflicts]),
            {[Doc | Docs], DocIdViewIdKeys}
        end,
        
        case couch_util:should_flush() of
        true ->
            {Group1, Results} = view_compute(Group, Docs2),
            {ViewKVs3, DocIdViewIdKeys3} = view_insert_query_results(Docs2, 
                Results, ViewKVs, DocIdViewIdKeys2),
            {ok, Group2} = write_changes(Group1, ViewKVs3, DocIdViewIdKeys3,
                DocInfo#doc_info.update_seq),
            garbage_collect(),
            ViewEmptyKeyValues = [{View, []} || View <- Group2#group.views],
            {[], Group2, ViewEmptyKeyValues, []};
        false ->
            {Docs2, Group, ViewKVs, DocIdViewIdKeys2}
        end
    end.

view_insert_query_results([], [], ViewKVs, DocIdViewIdKeysAcc) ->
    {ViewKVs, DocIdViewIdKeysAcc};
view_insert_query_results([Doc|RestDocs], [QueryResults | RestResults], ViewKVs, DocIdViewIdKeysAcc) ->
    {NewViewKVs, NewViewIdKeys} = view_insert_doc_query_results(Doc, QueryResults, ViewKVs, [], []),
    NewDocIdViewIdKeys = [{Doc#doc.id, NewViewIdKeys} | DocIdViewIdKeysAcc],
    view_insert_query_results(RestDocs, RestResults, NewViewKVs, NewDocIdViewIdKeys).


view_insert_doc_query_results(_Doc, [], [], ViewKVsAcc, ViewIdKeysAcc) ->
    {lists:reverse(ViewKVsAcc), lists:reverse(ViewIdKeysAcc)};
view_insert_doc_query_results(#doc{id=DocId}=Doc, [ResultKVs|RestResults], [{View, KVs}|RestViewKVs], ViewKVsAcc, ViewIdKeysAcc) ->
    % Take any identical keys and combine the values
    ResultKVs2 = lists:foldl(
        fun({Key,Value}, [{PrevKey,PrevVal}|AccRest]) ->
            case Key == PrevKey of
            true ->
                case PrevVal of
                {dups, Dups} ->
                    [{PrevKey, {dups, [Value|Dups]}} | AccRest];
                _ ->
                    [{PrevKey, {dups, [Value,PrevVal]}} | AccRest]
                end;
            false ->
                [{Key,Value},{PrevKey,PrevVal}|AccRest]
            end;
        (KV, []) ->
           [KV] 
        end, [], lists:sort(ResultKVs)),
    NewKVs = [{{Key, DocId}, Value} || {Key, Value} <- ResultKVs2],
    NewViewKVsAcc = [{View, NewKVs ++ KVs} | ViewKVsAcc],
    NewViewIdKeys = [{View#view.id_num, Key} || {Key, _Value} <- ResultKVs2],
    NewViewIdKeysAcc = NewViewIdKeys ++ ViewIdKeysAcc,
    view_insert_doc_query_results(Doc, RestResults, RestViewKVs, NewViewKVsAcc, NewViewIdKeysAcc).

view_compute(Group, []) ->
    {Group, []};
view_compute(#group{def_lang=DefLang, query_server=QueryServerIn}=Group, Docs) ->
    {ok, QueryServer} =
    case QueryServerIn of
    nil -> % doc map not started
        Definitions = [View#view.def || View <- Group#group.views],
        couch_query_servers:start_doc_map(DefLang, Definitions);
    _ ->
        {ok, QueryServerIn}
    end,
    {ok, Results} = couch_query_servers:map_docs(QueryServer, Docs),
    {Group#group{query_server=QueryServer}, Results}.



write_changes(Group, ViewKeyValuesToAdd, DocIdViewIdKeys, NewSeq) ->
    #group{id_btree=IdBtree} = Group,

    AddDocIdViewIdKeys = [{DocId, ViewIdKeys} || {DocId, ViewIdKeys} <- DocIdViewIdKeys, ViewIdKeys /= []],
    RemoveDocIds = [DocId || {DocId, ViewIdKeys} <- DocIdViewIdKeys, ViewIdKeys == []],
    LookupDocIds = [DocId || {DocId, _ViewIdKeys} <- DocIdViewIdKeys],
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

    Views2 = [
        begin
            KeysToRemove = couch_util:dict_find(View#view.id_num, KeysToRemoveByView, []),
            {ok, ViewBtree2} = couch_btree:add_remove(View#view.btree, AddKeyValues, KeysToRemove),
            View#view{btree = ViewBtree2}
        end
    ||
        {View, AddKeyValues} <- ViewKeyValuesToAdd
    ],
    Group2 = Group#group{views=Views2, current_seq=NewSeq, id_btree=IdBtree2},
    {ok, Group2}.

