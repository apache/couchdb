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

-export([update/4, temp_update/6]).

-include("couch_db.hrl").



update(RootDir, DbName, GroupId, NotifyPid) ->
    {ok, #group{sig=Sig,fd=Fd}=Group} = prepare_group(RootDir, DbName, GroupId),
    {ok, Db} = couch_db:open(DbName, []),
    Result = update_group(Group#group{db=Db}),
    ?LOG_DEBUG("update {Result} DONE ~p", [{Result}]),    
    couch_db:close(Db),
    case Result of
    {same, Group2} ->
        gen_server:cast(NotifyPid, {new_group, Group2});
    {updated, Group2} ->
        HeaderData = {Sig, get_index_header_data(Group2)},
        ok = couch_file:write_header(Fd, <<$r, $c, $k, 0>>, HeaderData),
        gen_server:cast(NotifyPid, {new_group, Group2})
    end,
    garbage_collect().
    
temp_update(DbName, Fd, Lang, MapSrc, RedSrc, NotifyPid) ->
    case couch_db:open(DbName, []) of
    {ok, Db} ->
        View = #view{map_names=["_temp"],
            id_num=0,
            btree=nil,
            def=MapSrc,
            reduce_funs= if RedSrc==[] -> []; true -> [{"_temp", RedSrc}] end},
        Group = #group{name="_temp",
            db=Db,
            views=[View],
            current_seq=0,
            def_lang=Lang,
            id_btree=nil},
        Group2 = init_group(Db, Fd, Group,nil),
        couch_db:monitor(Db),
        {_Updated, Group3} = update_group(Group2#group{db=Db}),
        couch_db:close(Db),
        gen_server:cast(NotifyPid, {new_group, Group3}),
        garbage_collect();
    Else ->
        exit(Else)
    end.


update_group(#group{db=Db,current_seq=CurrentSeq}=Group) ->
    ViewEmptyKVs = [{View, []} || View <- Group#group.views],
    % compute on all docs modified since we last computed.
    {ok, {UncomputedDocs, Group3, ViewKVsToAdd, DocIdViewIdKeys}}
        = couch_db:enum_docs_since(
            Db,
            CurrentSeq,
            fun(DocInfo, _, Acc) -> process_doc(Db, DocInfo, Acc) end,
            {[], Group, ViewEmptyKVs, []}
            ),
    {Group4, Results} = view_compute(Group3, UncomputedDocs),
    {ViewKVsToAdd2, DocIdViewIdKeys2} = view_insert_query_results(UncomputedDocs, Results, ViewKVsToAdd, DocIdViewIdKeys),
    couch_query_servers:stop_doc_map(Group4#group.query_server),
    NewSeq = couch_db:get_update_seq(Db),
    if CurrentSeq /= NewSeq ->
        {ok, Group5} = write_changes(Group4, ViewKVsToAdd2, DocIdViewIdKeys2, NewSeq),
        {updated, Group5#group{query_server=nil}};
    true ->
        {same, Group4#group{query_server=nil}}
    end.


get_index_header_data(#group{current_seq=Seq, purge_seq=PurgeSeq, 
            id_btree=IdBtree,views=Views}) ->
    ViewStates = [couch_btree:get_state(Btree) || #view{btree=Btree} <- Views],
    #index_header{seq=Seq,
            purge_seq=PurgeSeq,
            id_btree_state=couch_btree:get_state(IdBtree),
            view_states=ViewStates}.


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

process_doc(Db, DocInfo, {Docs, #group{sig=Sig,name=GroupId}=Group, ViewKVs, DocIdViewIdKeys}) ->
    % This fun computes once for each document        
    #doc_info{id=DocId, deleted=Deleted} = DocInfo,
    case DocId of
    GroupId ->
        % uh oh. this is the design doc with our definitions. See if
        % anything in the definition changed.
        case couch_db:open_doc(Db, DocInfo) of
        {ok, Doc} ->
            case design_doc_to_view_group(Doc) of
            #group{sig=Sig} ->
                % The same md5 signature, keep on computing
                {ok, {Docs, Group, ViewKVs, DocIdViewIdKeys}};
            _ ->
                ?LOG_DEBUG("throw(restart) md5 broke ~p", [DocId]),
                throw(restart)
            end;
        {not_found, deleted} ->
            ?LOG_DEBUG("throw(restart) {not_found, deleted} ~p", [DocId]),
            throw(restart)
        end;
    <<?DESIGN_DOC_PREFIX, _/binary>> -> % we skip design docs
        {ok, {Docs, Group, ViewKVs, DocIdViewIdKeys}};
    _ ->
        {Docs2, DocIdViewIdKeys2} =
        if Deleted ->
            {Docs, [{DocId, []} | DocIdViewIdKeys]};
        true ->
            {ok, Doc} = couch_db:open_doc(Db, DocInfo, [conflicts, deleted_conflicts]),
            {[Doc | Docs], DocIdViewIdKeys}
        end,
        
        case couch_util:should_flush() of
        true ->
            {Group1, Results} = view_compute(Group, Docs2),
            {ViewKVs3, DocIdViewIdKeys3} = view_insert_query_results(Docs2, Results, ViewKVs, DocIdViewIdKeys2),
            {ok, Group2} = write_changes(Group1, ViewKVs3, DocIdViewIdKeys3,
                    DocInfo#doc_info.update_seq),
            garbage_collect(),
            ViewEmptyKeyValues = [{View, []} || View <- Group2#group.views],
            {ok, {[], Group2, ViewEmptyKeyValues, []}};
        false ->
            {ok, {Docs2, Group, ViewKVs, DocIdViewIdKeys2}}
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
    
prepare_group(RootDir, DbName, GroupId) ->
    {Db, Group} = case (catch couch_db:open(DbName, [])) of
    {ok, Db0} ->
        case (catch couch_db:open_doc(Db0, GroupId)) of
        {ok, Doc} ->
            {Db0, design_doc_to_view_group(Doc)};
        Else ->
            delete_index_file(RootDir, DbName, GroupId),
            ?LOG_DEBUG("prepare_group exit Else ~p self() ~p", [Else, self()]),    
            exit(Else)
        end;
    Else ->
        delete_index_file(RootDir, DbName, GroupId),
        exit(Else)
    end,
    FileName = RootDir ++ "/." ++ binary_to_list(DbName) ++ 
            binary_to_list(GroupId) ++".view",
    Group2 =
    case couch_file:open(FileName) of
    {ok, Fd} ->
        Sig = Group#group.sig,
        case (catch couch_file:read_header(Fd, <<$r, $c, $k, 0>>)) of
        {ok, {Sig, #index_header{purge_seq=PurgeSeq}=HeaderInfo}} ->
            % sigs match!
            DbPurgeSeq = couch_db:get_purge_seq(Db),
            % We can only use index with the same, or next purge seq as the db.
            if DbPurgeSeq == PurgeSeq ->
                init_group(Db, Fd, Group, HeaderInfo);
            DbPurgeSeq == PurgeSeq + 1 ->
                ?LOG_DEBUG("Purging entries from view index.",[]),
                purge_index(init_group(Db, Fd, Group, HeaderInfo));
            true ->
                ?LOG_DEBUG("Reseting view index due to lost purge entries.",[]),
                reset_file(Db, Fd, DbName, Group)
            end;
        _ ->
            reset_file(Db, Fd, DbName, Group)
        end;
    {error, enoent} ->
        case couch_file:open(FileName, [create]) of
        {ok, Fd} -> reset_file(Db, Fd, DbName, Group);
        Error    -> throw(Error)
        end
    end,

    couch_db:monitor(Db),
    couch_db:close(Db),
    {ok, Group2}.

% maybe move to another module
design_doc_to_view_group(#doc{id=Id,body={Fields}}) ->
    Language = proplists:get_value(<<"language">>, Fields, <<"javascript">>),
    {RawViews} = proplists:get_value(<<"views">>, Fields, {[]}),

    % add the views to a dictionary object, with the map source as the key
    DictBySrc =
    lists:foldl(
        fun({Name, {MRFuns}}, DictBySrcAcc) ->
            MapSrc = proplists:get_value(<<"map">>, MRFuns),
            RedSrc = proplists:get_value(<<"reduce">>, MRFuns, null),
            View =
            case dict:find(MapSrc, DictBySrcAcc) of
                {ok, View0} -> View0;
                error -> #view{def=MapSrc} % create new view object
            end,
            View2 =
            if RedSrc == null ->
                View#view{map_names=[Name|View#view.map_names]};
            true ->
                View#view{reduce_funs=[{Name,RedSrc}|View#view.reduce_funs]}
            end,
            dict:store(MapSrc, View2, DictBySrcAcc)
        end, dict:new(), RawViews),
    % number the views
    {Views, _N} = lists:mapfoldl(
        fun({_Src, View}, N) ->
            {View#view{id_num=N},N+1}
        end, 0, dict:to_list(DictBySrc)),

    Group = #group{name=Id, views=Views, def_lang=Language},
    Group#group{sig=erlang:md5(term_to_binary(Group))}.

reset_group(#group{views=Views}=Group) ->
    Views2 = [View#view{btree=nil} || View <- Views],
    Group#group{db=nil,fd=nil,query_server=nil,current_seq=0,
            id_btree=nil,views=Views2}.

reset_file(Db, Fd, DbName, #group{sig=Sig,name=Name} = Group) ->
    ?LOG_DEBUG("Reseting group index \"~s\" in db ~s", [Name, DbName]),
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, <<$r, $c, $k, 0>>, {Sig, nil}),
    init_group(Db, Fd, reset_group(Group), nil).

delete_index_file(RootDir, DbName, GroupId) ->
    file:delete(RootDir ++ "/." ++ binary_to_list(DbName)
            ++ binary_to_list(GroupId) ++ ".view").

init_group(Db, Fd, #group{views=Views}=Group, nil) ->
    init_group(Db, Fd, Group,
        #index_header{seq=0, purge_seq=couch_db:get_purge_seq(Db),
            id_btree_state=nil, view_states=[nil || _ <- Views]});
init_group(Db, Fd, #group{def_lang=Lang,views=Views}=Group, IndexHeader) ->
     #index_header{seq=Seq, purge_seq=PurgeSeq,
            id_btree_state=IdBtreeState, view_states=ViewStates} = IndexHeader,
    {ok, IdBtree} = couch_btree:open(IdBtreeState, Fd),
    Views2 = lists:zipwith(
        fun(BtreeState, #view{reduce_funs=RedFuns}=View) ->
            FunSrcs = [FunSrc || {_Name, FunSrc} <- RedFuns],
            ReduceFun = 
                fun(reduce, KVs) ->
                    KVs2 = couch_view:expand_dups(KVs,[]),
                    KVs3 = couch_view:detuple_kvs(KVs2,[]),
                    {ok, Reduced} = couch_query_servers:reduce(Lang, FunSrcs, 
                        KVs3),
                    {length(KVs3), Reduced};
                (rereduce, Reds) ->
                    Count = lists:sum([Count0 || {Count0, _} <- Reds]),
                    UserReds = [UserRedsList || {_, UserRedsList} <- Reds],
                    {ok, Reduced} = couch_query_servers:rereduce(Lang, FunSrcs,
                        UserReds),
                    {Count, Reduced}
                end,
            {ok, Btree} = couch_btree:open(BtreeState, Fd,
                        [{less, fun couch_view:less_json_keys/2},
                            {reduce, ReduceFun}]),
            View#view{btree=Btree}
        end,
        ViewStates, Views),
    Group#group{db=Db, fd=Fd, current_seq=Seq, purge_seq=PurgeSeq,
        id_btree=IdBtree, views=Views2}.