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

-module(couch_views_trees).

-export([
    open/2,

    get_row_count/2,
    get_kv_size/2,

    fold_map_idx/5,

    update_views/3
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.


-include("couch_views.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


open(TxDb, Mrst) ->
    #mrst{
        sig = Sig,
        language = Lang,
        views = Views
    } = Mrst,
    Mrst#mrst{
        id_btree = open_id_tree(TxDb, Sig),
        views = [open_view_tree(TxDb, Sig, Lang, V) || V <- Views]
    }.


get_row_count(TxDb, View) ->
    #{
        tx := Tx
    } = TxDb,
    {Count, _} = ebtree:full_reduce(Tx, View#mrview.btree),
    Count.


get_kv_size(TxDb, View) ->
    #{
        tx := Tx
    } = TxDb,
    {_, TotalSize} = ebtree:full_reduce(Tx, View#mrview.btree),
    TotalSize.


fold_map_idx(TxDb, View, Options, Callback, Acc0) ->
    #{
        tx := Tx
    } = TxDb,
    #mrview{
        btree = Btree
    } = View,

    CollateFun = couch_views_util:collate_fun(View),

    {Dir, StartKey, EndKey, InclusiveEnd} = to_map_opts(Options),

    Wrapper = fun(KVs0, WAcc) ->
        % Remove any keys that match Start or End key
        % depending on direction
        KVs1 = case InclusiveEnd of
            true ->
                KVs0;
            false when Dir == fwd ->
                lists:filter(fun({K, _V}) ->
                    case CollateFun(K, EndKey) of
                        lt -> true;
                        eq -> false;
                        gt -> false
                    end
                end, KVs0);
            false when Dir == rev ->
                lists:filter(fun({K, _V}) ->
                    case CollateFun(K, EndKey) of
                        lt -> false;
                        eq -> false;
                        gt -> true
                    end
                end, KVs0)
        end,
        % Expand dups
        KVs2 = lists:flatmap(fun({K, V}) ->
            case V of
                {dups, Dups} when Dir == fwd ->
                    [{K, D} || D <- Dups];
                {dups, Dups} when Dir == rev ->
                    [{K, D} || D <- lists:reverse(Dups)];
                _ ->
                    [{K, V}]
            end
        end, KVs1),
        lists:foldl(fun({{Key, DocId}, Value}, WAccInner) ->
            Callback(DocId, Key, Value, WAccInner)
        end, WAcc, KVs2)
    end,

    case Dir of
        fwd ->
            ebtree:range(Tx, Btree, StartKey, EndKey, Wrapper, Acc0);
        rev ->
            % Start/End keys swapped on purpose because ebtree
            ebtree:reverse_range(Tx, Btree, EndKey, StartKey, Wrapper, Acc0)
    end.


update_views(TxDb, Mrst, Docs) ->
    #{
        tx := Tx
    } = TxDb,

    % Collect update information
    #{
        ids := IdMap,
        views := ViewMaps,
        delete_ref := DeleteRef
    } = gather_update_info(Tx, Mrst, Docs),

    % Update the IdBtree
    update_btree(Tx, Mrst#mrst.id_btree, IdMap, DeleteRef),

    % Update each view's BTree
    lists:foreach(fun(View) ->
        #mrview{
            id_num = ViewId,
            btree = BTree
        } = View,

        ViewMap = maps:get(ViewId, ViewMaps, #{}),
        update_btree(Tx, BTree, ViewMap, DeleteRef)
    end, Mrst#mrst.views).


open_id_tree(TxDb, Sig) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    Prefix = id_tree_prefix(DbPrefix, Sig),
    TreeOpts = [
        {persist_fun, fun couch_views_fdb:persist_chunks/3},
        {cache_fun, create_cache_fun(id_tree)}
    ],
    ebtree:open(Tx, Prefix, get_order(id_btree), TreeOpts).


open_view_tree(TxDb, Sig, Lang, View) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    #mrview{
        id_num = ViewId
    } = View,
    Prefix = view_tree_prefix(DbPrefix, Sig, ViewId),
    TreeOpts = [
        {collate_fun, couch_views_util:collate_fun(View)},
        {reduce_fun, make_reduce_fun(Lang, View)},
        {persist_fun, fun couch_views_fdb:persist_chunks/3},
        {cache_fun, create_cache_fun({view, ViewId})}
    ],
    View#mrview{
        btree = ebtree:open(Tx, Prefix, get_order(view_btree), TreeOpts)
    }.


get_order(id_btree) ->
    min_order(config:get_integer("couch_views", "id_btree_node_size", 100));
get_order(view_btree) ->
    min_order(config:get_integer("couch_views", "view_btree_node_size", 100)).


min_order(V) when is_integer(V), V < 2 ->
    2;
min_order(V) when is_integer(V), V rem 2 == 0 ->
    V;
min_order(V) ->
    V + 1.


make_reduce_fun(_Lang, #mrview{}) ->
    fun
        (KVs, _ReReduce = false) ->
            TotalSize = lists:foldl(fun({{K, _DocId}, V}, Acc) ->
                KSize = couch_ejson_size:encoded_size(K),
                VSize = case V of
                    {dups, Dups} ->
                        lists:foldl(fun(D, DAcc) ->
                            DAcc + couch_ejson_size:encoded_size(D)
                        end, 0, Dups);
                    _ ->
                        couch_ejson_size:encoded_size(V)
                end,
                KSize + VSize + Acc
            end, 0, KVs),
            {length(KVs), TotalSize};
        (KRs, _ReReduce = true) ->
            lists:foldl(fun({Count, Size}, {CountAcc, SizeAcc}) ->
                {Count + CountAcc, Size + SizeAcc}
            end, {0, 0}, KRs)
    end.


create_cache_fun(TreeId) ->
    CacheTid = case get(TreeId) of
        undefined ->
            Tid = ets:new(?MODULE, [protected, set]),
            put(TreeId, {ebtree_cache, Tid}),
            Tid;
        {ebtree_cache, Tid} ->
            Tid
    end,
    fun
        (set, [Id, Node]) ->
            true = ets:insert_new(CacheTid, {Id, Node}),
            ok;
        (clear, Id) ->
            ets:delete(CacheTid, Id),
            ok;
        (get, Id) ->
            case ets:lookup(CacheTid, Id) of
                [{Id, Node}] -> Node;
                [] -> undefined
            end
    end.


to_map_opts(Options) ->
    Dir = case lists:keyfind(dir, 1, Options) of
        {dir, D} -> D;
        _ -> fwd
    end,

    InclusiveEnd = case lists:keyfind(inclusive_end, 1, Options) of
        {inclusive_end, IE} -> IE;
        _ -> true
    end,

    StartKey = case lists:keyfind(start_key, 1, Options) of
        {start_key, SK} -> SK;
        false when Dir == fwd -> ebtree:min();
        false when Dir == rev -> ebtree:max()
    end,

    EndKey = case lists:keyfind(end_key, 1, Options) of
        {end_key, EK} -> EK;
        false when Dir == fwd -> ebtree:max();
        false when Dir == rev -> ebtree:min()
    end,

    {Dir, StartKey, EndKey, InclusiveEnd}.


gather_update_info(Tx, Mrst, Docs) ->
    % A special token used to indicate that the row should be deleted
    DeleteRef = erlang:make_ref(),

    AllDocIds = [DocId || #{id := DocId} <- Docs],

    BaseIdMap = lists:foldl(fun(DocId, Acc) ->
        maps:put(DocId, DeleteRef, Acc)
    end, #{}, AllDocIds),

    % Build the initial set of rows to delete
    % ExistingViewKeys is a list of {DocId, [{ViewId, [Key | _]} | _]}
    ExistingViewKeys = ebtree:lookup_multi(Tx, Mrst#mrst.id_btree, AllDocIds),

    % For each view, create an initial map that contains the
    % list of keys to delete. The final result is a map of
    % maps:
    %  #{ViewId => #{Key => DeleteRef}}
    BaseViewMaps = lists:foldl(fun({DocId, ViewIdKeys}, ViewIdAcc1) ->
        lists:foldl(fun({ViewId, Keys}, ViewIdAcc2) ->
            OldViewMap = maps:get(ViewId, ViewIdAcc2, #{}),
            NewViewMap = lists:foldl(fun(Key, ViewMapAcc) ->
                maps:put({Key, DocId}, DeleteRef, ViewMapAcc)
            end, OldViewMap, Keys),
            maps:put(ViewId, NewViewMap, ViewIdAcc2)
        end, ViewIdAcc1, ViewIdKeys)
    end, #{}, ExistingViewKeys),

    % Build our base accumulator
    InfoAcc1 = #{
        ids => BaseIdMap,
        views => BaseViewMaps,
        delete_ref => DeleteRef
    },

    % Insert results from each document into the map of
    % maps which leaves us with a final shape of:
    %   #{ViewId => #{Key => Value}}
    % where Value may be a copy of `DeleteRef` which flags
    % that the Key should be deleted from the view.
    lists:foldl(fun(Doc, InfoAcc2) ->
        insert_doc(Mrst, Doc, InfoAcc2)
    end, InfoAcc1, Docs).


insert_doc(_Mrst, #{deleted := true} = _Doc, InfoAcc) ->
    InfoAcc;
insert_doc(Mrst, Doc, InfoAcc0) ->
    #{
        id := DocId,
        results := Results
    } = Doc,

    FinalAcc = lists:foldl(fun({View, RawNewRows}, {IdKeyAcc, InfoAcc1}) ->
        #mrview{
            id_num = ViewId
        } = View,
        #{
            views := ViewMaps
        } = InfoAcc1,

        DedupedRows = dedupe_rows(View, RawNewRows),
        IdKeys = lists:usort([K || {K, _V} <- DedupedRows]),

        OldViewMap = maps:get(ViewId, ViewMaps, #{}),
        NewViewMap = lists:foldl(fun({K, V}, ViewMapAcc) ->
            maps:put({K, DocId}, V, ViewMapAcc)
        end, OldViewMap, DedupedRows),

        {[{ViewId, IdKeys} | IdKeyAcc], InfoAcc1#{
            views := maps:put(ViewId, NewViewMap, ViewMaps)
        }}
    end, {[], InfoAcc0}, lists:zip(Mrst#mrst.views, Results)),

    {IdRows, #{ids := IdMap} = InfoAcc2} = FinalAcc,

    % Don't store a row in the id_btree if it hasn't got any
    % keys that will need to be deleted.
    NonEmptyRows = [1 || {_ViewId, Rows} <- IdRows, Rows /= []],
    if length(NonEmptyRows) == 0 -> InfoAcc2; true ->
        InfoAcc2#{ids := maps:put(DocId, IdRows, IdMap)}
    end.


update_btree(Tx, BTree, Map, DeleteRef) ->
    {ToRemove, ToInsert} = maps:fold(fun(Key, Value, {Keys, Rows}) ->
        case Value of
            DeleteRef -> {[Key | Keys], Rows};
            _ -> {Keys, [{Key, Value} | Rows]}
        end
    end, {[], []}, Map),

    lists:foreach(fun(Key) ->
        ebtree:delete(Tx, BTree, Key)
    end, ToRemove),

    ebtree:insert_multi(Tx, BTree, ToInsert).


dedupe_rows(View, KVs0) ->
    CollateFun = couch_views_util:collate_fun(View),
    KVs1 = lists:sort(fun({KeyA, ValA}, {KeyB, ValB}) ->
        case CollateFun({KeyA, <<>>}, {KeyB, <<>>}) of
            lt -> true;
            eq -> ValA =< ValB;
            gt -> false
        end
    end, KVs0),
    dedupe_rows_int(CollateFun, KVs1).


dedupe_rows_int(_CollateFun, []) ->
    [];

dedupe_rows_int(_CollateFun, [KV]) ->
    [KV];

dedupe_rows_int(CollateFun, [{K1, V1} | RestKVs]) ->
    RestDeduped = dedupe_rows_int(CollateFun, RestKVs),
    case RestDeduped of
        [{K2, V2} | RestRestDeduped] ->
            case CollateFun({K1, <<>>}, {K2, <<>>}) of
                eq -> [{K1, combine_vals(V1, V2)} | RestRestDeduped];
                _ -> [{K1, V1} | RestDeduped]
            end;
        [] ->
            [{K1, V1}]
    end.


combine_vals(V1, {dups, V2}) ->
    {dups, [V1 | V2]};
combine_vals(V1, V2) ->
    {dups, [V1, V2]}.


id_tree_prefix(DbPrefix, Sig) ->
    Key = {?DB_VIEWS, ?VIEW_TREES, Sig, ?VIEW_ID_TREE},
    erlfdb_tuple:pack(Key, DbPrefix).


view_tree_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_TREES, Sig, ?VIEW_ROW_TREES, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dedupe_basic_test() ->
    View = #mrview{},
    ?assertEqual([{1, 1}], dedupe_rows(View, [{1, 1}])).

dedupe_simple_test() ->
    View = #mrview{},
    ?assertEqual([{1, {dups, [1, 2]}}], dedupe_rows(View, [{1, 1}, {1, 2}])).

-endif.
