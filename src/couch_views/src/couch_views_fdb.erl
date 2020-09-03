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

-module(couch_views_fdb).

-export([
    new_interactive_index/3,
    new_creation_vs/3,
    get_creation_vs/2,
    get_build_status/2,
    set_build_status/3,

    get_update_seq/2,
    set_update_seq/3,

    set_trees/2,

    get_row_count/2,
    get_kv_size/2,

    fold_map_idx/5,
    fold_red_idx/6,

    update_views/3,

    list_signatures/1,
    clear_index/2
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-define(LIST_VALUE, 0).
-define(JSON_VALUE, 1).
-define(VALUE, 2).


-include("couch_views.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


new_interactive_index(Db, Mrst, VS) ->
    couch_views_fdb:new_creation_vs(Db, Mrst, VS),
    couch_views_fdb:set_build_status(Db, Mrst, ?INDEX_BUILDING).


%Interactive View Creation Versionstamp
%(<db>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_CREATION_VS, Sig) = VS

new_creation_vs(TxDb, #mrst{} = Mrst, VS) ->
    #{
        tx := Tx
    } = TxDb,
    Key = creation_vs_key(TxDb, Mrst#mrst.sig),
    Value = erlfdb_tuple:pack_vs({VS}),
    ok = erlfdb:set_versionstamped_value(Tx, Key, Value).


get_creation_vs(TxDb, #mrst{} = Mrst) ->
    get_creation_vs(TxDb, Mrst#mrst.sig);

get_creation_vs(TxDb, Sig) ->
    #{
        tx := Tx
    } = TxDb,
    Key = creation_vs_key(TxDb, Sig),
    case erlfdb:wait(erlfdb:get(Tx, Key)) of
        not_found ->
            not_found;
        EK ->
            {VS} = erlfdb_tuple:unpack(EK),
            VS
    end.


%Interactive View Build Status
%(<db>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_BUILD_STATUS, Sig) = INDEX_BUILDING | INDEX_READY

get_build_status(TxDb, #mrst{sig = Sig}) ->
    #{
        tx := Tx
    } = TxDb,
    Key = build_status_key(TxDb, Sig),
    erlfdb:wait(erlfdb:get(Tx, Key)).


set_build_status(TxDb, #mrst{sig = Sig}, State) ->
    #{
        tx := Tx
    } = TxDb,

    Key = build_status_key(TxDb, Sig),
    ok = erlfdb:set(Tx, Key, State).


% View Build Sequence Access
% (<db>, ?DB_VIEWS, Sig, ?VIEW_UPDATE_SEQ) = Sequence


get_update_seq(TxDb, #mrst{sig = Sig}) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    case erlfdb:wait(erlfdb:get(Tx, seq_key(DbPrefix, Sig))) of
        not_found -> <<>>;
        UpdateSeq -> UpdateSeq
    end.


set_update_seq(TxDb, Sig, Seq) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    ok = erlfdb:set(Tx, seq_key(DbPrefix, Sig), Seq).


set_trees(TxDb, Mrst) ->
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
    {Count, _, _} = ebtree:full_reduce(Tx, View#mrview.btree),
    Count.


get_kv_size(TxDb, View) ->
    #{
        tx := Tx
    } = TxDb,
    {_, TotalSize, _} = ebtree:full_reduce(Tx, View#mrview.btree),
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


fold_red_idx(TxDb, View, Idx, Options, Callback, Acc0) ->
    #{
        tx := Tx
    } = TxDb,
    #mrview{
        btree = Btree
    } = View,

    {Dir, StartKey, EndKey, InclusiveEnd, GroupKeyFun} = to_red_opts(Options),

    Wrapper = fun({GroupKey, Reduction}, WAcc) ->
        {_RowCount, _RowSize, UserReds} = Reduction,
        RedValue = lists:nth(Idx, UserReds),
        Callback(GroupKey, RedValue, WAcc)
    end,

    case {GroupKeyFun, Dir} of
        {group_all, fwd} ->
            EBtreeOpts = [
                {dir, fwd},
                {inclusive_end, InclusiveEnd}
            ],
            Reduction = ebtree:reduce(Tx, Btree, StartKey, EndKey, EBtreeOpts),
            Wrapper({null, Reduction}, Acc0);
        {F, fwd} when is_function(F) ->
            EBtreeOpts = [
                {dir, fwd},
                {inclusive_end, InclusiveEnd}
            ],
            ebtree:group_reduce(
                    Tx,
                    Btree,
                    StartKey,
                    EndKey,
                    GroupKeyFun,
                    Wrapper,
                    Acc0,
                    EBtreeOpts
                );
        {group_all, rev} ->
            % Start/End keys swapped on purpose because ebtree. Also
            % inclusive_start for same reason.
            EBtreeOpts = [
                {dir, rev},
                {inclusive_start, InclusiveEnd}
            ],
            Reduction = ebtree:reduce(Tx, Btree, EndKey, StartKey, EBtreeOpts),
            Wrapper({null, Reduction}, Acc0);
        {F, rev} when is_function(F) ->
            % Start/End keys swapped on purpose because ebtree. Also
            % inclusive_start for same reason.
            EBtreeOpts = [
                {dir, rev},
                {inclusive_start, InclusiveEnd}
            ],
            ebtree:group_reduce(
                    Tx,
                    Btree,
                    EndKey,
                    StartKey,
                    GroupKeyFun,
                    Wrapper,
                    Acc0,
                    EBtreeOpts
                )
    end.


update_views(TxDb, Mrst, Docs) ->
    #{
        tx := Tx
    } = TxDb,

    % Get initial KV size
    OldKVSize = lists:foldl(fun(View, SizeAcc) ->
        {_, Size, _} = ebtree:full_reduce(Tx, View#mrview.btree),
        SizeAcc + Size
    end, 0, Mrst#mrst.views),

    % Collect update information
    #{
        ids := IdMap,
        views := ViewMaps,
        delete_ref := DeleteRef
    } = gather_update_info(Tx, Mrst, Docs),

    UpdateBTree = fun(BTree, Map) ->
        {ToRemove, ToInsert} = maps:fold(fun(Key, Value, {Keys, Rows}) ->
            case Value of
                DeleteRef -> {[Key | Keys], Rows};
                _ -> {Keys, [{Key, Value} | Rows]}
            end
        end, {[], []}, Map),

        lists:foreach(fun(Key) ->
            ebtree:delete(Tx, BTree, Key)
        end, ToRemove),

        ebtree:insert_multi(Tx, BTree, ToInsert)
    end,

    % Update the IdBtree
    UpdateBTree(Mrst#mrst.id_btree, IdMap),

    % Update each view's BTree
    lists:foreach(fun(View) ->
        #mrview{
            id_num = ViewId,
            btree = BTree
        } = View,

        ViewMap = maps:get(ViewId, ViewMaps, #{}),
        UpdateBTree(BTree, ViewMap)
    end, Mrst#mrst.views),

    % Get new KV size after update
    NewKVSize = lists:foldl(fun(View, SizeAcc) ->
        {_, Size, _} = ebtree:full_reduce(Tx, View#mrview.btree),
        SizeAcc + Size
    end, 0, Mrst#mrst.views),

    update_kv_size(TxDb, Mrst#mrst.sig, OldKVSize, NewKVSize).


list_signatures(Db) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    ViewSeqRange = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ},
    RangePrefix = erlfdb_tuple:pack(ViewSeqRange, DbPrefix),
    fabric2_fdb:fold_range(Db, RangePrefix, fun({Key, _Val}, Acc) ->
        {Sig} = erlfdb_tuple:unpack(Key, RangePrefix),
        [Sig | Acc]
    end, [], []).


clear_index(Db, Signature) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    % Get view size to remove from global counter
    SizeTuple = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Signature},
    SizeKey = erlfdb_tuple:pack(SizeTuple, DbPrefix),
    ViewSize = ?bin2uint(erlfdb:wait(erlfdb:get(Tx, SizeKey))),

    % Clear index info keys
    Keys = [
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Signature},
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_ROW_COUNT, Signature},
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Signature}
    ],
    lists:foreach(fun(Key) ->
        FDBKey = erlfdb_tuple:pack(Key, DbPrefix),
        erlfdb:clear(Tx, FDBKey)
    end, Keys),

    % Clear index data
    DataTuple = {?DB_VIEWS, ?VIEW_DATA, Signature},
    DataPrefix = erlfdb_tuple:pack(DataTuple, DbPrefix),
    erlfdb:clear_range_startswith(Tx, DataPrefix),

    % Clear tree data
    TreeTuple = {?DB_VIEWS, ?VIEW_TREES, Signature},
    TreePrefix = erlfdb_tuple:pack(TreeTuple, DbPrefix),
    erlfdb:clear_range_startswith(Tx, TreePrefix),

    % Decrement db wide view size counter
    DbSizeTuple = {?DB_STATS, <<"sizes">>, <<"views">>},
    DbSizeKey = erlfdb_tuple:pack(DbSizeTuple, DbPrefix),
    erlfdb:add(Tx, DbSizeKey, -ViewSize).


open_id_tree(TxDb, Sig) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    Prefix = id_tree_prefix(DbPrefix, Sig),
    TreeOpts = [
        {persist_fun, fun persist_chunks/3},
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
        {persist_fun, fun persist_chunks/3},
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


make_reduce_fun(Lang, #mrview{} = View) ->
    RedFuns = [Src || {_, Src} <- View#mrview.reduce_funs],
    fun
        (KVs0, _ReReduce = false) ->
            KVs1 = expand_dupes(KVs0),
            TotalSize = lists:foldl(fun({K, V}, Acc) ->
                KSize = couch_ejson_size:encoded_size(K),
                VSize = couch_ejson_size:encoded_size(V),
                KSize + VSize + Acc
            end, 0, KVs1),
            KVs2 = detuple_kvs(KVs1),
            {ok, UserReds} = couch_query_servers:reduce(Lang, RedFuns, KVs2),
            {length(KVs1), TotalSize, UserReds};
        (Reductions, _ReReduce = true) ->
            FoldFun = fun({Count, Size, UserReds}, {CAcc, SAcc, URedAcc}) ->
                NewCAcc = Count + CAcc,
                NewSAcc = Size + SAcc,
                NewURedAcc = [UserReds | URedAcc],
                {NewCAcc, NewSAcc, NewURedAcc}
            end,
            InitAcc = {0, 0, []},
            FinalAcc = lists:foldl(FoldFun, InitAcc, Reductions),
            {FinalCount, FinalSize, UReds} = FinalAcc,
            {ok, Result} = couch_query_servers:rereduce(Lang, RedFuns, UReds),
            {FinalCount, FinalSize, Result}
    end.


persist_chunks(Tx, set, [Key, Value]) ->
    Chunks = fabric2_fdb:chunkify_binary(Value),
    LastId = lists:foldl(fun(Chunk, Id) ->
        ChunkKey = erlfdb_tuple:pack({Id}, Key),
        erlfdb:set(Tx, ChunkKey, Chunk),
        Id + 1
    end, 0, Chunks),

    % We update nodes in place, so its possible that
    % a node shrank. This clears any keys that we haven't
    % just overwritten for the provided key.
    LastIdKey = erlfdb_tuple:pack({LastId}, Key),
    EndRange = <<Key/binary, 16#FF>>,
    erlfdb:clear_range(Tx, LastIdKey, EndRange);

persist_chunks(Tx, get, Key) ->
    Rows = erlfdb:get_range_startswith(Tx, Key),
    Values = [V || {_K, V} <- Rows],
    iolist_to_binary(Values);

persist_chunks(Tx, clear, Key) ->
    erlfdb:clear_range_startswith(Tx, Key).


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


to_red_opts(Options) ->
    {Dir, StartKey, EndKey, InclusiveEnd} = to_map_opts(Options),

    GroupKeyFun = case lists:keyfind(group_key_fun, 1, Options) of
        {group_key_fun, GKF} -> GKF;
        false -> fun({_Key, _DocId}) -> global_group end
    end,

    {Dir, StartKey, EndKey, InclusiveEnd, GroupKeyFun}.


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

    lists:foldl(fun(Doc, InfoAcc2) ->
        #{
            id := DocId,
            deleted := Deleted,
            results := Results
        } = Doc,

        if Deleted -> InfoAcc2; true ->
            Out = lists:foldl(fun({View, RawNewRows}, {IdKeyAcc, InfoAcc3}) ->
                #mrview{
                    id_num = ViewId
                } = View,
                #{
                    views := ViewMaps
                } = InfoAcc3,

                DedupedRows = dedupe_rows(View, RawNewRows),
                IdKeys = lists:usort([K || {K, _V} <- DedupedRows]),

                OldViewMap = maps:get(ViewId, ViewMaps, #{}),
                NewViewMap = lists:foldl(fun({K, V}, ViewMapAcc) ->
                    maps:put({K, DocId}, V, ViewMapAcc)
                end, OldViewMap, DedupedRows),

                {[{ViewId, IdKeys} | IdKeyAcc], InfoAcc3#{
                    views := maps:put(ViewId, NewViewMap, ViewMaps)
                }}
            end, {[], InfoAcc2}, lists:zip(Mrst#mrst.views, Results)),

            {IdRows, #{ids := IdMap} = InfoAcc4} = Out,

            % Don't store a row in the id_btree if it hasn't got any
            % keys that will need to be deleted.
            NonEmptyRows = [1 || {_ViewId, Rows} <- IdRows, Rows /= []],
            if length(NonEmptyRows) == 0 -> InfoAcc4; true ->
                InfoAcc4#{ids := maps:put(DocId, IdRows, IdMap)}
            end
        end
    end, InfoAcc1, Docs).


update_kv_size(TxDb, Sig, OldSize, NewSize) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    ViewTuple = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Sig},
    ViewKey = erlfdb_tuple:pack(ViewTuple, DbPrefix),
    erlfdb:set(Tx, ViewKey, ?uint2bin(NewSize)),

    DbTuple = {?DB_STATS, <<"sizes">>, <<"views">>},
    DbKey = erlfdb_tuple:pack(DbTuple, DbPrefix),
    erlfdb:add(Tx, DbKey, NewSize - OldSize).


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


expand_dupes([]) ->
    [];
expand_dupes([{K, {dups, Dups}} | Rest]) ->
    Expanded = [{K, D} || D <- Dups],
    Expanded ++ expand_dupes(Rest);
expand_dupes([{K, V} | Rest]) ->
    [{K, V} | expand_dupes(Rest)].


detuple_kvs([]) ->
    [];
detuple_kvs([KV | Rest]) ->
    {{Key, Id}, Value} = KV,
    [[[Key, Id], Value] | detuple_kvs(Rest)].


id_tree_prefix(DbPrefix, Sig) ->
    Key = {?DB_VIEWS, ?VIEW_TREES, Sig, ?VIEW_ID_TREE},
    erlfdb_tuple:pack(Key, DbPrefix).


view_tree_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_TREES, Sig, ?VIEW_ROW_TREES, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


seq_key(DbPrefix, Sig) ->
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Sig},
    erlfdb_tuple:pack(Key, DbPrefix).


creation_vs_key(Db, Sig) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_CREATION_VS, Sig},
    erlfdb_tuple:pack(Key, DbPrefix).


build_status_key(Db, Sig) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_BUILD_STATUS, Sig},
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
