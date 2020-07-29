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

    write_doc/3,

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


write_doc(TxDb, Mrst, #{deleted := true} = Doc) ->
    #{
        tx := Tx
    } = TxDb,
    #{
        id := DocId
    } = Doc,

    ExistingViewKeys = get_view_keys(TxDb, Mrst, DocId),

    ebtree:delete(Tx, Mrst#mrst.id_btree, DocId),
    lists:foreach(fun(#mrview{id_num = ViewId, btree = Btree}) ->
        ViewKeys = case lists:keyfind(ViewId, 1, ExistingViewKeys) of
            {ViewId, Keys} -> Keys;
            false -> []
        end,
        lists:foreach(fun(Key) ->
            ebtree:delete(Tx, Btree, {Key, DocId})
        end, ViewKeys)
    end, Mrst#mrst.views);

write_doc(TxDb, Mrst, Doc) ->
    #{
        tx := Tx
    } = TxDb,
    #{
        id := DocId,
        results := Results
    } = Doc,

    ExistingViewKeys = get_view_keys(TxDb, Mrst, DocId),

    NewIdKeys = lists:foldl(fun({View, RawNewRows}, IdKeyAcc) ->
        #mrview{
            id_num = ViewId
        } = View,

        % Remove old keys in the view
        ExistingKeys = case lists:keyfind(ViewId, 1, ExistingViewKeys) of
            {ViewId, Keys} -> Keys;
            false -> []
        end,
        lists:foreach(fun(K) ->
            ebtree:delete(Tx, View#mrview.btree, {K, DocId})
        end, ExistingKeys),

        % Insert new rows
        NewRows = dedupe_rows(View, RawNewRows),
        lists:foreach(fun({K, V}) ->
            ebtree:insert(Tx, View#mrview.btree, {K, DocId}, V)
        end, NewRows),
        ViewKeys = {View#mrview.id_num, lists:usort([K || {K, _V} <- NewRows])},
        [ViewKeys | IdKeyAcc]
    end, [], lists:zip(Mrst#mrst.views, Results)),

    ebtree:insert(Tx, Mrst#mrst.id_btree, DocId, NewIdKeys).


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
    erlfdb:clear_range_startswith(Tx, TreePrefix).


get_view_keys(TxDb, Mrst, DocId) ->
    #{
        tx := Tx
    } = TxDb,
    #mrst{
        id_btree = IdTree
    } = Mrst,
    case ebtree:lookup(Tx, IdTree, DocId) of
        {DocId, ViewKeys} -> ViewKeys;
        false -> []
    end.


open_id_tree(TxDb, Sig) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    Prefix = id_tree_prefix(DbPrefix, Sig),
    TreeOpts = [
        {persist_fun, fun persist_chunks/3}
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
        {persist_fun, fun persist_chunks/3}
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
            KVs1 = detuple_kvs(expand_dupes(KVs0)),
            TotalSize = lists:foldl(fun([K, V], Acc) ->
                KSize = couch_ejson_size:encoded_size(K),
                VSize = couch_ejson_size:encoded_size(V),
                KSize + VSize + Acc
            end, 0, KVs1),
            {ok, UserReds} = couch_query_servers:reduce(Lang, RedFuns, KVs1),
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
    lists:foldl(fun(Chunk, Id) ->
        ChunkKey = erlfdb_tuple:pack({Id}, Key),
        erlfdb:set(Tx, ChunkKey, Chunk),
        Id + 1
    end, 0, Chunks);

persist_chunks(Tx, get, Key) ->
    Rows = erlfdb:get_range_startswith(Tx, Key),
    Values = [V || {_K, V} <- Rows],
    iolist_to_binary(Values);

persist_chunks(Tx, clear, Key) ->
    erlfdb:clear_range_startswith(Tx, Key).


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
