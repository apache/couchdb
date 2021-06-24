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
    open/3,

    get_row_count/2,
    get_kv_size/2,

    fold_map_idx/5,
    fold_red_idx/6,

    update_views/3
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-include("couch_views.hrl").
-include_lib("fabric/include/fabric2.hrl").

open(TxDb, Mrst) ->
    open(TxDb, Mrst, []).

open(TxDb, Mrst, Options) ->
    #mrst{
        sig = Sig,
        language = Lang,
        views = Views
    } = Mrst,
    Mrst#mrst{
        id_btree = open_id_tree(TxDb, Sig),
        views = [open_view_tree(TxDb, Sig, Lang, V, Options) || V <- Views]
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
        KVs1 =
            case InclusiveEnd of
                true ->
                    KVs0;
                false when Dir == fwd ->
                    lists:filter(
                        fun({K, _V}) ->
                            case CollateFun(K, EndKey) of
                                lt -> true;
                                eq -> false;
                                gt -> false
                            end
                        end,
                        KVs0
                    );
                false when Dir == rev ->
                    lists:filter(
                        fun({K, _V}) ->
                            case CollateFun(K, EndKey) of
                                lt -> false;
                                eq -> false;
                                gt -> true
                            end
                        end,
                        KVs0
                    )
            end,
        % Expand dups
        KVs2 = lists:flatmap(
            fun({K, V}) ->
                case V of
                    {dups, Dups} when Dir == fwd ->
                        [{K, D} || D <- Dups];
                    {dups, Dups} when Dir == rev ->
                        [{K, D} || D <- lists:reverse(Dups)];
                    _ ->
                        [{K, V}]
                end
            end,
            KVs1
        ),
        lists:foldl(
            fun({{Key, DocId}, Value}, WAccInner) ->
                Callback(DocId, Key, Value, WAccInner)
            end,
            WAcc,
            KVs2
        )
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

    case Dir of
        fwd ->
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
        rev ->
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
    OldKVSize = lists:foldl(
        fun(View, SizeAcc) ->
            {_, Size, _} = ebtree:full_reduce(Tx, View#mrview.btree),
            SizeAcc + Size
        end,
        0,
        Mrst#mrst.views
    ),

    % Collect update information
    #{
        ids := IdMap,
        views := ViewMaps,
        delete_ref := DeleteRef
    } = gather_update_info(Tx, Mrst, Docs),

    % Update the IdBtree
    update_btree(Tx, Mrst#mrst.id_btree, IdMap, DeleteRef),

    % Update each view's BTree
    lists:foreach(
        fun(View) ->
            #mrview{
                id_num = ViewId,
                btree = BTree
            } = View,

            ViewMap = maps:get(ViewId, ViewMaps, #{}),
            update_btree(Tx, BTree, ViewMap, DeleteRef)
        end,
        Mrst#mrst.views
    ),

    % Get new KV size after update
    NewKVSize = lists:foldl(
        fun(View, SizeAcc) ->
            {_, Size, _} = ebtree:full_reduce(Tx, View#mrview.btree),
            SizeAcc + Size
        end,
        0,
        Mrst#mrst.views
    ),

    couch_views_fdb:update_kv_size(TxDb, Mrst#mrst.sig, OldKVSize, NewKVSize).

open_id_tree(TxDb, Sig) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    Prefix = id_tree_prefix(DbPrefix, Sig),
    TreeOpts = [
        {persist_fun, fun couch_views_fdb:persist_chunks/3},
        {encode_fun, create_encode_fun(TxDb)}
    ],
    ebtree:open(Tx, Prefix, get_order(id_btree), TreeOpts).

open_view_tree(TxDb, Sig, Lang, View, Options) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    #mrview{
        id_num = ViewId
    } = View,
    Prefix = view_tree_prefix(DbPrefix, Sig, ViewId),
    BaseOpts = [
        {collate_fun, couch_views_util:collate_fun(View)},
        {persist_fun, fun couch_views_fdb:persist_chunks/3},
        {encode_fun, create_encode_fun(TxDb)}
    ],
    ExtraOpts =
        case lists:keyfind(read_only, 1, Options) of
            {read_only, Idx} ->
                RedFun = make_read_only_reduce_fun(Lang, View, Idx),
                [{reduce_fun, RedFun}];
            false ->
                [
                    {reduce_fun, make_reduce_fun(Lang, View)}
                ]
        end,
    TreeOpts = BaseOpts ++ ExtraOpts,
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

make_read_only_reduce_fun(Lang, View, NthRed) ->
    RedFuns = [Src || {_, Src} <- View#mrview.reduce_funs],
    LPad = lists:duplicate(NthRed - 1, []),
    RPad = lists:duplicate(length(RedFuns) - NthRed, []),
    FunSrc = lists:nth(NthRed, RedFuns),
    fun
        (KVs0, _ReReduce = false) ->
            KVs1 = detuple_kvs(expand_dupes(KVs0)),
            {ok, Result} = couch_query_servers:reduce(Lang, [FunSrc], KVs1),
            {0, 0, LPad ++ Result ++ RPad};
        (Reductions, _ReReduce = true) ->
            ExtractFun = fun(Reds) ->
                {_Count, _Size, UReds} = Reds,
                [lists:nth(NthRed, UReds)]
            end,
            UReds = lists:map(ExtractFun, Reductions),
            {ok, Result} =
                case UReds of
                    [RedVal] ->
                        {ok, RedVal};
                    _ ->
                        couch_query_servers:rereduce(Lang, [FunSrc], UReds)
                end,
            {0, 0, LPad ++ Result ++ RPad}
    end.

make_reduce_fun(Lang, #mrview{} = View) ->
    RedFuns = [Src || {_, Src} <- View#mrview.reduce_funs, Src /= disabled],
    fun
        (KVs0, _ReReduce = false) ->
            KVs1 = expand_dupes(KVs0),
            TotalSize = lists:foldl(
                fun({{K, _DocId}, V}, Acc) ->
                    KSize = couch_ejson_size:encoded_size(K),
                    VSize = couch_ejson_size:encoded_size(V),
                    KSize + VSize + Acc
                end,
                0,
                KVs1
            ),
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

create_encode_fun(TxDb) ->
    fun
        (encode, Key, Term) ->
            Bin = term_to_binary(Term, [compressed, {minor_version, 2}]),
            aegis:encrypt(TxDb, Key, Bin);
        (decode, Key, Ciphertext) ->
            Bin = aegis:decrypt(TxDb, Key, Ciphertext),
            binary_to_term(Bin, [safe])
    end.

to_map_opts(Options) ->
    Dir =
        case lists:keyfind(dir, 1, Options) of
            {dir, D} -> D;
            _ -> fwd
        end,

    InclusiveEnd =
        case lists:keyfind(inclusive_end, 1, Options) of
            {inclusive_end, IE} -> IE;
            _ -> true
        end,

    StartKey =
        case lists:keyfind(start_key, 1, Options) of
            {start_key, SK} -> SK;
            false when Dir == fwd -> ebtree:min();
            false when Dir == rev -> ebtree:max()
        end,

    EndKey =
        case lists:keyfind(end_key, 1, Options) of
            {end_key, EK} -> EK;
            false when Dir == fwd -> ebtree:max();
            false when Dir == rev -> ebtree:min()
        end,

    {Dir, StartKey, EndKey, InclusiveEnd}.

to_red_opts(Options) ->
    {Dir, StartKey, EndKey, InclusiveEnd} = to_map_opts(Options),

    GroupKeyFun =
        case lists:keyfind(group_key_fun, 1, Options) of
            {group_key_fun, group_all} -> fun({_Key, _DocId}) -> null end;
            {group_key_fun, GKF} -> GKF;
            false -> fun({_Key, _DocId}) -> null end
        end,

    {Dir, StartKey, EndKey, InclusiveEnd, GroupKeyFun}.

gather_update_info(Tx, Mrst, Docs) ->
    % A special token used to indicate that the row should be deleted
    DeleteRef = erlang:make_ref(),

    AllDocIds = [DocId || #{id := DocId} <- Docs],

    BaseIdMap = lists:foldl(
        fun(DocId, Acc) ->
            maps:put(DocId, DeleteRef, Acc)
        end,
        #{},
        AllDocIds
    ),

    % Build the initial set of rows to delete
    % ExistingViewKeys is a list of {DocId, [{ViewId, [Key | _]} | _]}
    ExistingViewKeys = ebtree:lookup_multi(Tx, Mrst#mrst.id_btree, AllDocIds),

    % For each view, create an initial map that contains the
    % list of keys to delete. The final result is a map of
    % maps:
    %  #{ViewId => #{Key => DeleteRef}}
    BaseViewMaps = lists:foldl(
        fun({DocId, ViewIdKeys}, ViewIdAcc1) ->
            lists:foldl(
                fun({ViewId, Keys}, ViewIdAcc2) ->
                    OldViewMap = maps:get(ViewId, ViewIdAcc2, #{}),
                    NewViewMap = lists:foldl(
                        fun(Key, ViewMapAcc) ->
                            maps:put({Key, DocId}, DeleteRef, ViewMapAcc)
                        end,
                        OldViewMap,
                        Keys
                    ),
                    maps:put(ViewId, NewViewMap, ViewIdAcc2)
                end,
                ViewIdAcc1,
                ViewIdKeys
            )
        end,
        #{},
        ExistingViewKeys
    ),

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
    lists:foldl(
        fun(Doc, InfoAcc2) ->
            insert_doc(Mrst, Doc, InfoAcc2)
        end,
        InfoAcc1,
        Docs
    ).

insert_doc(_Mrst, #{deleted := true} = _Doc, InfoAcc) ->
    InfoAcc;
insert_doc(Mrst, Doc, InfoAcc0) ->
    #{
        id := DocId,
        results := Results
    } = Doc,

    FinalAcc = lists:foldl(
        fun({View, RawNewRows}, {IdKeyAcc, InfoAcc1}) ->
            #mrview{
                id_num = ViewId
            } = View,
            #{
                views := ViewMaps
            } = InfoAcc1,

            DedupedRows = dedupe_rows(View, RawNewRows),
            IdKeys = lists:usort([K || {K, _V} <- DedupedRows]),

            OldViewMap = maps:get(ViewId, ViewMaps, #{}),
            NewViewMap = lists:foldl(
                fun({K, V}, ViewMapAcc) ->
                    maps:put({K, DocId}, V, ViewMapAcc)
                end,
                OldViewMap,
                DedupedRows
            ),

            {[{ViewId, IdKeys} | IdKeyAcc], InfoAcc1#{
                views := maps:put(ViewId, NewViewMap, ViewMaps)
            }}
        end,
        {[], InfoAcc0},
        lists:zip(Mrst#mrst.views, Results)
    ),

    {IdRows, #{ids := IdMap} = InfoAcc2} = FinalAcc,

    % Don't store a row in the id_btree if it hasn't got any
    % keys that will need to be deleted.
    NonEmptyRows = [1 || {_ViewId, Rows} <- IdRows, Rows /= []],
    if
        length(NonEmptyRows) == 0 -> InfoAcc2;
        true -> InfoAcc2#{ids := maps:put(DocId, IdRows, IdMap)}
    end.

update_btree(Tx, BTree, Map, DeleteRef) ->
    {ToRemove, ToInsert} = maps:fold(
        fun(Key, Value, {Keys, Rows}) ->
            case Value of
                DeleteRef -> {[Key | Keys], Rows};
                _ -> {Keys, [{Key, Value} | Rows]}
            end
        end,
        {[], []},
        Map
    ),

    lists:foreach(
        fun(Key) ->
            ebtree:delete(Tx, BTree, Key)
        end,
        ToRemove
    ),

    ebtree:insert_multi(Tx, BTree, ToInsert).

dedupe_rows(View, KVs0) ->
    CollateFun = couch_views_util:collate_fun(View),
    KVs1 = lists:sort(
        fun({KeyA, ValA}, {KeyB, ValB}) ->
            case CollateFun({KeyA, <<>>}, {KeyB, <<>>}) of
                lt -> true;
                eq -> ValA =< ValB;
                gt -> false
            end
        end,
        KVs0
    ),
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dedupe_basic_test() ->
    View = #mrview{},
    ?assertEqual([{1, 1}], dedupe_rows(View, [{1, 1}])).

dedupe_simple_test() ->
    View = #mrview{},
    ?assertEqual([{1, {dups, [1, 2]}}], dedupe_rows(View, [{1, 1}, {1, 2}])).

-endif.
