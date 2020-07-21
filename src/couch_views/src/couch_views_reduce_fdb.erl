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


-module(couch_views_reduce_fdb).


-export([
    write_doc/5,
    idx_prefix/3,
    get_kv_size/3,
    fold/8
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


write_doc(TxDb, Sig, Views, #{deleted := true} = Doc, ExistingViewKeys) ->
    #{
        id := DocId
    } = Doc,

    #{
        tx := Tx
    } = TxDb,

    lists:foreach(fun(View) ->
        #mrview{
            reduce_funs = ViewReduceFuns,
            id_num = ViewId
        } = View,

        ExistingKeys = get_existing_keys(ViewId, ExistingViewKeys),
        lists:foreach(fun(ViewReduceFun) ->
            Tree = open_tree(TxDb, Sig, ViewId, ViewReduceFun),
            delete_keys(Tx, Tree, DocId, ExistingKeys)

        end, ViewReduceFuns)

    end, Views);

write_doc(TxDb, Sig, Views, #{reduce_results := ViewReduceResults, id := DocId},
    ExistingViewKeys) ->
    #{
        tx := Tx
    } = TxDb,

    lists:foreach(fun({View, ReduceResults}) ->
        #mrview{
            reduce_funs = ViewReduceFuns,
            id_num = ViewId
        } = View,

        ExistingKeys = get_existing_keys(ViewId, ExistingViewKeys),

        lists:foreach(fun({ViewReduceFun, ReduceResult}) ->
            Tree = open_tree(TxDb, Sig, ViewId, ViewReduceFun),

            delete_keys(Tx, Tree, DocId, ExistingKeys),
            add_keys(Tx, Tree, DocId, ReduceResult)

        end, lists:zip(ViewReduceFuns, ReduceResults))
    end, lists:zip(Views, ViewReduceResults));

write_doc(_TxDb, _Sig, _Views, _Doc, _ExistingViewKeys) ->
    ok.


idx_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_REDUCE_RANGE, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


get_kv_size(TxDb, Mrst, ViewId) ->
    #mrst {
        views = Views,
        sig = Sig
    } = Mrst,

    #{
        tx := Tx
    } = TxDb,

    [View] = lists:filter(fun(View) -> View#mrview.id_num == ViewId end, Views),
    #mrview{
        reduce_funs = ViewReduceFuns,
        id_num = ViewId
    } = View,

    lists:foldl(fun(ReduceFun, Acc) ->
        Tree = open_tree(TxDb, Sig, ViewId, ReduceFun),
        {_, KVSize} = ebtree:full_reduce(Tx, Tree),
        Acc + KVSize
    end, 0, ViewReduceFuns).


fold(Db, Sig, ViewId, Reducer, GroupLevel, Opts, UserCallback,
    UserAcc0) ->

    Acc0 = #{
        user_callback => UserCallback,
        user_acc => UserAcc0,
        reducer => Reducer
    },

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx
        } = TxDb,

        StartKey = fabric2_util:get_value(start_key, Opts),
        EndKey = fabric2_util:get_value(end_key, Opts),

        Tree = open_tree(TxDb, Sig, ViewId, Reducer),
        GroupKeyFun = fun({Key, _DocId}) ->
            couch_views_util:group_level_key(Key, GroupLevel)
        end,

        Acc1 = ebtree:group_reduce(Tx, Tree, StartKey, EndKey, GroupKeyFun,
            fun fold_cb/2, Acc0, Opts),

        maps:get(user_acc, Acc1)
    end).


get_existing_keys(ViewId, ExistingViewKeys) ->
    case lists:keyfind(ViewId, 1, ExistingViewKeys) of
        {ViewId, _TotalRows, _TotalSize, EKeys} ->
            EKeys;
        false ->
            []
    end.


% The reduce values are stored as keys in the b-tree
% So every call to the reducer from the b-tree is always
% a rereduce call.
get_reducer({_, ReduceFun}) ->
    get_reducer(ReduceFun);

get_reducer(Reducer) ->
    fun
        (Vs, true) ->
            rereduce_val_and_size(Reducer, Vs);
        (KVs, false) ->
            {_, Vs} = lists:unzip(KVs),
            rereduce_val_and_size(Reducer, Vs)
    end.


% This happens if a reduce is called without the index being built
% for example from `get_kv_size`
rereduce_val_and_size(Reducer, []) ->
    {0, 0};

rereduce_val_and_size(Reducer, Vs) ->
    {ReduceVs, SizeVs} = lists:unzip(Vs),
    ReduceVal = couch_views_reducer:rereduce_values(Reducer, ReduceVs),
    SizeVal = lists:sum(SizeVs),
    {ReduceVal, SizeVal}.


open_tree(TxDb, Sig, ViewId, ViewReduceFun) ->
    #{
        db_prefix := DbPrefix,
        tx := Tx
    } = TxDb,

    ReduceId = couch_views_util:reduce_id(ViewId, ViewReduceFun),
    ReduceIdxPrefix = idx_prefix(DbPrefix, Sig, ReduceId),

    EncodeFun = fun
        (encode, Key, Term) ->
            Bin = term_to_binary(Term, [compressed, {minor_version, 2}]),
            aegis:encrypt(TxDb, Key, Bin);
        (decode, Key, Ciphertext) ->
            Bin = aegis:decrypt(TxDb, Key, Ciphertext),
            binary_to_term(Bin, [safe])
    end,

    TreeOpts = [
        {reduce_fun, get_reducer(ViewReduceFun)},
        {encode_fun, EncodeFun}
    ],
    ebtree:open(Tx, ReduceIdxPrefix, btree_order_size(), TreeOpts).


delete_keys(Tx, Tree, DocId, Keys) ->
    lists:foreach(fun (Key) ->
        EK = create_key(Key, DocId),
        ebtree:delete(Tx, Tree, EK)
    end, Keys).


add_keys(Tx, Tree, DocId, Results) ->
    lists:foreach(fun ({Key, Val}) ->
        EK = create_key(Key, DocId),
        EV = create_val(Key, Val),
        ebtree:insert(Tx, Tree, EK, EV)
    end, Results).


create_key(Key, DocId) ->
    {Key, DocId}.


create_val(Key, Val) ->
    KeySize = erlang:external_size(Key),
    ValSize = erlang:external_size(Val),
    {Val, KeySize + ValSize}.


get_reduce_val({ReducedVal, _Size}) ->
    ReducedVal.


fold_cb({GroupKey, Val}, Acc) ->
    #{
        user_callback := Callback,
        user_acc := UserAcc0,
        reducer := Reducer
    } = Acc,

    ReducedVal = get_reduce_val(Val),

    {ok, FinalizedVal} = couch_views_reducer:finalize(Reducer, ReducedVal),
    UserAcc1 = Callback(GroupKey, FinalizedVal, UserAcc0),

    Acc#{
        user_acc := UserAcc1
    }.


btree_order_size() ->
    config:get_integer("couch_views", "btree_order_size", 100).
