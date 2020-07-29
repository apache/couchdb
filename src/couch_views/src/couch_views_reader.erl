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

-module(couch_views_reader).

-export([
    read/6
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


read(Db, Mrst, ViewName, UserCallback, UserAcc, Args) ->
    ReadFun = case Args of
        #mrargs{view_type = map} -> fun read_map_view/6;
        #mrargs{view_type = red} -> fun read_red_view/6
    end,
    ReadFun(Db, Mrst, ViewName, UserCallback, UserAcc, Args).


read_map_view(Db, Mrst0, ViewName, UserCallback, UserAcc0, Args) ->
    try
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            #mrst{
                language = Lang,
                views = Views
            } = Mrst = couch_views_fdb:set_trees(TxDb, Mrst0),

            View = get_map_view(Lang, Args, ViewName, Views),
            Fun = fun handle_map_row/4,

            Meta = get_map_meta(TxDb, Mrst, View, Args),
            UserAcc1 = maybe_stop(UserCallback(Meta, UserAcc0)),

            Acc0 = #{
                db => TxDb,
                skip => Args#mrargs.skip,
                limit => Args#mrargs.limit,
                mrargs => undefined,
                callback => UserCallback,
                acc => UserAcc1
            },

            Acc1 = lists:foldl(fun(KeyArgs, KeyAcc0) ->
                Opts = mrargs_to_fdb_options(KeyArgs),
                KeyAcc1 = KeyAcc0#{
                    mrargs := KeyArgs
                },
                couch_views_fdb:fold_map_idx(TxDb, View, Opts, Fun, KeyAcc1)
            end, Acc0, expand_keys_args(Args)),

            #{
                acc := UserAcc2
            } = Acc1,
            {ok, maybe_stop(UserCallback(complete, UserAcc2))}
        end)
    catch
        throw:{complete, Out} ->
            {_, Final} = UserCallback(complete, Out),
            {ok, Final};
        throw:{done, Out} ->
            {ok, Out}
    end.


read_red_view(Db, Mrst0, ViewName, UserCallback, UserAcc0, Args) ->
    try
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            #mrst{
                language = Lang,
                views = Views
            } = Mrst = couch_views_fdb:set_trees(TxDb, Mrst0),

            #mrargs{
                extra = Extra
            } = Args,

            {Idx, Lang, View} = get_red_view(Lang, Args, ViewName, Views),
            Fun = fun handle_red_row/3,

            Meta = get_red_meta(TxDb, Mrst, View, Args),
            UserAcc1 = maybe_stop(UserCallback(Meta, UserAcc0)),

            Finalizer = case couch_util:get_value(finalizer, Extra) of
                undefined ->
                    {_, FunSrc} = lists:nth(Idx, View#mrview.reduce_funs),
                    FunSrc;
                CustomFun->
                    CustomFun
            end,

            Acc0 = #{
                db => TxDb,
                skip => Args#mrargs.skip,
                limit => Args#mrargs.limit,
                mrargs => undefined,
                finalizer => Finalizer,
                red_idx => Idx,
                language => Lang,
                callback => UserCallback,
                acc => UserAcc1
            },

            Acc1 = lists:foldl(fun(KeyArgs, KeyAcc0) ->
                Opts = mrargs_to_fdb_options(KeyArgs),
                KeyAcc1 = KeyAcc0#{
                    mrargs := KeyArgs
                },
                couch_views_fdb:fold_red_idx(
                        TxDb,
                        View,
                        Idx,
                        Opts,
                        Fun,
                        KeyAcc1
                    )
            end, Acc0, expand_keys_args(Args)),

            #{
                acc := UserAcc2
            } = Acc1,
            {ok, maybe_stop(UserCallback(complete, UserAcc2))}
        end)
    catch
        throw:{complete, Out} ->
            {_, Final} = UserCallback(complete, Out),
            {ok, Final};
        throw:{done, Out} ->
            {ok, Out}
    end.


get_map_meta(TxDb, Mrst, View, #mrargs{update_seq = true}) ->
    TotalRows = couch_views_fdb:get_row_count(TxDb, View),
    ViewSeq = couch_views_fdb:get_update_seq(TxDb, Mrst),
    {meta,  [{update_seq, ViewSeq}, {total, TotalRows}, {offset, null}]};

get_map_meta(TxDb, _Mrst, View, #mrargs{}) ->
    TotalRows = couch_views_fdb:get_row_count(TxDb, View),
    {meta, [{total, TotalRows}, {offset, null}]}.


get_red_meta(TxDb, Mrst, _View, #mrargs{update_seq = true}) ->
    ViewSeq = couch_views_fdb:get_update_seq(TxDb, Mrst),
    {meta,  [{update_seq, ViewSeq}]};

get_red_meta(_TxDb, _Mrst, _View, #mrargs{}) ->
    {meta, []}.


handle_map_row(_DocId, _Key, _Value, #{skip := Skip} = Acc) when Skip > 0 ->
    Acc#{skip := Skip - 1};

handle_map_row(_DocID, _Key, _Value, #{limit := 0, acc := UserAcc}) ->
    throw({complete, UserAcc});

handle_map_row(DocId, Key, Value, Acc) ->
    #{
        db := TxDb,
        limit := Limit,
        mrargs := Args,
        callback := UserCallback,
        acc := UserAcc0
    } = Acc,

    BaseRow = [
        {id, DocId},
        {key, Key},
        {value, Value}
    ],

    Row = BaseRow ++ if not Args#mrargs.include_docs -> []; true ->
        DocOpts0 = Args#mrargs.doc_options,
        DocOpts1 = DocOpts0 ++ case Args#mrargs.conflicts of
            true -> [conflicts];
            _ -> []
        end,

        {TargetDocId, Rev} = get_doc_id(DocId, Value),
        DocObj = load_doc(TxDb, TargetDocId, Rev, DocOpts1),
        [{doc, DocObj}]
    end,

    UserAcc1 = maybe_stop(UserCallback({row, Row}, UserAcc0)),
    Acc#{limit := Limit - 1, acc := UserAcc1}.


handle_red_row(_Key, _Red, #{skip := Skip} = Acc) when Skip > 0 ->
    Acc#{skip := Skip - 1};

handle_red_row(_Key, _Value, #{limit := 0, acc := UserAcc}) ->
    throw({complete, UserAcc});

handle_red_row(Key0, Value0, Acc) ->
    #{
        limit := Limit,
        finalizer := Finalizer,
        callback := UserCallback,
        acc := UserAcc0
    } = Acc,

    Key1 = case Key0 of
        undefined -> null;
        _ -> Key0
    end,
    Value1 = maybe_finalize(Finalizer, Value0),
    Row = [{key, Key1}, {value, Value1}],

    UserAcc1 = maybe_stop(UserCallback({row, Row}, UserAcc0)),
    Acc#{limit := Limit - 1, acc := UserAcc1}.


maybe_finalize(null, Red) ->
    Red;
maybe_finalize(Finalizer, Red) ->
    {ok, Finalized} = couch_query_servers:finalize(Finalizer, Red),
    Finalized.


get_map_view(Lang, Args, ViewName, Views) ->
    case couch_mrview_util:extract_view(Lang, Args, ViewName, Views) of
        {map, View, _Args} -> View;
        {red, {_Idx, _Lang, View}, _} -> View
    end.


get_red_view(Lang, Args, ViewName, Views) ->
    case couch_mrview_util:extract_view(Lang, Args, ViewName, Views) of
        {red, {Idx, Lang, View}, _} -> {Idx, Lang, View};
        _ -> throw({not_found, missing_named_view})
    end.


expand_keys_args(#mrargs{keys = undefined} = Args) ->
    [Args];

expand_keys_args(#mrargs{keys = Keys} = Args) ->
    lists:map(fun(Key) ->
        Args#mrargs{
            start_key = Key,
            end_key = Key
        }
    end, Keys).


mrargs_to_fdb_options(Args) ->
    #mrargs{
        view_type = ViewType,
        start_key = StartKey,
        start_key_docid = StartKeyDocId,
        end_key = EndKey,
        end_key_docid = EndKeyDocId0,
        direction = Direction,
        inclusive_end = InclusiveEnd,
        group_level = GroupLevel
    } = Args,

    StartKeyOpts = if StartKey == undefined -> []; true ->
        [{start_key, {StartKey, StartKeyDocId}}]
    end,

    EndKeyDocId = case {Direction, EndKeyDocId0} of
        {fwd, <<255>>} when InclusiveEnd -> <<255>>;
        {fwd, <<255>>} when not InclusiveEnd -> <<>>;
        {rev, <<>>} when InclusiveEnd -> <<>>;
        {rev, <<>>} when not InclusiveEnd -> <<255>>;
        _ -> EndKeyDocId0
    end,

    EndKeyOpts = if EndKey == undefined -> []; true ->
        [{end_key, {EndKey, EndKeyDocId}}]
    end,

    GroupFunOpt = make_group_key_fun(ViewType, GroupLevel),

    [
        {dir, Direction},
        {inclusive_end, InclusiveEnd}
    ] ++ StartKeyOpts ++ EndKeyOpts ++ GroupFunOpt.


make_group_key_fun(map, _) ->
    [];

make_group_key_fun(red, exact) ->
    [
        {group_key_fun, fun({Key, _DocId}) -> Key end}
    ];

make_group_key_fun(red, 0) ->
    [
        {group_key_fun, group_all}
    ];

make_group_key_fun(red, N) when is_integer(N), N > 0 ->
    GKFun = fun
        ({Key, _DocId}) when is_list(Key) -> lists:sublist(Key, N);
        ({Key, _DocId}) -> Key
    end,
    [{group_key_fun, GKFun}].


maybe_stop({ok, Acc}) -> Acc;
maybe_stop({stop, Acc}) -> throw({done, Acc}).


get_doc_id(Id, {Props}) ->
    DocId = couch_util:get_value(<<"_id">>, Props, Id),
    Rev = couch_util:get_value(<<"_rev">>, Props, null),
    {DocId, Rev};

get_doc_id(Id, _Value) ->
    {Id, null}.


load_doc(TxDb, Id, null, DocOpts) ->
    case fabric2_db:open_doc(TxDb, Id, DocOpts) of
        {ok, Doc} -> couch_doc:to_json_obj(Doc, DocOpts);
        {not_found, _} -> null
    end;

load_doc(TxDb, Id, Rev, DocOpts) ->
    Rev1 = couch_doc:parse_rev(Rev),
    case fabric2_db:open_doc_revs(TxDb, Id, [Rev1], DocOpts) of
        {ok, [{ok, Doc}]} -> couch_doc:to_json_obj(Doc, DocOpts);
        {ok, [_Else]} -> null
    end.
