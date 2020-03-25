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


read(Db, Mrst, ViewName, UserCallback, UserAcc0, Args) ->
    #mrst{
        language = Lang,
        sig = Sig,
        views = Views
    } = Mrst,

    ViewId = get_view_id(Lang, Args, ViewName, Views),
    Fun = fun handle_row/4,

    try
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            Meta = get_meta(TxDb, Mrst, ViewId, Args),
            UserAcc1 = maybe_stop(UserCallback(Meta, UserAcc0)),

            Acc0 = #{
                db => TxDb,
                skip => Args#mrargs.skip,
                mrargs => undefined,
                callback => UserCallback,
                acc => UserAcc1
            },

            Acc1 = lists:foldl(fun(KeyArgs, KeyAcc0) ->
                Opts = mrargs_to_fdb_options(KeyArgs),
                KeyAcc1 = KeyAcc0#{
                    mrargs := KeyArgs
                },
                couch_views_fdb:fold_map_idx(
                        TxDb,
                        Sig,
                        ViewId,
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
    catch throw:{done, Out} ->
        {ok, Out}
    end.


get_meta(TxDb, Mrst, ViewId, #mrargs{update_seq = true}) ->
    TotalRows = couch_views_fdb:get_row_count(TxDb, Mrst, ViewId),
    ViewSeq = couch_views_fdb:get_update_seq(TxDb, Mrst),
    {meta,  [{update_seq, ViewSeq}, {total, TotalRows}, {offset, null}]};

get_meta(TxDb, Mrst, ViewId, #mrargs{}) ->
    TotalRows = couch_views_fdb:get_row_count(TxDb, Mrst, ViewId),
    {meta, [{total, TotalRows}, {offset, null}]}.


handle_row(_DocId, _Key, _Value, #{skip := Skip} = Acc) when Skip > 0 ->
    Acc#{skip := Skip - 1};

handle_row(DocId, Key, Value, Acc) ->
    #{
        db := TxDb,
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
    Acc#{acc := UserAcc1}.


get_view_id(Lang, Args, ViewName, Views) ->
    case couch_mrview_util:extract_view(Lang, Args, ViewName, Views) of
        {map, View, _Args} -> View#mrview.id_num;
        {red, {_Idx, _Lang, View}} -> View#mrview.id_num
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
        start_key = StartKey0,
        start_key_docid = StartKeyDocId,
        end_key = EndKey0,
        end_key_docid = EndKeyDocId,
        direction = Direction,
        limit = Limit,
        skip = Skip,
        inclusive_end = InclusiveEnd
    } = Args,

    StartKey1 = if StartKey0 == undefined -> undefined; true ->
        couch_views_encoding:encode(StartKey0, key)
    end,

    StartKeyOpts = case {StartKey1, StartKeyDocId} of
        {undefined, _} ->
            [];
        {StartKey1, StartKeyDocId} ->
            [{start_key, {StartKey1, StartKeyDocId}}]
    end,

    EndKey1 = if EndKey0 == undefined -> undefined; true ->
        couch_views_encoding:encode(EndKey0, key)
    end,

    EndKeyOpts = case {EndKey1, EndKeyDocId, Direction} of
        {undefined, _, _} ->
            [];
        {EndKey1, <<>>, rev} when not InclusiveEnd ->
            % When we iterate in reverse with
            % inclusive_end=false we have to set the
            % EndKeyDocId to <<255>> so that we don't
            % include matching rows.
            [{end_key_gt, {EndKey1, <<255>>}}];
        {EndKey1, <<255>>, _} when not InclusiveEnd ->
            % When inclusive_end=false we need to
            % elide the default end_key_docid so as
            % to not sort past the docids with the
            % given end key.
            [{end_key_gt, {EndKey1}}];
        {EndKey1, EndKeyDocId, _} when not InclusiveEnd ->
            [{end_key_gt, {EndKey1, EndKeyDocId}}];
        {EndKey1, EndKeyDocId, _} when InclusiveEnd ->
            [{end_key, {EndKey1, EndKeyDocId}}]
    end,

    [
        {dir, Direction},
        {limit, Limit + Skip},
        {streaming_mode, want_all},
        {restart_tx, true}
    ] ++ StartKeyOpts ++ EndKeyOpts.


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
    case (catch fabric2_db:open_doc_revs(TxDb, Id, [Rev1], DocOpts)) of
        {ok, [{ok, Doc}]} -> couch_doc:to_json_obj(Doc, DocOpts);
        {ok, [{{not_found, missing}, Rev}]} -> null;
        {ok, [_Else]} -> null
    end.
