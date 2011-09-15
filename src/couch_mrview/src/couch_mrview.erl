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

-module(couch_mrview).

-export([query_all_docs/2, query_all_docs/4]).
-export([query_view/3, query_view/4, query_view/6]).
-export([get_info/2]).
-export([compact/2, monitor_compaction/2, cancel_compaction/2]).
-export([cleanup/1]).

-include("couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-record(mracc, {
    db,
    meta_sent=false,
    total_rows,
    offset,
    limit,
    skip,
    group_level,
    callback,
    user_acc,
    last_go=ok,
    reduce_fun,
    update_seq,
    args
}).


query_all_docs(Db, Args) ->
    query_all_docs(Db, Args, fun default_cb/2, []).


query_all_docs(Db, Args, Callback, Acc) when is_list(Args) ->
    query_all_docs(Db, to_mrargs(Args), Callback, Acc);
query_all_docs(Db, Args0, Callback, Acc) ->
    Sig = couch_util:with_db(Db, fun(WDb) ->
        {ok, Info} = couch_db:get_db_info(WDb),
        couch_index_util:hexsig(couch_util:md5(term_to_binary(Info)))
    end),
    Args1 = Args0#mrargs{view_type=map},
    Args2 = couch_mrview_util:validate_args(Args1),
    {ok, Acc1} = case Args2#mrargs.preflight_fun of
        PFFun when is_function(PFFun, 2) -> PFFun(Sig, Acc);
        _ -> {ok, Acc}
    end,
    all_docs_fold(Db, Args2, Callback, Acc1).


query_view(Db, DDoc, VName) ->
    query_view(Db, DDoc, VName, #mrargs{}).


query_view(Db, DDoc, VName, Args) when is_list(Args) ->
    query_view(Db, DDoc, VName, to_mrargs(Args), fun default_cb/2, []);
query_view(Db, DDoc, VName, Args) ->
    query_view(Db, DDoc, VName, Args, fun default_cb/2, []).


query_view(Db, DDoc, VName, Args, Callback, Acc) when is_list(Args) ->
    query_view(Db, DDoc, VName, to_mrargs(Args), Callback, Acc);
query_view(Db, DDoc, VName, Args0, Callback, Acc0) ->
    {ok, VInfo, Sig, Args} = couch_mrview_util:get_view(Db, DDoc, VName, Args0),
    {ok, Acc1} = case Args#mrargs.preflight_fun of
        PFFun when is_function(PFFun, 2) -> PFFun(Sig, Acc0);
        _ -> {ok, Acc0}
    end,
    query_view(Db, VInfo, Args, Callback, Acc1).


query_view(Db, {Type, View}, Args, Callback, Acc) ->
    case Type of
        map -> map_fold(Db, View, Args, Callback, Acc);
        red -> red_fold(Db, View, Args, Callback, Acc)
    end.


get_info(Db, DDoc) ->
    {ok, Pid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    couch_index:get_info(Pid).


compact(Db, DDoc) ->
    {ok, Pid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    couch_index:compact(Pid).


monitor_compaction(Db, DDoc) ->
    {ok, IPid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    {ok, CPid} = couch_index:get_compactor_pid(IPid),
    couch_index_compactor:monitor(CPid).


cancel_compaction(Db, DDoc) ->
    {ok, IPid} = couch_index_server:get_index(couch_mrview_index, Db, DDoc),
    {ok, CPid} = couch_index:get_compactor_pid(IPid),
    ok = couch_index_compactor:cancel(CPid),

    % Cleanup the compaction file if it exists
    {ok, #mrst{sig=Sig, db_name=DbName}} = couch_index:get_state(IPid, 0),
    couch_mrview_util:delete_compaction_file(DbName, Sig),
    ok.


cleanup(Db) ->
    couch_mrview_cleanup:run(Db).


all_docs_fold(Db, Args, Callback, UAcc) ->
    {ok, Info} = couch_db:get_db_info(Db),
    Total = couch_util:get_value(doc_count, Info),
    UpdateSeq = couch_db:get_update_seq(Db),
    Acc = #mracc{
        db=Db,
        total_rows=Total,
        limit=Args#mrargs.limit,
        skip=Args#mrargs.skip,
        callback=Callback,
        user_acc=UAcc,
        reduce_fun=fun couch_mrview_util:all_docs_reduce_to_count/1,
        update_seq=UpdateSeq,
        args=Args
    },
    {Reds, FinalAcc} = case Args#mrargs.keys of
        Keys when is_list(Keys) ->
            KeyFoldFun = case Args#mrargs.direction of
                fwd -> fun lists:foldl/3;
                rev -> fun lists:foldr/3
            end,
            FoldFun = fun(Key, Acc0) ->
                DocInfo = (catch couch_db:get_doc_info(Db, Key)),
                Doc = case DocInfo of
                    {ok, #doc_info{id=Id, revs=[RevInfo | _RestRevs]}} ->
                        Rev = couch_doc:rev_to_str(RevInfo#rev_info.rev),
                        Props = [{rev, Rev}] ++ case RevInfo#rev_info.deleted of
                            true -> [{deleted, true}];
                            false -> []
                        end,
                        {{Id, Id}, {Props}};
                    not_found ->
                        {{Key, error}, not_found}
                end,
                {_, Acc1} = map_fold(Doc, {[], [{0, 0, 0}]}, Acc0),
                Acc1
            end,
            KeyAcc = KeyFoldFun(FoldFun, Acc, Keys),
            {{[], [{0, 0, 0}]}, KeyAcc};
        _ ->
            [Opts] = couch_mrview_util:all_docs_key_opts(Args),
            Bt = Db#db.fulldocinfo_by_id_btree,
            {ok, R, A} = couch_btree:fold(Bt, fun map_fold/3, Acc, Opts),
            {R, A}
    end,
    Offset = couch_mrview_util:all_docs_reduce_to_count(Reds),
    finish_fold(FinalAcc, [{total, Total}, {offset, Offset}]).


map_fold(Db, View, Args, Callback, UAcc) ->
    {ok, Total} = couch_mrview_util:get_row_count(View),
    Acc = #mracc{
        db=Db,
        total_rows=Total,
        limit=Args#mrargs.limit,
        skip=Args#mrargs.skip,
        callback=Callback,
        user_acc=UAcc,
        reduce_fun=fun couch_mrview_util:reduce_to_count/1,
        update_seq=View#mrview.update_seq,
        args=Args
    },
    OptList = couch_mrview_util:key_opts(Args),
    {Reds, Acc2} = lists:foldl(fun(Opts, {_, Acc0}) ->
        {ok, R, A} = couch_mrview_util:fold(View, fun map_fold/3, Acc0, Opts),
        {R, A}
    end, {nil, Acc}, OptList),
    Offset = couch_mrview_util:reduce_to_count(Reds),
    finish_fold(Acc2, [{total, Total}, {offset, Offset}]).


map_fold(#full_doc_info{} = FullDocInfo, OffsetReds, Acc) ->
    % matches for _all_docs and translates #full_doc_info{} -> KV pair
    case couch_doc:to_doc_info(FullDocInfo) of
        #doc_info{id=Id, revs=[#rev_info{deleted=false, rev=Rev}|_]} ->
            Value = {[{rev, couch_doc:rev_to_str(Rev)}]},
            map_fold({{Id, Id}, Value}, OffsetReds, Acc);
        #doc_info{revs=[#rev_info{deleted=true}|_]} ->
            {ok, Acc}
    end;
map_fold(_KV, _Offset, #mracc{skip=N}=Acc) when N > 0 ->
    {ok, Acc#mracc{skip=N-1, last_go=ok}};
map_fold(KV, OffsetReds, #mracc{offset=undefined}=Acc) ->
    #mracc{
        total_rows=Total,
        callback=Callback,
        user_acc=UAcc0,
        reduce_fun=Reduce,
        update_seq=UpdateSeq,
        args=Args
    } = Acc,
    Offset = Reduce(OffsetReds),
    Meta = make_meta(Args, UpdateSeq, [{total, Total}, {offset, Offset}]),
    {Go, UAcc1} = Callback(Meta, UAcc0),
    Acc1 = Acc#mracc{meta_sent=true, offset=Offset, user_acc=UAcc1, last_go=Go},
    case Go of
        ok -> map_fold(KV, OffsetReds, Acc1);
        stop -> {stop, Acc1}
    end;
map_fold(_KV, _Offset, #mracc{limit=0}=Acc) ->
    {stop, Acc};
map_fold({{Key, Id}, Val}, _Offset, Acc) ->
    #mracc{
        db=Db,
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0,
        args=Args
    } = Acc,
    Doc = couch_mrview_util:maybe_load_doc(Db, Id, Val, Args),
    Row = [{id, Id}, {key, Key}, {val, Val}] ++ Doc,
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{limit=Limit-1, user_acc=UAcc1, last_go=Go}}.


red_fold(Db, {_Nth, _Lang, View}=RedView, Args, Callback, UAcc) ->
    Acc = #mracc{
        db=Db,
        total_rows=null,
        limit=Args#mrargs.limit,
        skip=Args#mrargs.skip,
        group_level=Args#mrargs.group_level,
        callback=Callback,
        user_acc=UAcc,
        update_seq=View#mrview.update_seq,
        args=Args
    },
    GroupFun = group_rows_fun(Args#mrargs.group_level),
    OptList = couch_mrview_util:key_opts(Args, [{key_group_fun, GroupFun}]),
    Acc2 = lists:foldl(fun(Opts, Acc0) ->
        {ok, Acc1} =
            couch_mrview_util:fold_reduce(RedView, fun red_fold/3,  Acc0, Opts),
        Acc1
    end, Acc, OptList),
    finish_fold(Acc2, []).

red_fold(_Key, _Red, #mracc{skip=N}=Acc) when N > 0 ->
    {ok, Acc#mracc{skip=N-1, last_go=ok}};
red_fold(Key, Red, #mracc{meta_sent=false}=Acc) ->
    #mracc{
        args=Args,
        callback=Callback,
        user_acc=UAcc0,
        update_seq=UpdateSeq
    } = Acc,
    Meta = make_meta(Args, UpdateSeq, []),
    {Go, UAcc1} = Callback(Meta, UAcc0),
    Acc1 = Acc#mracc{user_acc=UAcc1, meta_sent=true, last_go=Go},
    case Go of
        ok -> red_fold(Key, Red, Acc1);
        _ -> {Go, Acc1}
    end;
red_fold(_Key, _Red, #mracc{limit=0} = Acc) ->
    {stop, Acc};
red_fold(_Key, Red, #mracc{group_level=0} = Acc) ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, null}, {val, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}};
red_fold(Key, Red, #mracc{group_level=exact} = Acc) ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, Key}, {val, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}};
red_fold(K, Red, #mracc{group_level=I} = Acc) when I > 0, is_list(K) ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, lists:sublist(K, I)}, {val, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}};
red_fold(K, Red, #mracc{group_level=I} = Acc) when I > 0 ->
    #mracc{
        limit=Limit,
        callback=Callback,
        user_acc=UAcc0
    } = Acc,
    Row = [{key, K}, {val, Red}],
    {Go, UAcc1} = Callback({row, Row}, UAcc0),
    {Go, Acc#mracc{user_acc=UAcc1, limit=Limit-1, last_go=Go}}.


finish_fold(#mracc{last_go=ok, update_seq=UpdateSeq}=Acc,  ExtraMeta) ->
    #mracc{callback=Callback, user_acc=UAcc, args=Args}=Acc,
    % Possible send meta info
    Meta = make_meta(Args, UpdateSeq, ExtraMeta),
    {Go, UAcc1} = case Acc#mracc.meta_sent of
        false -> Callback(Meta, UAcc);
        _ -> {ok, Acc#mracc.user_acc}
    end,
    % Notify callback that the fold is complete.
    {_, UAcc2} = case Go of
        ok -> Callback(complete, UAcc1);
        _ -> {ok, UAcc1}
    end,
    {ok, UAcc2};
finish_fold(#mracc{user_acc=UAcc}, _ExtraMeta) ->
    {ok, UAcc}.


make_meta(Args, UpdateSeq, Base) ->
    case Args#mrargs.update_seq of
        true -> {meta, Base ++ [{update_seq, UpdateSeq}]};
        _ -> {meta, Base}
    end.


group_rows_fun(exact) ->
    fun({Key1,_}, {Key2,_}) -> Key1 == Key2 end;
group_rows_fun(0) ->
    fun(_A, _B) -> true end;
group_rows_fun(GroupLevel) when is_integer(GroupLevel) ->
    fun({[_|_] = Key1,_}, {[_|_] = Key2,_}) ->
        lists:sublist(Key1, GroupLevel) == lists:sublist(Key2, GroupLevel);
    ({Key1,_}, {Key2,_}) ->
        Key1 == Key2
    end.


default_cb(complete, Acc) ->
    {ok, lists:reverse(Acc)};
default_cb({final, Info}, []) ->
    {ok, [Info]};
default_cb({final, _}, Acc) ->
    {ok, Acc};
default_cb(Row, Acc) ->
    {ok, [Row | Acc]}.


to_mrargs(KeyList) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        Index = lookup_index(couch_util:to_existing_atom(Key)),
        setelement(Index, Acc, Value)
    end, #mrargs{}, KeyList).


lookup_index(Key) ->
    Index = lists:zip(
        record_info(fields, mrargs), lists:seq(2, record_info(size, mrargs))
    ),
    couch_util:get_value(Key, Index).
