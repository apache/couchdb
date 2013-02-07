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

-module(couch_mrview_util).

-export([get_view/4]).
-export([ddoc_to_mrst/2, init_state/4, reset_index/3]).
-export([make_header/1]).
-export([index_file/2, compaction_file/2, open_file/1]).
-export([delete_files/2, delete_index_file/2, delete_compaction_file/2]).
-export([get_row_count/1, all_docs_reduce_to_count/1, reduce_to_count/1]).
-export([all_docs_key_opts/1, all_docs_key_opts/2, key_opts/1, key_opts/2]).
-export([fold/4, fold_reduce/4]).
-export([temp_view_to_ddoc/1]).
-export([calculate_data_size/2]).
-export([validate_args/1]).
-export([maybe_load_doc/3, maybe_load_doc/4]).
-export([maybe_update_index_file/1]).

-define(MOD, couch_mrview_index).

-include("couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


get_view(Db, DDoc, ViewName, Args0) ->
    ArgCheck = fun(InitState) ->
        Args1 = set_view_type(Args0, ViewName, InitState#mrst.views),
        {ok, validate_args(Args1)}
    end,
    {ok, Pid, Args2} = couch_index_server:get_index(?MOD, Db, DDoc, ArgCheck),
    DbUpdateSeq = couch_util:with_db(Db, fun(WDb) ->
        couch_db:get_update_seq(WDb)
    end),
    MinSeq = case Args2#mrargs.stale of
        ok -> 0; update_after -> 0; _ -> DbUpdateSeq
    end,
    {ok, State} = case couch_index:get_state(Pid, MinSeq) of
        {ok, _} = Resp -> Resp;
        Error -> throw(Error)
    end,
    couch_ref_counter:add(State#mrst.refc),
    if Args2#mrargs.stale == update_after ->
        spawn(fun() -> catch couch_index:get_state(Pid, DbUpdateSeq) end);
        true -> ok
    end,
    #mrst{language=Lang, views=Views} = State,
    {Type, View, Args3} = extract_view(Lang, Args2, ViewName, Views),
    check_range(Args3, view_cmp(View)),
    Sig = view_sig(Db, State, View, Args3),
    {ok, {Type, View}, Sig, Args3}.


ddoc_to_mrst(DbName, #doc{id=Id, body={Fields}}) ->
    MakeDict = fun({Name, {MRFuns}}, DictBySrcAcc) ->
        case couch_util:get_value(<<"map">>, MRFuns) of
            MapSrc when is_binary(MapSrc) ->
                RedSrc = couch_util:get_value(<<"reduce">>, MRFuns, null),
                {ViewOpts} = couch_util:get_value(<<"options">>, MRFuns, {[]}),
                View = case dict:find({MapSrc, ViewOpts}, DictBySrcAcc) of
                    {ok, View0} -> View0;
                    error -> #mrview{def=MapSrc, options=ViewOpts}
                end,
                {MapNames, RedSrcs} = case RedSrc of
                    null ->
                        MNames = [Name | View#mrview.map_names],
                        {MNames, View#mrview.reduce_funs};
                    _ ->
                        RedFuns = [{Name, RedSrc} | View#mrview.reduce_funs],
                        {View#mrview.map_names, RedFuns}
                end,
                View2 = View#mrview{map_names=MapNames, reduce_funs=RedSrcs},
                dict:store({MapSrc, ViewOpts}, View2, DictBySrcAcc);
            undefined ->
                DictBySrcAcc
        end
    end,
    {RawViews} = couch_util:get_value(<<"views">>, Fields, {[]}),
    BySrc = lists:foldl(MakeDict, dict:new(), RawViews),

    NumViews = fun({_, View}, N) -> {View#mrview{id_num=N}, N+1} end,
    {Views, _} = lists:mapfoldl(NumViews, 0, lists:sort(dict:to_list(BySrc))),

    Language = couch_util:get_value(<<"language">>, Fields, <<"javascript">>),
    {DesignOpts} = couch_util:get_value(<<"options">>, Fields, {[]}),
    {RawViews} = couch_util:get_value(<<"views">>, Fields, {[]}),
    Lib = couch_util:get_value(<<"lib">>, RawViews, {[]}),

    IdxState = #mrst{
        db_name=DbName,
        idx_name=Id,
        lib=Lib,
        views=Views,
        language=Language,
        design_opts=DesignOpts
    },
    SigInfo = {Views, Language, DesignOpts, couch_index_util:sort_lib(Lib)},
    {ok, IdxState#mrst{sig=couch_util:md5(term_to_binary(SigInfo))}}.


set_view_type(_Args, _ViewName, []) ->
    throw({not_found, missing_named_view});
set_view_type(Args, ViewName, [View | Rest]) ->
    RedNames = [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(ViewName, RedNames) of
        true ->
            case Args#mrargs.reduce of
                false -> Args#mrargs{view_type=map};
                _ -> Args#mrargs{view_type=red}
            end;
        false ->
            case lists:member(ViewName, View#mrview.map_names) of
                true -> Args#mrargs{view_type=map};
                false -> set_view_type(Args, ViewName, Rest)
            end
    end.


extract_view(_Lang, _Args, _ViewName, []) ->
    throw({not_found, missing_named_view});
extract_view(Lang, #mrargs{view_type=map}=Args, Name, [View | Rest]) ->
    Names = View#mrview.map_names ++ [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(Name, Names) of
        true -> {map, View, Args};
        _ -> extract_view(Lang, Args, Name, Rest)
    end;
extract_view(Lang, #mrargs{view_type=red}=Args, Name, [View | Rest]) ->
    RedNames = [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(Name, RedNames) of
        true -> {red, {index_of(Name, RedNames), Lang, View}, Args};
        false -> extract_view(Lang, Args, Name, Rest)
    end.


view_sig(Db, State, View, #mrargs{include_docs=true}=Args) ->
    BaseSig = view_sig(Db, State, View, Args#mrargs{include_docs=false}),
    UpdateSeq = couch_db:get_update_seq(Db),
    PurgeSeq = couch_db:get_purge_seq(Db),
    Bin = term_to_binary({BaseSig, UpdateSeq, PurgeSeq}),
    couch_index_util:hexsig(couch_util:md5(Bin));
view_sig(Db, State, {_Nth, _Lang, View}, Args) ->
    view_sig(Db, State, View, Args);
view_sig(_Db, State, View, Args0) ->
    Sig = State#mrst.sig,
    UpdateSeq = View#mrview.update_seq,
    PurgeSeq = View#mrview.purge_seq,
    Args = Args0#mrargs{
        preflight_fun=undefined,
        extra=[]
    },
    Bin = term_to_binary({Sig, UpdateSeq, PurgeSeq, Args}),
    couch_index_util:hexsig(couch_util:md5(Bin)).


init_state(Db, Fd, #mrst{views=Views}=State, nil) ->
    Header = #mrheader{
        seq=0,
        purge_seq=couch_db:get_purge_seq(Db),
        id_btree_state=nil,
        view_states=[{nil, 0, 0} || _ <- Views]
    },
    init_state(Db, Fd, State, Header);
% read <= 1.2.x header record and transpile it to >=1.3.x
% header record
init_state(Db, Fd, State, #index_header{
    seq=Seq,
    purge_seq=PurgeSeq,
    id_btree_state=IdBtreeState,
    view_states=ViewStates}) ->
    init_state(Db, Fd, State, #mrheader{
        seq=Seq,
        purge_seq=PurgeSeq,
        id_btree_state=IdBtreeState,
        view_states=ViewStates
        });
init_state(Db, Fd, State, Header) ->
    #mrst{language=Lang, views=Views} = State,
    #mrheader{
        seq=Seq,
        purge_seq=PurgeSeq,
        id_btree_state=IdBtreeState,
        view_states=ViewStates
    } = Header,

    StateUpdate = fun
        ({_, _, _}=St) -> St;
        (St) -> {St, 0, 0}
    end,
    ViewStates2 = lists:map(StateUpdate, ViewStates),

    IdBtOpts = [{compression, couch_db:compression(Db)}],
    {ok, IdBtree} = couch_btree:open(IdBtreeState, Fd, IdBtOpts),

    OpenViewFun = fun(St, View) -> open_view(Db, Fd, Lang, St, View) end,
    Views2 = lists:zipwith(OpenViewFun, ViewStates2, Views),

    State#mrst{
        fd=Fd,
        update_seq=Seq,
        purge_seq=PurgeSeq,
        id_btree=IdBtree,
        views=Views2
    }.


open_view(Db, Fd, Lang, {BTState, USeq, PSeq}, View) ->
    FunSrcs = [FunSrc || {_Name, FunSrc} <- View#mrview.reduce_funs],
    ReduceFun =
        fun(reduce, KVs) ->
            KVs2 = detuple_kvs(expand_dups(KVs, []), []),
            {ok, Result} = couch_query_servers:reduce(Lang, FunSrcs, KVs2),
            {length(KVs2), Result};
        (rereduce, Reds) ->
            Count = lists:sum([Count0 || {Count0, _} <- Reds]),
            UsrReds = [UsrRedsList || {_, UsrRedsList} <- Reds],
            {ok, Result} = couch_query_servers:rereduce(Lang, FunSrcs, UsrReds),
            {Count, Result}
        end,

    Less = case couch_util:get_value(<<"collation">>, View#mrview.options) of
        <<"raw">> -> fun(A, B) -> A < B end;
        _ -> fun couch_ejson_compare:less_json_ids/2
    end,

    ViewBtOpts = [
        {less, Less},
        {reduce, ReduceFun},
        {compression, couch_db:compression(Db)}
    ],
    {ok, Btree} = couch_btree:open(BTState, Fd, ViewBtOpts),
    View#mrview{btree=Btree, update_seq=USeq, purge_seq=PSeq}.


temp_view_to_ddoc({Props}) ->
    Language = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    Options = couch_util:get_value(<<"options">>, Props, {[]}),
    View0 = [{<<"map">>, couch_util:get_value(<<"map">>, Props)}],
    View1 = View0 ++ case couch_util:get_value(<<"reduce">>, Props) of
        RedSrc when is_binary(RedSrc) -> [{<<"reduce">>, RedSrc}];
        _ -> []
    end,
    DDoc = {[
        {<<"_id">>, couch_uuids:random()},
        {<<"language">>, Language},
        {<<"options">>, Options},
        {<<"views">>, {[
            {<<"temp">>, {View1}}
        ]}}
    ]},
    couch_doc:from_json_obj(DDoc).


get_row_count(#mrview{btree=Bt}) ->
    {ok, {Count, _Reds}} = couch_btree:full_reduce(Bt),
    {ok, Count}.


all_docs_reduce_to_count(Reductions) ->
    Reduce = fun couch_db_updater:btree_by_id_reduce/2,
    {Count, _, _} = couch_btree:final_reduce(Reduce, Reductions),
    Count.

reduce_to_count(nil) ->
    0;
reduce_to_count(Reductions) ->
    Reduce = fun
        (reduce, KVs) ->
            Counts = [
                case V of {dups, Vals} -> length(Vals); _ -> 1 end
                || {_,V} <- KVs
            ],
            {lists:sum(Counts), []};
        (rereduce, Reds) ->
            {lists:sum([Count0 || {Count0, _} <- Reds]), []}
    end,
    {Count, _} = couch_btree:final_reduce(Reduce, Reductions),
    Count.


fold(#mrview{btree=Bt}, Fun, Acc, Opts) ->
    WrapperFun = fun(KV, Reds, Acc2) ->
        fold_fun(Fun, expand_dups([KV], []), Reds, Acc2)
    end,
    {ok, _LastRed, _Acc} = couch_btree:fold(Bt, WrapperFun, Acc, Opts).


fold_fun(_Fun, [], _, Acc) ->
    {ok, Acc};
fold_fun(Fun, [KV|Rest], {KVReds, Reds}, Acc) ->
    case Fun(KV, {KVReds, Reds}, Acc) of
        {ok, Acc2} ->
            fold_fun(Fun, Rest, {[KV|KVReds], Reds}, Acc2);
        {stop, Acc2} ->
            {stop, Acc2}
    end.


fold_reduce({NthRed, Lang, View}, Fun,  Acc, Options) ->
    #mrview{
        btree=Bt,
        reduce_funs=RedFuns
    } = View,
    LPad = lists:duplicate(NthRed - 1, []),
    RPad = lists:duplicate(length(RedFuns) - NthRed, []),
    {_Name, FunSrc} = lists:nth(NthRed,RedFuns),

    ReduceFun = fun
        (reduce, KVs0) ->
            KVs1 = detuple_kvs(expand_dups(KVs0, []), []),
            {ok, Red} = couch_query_servers:reduce(Lang, [FunSrc], KVs1),
            {0, LPad ++ Red ++ RPad};
        (rereduce, Reds) ->
            ExtractRed = fun({_, UReds0}) -> [lists:nth(NthRed, UReds0)] end,
            UReds = lists:map(ExtractRed, Reds),
            {ok, Red} = couch_query_servers:rereduce(Lang, [FunSrc], UReds),
            {0, LPad ++ Red ++ RPad}
    end,

    WrapperFun = fun({GroupedKey, _}, PartialReds, Acc0) ->
        {_, Reds} = couch_btree:final_reduce(ReduceFun, PartialReds),
        Fun(GroupedKey, lists:nth(NthRed, Reds), Acc0)
    end,

    couch_btree:fold_reduce(Bt, WrapperFun, Acc, Options).


validate_args(Args) ->
    Reduce = Args#mrargs.reduce,
    case Reduce == undefined orelse is_boolean(Reduce) of
        true -> ok;
        _ -> mrverror(<<"Invalid `reduce` value.">>)
    end,

    case {Args#mrargs.view_type, Reduce} of
        {map, true} -> mrverror(<<"Reduce is invalid for map-only views.">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, Args#mrargs.group_level, Args#mrargs.keys} of
        {red, exact, _} -> ok;
        {red, _, KeyList} when is_list(KeyList) ->
            Msg = <<"Multi-key fetchs for reduce views must use `group=true`">>,
            mrverror(Msg);
        _ -> ok
    end,

    case Args#mrargs.keys of
        Keys when is_list(Keys) -> ok;
        undefined -> ok;
        _ -> mrverror(<<"`keys` must be an array of strings.">>)
    end,

    case {Args#mrargs.keys, Args#mrargs.start_key} of
        {undefined, _} -> ok;
        {[], _} -> ok;
        {[_|_], undefined} -> ok;
        _ -> mrverror(<<"`start_key` is incompatible with `keys`">>)
    end,

    case Args#mrargs.start_key_docid of
        undefined -> ok;
        SKDocId0 when is_binary(SKDocId0) -> ok;
        _ -> mrverror(<<"`start_key_docid` must be a string.">>)
    end,

    case {Args#mrargs.keys, Args#mrargs.end_key} of
        {undefined, _} -> ok;
        {[], _} -> ok;
        {[_|_], undefined} -> ok;
        _ -> mrverror(<<"`end_key` is incompatible with `keys`">>)
    end,

    case Args#mrargs.end_key_docid of
        undefined -> ok;
        EKDocId0 when is_binary(EKDocId0) -> ok;
        _ -> mrverror(<<"`end_key_docid` must be a string.">>)
    end,

    case Args#mrargs.direction of
        fwd -> ok;
        rev -> ok;
        _ -> mrverror(<<"Invalid direction.">>)
    end,

    case {Args#mrargs.limit >= 0, Args#mrargs.limit == undefined} of
        {true, _} -> ok;
        {_, true} -> ok;
        _ -> mrverror(<<"`limit` must be a positive integer.">>)
    end,

    case Args#mrargs.skip < 0 of
        true -> mrverror(<<"`skip` must be >= 0">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, Args#mrargs.group_level} of
        {red, exact} -> ok;
        {_, 0} -> ok;
        {red, Int} when is_integer(Int), Int >= 0 -> ok;
        {red, _} -> mrverror(<<"`group_level` must be >= 0">>);
        {map, _} -> mrverror(<<"Invalid use of grouping on a map view.">>)
    end,

    case Args#mrargs.stale of
        ok -> ok;
        update_after -> ok;
        false -> ok;
        _ -> mrverror(<<"Invalid value for `stale`.">>)
    end,

    case is_boolean(Args#mrargs.inclusive_end) of
        true -> ok;
        _ -> mrverror(<<"Invalid value for `inclusive_end`.">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.include_docs} of
        {red, true} -> mrverror(<<"`include_docs` is invalid for reduce">>);
        {_, ID} when is_boolean(ID) -> ok;
        _ -> mrverror(<<"Invalid value for `include_docs`">>)
    end,

    case {Args#mrargs.view_type, Args#mrargs.conflicts} of
        {_, undefined} -> ok;
        {map, V} when is_boolean(V) -> ok;
        {red, undefined} -> ok;
        {map, _} -> mrverror(<<"Invalid value for `conflicts`.">>);
        {red, _} -> mrverror(<<"`conflicts` is invalid for reduce views.">>)
    end,

    SKDocId = case {Args#mrargs.direction, Args#mrargs.start_key_docid} of
        {fwd, undefined} -> <<>>;
        {rev, undefined} -> <<255>>;
        {_, SKDocId1} -> SKDocId1
    end,

    EKDocId = case {Args#mrargs.direction, Args#mrargs.end_key_docid} of
        {fwd, undefined} -> <<255>>;
        {rev, undefined} -> <<>>;
        {_, EKDocId1} -> EKDocId1
    end,

    Args#mrargs{
        start_key_docid=SKDocId,
        end_key_docid=EKDocId
    }.


check_range(#mrargs{start_key=undefined}, _Cmp) ->
    ok;
check_range(#mrargs{end_key=undefined}, _Cmp) ->
    ok;
check_range(#mrargs{start_key=K, end_key=K}, _Cmp) ->
    ok;
check_range(Args, Cmp) ->
    #mrargs{
        direction=Dir,
        start_key=SK,
        start_key_docid=SKD,
        end_key=EK,
        end_key_docid=EKD
    } = Args,
    case {Dir, Cmp({SK, SKD}, {EK, EKD})} of
        {fwd, false} ->
            throw({query_parse_error,
                <<"No rows can match your key range, reverse your ",
                    "start_key and end_key or set descending=true">>});
        {rev, true} ->
            throw({query_parse_error,
                <<"No rows can match your key range, reverse your ",
                    "start_key and end_key or set descending=false">>});
        _ -> ok
    end.


view_cmp({_Nth, _Lang, View}) ->
    view_cmp(View);
view_cmp(View) ->
    fun(A, B) -> couch_btree:less(View#mrview.btree, A, B) end.


make_header(State) ->
    #mrst{
        update_seq=Seq,
        purge_seq=PurgeSeq,
        id_btree=IdBtree,
        views=Views
    } = State,
    ViewStates = [
        {
            couch_btree:get_state(V#mrview.btree),
            V#mrview.update_seq,
            V#mrview.purge_seq
        }
        ||
        V <- Views
    ],
    #mrheader{
        seq=Seq,
        purge_seq=PurgeSeq,
        id_btree_state=couch_btree:get_state(IdBtree),
        view_states=ViewStates
    }.


index_file(DbName, Sig) ->
    FileName = couch_index_util:hexsig(Sig) ++ ".view",
    couch_index_util:index_file(mrview, DbName, FileName).


compaction_file(DbName, Sig) ->
    FileName = couch_index_util:hexsig(Sig) ++ ".compact.view",
    couch_index_util:index_file(mrview, DbName, FileName).


open_file(FName) ->
    case couch_file:open(FName, [nologifmissing]) of
        {ok, Fd} -> {ok, Fd};
        {error, enoent} -> couch_file:open(FName, [create]);
        Error -> Error
    end.


delete_files(DbName, Sig) ->
    delete_index_file(DbName, Sig),
    delete_compaction_file(DbName, Sig).


delete_index_file(DbName, Sig) ->
    delete_file(index_file(DbName, Sig)).


delete_compaction_file(DbName, Sig) ->
    delete_file(compaction_file(DbName, Sig)).
    

delete_file(FName) ->
    case filelib:is_file(FName) of
        true ->
            RootDir = couch_index_util:root_dir(),
            couch_file:delete(RootDir, FName);
        _ ->
            ok
    end.


reset_index(Db, Fd, #mrst{sig=Sig}=State) ->
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, {Sig, nil}),
    init_state(Db, Fd, reset_state(State), nil).


reset_state(State) ->
    State#mrst{
        fd=nil,
        qserver=nil,
        update_seq=0,
        id_btree=nil,
        views=[View#mrview{btree=nil} || View <- State#mrst.views]
    }.


all_docs_key_opts(Args) ->
    all_docs_key_opts(Args, []).


all_docs_key_opts(#mrargs{keys=undefined}=Args, Extra) ->
    all_docs_key_opts(Args#mrargs{keys=[]}, Extra);
all_docs_key_opts(#mrargs{keys=[], direction=Dir}=Args, Extra) ->
    [[{dir, Dir}] ++ ad_skey_opts(Args) ++ ad_ekey_opts(Args) ++ Extra];
all_docs_key_opts(#mrargs{keys=Keys, direction=Dir}=Args, Extra) ->
    lists:map(fun(K) ->
        [{dir, Dir}]
        ++ ad_skey_opts(Args#mrargs{start_key=K})
        ++ ad_ekey_opts(Args#mrargs{end_key=K})
        ++ Extra
    end, Keys).


ad_skey_opts(#mrargs{start_key=SKey}) when is_binary(SKey) ->
    [{start_key, SKey}];
ad_skey_opts(#mrargs{start_key_docid=SKeyDocId}) ->
    [{start_key, SKeyDocId}].


ad_ekey_opts(#mrargs{end_key=EKey}=Args) when is_binary(EKey) ->
    Type = if Args#mrargs.inclusive_end -> end_key; true -> end_key_gt end,
    [{Type, EKey}];
ad_ekey_opts(#mrargs{end_key_docid=EKeyDocId}=Args) ->
    Type = if Args#mrargs.inclusive_end -> end_key; true -> end_key_gt end,
    [{Type, EKeyDocId}].


key_opts(Args) ->
    key_opts(Args, []).

key_opts(#mrargs{keys=undefined, direction=Dir}=Args, Extra) ->
    [[{dir, Dir}] ++ skey_opts(Args) ++ ekey_opts(Args) ++ Extra];
key_opts(#mrargs{keys=Keys, direction=Dir}=Args, Extra) ->
    lists:map(fun(K) ->
        [{dir, Dir}]
        ++ skey_opts(Args#mrargs{start_key=K})
        ++ ekey_opts(Args#mrargs{end_key=K})
        ++ Extra
    end, Keys).


skey_opts(#mrargs{start_key=undefined}) ->
    [];
skey_opts(#mrargs{start_key=SKey, start_key_docid=SKeyDocId}) ->
    [{start_key, {SKey, SKeyDocId}}].


ekey_opts(#mrargs{end_key=undefined}) ->
    [];
ekey_opts(#mrargs{end_key=EKey, end_key_docid=EKeyDocId}=Args) ->
    case Args#mrargs.inclusive_end of
        true -> [{end_key, {EKey, EKeyDocId}}];
        false -> [{end_key_gt, {EKey, reverse_key_default(EKeyDocId)}}]
    end.


reverse_key_default(<<>>) -> <<255>>;
reverse_key_default(<<255>>) -> <<>>;
reverse_key_default(Key) -> Key.


calculate_data_size(IdBt, Views) ->
    SumFun = fun(#mrview{btree=Bt}, Acc) ->
        sum_btree_sizes(Acc, couch_btree:size(Bt))
    end,
    Size = lists:foldl(SumFun, couch_btree:size(IdBt), Views),
    {ok, Size}.


sum_btree_sizes(nil, _) ->
    null;
sum_btree_sizes(_, nil) ->
    null;
sum_btree_sizes(Size1, Size2) ->
    Size1 + Size2.


detuple_kvs([], Acc) ->
    lists:reverse(Acc);
detuple_kvs([KV | Rest], Acc) ->
    {{Key,Id},Value} = KV,
    NKV = [[Key, Id], Value],
    detuple_kvs(Rest, [NKV | Acc]).


expand_dups([], Acc) ->
    lists:reverse(Acc);
expand_dups([{Key, {dups, Vals}} | Rest], Acc) ->
    Expanded = [{Key, Val} || Val <- Vals],
    expand_dups(Rest, Expanded ++ Acc);
expand_dups([KV | Rest], Acc) ->
    expand_dups(Rest, [KV | Acc]).


maybe_load_doc(_Db, _DI, #mrargs{include_docs=false}) ->
    [];
maybe_load_doc(Db, #doc_info{}=DI, #mrargs{conflicts=true}) ->
    doc_row(couch_index_util:load_doc(Db, DI, [conflicts]));
maybe_load_doc(Db, #doc_info{}=DI, _Args) ->
    doc_row(couch_index_util:load_doc(Db, DI, [])).


maybe_load_doc(_Db, _Id, _Val, #mrargs{include_docs=false}) ->
    [];
maybe_load_doc(Db, Id, Val, #mrargs{conflicts=true}) ->
    doc_row(couch_index_util:load_doc(Db, docid_rev(Id, Val), [conflicts]));
maybe_load_doc(Db, Id, Val, _Args) ->
    doc_row(couch_index_util:load_doc(Db, docid_rev(Id, Val), [])).


doc_row(null) ->
    [{doc, null}];
doc_row(Doc) ->
    [{doc, couch_doc:to_json_obj(Doc, [])}].


docid_rev(Id, {Props}) ->
    DocId = couch_util:get_value(<<"_id">>, Props, Id),
    Rev = case couch_util:get_value(<<"_rev">>, Props, nil) of
        nil -> nil;
        Rev0 -> couch_doc:parse_rev(Rev0)
    end,
    {DocId, Rev};
docid_rev(Id, _) ->
    {Id, nil}.


index_of(Key, List) ->
    index_of(Key, List, 1).


index_of(_, [], _) ->
    throw({error, missing_named_view});
index_of(Key, [Key | _], Idx) ->
    Idx;
index_of(Key, [_ | Rest], Idx) ->
    index_of(Key, Rest, Idx+1).


mrverror(Mesg) ->
    throw({query_parse_error, Mesg}).


%% Updates 1.2.x or earlier view files to 1.3.x or later view files
%% transparently, the first time the 1.2.x view file is opened by
%% 1.3.x or later.
%%
%% Here's how it works:
%%
%% Before opening a view index,
%% If no matching index file is found in the new location:
%%  calculate the <= 1.2.x view signature
%%  if a file with that signature lives in the old location
%%    rename it to the new location with the new signature in the name.
%% Then proceed to open the view index as usual.
%% After opening, read its header.
%%
%% If the header matches the <= 1.2.x style #index_header record:
%%   upgrade the header to the new #mrheader record
%% The next time the view is used, the new header is used.
%%
%% If we crash after the rename, but before the header upgrade,
%%   the header upgrade is done on the next view opening.
%%
%% If we crash between upgrading to the new header and writing
%%   that header to disk, we start with the old header again,
%%   do the upgrade and write to disk.

maybe_update_index_file(State) ->
    DbName = State#mrst.db_name,
    NewIndexFile = index_file(DbName, State#mrst.sig),
    % open in read-only mode so we don't create
    % the file if it doesn't exist.
    case file:open(NewIndexFile, [read, raw]) of
    {ok, Fd_Read} ->
        % the new index file exists, there is nothing to do here.
        file:close(Fd_Read);
    _Error ->
        update_index_file(State)
    end.

update_index_file(State) ->
    Sig = sig_vsn_12x(State),
    DbName = State#mrst.db_name,
    FileName = couch_index_util:hexsig(Sig) ++ ".view",
    IndexFile = couch_index_util:index_file("", DbName, FileName),

    % If we have an old index, rename it to the new position.
    case file:read_file_info(IndexFile) of
    {ok, _FileInfo} ->
        % Crash if the rename fails for any reason.
        % If the target exists, e.g. the next request will find the
        % new file and we are good. We might need to catch this
        % further up to avoid a full server crash.
        ?LOG_INFO("Attempting to update legacy view index file.", []),
        NewIndexFile = index_file(DbName, State#mrst.sig),
        ok = filelib:ensure_dir(NewIndexFile),
        ok = file:rename(IndexFile, NewIndexFile),
        ?LOG_INFO("Successfully updated legacy view index file.", []),
        Sig;
    _ ->
        % Ignore missing index file
        ok
    end.

sig_vsn_12x(State) ->
    ViewInfo = [old_view_format(V) || V <- State#mrst.views],
    SigData = case State#mrst.lib of
    {[]} ->
        {ViewInfo, State#mrst.language, State#mrst.design_opts};
    _ ->
        {ViewInfo, State#mrst.language, State#mrst.design_opts,
            couch_index_util:sort_lib(State#mrst.lib)}
    end,
    couch_util:md5(term_to_binary(SigData)).

old_view_format(View) ->
{
    view,
    View#mrview.id_num,
    View#mrview.map_names,
    View#mrview.def,
    View#mrview.btree,
    View#mrview.reduce_funs,
    View#mrview.options
}.

%% End of <= 1.2.x upgrade code.
