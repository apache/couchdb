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

-export([get_view/4, get_view_index_pid/4]).
-export([get_local_purge_doc_id/1, get_value_from_options/2]).
-export([verify_view_filename/1, get_signature_from_filename/1]).
-export([get_signatures/1, get_purge_checkpoints/1, get_index_files/1]).
-export([ddoc_to_mrst/2, init_state/4, reset_index/3]).
-export([make_header/1]).
-export([index_file/2, compaction_file/2, open_file/1]).
-export([delete_files/2, delete_index_file/2, delete_compaction_file/2]).
-export([get_row_count/1, all_docs_reduce_to_count/1, reduce_to_count/1]).
-export([all_docs_key_opts/1, all_docs_key_opts/2, key_opts/1, key_opts/2]).
-export([fold/4, fold_reduce/4]).
-export([temp_view_to_ddoc/1]).
-export([calculate_external_size/1]).
-export([calculate_active_size/1]).
-export([validate_all_docs_args/2, validate_args/1, validate_args/3]).
-export([maybe_load_doc/3, maybe_load_doc/4]).
-export([maybe_update_index_file/1]).
-export([extract_view/4, extract_view_reduce/1]).
-export([get_view_keys/1, get_view_queries/1]).
-export([set_view_type/3]).
-export([set_extra/3, get_extra/2, get_extra/3]).
-export([get_collator_versions/1]).
-export([compact_on_collator_upgrade/0]).
-export([commit_on_header_upgrade/0]).

-define(MOD, couch_mrview_index).
-define(GET_VIEW_RETRY_COUNT, 1).
-define(GET_VIEW_RETRY_DELAY, 50).
-define(LOWEST_KEY, null).
-define(HIGHEST_KEY, {<<255, 255, 255, 255>>}).
-define(LOWEST(A, B),
    (if
        A < B -> A;
        true -> B
    end)
).
-define(HIGHEST(A, B),
    (if
        A > B -> A;
        true -> B
    end)
).
-define(IS_HEX(C),
    ((C >= $0 andalso C =< $9) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $A andalso C =< $F))
).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

get_local_purge_doc_id(Sig) ->
    ?l2b(?LOCAL_DOC_PREFIX ++ "purge-mrview-" ++ Sig).

get_value_from_options(Key, Options) ->
    case couch_util:get_value(Key, Options) of
        undefined ->
            Reason = <<"'", Key/binary, "' must exists in options.">>,
            throw({bad_request, Reason});
        Value ->
            Value
    end.

verify_view_filename(FileName) ->
    case filename:extension(FileName) of
        ".view" ->
            Sig = get_signature_from_filename(FileName),
            lists:all(fun(C) -> ?IS_HEX(C) end, Sig);
        _ ->
            false
    end.

get_signature_from_filename(Path) ->
    filename:basename(filename:basename(Path, ".view"), ".compact").

% Returns map of `Sig => true` elements with all the active signatures.
% Sig is a hex-encoded binary.
%
get_signatures(DbName) when is_binary(DbName) ->
    couch_util:with_db(DbName, fun get_signatures/1);
get_signatures(Db) ->
    DbName = couch_db:name(Db),
    % get_design_docs/1 returns ejson for clustered shards, and
    % #full_doc_info{}'s for other cases.
    {ok, DDocs} = couch_db:get_design_docs(Db),
    FoldFun = fun
        ({[_ | _]} = EJsonDoc, Acc) ->
            Doc = couch_doc:from_json_obj(EJsonDoc),
            {ok, Mrst} = ddoc_to_mrst(DbName, Doc),
            Sig = couch_util:to_hex_bin(Mrst#mrst.sig),
            Acc#{Sig => true};
        (#full_doc_info{} = FDI, Acc) ->
            {ok, Doc} = couch_db:open_doc_int(Db, FDI, [ejson_body]),
            {ok, Mrst} = ddoc_to_mrst(DbName, Doc),
            Sig = couch_util:to_hex_bin(Mrst#mrst.sig),
            Acc#{Sig => true}
    end,
    lists:foldl(FoldFun, #{}, DDocs).

% Returns a map of `Sig => DocId` elements for all the purge view
% checkpoint docs. Sig is a hex-encoded binary.
%
get_purge_checkpoints(DbName) when is_binary(DbName) ->
    couch_util:with_db(DbName, fun get_purge_checkpoints/1);
get_purge_checkpoints(Db) ->
    FoldFun = fun(#doc{id = Id}, Acc) ->
        case Id of
            <<?LOCAL_DOC_PREFIX, "purge-mrview-", Sig/binary>> ->
                {ok, Acc#{Sig => Id}};
            _ ->
                {stop, Acc}
        end
    end,
    Opts = [{start_key, <<?LOCAL_DOC_PREFIX, "purge-mrview-">>}],
    {ok, Signatures = #{}} = couch_db:fold_local_docs(Db, FoldFun, #{}, Opts),
    Signatures.

% Returns a map of `Sig => [FilePath, ...]` elements. Sig is a hex-encoded
% binary and FilePaths are lists as they intended to be passed to couch_file
% and file module functions.
%
get_index_files(DbName) when is_binary(DbName) ->
    IdxDir = couch_index_util:index_dir(mrview, DbName),
    WildcardPath = filename:join(IdxDir, "*"),
    FoldFun = fun(F, Acc) ->
        case verify_view_filename(F) of
            true ->
                Sig = ?l2b(get_signature_from_filename(F)),
                maps:update_with(Sig, fun(Fs) -> [F | Fs] end, [F], Acc);
            false ->
                Acc
        end
    end,
    lists:foldl(FoldFun, #{}, filelib:wildcard(WildcardPath));
get_index_files(Db) ->
    get_index_files(couch_db:name(Db)).

get_view(Db, DDoc, ViewName, Args0) ->
    case get_view_index_state(Db, DDoc, ViewName, Args0) of
        {ok, State, Args2} ->
            Ref = erlang:monitor(process, State#mrst.fd),
            #mrst{language = Lang, views = Views} = State,
            {Type, View, Args3} = extract_view(Lang, Args2, ViewName, Views),
            check_range(Args3, view_cmp(View)),
            Sig = view_sig(Db, State, View, Args3),
            {ok, {Type, View, Ref}, Sig, Args3};
        ddoc_updated ->
            ddoc_updated
    end.

get_view_index_pid(Db, DDoc, ViewName, Args0) ->
    ArgCheck = fun(InitState) ->
        Args1 = set_view_type(Args0, ViewName, InitState#mrst.views),
        {ok, validate_args(InitState, Args1)}
    end,
    couch_index_server:get_index(?MOD, Db, DDoc, ArgCheck).

get_view_index_state(Db, DDoc, ViewName, Args0) ->
    get_view_index_state(Db, DDoc, ViewName, Args0, ?GET_VIEW_RETRY_COUNT).

get_view_index_state(_, DDoc, _, _, RetryCount) when RetryCount < 0 ->
    couch_log:warning("DDoc '~s' recreated too frequently", [DDoc#doc.id]),
    throw({get_view_state, exceeded_retry_count});
get_view_index_state(Db, DDoc, ViewName, Args0, RetryCount) ->
    try
        {ok, Pid, Args} = get_view_index_pid(Db, DDoc, ViewName, Args0),
        UpdateSeq = couch_util:with_db(Db, fun(WDb) ->
            couch_db:get_update_seq(WDb)
        end),
        State =
            case Args#mrargs.update of
                lazy ->
                    spawn(fun() ->
                        catch couch_index:get_state(Pid, UpdateSeq)
                    end),
                    couch_index:get_state(Pid, 0);
                false ->
                    couch_index:get_state(Pid, 0);
                _ ->
                    couch_index:get_state(Pid, UpdateSeq)
            end,
        case State of
            {ok, State0} -> {ok, State0, Args};
            ddoc_updated -> ddoc_updated;
            Else -> throw(Else)
        end
    catch
        exit:{Reason, _} when Reason == noproc; Reason == normal ->
            timer:sleep(?GET_VIEW_RETRY_DELAY),
            get_view_index_state(Db, DDoc, ViewName, Args0, RetryCount - 1);
        error:{badmatch, Error} ->
            throw(Error);
        Error ->
            throw(Error)
    end.

ddoc_to_mrst(DbName, #doc{id = Id, body = {Fields}}) ->
    MakeDict = fun
        ({Name, {MRFuns}}, DictBySrcAcc) ->
            case couch_util:get_value(<<"map">>, MRFuns) of
                MapSrc when MapSrc /= undefined ->
                    RedSrc = couch_util:get_value(<<"reduce">>, MRFuns, null),
                    {ViewOpts} = couch_util:get_value(<<"options">>, MRFuns, {[]}),
                    View =
                        case dict:find({MapSrc, ViewOpts}, DictBySrcAcc) of
                            {ok, View0} -> View0;
                            error -> #mrview{def = MapSrc, options = ViewOpts}
                        end,
                    {MapNames, RedSrcs} =
                        case RedSrc of
                            null ->
                                MNames = [Name | View#mrview.map_names],
                                {MNames, View#mrview.reduce_funs};
                            _ ->
                                RedFuns = [{Name, RedSrc} | View#mrview.reduce_funs],
                                {View#mrview.map_names, RedFuns}
                        end,
                    View2 = View#mrview{map_names = MapNames, reduce_funs = RedSrcs},
                    dict:store({MapSrc, ViewOpts}, View2, DictBySrcAcc);
                undefined ->
                    DictBySrcAcc
            end;
        ({Name, Else}, DictBySrcAcc) ->
            couch_log:error(
                "design_doc_to_view_group ~s views ~p",
                [Name, Else]
            ),
            DictBySrcAcc
    end,
    {DesignOpts} = proplists:get_value(<<"options">>, Fields, {[]}),
    Partitioned = proplists:get_value(<<"partitioned">>, DesignOpts, false),

    {RawViews} = couch_util:get_value(<<"views">>, Fields, {[]}),
    BySrc = lists:foldl(MakeDict, dict:new(), RawViews),

    NumViews = fun({_, View}, N) ->
        {View#mrview{id_num = N}, N + 1}
    end,
    {Views, _} = lists:mapfoldl(NumViews, 0, lists:sort(dict:to_list(BySrc))),

    Language = couch_util:get_value(<<"language">>, Fields, <<"javascript">>),
    Lib = couch_util:get_value(<<"lib">>, RawViews, {[]}),

    IdxState = #mrst{
        db_name = DbName,
        idx_name = Id,
        lib = Lib,
        views = Views,
        language = Language,
        design_opts = DesignOpts,
        partitioned = Partitioned
    },
    SigInfo = {Views, Language, DesignOpts, couch_index_util:sort_lib(Lib)},
    {ok, IdxState#mrst{sig = couch_hash:md5_hash(term_to_binary(SigInfo))}}.

set_view_type(_Args, _ViewName, []) ->
    throw({not_found, missing_named_view});
set_view_type(Args, ViewName, [View | Rest]) ->
    RedNames = [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(ViewName, RedNames) of
        true ->
            case Args#mrargs.reduce of
                false -> Args#mrargs{view_type = map};
                _ -> Args#mrargs{view_type = red}
            end;
        false ->
            case lists:member(ViewName, View#mrview.map_names) of
                true -> Args#mrargs{view_type = map};
                false -> set_view_type(Args, ViewName, Rest)
            end
    end.

set_extra(#mrargs{} = Args, Key, Value) ->
    Extra0 = Args#mrargs.extra,
    Extra1 = lists:ukeysort(1, [{Key, Value} | Extra0]),
    Args#mrargs{extra = Extra1}.

get_extra(#mrargs{} = Args, Key) ->
    couch_util:get_value(Key, Args#mrargs.extra).

get_extra(#mrargs{} = Args, Key, Default) ->
    couch_util:get_value(Key, Args#mrargs.extra, Default).

extract_view(_Lang, _Args, _ViewName, []) ->
    throw({not_found, missing_named_view});
extract_view(Lang, #mrargs{view_type = map} = Args, Name, [View | Rest]) ->
    Names = View#mrview.map_names ++ [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(Name, Names) of
        true -> {map, View, Args};
        _ -> extract_view(Lang, Args, Name, Rest)
    end;
extract_view(Lang, #mrargs{view_type = red} = Args, Name, [View | Rest]) ->
    RedNames = [N || {N, _} <- View#mrview.reduce_funs],
    case lists:member(Name, RedNames) of
        true -> {red, {index_of(Name, RedNames), Lang, View}, Args};
        false -> extract_view(Lang, Args, Name, Rest)
    end.

view_sig(Db, State, View, #mrargs{include_docs = true} = Args) ->
    BaseSig = view_sig(Db, State, View, Args#mrargs{include_docs = false}),
    UpdateSeq = couch_db:get_update_seq(Db),
    PurgeSeq = couch_db:get_purge_seq(Db),
    Term = view_sig_term(BaseSig, UpdateSeq, PurgeSeq),
    couch_index_util:hexsig(couch_hash:md5_hash(term_to_binary(Term)));
view_sig(Db, State, {_Nth, _Lang, View}, Args) ->
    view_sig(Db, State, View, Args);
view_sig(_Db, State, View, Args0) ->
    Sig = State#mrst.sig,
    UpdateSeq = View#mrview.update_seq,
    PurgeSeq = View#mrview.purge_seq,
    Args = Args0#mrargs{
        preflight_fun = undefined,
        extra = []
    },
    Term = view_sig_term(Sig, UpdateSeq, PurgeSeq, Args),
    couch_index_util:hexsig(couch_hash:md5_hash(term_to_binary(Term))).

view_sig_term(BaseSig, UpdateSeq, PurgeSeq) ->
    {BaseSig, UpdateSeq, PurgeSeq}.

view_sig_term(BaseSig, UpdateSeq, PurgeSeq, Args) ->
    {BaseSig, UpdateSeq, PurgeSeq, Args}.

init_state(Db, Fd, #mrst{views = Views} = State, nil) ->
    PurgeSeq = couch_db:get_purge_seq(Db),
    Header = #mrheader{
        seq = 0,
        purge_seq = PurgeSeq,
        id_btree_state = nil,
        view_info = update_collator_versions(#{}),
        view_states = [make_view_state(#mrview{}) || _ <- Views]
    },
    init_state(Db, Fd, State, Header);
init_state(Db, Fd, State, Header) ->
    #mrst{
        language = Lang,
        views = Views
    } = State,

    {ShouldCommit, #mrheader{
        seq = Seq,
        purge_seq = PurgeSeq,
        id_btree_state = IdBtreeState,
        view_info = ViewInfo,
        view_states = ViewStates
    }} = maybe_update_header(Header),

    IdBtOpts = [
        {compression, couch_compress:get_compression_method()}
    ],
    {ok, IdBtree} = couch_btree:open(IdBtreeState, Fd, IdBtOpts),

    OpenViewFun = fun(St, View) -> open_view(Db, Fd, Lang, St, View) end,
    Views2 = lists:zipwith(OpenViewFun, ViewStates, Views),

    {ShouldCommit, State#mrst{
        fd = Fd,
        fd_monitor = erlang:monitor(process, Fd),
        update_seq = Seq,
        purge_seq = PurgeSeq,
        id_btree = IdBtree,
        views = Views2,
        view_info = ViewInfo
    }}.

open_view(_Db, Fd, Lang, ViewState, View) ->
    ReduceFun = make_reduce_fun(Lang, View#mrview.reduce_funs),
    LessFun = maybe_define_less_fun(View),
    Compression = couch_compress:get_compression_method(),
    BTState = get_key_btree_state(ViewState),
    ViewBtOpts = [
        {less, LessFun},
        {reduce, ReduceFun},
        {compression, Compression}
    ],
    {ok, Btree} = couch_btree:open(BTState, Fd, ViewBtOpts),

    View#mrview{
        btree = Btree,
        update_seq = get_update_seq(ViewState),
        purge_seq = get_purge_seq(ViewState)
    }.

temp_view_to_ddoc({Props}) ->
    Language = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    Options = couch_util:get_value(<<"options">>, Props, {[]}),
    View0 = [{<<"map">>, couch_util:get_value(<<"map">>, Props)}],
    View1 =
        View0 ++
            case couch_util:get_value(<<"reduce">>, Props) of
                RedSrc when is_binary(RedSrc) -> [{<<"reduce">>, RedSrc}];
                _ -> []
            end,
    DDoc =
        {[
            {<<"_id">>, couch_uuids:random()},
            {<<"language">>, Language},
            {<<"options">>, Options},
            {<<"views">>,
                {[
                    {<<"temp">>, {View1}}
                ]}}
        ]},
    couch_doc:from_json_obj(DDoc).

get_row_count(#mrview{btree = Bt}) ->
    Count =
        case couch_btree:full_reduce(Bt) of
            {ok, {Count0, _Reds, _}} -> Count0;
            {ok, {Count0, _Reds}} -> Count0
        end,
    {ok, Count}.

all_docs_reduce_to_count(Reductions) ->
    Reduce = fun couch_bt_engine:id_tree_reduce/2,
    {Count, _, _} = couch_btree:final_reduce(Reduce, Reductions),
    Count.

reduce_to_count(nil) ->
    0;
reduce_to_count(Reductions) ->
    CountReduceFun = fun count_reduce/2,
    FinalReduction = couch_btree:final_reduce(CountReduceFun, Reductions),
    get_count(FinalReduction).

fold(#mrview{btree = Bt}, Fun, Acc, Opts) ->
    WrapperFun = fun(KV, Reds, Acc2) ->
        fold_fun(Fun, expand_dups([KV], []), Reds, Acc2)
    end,
    {ok, _LastRed, _Acc} = couch_btree:fold(Bt, WrapperFun, Acc, Opts).

fold_fun(_Fun, [], _, Acc) ->
    {ok, Acc};
fold_fun(Fun, [KV | Rest], {KVReds, Reds}, Acc) ->
    case Fun(KV, {KVReds, Reds}, Acc) of
        {ok, Acc2} ->
            fold_fun(Fun, Rest, {[KV | KVReds], Reds}, Acc2);
        {stop, Acc2} ->
            {stop, Acc2}
    end.

fold_reduce({NthRed, Lang, View}, Fun, Acc, Options) ->
    #mrview{
        btree = Bt,
        reduce_funs = RedFuns
    } = View,

    ReduceFun = make_user_reds_reduce_fun(Lang, RedFuns, NthRed),

    WrapperFun = fun({GroupedKey, _}, PartialReds, Acc0) ->
        FinalReduction = couch_btree:final_reduce(ReduceFun, PartialReds),
        UserReductions = get_user_reds(FinalReduction),
        Fun(GroupedKey, lists:nth(NthRed, UserReductions), Acc0)
    end,

    couch_btree:fold_reduce(Bt, WrapperFun, Acc, Options).

validate_args(Db, DDoc, Args0) ->
    {ok, State} = couch_mrview_index:init(Db, DDoc),
    Args1 = apply_limit(State#mrst.partitioned, Args0),
    validate_args(State, Args1).

validate_args(#mrst{} = State, Args0) ->
    Args = validate_args(Args0),

    ViewPartitioned = State#mrst.partitioned,
    Partition = get_extra(Args, partition),

    case {ViewPartitioned, Partition} of
        {true, undefined} ->
            Msg1 = <<
                "`partition` parameter is mandatory "
                "for queries to this view."
            >>,
            mrverror(Msg1);
        {true, _} ->
            apply_partition(Args, Partition);
        {false, undefined} ->
            Args;
        {false, Value} when is_binary(Value) ->
            Msg2 = <<
                "`partition` parameter is not "
                "supported in this design doc"
            >>,
            mrverror(Msg2)
    end.

apply_limit(ViewPartitioned, Args) ->
    Options = Args#mrargs.extra,
    IgnorePQLimit = lists:keyfind(ignore_partition_query_limit, 1, Options),
    LimitType =
        case {ViewPartitioned, IgnorePQLimit} of
            {true, false} -> "partition_query_limit";
            {true, _} -> "query_limit";
            {false, _} -> "query_limit"
        end,

    MaxLimit = config:get_integer(
        "query_server_config",
        LimitType,
        ?MAX_VIEW_LIMIT
    ),

    % Set the highest limit possible if a user has not
    % specified a limit
    Args1 =
        case Args#mrargs.limit == ?MAX_VIEW_LIMIT of
            true -> Args#mrargs{limit = MaxLimit};
            false -> Args
        end,

    if
        Args1#mrargs.limit =< MaxLimit ->
            Args1;
        true ->
            Fmt = "Limit is too large, must not exceed ~p",
            mrverror(io_lib:format(Fmt, [MaxLimit]))
    end.

validate_all_docs_args(Db, Args0) ->
    Args = validate_args(Args0),

    DbPartitioned = couch_db:is_partitioned(Db),
    Partition = get_extra(Args, partition),

    case {DbPartitioned, Partition} of
        {false, <<_/binary>>} ->
            mrverror(<<"`partition` parameter is not supported on this db">>);
        {_, <<_/binary>>} ->
            Args1 = apply_limit(true, Args),
            apply_all_docs_partition(Args1, Partition);
        _ ->
            Args
    end.

validate_args(Args) ->
    GroupLevel = determine_group_level(Args),
    Reduce = Args#mrargs.reduce,
    case Reduce == undefined orelse is_boolean(Reduce) of
        true -> ok;
        _ -> mrverror(<<"Invalid `reduce` value.">>)
    end,

    case {Args#mrargs.view_type, Reduce} of
        {map, true} -> mrverror(<<"Reduce is invalid for map-only views.">>);
        _ -> ok
    end,

    case {Args#mrargs.view_type, GroupLevel, Args#mrargs.keys} of
        {red, exact, _} ->
            ok;
        {red, _, KeyList} when is_list(KeyList) ->
            Msg = <<"Multi-key fetchs for reduce views must use `group=true`">>,
            mrverror(Msg);
        _ ->
            ok
    end,

    case Args#mrargs.keys of
        Keys when is_list(Keys) -> ok;
        undefined -> ok;
        _ -> mrverror(<<"`keys` must be an array of strings.">>)
    end,

    case {Args#mrargs.keys, Args#mrargs.start_key, Args#mrargs.end_key} of
        {undefined, _, _} ->
            ok;
        {[], _, _} ->
            ok;
        {[_ | _], undefined, undefined} ->
            ok;
        _ ->
            mrverror(<<
                "`keys` is incompatible with `key`"
                ", `start_key` and `end_key`"
            >>)
    end,

    case Args#mrargs.start_key_docid of
        undefined -> ok;
        SKDocId0 when is_binary(SKDocId0) -> ok;
        _ -> mrverror(<<"`start_key_docid` must be a string.">>)
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

    case {Args#mrargs.view_type, GroupLevel} of
        {red, exact} -> ok;
        {_, 0} -> ok;
        {red, Int} when is_integer(Int), Int >= 0 -> ok;
        {red, _} -> mrverror(<<"`group_level` must be >= 0">>);
        {map, _} -> mrverror(<<"Invalid use of grouping on a map view.">>)
    end,

    case Args#mrargs.stable of
        true -> ok;
        false -> ok;
        _ -> mrverror(<<"Invalid value for `stable`.">>)
    end,

    case Args#mrargs.update of
        true -> ok;
        false -> ok;
        lazy -> ok;
        _ -> mrverror(<<"Invalid value for `update`.">>)
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

    SKDocId =
        case {Args#mrargs.direction, Args#mrargs.start_key_docid} of
            {fwd, undefined} -> <<>>;
            {rev, undefined} -> <<255>>;
            {_, SKDocId1} -> SKDocId1
        end,

    EKDocId =
        case {Args#mrargs.direction, Args#mrargs.end_key_docid} of
            {fwd, undefined} -> <<255>>;
            {rev, undefined} -> <<>>;
            {_, EKDocId1} -> EKDocId1
        end,

    case is_boolean(Args#mrargs.sorted) of
        true -> ok;
        _ -> mrverror(<<"Invalid value for `sorted`.">>)
    end,

    case get_extra(Args, partition) of
        undefined -> ok;
        Partition when is_binary(Partition), Partition /= <<>> -> ok;
        _ -> mrverror(<<"Invalid value for `partition`.">>)
    end,

    Args#mrargs{
        start_key_docid = SKDocId,
        end_key_docid = EKDocId,
        group_level = GroupLevel
    }.

determine_group_level(#mrargs{group = undefined, group_level = undefined}) ->
    0;
determine_group_level(#mrargs{group = false, group_level = undefined}) ->
    0;
determine_group_level(#mrargs{group = false, group_level = Level}) when Level > 0 ->
    mrverror(<<"Can't specify group=false and group_level>0 at the same time">>);
determine_group_level(#mrargs{group = true, group_level = undefined}) ->
    exact;
determine_group_level(#mrargs{group_level = GroupLevel}) ->
    GroupLevel.

apply_partition(#mrargs{keys = [{p, _, _} | _]} = Args, _Partition) ->
    % already applied
    Args;
apply_partition(#mrargs{keys = Keys} = Args, Partition) when Keys /= undefined ->
    Args#mrargs{keys = [{p, Partition, K} || K <- Keys]};
apply_partition(#mrargs{start_key = {p, _, _}, end_key = {p, _, _}} = Args, _Partition) ->
    % already applied.
    Args;
apply_partition(Args, Partition) ->
    #mrargs{
        direction = Dir,
        start_key = StartKey,
        end_key = EndKey
    } = Args,

    {DefSK, DefEK} =
        case Dir of
            fwd -> {?LOWEST_KEY, ?HIGHEST_KEY};
            rev -> {?HIGHEST_KEY, ?LOWEST_KEY}
        end,

    SK0 =
        if
            StartKey /= undefined -> StartKey;
            true -> DefSK
        end,
    EK0 =
        if
            EndKey /= undefined -> EndKey;
            true -> DefEK
        end,

    Args#mrargs{
        start_key = {p, Partition, SK0},
        end_key = {p, Partition, EK0}
    }.

%% all_docs is special as it's not really a view and is already
%% effectively partitioned as the partition is a prefix of all keys.
apply_all_docs_partition(#mrargs{} = Args, Partition) ->
    #mrargs{
        direction = Dir,
        start_key = StartKey,
        end_key = EndKey
    } = Args,

    {DefSK, DefEK} =
        case Dir of
            fwd ->
                {
                    couch_partition:start_key(Partition),
                    couch_partition:end_key(Partition)
                };
            rev ->
                {
                    couch_partition:end_key(Partition),
                    couch_partition:start_key(Partition)
                }
        end,

    SK0 =
        if
            StartKey == undefined -> DefSK;
            true -> StartKey
        end,
    EK0 =
        if
            EndKey == undefined -> DefEK;
            true -> EndKey
        end,

    {SK1, EK1} =
        case Dir of
            fwd -> {?HIGHEST(DefSK, SK0), ?LOWEST(DefEK, EK0)};
            rev -> {?LOWEST(DefSK, SK0), ?HIGHEST(DefEK, EK0)}
        end,

    Args#mrargs{
        start_key = SK1,
        end_key = EK1
    }.

check_range(#mrargs{start_key = undefined}, _Cmp) ->
    ok;
check_range(#mrargs{end_key = undefined}, _Cmp) ->
    ok;
check_range(#mrargs{start_key = K, end_key = K}, _Cmp) ->
    ok;
check_range(Args, Cmp) ->
    #mrargs{
        direction = Dir,
        start_key = SK,
        start_key_docid = SKD,
        end_key = EK,
        end_key_docid = EKD
    } = Args,
    case {Dir, Cmp({SK, SKD}, {EK, EKD})} of
        {fwd, false} ->
            throw(
                {query_parse_error,
                    <<"No rows can match your key range, reverse your ",
                        "start_key and end_key or set descending=true">>}
            );
        {rev, true} ->
            throw(
                {query_parse_error,
                    <<"No rows can match your key range, reverse your ",
                        "start_key and end_key or set descending=false">>}
            );
        _ ->
            ok
    end.

view_cmp({_Nth, _Lang, View}) ->
    view_cmp(View);
view_cmp(View) ->
    fun(A, B) -> couch_btree:less(View#mrview.btree, A, B) end.

make_header(State) ->
    #mrst{
        update_seq = Seq,
        purge_seq = PurgeSeq,
        id_btree = IdBtree,
        views = Views,
        view_info = ViewInfo
    } = State,

    #mrheader{
        seq = Seq,
        purge_seq = PurgeSeq,
        id_btree_state = get_btree_state(IdBtree),
        view_info = ViewInfo,
        view_states = [make_disk_view_state(V) || V <- Views]
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

reset_index(Db, Fd, #mrst{sig = Sig} = State) ->
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, {Sig, nil}),
    {_Commit, NewSt} = init_state(Db, Fd, reset_state(State), nil),
    NewSt.

reset_state(State) ->
    State#mrst{
        fd = nil,
        qserver = nil,
        update_seq = 0,
        id_btree = nil,
        views = [View#mrview{btree = nil} || View <- State#mrst.views],
        view_info = #{}
    }.

all_docs_key_opts(#mrargs{extra = Extra} = Args) ->
    all_docs_key_opts(Args, Extra).

all_docs_key_opts(#mrargs{keys = undefined} = Args, Extra) ->
    all_docs_key_opts(Args#mrargs{keys = []}, Extra);
all_docs_key_opts(#mrargs{keys = [], direction = Dir} = Args, Extra) ->
    [[{dir, Dir}] ++ ad_skey_opts(Args) ++ ad_ekey_opts(Args) ++ Extra];
all_docs_key_opts(#mrargs{keys = Keys, direction = Dir} = Args, Extra) ->
    lists:map(
        fun(K) ->
            [{dir, Dir}] ++
                ad_skey_opts(Args#mrargs{start_key = K}) ++
                ad_ekey_opts(Args#mrargs{end_key = K}) ++
                Extra
        end,
        Keys
    ).

ad_skey_opts(#mrargs{start_key = SKey}) when is_binary(SKey) ->
    [{start_key, SKey}];
ad_skey_opts(#mrargs{start_key_docid = SKeyDocId}) ->
    [{start_key, SKeyDocId}].

ad_ekey_opts(#mrargs{end_key = EKey} = Args) when is_binary(EKey) ->
    Type =
        if
            Args#mrargs.inclusive_end -> end_key;
            true -> end_key_gt
        end,
    [{Type, EKey}];
ad_ekey_opts(#mrargs{end_key_docid = EKeyDocId} = Args) ->
    Type =
        if
            Args#mrargs.inclusive_end -> end_key;
            true -> end_key_gt
        end,
    [{Type, EKeyDocId}].

key_opts(Args) ->
    key_opts(Args, []).

key_opts(#mrargs{keys = undefined, direction = Dir} = Args, Extra) ->
    [[{dir, Dir}] ++ skey_opts(Args) ++ ekey_opts(Args) ++ Extra];
key_opts(#mrargs{keys = Keys, direction = Dir} = Args, Extra) ->
    lists:map(
        fun(K) ->
            [{dir, Dir}] ++
                skey_opts(Args#mrargs{start_key = K}) ++
                ekey_opts(Args#mrargs{end_key = K}) ++
                Extra
        end,
        Keys
    ).

skey_opts(#mrargs{start_key = undefined}) ->
    [];
skey_opts(#mrargs{start_key = SKey, start_key_docid = SKeyDocId}) ->
    [{start_key, {SKey, SKeyDocId}}].

ekey_opts(#mrargs{end_key = undefined}) ->
    [];
ekey_opts(#mrargs{end_key = EKey, end_key_docid = EKeyDocId} = Args) ->
    case Args#mrargs.inclusive_end of
        true -> [{end_key, {EKey, EKeyDocId}}];
        false -> [{end_key_gt, {EKey, reverse_key_default(EKeyDocId)}}]
    end.

reverse_key_default(<<>>) -> <<255>>;
reverse_key_default(<<255>>) -> <<>>;
reverse_key_default(Key) -> Key.

reduced_external_size(Tree) ->
    case couch_btree:full_reduce(Tree) of
        {ok, {_, _, Size}} -> Size;
        % return 0 for versions of the reduce function without Size
        {ok, {_, _}} -> 0
    end.

calculate_external_size(Views) ->
    SumFun = fun
        (#mrview{btree = nil}, Acc) ->
            Acc;
        (#mrview{btree = Bt}, Acc) ->
            Acc + reduced_external_size(Bt)
    end,
    {ok, lists:foldl(SumFun, 0, Views)}.

calculate_active_size(Views) ->
    FoldFun = fun
        (#mrview{btree = nil}, Acc) ->
            Acc;
        (#mrview{btree = Bt}, Acc) ->
            Acc + couch_btree:size(Bt)
    end,
    {ok, lists:foldl(FoldFun, 0, Views)}.

detuple_kvs([], Acc) ->
    lists:reverse(Acc);
detuple_kvs([KV | Rest], Acc) ->
    {{Key, Id}, Value} = KV,
    NKV = [[Key, Id], Value],
    detuple_kvs(Rest, [NKV | Acc]).

expand_dups([], Acc) ->
    lists:reverse(Acc);
expand_dups([{Key, {dups, Vals}} | Rest], Acc) ->
    Expanded = [{Key, Val} || Val <- Vals],
    expand_dups(Rest, Expanded ++ Acc);
expand_dups([KV | Rest], Acc) ->
    expand_dups(Rest, [KV | Acc]).

maybe_load_doc(_Db, _DI, #mrargs{include_docs = false}) ->
    [];
maybe_load_doc(Db, #doc_info{} = DI, #mrargs{conflicts = true, doc_options = Opts}) ->
    doc_row(couch_index_util:load_doc(Db, DI, [conflicts]), Opts);
maybe_load_doc(Db, #doc_info{} = DI, #mrargs{doc_options = Opts}) ->
    doc_row(couch_index_util:load_doc(Db, DI, []), Opts).

maybe_load_doc(_Db, _Id, _Val, #mrargs{include_docs = false}) ->
    [];
maybe_load_doc(Db, Id, Val, #mrargs{conflicts = true, doc_options = Opts}) ->
    doc_row(couch_index_util:load_doc(Db, docid_rev(Id, Val), [conflicts]), Opts);
maybe_load_doc(Db, Id, Val, #mrargs{doc_options = Opts}) ->
    doc_row(couch_index_util:load_doc(Db, docid_rev(Id, Val), []), Opts).

doc_row(null, _Opts) ->
    [{doc, null}];
doc_row(Doc, Opts) ->
    [{doc, couch_doc:to_json_obj(Doc, Opts)}].

docid_rev(Id, {Props}) ->
    DocId = couch_util:get_value(<<"_id">>, Props, Id),
    Rev =
        case couch_util:get_value(<<"_rev">>, Props, nil) of
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
    index_of(Key, Rest, Idx + 1).

mrverror(Mesg) ->
    throw({query_parse_error, Mesg}).

%% Updates 2.x  view files to 3.x or later view files
%% transparently, the first time the 2.x view file is opened by
%% 3.x or later.
%%
%% Here's how it works:
%%
%% Before opening a view index,
%% If no matching index file is found in the new location:
%%  calculate the <= 2.x view signature
%%  if a file with that signature lives in the old location
%%    rename it to the new location with the new signature in the name.
%% Then proceed to open the view index as usual.

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
    Sig = sig_vsn_2x(State),
    DbName = State#mrst.db_name,
    FileName = couch_index_util:hexsig(Sig) ++ ".view",
    IndexFile = couch_index_util:index_file("mrview", DbName, FileName),

    % If we have an old index, rename it to the new position.
    case file:read_file_info(IndexFile) of
        {ok, _FileInfo} ->
            % Crash if the rename fails for any reason.
            % If the target exists, e.g. the next request will find the
            % new file and we are good. We might need to catch this
            % further up to avoid a full server crash.
            NewIndexFile = index_file(DbName, State#mrst.sig),
            couch_log:notice(
                "Attempting to update legacy view index file"
                " from ~p to ~s",
                [IndexFile, NewIndexFile]
            ),
            ok = filelib:ensure_dir(NewIndexFile),
            ok = file:rename(IndexFile, NewIndexFile),
            couch_log:notice(
                "Successfully updated legacy view index file"
                " ~s",
                [IndexFile]
            ),
            Sig;
        {error, enoent} ->
            % Ignore missing index file
            ok;
        {error, Reason} ->
            couch_log:error(
                "Failed to update legacy view index file"
                " ~s : ~s",
                [IndexFile, file:format_error(Reason)]
            ),
            ok
    end.

sig_vsn_2x(State) ->
    #mrst{
        lib = Lib,
        language = Language,
        design_opts = DesignOpts
    } = State,
    SI = proplists:get_value(<<"seq_indexed">>, DesignOpts, false),
    KSI = proplists:get_value(<<"keyseq_indexed">>, DesignOpts, false),
    Views = [old_view_format(V, SI, KSI) || V <- State#mrst.views],
    SigInfo = {Views, Language, DesignOpts, couch_index_util:sort_lib(Lib)},
    couch_hash:md5_hash(term_to_binary(SigInfo)).

old_view_format(View, SI, KSI) ->
    {
        mrview,
        View#mrview.id_num,
        View#mrview.update_seq,
        View#mrview.purge_seq,
        View#mrview.map_names,
        View#mrview.reduce_funs,
        View#mrview.def,
        View#mrview.btree,
        nil,
        nil,
        SI,
        KSI,
        View#mrview.options
    }.

maybe_update_header(#mrheader{view_info = Info} = Header) when is_map(Info) ->
    % Latest (3.2.1+) version. The size of the record is the same as
    % the <2.3.1 version. The main difference is that the LogBt field
    % is now a map. This trick allows for easy downgrading back to
    % version 3.2.1 and then upgrading back to 3.2.1+ if needed.
    {false, Header#mrheader{
        view_info = update_collator_versions(Info),
        view_states = [make_view_state(S) || S <- Header#mrheader.view_states]
    }};
maybe_update_header({mrheader, Seq, PSeq, IDBt, ViewStates}) ->
    % Versions >2.3.1 and =<3.2.1 (no view info map)
    {true, #mrheader{
        seq = Seq,
        purge_seq = PSeq,
        id_btree_state = IDBt,
        view_info = update_collator_versions(#{}),
        view_states = [make_view_state(S) || S <- ViewStates]
    }};
maybe_update_header({mrheader, Seq, PSeq, IDBt, _LogBt, ViewStates}) ->
    % Versions <2.3.1.
    {true, #mrheader{
        seq = Seq,
        purge_seq = PSeq,
        id_btree_state = IDBt,
        view_info = update_collator_versions(#{}),
        view_states = [make_view_state(S) || S <- ViewStates]
    }}.

%% End of <= 2.x upgrade code.

% Used for creating a new view states or reading (upgrading) from
% disk. On disk, the state will be a 5 tuple with nil values in
% positions 2 and 3 to allow downgrading between current version and
% =<3.2.1 views.
%
make_view_state(#mrview{} = View) ->
    BTState = get_btree_state(View#mrview.btree),
    {
        BTState,
        View#mrview.update_seq,
        View#mrview.purge_seq
    };
make_view_state({BTState, UpdateSeq, PurgeSeq}) ->
    % Versions >2.x and =<3.2.1
    {BTState, UpdateSeq, PurgeSeq};
make_view_state({BTState, _SeqBTOrNil, _KSeqBTOrNil, UpdateSeq, PurgeSeq}) ->
    % Current disk version and version 2.x views
    {BTState, UpdateSeq, PurgeSeq};
make_view_state(nil) ->
    {nil, 0, 0}.

% Used by make_header/1 before committing to disk. The two added nil
% values in position 2 and 3 make the state on disk look like a 2.x
% view, where those fields used to be SeqBTState and KSeqBTState,
% respectively. This is to allow easy downgrading between current
% version and >2.x and =<3.2.1 views.
%
make_disk_view_state(#mrview{} = View) ->
    BTState = get_btree_state(View#mrview.btree),
    {
        BTState,
        nil,
        nil,
        View#mrview.update_seq,
        View#mrview.purge_seq
    };
make_disk_view_state({BTState, UpdateSeq, PurgeSeq}) ->
    {BTState, nil, nil, UpdateSeq, PurgeSeq};
make_disk_view_state(nil) ->
    {nil, nil, nil, 0, 0}.

get_key_btree_state(ViewState) ->
    element(1, ViewState).

get_update_seq(ViewState) ->
    element(2, ViewState).

get_purge_seq(ViewState) ->
    element(3, ViewState).

get_count(Reduction) ->
    element(1, Reduction).

get_user_reds(Reduction) ->
    element(2, Reduction).

% This is for backwards compatibility for seq btree reduces
get_external_size_reds(Reduction) when is_integer(Reduction) ->
    0;
get_external_size_reds(Reduction) when tuple_size(Reduction) == 2 ->
    0;
get_external_size_reds(Reduction) when tuple_size(Reduction) == 3 ->
    element(3, Reduction).

make_reduce_fun(Lang, ReduceFuns) ->
    FunSrcs = [FunSrc || {_, FunSrc} <- ReduceFuns],
    fun
        (reduce, KVs0) ->
            KVs = detuple_kvs(expand_dups(KVs0, []), []),
            {ok, Result} = couch_query_servers:reduce(Lang, FunSrcs, KVs),
            ExternalSize = kv_external_size(KVs, Result),
            {length(KVs), Result, ExternalSize};
        (rereduce, Reds) ->
            ExtractFun = fun(Red, {CountsAcc0, URedsAcc0, ExtAcc0}) ->
                CountsAcc = CountsAcc0 + get_count(Red),
                URedsAcc = lists:append(URedsAcc0, [get_user_reds(Red)]),
                ExtAcc = ExtAcc0 + get_external_size_reds(Red),
                {CountsAcc, URedsAcc, ExtAcc}
            end,
            {Counts, UReds, ExternalSize} = lists:foldl(
                ExtractFun,
                {0, [], 0},
                Reds
            ),
            {ok, Result} = couch_query_servers:rereduce(Lang, FunSrcs, UReds),
            {Counts, Result, ExternalSize}
    end.

maybe_define_less_fun(#mrview{options = Options}) ->
    case couch_util:get_value(<<"collation">>, Options) of
        <<"raw">> -> undefined;
        _ -> fun couch_ejson_compare:less_json_ids/2
    end.

count_reduce(reduce, KVs) ->
    CountFun = fun
        ({_, {dups, Vals}}, Acc) -> Acc + length(Vals);
        (_, Acc) -> Acc + 1
    end,
    Count = lists:foldl(CountFun, 0, KVs),
    {Count, []};
count_reduce(rereduce, Reds) ->
    CountFun = fun(Red, Acc) ->
        Acc + get_count(Red)
    end,
    Count = lists:foldl(CountFun, 0, Reds),
    {Count, []}.

make_user_reds_reduce_fun(Lang, ReduceFuns, NthRed) ->
    LPad = lists:duplicate(NthRed - 1, []),
    RPad = lists:duplicate(length(ReduceFuns) - NthRed, []),
    {_, FunSrc} = lists:nth(NthRed, ReduceFuns),
    fun
        (reduce, KVs0) ->
            KVs = detuple_kvs(expand_dups(KVs0, []), []),
            {ok, Result} = couch_query_servers:reduce(Lang, [FunSrc], KVs),
            {0, LPad ++ Result ++ RPad};
        (rereduce, Reds) ->
            ExtractFun = fun(Reds0) ->
                [lists:nth(NthRed, get_user_reds(Reds0))]
            end,
            UReds = lists:map(ExtractFun, Reds),
            {ok, Result} = couch_query_servers:rereduce(Lang, [FunSrc], UReds),
            {0, LPad ++ Result ++ RPad}
    end.

get_btree_state(nil) ->
    nil;
get_btree_state(#btree{} = Btree) ->
    couch_btree:get_state(Btree).

extract_view_reduce({red, {N, _Lang, #mrview{reduce_funs = Reds}}, _Ref}) ->
    {_Name, FunSrc} = lists:nth(N, Reds),
    FunSrc.

get_view_keys({Props}) ->
    case couch_util:get_value(<<"keys">>, Props) of
        undefined ->
            undefined;
        Keys when is_list(Keys) ->
            Keys;
        _ ->
            throw({bad_request, "`keys` member must be an array."})
    end.

get_view_queries({Props}) ->
    case couch_util:get_value(<<"queries">>, Props) of
        undefined ->
            undefined;
        Queries when is_list(Queries) ->
            Queries;
        _ ->
            throw({bad_request, "`queries` member must be an array."})
    end.

kv_external_size(KVList, Reduction) ->
    lists:foldl(
        fun([[Key, _], Value], Acc) ->
            ?term_size(Key) + ?term_size(Value) + Acc
        end,
        ?term_size(Reduction),
        KVList
    ).

update_collator_versions(#{} = ViewInfo) ->
    Versions = maps:get(ucol_vs, ViewInfo, []),
    Ver = tuple_to_list(couch_ejson_compare:get_collator_version()),
    ViewInfo#{ucol_vs => lists:usort([Ver | Versions])}.

get_collator_versions(#{ucol_vs := Versions}) when is_list(Versions) ->
    Versions;
get_collator_versions(#{}) ->
    [].

compact_on_collator_upgrade() ->
    config:get_boolean("view_upgrade", "compact_on_collator_upgrade", true).

commit_on_header_upgrade() ->
    config:get_boolean("view_upgrade", "commit_on_header_upgrade", true).
