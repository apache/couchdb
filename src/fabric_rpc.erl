-module(fabric_rpc).

-export([get_db_info/1, get_doc_count/1, get_update_seq/1]).
-export([open_doc/3, open_revs/4, get_missing_revs/2, update_docs/3]).
-export([all_docs/2, map_view/4]).

-include("fabric.hrl").

-record (view_acc, {
    db,
    limit,
    include_docs,
    offset = nil,
    total_rows,
    reduce_fun = fun couch_db:enum_docs_reduce_to_count/1,
    stop_fun,
    group_level = 0
}).

%% rpc endpoints
%%  call to with_db will supply your M:F with a #db{} and then remaining args

all_docs(DbName, #view_query_args{keys=nil} = QueryArgs) ->
    {ok, Db} = couch_db:open(DbName, []),
    #view_query_args{
        start_key = StartKey,
        start_docid = StartDocId,
        limit = Limit,
        skip = Skip,
        include_docs = IncludeDocs,
        direction = Dir
    } = QueryArgs,
    StartId = if is_binary(StartKey) -> StartKey; true -> StartDocId end,
    {ok, Total} = couch_db:get_doc_count(Db),
    Acc0 = #view_acc{
        db = Db,
        include_docs = IncludeDocs,
        limit = Limit+Skip,
        total_rows = Total,
        stop_fun = all_docs_stop_fun(QueryArgs)
    },
    {ok, Acc} = couch_db:enum_docs(Db, StartId, Dir, fun view_fold/3, Acc0),
    final_response(Total, Acc#view_acc.offset).

map_view(DbName, DDoc, ViewName, QueryArgs) ->
    {ok, Db} = couch_db:open(DbName, []),
    #view_query_args{
        start_key = StartKey,
        start_docid = StartDocId,
        limit = Limit,
        skip = Skip,
        keys = Keys,
        include_docs = IncludeDocs,
        direction = Dir,
        stale = Stale,
        view_type = ViewType
    } = QueryArgs,
    Start = {StartKey, StartDocId},
    MinSeq = if Stale == ok -> 0; true -> couch_db:get_update_seq(Db) end,
    Group0 = couch_view_group:design_doc_to_view_group(Db, DDoc),
    {ok, Pid} = gen_server:call(couch_view, {get_group_server, DbName, Group0}),
    {ok, Group} = couch_view_group:request_group(Pid, MinSeq),
    View = extract_view(Pid, ViewName, Group#group.views, ViewType),
    {ok, Total} = couch_view:get_row_count(View),
    Acc0 = #view_acc{
        db = Db,
        include_docs = IncludeDocs,
        limit = Limit+Skip,
        total_rows = Total,
        reduce_fun = fun couch_view:reduce_to_count/1,
        stop_fun = default_stop_fun(QueryArgs)
    },
    case Keys of
    nil ->
        {ok, Acc} = couch_view:fold(View, Start, Dir, fun view_fold/3, Acc0);
    _ ->
        Acc = lists:foldl(fun(Key, AccIn) ->
            KeyStart = {Key, StartDocId},
            KeyStop = default_stop_fun(QueryArgs#view_query_args{start_key=Key,
                end_key=Key}),
            {_Go, Out} = couch_view:fold(View, KeyStart, Dir, fun view_fold/3,
                AccIn#view_acc{stop_fun = KeyStop}),
            Out
        end, Acc0, Keys)
    end,
    final_response(Total, Acc#view_acc.offset).

get_db_info(DbName) ->
    with_db(DbName, [], {couch_db, get_db_info, []}).

get_doc_count(DbName) ->
    with_db(DbName, [], {couch_db, get_doc_count, []}).

get_update_seq(DbName) ->
    rexi:reply(case couch_db:open(DbName, []) of
    {ok, #db{update_seq = Seq}} ->
        {ok, Seq};
    Error ->
        Error
    end).

open_doc(DbName, DocId, Options) ->
    with_db(DbName, Options, {couch_db, open_doc_int, [DocId, Options]}).

open_revs(DbName, Id, Revs, Options) ->
    with_db(DbName, Options, {couch_db, open_doc_revs, [Id, Revs, Options]}).

get_missing_revs(DbName, IdRevsList) ->
    % reimplement here so we get [] for Ids with no missing revs in response
    rexi:reply(case couch_db:open(DbName, []) of
    {ok, Db} ->
        Ids = [Id1 || {Id1, _Revs} <- IdRevsList],
        {ok, lists:zipwith(fun({Id, Revs}, FullDocInfoResult) ->
            case FullDocInfoResult of
            {ok, #full_doc_info{rev_tree=RevisionTree}} ->
                {Id, couch_key_tree:find_missing(RevisionTree, Revs)};
            not_found ->
                {Id, Revs}
            end
        end, IdRevsList, couch_btree:lookup(Db#db.id_tree, Ids))};
    Error ->
        Error
    end).

update_docs(DbName, Docs, Options) ->
    with_db(DbName, Options, {couch_db, update_docs, [Docs, Options]}).

%%
%% internal
%%

with_db(DbName, Options, {M,F,A}) ->
    case couch_db:open(DbName, Options) of
    {ok, Db} ->
        rexi:reply(apply(M, F, [Db | A]));
    Error ->
        rexi:reply(Error)
    end.

view_fold(#full_doc_info{} = FullDocInfo, OffsetReds, Acc) ->
    % matches for _all_docs and translates #full_doc_info{} -> KV pair
    case couch_doc:to_doc_info(FullDocInfo) of
    #doc_info{revs=[#rev_info{deleted=false, rev=Rev}|_]} ->
        Id = FullDocInfo#full_doc_info.id,
        Value = {[{rev,couch_doc:rev_to_str(Rev)}]},
        view_fold({{Id,Id}, Value}, OffsetReds, Acc);
    #doc_info{revs=[#rev_info{deleted=true}|_]} ->
        {ok, Acc}
    end;
view_fold(KV, OffsetReds, #view_acc{offset=nil, total_rows=Total} = Acc) ->
    % calculates the offset for this shard
    #view_acc{reduce_fun=Reduce} = Acc,
    Offset = Reduce(OffsetReds),
    case rexi:sync_reply({total_and_offset, Total, Offset}) of
    ok ->
        view_fold(KV, OffsetReds, Acc#view_acc{offset=Offset});
    stop ->
        exit(normal)
    end;
view_fold(_KV, _Offset, #view_acc{limit=0} = Acc) ->
    % we scanned through limit+skip local rows
    {stop, Acc};
view_fold({{Key,Id}, Value}, _Offset, Acc) ->
    % the normal case
    #view_acc{
        db = Db,
        limit = Limit,
        include_docs = IncludeDocs,
        stop_fun = PassedEnd
    } = Acc,
    case PassedEnd(Key, Id) of
    true ->
        {stop, Acc};
    false ->
        Doc = if not IncludeDocs -> undefined; true ->
            case couch_db:open_doc(Db, Id, []) of
            {not_found, deleted} ->
                null;
            {not_found, missing} ->
                undefined;
            {ok, Doc0} ->
                couch_doc:to_json_obj(Doc0, [])
            end
        end,
        rexi:sync_reply(#view_row{key=Key, id=Id, value=Value, doc=Doc}),
        {ok, Acc#view_acc{limit=Limit-1}}
    end.

all_docs_stop_fun(#view_query_args{direction=fwd, end_key=EndKey}) ->
    fun(ViewKey, _) ->
        couch_db_updater:less_docid(EndKey, ViewKey)
    end;
all_docs_stop_fun(#view_query_args{direction=rev, end_key=EndKey}) ->
    fun(ViewKey, _) ->
        couch_db_updater:less_docid(ViewKey, EndKey)
    end.

final_response(Total, nil) ->
    case rexi:sync_reply({total_and_offset, Total, Total}) of ok ->
        rexi:reply(complete);
    stop -> ok end;
final_response(_Total, _Offset) ->
    rexi:reply(complete).

default_stop_fun(#view_query_args{direction=fwd, inclusive_end=true} = Args) ->
    #view_query_args{end_key=EndKey, end_docid=EndDocId} = Args,
    fun(ViewKey, ViewId) ->
        couch_view:less_json([EndKey, EndDocId], [ViewKey, ViewId])
    end;
default_stop_fun(#view_query_args{direction=fwd} = Args) ->
    #view_query_args{end_key=EndKey, end_docid=EndDocId} = Args,
    fun
        (ViewKey, _ViewId) when ViewKey == EndKey ->
            true;
        (ViewKey, ViewId) ->
            couch_view:less_json([EndKey, EndDocId], [ViewKey, ViewId])
    end;
default_stop_fun(#view_query_args{direction=rev, inclusive_end=true} = Args) ->
    #view_query_args{end_key=EndKey, end_docid=EndDocId} = Args,
    fun(ViewKey, ViewId) ->
        couch_view:less_json([ViewKey, ViewId], [EndKey, EndDocId])
    end;
default_stop_fun(#view_query_args{direction=rev} = Args) ->
    #view_query_args{end_key=EndKey, end_docid=EndDocId} = Args,
    fun
        (ViewKey, _ViewId) when ViewKey == EndKey ->
            true;
        (ViewKey, ViewId) ->
            couch_view:less_json([ViewKey, ViewId], [EndKey, EndDocId])
    end.

extract_view(Pid, ViewName, [], _ViewType) ->
    ?LOG_ERROR("missing_named_view ~p", [ViewName]),
    exit(Pid, kill),
    exit(missing_named_view);
extract_view(Pid, ViewName, [View|Rest], ViewType) ->
    case lists:member(ViewName, view_names(View, ViewType)) of
    true ->
        if ViewType == reduce ->
            {index_of(ViewName, view_names(View, reduce)), View};
        true ->
            View
        end;
    false ->
        extract_view(Pid, ViewName, Rest, ViewType)
    end.

view_names(View, Type) when Type == red_map; Type == reduce ->
    [Name || {Name, _} <- View#view.reduce_funs];
view_names(View, map) ->
    View#view.map_names.

index_of(X, List) ->
    index_of(X, List, 1).

index_of(_X, [], _I) ->
    not_found;
index_of(X, [X|_Rest], I) ->
    I;
index_of(X, [_|Rest], I) ->
    index_of(X, Rest, I+1).
