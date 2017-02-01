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

-module(couch_changes).
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([
    handle_db_changes/3,
    handle_changes/4,
    get_changes_timeout/2,
    wait_updated/3,
    get_rest_updated/1,
    configure_filter/4,
    filter/3,
    handle_db_event/3,
    handle_view_event/3,
    view_filter/3
]).

-export([changes_enumerator/2]).

% For the builtin filter _docs_ids, this is the maximum number
% of documents for which we trigger the optimized code path.
-define(MAX_DOC_IDS, 100).

-record(changes_acc, {
    db,
    view_name,
    ddoc_name,
    view,
    seq,
    prepend,
    filter,
    callback,
    user_acc,
    resp_type,
    limit,
    include_docs,
    doc_options,
    conflicts,
    timeout,
    timeout_fun,
    aggregation_kvs,
    aggregation_results
}).

handle_db_changes(Args, Req, Db) ->
    handle_changes(Args, Req, Db, db).

handle_changes(Args1, Req, Db0, Type) ->
    #changes_args{
        style = Style,
        filter = FilterName,
        feed = Feed,
        dir = Dir,
        since = Since
    } = Args1,
    Filter = configure_filter(FilterName, Style, Req, Db0),
    Args = Args1#changes_args{filter_fun = Filter},
    % The type of changes feed depends on the supplied filter. If the query is
    % for an optimized view-filtered db changes, we need to use the view
    % sequence tree.
    {UseViewChanges, DDocName, ViewName} = case {Type, Filter} of
        {{view, DDocName0, ViewName0}, _} ->
            {true, DDocName0, ViewName0};
        {_, {fast_view, _, DDoc, ViewName0}} ->
            {true, DDoc#doc.id, ViewName0};
        _ ->
            {false, undefined, undefined}
    end,
    DbName = couch_db:name(Db0),
    {StartListenerFun, View} = if UseViewChanges ->
        {ok, {_, View0, _}, _, _} = couch_mrview_util:get_view(
                DbName, DDocName, ViewName, #mrargs{}),
        case View0#mrview.seq_btree of
            #btree{} ->
                ok;
            _ ->
                throw({bad_request, "view changes not enabled"})
        end,
        SNFun = fun() ->
            couch_event:link_listener(
                 ?MODULE, handle_view_event, {self(), DDocName}, [{dbname, DbName}]
            )
        end,
        {SNFun, View0};
    true ->
        SNFun = fun() ->
            couch_event:link_listener(
                 ?MODULE, handle_db_event, self(), [{dbname, DbName}]
            )
        end,
        {SNFun, undefined}
    end,
    Start = fun() ->
        {ok, Db} = couch_db:reopen(Db0),
        StartSeq = case Dir of
        rev ->
            couch_db:get_update_seq(Db);
        fwd ->
            Since
        end,
        View2 = if UseViewChanges ->
            {ok, {_, View1, _}, _, _} = couch_mrview_util:get_view(
                    DbName, DDocName, ViewName, #mrargs{}),
            View1;
        true ->
            undefined
        end,
        {Db, View2, StartSeq}
    end,
    % begin timer to deal with heartbeat when filter function fails
    case Args#changes_args.heartbeat of
    undefined ->
        erlang:erase(last_changes_heartbeat);
    Val when is_integer(Val); Val =:= true ->
        put(last_changes_heartbeat, os:timestamp())
    end,

    case lists:member(Feed, ["continuous", "longpoll", "eventsource"]) of
    true ->
        fun(CallbackAcc) ->
            {Callback, UserAcc} = get_callback_acc(CallbackAcc),
            {ok, Listener} = StartListenerFun(),

            {Db, View, StartSeq} = Start(),
            UserAcc2 = start_sending_changes(Callback, UserAcc, Feed),
            {Timeout, TimeoutFun} = get_changes_timeout(Args, Callback),
            Acc0 = build_acc(Args, Callback, UserAcc2, Db, StartSeq,
                             <<"">>, Timeout, TimeoutFun, DDocName, ViewName,
                             View),
            try
                keep_sending_changes(
                    Args#changes_args{dir=fwd},
                    Acc0,
                    true)
            after
                couch_event:stop_listener(Listener),
                get_rest_updated(ok) % clean out any remaining update messages
            end
        end;
    false ->
        fun(CallbackAcc) ->
            {Callback, UserAcc} = get_callback_acc(CallbackAcc),
            UserAcc2 = start_sending_changes(Callback, UserAcc, Feed),
            {Timeout, TimeoutFun} = get_changes_timeout(Args, Callback),
            {Db, View, StartSeq} = Start(),
            Acc0 = build_acc(Args#changes_args{feed="normal"}, Callback,
                             UserAcc2, Db, StartSeq, <<>>, Timeout, TimeoutFun,
                             DDocName, ViewName, View),
            {ok, #changes_acc{seq = LastSeq, user_acc = UserAcc3}} =
                send_changes(
                    Acc0,
                    Dir,
                    true),
            end_sending_changes(Callback, UserAcc3, LastSeq, Feed)
        end
    end.


handle_db_event(_DbName, updated, Parent) ->
    Parent ! updated,
    {ok, Parent};
handle_db_event(_DbName, deleted, Parent) ->
    Parent ! deleted,
    {ok, Parent};
handle_db_event(_DbName, _Event, Parent) ->
    {ok, Parent}.


handle_view_event(_DbName, Msg, {Parent, DDocId}) ->
    case Msg of
        {index_commit, DDocId} ->
            Parent ! updated;
        {index_delete, DDocId} ->
            Parent ! deleted;
        _ ->
            ok
    end,
    {ok, {Parent, DDocId}}.

get_callback_acc({Callback, _UserAcc} = Pair) when is_function(Callback, 3) ->
    Pair;
get_callback_acc(Callback) when is_function(Callback, 2) ->
    {fun(Ev, Data, _) -> Callback(Ev, Data) end, ok}.


configure_filter("_doc_ids", Style, Req, _Db) ->
    {doc_ids, Style, get_doc_ids(Req)};
configure_filter("_selector", Style, Req, _Db) ->
    {selector, Style,  get_selector_and_fields(Req)};
configure_filter("_design", Style, _Req, _Db) ->
    {design_docs, Style};
configure_filter("_view", Style, Req, Db) ->
    ViewName = get_view_qs(Req),
    if ViewName /= "" -> ok; true ->
        throw({bad_request, "`view` filter parameter is not provided."})
    end,
    ViewNameParts = string:tokens(ViewName, "/"),
    case [?l2b(couch_httpd:unquote(Part)) || Part <- ViewNameParts] of
        [DName, VName] ->
            {ok, DDoc} = open_ddoc(Db, <<"_design/", DName/binary>>),
            check_member_exists(DDoc, [<<"views">>, VName]),
            FilterType = try
                true = couch_util:get_nested_json_value(
                        DDoc#doc.body,
                        [<<"options">>, <<"seq_indexed">>]
                ),
                fast_view
            catch _:_ ->
                view
            end,
            case couch_db:is_clustered(Db) of
                true ->
                    DIR = fabric_util:doc_id_and_rev(DDoc),
                    {fetch, FilterType, Style, DIR, VName};
                false ->
                    {FilterType, Style, DDoc, VName}
            end;
        [] ->
            Msg = "`view` must be of the form `designname/viewname`",
            throw({bad_request, Msg})
    end;
configure_filter([$_ | _], _Style, _Req, _Db) ->
    throw({bad_request, "unknown builtin filter name"});
configure_filter("", main_only, _Req, _Db) ->
    {default, main_only};
configure_filter("", all_docs, _Req, _Db) ->
    {default, all_docs};
configure_filter(FilterName, Style, Req, Db) ->
    FilterNameParts = string:tokens(FilterName, "/"),
    case [?l2b(couch_httpd:unquote(Part)) || Part <- FilterNameParts] of
        [DName, FName] ->
            {ok, DDoc} = open_ddoc(Db, <<"_design/", DName/binary>>),
            check_member_exists(DDoc, [<<"filters">>, FName]),
            case couch_db:is_clustered(Db) of
                true ->
                    DIR = fabric_util:doc_id_and_rev(DDoc),
                    {fetch, custom, Style, Req, DIR, FName};
                false->
                    {custom, Style, Req, DDoc, FName}
            end;

        [] ->
            {default, Style};
        _Else ->
            Msg = "`filter` must be of the form `designname/filtername`",
            throw({bad_request, Msg})
    end.


filter(Db, #full_doc_info{}=FDI, Filter) ->
    filter(Db, couch_doc:to_doc_info(FDI), Filter);
filter(_Db, DocInfo, {default, Style}) ->
    apply_style(DocInfo, Style);
filter(_Db, DocInfo, {doc_ids, Style, DocIds}) ->
    case lists:member(DocInfo#doc_info.id, DocIds) of
        true ->
            apply_style(DocInfo, Style);
        false ->
            []
    end;
filter(Db, DocInfo, {selector, Style, {Selector, _Fields}}) ->
    Docs = open_revs(Db, DocInfo, Style),
    Passes = [mango_selector:match(Selector, couch_doc:to_json_obj(Doc, []))
        || Doc <- Docs],
    filter_revs(Passes, Docs);
filter(_Db, DocInfo, {design_docs, Style}) ->
    case DocInfo#doc_info.id of
        <<"_design", _/binary>> ->
            apply_style(DocInfo, Style);
        _ ->
            []
    end;
filter(Db, DocInfo, {FilterType, Style, DDoc, VName})
        when FilterType == view; FilterType == fast_view ->
    Docs = open_revs(Db, DocInfo, Style),
    {ok, Passes} = couch_query_servers:filter_view(DDoc, VName, Docs),
    filter_revs(Passes, Docs);
filter(Db, DocInfo, {custom, Style, Req0, DDoc, FName}) ->
    Req = case Req0 of
        {json_req, _} -> Req0;
        #httpd{} -> {json_req, couch_httpd_external:json_req_obj(Req0, Db)}
    end,
    Docs = open_revs(Db, DocInfo, Style),
    {ok, Passes} = couch_query_servers:filter_docs(Req, Db, DDoc, FName, Docs),
    filter_revs(Passes, Docs).

fast_view_filter(Db, {{Seq, _}, {ID, _, _}}, {fast_view, Style, _, _}) ->
    case couch_db:get_doc_info(Db, ID) of
        {ok, #doc_info{high_seq=Seq}=DocInfo} ->
            Docs = open_revs(Db, DocInfo, Style),
            Changes = lists:map(fun(#doc{revs={RevPos, [RevId | _]}}) ->
                RevStr = couch_doc:rev_to_str({RevPos, RevId}),
                {[{<<"rev">>, RevStr}]}
            end, Docs),
            {DocInfo, Changes};
        {ok, #doc_info{high_seq=HighSeq}} when Seq > HighSeq ->
            % If the view seq tree is out of date (or if the view seq tree
            % was opened before the db) seqs may come by from the seq tree
            % which correspond to the not-most-current revision of a document.
            % The proper thing to do is to not send this old revision, but wait
            % until we reopen the up-to-date view seq tree and continue the
            % fold.
            % I left the Seq > HighSeq guard in so if (for some godforsaken
            % reason) the seq in the view is more current than the database,
            % we'll throw an error.
            {undefined, []};
        {error, not_found} ->
            {undefined, []}
    end.



view_filter(Db, KV, {default, Style}) ->
    apply_view_style(Db, KV, Style).


get_view_qs({json_req, {Props}}) ->
    {Query} = couch_util:get_value(<<"query">>, Props, {[]}),
    binary_to_list(couch_util:get_value(<<"view">>, Query, ""));
get_view_qs(Req) ->
    couch_httpd:qs_value(Req, "view", "").

get_doc_ids({json_req, {Props}}) ->
    check_docids(couch_util:get_value(<<"doc_ids">>, Props));
get_doc_ids(#httpd{method='POST'}=Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body_obj(Req),
    check_docids(couch_util:get_value(<<"doc_ids">>, Props));
get_doc_ids(#httpd{method='GET'}=Req) ->
    DocIds = ?JSON_DECODE(couch_httpd:qs_value(Req, "doc_ids", "null")),
    check_docids(DocIds);
get_doc_ids(_) ->
    throw({bad_request, no_doc_ids_provided}).


get_selector_and_fields({json_req, {Props}}) ->
    Selector = check_selector(couch_util:get_value(<<"selector">>, Props)),
    Fields = check_fields(couch_util:get_value(<<"fields">>, Props, nil)),
    {Selector, Fields};
get_selector_and_fields(#httpd{method='POST'}=Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    get_selector_and_fields({json_req,  couch_httpd:json_body_obj(Req)});
get_selector_and_fields(_) ->
    throw({bad_request, "Selector must be specified in POST payload"}).


check_docids(DocIds) when is_list(DocIds) ->
    lists:foreach(fun
        (DocId) when not is_binary(DocId) ->
            Msg = "`doc_ids` filter parameter is not a list of doc ids.",
            throw({bad_request, Msg});
        (_) -> ok
    end, DocIds),
    DocIds;
check_docids(_) ->
    Msg = "`doc_ids` filter parameter is not a list of doc ids.",
    throw({bad_request, Msg}).


check_selector(Selector={_}) ->
    try
        mango_selector:normalize(Selector)
    catch
        {mango_error, Mod, Reason0} ->
            {_StatusCode, _Error, Reason} = mango_error:info(Mod, Reason0),
            throw({bad_request, Reason})
    end;
check_selector(_Selector) ->
    throw({bad_request, "Selector error: expected a JSON object"}).


check_fields(nil) ->
    nil;
check_fields(Fields) when is_list(Fields) ->
    try
        {ok, Fields1} = mango_fields:new(Fields),
        Fields1
    catch
        {mango_error, Mod, Reason0} ->
            {_StatusCode, _Error, Reason} = mango_error:info(Mod, Reason0),
            throw({bad_request, Reason})
    end;
check_fields(_Fields) ->
    throw({bad_request, "Selector error: fields must be JSON array"}).


open_ddoc(Db, DDocId) ->
    DbName = couch_db:name(Db),
    case couch_db:is_clustered(Db) of
        true ->
            case ddoc_cache:open_doc(mem3:dbname(DbName), DDocId) of
                {ok, _} = Resp -> Resp;
                Else -> throw(Else)
            end;
        false ->
            case couch_db:open_doc(Db, DDocId, [ejson_body]) of
                {ok, _} = Resp -> Resp;
                Else -> throw(Else)
            end
    end.


check_member_exists(#doc{body={Props}}, Path) ->
    couch_util:get_nested_json_value({Props}, Path).


apply_style(#doc_info{revs=Revs}, main_only) ->
    [#rev_info{rev=Rev} | _] = Revs,
    [{[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}];
apply_style(#doc_info{revs=Revs}, all_docs) ->
    [{[{<<"rev">>, couch_doc:rev_to_str(R)}]} || #rev_info{rev=R} <- Revs].

apply_view_style(_Db, {{_Seq, _Key}, {_ID, _Value, Rev}}, main_only) ->
    [{[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}];
apply_view_style(Db, {{_Seq, _Key}, {ID, _Value, _Rev}}, all_docs) ->
    case couch_db:get_doc_info(Db, ID) of
        {ok, DocInfo} ->
            apply_style(DocInfo, all_docs);
        {error, not_found} ->
            []
    end.


open_revs(Db, DocInfo, Style) ->
    DocInfos = case Style of
        main_only -> [DocInfo];
        all_docs -> [DocInfo#doc_info{revs=[R]}|| R <- DocInfo#doc_info.revs]
    end,
    OpenOpts = [deleted, conflicts],
    % Relying on list comprehensions to silence errors
    OpenResults = [couch_db:open_doc(Db, DI, OpenOpts) || DI <- DocInfos],
    [Doc || {ok, Doc} <- OpenResults].


filter_revs(Passes, Docs) ->
    lists:flatmap(fun
        ({true, #doc{revs={RevPos, [RevId | _]}}}) ->
            RevStr = couch_doc:rev_to_str({RevPos, RevId}),
            Change = {[{<<"rev">>, RevStr}]},
            [Change];
        (_) ->
            []
    end, lists:zip(Passes, Docs)).


get_changes_timeout(Args, Callback) ->
    #changes_args{
        heartbeat = Heartbeat,
        timeout = Timeout,
        feed = ResponseType
    } = Args,
    DefaultTimeout = list_to_integer(
        config:get("httpd", "changes_timeout", "60000")
    ),
    case Heartbeat of
    undefined ->
        case Timeout of
        undefined ->
            {DefaultTimeout, fun(UserAcc) -> {stop, UserAcc} end};
        infinity ->
            {infinity, fun(UserAcc) -> {stop, UserAcc} end};
        _ ->
            {lists:min([DefaultTimeout, Timeout]),
                fun(UserAcc) -> {stop, UserAcc} end}
        end;
    true ->
        {DefaultTimeout,
            fun(UserAcc) -> {ok, Callback(timeout, ResponseType, UserAcc)} end};
    _ ->
        {lists:min([DefaultTimeout, Heartbeat]),
            fun(UserAcc) -> {ok, Callback(timeout, ResponseType, UserAcc)} end}
    end.

start_sending_changes(_Callback, UserAcc, ResponseType)
        when ResponseType =:= "continuous"
        orelse ResponseType =:= "eventsource" ->
    UserAcc;
start_sending_changes(Callback, UserAcc, ResponseType) ->
    Callback(start, ResponseType, UserAcc).

build_acc(Args, Callback, UserAcc, Db, StartSeq, Prepend, Timeout, TimeoutFun, DDocName, ViewName, View) ->
    #changes_args{
        include_docs = IncludeDocs,
        doc_options = DocOpts,
        conflicts = Conflicts,
        limit = Limit,
        feed = ResponseType,
        filter_fun = Filter
    } = Args,
    #changes_acc{
        db = Db,
        seq = StartSeq,
        prepend = Prepend,
        filter = Filter,
        callback = Callback,
        user_acc = UserAcc,
        resp_type = ResponseType,
        limit = Limit,
        include_docs = IncludeDocs,
        doc_options = DocOpts,
        conflicts = Conflicts,
        timeout = Timeout,
        timeout_fun = TimeoutFun,
        ddoc_name = DDocName,
        view_name = ViewName,
        view = View,
        aggregation_results=[],
        aggregation_kvs=[]
    }.

send_changes(Acc, Dir, FirstRound) ->
    #changes_acc{
        db = Db,
        seq = StartSeq,
        filter = Filter,
        view = View
    } = Acc,
    DbEnumFun = fun changes_enumerator/2,
    case can_optimize(FirstRound, Filter) of
        {true, Fun} ->
            Fun(Db, StartSeq, Dir, DbEnumFun, Acc, Filter);
        _ ->
            case {View, Filter}  of
                {#mrview{}, {fast_view, _, _, _}} ->
                    couch_mrview:view_changes_since(View, StartSeq, DbEnumFun, [{dir, Dir}], Acc);
                {undefined, _} ->
                    couch_db:changes_since(Db, StartSeq, DbEnumFun, [{dir, Dir}], Acc);
                {#mrview{}, _} ->
                    ViewEnumFun = fun view_changes_enumerator/2,
                    {Go, Acc0} = couch_mrview:view_changes_since(View, StartSeq, ViewEnumFun, [{dir, Dir}], Acc),
                    case Acc0 of
                        #changes_acc{aggregation_results=[]} ->
                            {Go, Acc0};
                        _ ->
                            #changes_acc{
                                aggregation_results = AggResults,
                                aggregation_kvs = AggKVs,
                                user_acc = UserAcc,
                                callback = Callback,
                                resp_type = ResponseType,
                                prepend = Prepend
                            } = Acc0,
                            ChangesRow = view_changes_row(AggResults, AggKVs, Acc0),
                            UserAcc0 = Callback({change, ChangesRow, Prepend}, ResponseType, UserAcc),
                            reset_heartbeat(),
                            {Go, Acc0#changes_acc{user_acc=UserAcc0}}
                    end
            end
    end.


can_optimize(true, {doc_ids, _Style, DocIds})
        when length(DocIds) =< ?MAX_DOC_IDS ->
    {true, fun send_changes_doc_ids/6};
can_optimize(true, {design_docs, _Style}) ->
    {true, fun send_changes_design_docs/6};
can_optimize(_, _) ->
    false.


send_changes_doc_ids(Db, StartSeq, Dir, Fun, Acc0, {doc_ids, _Style, DocIds}) ->
    Lookups = couch_db:get_full_doc_infos(Db, DocIds),
    FullInfos = lists:foldl(fun
        ({ok, FDI}, Acc) -> [FDI | Acc];
        (not_found, Acc) -> Acc
    end, [], Lookups),
    send_lookup_changes(FullInfos, StartSeq, Dir, Db, Fun, Acc0).


send_changes_design_docs(Db, StartSeq, Dir, Fun, Acc0, {design_docs, _Style}) ->
    FoldFun = fun(FDI, Acc) -> {ok, [FDI | Acc]} end,
    KeyOpts = [{start_key, <<"_design/">>}, {end_key_gt, <<"_design0">>}],
    {ok, FullInfos} = couch_db:fold_docs(Db, FoldFun, [], KeyOpts),
    send_lookup_changes(FullInfos, StartSeq, Dir, Db, Fun, Acc0).


send_lookup_changes(FullDocInfos, StartSeq, Dir, Db, Fun, Acc0) ->
    FoldFun = case Dir of
        fwd -> fun lists:foldl/3;
        rev -> fun lists:foldr/3
    end,
    GreaterFun = case Dir of
        fwd -> fun(A, B) -> A > B end;
        rev -> fun(A, B) -> A =< B end
    end,
    DocInfos = lists:foldl(fun(FDI, Acc) ->
        DI = couch_doc:to_doc_info(FDI),
        case GreaterFun(DI#doc_info.high_seq, StartSeq) of
            true -> [DI | Acc];
            false -> Acc
        end
    end, [], FullDocInfos),
    SortedDocInfos = lists:keysort(#doc_info.high_seq, DocInfos),
    FinalAcc = try
        FoldFun(fun(DocInfo, Acc) ->
            case Fun(DocInfo, Acc) of
                {ok, NewAcc} ->
                    NewAcc;
                {stop, NewAcc} ->
                    throw({stop, NewAcc})
            end
        end, Acc0, SortedDocInfos)
    catch
        {stop, Acc} -> Acc
    end,
    case Dir of
        fwd -> {ok, FinalAcc#changes_acc{seq = couch_db:get_update_seq(Db)}};
        rev -> {ok, FinalAcc}
    end.


keep_sending_changes(Args, Acc0, FirstRound) ->
    #changes_args{
        feed = ResponseType,
        limit = Limit,
        db_open_options = DbOptions
    } = Args,

    {ok, ChangesAcc} = send_changes(Acc0, fwd, FirstRound),

    #changes_acc{
        db = Db, callback = Callback,
        timeout = Timeout, timeout_fun = TimeoutFun, seq = EndSeq,
        prepend = Prepend2, user_acc = UserAcc2, limit = NewLimit,
        ddoc_name = DDocName, view_name = ViewName
    } = ChangesAcc,

    couch_db:close(Db),
    if Limit > NewLimit, ResponseType == "longpoll" ->
        end_sending_changes(Callback, UserAcc2, EndSeq, ResponseType);
    true ->
        case wait_updated(Timeout, TimeoutFun, UserAcc2) of
        {updated, UserAcc4} ->
            DbOptions1 = [{user_ctx, couch_db:get_user_ctx(Db)} | DbOptions],
            case couch_db:open(couch_db:name(Db), DbOptions1) of
            {ok, Db2} ->
                keep_sending_changes(
                  Args#changes_args{limit=NewLimit},
                  ChangesAcc#changes_acc{
                    db = Db2,
                    view = maybe_refresh_view(Db2, DDocName, ViewName),
                    user_acc = UserAcc4,
                    seq = EndSeq,
                    prepend = Prepend2,
                    timeout = Timeout,
                    timeout_fun = TimeoutFun},
                  false);
            _Else ->
                end_sending_changes(Callback, UserAcc2, EndSeq, ResponseType)
            end;
        {stop, UserAcc4} ->
            end_sending_changes(Callback, UserAcc4, EndSeq, ResponseType)
        end
    end.

maybe_refresh_view(_, undefined, undefined) ->
    undefined;
maybe_refresh_view(Db, DDocName, ViewName) ->
    DbName = couch_db:name(Db),
    {ok, {_, View, _}, _, _} = couch_mrview_util:get_view(DbName, DDocName, ViewName, #mrargs{}),
    View.

end_sending_changes(Callback, UserAcc, EndSeq, ResponseType) ->
    Callback({stop, EndSeq}, ResponseType, UserAcc).

view_changes_enumerator(Value, Acc) ->
    #changes_acc{
        filter = Filter, callback = Callback, prepend = Prepend,
        user_acc = UserAcc, limit = Limit, resp_type = ResponseType, db = Db,
        timeout = Timeout, timeout_fun = TimeoutFun, seq = CurrentSeq,
        aggregation_kvs=AggKVs, aggregation_results=AggResults
    } = Acc,

    Results0 = view_filter(Db, Value, Filter),
    Results = [Result || Result <- Results0, Result /= null],
    {{Seq, _}, _} = Value,

    Go = if (Limit =< 1) andalso Results =/= [] -> stop; true -> ok end,

    if CurrentSeq =:= Seq ->
        NewAggKVs = case Results of
            [] -> AggKVs;
            _ -> [Value|AggKVs]
        end,
        {Done, UserAcc2} = maybe_heartbeat(Timeout, TimeoutFun, UserAcc),
        Acc0 = Acc#changes_acc{
            seq = Seq,
            user_acc = UserAcc2,
            aggregation_kvs=NewAggKVs
        },
        case Done of
            stop -> {stop, Acc0};
            ok -> {Go, Acc0}
        end;
    AggResults =/= [] ->
        {NewAggKVs, NewAggResults} = case Results of
            [] -> {[], []};
            _ -> {[Value], Results}
        end,
        if ResponseType =:= "continuous" orelse ResponseType =:= "eventsource" ->
            ChangesRow = view_changes_row(AggResults, AggKVs, Acc),
            UserAcc2 = Callback({change, ChangesRow, <<>>}, ResponseType, UserAcc),
            reset_heartbeat(),
            {Go, Acc#changes_acc{
                seq = Seq, user_acc = UserAcc2, limit = Limit - 1,
                aggregation_kvs=NewAggKVs, aggregation_results=NewAggResults}};
        true ->
            ChangesRow = view_changes_row(AggResults, AggKVs, Acc),
            UserAcc2 = Callback({change, ChangesRow, Prepend}, ResponseType, UserAcc),
            reset_heartbeat(),
            {Go, Acc#changes_acc{
                seq = Seq, prepend = <<",\n">>, user_acc = UserAcc2,
                limit = Limit - 1, aggregation_kvs=[Value],
                aggregation_results=Results}}
        end;
    true ->
        {NewAggKVs, NewAggResults} = case Results of
            [] -> {[], []};
            _ -> {[Value], Results}
        end,
        {Done, UserAcc2} = maybe_heartbeat(Timeout, TimeoutFun, UserAcc),
        Acc0 = Acc#changes_acc{
            seq = Seq,
            user_acc = UserAcc2,
            aggregation_kvs=NewAggKVs,
            aggregation_results=NewAggResults
        },
        case Done of
            stop -> {stop, Acc0};
            ok -> {Go, Acc0}
        end
    end.

changes_enumerator(Value0, Acc) ->
    #changes_acc{
        filter = Filter, callback = Callback, prepend = Prepend,
        user_acc = UserAcc, limit = Limit, resp_type = ResponseType, db = Db,
        timeout = Timeout, timeout_fun = TimeoutFun
    } = Acc,
    {Value, Results0} = case Filter of
        {fast_view, _, _, _} ->
            fast_view_filter(Db, Value0, Filter);
        _ ->
            {Value0, filter(Db, Value0, Filter)}
    end,
    Results = [Result || Result <- Results0, Result /= null],
    Seq = case Value of
        #doc_info{} ->
            Value#doc_info.high_seq;
        {{Seq0, _}, _} ->
            Seq0
    end,
    Go = if (Limit =< 1) andalso Results =/= [] -> stop; true -> ok end,
    case Results of
    [] ->
        {Done, UserAcc2} = maybe_heartbeat(Timeout, TimeoutFun, UserAcc),
        case Done of
        stop ->
            {stop, Acc#changes_acc{seq = Seq, user_acc = UserAcc2}};
        ok ->
            {Go, Acc#changes_acc{seq = Seq, user_acc = UserAcc2}}
        end;
    _ ->
        if ResponseType =:= "continuous" orelse ResponseType =:= "eventsource" ->
            ChangesRow = changes_row(Results, Value, Acc),
            UserAcc2 = Callback({change, ChangesRow, <<>>}, ResponseType, UserAcc),
            reset_heartbeat(),
            {Go, Acc#changes_acc{seq = Seq, user_acc = UserAcc2, limit = Limit - 1}};
        true ->
            ChangesRow = changes_row(Results, Value, Acc),
            UserAcc2 = Callback({change, ChangesRow, Prepend}, ResponseType, UserAcc),
            reset_heartbeat(),
            {Go, Acc#changes_acc{
                seq = Seq, prepend = <<",\n">>,
                user_acc = UserAcc2, limit = Limit - 1}}
        end
    end.



view_changes_row(Results, KVs, Acc) ->
    {Add, Remove} = lists:foldl(fun(Row, {AddAcc, RemAcc}) ->
        {{_Seq, Key}, {_Id, Value, _Rev}} = Row,
        case Value of
            removed ->
                {AddAcc, [Key|RemAcc]};
            {dups, DupValues} ->
                AddAcc1 = lists:foldl(fun(DupValue, AddAcc0) ->
                    [[Key, DupValue]|AddAcc0]
                end, AddAcc, DupValues),
                {AddAcc1, RemAcc};
            _ ->
                {[[Key, Value]|AddAcc], RemAcc}
        end
    end, {[], []}, KVs),

    % Seq, Id, and Rev should be the same for all KVs, since we're aggregating
    % by seq.
    [{{Seq, _Key}, {Id, _Value, Rev}}|_] = KVs,

    {[
        {<<"seq">>, Seq}, {<<"id">>, Id}, {<<"add">>, Add},
        {<<"remove">>, Remove}, {<<"changes">>, Results}
    ] ++ maybe_get_changes_doc({Id, Rev}, Acc)}.


changes_row(Results, DocInfo, Acc) ->
    #doc_info{
        id = Id, high_seq = Seq, revs = [#rev_info{deleted = Del} | _]
    } = DocInfo,
    {[{<<"seq">>, Seq}, {<<"id">>, Id}, {<<"changes">>, Results}] ++
        deleted_item(Del) ++ maybe_get_changes_doc(DocInfo, Acc)}.

maybe_get_changes_doc(Value, #changes_acc{include_docs=true}=Acc) ->
    #changes_acc{
        db = Db,
        doc_options = DocOpts,
        conflicts = Conflicts,
        filter = Filter
    } = Acc,
    Opts = case Conflicts of
               true -> [deleted, conflicts];
               false -> [deleted]
           end,
    load_doc(Db, Value, Opts, DocOpts, Filter);

maybe_get_changes_doc(_Value, _Acc) ->
    [].


load_doc(Db, Value, Opts, DocOpts, Filter) ->
    case couch_index_util:load_doc(Db, Value, Opts) of
        null ->
            [{doc, null}];
        Doc ->
            [{doc, doc_to_json(Doc, DocOpts, Filter)}]
    end.


doc_to_json(Doc, DocOpts, {selector, _Style, {_Selector, Fields}})
    when Fields =/= nil ->
    mango_fields:extract(couch_doc:to_json_obj(Doc, DocOpts), Fields);
doc_to_json(Doc, DocOpts, _Filter) ->
    couch_doc:to_json_obj(Doc, DocOpts).


deleted_item(true) -> [{<<"deleted">>, true}];
deleted_item(_) -> [].

% waits for a updated msg, if there are multiple msgs, collects them.
wait_updated(Timeout, TimeoutFun, UserAcc) ->
    receive
    updated ->
        get_rest_updated(UserAcc);
    deleted ->
        {stop, UserAcc}
    after Timeout ->
        {Go, UserAcc2} = TimeoutFun(UserAcc),
        case Go of
        ok ->
            wait_updated(Timeout, TimeoutFun, UserAcc2);
        stop ->
            {stop, UserAcc2}
        end
    end.

get_rest_updated(UserAcc) ->
    receive
    updated ->
        get_rest_updated(UserAcc)
    after 0 ->
        {updated, UserAcc}
    end.

reset_heartbeat() ->
    case get(last_changes_heartbeat) of
    undefined ->
        ok;
    _ ->
        put(last_changes_heartbeat, os:timestamp())
    end.

maybe_heartbeat(Timeout, TimeoutFun, Acc) ->
    Before = get(last_changes_heartbeat),
    case Before of
    undefined ->
        {ok, Acc};
    _ ->
        Now = os:timestamp(),
        case timer:now_diff(Now, Before) div 1000 >= Timeout of
        true ->
            Acc2 = TimeoutFun(Acc),
            put(last_changes_heartbeat, Now),
            Acc2;
        false ->
            {ok, Acc}
        end
    end.
