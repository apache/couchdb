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

-module(chttpd_changes).
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([
    handle_db_changes/3,
    get_changes_timeout/2,
    wait_updated/3,
    get_rest_updated/1,
    configure_filter/4,
    filter/3,
    handle_db_event/3,
    handle_view_event/3,
    send_changes_doc_ids/6,
    send_changes_design_docs/6
]).

-export([changes_enumerator/2]).

%% export so we can use fully qualified call to facilitate hot-code upgrade
-export([
    keep_sending_changes/3
]).

-record(changes_acc, {
    db,
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

handle_db_changes(Args0, Req, Db0) ->
    #changes_args{
        style = Style,
        filter = FilterName,
        feed = Feed,
        dir = Dir,
        since = Since
    } = Args0,
    Filter = configure_filter(FilterName, Style, Req, Db0),
    Args = Args0#changes_args{filter_fun = Filter},
    DbName = fabric2_db:name(Db0),
    StartListenerFun = fun() ->
        fabric2_events:link_listener(
                ?MODULE, handle_db_event, self(), [{dbname, DbName}]
            )
    end,
    Start = fun() ->
        StartSeq = case Dir =:= rev orelse Since =:= now of
            true -> fabric2_db:get_update_seq(Db0);
            false -> Since
        end,
        {Db0, StartSeq}
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

            {Db, StartSeq} = Start(),
            UserAcc2 = start_sending_changes(Callback, UserAcc),
            {Timeout, TimeoutFun} = get_changes_timeout(Args, Callback),
            Acc0 = build_acc(Args, Callback, UserAcc2, Db, StartSeq,
                             <<"">>, Timeout, TimeoutFun),
            try
                keep_sending_changes(
                    Args#changes_args{dir=fwd},
                    Acc0,
                    true)
            after
                fabric2_events:stop_listener(Listener),
                get_rest_updated(ok) % clean out any remaining update messages
            end
        end;
    false ->
        fun(CallbackAcc) ->
            {Callback, UserAcc} = get_callback_acc(CallbackAcc),
            UserAcc2 = start_sending_changes(Callback, UserAcc),
            {Timeout, TimeoutFun} = get_changes_timeout(Args, Callback),
            {Db, StartSeq} = Start(),
            Acc0 = build_acc(Args#changes_args{feed="normal"}, Callback,
                             UserAcc2, Db, StartSeq, <<>>,
                             Timeout, TimeoutFun),
            {ok, #changes_acc{seq = LastSeq, user_acc = UserAcc3}} =
                send_changes(
                    Acc0,
                    Dir,
                    true),
            end_sending_changes(Callback, UserAcc3, LastSeq)
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

get_callback_acc({Callback, _UserAcc} = Pair) when is_function(Callback, 2) ->
    Pair;
get_callback_acc(Callback) when is_function(Callback, 1) ->
    {fun(Ev, _) -> Callback(Ev) end, ok}.


configure_filter(Filter, _Style, _Req, _Db) when is_tuple(Filter) ->
    % Filter has already been configured
    Filter;
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
            case fabric2_db:is_clustered(Db) of
                true ->
                    DIR = fabric_util:doc_id_and_rev(DDoc),
                    {fetch, view, Style, DIR, VName};
                false ->
                    {view, Style, DDoc, VName}
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
            {custom, Style, Req, DDoc, FName};
        [] ->
            {default, Style};
        _Else ->
            Msg = "`filter` must be of the form `designname/filtername`",
            throw({bad_request, Msg})
    end.


filter(Db, Change, {default, Style}) ->
    apply_style(Db, Change, Style);
filter(Db, Change, {doc_ids, Style, DocIds}) ->
    case lists:member(maps:get(id, Change), DocIds) of
        true ->
            apply_style(Db, Change, Style);
        false ->
            []
    end;
filter(Db, Change, {selector, Style, {Selector, _Fields}}) ->
    Docs = open_revs(Db, Change, Style),
    Passes = [mango_selector:match(Selector, couch_doc:to_json_obj(Doc, []))
        || Doc <- Docs],
    filter_revs(Passes, Docs);
filter(Db, Change, {design_docs, Style}) ->
    case maps:get(id, Change) of
        <<"_design", _/binary>> ->
            apply_style(Db, Change, Style);
        _ ->
            []
    end;
filter(Db, Change, {view, Style, DDoc, VName}) ->
    Docs = open_revs(Db, Change, Style),
    {ok, Passes} = couch_query_servers:filter_view(DDoc, VName, Docs),
    filter_revs(Passes, Docs);
filter(Db, Change, {custom, Style, Req0, DDoc, FName}) ->
    Req = case Req0 of
        {json_req, _} -> Req0;
        #httpd{} -> {json_req, chttpd_external:json_req_obj(Req0, Db)}
    end,
    Docs = open_revs(Db, Change, Style),
    {ok, Passes} = couch_query_servers:filter_docs(Req, Db, DDoc, FName, Docs),
    filter_revs(Passes, Docs);
filter(Db, Change, Filter) ->
    erlang:error({filter_error, Db, Change, Filter}).


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
    case fabric2_db:open_doc(Db, DDocId, [ejson_body, ?ADMIN_CTX]) of
        {ok, _} = Resp -> Resp;
        Else -> throw(Else)
    end.


check_member_exists(#doc{body={Props}}, Path) ->
    couch_util:get_nested_json_value({Props}, Path).


apply_style(_Db, Change, main_only) ->
    #{rev_id := RevId} = Change,
    [{[{<<"rev">>, couch_doc:rev_to_str(RevId)}]}];
apply_style(Db, Change, all_docs) ->
    % We have to fetch all revs for this row
    #{id := DocId} = Change,
    {ok, Resps} = fabric2_db:open_doc_revs(Db, DocId, all, [deleted]),
    lists:flatmap(fun(Resp) ->
        case Resp of
            {ok, #doc{revs = {Pos, [Rev | _]}}} ->
                [{[{<<"rev">>, couch_doc:rev_to_str({Pos, Rev})}]}];
            _ ->
                []
        end
    end, Resps);
apply_style(Db, Change, Style) ->
    erlang:error({changes_apply_style, Db, Change, Style}).


open_revs(Db, Change, Style) ->
    #{id := DocId} = Change,
    Options = [deleted, conflicts],
    try
        case Style of
            main_only ->
                {ok, Doc} = fabric2_db:open_doc(Db, DocId, Options),
                [Doc];
            all_docs ->
                {ok, Docs} = fabric2_db:open_doc_revs(Db, DocId, all, Options),
                [Doc || {ok, Doc} <- Docs]
        end
    catch _:_ ->
        % We didn't log this before, should we now?
        []
    end.


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
            fun(UserAcc) -> Callback({timeout, ResponseType}, UserAcc) end};
    _ ->
        {lists:min([DefaultTimeout, Heartbeat]),
            fun(UserAcc) -> Callback({timeout, ResponseType}, UserAcc) end}
    end.

start_sending_changes(Callback, UserAcc) ->
    {_, NewUserAcc} = Callback(start, UserAcc),
    NewUserAcc.

build_acc(Args, Callback, UserAcc, Db, StartSeq, Prepend, Timeout, TimeoutFun) ->
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
        aggregation_results=[],
        aggregation_kvs=[]
    }.

send_changes(Acc, Dir, FirstRound) ->
    #changes_acc{
        db = Db,
        seq = StartSeq,
        filter = Filter
    } = maybe_upgrade_changes_acc(Acc),
    DbEnumFun = fun changes_enumerator/2,
    case can_optimize(FirstRound, Filter) of
        {true, Fun} ->
            Fun(Db, StartSeq, Dir, DbEnumFun, Acc, Filter);
        _ ->
            Opts = [{dir, Dir}],
            fabric2_db:fold_changes(Db, StartSeq, DbEnumFun, Acc, Opts)
    end.


can_optimize(true, {doc_ids, _Style, DocIds}) ->
    MaxDocIds = config:get_integer("couchdb",
        "changes_doc_ids_optimization_threshold", 100),
    if length(DocIds) =< MaxDocIds ->
        {true, fun send_changes_doc_ids/6};
    true ->
        false
    end;
can_optimize(true, {design_docs, _Style}) ->
    {true, fun send_changes_design_docs/6};
can_optimize(_, _) ->
    false.


send_changes_doc_ids(Db, StartSeq, Dir, Fun, Acc0, {doc_ids, _Style, DocIds}) ->
    Results = fabric2_db:get_full_doc_infos(Db, DocIds),
    FullInfos = lists:foldl(fun
        (#full_doc_info{}=FDI, Acc) -> [FDI | Acc];
        (not_found, Acc) -> Acc
    end, [], Results),
    send_lookup_changes(FullInfos, StartSeq, Dir, Db, Fun, Acc0).


send_changes_design_docs(Db, StartSeq, Dir, Fun, Acc0, {design_docs, _Style}) ->
    FoldFun = fun(FDI, Acc) -> {ok, [FDI | Acc]} end,
    Opts = [
        include_deleted,
        {start_key, <<"_design/">>},
        {end_key_gt, <<"_design0">>}
    ],
    {ok, FullInfos} = fabric2_db:fold_docs(Db, FoldFun, [], Opts),
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
            % Kinda gross that we're munging this back to a map
            % that will then have to re-read and rebuild the FDI
            % for all_docs style. But c'est la vie.
            #doc_info{
                id = DocId,
                high_seq = Seq,
                revs = [#rev_info{rev = Rev, deleted = Deleted} | _]
            } = DocInfo,
            Change = #{
                id => DocId,
                sequence => Seq,
                rev_id => Rev,
                deleted => Deleted
            },
            case Fun(Change, Acc) of
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
        fwd ->
            FinalAcc0 = case element(1, FinalAcc) of
                changes_acc -> % we came here via couch_http or internal call
                    FinalAcc#changes_acc{seq = fabric2_db:get_update_seq(Db)};
                fabric_changes_acc -> % we came here via chttpd / fabric / rexi
                    FinalAcc#fabric_changes_acc{seq = couch_db:get_update_seq(Db)}
            end,
            {ok, FinalAcc0};
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
        prepend = Prepend2, user_acc = UserAcc2, limit = NewLimit
    } = maybe_upgrade_changes_acc(ChangesAcc),

    if Limit > NewLimit, ResponseType == "longpoll" ->
        end_sending_changes(Callback, UserAcc2, EndSeq);
    true ->
        {Go, UserAcc3} = notify_waiting_for_updates(Callback, UserAcc2),
        if Go /= ok -> end_sending_changes(Callback, UserAcc3, EndSeq); true ->
            case wait_updated(Timeout, TimeoutFun, UserAcc3) of
            {updated, UserAcc4} ->
                UserCtx = fabric2_db:get_user_ctx(Db),
                DbOptions1 = [{user_ctx, UserCtx} | DbOptions],
                case fabric2_db:open(fabric2_db:name(Db), DbOptions1) of
                {ok, Db2} ->
                    ?MODULE:keep_sending_changes(
                      Args#changes_args{limit=NewLimit},
                      ChangesAcc#changes_acc{
                        db = Db2,
                        user_acc = UserAcc4,
                        seq = EndSeq,
                        prepend = Prepend2,
                        timeout = Timeout,
                        timeout_fun = TimeoutFun},
                      false);
                _Else ->
                    end_sending_changes(Callback, UserAcc3, EndSeq)
                end;
            {stop, UserAcc4} ->
                end_sending_changes(Callback, UserAcc4, EndSeq)
            end
        end
    end.

notify_waiting_for_updates(Callback, UserAcc) ->
    Callback(waiting_for_updates, UserAcc).

end_sending_changes(Callback, UserAcc, EndSeq) ->
    Callback({stop, EndSeq, null}, UserAcc).

changes_enumerator(Change, Acc) ->
    #changes_acc{
        filter = Filter,
        callback = Callback,
        user_acc = UserAcc,
        limit = Limit,
        db = Db,
        timeout = Timeout,
        timeout_fun = TimeoutFun
    } = maybe_upgrade_changes_acc(Acc),
    Results0 = filter(Db, Change, Filter),
    Results = [Result || Result <- Results0, Result /= null],
    Seq = maps:get(sequence, Change),
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
        ChangesRow = changes_row(Results, Change, Acc),
        {UserGo, UserAcc2} = Callback({change, ChangesRow}, UserAcc),
        RealGo = case UserGo of
            ok -> Go;
            stop -> stop
        end,
        reset_heartbeat(),
        {RealGo, Acc#changes_acc{
            seq = Seq,
            user_acc = UserAcc2,
            limit = Limit - 1
        }}
    end.


changes_row(Results, Change, Acc) ->
    #{
        id := Id,
        sequence := Seq,
        deleted := Del
    } = Change,
    {[
        {<<"seq">>, Seq},
        {<<"id">>, Id},
        {<<"changes">>, Results}
    ] ++ deleted_item(Del) ++ maybe_get_changes_doc(Change, Acc)}.

maybe_get_changes_doc(Value, #changes_acc{include_docs=true}=Acc) ->
    #changes_acc{
        db = Db,
        doc_options = DocOpts0,
        conflicts = Conflicts,
        filter = Filter
    } = Acc,
    OpenOpts = case Conflicts of
        true -> [deleted, conflicts];
        false -> [deleted]
    end,
    DocOpts1 = case Conflicts of
        true -> [conflicts | DocOpts0];
        false -> DocOpts0
    end,
    load_doc(Db, Value, OpenOpts, DocOpts1, Filter);

maybe_get_changes_doc(_Value, _Acc) ->
    [].


load_doc(Db, Value, Opts, DocOpts, Filter) ->
    case load_doc(Db, Value, Opts) of
        null ->
            [{doc, null}];
        Doc ->
            [{doc, doc_to_json(Doc, DocOpts, Filter)}]
    end.


load_doc(Db, Change, Opts) ->
    #{
        id := Id,
        rev_id := RevId
    } = Change,
    case fabric2_db:open_doc_revs(Db, Id, [RevId], Opts) of
        {ok, [{ok, Doc}]} ->
            Doc;
        _ ->
            null
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
            ?MODULE:wait_updated(Timeout, TimeoutFun, UserAcc2);
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
            {StopOrGo, Acc2} = TimeoutFun(Acc),
            put(last_changes_heartbeat, Now),
            {StopOrGo, Acc2};
        false ->
            {ok, Acc}
        end
    end.


maybe_upgrade_changes_acc(#changes_acc{} = Acc) ->
    Acc;
maybe_upgrade_changes_acc(Acc) when tuple_size(Acc) == 19 ->
    #changes_acc{
        db = element(2, Acc),
        seq = element(6, Acc),
        prepend = element(7, Acc),
        filter = element(8, Acc),
        callback = element(9, Acc),
        user_acc = element(10, Acc),
        resp_type = element(11, Acc),
        limit = element(12, Acc),
        include_docs = element(13, Acc),
        doc_options = element(14, Acc),
        conflicts = element(15, Acc),
        timeout = element(16, Acc),
        timeout_fun = element(17, Acc),
        aggregation_kvs = element(18, Acc),
        aggregation_results = element(19, Acc)
    }.
