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

-module(couch_mrview_http).

-export([
    handle_all_docs_req/2,
    handle_local_docs_req/2,
    handle_design_docs_req/2,
    handle_view_changes_req/3,
    handle_reindex_req/3,
    handle_view_req/3,
    handle_temp_view_req/2,
    handle_info_req/3,
    handle_compact_req/3,
    handle_cleanup_req/2
]).

-export([
    parse_boolean/1,
    parse_int/1,
    parse_pos_int/1,
    prepend_val/1,
    parse_params/2,
    parse_params/3,
    parse_params/4,
    view_cb/2,
    row_to_json/1,
    row_to_json/2,
    check_view_etag/3
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").


handle_all_docs_req(#httpd{method='GET'}=Req, Db) ->
    all_docs_req(Req, Db, undefined);
handle_all_docs_req(#httpd{method='POST'}=Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    Keys = couch_mrview_util:get_view_keys(chttpd:json_body_obj(Req)),
    all_docs_req(Req, Db, Keys);
handle_all_docs_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").

handle_local_docs_req(#httpd{method='GET'}=Req, Db) ->
    all_docs_req(Req, Db, undefined, <<"_local">>);
handle_local_docs_req(#httpd{method='POST'}=Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    Keys = couch_mrview_util:get_view_keys(chttpd:json_body_obj(Req)),
    all_docs_req(Req, Db, Keys, <<"_local">>);
handle_local_docs_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").

handle_design_docs_req(#httpd{method='GET'}=Req, Db) ->
    all_docs_req(Req, Db, undefined, <<"_design">>);
handle_design_docs_req(#httpd{method='POST'}=Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    Keys = couch_mrview_util:get_view_keys(chttpd:json_body_obj(Req)),
    all_docs_req(Req, Db, Keys, <<"_design">>);
handle_design_docs_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").

handle_reindex_req(#httpd{method='POST',
                          path_parts=[_, _, DName,<<"_reindex">>]}=Req,
                   Db, _DDoc) ->
    chttpd:validate_ctype(Req, "application/json"),
    ok = couch_db:check_is_admin(Db),
    couch_mrview:trigger_update(Db, <<"_design/", DName/binary>>),
    chttpd:send_json(Req, 201, {[{<<"ok">>, true}]});
handle_reindex_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "POST").


handle_view_changes_req(#httpd{path_parts=[_,<<"_design">>,DDocName,<<"_view_changes">>,ViewName]}=Req, Db, DDoc) ->
    {DDocBody} = DDoc#doc.body,
    case lists:keyfind(<<"options">>, 1, DDocBody) of
        {<<"options">>, {Options}} when is_list(Options) ->
            case lists:keyfind(<<"seq_indexed">>, 1, Options) of
                {<<"seq_indexed">>, true} ->
                    ok;
                _ ->
                    throw({bad_request, "view changes not enabled"})
            end;
        _ ->
            throw({bad_request, "view changes not enabled"})
    end,

    ChangesArgs = couch_httpd_db:parse_changes_query(Req, Db),
    ChangesFun = couch_mrview_changes:handle_view_changes(ChangesArgs, Req, Db, <<"_design/", DDocName/binary>>, ViewName),
    couch_httpd_db:handle_changes_req(Req, Db, ChangesArgs, ChangesFun).


handle_view_req(#httpd{method='GET',
                      path_parts=[_, _, DDocName, _, VName, <<"_info">>]}=Req,
                Db, _DDoc) ->
    DbName = couch_db:name(Db),
    DDocId = <<"_design/", DDocName/binary >>,
    {ok, Info} = couch_mrview:get_view_info(DbName, DDocId, VName),

    FinalInfo = [{db_name, DbName},
                 {ddoc, DDocId},
                 {view, VName}] ++ Info,
    chttpd:send_json(Req, 200, {FinalInfo});
handle_view_req(#httpd{method='GET'}=Req, Db, DDoc) ->
    [_, _, _, _, ViewName] = Req#httpd.path_parts,
    couch_stats:increment_counter([couchdb, httpd, view_reads]),
    design_doc_view(Req, Db, DDoc, ViewName, undefined);
handle_view_req(#httpd{method='POST'}=Req, Db, DDoc) ->
    chttpd:validate_ctype(Req, "application/json"),
    [_, _, _, _, ViewName] = Req#httpd.path_parts,
    Props = chttpd:json_body_obj(Req),
    Keys = couch_mrview_util:get_view_keys(Props),
    Queries = couch_mrview_util:get_view_queries(Props),
    case {Queries, Keys} of
        {Queries, undefined} when is_list(Queries) ->
            IncrBy = length(Queries),
            couch_stats:increment_counter([couchdb, httpd, view_reads], IncrBy),
            multi_query_view(Req, Db, DDoc, ViewName, Queries);
        {undefined, Keys} when is_list(Keys) ->
            couch_stats:increment_counter([couchdb, httpd, view_reads]),
            design_doc_view(Req, Db, DDoc, ViewName, Keys);
        {undefined, undefined} ->
            throw({
                bad_request,
                "POST body must contain `keys` or `queries` field"
            });
        {_, _} ->
            throw({bad_request, "`keys` and `queries` are mutually exclusive"})
    end;
handle_view_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").


handle_temp_view_req(#httpd{method='POST'}=Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    ok = couch_db:check_is_admin(Db),
    {Body} = chttpd:json_body_obj(Req),
    DDoc = couch_mrview_util:temp_view_to_ddoc({Body}),
    Keys = couch_mrview_util:get_view_keys({Body}),
    couch_stats:increment_counter([couchdb, httpd, temporary_view_reads]),
    design_doc_view(Req, Db, DDoc, <<"temp">>, Keys);
handle_temp_view_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


handle_info_req(#httpd{method='GET'}=Req, Db, DDoc) ->
    [_, _, Name, _] = Req#httpd.path_parts,
    {ok, Info} = couch_mrview:get_info(Db, DDoc),
    chttpd:send_json(Req, 200, {[
        {name, Name},
        {view_index, {Info}}
    ]});
handle_info_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "GET").


handle_compact_req(#httpd{method='POST'}=Req, Db, DDoc) ->
    chttpd:validate_ctype(Req, "application/json"),
    ok = couch_db:check_is_admin(Db),
    ok = couch_mrview:compact(Db, DDoc),
    chttpd:send_json(Req, 202, {[{ok, true}]});
handle_compact_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "POST").


handle_cleanup_req(#httpd{method='POST'}=Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    ok = couch_db:check_is_admin(Db),
    ok = couch_mrview:cleanup(Db),
    chttpd:send_json(Req, 202, {[{ok, true}]});
handle_cleanup_req(Req, _Db) ->
    chttpd:send_method_not_allowed(Req, "POST").


all_docs_req(Req, Db, Keys) ->
    all_docs_req(Req, Db, Keys, undefined).

all_docs_req(Req, Db, Keys, NS) ->
    case is_restricted(Db, NS) of
    true ->
        case (catch couch_db:check_is_admin(Db)) of
        ok ->
            do_all_docs_req(Req, Db, Keys, NS);
        _ when NS == <<"_local">> ->
            throw({forbidden, <<"Only admins can access _local_docs">>});
        _ ->
            case is_public_fields_configured(Db) of
                true ->
                    do_all_docs_req(Req, Db, Keys, NS);
                false ->
                    throw({forbidden, <<"Only admins can access _all_docs",
                                        " of system databases.">>})
            end
        end;
    false ->
        do_all_docs_req(Req, Db, Keys, NS)
    end.

is_restricted(_Db, <<"_local">>) ->
    true;
is_restricted(Db, _) ->
    couch_db:is_system_db(Db).

is_public_fields_configured(Db) ->
    DbName = ?b2l(couch_db:name(Db)),
    case config:get("couch_httpd_auth", "authentication_db", "_users") of
    DbName ->
        UsersDbPublic = config:get("couch_httpd_auth", "users_db_public", "false"),
        PublicFields = config:get("couch_httpd_auth", "public_fields"),
        case {UsersDbPublic, PublicFields} of
        {"true", PublicFields} when PublicFields =/= undefined ->
            true;
        {_, _} ->
            false
        end;
    _ ->
        false
    end.

do_all_docs_req(Req, Db, Keys, NS) ->
    Args0 = parse_params(Req, Keys),
    Args1 = set_namespace(NS, Args0),
    ETagFun = fun(Sig, Acc0) ->
        check_view_etag(Sig, Acc0, Req)
    end,
    Args = Args1#mrargs{preflight_fun=ETagFun},
    {ok, Resp} = couch_httpd:etag_maybe(Req, fun() ->
        Max = chttpd:chunked_response_buffer_size(),
        VAcc0 = #vacc{db=Db, req=Req, threshold=Max},
        DbName = ?b2l(couch_db:name(Db)),
        UsersDbName = config:get("couch_httpd_auth",
                                 "authentication_db",
                                 "_users"),
        IsAdmin = is_admin(Db),
        Callback = get_view_callback(DbName, UsersDbName, IsAdmin),
        couch_mrview:query_all_docs(Db, Args, Callback, VAcc0)
    end),
    case is_record(Resp, vacc) of
        true -> {ok, Resp#vacc.resp};
        _ -> {ok, Resp}
    end.

set_namespace(NS, #mrargs{extra = Extra} = Args) ->
    Args#mrargs{extra = [{namespace, NS} | Extra]}.

is_admin(Db) ->
    case catch couch_db:check_is_admin(Db) of
    {unauthorized, _} ->
        false;
    ok ->
        true
    end.


% admin users always get all fields
get_view_callback(_, _, true) ->
    fun view_cb/2;
% if we are operating on the users db and we aren't
% admin, filter the view
get_view_callback(_DbName, _DbName, false) ->
    fun filtered_view_cb/2;
% non _users databases get all fields
get_view_callback(_, _, _) ->
    fun view_cb/2.


design_doc_view(Req, Db, DDoc, ViewName, Keys) ->
    Args0 = parse_params(Req, Keys),
    ETagFun = fun(Sig, Acc0) ->
        check_view_etag(Sig, Acc0, Req)
    end,
    Args = Args0#mrargs{preflight_fun=ETagFun},
    {ok, Resp} = couch_httpd:etag_maybe(Req, fun() ->
        Max = chttpd:chunked_response_buffer_size(),
        VAcc0 = #vacc{db=Db, req=Req, threshold=Max},
        couch_mrview:query_view(Db, DDoc, ViewName, Args, fun view_cb/2, VAcc0)
    end),
    case is_record(Resp, vacc) of
        true -> {ok, Resp#vacc.resp};
        _ -> {ok, Resp}
    end.


multi_query_view(Req, Db, DDoc, ViewName, Queries) ->
    Args0 = parse_params(Req, undefined),
    {ok, _, _, Args1} = couch_mrview_util:get_view(Db, DDoc, ViewName, Args0),
    ArgQueries = lists:map(fun({Query}) ->
        QueryArg = parse_params(Query, undefined, Args1),
        couch_mrview_util:validate_args(QueryArg)
    end, Queries),
    {ok, Resp2} = couch_httpd:etag_maybe(Req, fun() ->
        Max = chttpd:chunked_response_buffer_size(),
        VAcc0 = #vacc{db=Db, req=Req, prepend="\r\n", threshold=Max},
        %% TODO: proper calculation of etag
        Etag = [$", couch_uuids:new(), $"],
        Headers = [{"ETag", Etag}],
        FirstChunk = "{\"results\":[",
        {ok, Resp0} = chttpd:start_delayed_json_response(VAcc0#vacc.req, 200, Headers, FirstChunk),
        VAcc1 = VAcc0#vacc{resp=Resp0},
        VAcc2 = lists:foldl(fun(Args, Acc0) ->
            {ok, Acc1} = couch_mrview:query_view(Db, DDoc, ViewName, Args, fun view_cb/2, Acc0),
            Acc1
        end, VAcc1, ArgQueries),
        {ok, Resp1} = chttpd:send_delayed_chunk(VAcc2#vacc.resp, "\r\n]}"),
        {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
        {ok, VAcc2#vacc{resp=Resp2}}
    end),
    case is_record(Resp2, vacc) of
        true -> {ok, Resp2#vacc.resp};
        _ -> {ok, Resp2}
    end.

filtered_view_cb({row, Row0}, Acc) ->
  Row1 = lists:map(fun({doc, null}) ->
        {doc, null};
    ({doc, Body}) ->
        Doc = couch_users_db:strip_non_public_fields(#doc{body=Body}),
        {doc, Doc#doc.body};
    (KV) ->
        KV
    end, Row0),
    view_cb({row, Row1}, Acc);
filtered_view_cb(Obj, Acc) ->
    view_cb(Obj, Acc).


%% these clauses start (and possibly end) the response
view_cb({error, Reason}, #vacc{resp=undefined}=Acc) ->
    {ok, Resp} = chttpd:send_error(Acc#vacc.req, Reason),
    {ok, Acc#vacc{resp=Resp}};

view_cb(complete, #vacc{resp=undefined}=Acc) ->
    % Nothing in view
    {ok, Resp} = chttpd:send_json(Acc#vacc.req, 200, {[{rows, []}]}),
    {ok, Acc#vacc{resp=Resp}};

view_cb(Msg, #vacc{resp=undefined}=Acc) ->
    %% Start response
    Headers = [],
    {ok, Resp} = chttpd:start_delayed_json_response(Acc#vacc.req, 200, Headers),
    view_cb(Msg, Acc#vacc{resp=Resp, should_close=true});

%% ---------------------------------------------------

%% From here on down, the response has been started.

view_cb({error, Reason}, #vacc{resp=Resp}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp, Reason),
    {ok, Acc#vacc{resp=Resp1}};

view_cb(complete, #vacc{resp=Resp, buffer=Buf, threshold=Max}=Acc) ->
    % Finish view output and possibly end the response
    {ok, Resp1} = chttpd:close_delayed_json_object(Resp, Buf, "\r\n]}", Max),
    case Acc#vacc.should_close of
        true ->
            {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
            {ok, Acc#vacc{resp=Resp2}};
        _ ->
            {ok, Acc#vacc{resp=Resp1, meta_sent=false, row_sent=false,
                prepend=",\r\n", buffer=[], bufsize=0}}
    end;

view_cb({meta, Meta}, #vacc{meta_sent=false, row_sent=false}=Acc) ->
    % Sending metadata as we've not sent it or any row yet
    Parts = case couch_util:get_value(total, Meta) of
        undefined -> [];
        Total -> [io_lib:format("\"total_rows\":~p", [Total])]
    end ++ case couch_util:get_value(offset, Meta) of
        undefined -> [];
        Offset -> [io_lib:format("\"offset\":~p", [Offset])]
    end ++ case couch_util:get_value(update_seq, Meta) of
        undefined -> [];
        null ->
            ["\"update_seq\":null"];
        UpdateSeq when is_integer(UpdateSeq) ->
            [io_lib:format("\"update_seq\":~B", [UpdateSeq])];
        UpdateSeq when is_binary(UpdateSeq) ->
            [io_lib:format("\"update_seq\":\"~s\"", [UpdateSeq])]
    end ++ ["\"rows\":["],
    Chunk = [prepend_val(Acc), "{", string:join(Parts, ","), "\r\n"],
    {ok, AccOut} = maybe_flush_response(Acc, Chunk, iolist_size(Chunk)),
    {ok, AccOut#vacc{prepend="", meta_sent=true}};

view_cb({meta, _Meta}, #vacc{}=Acc) ->
    %% ignore metadata
    {ok, Acc};

view_cb({row, Row}, #vacc{meta_sent=false}=Acc) ->
    %% sorted=false and row arrived before meta
    % Adding another row
    Chunk = [prepend_val(Acc), "{\"rows\":[\r\n", row_to_json(Row)],
    maybe_flush_response(Acc#vacc{meta_sent=true, row_sent=true}, Chunk, iolist_size(Chunk));

view_cb({row, Row}, #vacc{meta_sent=true}=Acc) ->
    % Adding another row
    Chunk = [prepend_val(Acc), row_to_json(Row)],
    maybe_flush_response(Acc#vacc{row_sent=true}, Chunk, iolist_size(Chunk)).


maybe_flush_response(#vacc{bufsize=Size, threshold=Max} = Acc, Data, Len)
        when Size > 0 andalso (Size + Len) > Max ->
    #vacc{buffer = Buffer, resp = Resp} = Acc,
    {ok, R1} = chttpd:send_delayed_chunk(Resp, Buffer),
    {ok, Acc#vacc{prepend = ",\r\n", buffer = Data, bufsize = Len, resp = R1}};
maybe_flush_response(Acc0, Data, Len) ->
    #vacc{buffer = Buf, bufsize = Size} = Acc0,
    Acc = Acc0#vacc{
        prepend = ",\r\n",
        buffer = [Buf | Data],
        bufsize = Size + Len
    },
    {ok, Acc}.

prepend_val(#vacc{prepend=Prepend}) ->
    case Prepend of
        undefined ->
            "";
        _ ->
            Prepend
    end.


row_to_json(Row) ->
    Id = couch_util:get_value(id, Row),
    row_to_json(Id, Row).


row_to_json(error, Row) ->
    % Special case for _all_docs request with KEYS to
    % match prior behavior.
    Key = couch_util:get_value(key, Row),
    Val = couch_util:get_value(value, Row),
    Reason = couch_util:get_value(reason, Row),
    ReasonProp = if Reason == undefined -> []; true ->
        [{reason, Reason}]
    end,
    Obj = {[{key, Key}, {error, Val}] ++ ReasonProp},
    ?JSON_ENCODE(Obj);
row_to_json(Id0, Row) ->
    Id = case Id0 of
        undefined -> [];
        Id0 -> [{id, Id0}]
    end,
    Key = couch_util:get_value(key, Row, null),
    Val = couch_util:get_value(value, Row),
    Doc = case couch_util:get_value(doc, Row) of
        undefined -> [];
        Doc0 -> [{doc, Doc0}]
    end,
    Obj = {Id ++ [{key, Key}, {value, Val}] ++ Doc},
    ?JSON_ENCODE(Obj).


parse_params(#httpd{}=Req, Keys) ->
    parse_params(chttpd:qs(Req), Keys);
parse_params(Props, Keys) ->
    Args = #mrargs{},
    parse_params(Props, Keys, Args).


parse_params(Props, Keys, Args) ->
    parse_params(Props, Keys, Args, []).

parse_params(Props, Keys, #mrargs{}=Args0, Options) ->
    IsDecoded = lists:member(decoded, Options),
    % group_level set to undefined to detect if explicitly set by user
    Args1 = Args0#mrargs{keys=Keys, group=undefined, group_level=undefined},
    lists:foldl(fun({K, V}, Acc) ->
        parse_param(K, V, Acc, IsDecoded)
    end, Args1, Props).


parse_param(Key, Val, Args, IsDecoded) when is_binary(Key) ->
    parse_param(binary_to_list(Key), Val, Args, IsDecoded);
parse_param(Key, Val, Args, IsDecoded) ->
    case Key of
        "" ->
            Args;
        "reduce" ->
            Args#mrargs{reduce=parse_boolean(Val)};
        "key" when IsDecoded ->
            Args#mrargs{start_key=Val, end_key=Val};
        "key" ->
            JsonKey = ?JSON_DECODE(Val),
            Args#mrargs{start_key=JsonKey, end_key=JsonKey};
        "keys" when IsDecoded ->
            Args#mrargs{keys=Val};
        "keys" ->
            Args#mrargs{keys=?JSON_DECODE(Val)};
        "startkey" when IsDecoded ->
            Args#mrargs{start_key=Val};
        "start_key" when IsDecoded ->
            Args#mrargs{start_key=Val};
        "startkey" ->
            Args#mrargs{start_key=?JSON_DECODE(Val)};
        "start_key" ->
            Args#mrargs{start_key=?JSON_DECODE(Val)};
        "startkey_docid" ->
            Args#mrargs{start_key_docid=couch_util:to_binary(Val)};
        "start_key_doc_id" ->
            Args#mrargs{start_key_docid=couch_util:to_binary(Val)};
        "endkey" when IsDecoded ->
            Args#mrargs{end_key=Val};
        "end_key" when IsDecoded ->
            Args#mrargs{end_key=Val};
        "endkey" ->
            Args#mrargs{end_key=?JSON_DECODE(Val)};
        "end_key" ->
            Args#mrargs{end_key=?JSON_DECODE(Val)};
        "endkey_docid" ->
            Args#mrargs{end_key_docid=couch_util:to_binary(Val)};
        "end_key_doc_id" ->
            Args#mrargs{end_key_docid=couch_util:to_binary(Val)};
        "limit" ->
            Args#mrargs{limit=parse_pos_int(Val)};
        "stale" when Val == "ok" orelse Val == <<"ok">> ->
            Args#mrargs{stable=true, update=false};
        "stale" when Val == "update_after" orelse Val == <<"update_after">> ->
            Args#mrargs{stable=true, update=lazy};
        "stale" ->
            throw({query_parse_error, <<"Invalid value for `stale`.">>});
        "stable" when Val == "true" orelse Val == <<"true">> ->
            Args#mrargs{stable=true};
        "stable" when Val == "false" orelse Val == <<"false">> ->
            Args#mrargs{stable=false};
        "stable" ->
            throw({query_parse_error, <<"Invalid value for `stable`.">>});
        "update" when Val == "true" orelse Val == <<"true">> ->
            Args#mrargs{update=true};
        "update" when Val == "false" orelse Val == <<"false">> ->
            Args#mrargs{update=false};
        "update" when Val == "lazy" orelse Val == <<"lazy">> ->
            Args#mrargs{update=lazy};
        "update" ->
            throw({query_parse_error, <<"Invalid value for `update`.">>});
        "descending" ->
            case parse_boolean(Val) of
                true -> Args#mrargs{direction=rev};
                _ -> Args#mrargs{direction=fwd}
            end;
        "skip" ->
            Args#mrargs{skip=parse_pos_int(Val)};
        "group" ->
            Args#mrargs{group=parse_boolean(Val)};
        "group_level" ->
            Args#mrargs{group_level=parse_pos_int(Val)};
        "inclusive_end" ->
            Args#mrargs{inclusive_end=parse_boolean(Val)};
        "include_docs" ->
            Args#mrargs{include_docs=parse_boolean(Val)};
        "attachments" ->
            case parse_boolean(Val) of
            true ->
                Opts = Args#mrargs.doc_options,
                Args#mrargs{doc_options=[attachments|Opts]};
            false ->
                Args
            end;
        "att_encoding_info" ->
            case parse_boolean(Val) of
            true ->
                Opts = Args#mrargs.doc_options,
                Args#mrargs{doc_options=[att_encoding_info|Opts]};
            false ->
                Args
            end;
        "update_seq" ->
            Args#mrargs{update_seq=parse_boolean(Val)};
        "conflicts" ->
            Args#mrargs{conflicts=parse_boolean(Val)};
        "callback" ->
            Args#mrargs{callback=couch_util:to_binary(Val)};
        "sorted" ->
            Args#mrargs{sorted=parse_boolean(Val)};
        _ ->
            BKey = couch_util:to_binary(Key),
            BVal = couch_util:to_binary(Val),
            Args#mrargs{extra=[{BKey, BVal} | Args#mrargs.extra]}
    end.


parse_boolean(true) ->
    true;
parse_boolean(false) ->
    false;

parse_boolean(Val) when is_binary(Val) ->
    parse_boolean(?b2l(Val));

parse_boolean(Val) ->
    case string:to_lower(Val) of
    "true" -> true;
    "false" -> false;
    _ ->
        Msg = io_lib:format("Invalid boolean parameter: ~p", [Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.

parse_int(Val) when is_integer(Val) ->
    Val;
parse_int(Val) ->
    case (catch list_to_integer(Val)) of
    IntVal when is_integer(IntVal) ->
        IntVal;
    _ ->
        Msg = io_lib:format("Invalid value for integer: ~p", [Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.

parse_pos_int(Val) ->
    case parse_int(Val) of
    IntVal when IntVal >= 0 ->
        IntVal;
    _ ->
        Fmt = "Invalid value for positive integer: ~p",
        Msg = io_lib:format(Fmt, [Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.


check_view_etag(Sig, Acc0, Req) ->
    ETag = chttpd:make_etag(Sig),
    case chttpd:etag_match(Req, ETag) of
        true -> throw({etag_match, ETag});
        false -> {ok, Acc0#vacc{etag=ETag}}
    end.
