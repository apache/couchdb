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

-module(chttpd_db).
-include("chttpd.hrl").

-export([handle_request/1, handle_compact_req/2, handle_design_req/2,
    db_req/2, couch_doc_open/4,handle_changes_req/2,
    update_doc_result_to_json/1, update_doc_result_to_json/2,
    handle_design_info_req/2, handle_view_cleanup_req/2]).

-import(chttpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1,
    start_chunked_response/3, absolute_uri/2, send/2,
    start_response_length/4]).

-record(doc_query_args, {
    options = [],
    rev = nil,
    open_revs = [],
    show = nil
}).

% Database request handlers
handle_request(#httpd{path_parts=[DbName|RestParts],method=Method,
        db_url_handlers=DbUrlHandlers}=Req)->
    case {Method, RestParts} of
    {'PUT', []} ->
        create_db_req(Req, DbName);
    {'DELETE', []} ->
        delete_db_req(Req, DbName);
    {_, []} ->
        do_db_req(Req, fun db_req/2);
    {_, [SecondPart|_]} ->
        Handler = couch_util:get_value(SecondPart, DbUrlHandlers, fun db_req/2),
        do_db_req(Req, Handler)
    end.

handle_changes_req(#httpd{method='GET'}=Req, Db) ->
    MakeCallback = fun(Resp) ->
        fun({change, Change, _}, "continuous") ->
            send_chunk(Resp, [?JSON_ENCODE(Change) | "\n"]);
        ({change, Change, Prepend}, _) ->
            send_chunk(Resp, [Prepend, ?JSON_ENCODE(Change)]);
        (start, "continuous") ->
            ok;
        (start, _) ->
            send_chunk(Resp, "{\"results\":[\n");
        ({stop, EndSeq}, "continuous") ->
            send_chunk(
                Resp,
                [?JSON_ENCODE({[{<<"last_seq">>, EndSeq}]}) | "\n"]
            ),
            end_json_response(Resp);
        ({stop, EndSeq}, _) ->
            send_chunk(
                Resp,
                io_lib:format("\n],\n\"last_seq\":~w}\n", [EndSeq])
            ),
            end_json_response(Resp);
        (timeout, _) ->
            send_chunk(Resp, "\n")
        end
    end,
    ChangesArgs = parse_changes_query(Req),
    ChangesFun = couch_changes:handle_changes(ChangesArgs, Req, Db),
    case ChangesArgs#changes_args.feed of
    "normal" ->
        {ok, Info} = couch_db:get_db_info(Db),
        CurrentEtag = chttpd:make_etag(Info),
        chttpd:etag_respond(
            Req,
            CurrentEtag,
            fun() ->
                {ok, Resp} = chttpd:start_json_response(
                     Req, 200, [{"Etag", CurrentEtag}]
                ),
                ChangesFun(MakeCallback(Resp))
            end
        );
    _ ->
        % "longpoll" or "continuous"
        {ok, Resp} = chttpd:start_json_response(Req, 200),
        ChangesFun(MakeCallback(Resp))
    end;

handle_changes_req(#httpd{path_parts=[_,<<"_changes">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_compact_req(#httpd{method='POST',path_parts=[DbName,_,Id|_]}=Req, _Db) ->
    ok = ?COUCH:compact_view_group(DbName, Id),
    send_json(Req, 202, {[{ok, true}]});

handle_compact_req(Req, _) ->
    Msg = <<"Compaction is handled automatically by Cloudant">>,
    chttpd:send_error(Req, 403, Msg).

handle_view_cleanup_req(Req, _) ->
    Msg = <<"Old view indices are purged automatically by Cloudant">>,
    chttpd:send_error(Req, 403, Msg).

handle_design_req(#httpd{
        path_parts=[_DbName,_Design,_DesName, <<"_",_/binary>> = Action | _Rest],
        design_url_handlers = DesignUrlHandlers
    }=Req, Db) ->
    Handler = couch_util:get_value(Action, DesignUrlHandlers, fun db_req/2),
    Handler(Req, Db);

handle_design_req(Req, Db) ->
    db_req(Req, Db).

handle_design_info_req(#httpd{method='GET', path_parts=[_,_,Name,_]}=Req, Db) ->
    {ok, GroupInfoList} = fabric:get_view_group_info(Db, Name),
    send_json(Req, 200, {[
        {name,  <<"_design/", Name/binary>>},
        {view_index, {GroupInfoList}}
    ]});

handle_design_info_req(Req, _Db) ->
    send_method_not_allowed(Req, "GET").

create_db_req(#httpd{user_ctx=UserCtx}=Req, DbName) ->
    N = chttpd:qs_value(Req, "n"),
    Q = chttpd:qs_value(Req, "q"),
    case fabric:create_db(DbName, [{user_ctx, UserCtx},{n,N},{q,Q}]) of
    ok ->
        DocUrl = absolute_uri(Req, "/" ++ couch_util:url_encode(DbName)),
        send_json(Req, 201, [{"Location", DocUrl}], {[{ok, true}]});
    Error ->
        throw(Error)
    end.

delete_db_req(#httpd{user_ctx=UserCtx}=Req, DbName) ->
    case fabric:delete_db(DbName, [{user_ctx, UserCtx}]) of
    ok ->
        send_json(Req, 200, {[{ok, true}]});
    Error ->
        throw(Error)
    end.

do_db_req(#httpd{path_parts=[DbName|_]}=Req, Fun) ->
    Fun(Req, #db{name=DbName}).

db_req(#httpd{method='GET',path_parts=[DbName]}=Req, _Db) ->
    {ok, DbInfo} = fabric:get_db_info(DbName),
    send_json(Req, {cloudant_util:customer_db_info(Req, DbInfo)});

db_req(#httpd{method='POST',path_parts=[DbName], user_ctx=Ctx}=Req, Db) ->
    Doc = couch_doc:from_json_obj(chttpd:json_body(Req)),
    Doc2 = case Doc#doc.id of
        <<"">> ->
            Doc#doc{id=couch_util:new_uuid(), revs={0, []}};
        _ ->
            Doc
    end,
    DocId = Doc2#doc.id,
    case chttpd:qs_value(Req, "batch") of
    "ok" ->
        % batch
        ok = couch_batch_save:eventually_save_doc(
            Db#db.name, Doc2, Db#db.user_ctx),
        send_json(Req, 202, [], {[
            {ok, true},
            {id, DocId}
        ]});
    _Normal ->
        % normal
        {ok, NewRev} = fabric:update_doc(Db, Doc2, [{user_ctx,Ctx}]),
        DocUrl = absolute_uri(
            Req, binary_to_list(<<"/",DbName/binary,"/", DocId/binary>>)),
        send_json(Req, 201, [{"Location", DocUrl}], {[
            {ok, true},
            {id, DocId},
            {rev, couch_doc:rev_to_str(NewRev)}
        ]})
    end;


db_req(#httpd{path_parts=[_DbName]}=Req, _Db) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_ensure_full_commit">>]}=Req, Db) ->
    UpdateSeq = ?COUCH:get_update_seq(Db),
    CommittedSeq = ?COUCH:get_committed_update_seq(Db),
    {ok, StartTime} =
    case chttpd:qs_value(Req, "seq") of
    undefined ->
        committed = couch_batch_save:commit_now(Db#db.name, Db#db.user_ctx),
        ?COUCH:ensure_full_commit(Db);
    RequiredStr ->
        RequiredSeq = list_to_integer(RequiredStr),
        if RequiredSeq > UpdateSeq ->
            throw({bad_request,
                "can't do a full commit ahead of current update_seq"});
        RequiredSeq > CommittedSeq ->
            % user asked for an explicit sequence, don't commit any batches
            ?COUCH:ensure_full_commit(Db);
        true ->
            %% hack to make sure we always get cluster max time - APK
            ?COUCH:ensure_full_commit(Db)
            % {ok, Db#db.instance_start_time}
        end
    end,
    send_json(Req, 201, {[
        {ok, true},
        {instance_start_time, StartTime}
    ]});

db_req(#httpd{path_parts=[_,<<"_ensure_full_commit">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_bulk_docs">>], user_ctx=Ctx}=Req, Db) ->
    couch_stats_collector:increment({httpd, bulk_requests}),
    {JsonProps} = chttpd:json_body_obj(Req),
    DocsArray = couch_util:get_value(<<"docs">>, JsonProps),
    case chttpd:header_value(Req, "X-Couch-Full-Commit") of
    "true" ->
        Options = [full_commit, {user_ctx,Ctx}];
    "false" ->
        Options = [delay_commit, {user_ctx,Ctx}];
    _ ->
        Options = [{user_ctx,Ctx}]
    end,
    case couch_util:get_value(<<"new_edits">>, JsonProps, true) of
    true ->
        Docs = lists:map(
            fun({ObjProps} = JsonObj) ->
                Doc = couch_doc:from_json_obj(JsonObj),
                validate_attachment_names(Doc),
                Id = case Doc#doc.id of
                    <<>> -> couch_util:new_uuid();
                    Id0 -> Id0
                end,
                case couch_util:get_value(<<"_rev">>, ObjProps) of
                undefined ->
                    Revs = {0, []};
                Rev  ->
                    {Pos, RevId} = couch_doc:parse_rev(Rev),
                    Revs = {Pos, [RevId]}
                end,
                Doc#doc{id=Id,revs=Revs}
            end,
            DocsArray),
        Options2 =
        case couch_util:get_value(<<"all_or_nothing">>, JsonProps) of
        true  -> [all_or_nothing|Options];
        _ -> Options
        end,
        case fabric:update_docs(Db, Docs, [Options2]) of
        {ok, Results} ->
            % output the results
            DocResults = lists:zipwith(fun update_doc_result_to_json/2,
                Docs, Results),
            send_json(Req, 201, DocResults);
        {aborted, Errors} ->
            ErrorsJson =
                lists:map(fun update_doc_result_to_json/1, Errors),
            send_json(Req, 417, ErrorsJson)
        end;
    false ->
        Docs = [couch_doc:from_json_obj(JsonObj) || JsonObj <- DocsArray],
        {ok, Errors} = fabric:update_docs(Db, Docs, [replicated_changes|Options]),
        ErrorsJson =
            lists:map(fun update_doc_result_to_json/1, Errors),
        send_json(Req, 201, ErrorsJson)
    end;
db_req(#httpd{path_parts=[_,<<"_bulk_docs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_purge">>]}=Req, Db) ->
    {IdsRevs} = chttpd:json_body_obj(Req),
    IdsRevs2 = [{Id, couch_doc:parse_revs(Revs)} || {Id, Revs} <- IdsRevs],

    case ?COUCH:purge_docs(Db, IdsRevs2) of
    {ok, PurgeSeq, PurgedIdsRevs} ->
        PurgedIdsRevs2 = [{Id, couch_doc:rev_to_strs(Revs)} || {Id, Revs} <- PurgedIdsRevs],
        send_json(Req, 200, {[{<<"purge_seq">>, PurgeSeq}, {<<"purged">>, {PurgedIdsRevs2}}]});
    Error ->
        throw(Error)
    end;

db_req(#httpd{path_parts=[_,<<"_purge">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='GET',path_parts=[_,<<"_all_docs">>]}=Req, Db) ->
    all_docs_view(Req, Db, nil);

db_req(#httpd{method='POST',path_parts=[_,<<"_all_docs">>]}=Req, Db) ->
    {Fields} = chttpd:json_body_obj(Req),
    Keys = couch_util:get_value(<<"keys">>, Fields, nil),
    case Keys of
        Keys when is_list(Keys) -> ok;
        nil -> ?LOG_DEBUG("POST to _all_docs with no keys member.", []);
        _ -> throw({bad_request, "`keys` member must be a array."})
    end,
    all_docs_view(Req, Db, Keys);

db_req(#httpd{path_parts=[_,<<"_all_docs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD,POST");

db_req(#httpd{method='GET',path_parts=[_,<<"_all_docs_by_seq">>]}=Req, Db) ->
    throw(not_implemented),
    #view_query_args{
        start_key = StartKey,
        limit = Limit,
        skip = SkipCount,
        direction = Dir
    } = QueryArgs = chttpd_view:parse_view_params(Req, nil, map),

    {ok, Info} = fabric:get_db_info(Db),
    CurrentEtag = chttpd:make_etag(Info),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        TotalRowCount = couch_util:get_value(doc_count, Info),
        FoldlFun = chttpd_view:make_view_fold_fun(Req, QueryArgs, CurrentEtag, Db,
            TotalRowCount, #view_fold_helper_funs{
                reduce_count = fun ?COUCH:enum_docs_since_reduce_to_count/1
            }),
        StartKey2 = case StartKey of
            nil -> 0;
            <<>> -> 100000000000;
            {} -> 100000000000;
            StartKey when is_integer(StartKey) -> StartKey
        end,
        {ok, FoldResult} = ?COUCH:enum_docs_since(Db, StartKey2, Dir,
            fun(DocInfo, Offset, Acc) ->
                #doc_info{
                    id=Id,
                    high_seq=Seq,
                    revs=[#rev_info{rev=Rev,deleted=Deleted} | RestInfo]
                } = DocInfo,
                ConflictRevs = couch_doc:rev_to_strs(
                    [Rev1 || #rev_info{deleted=false, rev=Rev1} <- RestInfo]),
                DelConflictRevs = couch_doc:rev_to_strs(
                    [Rev1 || #rev_info{deleted=true, rev=Rev1} <- RestInfo]),
                Json = {
                    [{<<"rev">>, couch_doc:rev_to_str(Rev)}] ++
                    case ConflictRevs of
                    []  -> [];
                    _   -> [{<<"conflicts">>, ConflictRevs}]
                    end ++
                    case DelConflictRevs of
                    []  ->  [];
                    _   ->  [{<<"deleted_conflicts">>, DelConflictRevs}]
                    end ++
                    case Deleted of
                    true -> [{<<"deleted">>, true}];
                    false -> []
                    end
                },
                FoldlFun({{Seq, Id}, Json}, Offset, Acc)
            end, {Limit, SkipCount, undefined, [], nil}),
        chttpd_view:finish_view_fold(Req, TotalRowCount, {ok, FoldResult})
    end);

db_req(#httpd{path_parts=[_,<<"_all_docs_by_seq">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD");

db_req(#httpd{method='POST',path_parts=[_,<<"_missing_revs">>]}=Req, Db) ->
    {JsonDocIdRevs} = chttpd:json_body_obj(Req),
    JsonDocIdRevs2 = [{Id, [couch_doc:parse_rev(RevStr) || RevStr <- RevStrs]} || {Id, RevStrs} <- JsonDocIdRevs],
    {ok, Results} = fabric:get_missing_revs(Db, JsonDocIdRevs2),
    Results2 = [{Id, [couch_doc:rev_to_str(Rev) || Rev <- Revs]} || {Id, Revs} <- Results],
    send_json(Req, {[
        {missing_revs, {Results2}}
    ]});

db_req(#httpd{path_parts=[_,<<"_missing_revs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='PUT',path_parts=[_,<<"_admins">>]}=Req,
        Db) ->
    Admins = chttpd:json_body(Req),
    ok = ?COUCH:set_admins(Db, Admins),
    send_json(Req, {[{<<"ok">>, true}]});

db_req(#httpd{method='GET',path_parts=[_,<<"_admins">>]}=Req, Db) ->
    send_json(Req, ?COUCH:get_admins(Db));

db_req(#httpd{path_parts=[_,<<"_admins">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "PUT,GET");

db_req(#httpd{method='PUT',path_parts=[_,<<"_revs_limit">>]}=Req,
        Db) ->
    Limit = chttpd:json_body(Req),
    ok = ?COUCH:set_revs_limit(Db, Limit),
    send_json(Req, {[{<<"ok">>, true}]});

db_req(#httpd{method='GET',path_parts=[_,<<"_revs_limit">>]}=Req, Db) ->
    send_json(Req, ?COUCH:get_revs_limit(Db));

db_req(#httpd{path_parts=[_,<<"_revs_limit">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "PUT,GET");

% Special case to enable using an unencoded slash in the URL of design docs,
% as slashes in document IDs must otherwise be URL encoded.
db_req(#httpd{path_parts=[_DbName,<<"_design">>,Name]}=Req, Db) ->
    db_doc_req(Req, Db, <<"_design/",Name/binary>>);

db_req(#httpd{path_parts=[_DbName,<<"_design">>,Name|FileNameParts]}=Req, Db) ->
    db_attachment_req(Req, Db, <<"_design/",Name/binary>>, FileNameParts);


% Special case to allow for accessing local documents without %2F
% encoding the docid. Throws out requests that don't have the second
% path part or that specify an attachment name.
db_req(#httpd{path_parts=[_DbName, <<"_local">>, Name]}=Req, Db) ->
    db_doc_req(Req, Db, <<"_local/", Name/binary>>);

db_req(#httpd{path_parts=[_DbName, <<"_local">>]}, _Db) ->
    throw({bad_request, <<"Missing _local document id.">>});

db_req(#httpd{path_parts=[_DbName, <<"_local/">>]}, _Db) ->
    throw({bad_request, <<"Missing _local document id.">>});

db_req(#httpd{path_parts=[_DbName, <<"_local">> | _Rest]}, _Db) ->
    throw({bad_request, <<"_local documents do not accept attachments.">>});

db_req(#httpd{path_parts=[_DbName, <<"_local/", _/binary>>, _ | _]}, _Db) ->
    throw({bad_request, <<"_local documents do not accept attachments.">>});

db_req(#httpd{path_parts=[_, DocId]}=Req, Db) ->
    db_doc_req(Req, Db, DocId);

db_req(#httpd{path_parts=[_, DocId | FileNameParts]}=Req, Db) ->
    db_attachment_req(Req, Db, DocId, FileNameParts).

all_docs_view(Req, Db, Keys) ->
    Etag = couch_util:new_uuid(),
    QueryArgs = chttpd_view:parse_view_params(Req, nil, map),
    chttpd:etag_respond(Req, Etag, fun() ->
        {ok, Resp} = chttpd:start_json_response(Req, 200, [{"Etag",Etag}]),
        {ok, Total, Result} = ?COUCH:all_docs_view(Resp, Db, Keys, QueryArgs),
        send_chunk(Resp, all_docs_final_chunk(Total, Result)),
        end_json_response(Resp)
    end).

all_docs_final_chunk(Total, {_, _, undefined, _, nil}) ->
    ?JSON_ENCODE({[{total_rows, Total}, {offset, Total}, {rows, []}]});
all_docs_final_chunk(Total, {_, _, undefined, _, Offset}) ->
    ?JSON_ENCODE({[{total_rows, Total}, {offset, Offset}, {rows, []}]});
all_docs_final_chunk(_, {_, _, _, _, _}) ->
    "\r\n]}";
all_docs_final_chunk(_, Error) ->
    throw(Error).

db_doc_req(#httpd{method='DELETE'}=Req, Db, DocId) ->
    % check for the existence of the doc to handle the 404 case.
    couch_doc_open(Db, DocId, nil, []),
    case chttpd:qs_value(Req, "rev") of
    undefined ->
        update_doc(Req, Db, DocId, {[{<<"_deleted">>,true}]});
    Rev ->
        update_doc(Req, Db, DocId, {[{<<"_rev">>, ?l2b(Rev)},{<<"_deleted">>,true}]})
    end;

db_doc_req(#httpd{method='GET'}=Req, Db, DocId) ->
    #doc_query_args{
        show = Format,
        rev = Rev,
        open_revs = Revs,
        options = Options
    } = parse_doc_query(Req),
    case Format of
    nil ->
        case Revs of
        [] ->
            Doc = couch_doc_open(Db, DocId, Rev, Options),
            DiskEtag = chttpd:doc_etag(Doc),
            case Doc#doc.meta of
            [] ->
                % output etag only when we have no meta
                chttpd:etag_respond(Req, DiskEtag, fun() ->
                    send_json(Req, 200, [{"Etag", DiskEtag}], couch_doc:to_json_obj(Doc, Options))
                end);
            _ ->
                send_json(Req, 200, [], couch_doc:to_json_obj(Doc, Options))
            end;
        _ ->
            {ok, Results} = fabric:open_revs(Db, DocId, Revs, Options),
            {ok, Resp} = start_json_response(Req, 200),
            send_chunk(Resp, "["),
            % We loop through the docs. The first time through the separator
            % is whitespace, then a comma on subsequent iterations.
            lists:foldl(
                fun(Result, AccSeparator) ->
                    case Result of
                    {ok, Doc} ->
                        JsonDoc = couch_doc:to_json_obj(Doc, Options),
                        Json = ?JSON_ENCODE({[{ok, JsonDoc}]}),
                        send_chunk(Resp, AccSeparator ++ Json);
                    {{not_found, missing}, RevId} ->
                        Json = ?JSON_ENCODE({[{"missing", RevId}]}),
                        send_chunk(Resp, AccSeparator ++ Json)
                    end,
                    "," % AccSeparator now has a comma
                end,
                "", Results),
            send_chunk(Resp, "]"),
            end_json_response(Resp)
        end;
    _ ->
        {DesignName, ShowName} = Format,
        chttpd_show:handle_doc_show(Req, DesignName, ShowName, DocId, Db)
    end;

db_doc_req(#httpd{method='POST', user_ctx=Ctx}=Req, Db, DocId) ->
    couch_doc:validate_docid(DocId),
    case chttpd:header_value(Req, "content-type") of
    "multipart/form-data" ++  _Rest ->
        ok;
    _Else ->
        throw({bad_ctype, <<"Invalid Content-Type header for form upload">>})
    end,
    Form = chttpd:parse_form(Req),
    Rev = couch_doc:parse_rev(list_to_binary(couch_util:get_value("_rev", Form))),
    {ok, [{ok, Doc}]} = fabric:open_revs(Db, DocId, [Rev], []),

    UpdatedAtts = [
        #att{name=validate_attachment_name(Name),
            type=list_to_binary(ContentType),
            data=Content} ||
        {Name, {ContentType, _}, Content} <-
        proplists:get_all_values("_attachments", Form)
    ],
    #doc{atts=OldAtts} = Doc,
    OldAtts2 = lists:flatmap(
        fun(#att{name=OldName}=Att) ->
            case [1 || A <- UpdatedAtts, A#att.name == OldName] of
            [] -> [Att]; % the attachment wasn't in the UpdatedAtts, return it
            _ -> [] % the attachment was in the UpdatedAtts, drop it
            end
        end, OldAtts),
    NewDoc = Doc#doc{
        atts = UpdatedAtts ++ OldAtts2
    },
    {ok, NewRev} = fabric:update_doc(Db, NewDoc, [{user_ctx,Ctx}]),

    send_json(Req, 201, [{"Etag", "\"" ++ ?b2l(couch_doc:rev_to_str(NewRev)) ++ "\""}], {[
        {ok, true},
        {id, DocId},
        {rev, couch_doc:rev_to_str(NewRev)}
    ]});

db_doc_req(#httpd{method='PUT'}=Req, Db, DocId) ->
    couch_doc:validate_docid(DocId),
    Json = chttpd:json_body(Req),
    case chttpd:qs_value(Req, "batch") of
    "ok" ->
        % batch
        Doc = couch_doc_from_req(Req, DocId, Json),
        ok = couch_batch_save:eventually_save_doc(Db#db.name, Doc, Db#db.user_ctx),
        send_json(Req, 202, [], {[
            {ok, true},
            {id, DocId}
        ]});
    _Normal ->
        % normal
        DbName = couch_db:name(Db),
        Location = absolute_uri(Req, <<"/", DbName/binary, "/", DocId/binary>>),
        update_doc(Req, Db, DocId, Json, [{"Location", Location}])
    end;

db_doc_req(#httpd{method='COPY', user_ctx=Ctx}=Req, Db, SourceDocId) ->
    SourceRev =
    case extract_header_rev(Req, chttpd:qs_value(Req, "rev")) of
        missing_rev -> nil;
        Rev -> Rev
    end,
    {TargetDocId, TargetRevs} = parse_copy_destination_header(Req),
    % open old doc
    Doc = couch_doc_open(Db, SourceDocId, SourceRev, []),
    % save new doc
    {ok, NewTargetRev} = fabric:update_doc(Db,
        Doc#doc{id=TargetDocId, revs=TargetRevs}, [{user_ctx,Ctx}]),
    % respond
    send_json(Req, 201,
        [{"Etag", "\"" ++ ?b2l(couch_doc:rev_to_str(NewTargetRev)) ++ "\""}],
        update_doc_result_to_json(TargetDocId, {ok, NewTargetRev}));

db_doc_req(Req, _Db, _DocId) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST,PUT,COPY").


update_doc_result_to_json({{Id, Rev}, Error}) ->
        {_Code, Err, Msg} = chttpd:error_info(Error),
        {[{id, Id}, {rev, couch_doc:rev_to_str(Rev)},
            {error, Err}, {reason, Msg}]}.

update_doc_result_to_json(#doc{id=DocId}, Result) ->
    update_doc_result_to_json(DocId, Result);
update_doc_result_to_json(DocId, {ok, NewRev}) ->
    {[{id, DocId}, {rev, couch_doc:rev_to_str(NewRev)}]};
update_doc_result_to_json(DocId, Error) ->
    {_Code, ErrorStr, Reason} = chttpd:error_info(Error),
    {[{id, DocId}, {error, ErrorStr}, {reason, Reason}]}.


update_doc(Req, Db, DocId, Json) ->
    update_doc(Req, Db, DocId, Json, []).

update_doc(#httpd{user_ctx=Ctx} = Req, Db, DocId, Json, Headers) ->
    #doc{deleted=Deleted} = Doc = couch_doc_from_req(Req, DocId, Json),

    case chttpd:header_value(Req, "X-Couch-Full-Commit") of
    "true" ->
        Options = [full_commit, {user_ctx,Ctx}];
    "false" ->
        Options = [delay_commit, {user_ctx,Ctx}];
    _ ->
        Options = [{user_ctx,Ctx}]
    end,
    {Status, NewRev} = case fabric:update_doc(Db, Doc, Options) of
    {ok, NewRev1} -> {201, NewRev1};
    {accepted, NewRev1} -> {202, NewRev1}
    end,
    NewRevStr = couch_doc:rev_to_str(NewRev),
    ResponseHeaders = [{"Etag", <<"\"", NewRevStr/binary, "\"">>}] ++ Headers,
    send_json(Req, if Deleted -> 200; true -> Status end,
        ResponseHeaders, {[
            {ok, true},
            {id, DocId},
            {rev, NewRevStr}]}).

couch_doc_from_req(Req, DocId, Json) ->
    Doc = couch_doc:from_json_obj(Json),
    validate_attachment_names(Doc),
    ExplicitDocRev =
    case Doc#doc.revs of
        {Start,[RevId|_]} -> {Start, RevId};
        _ -> undefined
    end,
    case extract_header_rev(Req, ExplicitDocRev) of
    missing_rev ->
        Revs = {0, []};
    {Pos, Rev} ->
        Revs = {Pos, [Rev]}
    end,
    Doc#doc{id=DocId, revs=Revs}.


% Useful for debugging
% couch_doc_open(Db, DocId) ->
%   couch_doc_open(Db, DocId, nil, []).

couch_doc_open(Db, DocId, Rev, Options) ->
    case Rev of
    nil -> % open most recent rev
        case fabric:open_doc(Db, DocId, Options) of
        {ok, Doc} ->
            Doc;
         Error ->
             throw(Error)
         end;
  _ -> % open a specific rev (deletions come back as stubs)
      case fabric:open_revs(Db, DocId, [Rev], Options) of
          {ok, [{ok, Doc}]} ->
              Doc;
          {ok, [Else]} ->
              throw(Else)
      end
  end.

% Attachment request handlers

db_attachment_req(#httpd{method='GET'}=Req, Db, DocId, FileNameParts) ->
    FileName = list_to_binary(mochiweb_util:join(lists:map(fun binary_to_list/1, FileNameParts),"/")),
    #doc_query_args{
        rev=Rev,
        options=Options
    } = parse_doc_query(Req),
    #doc{
        atts=Atts
    } = Doc = couch_doc_open(Db, DocId, Rev, Options),
    case [A || A <- Atts, A#att.name == FileName] of
    [] ->
        throw({not_found, "Document is missing attachment"});
    [#att{type=Type, len=Len}=Att] ->
        Etag = chttpd:doc_etag(Doc),
        chttpd:etag_respond(Req, Etag, fun() ->
            {ok, Resp} = start_response_length(Req, 200, [
                {"ETag", Etag},
                {"Cache-Control", "must-revalidate"},
                {"Content-Type", binary_to_list(Type)}
                ], integer_to_list(Len)),
            couch_doc:att_foldl(Att, fun(BinSegment, _) ->
                send(Resp, BinSegment)
            end, {ok, Resp})
        end)
    end;


db_attachment_req(#httpd{method=Method, user_ctx=Ctx}=Req, Db, DocId, FileNameParts)
        when (Method == 'PUT') or (Method == 'DELETE') ->
    FileName = validate_attachment_name(
                    mochiweb_util:join(
                        lists:map(fun binary_to_list/1,
                            FileNameParts),"/")),

    NewAtt = case Method of
        'DELETE' ->
            [];
        _ ->
            [#att{
                name=FileName,
                type = case chttpd:header_value(Req,"Content-Type") of
                    undefined ->
                        % We could throw an error here or guess by the FileName.
                        % Currently, just giving it a default.
                        <<"application/octet-stream">>;
                    CType ->
                        list_to_binary(CType)
                    end,
                data = ?COUCH:att_receiver(Req, chttpd:body_length(Req)),
                len = case chttpd:header_value(Req,"Content-Length") of
                    undefined ->
                        undefined;
                    Length ->
                        list_to_integer(Length)
                    end
                    }]
    end,

    Doc = case extract_header_rev(Req, chttpd:qs_value(Req, "rev")) of
        missing_rev -> % make the new doc
            couch_doc:validate_docid(DocId),
            #doc{id=DocId};
        Rev ->
            case fabric:open_revs(Db, DocId, [Rev], []) of
            {ok, [{ok, Doc0}]}  -> Doc0;
            {ok, [Error]}       -> throw(Error)
            end
    end,

    #doc{atts=Atts} = Doc,
    DocEdited = Doc#doc{
        atts = NewAtt ++ [A || A <- Atts, A#att.name /= FileName]
    },
    {ok, UpdatedRev} = fabric:update_doc(Db, DocEdited, [{user_ctx,Ctx}]),
    DbName = couch_db:name(Db),

    {Status, Headers} = case Method of
        'DELETE' ->
            {200, []};
        _ ->
            {201, [{"Location", absolute_uri(Req, "/" ++
                binary_to_list(DbName) ++ "/" ++
                binary_to_list(DocId) ++ "/" ++
                binary_to_list(FileName)
            )}]}
        end,
    send_json(Req,Status, Headers, {[
        {ok, true},
        {id, DocId},
        {rev, couch_doc:rev_to_str(UpdatedRev)}
    ]});

db_attachment_req(Req, _Db, _DocId, _FileNameParts) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,PUT").

parse_doc_format(FormatStr) when is_binary(FormatStr) ->
    parse_doc_format(?b2l(FormatStr));
parse_doc_format(FormatStr) when is_list(FormatStr) ->
    SplitFormat = lists:splitwith(fun($/) -> false; (_) -> true end, FormatStr),
    case SplitFormat of
        {DesignName, [$/ | ShowName]} -> {?l2b(DesignName), ?l2b(ShowName)};
        _Else -> throw({bad_request, <<"Invalid doc format">>})
    end;
parse_doc_format(_BadFormatStr) ->
    throw({bad_request, <<"Invalid doc format">>}).

parse_doc_query(Req) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"attachments", "true"} ->
            Options = [attachments | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"meta", "true"} ->
            Options = [revs_info, conflicts, deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"revs", "true"} ->
            Options = [revs | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"local_seq", "true"} ->
            Options = [local_seq | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"revs_info", "true"} ->
            Options = [revs_info | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"conflicts", "true"} ->
            Options = [conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"deleted_conflicts", "true"} ->
            Options = [deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"rev", Rev} ->
            Args#doc_query_args{rev=couch_doc:parse_rev(Rev)};
        {"open_revs", "all"} ->
            Args#doc_query_args{open_revs=all};
        {"open_revs", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{open_revs=[couch_doc:parse_rev(Rev) || Rev <- JsonArray]};
        {"show", FormatStr} ->
            Args#doc_query_args{show=parse_doc_format(FormatStr)};
        {"r", R} ->
            Options = [{r,R} | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"w", W} ->
            Options = [{w,W} | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #doc_query_args{}, chttpd:qs(Req)).

parse_changes_query(Req) ->
    lists:foldl(fun({Key, Value}, Args) ->
        case {Key, Value} of
        {"feed", _} ->
            Args#changes_args{feed=Value};
        {"descending", "true"} ->
            Args#changes_args{dir=rev};
        {"since", _} ->
            Args#changes_args{since=list_to_integer(Value)};
        {"limit", _} ->
            Args#changes_args{limit=list_to_integer(Value)};
        {"style", _} ->
            Args#changes_args{style=list_to_existing_atom(Value)};
        {"heartbeat", "true"} ->
            Args#changes_args{heartbeat=true};
        {"heartbeat", _} ->
            Args#changes_args{heartbeat=list_to_integer(Value)};
        {"timeout", _} ->
            Args#changes_args{timeout=list_to_integer(Value)};
        {"include_docs", "true"} ->
            Args#changes_args{include_docs=true};
        {"filter", _} ->
            Args#changes_args{filter=Value};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #changes_args{}, chttpd:qs(Req)).

extract_header_rev(Req, ExplicitRev) when is_binary(ExplicitRev) or is_list(ExplicitRev)->
    extract_header_rev(Req, couch_doc:parse_rev(ExplicitRev));
extract_header_rev(Req, ExplicitRev) ->
    Etag = case chttpd:header_value(Req, "If-Match") of
        undefined -> undefined;
        Value -> couch_doc:parse_rev(string:strip(Value, both, $"))
    end,
    case {ExplicitRev, Etag} of
    {undefined, undefined} -> missing_rev;
    {_, undefined} -> ExplicitRev;
    {undefined, _} -> Etag;
    _ when ExplicitRev == Etag -> Etag;
    _ ->
        throw({bad_request, "Document rev and etag have different values"})
    end.


parse_copy_destination_header(Req) ->
    Destination = chttpd:header_value(Req, "Destination"),
    case re:run(Destination, "\\?", [{capture, none}]) of
    nomatch ->
        {list_to_binary(Destination), {0, []}};
    match ->
        [DocId, RevQs] = re:split(Destination, "\\?", [{return, list}]),
        [_RevQueryKey, Rev] = re:split(RevQs, "=", [{return, list}]),
        {Pos, RevId} = couch_doc:parse_rev(Rev),
        {list_to_binary(DocId), {Pos, [RevId]}}
    end.

validate_attachment_names(Doc) ->
    lists:foreach(fun(#att{name=Name}) ->
        validate_attachment_name(Name)
    end, Doc#doc.atts).

validate_attachment_name(Name) when is_list(Name) ->
    validate_attachment_name(list_to_binary(Name));
validate_attachment_name(<<"_",_/binary>>) ->
    throw({bad_request, <<"Attachment name can't start with '_'">>});
validate_attachment_name(Name) ->
    case is_valid_utf8(Name) of
        true -> Name;
        false -> throw({bad_request, <<"Attachment name is not UTF-8 encoded">>})
    end.

%% borrowed from mochijson2:json_bin_is_safe()
is_valid_utf8(<<>>) ->
    true;
is_valid_utf8(<<C, Rest/binary>>) ->
    case C of
        $\" ->
            false;
        $\\ ->
            false;
        $\b ->
            false;
        $\f ->
            false;
        $\n ->
            false;
        $\r ->
            false;
        $\t ->
            false;
        C when C >= 0, C < $\s; C >= 16#7f, C =< 16#10FFFF ->
            false;
        C when C < 16#7f ->
            is_valid_utf8(Rest);
        _ ->
            false
    end.
