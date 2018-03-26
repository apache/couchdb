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

-module(couch_httpd_db).
-include_lib("couch/include/couch_db.hrl").

-export([handle_request/1, handle_compact_req/2, handle_design_req/2,
    db_req/2, couch_doc_open/4, handle_db_changes_req/2,
    update_doc_result_to_json/1, update_doc_result_to_json/2,
    handle_design_info_req/3, parse_copy_destination_header/1,
    parse_changes_query/2, handle_changes_req/4]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, absolute_uri/2, send/2,
    start_response_length/4, send_error/4]).

-record(doc_query_args, {
    options = [],
    rev = nil,
    open_revs = [],
    update_type = interactive_edit,
    atts_since = nil
}).

% Database request handlers
handle_request(#httpd{path_parts=[DbName|RestParts],method=Method,
        db_url_handlers=DbUrlHandlers}=Req)->
    case {Method, RestParts} of
    {'PUT', []} ->
        create_db_req(Req, DbName);
    {'DELETE', []} ->
        % if we get ?rev=... the user is using a faulty script where the
        % document id is empty by accident. Let them recover safely.
        case couch_httpd:qs_value(Req, "rev", false) of
            false -> delete_db_req(Req, DbName);
            _Rev -> throw({bad_request,
                "You tried to DELETE a database with a ?rev= parameter. "
                ++ "Did you mean to DELETE a document instead?"})
        end;
    {_, []} ->
        do_db_req(Req, fun db_req/2);
    {_, [SecondPart|_]} ->
        Handler = couch_util:dict_find(SecondPart, DbUrlHandlers, fun db_req/2),
        do_db_req(Req, Handler)
    end.


handle_db_changes_req(Req, Db) ->
    ChangesArgs = parse_changes_query(Req, Db),
    ChangesFun = couch_changes:handle_db_changes(ChangesArgs, Req, Db),
    handle_changes_req(Req, Db, ChangesArgs, ChangesFun).


handle_changes_req(#httpd{method='POST'}=Req, Db, ChangesArgs, ChangesFun) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    handle_changes_req1(Req, Db, ChangesArgs, ChangesFun);
handle_changes_req(#httpd{method='GET'}=Req, Db, ChangesArgs, ChangesFun) ->
    handle_changes_req1(Req, Db, ChangesArgs, ChangesFun);
handle_changes_req(#httpd{}=Req, _Db, _ChangesArgs, _ChangesFun) ->
    couch_httpd:send_method_not_allowed(Req, "GET,HEAD,POST").

handle_changes_req1(Req, Db, ChangesArgs, ChangesFun) ->
    DbName = couch_db:name(Db),
    AuthDbName = ?l2b(config:get("couch_httpd_auth", "authentication_db")),
    case AuthDbName of
    DbName ->
        % in the authentication database, _changes is admin-only.
        ok = couch_db:check_is_admin(Db);
    _Else ->
        % on other databases, _changes is free for all.
        ok
    end,

    MakeCallback = fun(Resp) ->
        fun({change, {ChangeProp}=Change, _}, "eventsource") ->
            Seq = proplists:get_value(<<"seq">>, ChangeProp),
            couch_httpd:send_chunk(Resp, ["data: ", ?JSON_ENCODE(Change),
                              "\n", "id: ", ?JSON_ENCODE(Seq),
                              "\n\n"]);
        ({change, Change, _}, "continuous") ->
            couch_httpd:send_chunk(Resp, [?JSON_ENCODE(Change) | "\n"]);
        ({change, Change, Prepend}, _) ->
            couch_httpd:send_chunk(Resp, [Prepend, ?JSON_ENCODE(Change)]);
        (start, "eventsource") ->
            ok;
        (start, "continuous") ->
            ok;
        (start, _) ->
            couch_httpd:send_chunk(Resp, "{\"results\":[\n");
        ({stop, _EndSeq}, "eventsource") ->
            couch_httpd:end_json_response(Resp);
        ({stop, EndSeq}, "continuous") ->
            couch_httpd:send_chunk(
                Resp,
                [?JSON_ENCODE({[{<<"last_seq">>, EndSeq}]}) | "\n"]
            ),
            couch_httpd:end_json_response(Resp);
        ({stop, EndSeq}, _) ->
            couch_httpd:send_chunk(
                Resp,
                io_lib:format("\n],\n\"last_seq\":~w}\n", [EndSeq])
            ),
            couch_httpd:end_json_response(Resp);
        (timeout, "eventsource") ->
            couch_httpd:send_chunk(Resp, "event: heartbeat\ndata: \n\n");
        (timeout, _) ->
            couch_httpd:send_chunk(Resp, "\n")
        end
    end,
    WrapperFun = case ChangesArgs#changes_args.feed of
    "normal" ->
        {ok, Info} = couch_db:get_db_info(Db),
        CurrentEtag = couch_httpd:make_etag(Info),
        fun(FeedChangesFun) ->
            couch_httpd:etag_respond(
                Req,
                CurrentEtag,
                fun() ->
                    {ok, Resp} = couch_httpd:start_json_response(
                         Req, 200, [{"ETag", CurrentEtag}]
                    ),
                    FeedChangesFun(MakeCallback(Resp))
                end
            )
        end;
    "eventsource" ->
        Headers = [
            {"Content-Type", "text/event-stream"},
            {"Cache-Control", "no-cache"}
        ],
        {ok, Resp} = couch_httpd:start_chunked_response(Req, 200, Headers),
        fun(FeedChangesFun) ->
            FeedChangesFun(MakeCallback(Resp))
        end;
    _ ->
        % "longpoll" or "continuous"
        {ok, Resp} = couch_httpd:start_json_response(Req, 200),
        fun(FeedChangesFun) ->
            FeedChangesFun(MakeCallback(Resp))
        end
    end,
    couch_stats:increment_counter(
        [couchdb, httpd, clients_requesting_changes]),
    try
        WrapperFun(ChangesFun)
    after
        couch_stats:decrement_counter(
            [couchdb, httpd, clients_requesting_changes])
    end.



handle_compact_req(#httpd{method='POST'}=Req, Db) ->
    case Req#httpd.path_parts of
        [_DbName, <<"_compact">>] ->
            ok = couch_db:check_is_admin(Db),
            couch_httpd:validate_ctype(Req, "application/json"),
            _ = couch_httpd:body(Req),
            {ok, _} = couch_db:start_compact(Db),
            send_json(Req, 202, {[{ok, true}]});
        [_DbName, <<"_compact">>, DesignName | _] ->
            DesignId = <<"_design/", DesignName/binary>>,
            DDoc = couch_httpd_db:couch_doc_open(
                Db, DesignId, nil, [ejson_body]
            ),
            couch_mrview_http:handle_compact_req(Req, Db, DDoc)
    end;

handle_compact_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").


handle_design_req(#httpd{
        path_parts=[_DbName, _Design, DesignName, <<"_",_/binary>> = Action | _Rest],
        design_url_handlers = DesignUrlHandlers
    }=Req, Db) ->
    case couch_db:is_system_db(Db) of
    true ->
        case (catch couch_db:check_is_admin(Db)) of
        ok -> ok;
        _ ->
            throw({forbidden, <<"Only admins can access design document",
                " actions for system databases.">>})
        end;
    false -> ok
    end,

    % load ddoc
    DesignId = <<"_design/", DesignName/binary>>,
    DDoc = couch_httpd_db:couch_doc_open(Db, DesignId, nil, [ejson_body]),
    Handler = couch_util:dict_find(Action, DesignUrlHandlers, fun(_, _, _) ->
        throw({not_found, <<"missing handler: ", Action/binary>>})
    end),
    Handler(Req, Db, DDoc);

handle_design_req(Req, Db) ->
    db_req(Req, Db).

handle_design_info_req(#httpd{
            method='GET',
            path_parts=[_DbName, _Design, DesignName, _]
        }=Req, Db, _DDoc) ->
    DesignId = <<"_design/", DesignName/binary>>,
    DDoc = couch_httpd_db:couch_doc_open(Db, DesignId, nil, [ejson_body]),
    couch_mrview_http:handle_info_req(Req, Db, DDoc).

create_db_req(#httpd{user_ctx=UserCtx}=Req, DbName) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Engine = case couch_httpd:qs_value(Req, "engine") of
        EngineStr when is_list(EngineStr) ->
            [{engine, iolist_to_binary(EngineStr)}];
        _ ->
            []
    end,
    case couch_server:create(DbName, [{user_ctx, UserCtx}] ++ Engine) of
    {ok, Db} ->
        couch_db:close(Db),
        DbUrl = absolute_uri(Req, "/" ++ couch_util:url_encode(DbName)),
        send_json(Req, 201, [{"Location", DbUrl}], {[{ok, true}]});
    Error ->
        throw(Error)
    end.

delete_db_req(#httpd{user_ctx=UserCtx}=Req, DbName) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Options = case couch_httpd:qs_value(Req, "sync") of
        "true" -> [sync, {user_ctx, UserCtx}];
        _ -> [{user_ctx, UserCtx}]
    end,
    case couch_server:delete(DbName, Options) of
    ok ->
        send_json(Req, 200, {[{ok, true}]});
    Error ->
        throw(Error)
    end.

do_db_req(#httpd{user_ctx=UserCtx,path_parts=[DbName|_]}=Req, Fun) ->
    case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
    {ok, Db} ->
        try
            Fun(Req, Db)
        after
            catch couch_db:close(Db)
        end;
    Error ->
        throw(Error)
    end.

db_req(#httpd{method='GET',path_parts=[_DbName]}=Req, Db) ->
    {ok, DbInfo} = couch_db:get_db_info(Db),
    send_json(Req, {DbInfo});

db_req(#httpd{method='POST',path_parts=[_DbName]}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    DbName = couch_db:name(Db),
    Doc = couch_doc:from_json_obj_validate(couch_httpd:json_body(Req), DbName),
    validate_attachment_names(Doc),
    Doc2 = case Doc#doc.id of
        <<"">> ->
            Doc#doc{id=couch_uuids:new(), revs={0, []}};
        _ ->
            Doc
    end,
    DocId = Doc2#doc.id,
    update_doc(Req, Db, DocId, Doc2);

db_req(#httpd{path_parts=[_DbName]}=Req, _Db) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_ensure_full_commit">>]}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    _ = couch_httpd:body(Req),
    UpdateSeq = couch_db:get_update_seq(Db),
    CommittedSeq = couch_db:get_committed_update_seq(Db),
    {ok, StartTime} =
    case couch_httpd:qs_value(Req, "seq") of
    undefined ->
        couch_db:ensure_full_commit(Db);
    RequiredStr ->
        RequiredSeq = list_to_integer(RequiredStr),
        if RequiredSeq > UpdateSeq ->
            throw({bad_request,
                "can't do a full commit ahead of current update_seq"});
        RequiredSeq > CommittedSeq ->
            couch_db:ensure_full_commit(Db);
        true ->
            {ok, couch_db:get_instance_start_time(Db)}
        end
    end,
    send_json(Req, 201, {[
        {ok, true},
        {instance_start_time, StartTime}
    ]});

db_req(#httpd{path_parts=[_,<<"_ensure_full_commit">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_bulk_docs">>]}=Req, Db) ->
    couch_stats:increment_counter([couchdb, httpd, bulk_requests]),
    couch_httpd:validate_ctype(Req, "application/json"),
    {JsonProps} = couch_httpd:json_body_obj(Req),
    DbName = couch_db:name(Db),
    case couch_util:get_value(<<"docs">>, JsonProps) of
    undefined ->
        send_error(Req, 400, <<"bad_request">>, <<"Missing JSON list of 'docs'">>);
    DocsArray ->
        couch_stats:update_histogram([couchdb, httpd, bulk_docs], length(DocsArray)),
        case couch_httpd:header_value(Req, "X-Couch-Full-Commit") of
        "true" ->
            Options = [full_commit];
        "false" ->
            Options = [delay_commit];
        _ ->
            Options = []
        end,
        case couch_util:get_value(<<"new_edits">>, JsonProps, true) of
        true ->
            Docs = lists:map(
                fun({ObjProps} = JsonObj) ->
                    Doc = couch_doc:from_json_obj_validate(JsonObj, DbName),
                    validate_attachment_names(Doc),
                    Id = case Doc#doc.id of
                        <<>> -> couch_uuids:new();
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
            case couch_db:update_docs(Db, Docs, Options2) of
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
            Docs = lists:map(fun(JsonObj) ->
                    Doc = couch_doc:from_json_obj_validate(JsonObj, DbName),
                    validate_attachment_names(Doc),
                    Doc
                end, DocsArray),
            {ok, Errors} = couch_db:update_docs(Db, Docs, Options, replicated_changes),
            ErrorsJson =
                lists:map(fun update_doc_result_to_json/1, Errors),
            send_json(Req, 201, ErrorsJson)
        end
    end;
db_req(#httpd{path_parts=[_,<<"_bulk_docs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_purge">>]}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {IdsRevs} = couch_httpd:json_body_obj(Req),
    IdsRevs2 = [{Id, couch_doc:parse_revs(Revs)} || {Id, Revs} <- IdsRevs],

    case couch_db:purge_docs(Db, IdsRevs2) of
    {ok, PurgeSeq, PurgedIdsRevs} ->
        PurgedIdsRevs2 = [{Id, couch_doc:revs_to_strs(Revs)} || {Id, Revs} <- PurgedIdsRevs],
        send_json(Req, 200, {[{<<"purge_seq">>, PurgeSeq}, {<<"purged">>, {PurgedIdsRevs2}}]});
    Error ->
        throw(Error)
    end;

db_req(#httpd{path_parts=[_,<<"_purge">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_missing_revs">>]}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {JsonDocIdRevs} = couch_httpd:json_body_obj(Req),
    JsonDocIdRevs2 = [{Id, [couch_doc:parse_rev(RevStr) || RevStr <- RevStrs]} || {Id, RevStrs} <- JsonDocIdRevs],
    {ok, Results} = couch_db:get_missing_revs(Db, JsonDocIdRevs2),
    Results2 = [{Id, couch_doc:revs_to_strs(Revs)} || {Id, Revs, _} <- Results],
    send_json(Req, {[
        {missing_revs, {Results2}}
    ]});

db_req(#httpd{path_parts=[_,<<"_missing_revs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_revs_diff">>]}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {JsonDocIdRevs} = couch_httpd:json_body_obj(Req),
    JsonDocIdRevs2 =
        [{Id, couch_doc:parse_revs(RevStrs)} || {Id, RevStrs} <- JsonDocIdRevs],
    {ok, Results} = couch_db:get_missing_revs(Db, JsonDocIdRevs2),
    Results2 =
    lists:map(fun({Id, MissingRevs, PossibleAncestors}) ->
        {Id,
            {[{missing, couch_doc:revs_to_strs(MissingRevs)}] ++
                if PossibleAncestors == [] ->
                    [];
                true ->
                    [{possible_ancestors,
                        couch_doc:revs_to_strs(PossibleAncestors)}]
                end}}
    end, Results),
    send_json(Req, {Results2});

db_req(#httpd{path_parts=[_,<<"_revs_diff">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='PUT',path_parts=[_,<<"_security">>]}=Req, Db) ->
    SecObj = couch_httpd:json_body(Req),
    ok = couch_db:set_security(Db, SecObj),
    send_json(Req, {[{<<"ok">>, true}]});

db_req(#httpd{method='GET',path_parts=[_,<<"_security">>]}=Req, Db) ->
    send_json(Req, couch_db:get_security(Db));

db_req(#httpd{path_parts=[_,<<"_security">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "PUT,GET");

db_req(#httpd{method='PUT',path_parts=[_,<<"_revs_limit">>]}=Req,
        Db) ->
    Limit = couch_httpd:json_body(Req),
   case is_integer(Limit) of
   true ->
       ok = couch_db:set_revs_limit(Db, Limit),
       send_json(Req, {[{<<"ok">>, true}]});
   false ->
       throw({bad_request, <<"Rev limit has to be an integer">>})
   end;

db_req(#httpd{method='GET',path_parts=[_,<<"_revs_limit">>]}=Req, Db) ->
    send_json(Req, couch_db:get_revs_limit(Db));

db_req(#httpd{path_parts=[_,<<"_revs_limit">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "PUT,GET");

% Special case to enable using an unencoded slash in the URL of design docs,
% as slashes in document IDs must otherwise be URL encoded.
db_req(#httpd{method='GET',mochi_req=MochiReq, path_parts=[DbName,<<"_design/",_/binary>>|_]}=Req, _Db) ->
    PathFront = "/" ++ couch_httpd:quote(binary_to_list(DbName)) ++ "/",
    [_|PathTail] = re:split(MochiReq:get(raw_path), "_design%2F",
        [{return, list}]),
    couch_httpd:send_redirect(Req, PathFront ++ "_design/" ++
        mochiweb_util:join(PathTail, "_design%2F"));

db_req(#httpd{path_parts=[_DbName,<<"_design">>,Name]}=Req, Db) ->
    db_doc_req(Req, Db, <<"_design/",Name/binary>>);

db_req(#httpd{path_parts=[_DbName,<<"_design">>,Name|FileNameParts]}=Req, Db) ->
    db_attachment_req(Req, Db, <<"_design/",Name/binary>>, FileNameParts);


% Special case to allow for accessing local documents without %2F
% encoding the docid. Throws out requests that don't have the second
% path part or that specify an attachment name.
db_req(#httpd{path_parts=[_DbName, <<"_local">>]}, _Db) ->
    throw({bad_request, <<"Invalid _local document id.">>});

db_req(#httpd{path_parts=[_DbName, <<"_local/">>]}, _Db) ->
    throw({bad_request, <<"Invalid _local document id.">>});

db_req(#httpd{path_parts=[_DbName, <<"_local">>, Name]}=Req, Db) ->
    db_doc_req(Req, Db, <<"_local/", Name/binary>>);

db_req(#httpd{path_parts=[_DbName, <<"_local">> | _Rest]}, _Db) ->
    throw({bad_request, <<"_local documents do not accept attachments.">>});

db_req(#httpd{path_parts=[_, DocId]}=Req, Db) ->
    db_doc_req(Req, Db, DocId);

db_req(#httpd{path_parts=[_, DocId | FileNameParts]}=Req, Db) ->
    db_attachment_req(Req, Db, DocId, FileNameParts).

db_doc_req(#httpd{method='DELETE'}=Req, Db, DocId) ->
    % check for the existence of the doc to handle the 404 case.
    couch_doc_open(Db, DocId, nil, []),
    DbName = couch_db:name(Db),
    case couch_httpd:qs_value(Req, "rev") of
    undefined ->
        update_doc(Req, Db, DocId,
                couch_doc_from_req(Req, DocId, {[{<<"_deleted">>,true}]},
                    DbName));
    Rev ->
        update_doc(Req, Db, DocId,
                couch_doc_from_req(Req, DocId,
                    {[{<<"_rev">>, ?l2b(Rev)},{<<"_deleted">>,true}]},
                    DbName))
    end;

db_doc_req(#httpd{method = 'GET', mochi_req = MochiReq} = Req, Db, DocId) ->
    #doc_query_args{
        rev = Rev,
        open_revs = Revs,
        options = Options1,
        atts_since = AttsSince
    } = parse_doc_query(Req),
    Options = case AttsSince of
    nil ->
        Options1;
    RevList when is_list(RevList) ->
        [{atts_since, RevList}, attachments | Options1]
    end,
    case Revs of
    [] ->
        Doc = couch_doc_open(Db, DocId, Rev, Options),
        send_doc(Req, Doc, Options);
    _ ->
        {ok, Results} = couch_db:open_doc_revs(Db, DocId, Revs, Options),
        case MochiReq:accepts_content_type("multipart/mixed") of
        false ->
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
                        RevStr = couch_doc:rev_to_str(RevId),
                        Json = ?JSON_ENCODE({[{<<"missing">>, RevStr}]}),
                        send_chunk(Resp, AccSeparator ++ Json)
                    end,
                    "," % AccSeparator now has a comma
                end,
                "", Results),
            send_chunk(Resp, "]"),
            end_json_response(Resp);
        true ->
            send_docs_multipart(Req, Results, Options)
        end
    end;


db_doc_req(#httpd{method='POST'}=Req, Db, DocId) ->
    couch_httpd:validate_referer(Req),
    DbName = couch_db:name(Db),
    couch_doc:validate_docid(DocId, DbName),
    couch_httpd:validate_ctype(Req, "multipart/form-data"),
    Form = couch_httpd:parse_form(Req),
    case couch_util:get_value("_doc", Form) of
    undefined ->
        Rev = couch_doc:parse_rev(couch_util:get_value("_rev", Form)),
        {ok, [{ok, Doc}]} = couch_db:open_doc_revs(Db, DocId, [Rev], []);
    Json ->
        Doc = couch_doc_from_req(Req, DocId, ?JSON_DECODE(Json), DbName)
    end,
    UpdatedAtts = [
        couch_att:new([
            {name, validate_attachment_name(Name)},
            {type, list_to_binary(ContentType)},
            {data, Content}
        ]) ||
        {Name, {ContentType, _}, Content} <-
        proplists:get_all_values("_attachments", Form)
    ],
    #doc{atts=OldAtts} = Doc,
    OldAtts2 = lists:flatmap(
        fun(Att) ->
            OldName = couch_att:fetch(name, Att),
            case [1 || A <- UpdatedAtts, couch_att:fetch(name, A) == OldName] of
            [] -> [Att]; % the attachment wasn't in the UpdatedAtts, return it
            _ -> [] % the attachment was in the UpdatedAtts, drop it
            end
        end, OldAtts),
    NewDoc = Doc#doc{
        atts = UpdatedAtts ++ OldAtts2
    },
    update_doc(Req, Db, DocId, NewDoc);

db_doc_req(#httpd{method='PUT'}=Req, Db, DocId) ->
    DbName = couch_db:name(Db),
    couch_doc:validate_docid(DocId, DbName),

    case couch_util:to_list(couch_httpd:header_value(Req, "Content-Type")) of
    ("multipart/related;" ++ _) = ContentType ->
        couch_httpd:check_max_request_length(Req),
        {ok, Doc0, WaitFun, Parser} = couch_doc:doc_from_multi_part_stream(
            ContentType, fun() -> receive_request_data(Req) end),
        Doc = couch_doc_from_req(Req, DocId, Doc0, DbName),
        try
            Result = update_doc(Req, Db, DocId, Doc),
            WaitFun(),
            Result
        catch throw:Err ->
            % Document rejected by a validate_doc_update function.
            couch_httpd_multipart:abort_multipart_stream(Parser),
            throw(Err)
        end;
    _Else ->
        Body = couch_httpd:json_body(Req),
        Doc = couch_doc_from_req(Req, DocId, Body, DbName),
        update_doc(Req, Db, DocId, Doc)
    end;

db_doc_req(#httpd{method='COPY'}=Req, Db, SourceDocId) ->
    SourceRev =
    case extract_header_rev(Req, couch_httpd:qs_value(Req, "rev")) of
        missing_rev -> nil;
        Rev -> Rev
    end,
    {TargetDocId0, TargetRevs} = parse_copy_destination_header(Req),
    TargetDocId = list_to_binary(mochiweb_util:unquote(TargetDocId0)),
    % open old doc
    Doc = couch_doc_open(Db, SourceDocId, SourceRev, []),
    % save new doc
    update_doc(Req, Db, TargetDocId, Doc#doc{id=TargetDocId, revs=TargetRevs});

db_doc_req(Req, _Db, _DocId) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST,PUT,COPY").


send_doc(Req, Doc, Options) ->
    case Doc#doc.meta of
    [] ->
        DiskEtag = couch_httpd:doc_etag(Doc),
        % output etag only when we have no meta
        couch_httpd:etag_respond(Req, DiskEtag, fun() ->
            send_doc_efficiently(Req, Doc, [{"ETag", DiskEtag}], Options)
        end);
    _ ->
        send_doc_efficiently(Req, Doc, [], Options)
    end.


send_doc_efficiently(Req, #doc{atts=[]}=Doc, Headers, Options) ->
        send_json(Req, 200, Headers, couch_doc:to_json_obj(Doc, Options));
send_doc_efficiently(#httpd{mochi_req = MochiReq} = Req,
    #doc{atts = Atts} = Doc, Headers, Options) ->
    case lists:member(attachments, Options) of
    true ->
        case MochiReq:accepts_content_type("multipart/related") of
        false ->
            send_json(Req, 200, Headers, couch_doc:to_json_obj(Doc, Options));
        true ->
            Boundary = couch_uuids:random(),
            JsonBytes = ?JSON_ENCODE(couch_doc:to_json_obj(Doc,
                    [attachments, follows, att_encoding_info | Options])),
            {ContentType, Len} = couch_doc:len_doc_to_multi_part_stream(
                    Boundary,JsonBytes, Atts, true),
            CType = {"Content-Type", ?b2l(ContentType)},
            {ok, Resp} = start_response_length(Req, 200, [CType|Headers], Len),
            couch_doc:doc_to_multi_part_stream(Boundary,JsonBytes,Atts,
                    fun(Data) -> couch_httpd:send(Resp, Data) end, true)
        end;
    false ->
        send_json(Req, 200, Headers, couch_doc:to_json_obj(Doc, Options))
    end.

send_docs_multipart(Req, Results, Options1) ->
    OuterBoundary = couch_uuids:random(),
    InnerBoundary = couch_uuids:random(),
    Options = [attachments, follows, att_encoding_info | Options1],
    CType = {"Content-Type",
        "multipart/mixed; boundary=\"" ++ ?b2l(OuterBoundary) ++ "\""},
    {ok, Resp} = start_chunked_response(Req, 200, [CType]),
    couch_httpd:send_chunk(Resp, <<"--", OuterBoundary/binary>>),
    lists:foreach(
        fun({ok, #doc{atts=Atts}=Doc}) ->
            JsonBytes = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, Options)),
            {ContentType, _Len} = couch_doc:len_doc_to_multi_part_stream(
                    InnerBoundary, JsonBytes, Atts, true),
            couch_httpd:send_chunk(Resp, <<"\r\nContent-Type: ",
                    ContentType/binary, "\r\n\r\n">>),
            couch_doc:doc_to_multi_part_stream(InnerBoundary, JsonBytes, Atts,
                    fun(Data) -> couch_httpd:send_chunk(Resp, Data)
                    end, true),
             couch_httpd:send_chunk(Resp, <<"\r\n--", OuterBoundary/binary>>);
        ({{not_found, missing}, RevId}) ->
             RevStr = couch_doc:rev_to_str(RevId),
             Json = ?JSON_ENCODE({[{<<"missing">>, RevStr}]}),
             couch_httpd:send_chunk(Resp,
                [<<"\r\nContent-Type: application/json; error=\"true\"\r\n\r\n">>,
                Json,
                <<"\r\n--", OuterBoundary/binary>>])
         end, Results),
    couch_httpd:send_chunk(Resp, <<"--">>),
    couch_httpd:last_chunk(Resp).

send_ranges_multipart(Req, ContentType, Len, Att, Ranges) ->
    Boundary = couch_uuids:random(),
    CType = {"Content-Type",
        "multipart/byteranges; boundary=\"" ++ ?b2l(Boundary) ++ "\""},
    {ok, Resp} = start_chunked_response(Req, 206, [CType]),
    couch_httpd:send_chunk(Resp, <<"--", Boundary/binary>>),
    lists:foreach(fun({From, To}) ->
        ContentRange = ?l2b(make_content_range(From, To, Len)),
        couch_httpd:send_chunk(Resp,
            <<"\r\nContent-Type: ", ContentType/binary, "\r\n",
            "Content-Range: ", ContentRange/binary, "\r\n",
           "\r\n">>),
        couch_att:range_foldl(Att, From, To + 1,
            fun(Seg, _) -> send_chunk(Resp, Seg) end, {ok, Resp}),
        couch_httpd:send_chunk(Resp, <<"\r\n--", Boundary/binary>>)
    end, Ranges),
    couch_httpd:send_chunk(Resp, <<"--">>),
    couch_httpd:last_chunk(Resp),
    {ok, Resp}.

receive_request_data(Req) ->
    receive_request_data(Req, couch_httpd:body_length(Req)).

receive_request_data(Req, LenLeft) when LenLeft > 0 ->
    Len = erlang:min(4096, LenLeft),
    Data = couch_httpd:recv(Req, Len),
    {Data, fun() -> receive_request_data(Req, LenLeft - iolist_size(Data)) end};
receive_request_data(_Req, _) ->
    throw(<<"expected more data">>).

make_content_range(From, To, Len) ->
    io_lib:format("bytes ~B-~B/~B", [From, To, Len]).

update_doc_result_to_json({{Id, Rev}, Error}) ->
        {_Code, Err, Msg} = couch_httpd:error_info(Error),
        {[{id, Id}, {rev, couch_doc:rev_to_str(Rev)},
            {error, Err}, {reason, Msg}]}.

update_doc_result_to_json(#doc{id=DocId}, Result) ->
    update_doc_result_to_json(DocId, Result);
update_doc_result_to_json(DocId, {ok, NewRev}) ->
    {[{ok, true}, {id, DocId}, {rev, couch_doc:rev_to_str(NewRev)}]};
update_doc_result_to_json(DocId, Error) ->
    {_Code, ErrorStr, Reason} = couch_httpd:error_info(Error),
    {[{id, DocId}, {error, ErrorStr}, {reason, Reason}]}.


update_doc(Req, Db, DocId, #doc{deleted=false}=Doc) ->
    DbName = couch_db:name(Db),
    Loc = absolute_uri(Req, "/" ++ couch_util:url_encode(DbName) ++ "/" ++ couch_util:url_encode(DocId)),
    update_doc(Req, Db, DocId, Doc, [{"Location", Loc}]);
update_doc(Req, Db, DocId, Doc) ->
    update_doc(Req, Db, DocId, Doc, []).

update_doc(Req, Db, DocId, Doc, Headers) ->
    #doc_query_args{
        update_type = UpdateType
    } = parse_doc_query(Req),
    update_doc(Req, Db, DocId, Doc, Headers, UpdateType).

update_doc(Req, Db, DocId, #doc{deleted=Deleted}=Doc, Headers, UpdateType) ->
    case couch_httpd:header_value(Req, "X-Couch-Full-Commit") of
    "true" ->
        Options = [full_commit];
    "false" ->
        Options = [delay_commit];
    _ ->
        Options = []
    end,
    case couch_httpd:qs_value(Req, "batch") of
    "ok" ->
        % async batching
        spawn(fun() ->
                case catch(couch_db:update_doc(Db, Doc, Options, UpdateType)) of
                {ok, _} -> ok;
                Error ->
                    couch_log:info("Batch doc error (~s): ~p",[DocId, Error])
                end
            end),
        send_json(Req, 202, Headers, {[
            {ok, true},
            {id, DocId}
        ]});
    _Normal ->
        % normal
        {ok, NewRev} = couch_db:update_doc(Db, Doc, Options, UpdateType),
        NewRevStr = couch_doc:rev_to_str(NewRev),
        ResponseHeaders = [{"ETag", <<"\"", NewRevStr/binary, "\"">>}] ++ Headers,
        send_json(Req,
            if Deleted orelse Req#httpd.method == 'DELETE' -> 200;
            true -> 201 end,
            ResponseHeaders, {[
                {ok, true},
                {id, DocId},
                {rev, NewRevStr}]})
    end.

couch_doc_from_req(Req, DocId, #doc{revs=Revs}=Doc, _) ->
    validate_attachment_names(Doc),
    Rev = case couch_httpd:qs_value(Req, "rev") of
    undefined ->
        undefined;
    QSRev ->
        couch_doc:parse_rev(QSRev)
    end,
    Revs2 =
    case Revs of
    {Start, [RevId|_]} ->
        if Rev /= undefined andalso Rev /= {Start, RevId} ->
            throw({bad_request, "Document rev from request body and query "
                   "string have different values"});
        true ->
            case extract_header_rev(Req, {Start, RevId}) of
            missing_rev -> {0, []};
            _ -> Revs
            end
        end;
    _ ->
        case extract_header_rev(Req, Rev) of
        missing_rev -> {0, []};
        {Pos, RevId2} -> {Pos, [RevId2]}
        end
    end,
    Doc#doc{id=DocId, revs=Revs2};
couch_doc_from_req(Req, DocId, Json, DbName) ->
    couch_doc_from_req(Req, DocId,
        couch_doc:from_json_obj_validate(Json, DbName), DbName).

% Useful for debugging
% couch_doc_open(Db, DocId) ->
%   couch_doc_open(Db, DocId, nil, []).

couch_doc_open(Db, DocId, Rev, Options) ->
    case Rev of
    nil -> % open most recent rev
        case couch_db:open_doc(Db, DocId, Options) of
        {ok, Doc} ->
            Doc;
         Error ->
             throw(Error)
         end;
  _ -> % open a specific rev (deletions come back as stubs)
      case couch_db:open_doc_revs(Db, DocId, [Rev], Options) of
          {ok, [{ok, Doc}]} ->
              Doc;
          {ok, [{{not_found, missing}, Rev}]} ->
              throw(not_found);
          {ok, [Else]} ->
              throw(Else)
      end
  end.

% Attachment request handlers

db_attachment_req(#httpd{method='GET',mochi_req=MochiReq}=Req, Db, DocId, FileNameParts) ->
    FileName = list_to_binary(mochiweb_util:join(lists:map(fun binary_to_list/1, FileNameParts),"/")),
    #doc_query_args{
        rev=Rev,
        options=Options
    } = parse_doc_query(Req),
    #doc{
        atts=Atts
    } = Doc = couch_doc_open(Db, DocId, Rev, Options),
    case [A || A <- Atts, couch_att:fetch(name, A) == FileName] of
    [] ->
        throw({not_found, "Document is missing attachment"});
    [Att] ->
        [Type, Enc, DiskLen, AttLen, Md5] = couch_att:fetch([type, encoding, disk_len, att_len, md5], Att),
        Etag = case Md5 of
            <<>> -> couch_httpd:doc_etag(Doc);
            _ -> "\"" ++ ?b2l(base64:encode(Md5)) ++ "\""
        end,
        ReqAcceptsAttEnc = lists:member(
           atom_to_list(Enc),
           couch_httpd:accepted_encodings(Req)
        ),
        Len = case {Enc, ReqAcceptsAttEnc} of
        {identity, _} ->
            % stored and served in identity form
            DiskLen;
        {_, false} when DiskLen =/= AttLen ->
            % Stored encoded, but client doesn't accept the encoding we used,
            % so we need to decode on the fly.  DiskLen is the identity length
            % of the attachment.
            DiskLen;
        {_, true} ->
            % Stored and served encoded.  AttLen is the encoded length.
            AttLen;
        _ ->
            % We received an encoded attachment and stored it as such, so we
            % don't know the identity length.  The client doesn't accept the
            % encoding, and since we cannot serve a correct Content-Length
            % header we'll fall back to a chunked response.
            undefined
        end,
        Headers = [
            {"ETag", Etag},
            {"Cache-Control", "must-revalidate"},
            {"Content-Type", binary_to_list(Type)}
        ] ++ case ReqAcceptsAttEnc of
        true when Enc =/= identity ->
            % RFC 2616 says that the 'identify' encoding should not be used in
            % the Content-Encoding header
            [{"Content-Encoding", atom_to_list(Enc)}];
        _ ->
            []
        end ++ case Enc of
            identity ->
                [{"Accept-Ranges", "bytes"}];
            _ ->
                [{"Accept-Ranges", "none"}]
        end,
        AttFun = case ReqAcceptsAttEnc of
        false ->
            fun couch_att:foldl_decode/3;
        true ->
            fun couch_att:foldl/3
        end,
        couch_httpd:etag_respond(
            Req,
            Etag,
            fun() ->
                case Len of
                undefined ->
                    {ok, Resp} = start_chunked_response(Req, 200, Headers),
                    AttFun(Att, fun(Seg, _) -> send_chunk(Resp, Seg) end, {ok, Resp}),
                    last_chunk(Resp);
                _ ->
                    Ranges = parse_ranges(MochiReq:get(range), Len),
                    case {Enc, Ranges} of
                        {identity, [{From, To}]} ->
                            Headers1 = [{"Content-Range", make_content_range(From, To, Len)}]
                                ++ Headers,
                            {ok, Resp} = start_response_length(Req, 206, Headers1, To - From + 1),
                            couch_att:range_foldl(Att, From, To + 1,
                                fun(Seg, _) -> send(Resp, Seg) end, {ok, Resp});
                        {identity, Ranges} when is_list(Ranges) andalso length(Ranges) < 10 ->
                            send_ranges_multipart(Req, Type, Len, Att, Ranges);
                        _ ->
                            Headers1 = Headers ++
                                if Enc =:= identity orelse ReqAcceptsAttEnc =:= true ->
                                    [{"Content-MD5", base64:encode(Md5)}];
                                true ->
                                    []
                            end,
                            {ok, Resp} = start_response_length(Req, 200, Headers1, Len),
                            AttFun(Att, fun(Seg, _) -> send(Resp, Seg) end, {ok, Resp})
                    end
                end
            end
        )
    end;


db_attachment_req(#httpd{method=Method,mochi_req=MochiReq}=Req, Db, DocId, FileNameParts)
        when (Method == 'PUT') or (Method == 'DELETE') ->
    FileName = validate_attachment_name(
                    mochiweb_util:join(
                        lists:map(fun binary_to_list/1,
                            FileNameParts),"/")),
    NewAtt = case Method of
        'DELETE' ->
            [];
        _ ->
            MimeType = case couch_httpd:header_value(Req,"Content-Type") of
                % We could throw an error here or guess by the FileName.
                % Currently, just giving it a default.
                undefined -> <<"application/octet-stream">>;
                CType -> list_to_binary(CType)
            end,
            Data = case couch_httpd:body_length(Req) of
                undefined ->
                    <<"">>;
                {unknown_transfer_encoding, Unknown} ->
                    exit({unknown_transfer_encoding, Unknown});
                chunked ->
                    fun(MaxChunkSize, ChunkFun, InitState) ->
                        couch_httpd:recv_chunked(
                            Req, MaxChunkSize, ChunkFun, InitState
                        )
                    end;
                0 ->
                    <<"">>;
                Length when is_integer(Length) ->
                    Expect = case couch_httpd:header_value(Req, "expect") of
                        undefined ->
                            undefined;
                        Value when is_list(Value) ->
                            string:to_lower(Value)
                    end,
                    case Expect of
                        "100-continue" ->
                            MochiReq:start_raw_response({100, gb_trees:empty()});
                        _Else ->
                            ok
                    end,
                    fun() -> couch_httpd:recv(Req, 0) end;
                Length ->
                    exit({length_not_integer, Length})
            end,
            AttLen = case couch_httpd:header_value(Req,"Content-Length") of
                undefined -> undefined;
                Len -> list_to_integer(Len)
            end,
            ContentEnc = string:to_lower(string:strip(
                couch_httpd:header_value(Req,"Content-Encoding","identity")
            )),
            Encoding = case ContentEnc of
                "identity" ->
                   identity;
                "gzip" ->
                   gzip;
                _ ->
                   throw({
                       bad_ctype,
                       "Only gzip and identity content-encodings are supported"
                   })
            end,
            [couch_att:new([
                {name, FileName},
                {type, MimeType},
                {data, Data},
                {att_len, AttLen},
                {md5, get_md5_header(Req)},
                {encoding, Encoding}
            ])]
    end,

    Doc = case extract_header_rev(Req, couch_httpd:qs_value(Req, "rev")) of
        missing_rev -> % make the new doc
            if Method =/= 'DELETE' -> ok; true ->
                % check for the existence of the doc to handle the 404 case.
                couch_doc_open(Db, DocId, nil, [])
            end,
            couch_doc:validate_docid(DocId, couch_db:name(Db)),
            #doc{id=DocId};
        Rev ->
            case couch_db:open_doc_revs(Db, DocId, [Rev], []) of
                {ok, [{ok, Doc0}]} -> Doc0;
                {ok, [{{not_found, missing}, Rev}]} -> throw(conflict);
                {ok, [Error]} -> throw(Error)
            end
    end,

    #doc{atts=Atts} = Doc,
    DocEdited = Doc#doc{
        atts = NewAtt ++ [A || A <- Atts, couch_att:fetch(name, A) /= FileName]
    },

    Headers = case Method of
    'DELETE' ->
        [];
    _ ->
        [{"Location", absolute_uri(Req, "/" ++
            couch_util:url_encode(couch_db:name(Db)) ++ "/" ++
            couch_util:url_encode(DocId) ++ "/" ++
            couch_util:url_encode(FileName)
        )}]
    end,
    update_doc(Req, Db, DocId, DocEdited, Headers);

db_attachment_req(Req, _Db, _DocId, _FileNameParts) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,PUT").

parse_ranges(undefined, _Len) ->
    undefined;
parse_ranges(fail, _Len) ->
    undefined;
parse_ranges(Ranges, Len) ->
    parse_ranges(Ranges, Len, []).

parse_ranges([], _Len, Acc) ->
    lists:reverse(Acc);
parse_ranges([{0, none}|_], _Len, _Acc) ->
    undefined;
parse_ranges([{From, To}|_], _Len, _Acc) when is_integer(From) andalso is_integer(To) andalso To < From ->
    throw(requested_range_not_satisfiable);
parse_ranges([{From, To}|Rest], Len, Acc) when is_integer(To) andalso To >= Len ->
    parse_ranges([{From, Len-1}] ++ Rest, Len, Acc);
parse_ranges([{none, To}|Rest], Len, Acc) ->
    parse_ranges([{Len - To, Len - 1}] ++ Rest, Len, Acc);
parse_ranges([{From, none}|Rest], Len, Acc) ->
    parse_ranges([{From, Len - 1}] ++ Rest, Len, Acc);
parse_ranges([{From,To}|Rest], Len, Acc) ->
    parse_ranges(Rest, Len, [{From, To}] ++ Acc).

get_md5_header(Req) ->
    ContentMD5 = couch_httpd:header_value(Req, "Content-MD5"),
    Length = couch_httpd:body_length(Req),
    Trailer = couch_httpd:header_value(Req, "Trailer"),
    case {ContentMD5, Length, Trailer} of
        _ when is_list(ContentMD5) orelse is_binary(ContentMD5) ->
            base64:decode(ContentMD5);
        {_, chunked, undefined} ->
            <<>>;
        {_, chunked, _} ->
            case re:run(Trailer, "\\bContent-MD5\\b", [caseless]) of
                {match, _} ->
                    md5_in_footer;
                _ ->
                    <<>>
            end;
        _ ->
            <<>>
    end.

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
            Args#doc_query_args{open_revs=couch_doc:parse_revs(JsonArray)};
        {"latest", "true"} ->
            Options = [latest | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"atts_since", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{atts_since = couch_doc:parse_revs(JsonArray)};
        {"new_edits", "false"} ->
            Args#doc_query_args{update_type=replicated_changes};
        {"new_edits", "true"} ->
            Args#doc_query_args{update_type=interactive_edit};
        {"att_encoding_info", "true"} ->
            Options = [att_encoding_info | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #doc_query_args{}, couch_httpd:qs(Req)).

parse_changes_query(Req, Db) ->
    ChangesArgs = lists:foldl(fun({Key, Value}, Args) ->
        case {string:to_lower(Key), Value} of
        {"feed", "live"} ->
            %% sugar for continuous
            Args#changes_args{feed="continuous"};
        {"feed", _} ->
            Args#changes_args{feed=Value};
        {"descending", "true"} ->
            Args#changes_args{dir=rev};
        {"since", "now"} ->
            UpdateSeq = couch_util:with_db(couch_db:name(Db), fun(WDb) ->
                                        couch_db:get_update_seq(WDb)
                                end),
            Args#changes_args{since=UpdateSeq};
        {"since", _} ->
            Args#changes_args{since=list_to_integer(Value)};
        {"last-event-id", _} ->
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
        {"attachments", "true"} ->
            Opts = Args#changes_args.doc_options,
            Args#changes_args{doc_options=[attachments|Opts]};
        {"att_encoding_info", "true"} ->
            Opts = Args#changes_args.doc_options,
            Args#changes_args{doc_options=[att_encoding_info|Opts]};
        {"conflicts", "true"} ->
            Args#changes_args{conflicts=true};
        {"filter", _} ->
            Args#changes_args{filter=Value};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #changes_args{}, couch_httpd:qs(Req)),
    %% if it's an EventSource request with a Last-event-ID header
    %% that should override the `since` query string, since it's
    %% probably the browser reconnecting.
    case ChangesArgs#changes_args.feed of
        "eventsource" ->
            case couch_httpd:header_value(Req, "last-event-id") of
                undefined ->
                    ChangesArgs;
                Value ->
                    ChangesArgs#changes_args{since=list_to_integer(Value)}
            end;
        _ ->
            ChangesArgs
    end.

extract_header_rev(Req, ExplicitRev) when is_binary(ExplicitRev) or is_list(ExplicitRev)->
    extract_header_rev(Req, couch_doc:parse_rev(ExplicitRev));
extract_header_rev(Req, ExplicitRev) ->
    Etag = case couch_httpd:header_value(Req, "If-Match") of
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
    case couch_httpd:header_value(Req, "Destination") of
    undefined ->
        throw({bad_request, "Destination header is mandatory for COPY."});
    Destination ->
        case re:run(Destination, "^https?://", [{capture, none}]) of
        match ->
            throw({bad_request, "Destination URL must be relative."});
        nomatch ->
            % see if ?rev=revid got appended to the Destination header
            case re:run(Destination, "\\?", [{capture, none}]) of
            nomatch ->
                {list_to_binary(Destination), {0, []}};
            match ->
                [DocId, RevQs] = re:split(Destination, "\\?", [{return, list}]),
                [_RevQueryKey, Rev] = re:split(RevQs, "=", [{return, list}]),
                {Pos, RevId} = couch_doc:parse_rev(Rev),
                {list_to_binary(DocId), {Pos, [RevId]}}
            end
        end
    end.

validate_attachment_names(Doc) ->
    lists:foreach(fun(Att) ->
        Name = couch_att:fetch(name, Att),
        validate_attachment_name(Name)
    end, Doc#doc.atts).

validate_attachment_name(Name) when is_list(Name) ->
    validate_attachment_name(list_to_binary(Name));
validate_attachment_name(<<"_",_/binary>>) ->
    throw({bad_request, <<"Attachment name can't start with '_'">>});
validate_attachment_name(Name) ->
    case couch_util:validate_utf8(Name) of
        true -> Name;
        false -> throw({bad_request, <<"Attachment name is not UTF-8 encoded">>})
    end.
