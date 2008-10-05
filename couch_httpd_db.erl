% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_db).
-include("couch_db.hrl").

-export([handle_request/1, db_req/2, couch_doc_open/4]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1,
    start_chunked_response/3]).

-record(doc_query_args, {
    options = [],
    rev = "",
    open_revs = ""
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
        Handler = couch_util:dict_find(SecondPart, DbUrlHandlers, fun db_req/2),
        do_db_req(Req, Handler)
    end.

create_db_req(Req, DbName) ->
    case couch_server:create(DbName, []) of
    {ok, Db} ->
        couch_db:close(Db),
        send_json(Req, 201, {[{ok, true}]});
    Error ->
        throw(Error)
    end.

delete_db_req(Req, DbName) ->
    case couch_server:delete(DbName) of
    ok ->
        send_json(Req, 200, {[{ok, true}]});
    Error ->
        throw(Error)
    end.

do_db_req(#httpd{path_parts=[DbName|_]}=Req, Fun) ->
    case couch_db:open(DbName, []) of
    {ok, Db} ->
        try
            Fun(Req, Db)
        after
            couch_db:close(Db)
        end;
    Error ->
        throw(Error)
    end.

db_req(#httpd{method='GET',path_parts=[_DbName]}=Req, Db) ->
    {ok, DbInfo} = couch_db:get_db_info(Db),
    send_json(Req, {DbInfo});

db_req(#httpd{method='POST',path_parts=[_DbName]}=Req, Db) ->
    Doc = couch_doc:from_json_obj(couch_httpd:json_body(Req)),
    DocId = couch_util:new_uuid(),
    {ok, NewRev} = couch_db:update_doc(Db, Doc#doc{id=DocId, revs=[]}, []),
    send_json(Req, 201, {[
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

db_req(#httpd{path_parts=[_DbName]}=Req, _Db) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_bulk_docs">>]}=Req, Db) ->
    {JsonProps} = couch_httpd:json_body(Req),
    DocsArray = proplists:get_value(<<"docs">>, JsonProps),
    % convert all the doc elements to native docs
    case proplists:get_value(<<"new_edits">>, JsonProps, true) of
    true ->
        Docs = lists:map(
            fun({ObjProps} = JsonObj) ->
                Doc = couch_doc:from_json_obj(JsonObj),
                Id = case Doc#doc.id of
                    <<>> -> couch_util:new_uuid();
                    Id0 -> Id0
                end,
                Revs = case proplists:get_value(<<"_rev">>, ObjProps) of
                    undefined -> [];
                    Rev  -> [Rev]
                end,
                Doc#doc{id=Id,revs=Revs}
            end,
            DocsArray),
        {ok, ResultRevs} = couch_db:update_docs(Db, Docs, []),

        % output the results
        DocResults = lists:zipwith(
            fun(Doc, NewRev) ->
                {[{"id", Doc#doc.id}, {"rev", NewRev}]}
            end,
            Docs, ResultRevs),
        send_json(Req, 201, {[
            {ok, true},
            {new_revs, DocResults}
        ]});

    false ->
        Options =
        case proplists:get_value(<<"new_edits">>, JsonProps, true) of
            true -> [new_edits];
            _ -> []
        end,
        Docs = [couch_doc:from_json_obj(JsonObj) || JsonObj <- DocsArray],
        ok = couch_db:save_docs(Db, Docs, Options),
        send_json(Req, 201, {[
            {ok, true}
        ]})
    end;
db_req(#httpd{path_parts=[_,<<"_bulk_docs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_compact">>]}=Req, Db) ->
    ok = couch_db:start_compact(Db),
    send_json(Req, 202, {[{ok, true}]});

db_req(#httpd{path_parts=[_,<<"_compact">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[_,<<"_purge">>]}=Req, Db) ->
    {IdsRevs} = couch_httpd:json_body(Req),
    % validate the json input
    [{_Id, [_|_]=_Revs} = IdRevs || IdRevs <- IdsRevs],
    
    case couch_db:purge_docs(Db, IdsRevs) of
    {ok, PurgeSeq, PurgedIdsRevs} ->
        send_json(Req, 200, {[{<<"purge_seq">>, PurgeSeq}, {<<"purged">>, {PurgedIdsRevs}}]});
    Error ->
        throw(Error)
    end;

db_req(#httpd{path_parts=[_,<<"_purge">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");
   
db_req(#httpd{method='GET',path_parts=[_,<<"_all_docs">>]}=Req, Db) ->
    all_docs_view(Req, Db, nil);

db_req(#httpd{method='POST',path_parts=[_,<<"_all_docs">>]}=Req, Db) ->
    {Props} = couch_httpd:json_body(Req),
    Keys = proplists:get_value(<<"keys">>, Props, nil),
    all_docs_view(Req, Db, Keys);

db_req(#httpd{path_parts=[_,<<"_all_docs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD,POST");

db_req(#httpd{method='GET',path_parts=[_,<<"_all_docs_by_seq">>]}=Req, Db) ->
    #view_query_args{
        start_key = StartKey,
        count = Count,
        skip = SkipCount,
        direction = Dir
    } = QueryArgs = couch_httpd_view:parse_view_query(Req),

    {ok, Info} = couch_db:get_db_info(Db),
    TotalRowCount = proplists:get_value(doc_count, Info),

    FoldlFun = couch_httpd_view:make_view_fold_fun(Req, QueryArgs, Db,
        TotalRowCount, fun couch_db:enum_docs_since_reduce_to_count/1),
    StartKey2 = case StartKey of
        nil -> 0;
        <<>> -> 100000000000;
        StartKey when is_integer(StartKey) -> StartKey
    end,
    {ok, FoldResult} = couch_db:enum_docs_since(Db, StartKey2, Dir,
        fun(DocInfo, Offset, Acc) ->
            #doc_info{
                id=Id,
                rev=Rev,
                update_seq=UpdateSeq,
                deleted=Deleted,
                conflict_revs=ConflictRevs,
                deleted_conflict_revs=DelConflictRevs
            } = DocInfo,
            Json = {
                [{"rev", Rev}] ++
                case ConflictRevs of
                    []  ->  [];
                    _   ->  [{"conflicts", ConflictRevs}]
                end ++
                case DelConflictRevs of
                    []  ->  [];
                    _   ->  [{"deleted_conflicts", DelConflictRevs}]
                end ++
                case Deleted of
                    true -> [{"deleted", true}];
                    false -> []
                end
            },
            FoldlFun({{UpdateSeq, Id}, Json}, Offset, Acc)
        end, {Count, SkipCount, undefined, []}),
    couch_httpd_view:finish_view_fold(Req, TotalRowCount, {ok, FoldResult});

db_req(#httpd{path_parts=[_,<<"_all_docs_by_seq">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD");

db_req(#httpd{method='POST',path_parts=[_,<<"_missing_revs">>]}=Req, Db) ->
    {JsonDocIdRevs} = couch_httpd:json_body(Req),
    {ok, Results} = couch_db:get_missing_revs(Db, JsonDocIdRevs),
    send_json(Req, {[
        {missing_revs, {Results}}
    ]});

db_req(#httpd{path_parts=[_,<<"_missing_revs">>]}=Req, _Db) ->
    send_method_not_allowed(Req, "POST");

db_req(#httpd{method='POST',path_parts=[DbName,<<"_design">>,Name|Rest]}=Req,
        Db) ->
    % Special case to enable using an unencoded in the URL of design docs, as
    % slashes in document IDs must otherwise be URL encoded
    db_req(Req#httpd{path_parts=[DbName,<<"_design/",Name/binary>>|Rest]}, Db);

db_req(#httpd{path_parts=[_, DocId]}=Req, Db) ->
    db_doc_req(Req, Db, DocId);

db_req(#httpd{path_parts=[_, DocId, FileName]}=Req, Db) ->
    db_attachment_req(Req, Db, DocId, FileName).

all_docs_view(Req, Db, Keys) -> 
    #view_query_args{
        start_key = StartKey,
        start_docid = StartDocId,
        count = Count,
        skip = SkipCount,
        direction = Dir
    } = QueryArgs = couch_httpd_view:parse_view_query(Req, Keys),    
    {ok, Info} = couch_db:get_db_info(Db),
    TotalRowCount = proplists:get_value(doc_count, Info),
    StartId = if is_binary(StartKey) -> StartKey;
    true -> StartDocId
    end,
    FoldAccInit = {Count, SkipCount, undefined, []},
    
    case Keys of
    nil ->
        FoldlFun = couch_httpd_view:make_view_fold_fun(Req, QueryArgs, Db,
            TotalRowCount, fun couch_db:enum_docs_reduce_to_count/1),
        AdapterFun = fun(#full_doc_info{id=Id}=FullDocInfo, Offset, Acc) ->
            case couch_doc:to_doc_info(FullDocInfo) of
            #doc_info{deleted=false, rev=Rev} ->
                FoldlFun({{Id, Id}, {[{rev, Rev}]}}, Offset, Acc);
            #doc_info{deleted=true} ->
                {ok, Acc}
            end
        end,
        {ok, FoldResult} = couch_db:enum_docs(Db, StartId, Dir, 
            AdapterFun, FoldAccInit),
        couch_httpd_view:finish_view_fold(Req, TotalRowCount, {ok, FoldResult});
    _ ->
        FoldlFun = couch_httpd_view:make_view_fold_fun(Req, QueryArgs, Db,
            TotalRowCount, fun(Offset) -> Offset end),
        KeyFoldFun = case Dir of
        fwd ->
            fun lists:foldl/3;
        rev ->
            fun lists:foldr/3
        end,
        {ok, FoldResult} = KeyFoldFun(
            fun(Key, {ok, FoldAcc}) ->
                DocInfo = (catch couch_db:get_doc_info(Db, Key)),
                Doc = case DocInfo of
                {ok, #doc_info{id=Id, rev=Rev, deleted=false}} = DocInfo ->
                    {{Id, Id}, {[{rev, Rev}]}};
                {ok, #doc_info{id=Id, rev=Rev, deleted=true}} = DocInfo ->
                    {{Id, Id}, {[{rev, Rev}, {deleted, true}]}};
                not_found ->
                    {{Key, error}, not_found};
                _ ->
                    ?LOG_ERROR("Invalid DocInfo: ~p", [DocInfo]),
                    throw({error, invalid_doc_info})
                end,
                Acc = (catch FoldlFun(Doc, 0, FoldAcc)),
                case Acc of
                {stop, Acc2} ->
                    {ok, Acc2};
                _ ->
                    Acc
                end
            end, {ok, FoldAccInit}, Keys),
        couch_httpd_view:finish_view_fold(Req, TotalRowCount, {ok, FoldResult})        
    end.





db_doc_req(#httpd{method='DELETE'}=Req, Db, DocId) ->
    case extract_header_rev(Req, couch_httpd:qs_value(Req, "rev")) of
    missing_rev ->
        couch_httpd:send_error(Req, 412, <<"missing_rev">>,
            <<"Document rev/etag must be specified to delete">>);
    RevToDelete ->
        {ok, NewRev} = couch_db:delete_doc(Db, DocId, [RevToDelete]),
        send_json(Req, 200, {[
            {ok, true},
            {id, DocId},
            {rev, NewRev}
            ]})
    end;

db_doc_req(#httpd{method='GET'}=Req, Db, DocId) ->
    #doc_query_args{
        rev = Rev,
        open_revs = Revs,
        options = Options
    } = parse_doc_query(Req),
    case Revs of
    [] ->
        Doc = couch_doc_open(Db, DocId, Rev, Options),
        DiskEtag = couch_httpd:doc_etag(Doc),
        EtagsToMatch = string:tokens(
                    couch_httpd:header_value(Req, "If-None-Match", ""), ", "),
        case lists:member(DiskEtag, EtagsToMatch) of
        true ->
            % the client has this in their cache.
            couch_httpd:send_response(Req, 304, [{"Etag", DiskEtag}], <<>>);
        false ->
            Headers =
            case Doc#doc.meta of
            [] -> [{"Etag", DiskEtag}]; % output etag only when we have no meta
            _ -> []
            end,
            send_json(Req, 200, Headers, couch_doc:to_json_obj(Doc, Options))
        end;
    _ ->
        {ok, Results} = couch_db:open_doc_revs(Db, DocId, Revs, Options),
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

db_doc_req(#httpd{method='POST'}=Req, Db, DocId) ->
    Form = couch_httpd:parse_form(Req),
    Rev = list_to_binary(proplists:get_value("_rev", Form)),
    Doc = case couch_db:open_doc_revs(Db, DocId, [Rev], []) of
        {ok, [{ok, Doc0}]}  -> Doc0#doc{revs=[Rev]};
        {ok, [Error]}       -> throw(Error)
    end,

    NewAttachments = [
        {list_to_binary(Name), {list_to_binary(ContentType), Content}} ||
        {Name, {ContentType, _}, Content} <-
        proplists:get_all_values("_attachments", Form)
    ],
    #doc{attachments=Attachments} = Doc,
    NewDoc = Doc#doc{
        attachments = Attachments ++ NewAttachments
    },
    {ok, NewRev} = couch_db:update_doc(Db, NewDoc, []),

    send_json(Req, 201, [{"Etag", "\"" ++ NewRev ++ "\""}], {obj, [
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

db_doc_req(#httpd{method='PUT'}=Req, Db, DocId) ->
    Json = couch_httpd:json_body(Req),
    Doc = couch_doc:from_json_obj(Json),
    ExplicitRev =
    case Doc#doc.revs of
        [Rev0|_] -> Rev0;
        [] -> undefined
    end,
    case extract_header_rev(Req, ExplicitRev) of
    missing_rev ->
        Revs = [];
    Rev ->
        Revs = [Rev]
    end,
    {ok, NewRev} = couch_db:update_doc(Db, Doc#doc{id=DocId, revs=Revs}, []),
    send_json(Req, 201, [{"Etag", <<"\"", NewRev/binary, "\"">>}], {[
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

db_doc_req(#httpd{method='COPY'}=Req, Db, SourceDocId) ->
    SourceRev =
    case extract_header_rev(Req, couch_httpd:qs_value(Req, "rev")) of
        missing_rev -> [];
        Rev -> Rev
    end,

    {TargetDocId, TargetRev} = parse_copy_destination_header(Req),

    % open revision Rev or Current  
    Doc = couch_doc_open(Db, SourceDocId, SourceRev, []),

    % save new doc
    {ok, NewTargetRev} = couch_db:update_doc(Db, Doc#doc{id=TargetDocId, revs=TargetRev}, []),

    send_json(Req, 201, [{"Etag", "\"" ++ binary_to_list(NewTargetRev) ++ "\""}], {[
        {ok, true},
        {id, TargetDocId},
        {rev, NewTargetRev}
    ]});

db_doc_req(#httpd{method='MOVE'}=Req, Db, SourceDocId) ->
    SourceRev =
    case extract_header_rev(Req, couch_httpd:qs_value(Req, "rev")) of
    missing_rev -> 
        throw({bad_request, "MOVE requires a specified rev parameter"
                "for the origin resource."});
    Rev -> Rev
    end,

    {TargetDocId, TargetRev} = parse_copy_destination_header(Req),
    % open revision Rev or Current
    Doc = couch_doc_open(Db, SourceDocId, SourceRev, []),

    % save new doc & delete old doc in one operation
    Docs = [
        Doc#doc{id=TargetDocId, revs=TargetRev},
        #doc{id=SourceDocId, revs=[SourceRev], deleted=true}
        ],

    {ok, ResultRevs} = couch_db:update_docs(Db, Docs, []),

    DocResults = lists:zipwith(
        fun(FDoc, NewRev) ->
            {[{id, FDoc#doc.id}, {rev, NewRev}]}
        end,
        Docs, ResultRevs),
    send_json(Req, 201, {[
        {ok, true},
        {new_revs, DocResults}
    ]});

db_doc_req(Req, _Db, _DocId) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST,PUT,COPY,MOVE").

% Useful for debugging
% couch_doc_open(Db, DocId) ->
%   couch_doc_open(Db, DocId, [], []).

couch_doc_open(Db, DocId, Rev, Options) ->
    case Rev of
    "" -> % open most recent rev
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
          {ok, [Else]} ->
              throw(Else)
      end
  end.

% Attachment request handlers

db_attachment_req(#httpd{method='GET'}=Req, Db, DocId, FileName) ->
    case couch_db:open_doc(Db, DocId, []) of
    {ok, #doc{attachments=Attachments}} ->
        case proplists:get_value(FileName, Attachments) of
        undefined ->
            throw({not_found, "Document is missing attachment"});
        {Type, Bin} ->
            {ok, Resp} = start_chunked_response(Req, 200, [
                {"Cache-Control", "must-revalidate"},
                {"Content-Type", binary_to_list(Type)},
                {"Content-Length", integer_to_list(couch_doc:bin_size(Bin))}]),
            couch_doc:bin_foldl(Bin,
                fun(BinSegment, []) ->
                    send_chunk(Resp, BinSegment),
                    {ok, []}
                end,
                []
            ),
            send_chunk(Resp, "")
        end;
    Error ->
        throw(Error)
    end;

db_attachment_req(#httpd{method=Method}=Req, Db, DocId, FileName)
        when (Method == 'PUT') or (Method == 'DELETE') ->

    NewAttachment = case Method of
        'DELETE' ->
            [];
        _ ->
            [{FileName, {
                list_to_binary(couch_httpd:header_value(Req,"Content-Type")),
                couch_httpd:body(Req)
            }}]
    end,

    Doc = case extract_header_rev(Req, couch_httpd:qs_value(Req, "rev")) of
        missing_rev -> % make the new doc
            #doc{id=DocId};
        Rev ->
            case couch_db:open_doc_revs(Db, DocId, [Rev], []) of
            {ok, [{ok, Doc0}]}  -> Doc0#doc{revs=[Rev]};
            {ok, [Error]}       -> throw(Error)
            end
    end,

    #doc{attachments=Attachments} = Doc,
    DocEdited = Doc#doc{
        attachments = NewAttachment ++ proplists:delete(FileName, Attachments)
    },
    {ok, UpdatedRev} = couch_db:update_doc(Db, DocEdited, []),
    send_json(Req, case Method of 'DELETE' -> 200; _ -> 201 end, {[
        {ok, true},
        {id, DocId},
        {rev, UpdatedRev}
    ]});

db_attachment_req(Req, _Db, _DocId, _FileName) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,PUT").


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
            Args#doc_query_args{rev=list_to_binary(Rev)};
        {"open_revs", "all"} ->
            Args#doc_query_args{open_revs=all};
        {"open_revs", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{open_revs=JsonArray};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #doc_query_args{}, couch_httpd:qs(Req)).



extract_header_rev(Req, ExplictRev) when is_list(ExplictRev)->
    extract_header_rev(Req, list_to_binary(ExplictRev));
extract_header_rev(Req, ExplictRev) ->
    Etag = case couch_httpd:header_value(Req, "If-Match") of
        undefined -> undefined;
        Tag -> string:strip(Tag, both, $")
    end,
    case {ExplictRev, Etag} of
    {undefined, undefined} -> missing_rev;
    {_, undefined} -> ExplictRev;
    {undefined, _} -> list_to_binary(Etag);
    _ when ExplictRev == Etag -> list_to_binary(Etag);
    _ ->
        throw({bad_request, "Document rev and etag have different values"})
    end.


parse_copy_destination_header(Req) ->
    Destination = couch_httpd:header_value(Req, "Destination"),
    case regexp:match(Destination, "\\?") of
    nomatch -> 
        {list_to_binary(Destination), []};
    {match, _, _} ->
        {ok, [DocId, RevQueryOptions]} = regexp:split(Destination, "\\?"),
        {ok, [_RevQueryKey, Rev]} = regexp:split(RevQueryOptions, "="),
        {list_to_binary(DocId), [list_to_binary(Rev)]}
    end.

