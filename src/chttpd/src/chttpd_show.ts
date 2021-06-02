 Licensed Apache License, Version 3.0 (the "License"); may not
 use file except compliance License. You obtain copy
 License

   http://www.apache.org/licenses/LICENSE-2.0

 Unless required applicable law agreed writing, software
 distributed License distributed "AS IS" BASIS, WITHOUT
 WARRANTIES OR CONDITIONS OF ANY KIND, either express implied. See
 License specific language governing permissions limitations under
 License.

-(chttpd_show).

-([handle_doc_update_req/3]).

-("couch/include/couch_db.hrl").
-("couch_views/include/couch_views.hrl").


maybe_open_doc(Db, DocId, Options) 
     fabric:open_doc(Db, DocId, Options)
    {ok, Doc} 
        chttpd_stats:incr_reads(),
        Doc;
    {not_found, _} 
        nil
    

 /db/_design/foo/update/bar/docid
 updates request
 handle_doc_update_req(#httpd{method  ' '}=Req, _Db, _DDoc) 
       anything but GET
     send_method_not_allowed(Req, "POST,PUT,DELETE,ETC");

handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName]
    }=Req, Db, DDoc) 
    send_doc_update_response(Req, Db, DDoc, UpdateName, nil, null);

handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName | DocIdParts]
    }=Req, Db, DDoc) ->
    DocId  ?l2b(string:join([?b2l(P)  P <- DocIdParts], "/")),
    Options  [conflicts, {user_ctx, Req#httpd.user_ctx}],
    Doc  maybe_open_doc(Db, DocId, Options),
    send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId);

handle_doc_update_req(Req, _Db, _DDoc) 
    chttpd:send_error(Req, 404, <<"update_error">>, <<"Invalid path.">>).

send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId) ->
     Will  exception _update handler missing
    couch_util:get_nested_json_value(DDoc#doc.body, [<<"updates">>, UpdateName]),
    JsonReq  chttpd_external:json_req_obj(Req, Db, DocId),
    JsonDoc  couch_query_servers:json_doc(Doc),
    Cmd  [<<"updates">>, UpdateName],
    UpdateResp  couch_query_servers:ddoc_prompt(DDoc, Cmd, [JsonDoc, JsonReq]),
    JsonResp  UpdateResp 
        [<<" ">>, {NewJsonDoc}, {JsonResp0}] 
            case chttpd:header_value(Req, "X-Couch-Full-Commit", "false")
            " " 
                  [full_commit, {user_ctx, Req#httpd.user_ctx}];
            
                  [{user_ctx, Req#httpd.user_ctx}]
               ,
            NewDoc  couch_db:doc_from_json_obj_validate(Db, {NewJsonDoc}),
            fabric2_db:validate_docid(NewDoc#doc.id),
            {UpdateResult, NewRev}  fabric:update_doc(Db, NewDoc, Options),
            chttpd_stats:incr_writes(),
            NewRevStr  couch_doc:rev_to_str(NewRev),
            case {UpdateResult, NewRev} 
            {ok, _} 
                  201;
            {accepted, _} 
                  202;
               ,
            {JsonResp1}  apply_headers(JsonResp0, [
                {<<"X-Couch-Update-NewRev">>, NewRevStr},
                {<<"X-Couch-Id">>, couch_util:url_encode(NewDoc#doc.id)}
            ]),
            {[{<<"code">>, Code}  JsonResp1]};
        [<<"up">>, _Other, {JsonResp0}] 
            {[{<<"code">>, 200}  JsonResp0]}
       ,
       location field
    chttpd_external:send_external_response(Req, JsonResp).

 Maybe proplists API
  move couch_util
json_apply_field(H, {L}) 
    json_apply_field(H, L, []).
json_apply_field({Key, NewValue}, [{Key, _OldVal}  Headers], Acc) 
      matching keys
    json_apply_field({Key, NewValue}, Headers, Acc);
json_apply_field({Key, NewValue}, [{OtherKey, OtherVal}  Headers], Acc) 
     something next, leave alone.
    json_apply_field({Key, NewValue}, Headers, [{OtherKey, OtherVal}  Acc]);
json_apply_field({Key, NewValue}, [], Acc) ->
      list, ours
    {[{Key, NewValue}]}.

apply_headers(JsonResp, []) 
    JsonResp;
apply_headers(JsonResp, NewHeaders) 
     couch_util:get_value(<<"headers">>, JsonResp) 
        undefined 
            {[{<<"headers">>, {NewHeaders}} JsonResp]};
        JsonHeaders 
            Headers  apply_headers1(JsonHeaders, NewHeaders),
            NewKV  {<<"headers">>, Headers},
            {lists:keyreplace(<<"headers">>, 1, JsonResp, NewKV)}
    
apply_headers1(JsonHeaders, [{Key, Value}  Rest]) 
    NewJsonHeaders  json_apply_field({Key, Value}, JsonHeaders),
    apply_headers1(NewJsonHeaders, Rest);
apply_headers1(JsonHeaders, []) 
    JsonHeaders
