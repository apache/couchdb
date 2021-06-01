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

-module(chttpd_show).

-export([handle_doc_update_req/3]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_views/include/couch_views.hrl").

maybe_open_doc(Db, DocId, Options) ->
    case fabric:open_doc(Db, DocId, Options) of
        {ok, Doc} ->
            chttpd_stats:incr_reads(),
            Doc;
        {not_found, _} ->
            nil
    end.

% /db/_design/foo/update/bar/docid
% updates a doc based on a request
% handle_doc_update_req(#httpd{method = 'GET'}=Req, _Db, _DDoc) ->
%     % anything but GET
%     send_method_not_allowed(Req, "POST,PUT,DELETE,ETC");

handle_doc_update_req(
    #httpd{
        path_parts = [_, _, _, _, UpdateName]
    } = Req,
    Db,
    DDoc
) ->
    send_doc_update_response(Req, Db, DDoc, UpdateName, nil, null);
handle_doc_update_req(
    #httpd{
        path_parts = [_, _, _, _, UpdateName | DocIdParts]
    } = Req,
    Db,
    DDoc
) ->
    DocId = ?l2b(string:join([?b2l(P) || P <- DocIdParts], "/")),
    Options = [conflicts, {user_ctx, Req#httpd.user_ctx}],
    Doc = maybe_open_doc(Db, DocId, Options),
    send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId);
handle_doc_update_req(Req, _Db, _DDoc) ->
    chttpd:send_error(Req, 404, <<"update_error">>, <<"Invalid path.">>).

send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId) ->
    %% Will throw an exception if the _update handler is missing
    couch_util:get_nested_json_value(DDoc#doc.body, [<<"updates">>, UpdateName]),
    JsonReq = chttpd_external:json_req_obj(Req, Db, DocId),
    JsonDoc = couch_query_servers:json_doc(Doc),
    Cmd = [<<"updates">>, UpdateName],
    UpdateResp = couch_query_servers:ddoc_prompt(DDoc, Cmd, [JsonDoc, JsonReq]),
    JsonResp =
        case UpdateResp of
            [<<"up">>, {NewJsonDoc}, {JsonResp0}] ->
                case chttpd:header_value(Req, "X-Couch-Full-Commit", "false") of
                    "true" ->
                        Options = [full_commit, {user_ctx, Req#httpd.user_ctx}];
                    _ ->
                        Options = [{user_ctx, Req#httpd.user_ctx}]
                end,
                NewDoc = couch_db:doc_from_json_obj_validate(Db, {NewJsonDoc}),
                fabric2_db:validate_docid(NewDoc#doc.id),
                {UpdateResult, NewRev} = fabric:update_doc(Db, NewDoc, Options),
                chttpd_stats:incr_writes(),
                NewRevStr = couch_doc:rev_to_str(NewRev),
                case {UpdateResult, NewRev} of
                    {ok, _} ->
                        Code = 201;
                    {accepted, _} ->
                        Code = 202
                end,
                {JsonResp1} = apply_headers(JsonResp0, [
                    {<<"X-Couch-Update-NewRev">>, NewRevStr},
                    {<<"X-Couch-Id">>, couch_util:url_encode(NewDoc#doc.id)}
                ]),
                {[{<<"code">>, Code} | JsonResp1]};
            [<<"up">>, _Other, {JsonResp0}] ->
                {[{<<"code">>, 200} | JsonResp0]}
        end,
    % todo set location field
    chttpd_external:send_external_response(Req, JsonResp).

% Maybe this is in the proplists API
% todo move to couch_util
json_apply_field(H, {L}) ->
    json_apply_field(H, L, []).
json_apply_field({Key, NewValue}, [{Key, _OldVal} | Headers], Acc) ->
    % drop matching keys
    json_apply_field({Key, NewValue}, Headers, Acc);
json_apply_field({Key, NewValue}, [{OtherKey, OtherVal} | Headers], Acc) ->
    % something else is next, leave it alone.
    json_apply_field({Key, NewValue}, Headers, [{OtherKey, OtherVal} | Acc]);
json_apply_field({Key, NewValue}, [], Acc) ->
    % end of list, add ours
    {[{Key, NewValue} | Acc]}.

apply_headers(JsonResp, []) ->
    JsonResp;
apply_headers(JsonResp, NewHeaders) ->
    case couch_util:get_value(<<"headers">>, JsonResp) of
        undefined ->
            {[{<<"headers">>, {NewHeaders}} | JsonResp]};
        JsonHeaders ->
            Headers = apply_headers1(JsonHeaders, NewHeaders),
            NewKV = {<<"headers">>, Headers},
            {lists:keyreplace(<<"headers">>, 1, JsonResp, NewKV)}
    end.
apply_headers1(JsonHeaders, [{Key, Value} | Rest]) ->
    NewJsonHeaders = json_apply_field({Key, Value}, JsonHeaders),
    apply_headers1(NewJsonHeaders, Rest);
apply_headers1(JsonHeaders, []) ->
    JsonHeaders.
