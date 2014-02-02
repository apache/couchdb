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

-module(couch_httpd_changes).

-export([handle_changes_req/2]).

-include_lib("couch/include/couch_db.hrl").

handle_changes_req(#httpd{method='POST'}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    handle_changes_req1(Req, Db);
handle_changes_req(#httpd{method='GET'}=Req, Db) ->
    handle_changes_req1(Req, Db);
handle_changes_req(#httpd{path_parts=[_,<<"_changes">>]}=Req, _Db) ->
    couch_httpd:send_method_not_allowed(Req, "GET,HEAD,POST").

handle_changes_req1(Req, #db{name=DbName}=Db) ->
    AuthDbName = ?l2b(couch_config:get("couch_httpd_auth", "authentication_db")),
    case AuthDbName of
    DbName ->
        % in the authentication database, _changes is admin-only.
        ok = couch_db:check_is_admin(Db);
    _Else ->
        % on other databases, _changes is free for all.
        ok
    end,
    handle_changes_req2(Req, Db).

handle_changes_req2(Req, Db) ->
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
        (timeout, _) ->
            couch_httpd:send_chunk(Resp, "\n")
        end
    end,
    ChangesArgs = parse_changes_query(Req, Db),
    ChangesFun = couch_changes:handle_changes(ChangesArgs, Req, Db),
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
    couch_stats_collector:increment(
        {httpd, clients_requesting_changes}
    ),
    try
        WrapperFun(ChangesFun)
    after
    couch_stats_collector:decrement(
        {httpd, clients_requesting_changes}
    )
    end.

parse_changes_query(Req, Db) ->
    ChangesArgs = lists:foldl(fun({Key, Value}, Args) ->
        case {string:to_lower(Key), Value} of
        {"feed", _} ->
            Args#changes_args{feed=Value};
        {"descending", "true"} ->
            Args#changes_args{dir=rev};
        {"since", "now"} ->
            UpdateSeq = couch_util:with_db(Db#db.name, fun(WDb) ->
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
