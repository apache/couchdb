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

-export([handle_db_changes_req/2,
         handle_changes_req/4,
         parse_changes_query/2]).

-include_lib("couch/include/couch_db.hrl").

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

handle_changes_req1(Req, #db{name=DbName}=Db, ChangesArgs, ChangesFun) ->
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

parse_view_param({json_req, {Props}}) ->
    {Query} = couch_util:get_value(<<"query">>, Props),
    parse_view_param1(couch_util:get_value(<<"view">>, Query, <<"">>));
parse_view_param(Req) ->
    parse_view_param1(list_to_binary(couch_httpd:qs_value(Req, "view", ""))).

parse_view_param1(ViewParam) ->
    case re:split(ViewParam, <<"/">>) of
        [DName, ViewName] ->
            {<< "_design/", DName/binary >>, ViewName};
        _ ->
            throw({bad_request, "Invalid `view` parameter."})
    end.

parse_view_options([], Acc) ->
    Acc;
parse_view_options([{K, V} | Rest], Acc) ->
    Acc1 = case couch_util:to_binary(K) of
        <<"reduce">> ->
            [{reduce, couch_mrview_http:parse_boolean(V)}];
        <<"key">> ->
            V1 = parse_json(V),
            [{start_key, V1}, {end_key, V1} | Acc];
        <<"keys">> ->
            [{keys, parse_json(V)} | Acc];
        <<"startkey">> ->
            [{start_key, parse_json(V)} | Acc];
        <<"start_key">> ->
            [{start_key, parse_json(V)} | Acc];
        <<"startkey_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"start_key_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"endkey">> ->
            [{end_key, parse_json(V)} | Acc];
        <<"end_key">> ->
            [{end_key, parse_json(V)} | Acc];
        <<"endkey_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"end_key_docid">> ->
            [{start_key_docid, couch_util:to_binary(V)} | Acc];
        <<"limit">> ->
            [{limit, couch_mrview_http:parse_pos_int(V)} | Acc];
        <<"count">> ->
            throw({query_parse_error, <<"QS param `count` is not `limit`">>});
        <<"stale">> when V =:= <<"ok">> orelse V =:= "ok" ->
            [{stale, ok} | Acc];
        <<"stale">> when V =:= <<"update_after">> orelse V =:= "update_after" ->
            [{stale, update_after} | Acc];
        <<"stale">> ->
            throw({query_parse_error, <<"Invalid value for `stale`.">>});
        <<"descending">> ->
            case couch_mrview_http:parse_boolean(V) of
                true ->
                    [{direction, rev} | Acc];
                _ ->
                    [{direction, fwd} | Acc]
            end;
        <<"skip">> ->
            [{skip, couch_mrview_http:parse_pos_int(V)} | Acc];
        <<"group">> ->
            case couch_mrview_http:parse_booolean(V) of
                true ->
                    [{group_level, exact} | Acc];
                _ ->
                    [{group_level, 0} | Acc]
            end;
        <<"group_level">> ->
            [{group_level, couch_mrview_http:parse_pos_int(V)} | Acc];
        <<"inclusive_end">> ->
            [{inclusive_end, couch_mrview_http:parse_boolean(V)}];
        _ ->
            Acc
    end,
    parse_view_options(Rest, Acc1).

parse_json(V) when is_list(V) ->
    ?JSON_DECODE(V);
parse_json(V) ->
    V.
