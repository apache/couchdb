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

-export([handle_changes_req/2,
         handle_changes/3,
         handle_view_changes/3]).

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
    ChangesFun = handle_changes(ChangesArgs, Req, Db),
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


handle_changes(ChangesArgs, Req, Db) ->
    case ChangesArgs#changes_args.filter of
        "_view" ->
            handle_view_changes(ChangesArgs, Req, Db);
        _ ->
            couch_changes:handle_changes(ChangesArgs, Req, Db)
    end.

%% wrapper around couch_mrview_changes.
%% This wrapper mimic couch_changes:handle_changes/3 and return a
%% Changefun that can be used by the handle_changes_req function. Also
%% while couch_mrview_changes:handle_changes/6 is returning tha view
%% changes this function return docs corresponding to the changes
%% instead so it can be used to replace the _view filter.
handle_view_changes(ChangesArgs, Req, Db) ->
    %% parse view parameter
    {DDocId, VName} = parse_view_param(Req),

    %% get view options
    Query = case Req of
        {json_req, {Props}} ->
            {Q} = couch_util:get_value(<<"query">>, Props, {[]}),
            Q;
        _ ->
            couch_httpd:qs(Req)
    end,
    ViewOptions = parse_view_options(Query, []),

    {ok, Infos} = couch_mrview:get_info(Db, DDocId),
    case lists:member(<<"seq_indexed">>,
                      proplists:get_value(update_options, Infos, [])) of
        true ->
            handle_view_changes(Db, DDocId, VName, ViewOptions, ChangesArgs,
                                Req);
        false when ViewOptions /= [] ->
            ?LOG_ERROR("Tried to filter a non sequence indexed view~n",[]),
            throw({bad_request, seqs_not_indexed});
        false ->
            %% old method we are getting changes using the btree instead
            %% which is not efficient, log it
            ?LOG_WARN("Get view changes with seq_indexed=false.~n", []),
            couch_changes:handle_changes(ChangesArgs, Req, Db)
    end.

handle_view_changes(#db{name=DbName}=Db0, DDocId, VName, ViewOptions,
                    ChangesArgs, Req) ->
    #changes_args{
        feed = ResponseType,
        since = Since,
        db_open_options = DbOptions} = ChangesArgs,

    Options0 = [{since, Since},
                {view_options, ViewOptions}],
    Options = case ResponseType of
        "continuous" -> [stream | Options0];
        "eventsource" -> [stream | Options0];
        "longpoll" -> [{stream, once} | Options0];
        _ -> Options0
    end,

    %% reopen the db with the db options given to the changes args
    couch_db:close(Db0),
    DbOptions1 = [{user_ctx, Db0#db.user_ctx} | DbOptions],
    {ok, Db} = couch_db:open(DbName, DbOptions1),


    %% initialise the changes fun
    ChangesFun = fun(Callback) ->
            Callback(start, ResponseType),

            Acc0 = {"", 0, Db, Callback, ChangesArgs},
            couch_mrview_changes:handle_changes(DbName, DDocId, VName,
                                               fun view_changes_cb/2,
                                               Acc0, Options)
    end,
    ChangesFun.


view_changes_cb(stop, {LastSeq, {_, _, _, Callback, Args}}) ->
    Callback({stop, LastSeq}, Args#changes_args.feed);

view_changes_cb(heartbeat, {_, _, _, Callback, Args}=Acc) ->
    Callback(timeout, Args#changes_args.feed),
    {ok, Acc};
view_changes_cb({{Seq, _Key, DocId}, Val},
                {Prepend, OldLimit, Db0, Callback, Args}=Acc) ->

    %% is the key removed from the index?
    Removed = case Val of
        {[{<<"_removed">>, true}]} -> true;
        _ -> false
    end,

    #changes_args{
        feed = ResponseType,
        limit = Limit} = Args,

    %% if the doc sequence is > to the one in the db record, reopen the
    %% database since it means we don't have the latest db value.
    Db = case Db0#db.update_seq >= Seq of
        true -> Db0;
        false ->
            {ok, Db1} = couch_db:reopen_db(Db0),
            Db1
    end,

    case couch_db:get_doc_info(Db, DocId) of
        {ok, DocInfo} ->
            %% get change row
            {Deleted, ChangeRow} = view_change_row(Db, DocInfo, Args),

            case Removed of
                true when Deleted /= true ->
                    %% the key has been removed from the view but the
                    %% document hasn't been deleted so ignore it.
                    {ok, Acc};
                _ ->
                    %% emit change row
                    Callback({change, ChangeRow, Prepend}, ResponseType),

                    %% if we achieved the limit, stop here, else continue.
                    NewLimit = OldLimit + 1,
                    if Limit > NewLimit ->
                            {ok, {<<",\n">>, NewLimit, Db, Callback, Args}};
                        true ->
                            {stop, {<<"">>, NewLimit, Db, Callback, Args}}
                    end
            end;
        {error, not_found} ->
            %% doc not found, continue
            {ok, Acc};
        Error ->
            throw(Error)
    end.


view_change_row(Db, DocInfo, Args) ->
    #doc_info{id = Id, high_seq = Seq, revs = Revs} = DocInfo,
    [#rev_info{rev=Rev, deleted=Del} | _] = Revs,

    #changes_args{style=Style,
                  include_docs=InDoc,
                  doc_options = DocOpts,
                  conflicts=Conflicts}=Args,

    Changes = case Style of
        main_only ->
            [{[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}];
        all_docs ->
            [{[{<<"rev">>, couch_doc:rev_to_str(R)}]}
                || #rev_info{rev=R} <- Revs]
    end,

    {Del, {[{<<"seq">>, Seq}, {<<"id">>, Id}, {<<"changes">>, Changes}] ++
     deleted_item(Del) ++ case InDoc of
            true ->
                Opts = case Conflicts of
                    true -> [deleted, conflicts];
                    false -> [deleted]
                end,
                Doc = couch_index_util:load_doc(Db, DocInfo, Opts),
                case Doc of
                    null ->
                        [{doc, null}];
                    _ ->
                        [{doc, couch_doc:to_json_obj(Doc, DocOpts)}]
                end;
            false ->
                []
    end}}.

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

deleted_item(true) -> [{<<"deleted">>, true}];
deleted_item(_) -> [].
