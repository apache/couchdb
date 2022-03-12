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

-compile(tuple_calls).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("mem3/include/mem3.hrl").

-export([
    handle_request/1,
    handle_compact_req/2,
    handle_design_req/2,
    db_req/2,
    couch_doc_open/4,
    handle_changes_req/2,
    update_doc_result_to_json/1, update_doc_result_to_json/2,
    handle_design_info_req/3,
    handle_view_cleanup_req/2,
    update_doc/4,
    http_code_from_status/1,
    handle_partition_req/2
]).

-import(
    chttpd,
    [
        send_json/2, send_json/3, send_json/4,
        send_method_not_allowed/2,
        start_json_response/2,
        send_chunk/2,
        end_json_response/1,
        start_chunked_response/3,
        absolute_uri/2,
        send/2,
        start_response_length/4
    ]
).

-record(doc_query_args, {
    options = [],
    rev = nil,
    open_revs = [],
    update_type = interactive_edit,
    atts_since = nil
}).

% Accumulator for changes_callback function
-record(cacc, {
    etag,
    feed,
    mochi,
    prepend = "",
    responding = false,
    chunks_sent = 0,
    buffer = [],
    bufsize = 0,
    threshold
}).

-define(IS_ALL_DOCS(T),
    (T == <<"_all_docs">> orelse
        T == <<"_local_docs">> orelse
        T == <<"_design_docs">>)
).

-define(IS_MANGO(T),
    (T == <<"_index">> orelse
        T == <<"_find">> orelse
        T == <<"_explain">>)
).

% Database request handlers
handle_request(#httpd{path_parts = [DbName | RestParts], method = Method} = Req) ->
    case {Method, RestParts} of
        {'PUT', []} ->
            create_db_req(Req, DbName);
        {'DELETE', []} ->
            % if we get ?rev=... the user is using a faulty script where the
            % document id is empty by accident. Let them recover safely.
            case chttpd:qs_value(Req, "rev", false) of
                false ->
                    delete_db_req(Req, DbName);
                _Rev ->
                    throw(
                        {bad_request,
                            "You tried to DELETE a database with a ?=rev parameter. " ++
                                "Did you mean to DELETE a document instead?"}
                    )
            end;
        {_, []} ->
            do_db_req(Req, fun db_req/2);
        {_, [SecondPart | _]} ->
            Handler = chttpd_handlers:db_handler(SecondPart, fun db_req/2),
            do_db_req(Req, Handler)
    end.

handle_changes_req(#httpd{method = 'POST'} = Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    case chttpd:body_length(Req) of
        0 ->
            handle_changes_req1(Req, Db);
        _ ->
            {JsonProps} = chttpd:json_body_obj(Req),
            handle_changes_req1(Req#httpd{req_body = {JsonProps}}, Db)
    end;
handle_changes_req(#httpd{method = 'GET'} = Req, Db) ->
    handle_changes_req1(Req, Db);
handle_changes_req(#httpd{path_parts = [_, <<"_changes">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "GET,POST,HEAD").

handle_changes_req1(#httpd{} = Req, Db) ->
    #changes_args{filter = Raw, style = Style} = Args0 = parse_changes_query(Req),
    ChangesArgs = Args0#changes_args{
        filter_fun = couch_changes:configure_filter(Raw, Style, Req, Db),
        db_open_options = [{user_ctx, couch_db:get_user_ctx(Db)}]
    },
    Max = chttpd:chunked_response_buffer_size(),
    case ChangesArgs#changes_args.feed of
        "normal" ->
            T0 = os:timestamp(),
            {ok, Info} = fabric:get_db_info(Db),
            Suffix = mem3:shard_suffix(Db),
            Etag = chttpd:make_etag({Info, Suffix}),
            DeltaT = timer:now_diff(os:timestamp(), T0) / 1000,
            couch_stats:update_histogram([couchdb, dbinfo], DeltaT),
            chttpd:etag_respond(Req, Etag, fun() ->
                Acc0 = #cacc{
                    feed = normal,
                    etag = Etag,
                    mochi = Req,
                    threshold = Max
                },
                fabric:changes(Db, fun changes_callback/2, Acc0, ChangesArgs)
            end);
        Feed when Feed =:= "continuous"; Feed =:= "longpoll"; Feed =:= "eventsource" ->
            couch_stats:increment_counter([couchdb, httpd, clients_requesting_changes]),
            Acc0 = #cacc{
                feed = list_to_atom(Feed),
                mochi = Req,
                threshold = Max
            },
            try
                fabric:changes(Db, fun changes_callback/2, Acc0, ChangesArgs)
            after
                couch_stats:decrement_counter([couchdb, httpd, clients_requesting_changes])
            end;
        _ ->
            Msg = <<"Supported `feed` types: normal, continuous, live, longpoll, eventsource">>,
            throw({bad_request, Msg})
    end.

% callbacks for continuous feed (newline-delimited JSON Objects)
changes_callback(start, #cacc{feed = continuous} = Acc) ->
    {ok, Resp} = chttpd:start_delayed_json_response(Acc#cacc.mochi, 200),
    {ok, Acc#cacc{mochi = Resp, responding = true}};
changes_callback({change, Change}, #cacc{feed = continuous} = Acc) ->
    chttpd_stats:incr_rows(),
    Data = [?JSON_ENCODE(Change) | "\n"],
    Len = iolist_size(Data),
    maybe_flush_changes_feed(Acc, Data, Len);
changes_callback({stop, EndSeq, Pending}, #cacc{feed = continuous} = Acc) ->
    #cacc{mochi = Resp, buffer = Buf} = Acc,
    Row =
        {[
            {<<"last_seq">>, EndSeq},
            {<<"pending">>, Pending}
        ]},
    Data = [Buf, ?JSON_ENCODE(Row) | "\n"],
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp, Data),
    chttpd:end_delayed_json_response(Resp1);
% callbacks for eventsource feed (newline-delimited eventsource Objects)
changes_callback(start, #cacc{feed = eventsource} = Acc) ->
    #cacc{mochi = Req} = Acc,
    Headers = [
        {"Content-Type", "text/event-stream"},
        {"Cache-Control", "no-cache"}
    ],
    {ok, Resp} = chttpd:start_delayed_json_response(Req, 200, Headers),
    {ok, Acc#cacc{mochi = Resp, responding = true}};
changes_callback({change, {ChangeProp} = Change}, #cacc{feed = eventsource} = Acc) ->
    chttpd_stats:incr_rows(),
    Seq = proplists:get_value(seq, ChangeProp),
    Chunk = [
        "data: ",
        ?JSON_ENCODE(Change),
        "\n",
        "id: ",
        ?JSON_ENCODE(Seq),
        "\n\n"
    ],
    Len = iolist_size(Chunk),
    maybe_flush_changes_feed(Acc, Chunk, Len);
changes_callback(timeout, #cacc{feed = eventsource} = Acc) ->
    #cacc{mochi = Resp, chunks_sent = ChunksSet} = Acc,
    Chunk = "event: heartbeat\ndata: \n\n",
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp, Chunk),
    {ok, Acc#cacc{mochi = Resp1, chunks_sent = ChunksSet + 1}};
changes_callback({stop, _EndSeq}, #cacc{feed = eventsource} = Acc) ->
    #cacc{mochi = Resp, buffer = Buf} = Acc,
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp, Buf),
    chttpd:end_delayed_json_response(Resp1);
% callbacks for longpoll and normal (single JSON Object)
changes_callback(start, #cacc{feed = normal} = Acc) ->
    #cacc{etag = Etag, mochi = Req} = Acc,
    FirstChunk = "{\"results\":[\n",
    {ok, Resp} = chttpd:start_delayed_json_response(
        Req,
        200,
        [{"ETag", Etag}],
        FirstChunk
    ),
    {ok, Acc#cacc{mochi = Resp, responding = true}};
changes_callback(start, Acc) ->
    #cacc{mochi = Req} = Acc,
    FirstChunk = "{\"results\":[\n",
    {ok, Resp} = chttpd:start_delayed_json_response(Req, 200, [], FirstChunk),
    {ok, Acc#cacc{mochi = Resp, responding = true}};
changes_callback({change, Change}, Acc) ->
    chttpd_stats:incr_rows(),
    Data = [Acc#cacc.prepend, ?JSON_ENCODE(Change)],
    Len = iolist_size(Data),
    maybe_flush_changes_feed(Acc, Data, Len);
changes_callback({stop, EndSeq, Pending}, Acc) ->
    #cacc{buffer = Buf, mochi = Resp, threshold = Max} = Acc,
    Terminator = [
        "\n],\n\"last_seq\":",
        ?JSON_ENCODE(EndSeq),
        ",\"pending\":",
        ?JSON_ENCODE(Pending),
        "}\n"
    ],
    {ok, Resp1} = chttpd:close_delayed_json_object(Resp, Buf, Terminator, Max),
    chttpd:end_delayed_json_response(Resp1);
changes_callback(waiting_for_updates, #cacc{buffer = []} = Acc) ->
    #cacc{mochi = Resp, chunks_sent = ChunksSent} = Acc,
    case ChunksSent > 0 of
        true ->
            {ok, Acc};
        false ->
            {ok, Resp1} = chttpd:send_delayed_chunk(Resp, <<"\n">>),
            {ok, Acc#cacc{mochi = Resp1, chunks_sent = 1}}
    end;
changes_callback(waiting_for_updates, Acc) ->
    #cacc{buffer = Buf, mochi = Resp, chunks_sent = ChunksSent} = Acc,
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp, Buf),
    {ok, Acc#cacc{
        buffer = [],
        bufsize = 0,
        mochi = Resp1,
        chunks_sent = ChunksSent + 1
    }};
changes_callback(timeout, Acc) ->
    #cacc{mochi = Resp, chunks_sent = ChunksSent} = Acc,
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp, "\n"),
    {ok, Acc#cacc{mochi = Resp1, chunks_sent = ChunksSent + 1}};
changes_callback({error, Reason}, #cacc{mochi = #httpd{}} = Acc) ->
    #cacc{mochi = Req} = Acc,
    chttpd:send_error(Req, Reason);
changes_callback({error, Reason}, #cacc{feed = normal, responding = false} = Acc) ->
    #cacc{mochi = Req} = Acc,
    chttpd:send_error(Req, Reason);
changes_callback({error, Reason}, Acc) ->
    chttpd:send_delayed_error(Acc#cacc.mochi, Reason).

maybe_flush_changes_feed(#cacc{bufsize = Size, threshold = Max} = Acc, Data, Len) when
    Size > 0 andalso (Size + Len) > Max
->
    #cacc{buffer = Buffer, mochi = Resp} = Acc,
    {ok, R1} = chttpd:send_delayed_chunk(Resp, Buffer),
    {ok, Acc#cacc{prepend = ",\r\n", buffer = Data, bufsize = Len, mochi = R1}};
maybe_flush_changes_feed(Acc0, Data, Len) ->
    #cacc{buffer = Buf, bufsize = Size, chunks_sent = ChunksSent} = Acc0,
    Acc = Acc0#cacc{
        prepend = ",\r\n",
        buffer = [Buf | Data],
        bufsize = Size + Len,
        chunks_sent = ChunksSent + 1
    },
    {ok, Acc}.

handle_compact_req(#httpd{method = 'POST'} = Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    case Req#httpd.path_parts of
        [_DbName, <<"_compact">>] ->
            ok = fabric:compact(Db),
            send_json(Req, 202, {[{ok, true}]});
        [DbName, <<"_compact">>, DesignName | _] ->
            case ddoc_cache:open(DbName, <<"_design/", DesignName/binary>>) of
                {ok, _DDoc} ->
                    ok = fabric:compact(Db, DesignName),
                    send_json(Req, 202, {[{ok, true}]});
                Error ->
                    throw(Error)
            end
    end;
handle_compact_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

handle_view_cleanup_req(Req, Db) ->
    ok = fabric:cleanup_index_files_all_nodes(Db),
    send_json(Req, 202, {[{ok, true}]}).

handle_partition_req(#httpd{path_parts = [_, _]} = _Req, _Db) ->
    throw({bad_request, invalid_partition_req});
handle_partition_req(#httpd{method = 'GET', path_parts = [_, _, PartId]} = Req, Db) ->
    couch_partition:validate_partition(PartId),
    case couch_db:is_partitioned(Db) of
        true ->
            {ok, PartitionInfo} = fabric:get_partition_info(Db, PartId),
            send_json(Req, {PartitionInfo});
        false ->
            throw({bad_request, <<"database is not partitioned">>})
    end;
handle_partition_req(
    #httpd{
        method = 'POST',
        path_parts = [_, <<"_partition">>, <<"_", _/binary>>]
    },
    _Db
) ->
    Msg = <<"Partition must not start with an underscore">>,
    throw({illegal_partition, Msg});
handle_partition_req(#httpd{path_parts = [_, _, _]} = Req, _Db) ->
    send_method_not_allowed(Req, "GET");
handle_partition_req(#httpd{path_parts = [DbName, _, PartId | Rest]} = Req, Db) ->
    case couch_db:is_partitioned(Db) of
        true ->
            couch_partition:validate_partition(PartId),
            QS = chttpd:qs(Req),
            PartIdStr = ?b2l(PartId),
            QSPartIdStr = couch_util:get_value("partition", QS, PartIdStr),
            if
                QSPartIdStr == PartIdStr ->
                    ok;
                true ->
                    Msg = <<"Conflicting value for `partition` in query string">>,
                    throw({bad_request, Msg})
            end,
            NewQS = lists:ukeysort(1, [{"partition", PartIdStr} | QS]),
            NewReq = Req#httpd{
                path_parts = [DbName | Rest],
                qs = NewQS
            },
            update_partition_stats(Rest),
            case Rest of
                [OP | _] when OP == <<"_all_docs">> orelse ?IS_MANGO(OP) ->
                    case chttpd_handlers:db_handler(OP, fun db_req/2) of
                        Handler when is_function(Handler, 2) ->
                            Handler(NewReq, Db);
                        _ ->
                            chttpd:send_error(Req, not_found)
                    end;
                [<<"_design">>, _Name, <<"_", _/binary>> | _] ->
                    handle_design_req(NewReq, Db);
                _ ->
                    chttpd:send_error(Req, not_found)
            end;
        false ->
            throw({bad_request, <<"database is not partitioned">>})
    end;
handle_partition_req(Req, _Db) ->
    chttpd:send_error(Req, not_found).

update_partition_stats(PathParts) ->
    case PathParts of
        [<<"_design">> | _] ->
            couch_stats:increment_counter([couchdb, httpd, partition_view_requests]);
        [<<"_all_docs">> | _] ->
            couch_stats:increment_counter([couchdb, httpd, partition_all_docs_requests]);
        [<<"_find">> | _] ->
            couch_stats:increment_counter([couchdb, httpd, partition_find_requests]);
        [<<"_explain">> | _] ->
            couch_stats:increment_counter([couchdb, httpd, partition_explain_requests]);
        _ ->
            % ignore path that do not match
            ok
    end.

handle_design_req(
    #httpd{
        path_parts = [_DbName, _Design, Name, <<"_", _/binary>> = Action | _Rest]
    } = Req,
    Db
) ->
    DbName = mem3:dbname(couch_db:name(Db)),
    case ddoc_cache:open(DbName, <<"_design/", Name/binary>>) of
        {ok, DDoc} ->
            Handler = chttpd_handlers:design_handler(Action, fun bad_action_req/3),
            Handler(Req, Db, DDoc);
        Error ->
            throw(Error)
    end;
handle_design_req(Req, Db) ->
    db_req(Req, Db).

bad_action_req(#httpd{path_parts = [_, _, Name | FileNameParts]} = Req, Db, _DDoc) ->
    db_attachment_req(Req, Db, <<"_design/", Name/binary>>, FileNameParts).

handle_design_info_req(#httpd{method = 'GET'} = Req, Db, #doc{} = DDoc) ->
    [_, _, Name, _] = Req#httpd.path_parts,
    {ok, GroupInfoList} = fabric:get_view_group_info(Db, DDoc),
    send_json(
        Req,
        200,
        {[
            {name, Name},
            {view_index, {GroupInfoList}}
        ]}
    );
handle_design_info_req(Req, _Db, _DDoc) ->
    send_method_not_allowed(Req, "GET").

create_db_req(#httpd{} = Req, DbName) ->
    couch_httpd:verify_is_server_admin(Req),
    ShardsOpt = parse_shards_opt(Req),
    EngineOpt = parse_engine_opt(Req),
    DbProps = parse_partitioned_opt(Req),
    Options = lists:append([ShardsOpt, [{props, DbProps}], EngineOpt]),
    DocUrl = absolute_uri(Req, "/" ++ couch_util:url_encode(DbName)),
    case fabric:create_db(DbName, Options) of
        ok ->
            send_json(Req, 201, [{"Location", DocUrl}], {[{ok, true}]});
        accepted ->
            send_json(Req, 202, [{"Location", DocUrl}], {[{ok, true}]});
        {error, file_exists} ->
            chttpd:send_error(Req, file_exists);
        Error ->
            throw(Error)
    end.

delete_db_req(#httpd{} = Req, DbName) ->
    couch_httpd:verify_is_server_admin(Req),
    case fabric:delete_db(DbName, []) of
        ok ->
            send_json(Req, 200, {[{ok, true}]});
        accepted ->
            send_json(Req, 202, {[{ok, true}]});
        Error ->
            throw(Error)
    end.

do_db_req(#httpd{path_parts = [DbName | _], user_ctx = Ctx} = Req, Fun) ->
    Shard = hd(mem3:shards(DbName)),
    Props = couch_util:get_value(props, Shard#shard.opts, []),
    Opts =
        case Ctx of
            undefined ->
                [{props, Props}];
            #user_ctx{} ->
                [{user_ctx, Ctx}, {props, Props}]
        end,
    {ok, Db} = couch_db:clustered_db(DbName, Opts),
    Fun(Req, Db).

db_req(#httpd{method = 'GET', path_parts = [DbName]} = Req, _Db) ->
    % measure the time required to generate the etag, see if it's worth it
    T0 = os:timestamp(),
    {ok, DbInfo} = fabric:get_db_info(DbName),
    DeltaT = timer:now_diff(os:timestamp(), T0) / 1000,
    couch_stats:update_histogram([couchdb, dbinfo], DeltaT),
    send_json(Req, {DbInfo});
db_req(#httpd{method = 'POST', path_parts = [DbName], user_ctx = Ctx} = Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),

    W = chttpd:qs_value(Req, "w", integer_to_list(mem3:quorum(Db))),
    Options = [{user_ctx, Ctx}, {w, W}],

    Doc = couch_db:doc_from_json_obj_validate(Db, chttpd:json_body(Req)),
    validate_attachment_names(Doc),
    Doc2 =
        case Doc#doc.id of
            <<"">> ->
                Doc#doc{id = couch_uuids:new(), revs = {0, []}};
            _ ->
                Doc
        end,
    DocId = Doc2#doc.id,
    case chttpd:qs_value(Req, "batch") of
        "ok" ->
            % async_batching
            spawn(fun() ->
                case catch (fabric:update_doc(Db, Doc2, Options)) of
                    {ok, _} ->
                        chttpd_stats:incr_writes(),
                        ok;
                    {accepted, _} ->
                        chttpd_stats:incr_writes(),
                        ok;
                    Error ->
                        couch_log:debug("Batch doc error (~s): ~p", [DocId, Error])
                end
            end),

            send_json(
                Req,
                202,
                [],
                {[
                    {ok, true},
                    {id, DocId}
                ]}
            );
        _Normal ->
            % normal
            DocUrl = absolute_uri(Req, [
                $/,
                couch_util:url_encode(DbName),
                $/,
                couch_util:url_encode(DocId)
            ]),
            case fabric:update_doc(Db, Doc2, Options) of
                {ok, NewRev} ->
                    chttpd_stats:incr_writes(),
                    HttpCode = 201;
                {accepted, NewRev} ->
                    chttpd_stats:incr_writes(),
                    HttpCode = 202
            end,
            send_json(
                Req,
                HttpCode,
                [{"Location", DocUrl}],
                {[
                    {ok, true},
                    {id, DocId},
                    {rev, couch_doc:rev_to_str(NewRev)}
                ]}
            )
    end;
db_req(#httpd{path_parts = [_DbName]} = Req, _Db) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST");
db_req(
    #httpd{
        method = 'POST',
        path_parts = [DbName, <<"_ensure_full_commit">>],
        user_ctx = Ctx
    } = Req,
    _Db
) ->
    chttpd:validate_ctype(Req, "application/json"),
    %% use fabric call to trigger a database_does_not_exist exception
    %% for missing databases that'd return error 404 from chttpd
    %% get_security used to prefer shards on the same node over other nodes
    fabric:get_security(DbName, [{user_ctx, Ctx}]),
    CreationTime = mem3:shard_creation_time(DbName),
    send_json(
        Req,
        201,
        {[
            {ok, true},
            {instance_start_time, CreationTime}
        ]}
    );
db_req(#httpd{path_parts = [_, <<"_ensure_full_commit">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "POST");
db_req(#httpd{method = 'POST', path_parts = [_, <<"_bulk_docs">>], user_ctx = Ctx} = Req, Db) ->
    couch_stats:increment_counter([couchdb, httpd, bulk_requests]),
    chttpd:validate_ctype(Req, "application/json"),
    {JsonProps} = chttpd:json_body_obj(Req),
    DocsArray =
        case couch_util:get_value(<<"docs">>, JsonProps) of
            undefined ->
                throw({bad_request, <<"POST body must include `docs` parameter.">>});
            DocsArray0 when not is_list(DocsArray0) ->
                throw({bad_request, <<"`docs` parameter must be an array.">>});
            DocsArray0 ->
                DocsArray0
        end,
    couch_stats:update_histogram([couchdb, httpd, bulk_docs], length(DocsArray)),
    W =
        case couch_util:get_value(<<"w">>, JsonProps) of
            Value when is_integer(Value) ->
                integer_to_list(Value);
            _ ->
                chttpd:qs_value(Req, "w", integer_to_list(mem3:quorum(Db)))
        end,
    case chttpd:header_value(Req, "X-Couch-Full-Commit") of
        "true" ->
            Options = [full_commit, {user_ctx, Ctx}, {w, W}];
        "false" ->
            Options = [delay_commit, {user_ctx, Ctx}, {w, W}];
        _ ->
            Options = [{user_ctx, Ctx}, {w, W}]
    end,
    NewEdits = couch_util:get_value(<<"new_edits">>, JsonProps, true),
    Docs = lists:map(
        fun(JsonObj) ->
            Doc = couch_db:doc_from_json_obj_validate(Db, JsonObj),
            validate_revs(Doc, NewEdits),
            validate_attachment_names(Doc),
            case Doc#doc.id of
                <<>> -> Doc#doc{id = couch_uuids:new()};
                _ -> Doc
            end
        end,
        DocsArray
    ),
    case NewEdits of
        true ->
            Options2 =
                case couch_util:get_value(<<"all_or_nothing">>, JsonProps) of
                    true -> [all_or_nothing | Options];
                    _ -> Options
                end,
            case fabric:update_docs(Db, Docs, Options2) of
                {ok, Results} ->
                    % output the results
                    chttpd_stats:incr_writes(length(Results)),
                    DocResults = lists:zipwith(
                        fun update_doc_result_to_json/2,
                        Docs,
                        Results
                    ),
                    send_json(Req, 201, DocResults);
                {accepted, Results} ->
                    % output the results
                    chttpd_stats:incr_writes(length(Results)),
                    DocResults = lists:zipwith(
                        fun update_doc_result_to_json/2,
                        Docs,
                        Results
                    ),
                    send_json(Req, 202, DocResults);
                {aborted, Errors} ->
                    ErrorsJson =
                        lists:map(fun update_doc_result_to_json/1, Errors),
                    send_json(Req, 417, ErrorsJson)
            end;
        false ->
            case fabric:update_docs(Db, Docs, [replicated_changes | Options]) of
                {ok, Errors} ->
                    chttpd_stats:incr_writes(length(Docs)),
                    ErrorsJson = lists:map(fun update_doc_result_to_json/1, Errors),
                    send_json(Req, 201, ErrorsJson);
                {accepted, Errors} ->
                    chttpd_stats:incr_writes(length(Docs)),
                    ErrorsJson = lists:map(fun update_doc_result_to_json/1, Errors),
                    send_json(Req, 202, ErrorsJson)
            end;
        _ ->
            throw({bad_request, <<"`new_edits` parameter must be a boolean.">>})
    end;
db_req(#httpd{path_parts = [_, <<"_bulk_docs">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "POST");
db_req(
    #httpd{
        method = 'POST',
        path_parts = [_, <<"_bulk_get">>],
        mochi_req = MochiReq
    } = Req,
    Db
) ->
    couch_stats:increment_counter([couchdb, httpd, bulk_requests]),
    couch_httpd:validate_ctype(Req, "application/json"),
    {JsonProps} = chttpd:json_body_obj(Req),
    case couch_util:get_value(<<"docs">>, JsonProps) of
        undefined ->
            throw({bad_request, <<"Missing JSON list of 'docs'.">>});
        Docs ->
            #doc_query_args{
                options = Options0
            } = bulk_get_parse_doc_query(Req),
            Options = [{user_ctx, Req#httpd.user_ctx} | Options0],

            AcceptJson = MochiReq:accepts_content_type("application/json"),
            AcceptMixedMp = MochiReq:accepts_content_type("multipart/mixed"),
            AcceptRelatedMp = MochiReq:accepts_content_type("multipart/related"),
            AcceptMp = not AcceptJson andalso (AcceptMixedMp orelse AcceptRelatedMp),
            case AcceptMp of
                false ->
                    {ok, Resp} = start_json_response(Req, 200),
                    send_chunk(Resp, <<"{\"results\": [">>),
                    lists:foldl(
                        fun(Doc, Sep) ->
                            {DocId, Results, Options1} = bulk_get_open_doc_revs(
                                Db,
                                Doc,
                                Options
                            ),
                            bulk_get_send_docs_json(Resp, DocId, Results, Options1, Sep),
                            <<",">>
                        end,
                        <<"">>,
                        Docs
                    ),
                    send_chunk(Resp, <<"]}">>),
                    end_json_response(Resp);
                true ->
                    OuterBoundary = bulk_get_multipart_boundary(),
                    MpType =
                        case AcceptMixedMp of
                            true ->
                                "multipart/mixed";
                            _ ->
                                "multipart/related"
                        end,
                    CType =
                        {"Content-Type",
                            MpType ++ "; boundary=\"" ++
                                ?b2l(OuterBoundary) ++ "\""},
                    {ok, Resp} = start_chunked_response(Req, 200, [CType]),
                    lists:foldl(
                        fun(Doc, _Pre) ->
                            case bulk_get_open_doc_revs(Db, Doc, Options) of
                                {_, {ok, []}, _Options1} ->
                                    ok;
                                {_, {ok, Results}, Options1} ->
                                    send_docs_multipart_bulk_get(
                                        Results,
                                        Options1,
                                        OuterBoundary,
                                        Resp
                                    );
                                {DocId, {error, {RevId, Error, Reason}}, _Options1} ->
                                    Json = ?JSON_ENCODE(
                                        {[
                                            {<<"id">>, DocId},
                                            {<<"rev">>, RevId},
                                            {<<"error">>, Error},
                                            {<<"reason">>, Reason}
                                        ]}
                                    ),
                                    couch_httpd:send_chunk(Resp, [
                                        <<"\r\n--", OuterBoundary/binary>>,
                                        <<"\r\nContent-Type: application/json; error=\"true\"\r\n\r\n">>,
                                        Json
                                    ])
                            end
                        end,
                        <<"">>,
                        Docs
                    ),
                    case Docs of
                        [] ->
                            ok;
                        _ ->
                            couch_httpd:send_chunk(
                                Resp, <<"\r\n", "--", OuterBoundary/binary, "--\r\n">>
                            )
                    end,
                    couch_httpd:last_chunk(Resp)
            end
    end;
db_req(#httpd{path_parts = [_, <<"_bulk_get">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "POST");
db_req(#httpd{method = 'POST', path_parts = [_, <<"_purge">>]} = Req, Db) ->
    couch_stats:increment_counter([couchdb, httpd, purge_requests]),
    chttpd:validate_ctype(Req, "application/json"),
    W = chttpd:qs_value(Req, "w", integer_to_list(mem3:quorum(Db))),
    Options = [{user_ctx, Req#httpd.user_ctx}, {w, W}],
    {IdsRevs} = chttpd:json_body_obj(Req),
    IdsRevs2 = [{Id, couch_doc:parse_revs(Revs)} || {Id, Revs} <- IdsRevs],
    MaxIds = config:get_integer("purge", "max_document_id_number", 100),
    case length(IdsRevs2) =< MaxIds of
        false -> throw({bad_request, "Exceeded maximum number of documents."});
        true -> ok
    end,
    RevsLen = lists:foldl(
        fun({_Id, Revs}, Acc) ->
            length(Revs) + Acc
        end,
        0,
        IdsRevs2
    ),
    MaxRevs = config:get_integer("purge", "max_revisions_number", 1000),
    case RevsLen =< MaxRevs of
        false -> throw({bad_request, "Exceeded maximum number of revisions."});
        true -> ok
    end,
    couch_stats:increment_counter([couchdb, document_purges, total], length(IdsRevs2)),
    Results2 =
        case fabric:purge_docs(Db, IdsRevs2, Options) of
            {ok, Results} ->
                chttpd_stats:incr_writes(length(Results)),
                Results;
            {accepted, Results} ->
                chttpd_stats:incr_writes(length(Results)),
                Results
        end,
    {Code, Json} = purge_results_to_json(IdsRevs2, Results2),
    send_json(Req, Code, {[{<<"purge_seq">>, null}, {<<"purged">>, {Json}}]});
db_req(#httpd{path_parts = [_, <<"_purge">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "POST");
db_req(#httpd{method = 'GET', path_parts = [_, OP]} = Req, Db) when ?IS_ALL_DOCS(OP) ->
    case chttpd:qs_json_value(Req, "keys", nil) of
        Keys when is_list(Keys) ->
            all_docs_view(Req, Db, Keys, OP);
        nil ->
            all_docs_view(Req, Db, undefined, OP);
        _ ->
            throw({bad_request, "`keys` parameter must be an array."})
    end;
db_req(
    #httpd{
        method = 'POST',
        path_parts = [_, OP, <<"queries">>]
    } = Req,
    Db
) when ?IS_ALL_DOCS(OP) ->
    Props = chttpd:json_body_obj(Req),
    case couch_mrview_util:get_view_queries(Props) of
        undefined ->
            throw({bad_request, <<"POST body must include `queries` parameter.">>});
        Queries ->
            multi_all_docs_view(Req, Db, OP, Queries)
    end;
db_req(
    #httpd{path_parts = [_, OP, <<"queries">>]} = Req,
    _Db
) when ?IS_ALL_DOCS(OP) ->
    send_method_not_allowed(Req, "POST");
db_req(#httpd{method = 'POST', path_parts = [_, OP]} = Req, Db) when ?IS_ALL_DOCS(OP) ->
    chttpd:validate_ctype(Req, "application/json"),
    {Fields} = chttpd:json_body_obj(Req),
    case couch_util:get_value(<<"keys">>, Fields, nil) of
        Keys when is_list(Keys) ->
            all_docs_view(Req, Db, Keys, OP);
        nil ->
            all_docs_view(Req, Db, undefined, OP);
        _ ->
            throw({bad_request, "`keys` body member must be an array."})
    end;
db_req(#httpd{path_parts = [_, OP]} = Req, _Db) when ?IS_ALL_DOCS(OP) ->
    send_method_not_allowed(Req, "GET,HEAD,POST");
db_req(#httpd{method = 'POST', path_parts = [_, <<"_missing_revs">>]} = Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    {JsonDocIdRevs} = chttpd:json_body_obj(Req),
    case fabric:get_missing_revs(Db, JsonDocIdRevs) of
        {error, Reason} ->
            chttpd:send_error(Req, Reason);
        {ok, Results} ->
            Results2 = [
                {Id, couch_doc:revs_to_strs(Revs)}
             || {Id, Revs, _} <- Results
            ],
            send_json(
                Req,
                {[
                    {missing_revs, {Results2}}
                ]}
            )
    end;
db_req(#httpd{path_parts = [_, <<"_missing_revs">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "POST");
db_req(#httpd{method = 'POST', path_parts = [_, <<"_revs_diff">>]} = Req, Db) ->
    chttpd:validate_ctype(Req, "application/json"),
    {JsonDocIdRevs} = chttpd:json_body_obj(Req),
    case fabric:get_missing_revs(Db, JsonDocIdRevs) of
        {error, Reason} ->
            chttpd:send_error(Req, Reason);
        {ok, Results} ->
            Results2 =
                lists:map(
                    fun({Id, MissingRevs, PossibleAncestors}) ->
                        {Id, {
                            [{missing, couch_doc:revs_to_strs(MissingRevs)}] ++
                                if
                                    PossibleAncestors == [] ->
                                        [];
                                    true ->
                                        [
                                            {possible_ancestors,
                                                couch_doc:revs_to_strs(PossibleAncestors)}
                                        ]
                                end
                        }}
                    end,
                    Results
                ),
            send_json(Req, {Results2})
    end;
db_req(#httpd{path_parts = [_, <<"_revs_diff">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "POST");
db_req(
    #httpd{method = 'PUT', path_parts = [_, <<"_security">>], user_ctx = Ctx} = Req,
    Db
) ->
    DbName = ?b2l(couch_db:name(Db)),
    validate_security_can_be_edited(DbName),
    SecObj = chttpd:json_body(Req),
    case fabric:set_security(Db, SecObj, [{user_ctx, Ctx}]) of
        ok ->
            send_json(Req, {[{<<"ok">>, true}]});
        Else ->
            throw(Else)
    end;
db_req(#httpd{method = 'GET', path_parts = [_, <<"_security">>]} = Req, Db) ->
    send_json(Req, fabric:get_security(Db));
db_req(#httpd{path_parts = [_, <<"_security">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "PUT,GET");
db_req(
    #httpd{method = 'PUT', path_parts = [_, <<"_revs_limit">>], user_ctx = Ctx} = Req,
    Db
) ->
    Limit = chttpd:json_body(Req),
    ok = fabric:set_revs_limit(Db, Limit, [{user_ctx, Ctx}]),
    send_json(Req, {[{<<"ok">>, true}]});
db_req(#httpd{method = 'GET', path_parts = [_, <<"_revs_limit">>]} = Req, Db) ->
    send_json(Req, fabric:get_revs_limit(Db));
db_req(#httpd{path_parts = [_, <<"_revs_limit">>]} = Req, _Db) ->
    send_method_not_allowed(Req, "PUT,GET");
db_req(#httpd{method = 'PUT', path_parts = [_, <<"_purged_infos_limit">>]} = Req, Db) ->
    Options = [{user_ctx, Req#httpd.user_ctx}],
    case chttpd:json_body(Req) of
        Limit when is_integer(Limit), Limit > 0 ->
            case fabric:set_purge_infos_limit(Db, Limit, Options) of
                ok ->
                    send_json(Req, {[{<<"ok">>, true}]});
                Error ->
                    throw(Error)
            end;
        _ ->
            throw({bad_request, "`purge_infos_limit` must be positive integer"})
    end;
db_req(#httpd{method = 'GET', path_parts = [_, <<"_purged_infos_limit">>]} = Req, Db) ->
    send_json(Req, fabric:get_purge_infos_limit(Db));
% Special case to enable using an unencoded slash in the URL of design docs,
% as slashes in document IDs must otherwise be URL encoded.
db_req(
    #httpd{
        method = 'GET', mochi_req = MochiReq, path_parts = [_DbName, <<"_design/", _/binary>> | _]
    } = Req,
    _Db
) ->
    [Head | Tail] = re:split(MochiReq:get(raw_path), "_design%2F", [{return, list}, caseless]),
    chttpd:send_redirect(Req, Head ++ "_design/" ++ Tail);
db_req(#httpd{path_parts = [_DbName, <<"_design">>, Name]} = Req, Db) ->
    db_doc_req(Req, Db, <<"_design/", Name/binary>>);
db_req(#httpd{path_parts = [_DbName, <<"_design">>, Name | FileNameParts]} = Req, Db) ->
    db_attachment_req(Req, Db, <<"_design/", Name/binary>>, FileNameParts);
% Special case to allow for accessing local documents without %2F
% encoding the docid. Throws out requests that don't have the second
% path part or that specify an attachment name.
db_req(#httpd{path_parts = [_DbName, <<"_local">>]}, _Db) ->
    throw({bad_request, <<"Invalid _local document id.">>});
db_req(#httpd{path_parts = [_DbName, <<"_local/">>]}, _Db) ->
    throw({bad_request, <<"Invalid _local document id.">>});
db_req(#httpd{path_parts = [_DbName, <<"_local">>, Name]} = Req, Db) ->
    db_doc_req(Req, Db, <<"_local/", Name/binary>>);
db_req(#httpd{path_parts = [_DbName, <<"_local">> | _Rest]}, _Db) ->
    throw({bad_request, <<"_local documents do not accept attachments.">>});
db_req(#httpd{path_parts = [_, DocId]} = Req, Db) ->
    db_doc_req(Req, Db, DocId);
db_req(#httpd{method = 'DELETE', path_parts = [_, DocId | FileNameParts]} = Req, Db) ->
    chttpd:body(Req),
    db_attachment_req(Req, Db, DocId, FileNameParts);
db_req(#httpd{path_parts = [_, DocId | FileNameParts]} = Req, Db) ->
    db_attachment_req(Req, Db, DocId, FileNameParts).

multi_all_docs_view(Req, Db, OP, Queries) ->
    Args0 = couch_mrview_http:parse_params(Req, undefined),
    Args1 = Args0#mrargs{view_type = map},
    ArgQueries = lists:map(
        fun({Query}) ->
            QueryArg1 = couch_mrview_http:parse_params(
                Query,
                undefined,
                Args1,
                [decoded]
            ),
            QueryArgs2 = fabric_util:validate_all_docs_args(Db, QueryArg1),
            set_namespace(OP, QueryArgs2)
        end,
        Queries
    ),
    Options = [{user_ctx, Req#httpd.user_ctx}],
    VAcc0 = #vacc{db = Db, req = Req, prepend = "\r\n"},
    FirstChunk = "{\"results\":[",
    {ok, Resp0} = chttpd:start_delayed_json_response(
        VAcc0#vacc.req,
        200,
        [],
        FirstChunk
    ),
    VAcc1 = VAcc0#vacc{resp = Resp0},
    VAcc2 = lists:foldl(
        fun(Args, Acc0) ->
            {ok, Acc1} = fabric:all_docs(
                Db,
                Options,
                fun view_cb/2,
                Acc0,
                Args
            ),
            Acc1
        end,
        VAcc1,
        ArgQueries
    ),
    {ok, Resp1} = chttpd:send_delayed_chunk(VAcc2#vacc.resp, "\r\n]}"),
    chttpd:end_delayed_json_response(Resp1).

all_docs_view(Req, Db, Keys, OP) ->
    Args0 = couch_mrview_http:parse_body_and_query(Req, Keys),
    Args1 = Args0#mrargs{view_type = map},
    Args2 = fabric_util:validate_all_docs_args(Db, Args1),
    Args3 = set_namespace(OP, Args2),
    Options = [{user_ctx, Req#httpd.user_ctx}],
    Max = chttpd:chunked_response_buffer_size(),
    VAcc = #vacc{db = Db, req = Req, threshold = Max},
    {ok, Resp} = fabric:all_docs(Db, Options, fun view_cb/2, VAcc, Args3),
    {ok, Resp#vacc.resp}.

view_cb({row, Row} = Msg, Acc) ->
    case lists:keymember(doc, 1, Row) of
        true -> chttpd_stats:incr_reads();
        false -> ok
    end,
    chttpd_stats:incr_rows(),
    couch_mrview_http:view_cb(Msg, Acc);
view_cb(Msg, Acc) ->
    couch_mrview_http:view_cb(Msg, Acc).

db_doc_req(#httpd{method = 'DELETE'} = Req, Db, DocId) ->
    % check for the existence of the doc to handle the 404 case.
    couch_doc_open(Db, DocId, nil, []),
    case chttpd:qs_value(Req, "rev") of
        undefined ->
            Body = {[{<<"_deleted">>, true}]};
        Rev ->
            Body = {[{<<"_rev">>, ?l2b(Rev)}, {<<"_deleted">>, true}]}
    end,
    Doc = couch_doc_from_req(Req, Db, DocId, Body),
    send_updated_doc(Req, Db, DocId, Doc);
db_doc_req(#httpd{method = 'GET', mochi_req = MochiReq} = Req, Db, DocId) ->
    #doc_query_args{
        rev = Rev0,
        open_revs = Revs,
        options = Options0,
        atts_since = AttsSince
    } = parse_doc_query(Req),
    Options = [{user_ctx, Req#httpd.user_ctx} | Options0],
    case Revs of
        [] ->
            Options2 =
                if
                    AttsSince /= nil ->
                        [{atts_since, AttsSince}, attachments | Options];
                    true ->
                        Options
                end,
            Rev =
                case lists:member(latest, Options) of
                    % couch_doc_open will open the winning rev despite of a rev passed
                    % https://docs.couchdb.org/en/stable/api/document/common.html?highlight=latest#get--db-docid
                    true -> nil;
                    false -> Rev0
                end,
            Doc = couch_doc_open(Db, DocId, Rev, Options2),
            send_doc(Req, Doc, Options2);
        _ ->
            case fabric:open_revs(Db, DocId, Revs, Options) of
                {ok, []} when Revs == all ->
                    chttpd:send_error(Req, {not_found, missing});
                {ok, Results} ->
                    chttpd_stats:incr_reads(length(Results)),
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
                                    % AccSeparator now has a comma
                                    ","
                                end,
                                "",
                                Results
                            ),
                            send_chunk(Resp, "]"),
                            end_json_response(Resp);
                        true ->
                            send_docs_multipart(Req, Results, Options)
                    end;
                {error, Error} ->
                    chttpd:send_error(Req, Error)
            end
    end;
db_doc_req(#httpd{method = 'POST', user_ctx = Ctx} = Req, Db, DocId) ->
    couch_httpd:validate_referer(Req),
    couch_db:validate_docid(Db, DocId),
    chttpd:validate_ctype(Req, "multipart/form-data"),

    W = chttpd:qs_value(Req, "w", integer_to_list(mem3:quorum(Db))),
    Options = [{user_ctx, Ctx}, {w, W}],

    Form = couch_httpd:parse_form(Req),
    case proplists:is_defined("_doc", Form) of
        true ->
            Json = ?JSON_DECODE(couch_util:get_value("_doc", Form)),
            Doc = couch_doc_from_req(Req, Db, DocId, Json);
        false ->
            Rev = couch_doc:parse_rev(list_to_binary(couch_util:get_value("_rev", Form))),
            Doc =
                case fabric:open_revs(Db, DocId, [Rev], []) of
                    {ok, [{ok, Doc0}]} ->
                        chttpd_stats:incr_reads(),
                        Doc0;
                    {error, Error} ->
                        throw(Error)
                end
    end,
    UpdatedAtts = [
        couch_att:new([
            {name, validate_attachment_name(Name)},
            {type, list_to_binary(ContentType)},
            {data, Content}
        ])
     || {Name, {ContentType, _}, Content} <-
            proplists:get_all_values("_attachments", Form)
    ],
    #doc{atts = OldAtts} = Doc,
    OldAtts2 = lists:flatmap(
        fun(Att) ->
            OldName = couch_att:fetch(name, Att),
            case [1 || A <- UpdatedAtts, couch_att:fetch(name, A) == OldName] of
                % the attachment wasn't in the UpdatedAtts, return it
                [] -> [Att];
                % the attachment was in the UpdatedAtts, drop it
                _ -> []
            end
        end,
        OldAtts
    ),
    NewDoc = Doc#doc{
        atts = UpdatedAtts ++ OldAtts2
    },
    case fabric:update_doc(Db, NewDoc, Options) of
        {ok, NewRev} ->
            chttpd_stats:incr_writes(),
            HttpCode = 201;
        {accepted, NewRev} ->
            chttpd_stats:incr_writes(),
            HttpCode = 202
    end,
    send_json(
        Req,
        HttpCode,
        [{"ETag", "\"" ++ ?b2l(couch_doc:rev_to_str(NewRev)) ++ "\""}],
        {[
            {ok, true},
            {id, DocId},
            {rev, couch_doc:rev_to_str(NewRev)}
        ]}
    );
db_doc_req(#httpd{method = 'PUT', user_ctx = Ctx} = Req, Db, DocId) ->
    #doc_query_args{
        update_type = UpdateType
    } = parse_doc_query(Req),
    DbName = couch_db:name(Db),
    couch_db:validate_docid(Db, DocId),

    W = chttpd:qs_value(Req, "w", integer_to_list(mem3:quorum(Db))),
    Options = [{user_ctx, Ctx}, {w, W}],

    Loc = absolute_uri(Req, [
        $/,
        couch_util:url_encode(DbName),
        $/,
        couch_util:url_encode(DocId)
    ]),
    RespHeaders = [{"Location", Loc}],
    case couch_util:to_list(couch_httpd:header_value(Req, "Content-Type")) of
        ("multipart/related;" ++ _) = ContentType ->
            couch_httpd:check_max_request_length(Req),
            couch_httpd_multipart:num_mp_writers(mem3:n(mem3:dbname(DbName), DocId)),
            {ok, Doc0, WaitFun, Parser} = couch_doc:doc_from_multi_part_stream(
                ContentType,
                fun() -> receive_request_data(Req) end
            ),
            Doc = couch_doc_from_req(Req, Db, DocId, Doc0),
            try
                Result = send_updated_doc(Req, Db, DocId, Doc, RespHeaders, UpdateType),
                WaitFun(),
                Result
            catch
                throw:Err ->
                    % Document rejected by a validate_doc_update function.
                    couch_httpd_multipart:abort_multipart_stream(Parser),
                    throw(Err)
            end;
        _Else ->
            case chttpd:qs_value(Req, "batch") of
                "ok" ->
                    % batch
                    Doc = couch_doc_from_req(Req, Db, DocId, chttpd:json_body(Req)),

                    spawn(fun() ->
                        case catch (fabric:update_doc(Db, Doc, Options)) of
                            {ok, _} ->
                                chttpd_stats:incr_writes(),
                                ok;
                            {accepted, _} ->
                                chttpd_stats:incr_writes(),
                                ok;
                            Error ->
                                couch_log:notice("Batch doc error (~s): ~p", [DocId, Error])
                        end
                    end),
                    send_json(
                        Req,
                        202,
                        [],
                        {[
                            {ok, true},
                            {id, DocId}
                        ]}
                    );
                _Normal ->
                    % normal
                    Body = chttpd:json_body(Req),
                    Doc = couch_doc_from_req(Req, Db, DocId, Body),
                    send_updated_doc(Req, Db, DocId, Doc, RespHeaders, UpdateType)
            end
    end;
db_doc_req(#httpd{method = 'COPY', user_ctx = Ctx} = Req, Db, SourceDocId) ->
    SourceRev =
        case extract_header_rev(Req, chttpd:qs_value(Req, "rev")) of
            missing_rev -> nil;
            Rev -> Rev
        end,
    {TargetDocId0, TargetRevs} = couch_httpd_db:parse_copy_destination_header(Req),
    TargetDocId = list_to_binary(chttpd:unquote(TargetDocId0)),
    % open old doc
    Doc = couch_doc_open(Db, SourceDocId, SourceRev, []),
    % save new doc
    case
        fabric:update_doc(
            Db,
            Doc#doc{id = TargetDocId, revs = TargetRevs},
            [{user_ctx, Ctx}]
        )
    of
        {ok, NewTargetRev} ->
            chttpd_stats:incr_writes(),
            HttpCode = 201;
        {accepted, NewTargetRev} ->
            chttpd_stats:incr_writes(),
            HttpCode = 202
    end,
    % respond
    DbName = couch_db:name(Db),
    {PartRes} = update_doc_result_to_json(TargetDocId, {ok, NewTargetRev}),
    Loc = absolute_uri(
        Req, "/" ++ couch_util:url_encode(DbName) ++ "/" ++ couch_util:url_encode(TargetDocId)
    ),
    send_json(
        Req,
        HttpCode,
        [
            {"Location", Loc},
            {"ETag", "\"" ++ ?b2l(couch_doc:rev_to_str(NewTargetRev)) ++ "\""}
        ],
        {PartRes}
    );
db_doc_req(Req, _Db, _DocId) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,POST,PUT,COPY").

send_doc(Req, Doc, Options) ->
    case Doc#doc.meta of
        [] ->
            DiskEtag = couch_httpd:doc_etag(Doc),
            % output etag only when we have no meta
            chttpd:etag_respond(Req, DiskEtag, fun() ->
                send_doc_efficiently(Req, Doc, [{"ETag", DiskEtag}], Options)
            end);
        _ ->
            send_doc_efficiently(Req, Doc, [], Options)
    end.

send_doc_efficiently(Req, #doc{atts = []} = Doc, Headers, Options) ->
    send_json(Req, 200, Headers, couch_doc:to_json_obj(Doc, Options));
send_doc_efficiently(#httpd{mochi_req = MochiReq} = Req, #doc{atts = Atts} = Doc, Headers, Options) ->
    case lists:member(attachments, Options) of
        true ->
            Refs = monitor_attachments(Atts),
            try
                case MochiReq:accepts_content_type("multipart/related") of
                    false ->
                        send_json(Req, 200, Headers, couch_doc:to_json_obj(Doc, Options));
                    true ->
                        Boundary = couch_uuids:random(),
                        JsonBytes = ?JSON_ENCODE(
                            couch_doc:to_json_obj(
                                Doc,
                                [attachments, follows, att_encoding_info | Options]
                            )
                        ),
                        {ContentType, Len} = couch_doc:len_doc_to_multi_part_stream(
                            Boundary, JsonBytes, Atts, true
                        ),
                        CType = {"Content-Type", ContentType},
                        {ok, Resp} = start_response_length(Req, 200, [CType | Headers], Len),
                        couch_doc:doc_to_multi_part_stream(
                            Boundary,
                            JsonBytes,
                            Atts,
                            fun(Data) -> couch_httpd:send(Resp, Data) end,
                            true
                        )
                end
            after
                demonitor_refs(Refs)
            end;
        false ->
            send_json(Req, 200, Headers, couch_doc:to_json_obj(Doc, Options))
    end.

send_docs_multipart_bulk_get(Results, Options0, OuterBoundary, Resp) ->
    InnerBoundary = bulk_get_multipart_boundary(),
    Options = [attachments, follows, att_encoding_info | Options0],
    lists:foreach(
        fun
            ({ok, #doc{id = Id, revs = Revs, atts = Atts} = Doc}) ->
                Refs = monitor_attachments(Doc#doc.atts),
                try
                    JsonBytes = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, Options)),
                    couch_httpd:send_chunk(Resp, <<"\r\n--", OuterBoundary/binary>>),
                    case Atts of
                        [] ->
                            couch_httpd:send_chunk(
                                Resp, <<"\r\nContent-Type: application/json\r\n\r\n">>
                            );
                        _ ->
                            lists:foreach(
                                fun(Header) -> couch_httpd:send_chunk(Resp, Header) end,
                                bulk_get_multipart_headers(Revs, Id, InnerBoundary)
                            )
                    end,
                    couch_doc:doc_to_multi_part_stream(
                        InnerBoundary,
                        JsonBytes,
                        Atts,
                        fun(Data) -> couch_httpd:send_chunk(Resp, Data) end,
                        true
                    )
                after
                    demonitor_refs(Refs)
                end;
            ({{not_found, missing}, RevId}) ->
                RevStr = couch_doc:rev_to_str(RevId),
                Json = ?JSON_ENCODE(
                    {[
                        {<<"rev">>, RevStr},
                        {<<"error">>, <<"not_found">>},
                        {<<"reason">>, <<"missing">>}
                    ]}
                ),
                couch_httpd:send_chunk(
                    Resp,
                    [
                        <<"\r\n--", OuterBoundary/binary>>,
                        <<"\r\nContent-Type: application/json; error=\"true\"\r\n\r\n">>,
                        Json
                    ]
                )
        end,
        Results
    ).

send_docs_multipart(Req, Results, Options1) ->
    OuterBoundary = couch_uuids:random(),
    InnerBoundary = couch_uuids:random(),
    Options = [attachments, follows, att_encoding_info | Options1],
    CType = {"Content-Type", "multipart/mixed; boundary=\"" ++ ?b2l(OuterBoundary) ++ "\""},
    {ok, Resp} = start_chunked_response(Req, 200, [CType]),
    couch_httpd:send_chunk(Resp, <<"--", OuterBoundary/binary>>),
    lists:foreach(
        fun
            ({ok, #doc{atts = Atts} = Doc}) ->
                Refs = monitor_attachments(Doc#doc.atts),
                try
                    JsonBytes = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, Options)),
                    {ContentType, _Len} = couch_doc:len_doc_to_multi_part_stream(
                        InnerBoundary, JsonBytes, Atts, true
                    ),
                    couch_httpd:send_chunk(
                        Resp, <<"\r\nContent-Type: ", ContentType/binary, "\r\n\r\n">>
                    ),
                    couch_doc:doc_to_multi_part_stream(
                        InnerBoundary,
                        JsonBytes,
                        Atts,
                        fun(Data) -> couch_httpd:send_chunk(Resp, Data) end,
                        true
                    ),
                    couch_httpd:send_chunk(Resp, <<"\r\n--", OuterBoundary/binary>>)
                after
                    demonitor_refs(Refs)
                end;
            ({{not_found, missing}, RevId}) ->
                RevStr = couch_doc:rev_to_str(RevId),
                Json = ?JSON_ENCODE({[{<<"missing">>, RevStr}]}),
                couch_httpd:send_chunk(
                    Resp,
                    [
                        <<"\r\nContent-Type: application/json; error=\"true\"\r\n\r\n">>,
                        Json,
                        <<"\r\n--", OuterBoundary/binary>>
                    ]
                )
        end,
        Results
    ),
    couch_httpd:send_chunk(Resp, <<"--">>),
    couch_httpd:last_chunk(Resp).

bulk_get_multipart_headers({0, []}, Id, Boundary) ->
    [
        <<"\r\nX-Doc-Id: ", Id/binary>>,
        <<"\r\nContent-Type: multipart/related; boundary=", Boundary/binary, "\r\n\r\n">>
    ];
bulk_get_multipart_headers({Start, [FirstRevId | _]}, Id, Boundary) ->
    RevStr = couch_doc:rev_to_str({Start, FirstRevId}),
    [
        <<"\r\nX-Doc-Id: ", Id/binary>>,
        <<"\r\nX-Rev-Id: ", RevStr/binary>>,
        <<"\r\nContent-Type: multipart/related; boundary=", Boundary/binary, "\r\n\r\n">>
    ].

bulk_get_multipart_boundary() ->
    Unique = couch_uuids:random(),
    <<"--", Unique/binary>>.

receive_request_data(Req) ->
    receive_request_data(Req, chttpd:body_length(Req)).

receive_request_data(Req, Len) when Len == chunked ->
    Ref = make_ref(),
    ChunkFun = fun({_Length, Binary}, _State) ->
        self() ! {chunk, Ref, Binary}
    end,
    couch_httpd:recv_chunked(Req, 4096, ChunkFun, ok),
    GetChunk = fun GC() ->
        receive
            {chunk, Ref, Binary} -> {Binary, GC}
        end
    end,
    {
        receive
            {chunk, Ref, Binary} -> Binary
        end,
        GetChunk
    };
receive_request_data(Req, LenLeft) when LenLeft > 0 ->
    Len = erlang:min(4096, LenLeft),
    Data = chttpd:recv(Req, Len),
    {Data, fun() -> receive_request_data(Req, LenLeft - iolist_size(Data)) end};
receive_request_data(_Req, _) ->
    throw(<<"expected more data">>).

update_doc_result_to_json({{Id, Rev}, Error}) ->
    {_Code, Err, Msg} = chttpd:error_info(Error),
    {[
        {id, Id},
        {rev, couch_doc:rev_to_str(Rev)},
        {error, Err},
        {reason, Msg}
    ]}.

update_doc_result_to_json(#doc{id = DocId}, Result) ->
    update_doc_result_to_json(DocId, Result);
update_doc_result_to_json(DocId, {ok, NewRev}) ->
    {[{ok, true}, {id, DocId}, {rev, couch_doc:rev_to_str(NewRev)}]};
update_doc_result_to_json(DocId, {accepted, NewRev}) ->
    {[{ok, true}, {id, DocId}, {rev, couch_doc:rev_to_str(NewRev)}, {accepted, true}]};
update_doc_result_to_json(DocId, Error) ->
    {_Code, ErrorStr, Reason} = chttpd:error_info(Error),
    {[{id, DocId}, {error, ErrorStr}, {reason, Reason}]}.

purge_results_to_json([], []) ->
    {201, []};
purge_results_to_json([{DocId, _Revs} | RIn], [{ok, PRevs} | ROut]) ->
    {Code, Results} = purge_results_to_json(RIn, ROut),
    couch_stats:increment_counter([couchdb, document_purges, success]),
    {Code, [{DocId, couch_doc:revs_to_strs(PRevs)} | Results]};
purge_results_to_json([{DocId, _Revs} | RIn], [{accepted, PRevs} | ROut]) ->
    {Code, Results} = purge_results_to_json(RIn, ROut),
    couch_stats:increment_counter([couchdb, document_purges, success]),
    NewResults = [{DocId, couch_doc:revs_to_strs(PRevs)} | Results],
    {erlang:max(Code, 202), NewResults};
purge_results_to_json([{DocId, _Revs} | RIn], [Error | ROut]) ->
    {Code, Results} = purge_results_to_json(RIn, ROut),
    {NewCode, ErrorStr, Reason} = chttpd:error_info(Error),
    couch_stats:increment_counter([couchdb, document_purges, failure]),
    NewResults = [{DocId, {[{error, ErrorStr}, {reason, Reason}]}} | Results],
    {erlang:max(NewCode, Code), NewResults}.

send_updated_doc(Req, Db, DocId, Json) ->
    send_updated_doc(Req, Db, DocId, Json, []).

send_updated_doc(Req, Db, DocId, Doc, Headers) ->
    send_updated_doc(Req, Db, DocId, Doc, Headers, interactive_edit).

send_updated_doc(
    #httpd{user_ctx = Ctx} = Req,
    Db,
    DocId,
    #doc{deleted = Deleted} = Doc,
    Headers,
    UpdateType
) ->
    W = chttpd:qs_value(Req, "w", integer_to_list(mem3:quorum(Db))),
    Options =
        case couch_httpd:header_value(Req, "X-Couch-Full-Commit") of
            "true" ->
                [full_commit, UpdateType, {user_ctx, Ctx}, {w, W}];
            "false" ->
                [delay_commit, UpdateType, {user_ctx, Ctx}, {w, W}];
            _ ->
                [UpdateType, {user_ctx, Ctx}, {w, W}]
        end,
    {Status, {etag, Etag}, Body} = update_doc(
        Db,
        DocId,
        #doc{deleted = Deleted} = Doc,
        Options
    ),
    HttpCode = http_code_from_status(Status),
    ResponseHeaders = [{"ETag", Etag} | Headers],
    send_json(Req, HttpCode, ResponseHeaders, Body).

http_code_from_status(Status) ->
    case Status of
        accepted ->
            202;
        created ->
            201;
        ok ->
            200
    end.

update_doc(Db, DocId, #doc{deleted = Deleted, body = DocBody} = Doc, Options) ->
    {_, Ref} = spawn_monitor(fun() ->
        try fabric:update_doc(Db, Doc, Options) of
            Resp ->
                exit({exit_ok, Resp})
        catch
            throw:Reason ->
                exit({exit_throw, Reason});
            error:Reason ->
                exit({exit_error, Reason});
            exit:Reason ->
                exit({exit_exit, Reason})
        end
    end),
    Result =
        receive
            {'DOWN', Ref, _, _, {exit_ok, Ret}} ->
                Ret;
            {'DOWN', Ref, _, _, {exit_throw, Reason}} ->
                throw(Reason);
            {'DOWN', Ref, _, _, {exit_error, Reason}} ->
                erlang:error(Reason);
            {'DOWN', Ref, _, _, {exit_exit, Reason}} ->
                erlang:exit(Reason)
        end,

    case Result of
        {ok, NewRev} ->
            Accepted = false;
        {accepted, NewRev} ->
            Accepted = true
    end,
    Etag = couch_httpd:doc_etag(DocId, DocBody, NewRev),
    Status =
        case {Accepted, Deleted} of
            {true, _} ->
                accepted;
            {false, true} ->
                ok;
            {false, false} ->
                created
        end,
    NewRevStr = couch_doc:rev_to_str(NewRev),
    Body = {[{ok, true}, {id, DocId}, {rev, NewRevStr}]},
    {Status, {etag, Etag}, Body}.

couch_doc_from_req(Req, _Db, DocId, #doc{revs = Revs} = Doc) ->
    validate_attachment_names(Doc),
    Rev =
        case chttpd:qs_value(Req, "rev") of
            undefined ->
                undefined;
            QSRev ->
                couch_doc:parse_rev(QSRev)
        end,
    Revs2 =
        case Revs of
            {Start, [RevId | _]} ->
                if
                    Rev /= undefined andalso Rev /= {Start, RevId} ->
                        throw(
                            {bad_request,
                                "Document rev from request body and query "
                                "string have different values"}
                        );
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
    Doc#doc{id = DocId, revs = Revs2};
couch_doc_from_req(Req, Db, DocId, Json) ->
    Doc = couch_db:doc_from_json_obj_validate(Db, Json),
    couch_doc_from_req(Req, Db, DocId, Doc).

% Useful for debugging
% couch_doc_open(Db, DocId) ->
%   couch_doc_open(Db, DocId, nil, []).

couch_doc_open(Db, DocId, Rev, Options0) ->
    Options = [{user_ctx, couch_db:get_user_ctx(Db)} | Options0],
    case Rev of
        % open most recent rev
        nil ->
            case fabric:open_doc(Db, DocId, Options) of
                {ok, Doc} ->
                    chttpd_stats:incr_reads(),
                    Doc;
                Error ->
                    throw(Error)
            end;
        % open a specific rev (deletions come back as stubs)
        _ ->
            case fabric:open_revs(Db, DocId, [Rev], Options) of
                {ok, [{ok, Doc}]} ->
                    chttpd_stats:incr_reads(),
                    Doc;
                {ok, [{{not_found, missing}, Rev}]} ->
                    throw(not_found);
                {ok, [Else]} ->
                    throw(Else);
                {error, Error} ->
                    throw(Error)
            end
    end.

get_existing_attachment(Atts, FileName) ->
    % Check if attachment exists, if not throw not_found
    case [A || A <- Atts, couch_att:fetch(name, A) == FileName] of
        [] -> throw({not_found, "Document is missing attachment"});
        [Att] -> Att
    end.

% Attachment request handlers

db_attachment_req(#httpd{method = 'GET', mochi_req = MochiReq} = Req, Db, DocId, FileNameParts) ->
    FileName = list_to_binary(
        mochiweb_util:join(
            lists:map(
                fun binary_to_list/1,
                FileNameParts
            ),
            "/"
        )
    ),
    #doc_query_args{
        rev = Rev,
        options = Options
    } = parse_doc_query(Req),
    #doc{
        atts = Atts
    } = Doc = couch_doc_open(Db, DocId, Rev, Options),
    Att = get_existing_attachment(Atts, FileName),
    [Type, Enc, DiskLen, AttLen, Md5] = couch_att:fetch(
        [type, encoding, disk_len, att_len, md5], Att
    ),
    Refs = monitor_attachments(Att),
    try
        Etag =
            case Md5 of
                <<>> -> chttpd:doc_etag(Doc);
                _ -> "\"" ++ ?b2l(base64:encode(Md5)) ++ "\""
            end,
        ReqAcceptsAttEnc = lists:member(
            atom_to_list(Enc),
            couch_httpd:accepted_encodings(Req)
        ),
        Headers0 =
            [
                {"ETag", Etag},
                {"Cache-Control", "must-revalidate"},
                {"Content-Type", binary_to_list(Type)}
            ] ++
                case ReqAcceptsAttEnc of
                    true when Enc =/= identity ->
                        % RFC 2616 says that the 'identify' encoding should not be used in
                        % the Content-Encoding header
                        [{"Content-Encoding", atom_to_list(Enc)}];
                    _ ->
                        []
                end ++
                case Enc of
                    identity ->
                        [{"Accept-Ranges", "bytes"}];
                    _ ->
                        [{"Accept-Ranges", "none"}]
                end,
        Headers = chttpd_util:maybe_add_csp_header("attachments", Headers0, "sandbox"),
        Len =
            case {Enc, ReqAcceptsAttEnc} of
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
        AttFun =
            case ReqAcceptsAttEnc of
                false ->
                    fun couch_att:foldl_decode/3;
                true ->
                    fun couch_att:foldl/3
            end,
        chttpd:etag_respond(
            Req,
            Etag,
            fun() ->
                case Len of
                    undefined ->
                        {ok, Resp} = start_chunked_response(Req, 200, Headers),
                        AttFun(Att, fun(Seg, _) -> send_chunk(Resp, Seg) end, {ok, Resp}),
                        couch_httpd:last_chunk(Resp);
                    _ ->
                        Ranges = parse_ranges(MochiReq:get(range), Len),
                        case {Enc, Ranges} of
                            {identity, [{From, To}]} ->
                                Headers1 =
                                    [{"Content-Range", make_content_range(From, To, Len)}] ++
                                        Headers,
                                {ok, Resp} = start_response_length(
                                    Req, 206, Headers1, To - From + 1
                                ),
                                couch_att:range_foldl(
                                    Att,
                                    From,
                                    To + 1,
                                    fun(Seg, _) -> send(Resp, Seg) end,
                                    {ok, Resp}
                                );
                            {identity, Ranges} when is_list(Ranges) andalso length(Ranges) < 10 ->
                                send_ranges_multipart(Req, Type, Len, Att, Ranges);
                            _ ->
                                Headers1 =
                                    Headers ++
                                        if
                                            Enc =:= identity orelse ReqAcceptsAttEnc =:= true ->
                                                [
                                                    {"Content-MD5",
                                                        base64:encode(couch_att:fetch(md5, Att))}
                                                ];
                                            true ->
                                                []
                                        end,
                                {ok, Resp} = start_response_length(Req, 200, Headers1, Len),
                                AttFun(Att, fun(Seg, _) -> send(Resp, Seg) end, {ok, Resp})
                        end
                end
            end
        )
    after
        demonitor_refs(Refs)
    end;
db_attachment_req(#httpd{method = Method, user_ctx = Ctx} = Req, Db, DocId, FileNameParts) when
    (Method == 'PUT') or (Method == 'DELETE')
->
    FileName = validate_attachment_name(
        mochiweb_util:join(
            lists:map(
                fun binary_to_list/1,
                FileNameParts
            ),
            "/"
        )
    ),

    NewAtt =
        case Method of
            'DELETE' ->
                [];
            _ ->
                MimeType =
                    case couch_httpd:header_value(Req, "Content-Type") of
                        % We could throw an error here or guess by the FileName.
                        % Currently, just giving it a default.
                        undefined -> <<"application/octet-stream">>;
                        CType -> list_to_binary(CType)
                    end,
                Data = fabric:att_receiver(Req, couch_db:name(Db), chttpd:body_length(Req)),
                ContentLen =
                    case couch_httpd:header_value(Req, "Content-Length") of
                        undefined -> undefined;
                        Length -> list_to_integer(Length)
                    end,
                ContentEnc = string:to_lower(
                    string:strip(
                        couch_httpd:header_value(Req, "Content-Encoding", "identity")
                    )
                ),
                Encoding =
                    case ContentEnc of
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
                [
                    couch_att:new([
                        {name, FileName},
                        {type, MimeType},
                        {data, Data},
                        {att_len, ContentLen},
                        {md5, get_md5_header(Req)},
                        {encoding, Encoding}
                    ])
                ]
        end,

    Doc =
        case extract_header_rev(Req, chttpd:qs_value(Req, "rev")) of
            % make the new doc
            missing_rev ->
                if
                    Method =/= 'DELETE' ->
                        ok;
                    true ->
                        % check for the existence of the doc and attachment
                        CurrDoc = #doc{} = couch_doc_open(Db, DocId, nil, []),
                        get_existing_attachment(CurrDoc#doc.atts, FileName)
                end,
                couch_db:validate_docid(Db, DocId),
                #doc{id = DocId};
            Rev ->
                case fabric:open_revs(Db, DocId, [Rev], [{user_ctx, Ctx}]) of
                    {ok, [{ok, Doc0}]} ->
                        chttpd_stats:incr_reads(),
                        if
                            Method =/= 'DELETE' ->
                                ok;
                            true ->
                                % check if attachment exists
                                get_existing_attachment(Doc0#doc.atts, FileName)
                        end,
                        Doc0;
                    {ok, [Error]} ->
                        throw(Error);
                    {error, Error} ->
                        throw(Error)
                end
        end,

    #doc{atts = Atts} = Doc,
    DocEdited = Doc#doc{
        atts = NewAtt ++ [A || A <- Atts, couch_att:fetch(name, A) /= FileName]
    },
    W = chttpd:qs_value(Req, "w", integer_to_list(mem3:quorum(Db))),
    case fabric:update_doc(Db, DocEdited, [{user_ctx, Ctx}, {w, W}]) of
        {ok, UpdatedRev} ->
            chttpd_stats:incr_writes(),
            HttpCode = 201;
        {accepted, UpdatedRev} ->
            chttpd_stats:incr_writes(),
            HttpCode = 202
    end,
    erlang:put(mochiweb_request_recv, true),
    DbName = couch_db:name(Db),

    {Status, Headers} =
        case Method of
            'DELETE' ->
                {200, []};
            _ ->
                {HttpCode, [
                    {"Location",
                        absolute_uri(Req, [
                            $/,
                            DbName,
                            $/,
                            couch_util:url_encode(DocId),
                            $/,
                            couch_util:url_encode(FileName)
                        ])}
                ]}
        end,
    send_json(
        Req,
        Status,
        Headers,
        {[
            {ok, true},
            {id, DocId},
            {rev, couch_doc:rev_to_str(UpdatedRev)}
        ]}
    );
db_attachment_req(Req, _Db, _DocId, _FileNameParts) ->
    send_method_not_allowed(Req, "DELETE,GET,HEAD,PUT").

send_ranges_multipart(Req, ContentType, Len, Att, Ranges) ->
    Boundary = couch_uuids:random(),
    CType = {"Content-Type", "multipart/byteranges; boundary=\"" ++ ?b2l(Boundary) ++ "\""},
    {ok, Resp} = start_chunked_response(Req, 206, [CType]),
    couch_httpd:send_chunk(Resp, <<"--", Boundary/binary>>),
    lists:foreach(
        fun({From, To}) ->
            ContentRange = make_content_range(From, To, Len),
            couch_httpd:send_chunk(
                Resp,
                <<"\r\nContent-Type: ", ContentType/binary, "\r\n", "Content-Range: ",
                    ContentRange/binary, "\r\n", "\r\n">>
            ),
            couch_att:range_foldl(
                Att,
                From,
                To + 1,
                fun(Seg, _) -> send_chunk(Resp, Seg) end,
                {ok, Resp}
            ),
            couch_httpd:send_chunk(Resp, <<"\r\n--", Boundary/binary>>)
        end,
        Ranges
    ),
    couch_httpd:send_chunk(Resp, <<"--">>),
    couch_httpd:last_chunk(Resp),
    {ok, Resp}.

parse_ranges(undefined, _Len) ->
    undefined;
parse_ranges(fail, _Len) ->
    undefined;
parse_ranges(Ranges, Len) ->
    parse_ranges(Ranges, Len, []).

parse_ranges([], _Len, Acc) ->
    lists:reverse(Acc);
parse_ranges([{0, none} | _], _Len, _Acc) ->
    undefined;
parse_ranges([{From, To} | _], _Len, _Acc) when
    is_integer(From) andalso is_integer(To) andalso To < From
->
    throw(requested_range_not_satisfiable);
parse_ranges([{From, To} | Rest], Len, Acc) when
    is_integer(To) andalso To >= Len
->
    parse_ranges([{From, Len - 1}] ++ Rest, Len, Acc);
parse_ranges([{none, To} | Rest], Len, Acc) ->
    parse_ranges([{Len - To, Len - 1}] ++ Rest, Len, Acc);
parse_ranges([{From, none} | Rest], Len, Acc) ->
    parse_ranges([{From, Len - 1}] ++ Rest, Len, Acc);
parse_ranges([{From, To} | Rest], Len, Acc) ->
    parse_ranges(Rest, Len, [{From, To}] ++ Acc).

make_content_range(From, To, Len) ->
    ?l2b(io_lib:format("bytes ~B-~B/~B", [From, To, Len])).

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
    lists:foldl(fun parse_doc_query/2, #doc_query_args{}, chttpd:qs(Req)).

parse_shards_opt(Req) ->
    [
        {n, parse_shards_opt("n", Req, config:get_integer("cluster", "n", 3))},
        {q, parse_shards_opt("q", Req, config:get_integer("cluster", "q", 2))},
        {placement,
            parse_shards_opt(
                "placement", Req, config:get("cluster", "placement")
            )}
    ].

parse_shards_opt("placement", Req, Default) ->
    Err = <<"The `placement` value should be in a format `zone:n`.">>,
    case chttpd:qs_value(Req, "placement", Default) of
        Default ->
            Default;
        [] ->
            throw({bad_request, Err});
        Val ->
            try
                true = lists:all(
                    fun(Rule) ->
                        [_, N] = string:tokens(Rule, ":"),
                        couch_util:validate_positive_int(N)
                    end,
                    string:tokens(Val, ",")
                ),
                Val
            catch
                _:_ ->
                    throw({bad_request, Err})
            end
    end;
parse_shards_opt(Param, Req, Default) ->
    Val = chttpd:qs_value(Req, Param, Default),
    Err = ?l2b(["The `", Param, "` value should be a positive integer."]),
    case couch_util:validate_positive_int(Val) of
        true -> Val;
        false -> throw({bad_request, Err})
    end.

parse_engine_opt(Req) ->
    case chttpd:qs_value(Req, "engine") of
        undefined ->
            [];
        Extension ->
            Available = couch_server:get_engine_extensions(),
            case lists:member(Extension, Available) of
                true ->
                    [{engine, iolist_to_binary(Extension)}];
                false ->
                    throw({bad_request, invalid_engine_extension})
            end
    end.

parse_partitioned_opt(Req) ->
    case chttpd:qs_value(Req, "partitioned") of
        undefined ->
            [];
        "false" ->
            [];
        "true" ->
            ok = validate_partitioned_db_enabled(Req),
            [
                {partitioned, true},
                {hash, [couch_partition, hash, []]}
            ];
        _ ->
            throw({bad_request, <<"Invalid `partitioned` parameter">>})
    end.

validate_partitioned_db_enabled(Req) ->
    case couch_flags:is_enabled(partitioned, Req) of
        true ->
            ok;
        false ->
            throw({bad_request, <<"Partitioned feature is not enabled.">>})
    end.

parse_doc_query({Key, Value}, Args) ->
    case {Key, Value} of
        {"attachments", "true"} ->
            Options = [attachments | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"meta", "true"} ->
            Options = [revs_info, conflicts, deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"revs", "true"} ->
            Options = [revs | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"local_seq", "true"} ->
            Options = [local_seq | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"revs_info", "true"} ->
            Options = [revs_info | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"conflicts", "true"} ->
            Options = [conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"deleted", "true"} ->
            Options = [deleted | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"deleted_conflicts", "true"} ->
            Options = [deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"rev", Rev} ->
            Args#doc_query_args{rev = couch_doc:parse_rev(Rev)};
        {"open_revs", "all"} ->
            Args#doc_query_args{open_revs = all};
        {"open_revs", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{open_revs = couch_doc:parse_revs(JsonArray)};
        {"latest", "true"} ->
            Options = [latest | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"atts_since", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{atts_since = couch_doc:parse_revs(JsonArray)};
        {"new_edits", "false"} ->
            Args#doc_query_args{update_type = replicated_changes};
        {"new_edits", "true"} ->
            Args#doc_query_args{update_type = interactive_edit};
        {"att_encoding_info", "true"} ->
            Options = [att_encoding_info | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"r", R} ->
            Options = [{r, R} | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        {"w", W} ->
            Options = [{w, W} | Args#doc_query_args.options],
            Args#doc_query_args{options = Options};
        % unknown key value pair, ignore.
        _Else ->
            Args
    end.

parse_changes_query(Req) ->
    erlang:erase(changes_seq_interval),
    ChangesArgs = lists:foldl(
        fun({Key, Value}, Args) ->
            case {string:to_lower(Key), Value} of
                {"feed", "live"} ->
                    %% sugar for continuous
                    Args#changes_args{feed = "continuous"};
                {"feed", _} ->
                    Args#changes_args{feed = Value};
                {"descending", "true"} ->
                    Args#changes_args{dir = rev};
                {"since", _} ->
                    Args#changes_args{since = Value};
                {"last-event-id", _} ->
                    Args#changes_args{since = Value};
                {"limit", _} ->
                    Args#changes_args{limit = list_to_integer(Value)};
                {"style", _} ->
                    Args#changes_args{style = list_to_existing_atom(Value)};
                {"heartbeat", "true"} ->
                    Args#changes_args{heartbeat = true};
                {"heartbeat", _} ->
                    try list_to_integer(Value) of
                        HeartbeatInteger when HeartbeatInteger > 0 ->
                            Args#changes_args{heartbeat = HeartbeatInteger};
                        _ ->
                            throw(
                                {bad_request,
                                    <<"The heartbeat value should be a positive integer (in milliseconds).">>}
                            )
                    catch
                        error:badarg ->
                            throw(
                                {bad_request,
                                    <<"Invalid heartbeat value. Expecting a positive integer value (in milliseconds).">>}
                            )
                    end;
                {"timeout", _} ->
                    Args#changes_args{timeout = list_to_integer(Value)};
                {"include_docs", "true"} ->
                    Args#changes_args{include_docs = true};
                {"conflicts", "true"} ->
                    Args#changes_args{conflicts = true};
                {"attachments", "true"} ->
                    Options = [attachments | Args#changes_args.doc_options],
                    Args#changes_args{doc_options = Options};
                {"att_encoding_info", "true"} ->
                    Options = [att_encoding_info | Args#changes_args.doc_options],
                    Args#changes_args{doc_options = Options};
                {"filter", _} ->
                    Args#changes_args{filter = Value};
                {"seq_interval", _} ->
                    try list_to_integer(Value) of
                        V when V > 0 ->
                            erlang:put(changes_seq_interval, V),
                            Args;
                        _ ->
                            throw({bad_request, invalid_seq_interval})
                    catch
                        error:badarg ->
                            throw({bad_request, invalid_seq_interval})
                    end;
                % unknown key value pair, ignore.
                _Else ->
                    Args
            end
        end,
        #changes_args{},
        chttpd:qs(Req)
    ),
    %% if it's an EventSource request with a Last-event-ID header
    %% that should override the `since` query string, since it's
    %% probably the browser reconnecting.
    case ChangesArgs#changes_args.feed of
        "eventsource" ->
            case couch_httpd:header_value(Req, "last-event-id") of
                undefined ->
                    ChangesArgs;
                Value ->
                    ChangesArgs#changes_args{since = Value}
            end;
        _ ->
            ChangesArgs
    end.

extract_header_rev(Req, ExplicitRev) when is_binary(ExplicitRev) or is_list(ExplicitRev) ->
    extract_header_rev(Req, couch_doc:parse_rev(ExplicitRev));
extract_header_rev(Req, ExplicitRev) ->
    Etag =
        case chttpd:header_value(Req, "If-Match") of
            undefined -> undefined;
            Value -> couch_doc:parse_rev(string:strip(Value, both, $"))
        end,
    case {ExplicitRev, Etag} of
        {undefined, undefined} -> missing_rev;
        {_, undefined} -> ExplicitRev;
        {undefined, _} -> Etag;
        _ when ExplicitRev == Etag -> Etag;
        _ -> throw({bad_request, "Document rev and etag have different values"})
    end.

validate_security_can_be_edited(DbName) ->
    UserDbName = config:get("chttpd_auth", "authentication_db", "_users"),
    CanEditUserSecurityObject = config:get("couchdb", "users_db_security_editable", "false"),
    case {DbName, CanEditUserSecurityObject} of
        {UserDbName, "false"} ->
            Msg = "You can't edit the security object of the user database.",
            throw({forbidden, Msg});
        {_, _} ->
            ok
    end.

validate_revs(_Doc, true) ->
    ok;
validate_revs(#doc{revs = {0, []}}, false) ->
    throw(
        {bad_request,
            ?l2b(
                "When `new_edits: false`, " ++
                    "the document needs `_rev` or `_revisions` specified"
            )}
    );
validate_revs(_Doc, false) ->
    ok.

validate_attachment_names(Doc) ->
    lists:foreach(
        fun(Att) ->
            Name = couch_att:fetch(name, Att),
            validate_attachment_name(Name)
        end,
        Doc#doc.atts
    ).

validate_attachment_name(Name) when is_list(Name) ->
    validate_attachment_name(list_to_binary(Name));
validate_attachment_name(<<"_", Rest/binary>>) ->
    throw(
        {bad_request,
            <<"Attachment name '_", Rest/binary, "' starts with prohibited character '_'">>}
    );
validate_attachment_name(Name) ->
    case couch_util:validate_utf8(Name) of
        true -> Name;
        false -> throw({bad_request, <<"Attachment name is not UTF-8 encoded">>})
    end.

-spec monitor_attachments(couch_att:att() | [couch_att:att()]) -> [reference()].
monitor_attachments(Atts) when is_list(Atts) ->
    lists:foldl(
        fun(Att, Monitors) ->
            case couch_att:fetch(data, Att) of
                {Fd, _} ->
                    [monitor(process, Fd) | Monitors];
                stub ->
                    Monitors;
                Else ->
                    couch_log:error("~p from couch_att:fetch(data, ~p)", [Else, Att]),
                    Monitors
            end
        end,
        [],
        Atts
    );
monitor_attachments(Att) ->
    monitor_attachments([Att]).

demonitor_refs(Refs) when is_list(Refs) ->
    [demonitor(Ref) || Ref <- Refs].

set_namespace(<<"_all_docs">>, Args) ->
    set_namespace(undefined, Args);
set_namespace(<<"_local_docs">>, Args) ->
    set_namespace(<<"_local">>, Args);
set_namespace(<<"_design_docs">>, Args) ->
    set_namespace(<<"_design">>, Args);
set_namespace(NS, #mrargs{} = Args) ->
    couch_mrview_util:set_extra(Args, namespace, NS).

%% /db/_bulk_get stuff

bulk_get_parse_doc_query(Req) ->
    lists:foldl(
        fun({Key, Value}, Args) ->
            ok = validate_query_param(Key),
            parse_doc_query({Key, Value}, Args)
        end,
        #doc_query_args{},
        chttpd:qs(Req)
    ).

validate_query_param("open_revs" = Key) ->
    throw_bad_query_param(Key);
validate_query_param("new_edits" = Key) ->
    throw_bad_query_param(Key);
validate_query_param("w" = Key) ->
    throw_bad_query_param(Key);
validate_query_param("rev" = Key) ->
    throw_bad_query_param(Key);
validate_query_param("atts_since" = Key) ->
    throw_bad_query_param(Key);
validate_query_param(_) ->
    ok.

throw_bad_query_param(Key) when is_list(Key) ->
    throw_bad_query_param(?l2b(Key));
throw_bad_query_param(Key) when is_binary(Key) ->
    Msg = <<"\"", Key/binary, "\" query parameter is not acceptable">>,
    throw({bad_request, Msg}).

bulk_get_open_doc_revs(Db, {Props}, Options) ->
    bulk_get_open_doc_revs1(Db, Props, Options, {}).

bulk_get_open_doc_revs1(Db, Props, Options, {}) ->
    case couch_util:get_value(<<"id">>, Props) of
        undefined ->
            Error = {null, bad_request, <<"document id missed">>},
            {null, {error, Error}, Options};
        DocId ->
            try
                couch_db:validate_docid(Db, DocId),
                bulk_get_open_doc_revs1(Db, Props, Options, {DocId})
            catch
                throw:{Error, Reason} ->
                    {DocId, {error, {null, Error, Reason}}, Options}
            end
    end;
bulk_get_open_doc_revs1(Db, Props, Options, {DocId}) ->
    RevStr = couch_util:get_value(<<"rev">>, Props),

    case parse_field(<<"rev">>, RevStr) of
        {error, {RevStr, Error, Reason}} ->
            {DocId, {error, {RevStr, Error, Reason}}, Options};
        {ok, undefined} ->
            bulk_get_open_doc_revs1(Db, Props, Options, {DocId, all});
        {ok, Rev} ->
            bulk_get_open_doc_revs1(Db, Props, Options, {DocId, [Rev]})
    end;
bulk_get_open_doc_revs1(Db, Props, Options, {DocId, Revs}) ->
    AttsSinceStr = couch_util:get_value(<<"atts_since">>, Props),

    case parse_field(<<"atts_since">>, AttsSinceStr) of
        {error, {BadAttsSinceRev, Error, Reason}} ->
            {DocId, {error, {BadAttsSinceRev, Error, Reason}}, Options};
        {ok, []} ->
            bulk_get_open_doc_revs1(Db, Props, Options, {DocId, Revs, Options});
        {ok, RevList} ->
            Options1 = [{atts_since, RevList}, attachments | Options],
            bulk_get_open_doc_revs1(Db, Props, Options, {DocId, Revs, Options1})
    end;
bulk_get_open_doc_revs1(Db, Props, _, {DocId, Revs, Options}) ->
    case fabric:open_revs(Db, DocId, Revs, Options) of
        {ok, []} ->
            RevStr = couch_util:get_value(<<"rev">>, Props),
            Error = {RevStr, <<"not_found">>, <<"missing">>},
            {DocId, {error, Error}, Options};
        {ok, Resps} = Results ->
            chttpd_stats:incr_reads(length(Resps)),
            {DocId, Results, Options};
        Else ->
            {DocId, Else, Options}
    end.

parse_field(<<"rev">>, undefined) ->
    {ok, undefined};
parse_field(<<"rev">>, Value) ->
    try
        Rev = couch_doc:parse_rev(Value),
        {ok, Rev}
    catch
        throw:{bad_request = Error, Reason} ->
            {error, {Value, Error, Reason}}
    end;
parse_field(<<"atts_since">>, undefined) ->
    {ok, []};
parse_field(<<"atts_since">>, []) ->
    {ok, []};
parse_field(<<"atts_since">>, Value) when is_list(Value) ->
    parse_atts_since(Value, []);
parse_field(<<"atts_since">>, Value) ->
    {error, {Value, bad_request, <<"att_since value must be array of revs.">>}}.

parse_atts_since([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_atts_since([RevStr | Rest], Acc) ->
    case parse_field(<<"rev">>, RevStr) of
        {ok, Rev} ->
            parse_atts_since(Rest, [Rev | Acc]);
        {error, _} = Error ->
            Error
    end.

bulk_get_send_docs_json(Resp, DocId, Results, Options, Sep) ->
    Id = ?JSON_ENCODE(DocId),
    send_chunk(Resp, [Sep, <<"{\"id\": ">>, Id, <<", \"docs\": [">>]),
    bulk_get_send_docs_json1(Resp, DocId, Results, Options),
    send_chunk(Resp, <<"]}">>).

bulk_get_send_docs_json1(Resp, DocId, {error, {Rev, Error, Reason}}, _) ->
    send_chunk(Resp, [bulk_get_json_error(DocId, Rev, Error, Reason)]);
bulk_get_send_docs_json1(_Resp, _DocId, {ok, []}, _) ->
    ok;
bulk_get_send_docs_json1(Resp, DocId, {ok, Docs}, Options) ->
    lists:foldl(
        fun(Result, AccSeparator) ->
            case Result of
                {ok, Doc} ->
                    JsonDoc = couch_doc:to_json_obj(Doc, Options),
                    Json = ?JSON_ENCODE({[{ok, JsonDoc}]}),
                    send_chunk(Resp, [AccSeparator, Json]);
                {{Error, Reason}, RevId} ->
                    RevStr = couch_doc:rev_to_str(RevId),
                    Json = bulk_get_json_error(DocId, RevStr, Error, Reason),
                    send_chunk(Resp, [AccSeparator, Json])
            end,
            <<",">>
        end,
        <<"">>,
        Docs
    ).

bulk_get_json_error(DocId, Rev, Error, Reason) ->
    ?JSON_ENCODE(
        {[
            {error,
                {[
                    {<<"id">>, DocId},
                    {<<"rev">>, Rev},
                    {<<"error">>, Error},
                    {<<"reason">>, Reason}
                ]}}
        ]}
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

monitor_attachments_test_() ->
    {"ignore stubs", fun() ->
        Atts = [couch_att:new([{data, stub}])],
        ?_assertEqual([], monitor_attachments(Atts))
    end}.

parse_partitioned_opt_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_should_allow_partitioned_db(),
            t_should_throw_on_not_allowed_partitioned_db(),
            t_returns_empty_array_for_partitioned_false(),
            t_returns_empty_array_for_no_partitioned_qs()
        ]
    }.

parse_shards_opt_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_should_allow_valid_q(),
            t_should_default_on_missing_q(),
            t_should_throw_on_invalid_q(),
            t_should_allow_valid_n(),
            t_should_default_on_missing_n(),
            t_should_throw_on_invalid_n(),
            t_should_allow_valid_placement(),
            t_should_default_on_missing_placement(),
            t_should_throw_on_invalid_placement()
        ]
    }.

setup() ->
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    ok.

teardown(_) ->
    meck:unload().

mock_request(Url) ->
    Headers = mochiweb_headers:make([{"Host", "examples.com"}]),
    MochiReq = mochiweb_request:new(nil, 'PUT', Url, {1, 1}, Headers),
    #httpd{mochi_req = MochiReq}.

t_should_allow_partitioned_db() ->
    ?_test(begin
        meck:expect(couch_flags, is_enabled, 2, true),
        Req = mock_request("/all-test21?partitioned=true"),
        [Partitioned, _] = parse_partitioned_opt(Req),
        ?assertEqual(Partitioned, {partitioned, true})
    end).

t_should_throw_on_not_allowed_partitioned_db() ->
    ?_test(begin
        meck:expect(couch_flags, is_enabled, 2, false),
        Req = mock_request("/all-test21?partitioned=true"),
        Throw = {bad_request, <<"Partitioned feature is not enabled.">>},
        ?assertThrow(Throw, parse_partitioned_opt(Req))
    end).

t_returns_empty_array_for_partitioned_false() ->
    ?_test(begin
        Req = mock_request("/all-test21?partitioned=false"),
        ?assertEqual(parse_partitioned_opt(Req), [])
    end).

t_returns_empty_array_for_no_partitioned_qs() ->
    ?_test(begin
        Req = mock_request("/all-test21"),
        ?assertEqual(parse_partitioned_opt(Req), [])
    end).

t_should_allow_valid_q() ->
    ?_test(begin
        Req = mock_request("/all-test21?q=1"),
        Opts = parse_shards_opt(Req),
        ?assertEqual("1", couch_util:get_value(q, Opts))
    end).

t_should_default_on_missing_q() ->
    ?_test(begin
        Req = mock_request("/all-test21"),
        Opts = parse_shards_opt(Req),
        ?assertEqual(2, couch_util:get_value(q, Opts))
    end).

t_should_throw_on_invalid_q() ->
    ?_test(begin
        Req = mock_request("/all-test21?q="),
        Err = <<"The `q` value should be a positive integer.">>,
        ?assertThrow({bad_request, Err}, parse_shards_opt(Req))
    end).

t_should_allow_valid_n() ->
    ?_test(begin
        Req = mock_request("/all-test21?n=1"),
        Opts = parse_shards_opt(Req),
        ?assertEqual("1", couch_util:get_value(n, Opts))
    end).

t_should_default_on_missing_n() ->
    ?_test(begin
        Req = mock_request("/all-test21"),
        Opts = parse_shards_opt(Req),
        ?assertEqual(3, couch_util:get_value(n, Opts))
    end).

t_should_throw_on_invalid_n() ->
    ?_test(begin
        Req = mock_request("/all-test21?n="),
        Err = <<"The `n` value should be a positive integer.">>,
        ?assertThrow({bad_request, Err}, parse_shards_opt(Req))
    end).

t_should_allow_valid_placement() ->
    {
        foreach,
        fun() -> ok end,
        [
            {"single zone",
                ?_test(begin
                    Req = mock_request("/all-test21?placement=az:1"),
                    Opts = parse_shards_opt(Req),
                    ?assertEqual("az:1", couch_util:get_value(placement, Opts))
                end)},
            {"multi zone",
                ?_test(begin
                    Req = mock_request("/all-test21?placement=az:1,co:3"),
                    Opts = parse_shards_opt(Req),
                    ?assertEqual(
                        "az:1,co:3",
                        couch_util:get_value(placement, Opts)
                    )
                end)}
        ]
    }.

t_should_default_on_missing_placement() ->
    ?_test(begin
        Req = mock_request("/all-test21"),
        Opts = parse_shards_opt(Req),
        ?assertEqual(undefined, couch_util:get_value(placement, Opts))
    end).

t_should_throw_on_invalid_placement() ->
    Err = <<"The `placement` value should be in a format `zone:n`.">>,
    {
        foreach,
        fun() -> ok end,
        [
            {"empty placement",
                ?_test(begin
                    Req = mock_request("/all-test21?placement="),
                    ?assertThrow({bad_request, Err}, parse_shards_opt(Req))
                end)},
            {"invalid format",
                ?_test(begin
                    Req = mock_request("/all-test21?placement=moon"),
                    ?assertThrow({bad_request, Err}, parse_shards_opt(Req))
                end)},
            {"invalid n",
                ?_test(begin
                    Req = mock_request("/all-test21?placement=moon:eagle"),
                    ?assertThrow({bad_request, Err}, parse_shards_opt(Req))
                end)},
            {"one invalid zone",
                ?_test(begin
                    Req = mock_request("/all-test21?placement=az:1,co:moon"),
                    ?assertThrow({bad_request, Err}, parse_shards_opt(Req))
                end)}
        ]
    }.

-endif.
