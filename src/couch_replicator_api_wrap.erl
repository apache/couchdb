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

-module(couch_replicator_api_wrap).

% This module wraps the native erlang API, and allows for performing
% operations on a remote vs. local databases via the same API.
%
% Notes:
% Many options and apis aren't yet supported here, they are added as needed.

-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator_api_wrap.hrl").

-export([
    db_open/2,
    db_open/3,
    db_close/1,
    get_db_info/1,
    get_pending_count/2,
    update_doc/3,
    update_doc/4,
    update_docs/3,
    update_docs/4,
    ensure_full_commit/1,
    get_missing_revs/2,
    open_doc/3,
    open_doc_revs/6,
    changes_since/5,
    db_uri/1
    ]).

-import(couch_replicator_httpc, [
    send_req/3
    ]).

-import(couch_util, [
    encode_doc_id/1,
    get_value/2,
    get_value/3
    ]).

-define(MAX_WAIT, 5 * 60 * 1000).

-define(MAX_URL_LEN, 7000).
-define(MIN_URL_LEN, 200).

db_uri(#httpdb{url = Url}) ->
    couch_util:url_strip_password(Url);

db_uri(#db{name = Name}) ->
    db_uri(Name);

db_uri(DbName) ->
    ?b2l(DbName).


db_open(Db, Options) ->
    db_open(Db, Options, false).

db_open(#httpdb{} = Db1, _Options, Create) ->
    {ok, Db} = couch_replicator_httpc:setup(Db1),
    try
        case Create of
        false ->
            ok;
        true ->
            send_req(Db, [{method, put}],
                fun(401, _, _) ->
                    throw({unauthorized, ?l2b(db_uri(Db))});
                (_, _, _) ->
                    ok
                end)
        end,
        send_req(Db, [{method, head}],
            fun(200, _, _) ->
                {ok, Db};
            (401, _, _) ->
                throw({unauthorized, ?l2b(db_uri(Db))});
            (_, _, _) ->
                throw({db_not_found, ?l2b(db_uri(Db))})
            end)
    catch
        throw:Error ->
            db_close(Db),
            throw(Error);
        error:Error ->
            db_close(Db),
            erlang:error(Error);
        exit:Error ->
            db_close(Db),
            erlang:exit(Error)
    end;
db_open(DbName, Options, Create) ->
    try
        case Create of
        false ->
            ok;
        true ->
            ok = couch_httpd:verify_is_server_admin(
                get_value(user_ctx, Options)),
            couch_db:create(DbName, Options)
        end,
        case couch_db:open(DbName, Options) of
        {error, illegal_database_name, _} ->
            throw({db_not_found, DbName});
        {not_found, _Reason} ->
            throw({db_not_found, DbName});
        {ok, _Db} = Success ->
            Success
        end
    catch
    throw:{unauthorized, _} ->
        throw({unauthorized, DbName})
    end.

db_close(#httpdb{httpc_pool = Pool}) ->
    unlink(Pool),
    ok = couch_replicator_httpc_pool:stop(Pool);
db_close(DbName) ->
    catch couch_db:close(DbName).


get_db_info(#httpdb{} = Db) ->
    send_req(Db, [],
        fun(200, _, {Props}) ->
            {ok, Props}
        end);
get_db_info(#db{name = DbName, user_ctx = UserCtx}) ->
    {ok, Db} = couch_db:open(DbName, [{user_ctx, UserCtx}]),
    {ok, Info} = couch_db:get_db_info(Db),
    couch_db:close(Db),
    {ok, [{couch_util:to_binary(K), V} || {K, V} <- Info]}.


get_pending_count(#httpdb{} = Db, Seq) when is_number(Seq) ->
    % Source looks like Apache CouchDB and not Cloudant so we fall
    % back to using update sequence differences.
    send_req(Db, [], fun(200, _, {Props}) ->
        case get_value(<<"update_seq">>, Props) of
            UpdateSeq when is_number(UpdateSeq) ->
                {ok, UpdateSeq - Seq};
            _ ->
                {ok, null}
        end
    end);
get_pending_count(#httpdb{} = Db, Seq) ->
    Options = [{path, "_changes"}, {qs, [{"since", ?JSON_ENCODE(Seq)}, {"limit", "0"}]}],
    send_req(Db, Options, fun(200, _, {Props}) ->
        {ok, couch_util:get_value(<<"pending">>, Props, null)}
    end);
get_pending_count(#db{name=DbName}=Db, Seq) when is_number(Seq) ->
    {ok, CountDb} = couch_db:open(DbName, [{user_ctx, Db#db.user_ctx}]),
    Pending = couch_db:count_changes_since(CountDb, Seq),
    couch_db:close(CountDb),
    {ok, Pending}.


ensure_full_commit(#httpdb{} = Db) ->
    send_req(
        Db,
        [{method, post}, {path, "_ensure_full_commit"},
            {headers, [{"Content-Type", "application/json"}]}],
        fun(201, _, {Props}) ->
            {ok, get_value(<<"instance_start_time">>, Props)};
        (_, _, {Props}) ->
            {error, get_value(<<"error">>, Props)}
        end);
ensure_full_commit(Db) ->
    couch_db:ensure_full_commit(Db).


get_missing_revs(#httpdb{} = Db, IdRevs) ->
    JsonBody = {[{Id, couch_doc:revs_to_strs(Revs)} || {Id, Revs} <- IdRevs]},
    send_req(
        Db,
        [{method, post}, {path, "_revs_diff"}, {body, ?JSON_ENCODE(JsonBody)},
            {headers, [{"Content-Type", "application/json"}]}],
        fun(200, _, {Props}) ->
            ConvertToNativeFun = fun({Id, {Result}}) ->
                MissingRevs = couch_doc:parse_revs(
                    get_value(<<"missing">>, Result)
                ),
                PossibleAncestors = couch_doc:parse_revs(
                    get_value(<<"possible_ancestors">>, Result, [])
                ),
                {Id, MissingRevs, PossibleAncestors}
            end,
            {ok, lists:map(ConvertToNativeFun, Props)}
        end);
get_missing_revs(Db, IdRevs) ->
    couch_db:get_missing_revs(Db, IdRevs).



open_doc_revs(#httpdb{retries = 0} = HttpDb, Id, Revs, Options, _Fun, _Acc) ->
    Path = encode_doc_id(Id),
    QS = options_to_query_args(HttpDb, Path, [revs, {open_revs, Revs} | Options]),
    Url = couch_util:url_strip_password(
        couch_replicator_httpc:full_url(HttpDb, [{path,Path}, {qs,QS}])
    ),
    couch_log:error("Replication crashing because GET ~s failed", [Url]),
    exit(kaboom);
open_doc_revs(#httpdb{} = HttpDb, Id, Revs, Options, Fun, Acc) ->
    Path = encode_doc_id(Id),
    QS = options_to_query_args(HttpDb, Path, [revs, {open_revs, Revs} | Options]),
    {Pid, Ref} = spawn_monitor(fun() ->
        Self = self(),
        Callback = fun
          (200, Headers, StreamDataFun) ->
            remote_open_doc_revs_streamer_start(Self),
            {<<"--">>, _, _} = couch_httpd:parse_multipart_request(
                get_value("Content-Type", Headers),
                StreamDataFun,
                fun mp_parse_mixed/1
            );
          (414, _, _) ->
            exit(request_uri_too_long)
        end,
        Streamer = spawn_link(fun() ->
            Params = [
                {path, Path},
                {qs, QS},
                {ibrowse_options, [{stream_to, {self(), once}}]},
                {headers, [{"Accept", "multipart/mixed"}]}
            ],
            % We're setting retries to 0 here to avoid the case where the
            % Streamer retries the request and ends up jumbling together two
            % different response bodies.  Retries are handled explicitly by
            % open_doc_revs itself.
            send_req(HttpDb#httpdb{retries = 0}, Params, Callback)
        end),
        % If this process dies normally we can leave
        % the Streamer process hanging around keeping an
        % HTTP connection open. This is a bit of a
        % hammer approach to making sure it releases
        % that connection back to the pool.
        spawn(fun() ->
            Ref = erlang:monitor(process, Self),
            receive
                {'DOWN', Ref, process, Self, normal} ->
                    exit(Streamer, {streamer_parent_died, Self});
                {'DOWN', Ref, process, Self, _} ->
                    ok
                end
        end),
        receive
        {started_open_doc_revs, Ref} ->
            Ret = receive_docs_loop(Streamer, Fun, Id, Revs, Ref, Acc),
            exit({exit_ok, Ret})
        end
    end),
    receive
        {'DOWN', Ref, process, Pid, {exit_ok, Ret}} ->
            Ret;
        {'DOWN', Ref, process, Pid, {{nocatch, missing_doc}, _}} ->
            throw(missing_doc);
        {'DOWN', Ref, process, Pid, {{nocatch, {missing_stub,_} = Stub}, _}} ->
            throw(Stub);
        {'DOWN', Ref, process, Pid, request_uri_too_long} ->
            NewMaxLen = get_value(max_url_len, Options, ?MAX_URL_LEN) div 2,
            case NewMaxLen < ?MIN_URL_LEN of
                true ->
                    throw(request_uri_too_long);
                false ->
                    couch_log:info("Reducing url length to ~B because of"
                                   " 414 response", [NewMaxLen]),
                    Options1 = lists:keystore(max_url_len, 1, Options,
                                              {max_url_len, NewMaxLen}),
                    open_doc_revs(HttpDb, Id, Revs, Options1, Fun, Acc)
            end;
        {'DOWN', Ref, process, Pid, Else} ->
            Url = couch_util:url_strip_password(
                couch_replicator_httpc:full_url(HttpDb, [{path,Path}, {qs,QS}])
            ),
            #httpdb{retries = Retries, wait = Wait0} = HttpDb,
            Wait = 2 * erlang:min(Wait0 * 2, ?MAX_WAIT),
            couch_log:notice("Retrying GET to ~s in ~p seconds due to error ~w",
                [Url, Wait / 1000, error_reason(Else)]
            ),
            ok = timer:sleep(Wait),
            RetryDb = HttpDb#httpdb{
                retries = Retries - 1,
                wait = Wait
            },
            open_doc_revs(RetryDb, Id, Revs, Options, Fun, Acc)
    end;
open_doc_revs(Db, Id, Revs, Options, Fun, Acc) ->
    {ok, Results} = couch_db:open_doc_revs(Db, Id, Revs, Options),
    {ok, lists:foldl(fun(R, A) -> {_, A2} = Fun(R, A), A2 end, Acc, Results)}.

error_reason({http_request_failed, "GET", _Url, {error, timeout}}) ->
    timeout;
error_reason({http_request_failed, "GET", _Url, {error, {_, req_timedout}}}) ->
    req_timedout;
error_reason({http_request_failed, "GET", _Url, Error}) ->
    Error;
error_reason(Else) ->
    Else.

open_doc(#httpdb{} = Db, Id, Options) ->
    send_req(
        Db,
        [{path, encode_doc_id(Id)}, {qs, options_to_query_args(Options, [])}],
        fun(200, _, Body) ->
            {ok, couch_doc:from_json_obj(Body)};
        (_, _, {Props}) ->
            {error, get_value(<<"error">>, Props)}
        end);
open_doc(Db, Id, Options) ->
    case couch_db:open_doc(Db, Id, Options) of
    {ok, _} = Ok ->
        Ok;
    {not_found, _Reason} ->
        {error, <<"not_found">>}
    end.


update_doc(Db, Doc, Options) ->
    update_doc(Db, Doc, Options, interactive_edit).

update_doc(#httpdb{} = HttpDb, #doc{id = DocId} = Doc, Options, Type) ->
    QArgs = case Type of
    replicated_changes ->
        [{"new_edits", "false"}];
    _ ->
        []
    end ++ options_to_query_args(Options, []),
    Boundary = couch_uuids:random(),
    JsonBytes = ?JSON_ENCODE(
        couch_doc:to_json_obj(
          Doc, [revs, attachments, follows, att_encoding_info | Options])),
    {ContentType, Len} = couch_doc:len_doc_to_multi_part_stream(Boundary,
        JsonBytes, Doc#doc.atts, true),
    Headers = case lists:member(delay_commit, Options) of
    true ->
        [{"X-Couch-Full-Commit", "false"}];
    false ->
        []
    end ++ [{"Content-Type", ?b2l(ContentType)}, {"Content-Length", Len}],
    Body = {fun stream_doc/1, {JsonBytes, Doc#doc.atts, Boundary, Len}},
    send_req(
        % A crash here bubbles all the way back up to run_user_fun inside
        % open_doc_revs, which will retry the whole thing.  That's the
        % appropriate course of action, since we've already started streaming
        % the response body from the GET request.
        HttpDb#httpdb{retries = 0},
        [{method, put}, {path, encode_doc_id(DocId)},
            {qs, QArgs}, {headers, Headers}, {body, Body}],
        fun(Code, _, {Props}) when Code =:= 200 orelse Code =:= 201 orelse Code =:= 202 ->
                {ok, couch_doc:parse_rev(get_value(<<"rev">>, Props))};
            (409, _, _) ->
                throw(conflict);
            (Code, _, {Props}) ->
                case {Code, get_value(<<"error">>, Props)} of
                {401, <<"unauthorized">>} ->
                    throw({unauthorized, get_value(<<"reason">>, Props)});
                {403, <<"forbidden">>} ->
                    throw({forbidden, get_value(<<"reason">>, Props)});
                {412, <<"missing_stub">>} ->
                    throw({missing_stub, get_value(<<"reason">>, Props)});
                {_, Error} ->
                    {error, Error}
                end
        end);
update_doc(Db, Doc, Options, Type) ->
    couch_db:update_doc(Db, Doc, Options, Type).


update_docs(Db, DocList, Options) ->
    update_docs(Db, DocList, Options, interactive_edit).

update_docs(_Db, [], _Options, _UpdateType) ->
    {ok, []};
update_docs(#httpdb{} = HttpDb, DocList, Options, UpdateType) ->
    FullCommit = atom_to_list(not lists:member(delay_commit, Options)),
    Prefix = case UpdateType of
    replicated_changes ->
        <<"{\"new_edits\":false,\"docs\":[">>;
    interactive_edit ->
        <<"{\"docs\":[">>
    end,
    Suffix = <<"]}">>,
    % Note: nginx and other servers don't like PUT/POST requests without
    % a Content-Length header, so we can't do a chunked transfer encoding
    % and JSON encode each doc only before sending it through the socket.
    {Docs, Len} = lists:mapfoldl(
        fun(#doc{} = Doc, Acc) ->
            Json = ?JSON_ENCODE(couch_doc:to_json_obj(Doc, [revs, attachments])),
            {Json, Acc + iolist_size(Json)};
        (Doc, Acc) ->
            {Doc, Acc + iolist_size(Doc)}
        end,
        byte_size(Prefix) + byte_size(Suffix) + length(DocList) - 1,
        DocList),
    BodyFun = fun(eof) ->
            eof;
        ([]) ->
            {ok, Suffix, eof};
        ([prefix | Rest]) ->
            {ok, Prefix, Rest};
        ([Doc]) ->
            {ok, Doc, []};
        ([Doc | RestDocs]) ->
            {ok, [Doc, ","], RestDocs}
    end,
    Headers = [
        {"Content-Length", Len},
        {"Content-Type", "application/json"},
        {"X-Couch-Full-Commit", FullCommit}
    ],
    send_req(
        HttpDb,
        [{method, post}, {path, "_bulk_docs"},
            {body, {BodyFun, [prefix | Docs]}}, {headers, Headers}],
        fun(201, _, Results) when is_list(Results) ->
                {ok, bulk_results_to_errors(DocList, Results, remote)};
           (417, _, Results) when is_list(Results) ->
                {ok, bulk_results_to_errors(DocList, Results, remote)}
        end);
update_docs(Db, DocList, Options, UpdateType) ->
    Result = couch_db:update_docs(Db, DocList, Options, UpdateType),
    {ok, bulk_results_to_errors(DocList, Result, UpdateType)}.


changes_since(#httpdb{headers = Headers1, timeout = InactiveTimeout} = HttpDb,
              Style, StartSeq, UserFun, Options) ->
    Timeout = erlang:max(1000, InactiveTimeout div 3),
    BaseQArgs = case get_value(continuous, Options, false) of
    false ->
        [{"feed", "normal"}];
    true ->
        [{"feed", "continuous"}]
    end ++ [
        {"style", atom_to_list(Style)}, {"since", ?JSON_ENCODE(StartSeq)},
        {"timeout", integer_to_list(Timeout)}
           ],
    DocIds = get_value(doc_ids, Options),
    {QArgs, Method, Body, Headers} = case DocIds of
    undefined ->
        QArgs1 = maybe_add_changes_filter_q_args(BaseQArgs, Options),
        {QArgs1, get, [], Headers1};
    _ when is_list(DocIds) ->
        Headers2 = [{"Content-Type", "application/json"} | Headers1],
        JsonDocIds = ?JSON_ENCODE({[{<<"doc_ids">>, DocIds}]}),
        {[{"filter", "_doc_ids"} | BaseQArgs], post, JsonDocIds, Headers2}
    end,
    send_req(
        HttpDb,
        [{method, Method}, {path, "_changes"}, {qs, QArgs},
            {headers, Headers}, {body, Body},
            {ibrowse_options, [{stream_to, {self(), once}}]}],
        fun(200, _, DataStreamFun) ->
                parse_changes_feed(Options, UserFun, DataStreamFun);
            (405, _, _) when is_list(DocIds) ->
                % CouchDB versions < 1.1.0 don't have the builtin _changes feed
                % filter "_doc_ids" neither support POST
                send_req(HttpDb, [{method, get}, {path, "_changes"},
                    {qs, BaseQArgs}, {headers, Headers1},
                    {ibrowse_options, [{stream_to, {self(), once}}]}],
                    fun(200, _, DataStreamFun2) ->
                        UserFun2 = fun(#doc_info{id = Id} = DocInfo) ->
                            case lists:member(Id, DocIds) of
                            true ->
                                UserFun(DocInfo);
                            false ->
                                ok
                            end;
                        (LastSeq) ->
                            UserFun(LastSeq)
                        end,
                        parse_changes_feed(Options, UserFun2, DataStreamFun2)
                    end)
        end);
changes_since(Db, Style, StartSeq, UserFun, Options) ->
    Filter = case get_value(doc_ids, Options) of
    undefined ->
        ?b2l(get_value(filter, Options, <<>>));
    _DocIds ->
        "_doc_ids"
    end,
    Args = #changes_args{
        style = Style,
        since = StartSeq,
        filter = Filter,
        feed = case get_value(continuous, Options, false) of
            true ->
                "continuous";
            false ->
                "normal"
        end,
        timeout = infinity
    },
    QueryParams = get_value(query_params, Options, {[]}),
    Req = changes_json_req(Db, Filter, QueryParams, Options),
    ChangesFeedFun = couch_changes:handle_changes(Args, {json_req, Req}, Db),
    ChangesFeedFun(fun({change, Change, _}, _) ->
            UserFun(json_to_doc_info(Change));
        (_, _) ->
            ok
    end).


% internal functions

maybe_add_changes_filter_q_args(BaseQS, Options) ->
    case get_value(filter, Options) of
    undefined ->
        BaseQS;
    FilterName ->
        {Params} = get_value(query_params, Options, {[]}),
        [{"filter", ?b2l(FilterName)} | lists:foldl(
            fun({K, V}, QSAcc) ->
                Ks = couch_util:to_list(K),
                case lists:keymember(Ks, 1, QSAcc) of
                true ->
                    QSAcc;
                false ->
                    [{Ks, couch_util:to_list(V)} | QSAcc]
                end
            end,
            BaseQS, Params)]
    end.

parse_changes_feed(Options, UserFun, DataStreamFun) ->
    case get_value(continuous, Options, false) of
    true ->
        continuous_changes(DataStreamFun, UserFun);
    false ->
        EventFun = fun(Ev) ->
            changes_ev1(Ev, fun(DocInfo, _) -> UserFun(DocInfo) end, [])
        end,
        json_stream_parse:events(DataStreamFun, EventFun)
    end.

changes_json_req(_Db, "", _QueryParams, _Options) ->
    {[]};
changes_json_req(_Db, "_doc_ids", _QueryParams, Options) ->
    {[{<<"doc_ids">>, get_value(doc_ids, Options)}]};
changes_json_req(Db, FilterName, {QueryParams}, _Options) ->
    {ok, Info} = couch_db:get_db_info(Db),
    % simulate a request to db_name/_changes
    {[
        {<<"info">>, {Info}},
        {<<"id">>, null},
        {<<"method">>, 'GET'},
        {<<"path">>, [couch_db:name(Db), <<"_changes">>]},
        {<<"query">>, {[{<<"filter">>, FilterName} | QueryParams]}},
        {<<"headers">>, []},
        {<<"body">>, []},
        {<<"peer">>, <<"replicator">>},
        {<<"form">>, []},
        {<<"cookie">>, []},
        {<<"userCtx">>, couch_util:json_user_ctx(Db)}
    ]}.


options_to_query_args(HttpDb, Path, Options0) ->
    case lists:keytake(max_url_len, 1, Options0) of
        false -> MaxLen = ?MAX_URL_LEN, Options = Options0;
        {value, {max_url_len, MaxLen}, Options} -> ok
    end,
    case lists:keytake(atts_since, 1, Options) of
    false ->
        options_to_query_args(Options, []);
    {value, {atts_since, []}, Options2} ->
        options_to_query_args(Options2, []);
    {value, {atts_since, PAs}, Options2} ->
        QueryArgs1 = options_to_query_args(Options2, []),
        FullUrl = couch_replicator_httpc:full_url(
            HttpDb, [{path, Path}, {qs, QueryArgs1}]),
        RevList = atts_since_arg(
            length("GET " ++ FullUrl ++ " HTTP/1.1\r\n") +
            length("&atts_since=") + 6,  % +6 = % encoded [ and ]
            PAs, MaxLen, []),
        [{"atts_since", ?JSON_ENCODE(RevList)} | QueryArgs1]
    end.


options_to_query_args([], Acc) ->
    lists:reverse(Acc);
options_to_query_args([ejson_body | Rest], Acc) ->
    options_to_query_args(Rest, Acc);
options_to_query_args([delay_commit | Rest], Acc) ->
    options_to_query_args(Rest, Acc);
options_to_query_args([revs | Rest], Acc) ->
    options_to_query_args(Rest, [{"revs", "true"} | Acc]);
options_to_query_args([{open_revs, all} | Rest], Acc) ->
    options_to_query_args(Rest, [{"open_revs", "all"} | Acc]);
options_to_query_args([latest | Rest], Acc) ->
    options_to_query_args(Rest, [{"latest", "true"} | Acc]);
options_to_query_args([{open_revs, Revs} | Rest], Acc) ->
    JsonRevs = ?b2l(iolist_to_binary(?JSON_ENCODE(couch_doc:revs_to_strs(Revs)))),
    options_to_query_args(Rest, [{"open_revs", JsonRevs} | Acc]).

atts_since_arg(_UrlLen, [], _MaxLen, Acc) ->
    lists:reverse(Acc);
atts_since_arg(UrlLen, [PA | Rest], MaxLen, Acc) ->
    RevStr = couch_doc:rev_to_str(PA),
    NewUrlLen = case Rest of
    [] ->
        % plus 2 double quotes (% encoded)
        UrlLen + size(RevStr) + 6;
    _ ->
        % plus 2 double quotes and a comma (% encoded)
        UrlLen + size(RevStr) + 9
    end,
    case NewUrlLen >= MaxLen of
    true ->
        lists:reverse(Acc);
    false ->
        atts_since_arg(NewUrlLen, Rest, MaxLen, [RevStr | Acc])
    end.


% TODO: A less verbose, more elegant and automatic restart strategy for
%       the exported open_doc_revs/6 function. The restart should be
%       transparent to the caller like any other Couch API function exported
%       by this module.
receive_docs_loop(Streamer, Fun, Id, Revs, Ref, Acc) ->
    try
        % Left only for debugging purposes via an interactive or remote shell
        erlang:put(open_doc_revs, {Id, Revs, Ref, Streamer}),
        receive_docs(Streamer, Fun, Ref, Acc)
    catch
    error:{restart_open_doc_revs, NewRef} ->
        receive_docs_loop(Streamer, Fun, Id, Revs, NewRef, Acc)
    end.

receive_docs(Streamer, UserFun, Ref, UserAcc) ->
    Streamer ! {get_headers, Ref, self()},
    receive
    {started_open_doc_revs, NewRef} ->
        restart_remote_open_doc_revs(Ref, NewRef);
    {headers, Ref, Headers} ->
        case get_value("content-type", Headers) of
        {"multipart/related", _} = ContentType ->
            case couch_doc:doc_from_multi_part_stream(
                ContentType,
                fun() -> receive_doc_data(Streamer, Ref) end,
                Ref) of
            {ok, Doc, WaitFun, Parser} ->
                case run_user_fun(UserFun, {ok, Doc}, UserAcc, Ref) of
                {ok, UserAcc2} ->
                    ok;
                {skip, UserAcc2} ->
                    couch_doc:abort_multi_part_stream(Parser)
                end,
                WaitFun(),
                receive_docs(Streamer, UserFun, Ref, UserAcc2)
            end;
        {"application/json", []} ->
            Doc = couch_doc:from_json_obj(
                    ?JSON_DECODE(receive_all(Streamer, Ref, []))),
            {_, UserAcc2} = run_user_fun(UserFun, {ok, Doc}, UserAcc, Ref),
            receive_docs(Streamer, UserFun, Ref, UserAcc2);
        {"application/json", [{"error","true"}]} ->
            {ErrorProps} = ?JSON_DECODE(receive_all(Streamer, Ref, [])),
            Rev = get_value(<<"missing">>, ErrorProps),
            Result = {{not_found, missing}, couch_doc:parse_rev(Rev)},
            {_, UserAcc2} = run_user_fun(UserFun, Result, UserAcc, Ref),
            receive_docs(Streamer, UserFun, Ref, UserAcc2)
        end;
    {done, Ref} ->
        {ok, UserAcc}
    end.


run_user_fun(UserFun, Arg, UserAcc, OldRef) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        try UserFun(Arg, UserAcc) of
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
    receive
        {started_open_doc_revs, NewRef} ->
            erlang:demonitor(Ref, [flush]),
            exit(Pid, kill),
            restart_remote_open_doc_revs(OldRef, NewRef);
        {'DOWN', Ref, process, Pid, {exit_ok, Ret}} ->
            Ret;
        {'DOWN', Ref, process, Pid, {exit_throw, Reason}} ->
            throw(Reason);
        {'DOWN', Ref, process, Pid, {exit_error, Reason}} ->
            erlang:error(Reason);
        {'DOWN', Ref, process, Pid, {exit_exit, Reason}} ->
            erlang:exit(Reason)
    end.


restart_remote_open_doc_revs(Ref, NewRef) ->
    receive
    {body_bytes, Ref, _} ->
        restart_remote_open_doc_revs(Ref, NewRef);
    {body_done, Ref} ->
        restart_remote_open_doc_revs(Ref, NewRef);
    {done, Ref} ->
        restart_remote_open_doc_revs(Ref, NewRef);
    {headers, Ref, _} ->
        restart_remote_open_doc_revs(Ref, NewRef)
    after 0 ->
        erlang:error({restart_open_doc_revs, NewRef})
    end.


remote_open_doc_revs_streamer_start(Parent) ->
    receive
    {get_headers, _Ref, Parent} ->
        remote_open_doc_revs_streamer_start(Parent);
    {next_bytes, _Ref, Parent} ->
        remote_open_doc_revs_streamer_start(Parent)
    after 0 ->
        Parent ! {started_open_doc_revs, make_ref()}
    end.


receive_all(Streamer, Ref, Acc) ->
    Streamer ! {next_bytes, Ref, self()},
    receive
    {started_open_doc_revs, NewRef} ->
        restart_remote_open_doc_revs(Ref, NewRef);
    {body_bytes, Ref, Bytes} ->
        receive_all(Streamer, Ref, [Bytes | Acc]);
    {body_done, Ref} ->
        lists:reverse(Acc)
    end.


mp_parse_mixed(eof) ->
    receive {get_headers, Ref, From} ->
        From ! {done, Ref}
    end;
mp_parse_mixed({headers, H}) ->
    receive {get_headers, Ref, From} ->
        From ! {headers, Ref, H}
    end,
    fun mp_parse_mixed/1;
mp_parse_mixed({body, Bytes}) ->
    receive {next_bytes, Ref, From} ->
        From ! {body_bytes, Ref, Bytes}
    end,
    fun mp_parse_mixed/1;
mp_parse_mixed(body_end) ->
    receive {next_bytes, Ref, From} ->
        From ! {body_done, Ref};
    {get_headers, Ref, From} ->
        self() ! {get_headers, Ref, From}
    end,
    fun mp_parse_mixed/1.


receive_doc_data(Streamer, Ref) ->
    Streamer ! {next_bytes, Ref, self()},
    receive
    {body_bytes, Ref, Bytes} ->
        {Bytes, fun() -> receive_doc_data(Streamer, Ref) end};
    {body_done, Ref} ->
        {<<>>, fun() -> receive_doc_data(Streamer, Ref) end}
    end.


changes_ev1(object_start, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev2(Ev, UserFun, UserAcc) end.

changes_ev2({key, <<"results">>}, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev3(Ev, UserFun, UserAcc) end;
changes_ev2(_, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev2(Ev, UserFun, UserAcc) end.

changes_ev3(array_start, UserFun, UserAcc) ->
    fun(Ev) -> changes_ev_loop(Ev, UserFun, UserAcc) end.

changes_ev_loop(object_start, UserFun, UserAcc) ->
    fun(Ev) ->
        json_stream_parse:collect_object(Ev,
            fun(Obj) ->
                UserAcc2 = UserFun(json_to_doc_info(Obj), UserAcc),
                fun(Ev2) -> changes_ev_loop(Ev2, UserFun, UserAcc2) end
            end)
    end;
changes_ev_loop(array_end, _UserFun, _UserAcc) ->
    fun(_Ev) -> changes_ev_done() end.

changes_ev_done() ->
    fun(_Ev) -> changes_ev_done() end.

continuous_changes(DataFun, UserFun) ->
    {DataFun2, _, Rest} = json_stream_parse:events(
        DataFun,
        fun(Ev) -> parse_changes_line(Ev, UserFun) end),
    continuous_changes(fun() -> {Rest, DataFun2} end, UserFun).

parse_changes_line(object_start, UserFun) ->
    fun(Ev) ->
        json_stream_parse:collect_object(Ev,
            fun(Obj) -> UserFun(json_to_doc_info(Obj)) end)
    end.

json_to_doc_info({Props}) ->
    case get_value(<<"changes">>, Props) of
    undefined ->
        {last_seq, get_value(<<"last_seq">>, Props)};
    Changes ->
        RevsInfo = lists:map(
            fun({Change}) ->
                Rev = couch_doc:parse_rev(get_value(<<"rev">>, Change)),
                Del = couch_replicator_utils:is_deleted(Change),
                #rev_info{rev=Rev, deleted=Del}
            end, Changes),
        #doc_info{
            id = get_value(<<"id">>, Props),
            high_seq = get_value(<<"seq">>, Props),
            revs = RevsInfo
        }
    end.


bulk_results_to_errors(Docs, {ok, Results}, interactive_edit) ->
    lists:reverse(lists:foldl(
        fun({_, {ok, _}}, Acc) ->
            Acc;
        ({#doc{id = Id, revs = {Pos, [RevId | _]}}, Error}, Acc) ->
            {_, Error, Reason} = couch_httpd:error_info(Error),
            [ {[{id, Id}, {rev, rev_to_str({Pos, RevId})},
                {error, Error}, {reason, Reason}]} | Acc ]
        end,
        [], lists:zip(Docs, Results)));

bulk_results_to_errors(Docs, {ok, Results}, replicated_changes) ->
    bulk_results_to_errors(Docs, {aborted, Results}, interactive_edit);

bulk_results_to_errors(_Docs, {aborted, Results}, interactive_edit) ->
    lists:map(
        fun({{Id, Rev}, Err}) ->
            {_, Error, Reason} = couch_httpd:error_info(Err),
            {[{id, Id}, {rev, rev_to_str(Rev)}, {error, Error}, {reason, Reason}]}
        end,
        Results);

bulk_results_to_errors(_Docs, Results, remote) ->
    lists:reverse(lists:foldl(
        fun({Props}, Acc) ->
            case get_value(<<"error">>, Props, get_value(error, Props)) of
            undefined ->
                Acc;
            Error ->
                Id = get_value(<<"id">>, Props, get_value(id, Props)),
                Rev = get_value(<<"rev">>, Props, get_value(rev, Props)),
                Reason = get_value(<<"reason">>, Props, get_value(reason, Props)),
                [ {[{id, Id}, {rev, rev_to_str(Rev)},
                    {error, Error}, {reason, Reason}]} | Acc ]
            end
        end,
        [], Results)).


rev_to_str({_Pos, _Id} = Rev) ->
    couch_doc:rev_to_str(Rev);
rev_to_str(Rev) ->
    Rev.

write_fun() ->
    fun(Data) ->
        receive {get_data, Ref, From} ->
            From ! {data, Ref, Data}
        end
    end.

stream_doc({JsonBytes, Atts, Boundary, Len}) ->
    case erlang:erase({doc_streamer, Boundary}) of
    Pid when is_pid(Pid) ->
        unlink(Pid),
        exit(Pid, kill);
    _ ->
        ok
    end,
    DocStreamer = spawn_link(
        couch_doc,
        doc_to_multi_part_stream,
        [Boundary, JsonBytes, Atts, write_fun(), true]
    ),
    erlang:put({doc_streamer, Boundary}, DocStreamer),
    {ok, <<>>, {Len, Boundary}};
stream_doc({0, Id}) ->
    erlang:erase({doc_streamer, Id}),
    eof;
stream_doc({LenLeft, Id}) when LenLeft > 0 ->
    Ref = make_ref(),
    erlang:get({doc_streamer, Id}) ! {get_data, Ref, self()},
    receive {data, Ref, Data} ->
        {ok, Data, {LenLeft - iolist_size(Data), Id}}
    end.
