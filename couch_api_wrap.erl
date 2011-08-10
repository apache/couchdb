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

-module(couch_api_wrap).

% This module wraps the native erlang API, and allows for performing
% operations on a remote vs. local databases via the same API.
%
% Notes:
% Many options and apis aren't yet supported here, they are added as needed.

-include("couch_db.hrl").
-include("couch_api_wrap.hrl").

-export([
    db_open/2,
    db_open/3,
    db_close/1,
    get_db_info/1,
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

-import(couch_api_wrap_httpc, [
    httpdb_setup/1,
    send_req/3
    ]).

-import(couch_util, [
    encode_doc_id/1,
    get_value/2,
    get_value/3
    ]).


db_uri(#httpdb{url = Url}) ->
    couch_util:url_strip_password(Url);

db_uri(#db{name = Name}) ->
    db_uri(Name);

db_uri(DbName) ->
    ?b2l(DbName).


db_open(Db, Options) ->
    db_open(Db, Options, false).

db_open(#httpdb{} = Db1, _Options, Create) ->
    {ok, Db} = couch_api_wrap_httpc:setup(Db1),
    case Create of
    false ->
        ok;
    true ->
        send_req(Db, [{method, put}, {direct, true}], fun(_, _, _) -> ok end)
    end,
    send_req(Db, [{method, head}],
        fun(200, _, _) ->
            {ok, Db};
        (401, _, _) ->
            throw({unauthorized, ?l2b(db_uri(Db))});
        (_, _, _) ->
            throw({db_not_found, ?l2b(db_uri(Db))})
        end);
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
    ok = couch_httpc_pool:stop(Pool);
db_close(DbName) ->
    catch couch_db:close(DbName).


get_db_info(#httpdb{} = Db) ->
    send_req(Db, [],
        fun(200, _, {Props}) ->
            {ok, Props}
        end);
get_db_info(Db) ->
    {ok, Info} = couch_db:get_db_info(Db),
    {ok, [{couch_util:to_binary(K), V} || {K, V} <- Info]}.


ensure_full_commit(#httpdb{} = Db) ->
    send_req(
        Db,
        [{method, post}, {path, "_ensure_full_commit"}, {direct, true},
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
        [{method, post}, {path, "_revs_diff"}, {body, ?JSON_ENCODE(JsonBody)}],
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



open_doc_revs(#httpdb{} = HttpDb, Id, Revs, Options, Fun, Acc) ->
    Path = encode_doc_id(Id),
    QArgs = options_to_query_args(
        HttpDb, Path, [revs, {open_revs, Revs} | Options]),
    Self = self(),
    Streamer = spawn_link(fun() ->
            send_req(
                HttpDb,
                [{path, Path}, {qs, QArgs},
                    {ibrowse_options, [{stream_to, {self(), once}}]},
                    {headers, [{"Accept", "multipart/mixed"}]}],
                fun(200, Headers, StreamDataFun) ->
                    remote_open_doc_revs_streamer_start(Self),
                    {<<"--">>, _, _} = couch_httpd:parse_multipart_request(
                        get_value("Content-Type", Headers),
                        StreamDataFun,
                        fun mp_parse_mixed/1)
                end),
            unlink(Self)
        end),
    receive
    {started_open_doc_revs, Ref} ->
        receive_docs_loop(Streamer, Fun, Id, Revs, Ref, Acc)
    end;
open_doc_revs(Db, Id, Revs, Options, Fun, Acc) ->
    {ok, Results} = couch_db:open_doc_revs(Db, Id, Revs, Options),
    {ok, lists:foldl(fun(R, A) -> {_, A2} = Fun(R, A), A2 end, Acc, Results)}.


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
        HttpDb,
        [{method, put}, {path, encode_doc_id(DocId)},
           {direct, Type =:= interactive_edit}, {qs, QArgs},
           {headers, Headers}, {body, Body}],
        fun(Code, _, {Props}) when Code =:= 200 orelse Code =:= 201 ->
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

update_docs(#httpdb{} = HttpDb, DocList, Options, UpdateType) ->
    FullCommit = atom_to_list(not lists:member(delay_commit, Options)),
    Prefix1 = case UpdateType of
    replicated_changes ->
        {prefix, <<"{\"new_edits\":false,\"docs\":[">>};
    interactive_edit ->
        {prefix, <<"{\"docs\":[">>}
    end,
    BodyFun = fun(eof) ->
            eof;
        ([]) ->
            {ok, <<"]}">>, eof};
        ([{prefix, Prefix} | Rest]) ->
            {ok, Prefix, Rest};
        ([Doc]) when is_record(Doc, doc) ->
            DocJson = couch_doc:to_json_obj(Doc, [revs, attachments]),
            {ok, ?JSON_ENCODE(DocJson), []};
        ([Doc | RestDocs]) when is_record(Doc, doc) ->
            DocJson = couch_doc:to_json_obj(Doc, [revs, attachments]),
            {ok, [?JSON_ENCODE(DocJson), ","], RestDocs};
        ([Doc]) ->
            % IO list
            {ok, Doc, []};
        ([Doc | RestDocs]) ->
            % IO list
            {ok, [Doc, ","], RestDocs}
    end,
    send_req(
        HttpDb,
        [{method, post}, {path, "_bulk_docs"},
            {body, {BodyFun, [Prefix1 | DocList]}},
            {ibrowse_options, [{transfer_encoding, chunked}]},
            {headers, [
                {"X-Couch-Full-Commit", FullCommit},
                {"Content-Type", "application/json"} ]}],
        fun(201, _, Results) when is_list(Results) ->
                {ok, bulk_results_to_errors(DocList, Results, remote)};
           (417, _, Results) when is_list(Results) ->
                {ok, bulk_results_to_errors(DocList, Results, remote)}
        end);
update_docs(Db, DocList, Options, UpdateType) ->
    Result = couch_db:update_docs(Db, DocList, Options, UpdateType),
    {ok, bulk_results_to_errors(DocList, Result, UpdateType)}.


changes_since(#httpdb{headers = Headers1} = HttpDb, Style, StartSeq,
    UserFun, Options) ->
    BaseQArgs = case get_value(continuous, Options, false) of
    false ->
        [{"feed", "normal"}];
    true ->
        [{"feed", "continuous"}, {"heartbeat", "10000"}]
    end ++ [
        {"style", atom_to_list(Style)}, {"since", couch_util:to_list(StartSeq)}
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
                            end
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


options_to_query_args(HttpDb, Path, Options) ->
    case lists:keytake(atts_since, 1, Options) of
    false ->
        options_to_query_args(Options, []);
    {value, {atts_since, []}, Options2} ->
        options_to_query_args(Options2, []);
    {value, {atts_since, PAs}, Options2} ->
        QueryArgs1 = options_to_query_args(Options2, []),
        FullUrl = couch_api_wrap_httpc:full_url(
            HttpDb, [{path, Path}, {qs, QueryArgs1}]),
        RevList = atts_since_arg(
            length(FullUrl) + length("&atts_since=[]"), PAs, []),
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
options_to_query_args([{open_revs, Revs} | Rest], Acc) ->
    JsonRevs = ?b2l(?JSON_ENCODE(couch_doc:revs_to_strs(Revs))),
    options_to_query_args(Rest, [{"open_revs", JsonRevs} | Acc]).


-define(MAX_URL_LEN, 8192).

atts_since_arg(_UrlLen, [], Acc) ->
    lists:reverse(Acc);
atts_since_arg(UrlLen, [PA | Rest], Acc) ->
    RevStr = couch_doc:rev_to_str(PA),
    NewUrlLen = case Rest of
    [] ->
        UrlLen + size(RevStr) + 2;  % plus 2 double quotes
    _ ->
        UrlLen + size(RevStr) + 3   % plus 2 double quotes and a comma
    end,
    case NewUrlLen > ?MAX_URL_LEN of
    true ->
        lists:reverse(Acc);
    false ->
        atts_since_arg(NewUrlLen, Rest, [RevStr | Acc])
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
            case doc_from_multi_part_stream(
                ContentType,
                fun() -> receive_doc_data(Streamer, Ref) end,
                Ref) of
            {ok, Doc, Parser} ->
                case UserFun({ok, Doc}, UserAcc) of
                {ok, UserAcc2} ->
                    ok;
                {skip, UserAcc2} ->
                    couch_doc:abort_multi_part_stream(Parser)
                end,
                receive_docs(Streamer, UserFun, Ref, UserAcc2)
            end;
        {"application/json", []} ->
            Doc = couch_doc:from_json_obj(
                    ?JSON_DECODE(receive_all(Streamer, Ref, []))),
            {_, UserAcc2} = UserFun({ok, Doc}, UserAcc),
            receive_docs(Streamer, UserFun, Ref, UserAcc2);
        {"application/json", [{"error","true"}]} ->
            {ErrorProps} = ?JSON_DECODE(receive_all(Streamer, Ref, [])),
            Rev = get_value(<<"missing">>, ErrorProps),
            Result = {{not_found, missing}, couch_doc:parse_rev(Rev)},
            {_, UserAcc2} = UserFun(Result, UserAcc),
            receive_docs(Streamer, UserFun, Ref, UserAcc2)
        end;
    {done, Ref} ->
        {ok, UserAcc}
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

doc_from_multi_part_stream(ContentType, DataFun, Ref) ->
    Self = self(),
    Parser = spawn_link(fun() ->
        {<<"--">>, _, _} = couch_httpd:parse_multipart_request(
            ContentType, DataFun,
            fun(Next) -> couch_doc:mp_parse_doc(Next, []) end),
        unlink(Self)
        end),
    Parser ! {get_doc_bytes, Ref, self()},
    receive
    {started_open_doc_revs, NewRef} ->
        unlink(Parser),
        exit(Parser, kill),
        restart_remote_open_doc_revs(Ref, NewRef);
    {doc_bytes, Ref, DocBytes} ->
        Doc = couch_doc:from_json_obj(?JSON_DECODE(DocBytes)),
        ReadAttachmentDataFun = fun() ->
            Parser ! {get_bytes, Ref, self()},
            receive
            {started_open_doc_revs, NewRef} ->
                unlink(Parser),
                exit(Parser, kill),
                receive {bytes, Ref, _} -> ok after 0 -> ok end,
                restart_remote_open_doc_revs(Ref, NewRef);
            {bytes, Ref, Bytes} ->
                Bytes
            end
        end,
        Atts2 = lists:map(
            fun(#att{data = follows} = A) ->
                A#att{data = ReadAttachmentDataFun};
            (A) ->
                A
            end, Doc#doc.atts),
        {ok, Doc#doc{atts = Atts2}, Parser}
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
    RevsInfo = lists:map(
        fun({Change}) ->
            Rev = couch_doc:parse_rev(get_value(<<"rev">>, Change)),
            Del = (true =:= get_value(<<"deleted">>, Change)),
            #rev_info{rev=Rev, deleted=Del}
        end, get_value(<<"changes">>, Props)),
    #doc_info{
        id = get_value(<<"id">>, Props),
        high_seq = get_value(<<"seq">>, Props),
        revs = RevsInfo
    }.


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


stream_doc({JsonBytes, Atts, Boundary, Len}) ->
    case erlang:erase({doc_streamer, Boundary}) of
    Pid when is_pid(Pid) ->
        unlink(Pid),
        exit(Pid, kill);
    _ ->
        ok
    end,
    Self = self(),
    DocStreamer = spawn_link(fun() ->
        couch_doc:doc_to_multi_part_stream(
            Boundary, JsonBytes, Atts,
            fun(Data) ->
                receive {get_data, Ref, From} ->
                    From ! {data, Ref, Data}
                end
            end, true),
        unlink(Self)
    end),
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
