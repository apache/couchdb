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

-module(chttpd_misc).

-export([
    handle_all_dbs_req/1,
    handle_dbs_info_req/1,
    handle_favicon_req/1,
    handle_favicon_req/2,
    handle_replicate_req/1,
    handle_reload_query_servers_req/1,
    handle_task_status_req/1,
    handle_resource_status_req/1,
    handle_up_req/1,
    handle_utils_dir_req/1,
    handle_utils_dir_req/2,
    handle_uuids_req/1,
    handle_welcome_req/1,
    handle_welcome_req/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-import(
    chttpd,
    [
        send_json/2, send_json/3,
        send_method_not_allowed/2,
        send_chunk/2
    ]
).

-define(MAX_DB_NUM_FOR_DBS_INFO, 100).

% httpd global handlers

handle_welcome_req(Req) ->
    handle_welcome_req(Req, <<"Welcome">>).

handle_welcome_req(#httpd{method = 'GET'} = Req, WelcomeMessage) ->
    send_json(Req, {
        [
            {couchdb, WelcomeMessage},
            {version, list_to_binary(couch_server:get_version())},
            {git_sha, list_to_binary(couch_server:get_git_sha())},
            {uuid, couch_server:get_uuid()},
            {features, get_features()}
        ] ++
            case config:get("vendor") of
                [] ->
                    [];
                Properties ->
                    [{vendor, {[{?l2b(K), ?l2b(V)} || {K, V} <- Properties]}}]
            end
    });
handle_welcome_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

get_features() ->
    case dreyfus:available() of
        true -> [search];
        false -> []
    end ++
        case nouveau:enabled() of
            true -> [nouveau];
            false -> []
        end ++
        config:features().

handle_favicon_req(Req) ->
    handle_favicon_req(Req, get_docroot()).

handle_favicon_req(#httpd{method = 'GET'} = Req, DocumentRoot) ->
    {DateNow, TimeNow} = calendar:universal_time(),
    DaysNow = calendar:date_to_gregorian_days(DateNow),
    DaysWhenExpires = DaysNow + 365,
    DateWhenExpires = calendar:gregorian_days_to_date(DaysWhenExpires),
    CachingHeaders = [
        %favicon should expire a year from now
        {"Cache-Control", "public, max-age=31536000"},
        {"Expires", couch_util:rfc1123_date({DateWhenExpires, TimeNow})}
    ],
    chttpd:serve_file(Req, "favicon.ico", DocumentRoot, CachingHeaders);
handle_favicon_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_utils_dir_req(Req) ->
    handle_utils_dir_req(Req, get_docroot()).

handle_utils_dir_req(#httpd{method = 'GET'} = Req, DocumentRoot) ->
    "/" ++ UrlPath = chttpd:path(Req),
    case chttpd:partition(UrlPath) of
        {_ActionKey, "/", RelativePath} ->
            % GET /_utils/path or GET /_utils/
            CachingHeaders = [{"Cache-Control", "private, must-revalidate"}],
            DefaultValues =
                "child-src 'self' data: blob:; default-src 'self'; img-src 'self' data:; font-src 'self'; "
                "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';",
            Headers = chttpd_util:maybe_add_csp_header("utils", CachingHeaders, DefaultValues),
            chttpd:serve_file(Req, RelativePath, DocumentRoot, Headers);
        {_ActionKey, "", _RelativePath} ->
            % GET /_utils
            RedirectPath = chttpd:path(Req) ++ "/",
            chttpd:send_redirect(Req, RedirectPath)
    end;
handle_utils_dir_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_all_dbs_req(#httpd{method = 'GET'} = Req) ->
    handle_all_dbs_info_req(Req);
handle_all_dbs_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_all_dbs_info_req(Req) ->
    Args0 = couch_mrview_http:parse_params(Req, undefined),
    Args1 = couch_mrview_util:set_extra(Args0, namespace, <<"_non_design">>),
    ShardDbName = config:get("mem3", "shards_db", "_dbs"),
    %% shard_db is not sharded but mem3:shards treats it as an edge case
    %% so it can be pushed thru fabric
    {ok, Info} = fabric:get_db_info(ShardDbName),
    Etag = couch_httpd:make_etag({Info}),
    Options = [{user_ctx, Req#httpd.user_ctx}],
    {ok, Resp} = chttpd:etag_respond(Req, Etag, fun() ->
        {ok, Resp} = chttpd:start_delayed_json_response(Req, 200, [{"ETag", Etag}]),
        VAcc = #vacc{req = Req, resp = Resp},
        fabric:all_docs(ShardDbName, Options, fun all_dbs_info_callback/2, VAcc, Args1)
    end),
    case is_record(Resp, vacc) of
        true -> {ok, Resp#vacc.resp};
        _ -> {ok, Resp}
    end.

all_dbs_info_callback({meta, _Meta}, #vacc{resp = Resp0} = Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "["),
    {ok, Acc#vacc{resp = Resp1}};
all_dbs_info_callback({row, Row}, #vacc{resp = Resp0} = Acc) when
    Acc#vacc.req#httpd.path_parts =:= [<<"_all_dbs">>]
->
    Prepend = couch_mrview_http:prepend_val(Acc),
    DbName = couch_util:get_value(id, Row),
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, [Prepend, ?JSON_ENCODE(DbName)]),
    {ok, Acc#vacc{prepend = ",", resp = Resp1}};
all_dbs_info_callback({row, Row}, #vacc{resp = Resp0} = Acc) when
    Acc#vacc.req#httpd.path_parts =:= [<<"_dbs_info">>]
->
    Prepend = couch_mrview_http:prepend_val(Acc),
    DbName = couch_util:get_value(id, Row),
    case chttpd_util:get_db_info(DbName) of
        {ok, DbInfo} ->
            Chunk = [Prepend, ?JSON_ENCODE({[{key, DbName}, {info, {DbInfo}}]})],
            {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, Chunk),
            {ok, Acc#vacc{prepend = ",", resp = Resp1}};
        {error, database_does_not_exist} ->
            {ok, Acc#vacc{resp = Resp0}};
        {error, Reason} ->
            {ok, Resp1} = chttpd:send_delayed_error(Resp0, Reason),
            {stop, Acc#vacc{resp = Resp1}}
    end;
all_dbs_info_callback(complete, #vacc{resp = Resp0} = Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "]"),
    {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
    {ok, Acc#vacc{resp = Resp2}};
all_dbs_info_callback({error, Reason}, #vacc{resp = Resp0} = Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp0, Reason),
    {ok, Acc#vacc{resp = Resp1}}.

handle_dbs_info_req(#httpd{method = 'GET'} = Req) ->
    handle_all_dbs_info_req(Req);
handle_dbs_info_req(#httpd{method = 'POST'} = Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = chttpd:json_body_obj(Req),
    Keys = couch_mrview_util:get_view_keys(Props),
    case Keys of
        undefined -> throw({bad_request, "`keys` member must exist."});
        _ -> ok
    end,
    MaxNumber = config:get_integer(
        "chttpd",
        "max_db_number_for_dbs_info_req",
        ?MAX_DB_NUM_FOR_DBS_INFO
    ),
    case length(Keys) =< MaxNumber of
        true -> ok;
        false -> throw({bad_request, too_many_keys})
    end,
    {ok, Resp} = chttpd:start_json_response(Req, 200),
    send_chunk(Resp, "["),
    lists:foldl(
        fun(DbName, AccSeparator) ->
            case catch fabric:get_db_info(DbName) of
                {ok, Result} ->
                    Json = ?JSON_ENCODE({[{key, DbName}, {info, {Result}}]}),
                    send_chunk(Resp, AccSeparator ++ Json);
                _ ->
                    Json = ?JSON_ENCODE({[{key, DbName}, {error, not_found}]}),
                    send_chunk(Resp, AccSeparator ++ Json)
            end,
            % AccSeparator now has a comma
            ","
        end,
        "",
        Keys
    ),
    send_chunk(Resp, "]"),
    chttpd:end_json_response(Resp);
handle_dbs_info_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST").

handle_task_status_req(#httpd{method = 'GET'} = Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    TaskList = chttpd_util:get_active_tasks(nodes([visible, this])),
    send_json(Req, lists:sort(TaskList));
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_resource_status_req(#httpd{method = 'POST'} = Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    chttpd:validate_ctype(Req, "application/json"),
    {Props} = chttpd:json_body_obj(Req),
    Action = proplists:get_value(<<"action">>, Props),
    Key = proplists:get_value(<<"key">>, Props),
    Val = proplists:get_value(<<"val">>, Props),

    CountBy = fun couch_stats_resource_tracker:count_by/1,
    GroupBy = fun couch_stats_resource_tracker:group_by/2,
    SortedBy1 = fun couch_stats_resource_tracker:sorted_by/1,
    SortedBy2 = fun couch_stats_resource_tracker:sorted_by/2,
    ConvertEle = fun(K) -> list_to_existing_atom(binary_to_list(K)) end,
    ConvertList = fun(L) -> [ConvertEle(E) || E <- L] end,
    ToJson = fun couch_stats_resource_tracker:term_to_flat_json/1,
    JsonKeys = fun(PL) -> [[ToJson(K), V] || {K, V} <- PL] end,

    Fun = case {Action, Key, Val} of
        {<<"count_by">>, Keys, undefined} when is_list(Keys) ->
            Keys1 = [ConvertEle(K) || K <- Keys],
            fun() -> CountBy(Keys1) end;
        {<<"count_by">>, Key, undefined} ->
            Key1 = ConvertEle(Key),
            fun() -> CountBy(Key1) end;
        {<<"group_by">>, Keys, Vals} when is_list(Keys) andalso is_list(Vals) ->
            Keys1 = ConvertList(Keys),
            Vals1 = ConvertList(Vals),
            fun() -> GroupBy(Keys1, Vals1) end;
        {<<"group_by">>, Key, Vals} when is_list(Vals) ->
            Key1 = ConvertEle(Key),
            Vals1 = ConvertList(Vals),
            fun() -> GroupBy(Key1, Vals1) end;
        {<<"group_by">>, Keys, Val} when is_list(Keys) ->
            Keys1 = ConvertList(Keys),
            Val1 = ConvertEle(Val),
            fun() -> GroupBy(Keys1, Val1) end;
        {<<"group_by">>, Key, Val} ->
            Key1 = ConvertEle(Key),
            Val1 = ConvertList(Val),
            fun() -> GroupBy(Key1, Val1) end;

        {<<"sorted_by">>, Key, undefined} ->
            Key1 = ConvertEle(Key),
            fun() -> JsonKeys(SortedBy1(Key1)) end;
        {<<"sorted_by">>, Keys, undefined} when is_list(Keys) ->
            Keys1 = [ConvertEle(K) || K <- Keys],
            fun() -> JsonKeys(SortedBy1(Keys1)) end;
        {<<"sorted_by">>, Keys, Vals} when is_list(Keys) andalso is_list(Vals) ->
            Keys1 = ConvertList(Keys),
            Vals1 = ConvertList(Vals),
            fun() -> JsonKeys(SortedBy2(Keys1, Vals1)) end;
        {<<"sorted_by">>, Key, Vals} when is_list(Vals) ->
            Key1 = ConvertEle(Key),
            Vals1 = ConvertList(Vals),
            fun() -> JsonKeys(SortedBy2(Key1, Vals1)) end;
        {<<"sorted_by">>, Keys, Val} when is_list(Keys) ->
            Keys1 = ConvertList(Keys),
            Val1 = ConvertEle(Val),
            fun() -> JsonKeys(SortedBy2(Keys1, Val1)) end;
        {<<"sorted_by">>, Key, Val} ->
            Key1 = ConvertEle(Key),
            Val1 = ConvertList(Val),
            fun() -> JsonKeys(SortedBy2(Key1, Val1)) end;
        _ ->
            throw({badrequest, invalid_resource_request})
    end,

    Fun1 = fun() ->
        case Fun() of
            Map when is_map(Map) ->
                {maps:fold(
                    fun
                        (_K,0,A) -> A; %% TODO: Skip 0 value entries?
                        (K,V,A) -> [{ToJson(K), V} | A]
                    end,
                    [], Map)};
            List when is_list(List) ->
                List
        end
    end,

    {Resp, _Bad} = rpc:multicall(erlang, apply, [
        fun() ->
            {node(), Fun1()}
        end,
        []
    ]),
    %%io:format("{CSRT}***** GOT RESP: ~p~n", [Resp]),
    send_json(Req, {Resp});
handle_resource_status_req(#httpd{method = 'GET'} = Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    {Resp, Bad} = rpc:multicall(erlang, apply, [
        fun() ->
            {node(), couch_stats_resource_tracker:active()}
        end,
        []
    ]),
    %% TODO: incorporate Bad responses
    io:format("ACTIVE RESP: ~p~nBAD RESP: ~p~n", [Resp, Bad]),
    send_json(Req, {Resp});
handle_resource_status_req(Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    send_method_not_allowed(Req, "GET,HEAD,POST").

handle_replicate_req(#httpd{method = 'POST', user_ctx = Ctx, req_body = PostBody} = Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    %% see HACK in chttpd.erl about replication
    case replicate(PostBody, Ctx) of
        {ok, {continuous, RepId}} ->
            send_json(Req, 202, {[{ok, true}, {<<"_local_id">>, RepId}]});
        {ok, {cancelled, RepId}} ->
            send_json(Req, 200, {[{ok, true}, {<<"_local_id">>, RepId}]});
        {ok, {JsonResults}} ->
            send_json(Req, {[{ok, true} | JsonResults]});
        {ok, stopped} ->
            send_json(Req, 200, {[{ok, stopped}]});
        {error, not_found = Error} ->
            chttpd:send_error(Req, Error);
        {error, {_, _} = Error} ->
            chttpd:send_error(Req, Error);
        {_, _} = Error ->
            chttpd:send_error(Req, Error)
    end;
handle_replicate_req(Req) ->
    send_method_not_allowed(Req, "POST").

replicate({Props} = PostBody, Ctx) ->
    case couch_util:get_value(<<"cancel">>, Props) of
        true ->
            cancel_replication(PostBody, Ctx);
        _ ->
            Node = choose_node([
                couch_util:get_value(<<"source">>, Props),
                couch_util:get_value(<<"target">>, Props)
            ]),
            case rpc:call(Node, couch_replicator, replicate, [PostBody, Ctx]) of
                {badrpc, Reason} ->
                    erlang:error(Reason);
                Res ->
                    Res
            end
    end.

cancel_replication(PostBody, Ctx) ->
    {Res, _Bad} = rpc:multicall(couch_replicator, replicate, [PostBody, Ctx]),
    case [X || {ok, {cancelled, _}} = X <- Res] of
        [Success | _] ->
            % Report success if at least one node canceled the replication
            Success;
        [] ->
            case lists:usort(Res) of
                [UniqueReply] ->
                    % Report a universally agreed-upon reply
                    UniqueReply;
                [] ->
                    {error, badrpc};
                Else ->
                    % Unclear what to do here -- pick the first error?
                    % Except try ignoring any {error, not_found} responses
                    % because we'll always get two of those
                    hd(Else -- [{error, not_found}])
            end
    end.

choose_node(Key) when is_binary(Key) ->
    Checksum = erlang:crc32(Key),
    Nodes = lists:sort([node() | erlang:nodes()]),
    lists:nth(1 + Checksum rem length(Nodes), Nodes);
choose_node(Key) ->
    choose_node(?term_to_bin(Key)).

handle_reload_query_servers_req(#httpd{method = 'POST'} = Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    ok = couch_proc_manager:reload(),
    send_json(Req, 200, {[{ok, true}]});
handle_reload_query_servers_req(Req) ->
    send_method_not_allowed(Req, "POST").

handle_uuids_req(Req) ->
    couch_httpd_misc_handlers:handle_uuids_req(Req).

handle_up_req(#httpd{method = 'GET'} = Req) ->
    case config:get("couchdb", "maintenance_mode") of
        "true" ->
            send_json(Req, 404, {[{status, maintenance_mode}]});
        "nolb" ->
            send_json(Req, 404, {[{status, nolb}]});
        _ ->
            {ok, {Status}} = mem3_seeds:get_status(),
            case couch_util:get_value(status, Status) of
                ok ->
                    send_json(Req, 200, {Status});
                seeding ->
                    send_json(Req, 404, {Status})
            end
    end;
handle_up_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

get_docroot() ->
    % if the env var isn’t set, let’s not throw an error, but
    % assume the current working dir is what we want
    os:getenv("COUCHDB_FAUXTON_DOCROOT", "").
