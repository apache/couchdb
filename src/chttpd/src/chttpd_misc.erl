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
    handle_deleted_dbs_req/1,
    handle_favicon_req/1,
    handle_favicon_req/2,
    handle_replicate_req/1,
    handle_reload_query_servers_req/1,
    handle_task_status_req/1,
    handle_up_req/1,
    handle_utils_dir_req/1,
    handle_utils_dir_req/2,
    handle_uuids_req/1,
    handle_welcome_req/1,
    handle_welcome_req/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-import(chttpd,
    [send_json/2,send_json/3,send_method_not_allowed/2,
    send_chunk/2,start_chunked_response/3]).

-define(MAX_DB_NUM_FOR_DBS_INFO, 100).

% httpd global handlers

handle_welcome_req(Req) ->
    handle_welcome_req(Req, <<"Welcome">>).

handle_welcome_req(#httpd{method='GET'}=Req, WelcomeMessage) ->
    send_json(Req, {[
        {couchdb, WelcomeMessage},
        {version, list_to_binary(couch_server:get_version())},
        {git_sha, list_to_binary(couch_server:get_git_sha())},
        {uuid, couch_server:get_uuid()},
        {features, get_features()}
        ] ++ case config:get("vendor") of
        [] ->
            [];
        Properties ->
            [{vendor, {[{?l2b(K), ?l2b(V)} || {K, V} <- Properties]}}]
        end
    });
handle_welcome_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

get_features() ->
    case clouseau_rpc:connected() of
        true ->
            [search | config:features()];
        false ->
            config:features()
    end.

handle_favicon_req(Req) ->
    handle_favicon_req(Req, get_docroot()).

handle_favicon_req(#httpd{method='GET'}=Req, DocumentRoot) ->
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

handle_utils_dir_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    "/" ++ UrlPath = chttpd:path(Req),
    case chttpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        % GET /_utils/path or GET /_utils/
        CachingHeaders = [{"Cache-Control", "private, must-revalidate"}],
        EnableCsp = config:get("csp", "enable", "false"),
        Headers = maybe_add_csp_headers(CachingHeaders, EnableCsp),
        chttpd:serve_file(Req, RelativePath, DocumentRoot, Headers);
    {_ActionKey, "", _RelativePath} ->
        % GET /_utils
        RedirectPath = chttpd:path(Req) ++ "/",
        chttpd:send_redirect(Req, RedirectPath)
    end;
handle_utils_dir_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

maybe_add_csp_headers(Headers, "true") ->
    DefaultValues = "default-src 'self'; img-src 'self' data:; font-src 'self'; "
                    "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';",
    Value = config:get("csp", "header_value", DefaultValues),
    [{"Content-Security-Policy", Value} | Headers];
maybe_add_csp_headers(Headers, _) ->
    Headers.

handle_all_dbs_req(#httpd{method='GET'}=Req) ->
    #mrargs{
        start_key = StartKey,
        end_key = EndKey,
        direction = Dir,
        limit = Limit,
        skip = Skip
    } = couch_mrview_http:parse_params(Req, undefined),

    Options = [
        {start_key, StartKey},
        {end_key, EndKey},
        {dir, Dir},
        {limit, Limit},
        {skip, Skip}
    ],

    {ok, Resp} = chttpd:start_delayed_json_response(Req, 200, []),
    Callback = fun all_dbs_callback/2,
    Acc = #vacc{req=Req,resp=Resp},
    {ok, Acc1} = fabric2_db:list_dbs(Callback, Acc, Options),
    {ok, Acc1#vacc.resp};
handle_all_dbs_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

all_dbs_callback({meta, _Meta}, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "["),
    {ok, Acc#vacc{resp=Resp1}};
all_dbs_callback({row, Row}, #vacc{resp=Resp0}=Acc) ->
    Prepend = couch_mrview_http:prepend_val(Acc),
    DbName = couch_util:get_value(id, Row),
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, [Prepend, ?JSON_ENCODE(DbName)]),
    {ok, Acc#vacc{prepend=",", resp=Resp1}};
all_dbs_callback(complete, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "]"),
    {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
    {ok, Acc#vacc{resp=Resp2}};
all_dbs_callback({error, Reason}, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp0, Reason),
    {ok, Acc#vacc{resp=Resp1}}.

handle_dbs_info_req(#httpd{method = 'GET'} = Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    send_db_infos(Req, list_dbs_info);
handle_dbs_info_req(#httpd{method='POST', user_ctx=UserCtx}=Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = chttpd:json_body_obj(Req),
    Keys = couch_mrview_util:get_view_keys(Props),
    case Keys of
        undefined -> throw({bad_request, "`keys` member must exist."});
        _ -> ok
    end,
    MaxNumber = config:get_integer("chttpd",
        "max_db_number_for_dbs_info_req", ?MAX_DB_NUM_FOR_DBS_INFO),
    case length(Keys) =< MaxNumber of
        true -> ok;
        false -> throw({bad_request, too_many_keys})
    end,
    {ok, Resp} = chttpd:start_json_response(Req, 200),
    send_chunk(Resp, "["),
    lists:foldl(fun(DbName, AccSeparator) ->
        try
            {ok, Db} = fabric2_db:open(DbName, [{user_ctx, UserCtx}]),
            {ok, Info} = fabric2_db:get_db_info(Db),
            Json = ?JSON_ENCODE({[{key, DbName}, {info, {Info}}]}),
            send_chunk(Resp, AccSeparator ++ Json)
        catch error:database_does_not_exist ->
            ErrJson = ?JSON_ENCODE({[{key, DbName}, {error, not_found}]}),
            send_chunk(Resp, AccSeparator ++ ErrJson)
        end,
        "," % AccSeparator now has a comma
    end, "", Keys),
    send_chunk(Resp, "]"),
    chttpd:end_json_response(Resp);
handle_dbs_info_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD,POST").

handle_deleted_dbs_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    send_db_infos(Req, list_deleted_dbs_info);
handle_deleted_dbs_req(#httpd{method='POST', user_ctx=Ctx, path_parts=[_]}=Req) ->
    couch_httpd:verify_is_server_admin(Req),
    chttpd:validate_ctype(Req, "application/json"),
    GetJSON = fun(Key, Props, Default) ->
        case couch_util:get_value(Key, Props) of
            undefined when Default == error ->
                Fmt = "POST body must include `~s` parameter.",
                Msg = io_lib:format(Fmt, [Key]),
                throw({bad_request, iolist_to_binary(Msg)});
            undefined ->
                Default;
            Value ->
                Value
        end
    end,
    {BodyProps} = chttpd:json_body_obj(Req),
    {UndeleteProps} = GetJSON(<<"undelete">>, BodyProps, error),
    DbName = GetJSON(<<"source">>, UndeleteProps, error),
    TimeStamp = GetJSON(<<"timestamp">>, UndeleteProps, error),
    TgtDbName = GetJSON(<<"target">>, UndeleteProps, DbName),
    case fabric2_db:undelete(DbName, TgtDbName, TimeStamp, [{user_ctx, Ctx}]) of
        ok ->
            send_json(Req, 200, {[{ok, true}]});
        {error, file_exists} ->
            chttpd:send_error(Req, file_exists);
        {error, not_found} ->
            chttpd:send_error(Req, not_found);
        Error ->
            throw(Error)
    end;
handle_deleted_dbs_req(#httpd{path_parts = PP}=Req) when length(PP) == 1 ->
    send_method_not_allowed(Req, "GET,HEAD,POST");
handle_deleted_dbs_req(#httpd{method='DELETE', user_ctx=Ctx, path_parts=[_, DbName]}=Req) ->
    couch_httpd:verify_is_server_admin(Req),
    TS = case ?JSON_DECODE(couch_httpd:qs_value(Req, "timestamp", "null")) of
        null ->
            throw({bad_request, "`timestamp` parameter is not provided."});
        TS0 ->
            TS0
    end,
    case fabric2_db:delete(DbName, [{user_ctx, Ctx}, {deleted_at, TS}]) of
        ok ->
            send_json(Req, 200, {[{ok, true}]});
        {error, not_found} ->
            chttpd:send_error(Req, not_found);
        Error ->
            throw(Error)
    end;
handle_deleted_dbs_req(#httpd{path_parts = PP}=Req) when length(PP) == 2 ->
    send_method_not_allowed(Req, "HEAD,DELETE");
handle_deleted_dbs_req(Req) ->
    chttpd:send_error(Req, not_found).

send_db_infos(Req, ListFunctionName) ->
    #mrargs{
        start_key = StartKey,
        end_key = EndKey,
        direction = Dir,
        limit = Limit,
        skip = Skip
    } = couch_mrview_http:parse_params(Req, undefined),

    Options = [
        {start_key, StartKey},
        {end_key, EndKey},
        {dir, Dir},
        {limit, Limit},
        {skip, Skip}
    ],

    % TODO: Figure out if we can't calculate a valid
    % ETag for this request. \xFFmetadataVersion won't
    % work as we don't bump versions on size changes

    {ok, Resp1} = chttpd:start_delayed_json_response(Req, 200, []),
    Callback = fun dbs_info_callback/2,
    Acc = #vacc{req = Req, resp = Resp1},
    {ok, Resp2} = fabric2_db:ListFunctionName(Callback, Acc, Options),
    case is_record(Resp2, vacc) of
        true -> {ok, Resp2#vacc.resp};
        _ -> {ok, Resp2}
    end.

dbs_info_callback({meta, _Meta}, #vacc{resp = Resp0} = Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "["),
    {ok, Acc#vacc{resp = Resp1}};
dbs_info_callback({row, Props}, #vacc{resp = Resp0} = Acc) ->
    Prepend = couch_mrview_http:prepend_val(Acc),
    Chunk = [Prepend, ?JSON_ENCODE({Props})],
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, Chunk),
    {ok, Acc#vacc{prepend = ",", resp = Resp1}};
dbs_info_callback(complete, #vacc{resp = Resp0} = Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "]"),
    {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
    {ok, Acc#vacc{resp = Resp2}};
dbs_info_callback({error, Reason}, #vacc{resp = Resp0} = Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp0, Reason),
    {ok, Acc#vacc{resp = Resp1}}.

handle_task_status_req(#httpd{method='GET'}=Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    ActiveTasks = fabric2_active_tasks:get_active_tasks(),
    send_json(Req, ActiveTasks);
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_replicate_req(#httpd{method='POST', user_ctx=Ctx, req_body=PostBody} = Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    %% see HACK in chttpd.erl about replication
    case couch_replicator:replicate(PostBody, Ctx) of
        {ok, {continuous, RepId}} ->
            send_json(Req, 202, {[{ok, true}, {<<"_local_id">>, RepId}]});
        {ok, {cancelled, RepId}} ->
            send_json(Req, 200, {[{ok, true}, {<<"_local_id">>, RepId}]});
        {ok, #{} = JsonResults} ->
            send_json(Req, maps:merge(#{<<"ok">> => true}, JsonResults));
        {ok, stopped} ->
            send_json(Req, 200, {[{ok, stopped}]});
        {error, not_found=Error} ->
            chttpd:send_error(Req, Error);
        {error, #{<<"error">> := Err, <<"reason">> := Reason}} when
                is_binary(Err), is_binary(Reason) ->
            % Safe to use binary_to_atom since this is only built
            % from couch_replicator_jobs:error_info/1
            chttpd:send_error(Req, {binary_to_atom(Err, utf8), Reason});
        {error, {_, _}=Error} ->
            chttpd:send_error(Req, Error);
        {_, _}=Error ->
            chttpd:send_error(Req, Error)
    end;
handle_replicate_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_reload_query_servers_req(#httpd{method='POST'}=Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    ok = couch_proc_manager:reload(),
    send_json(Req, 200, {[{ok, true}]});
handle_reload_query_servers_req(Req) ->
    send_method_not_allowed(Req, "POST").

handle_uuids_req(Req) ->
    couch_httpd_misc_handlers:handle_uuids_req(Req).


handle_up_req(#httpd{method='GET'} = Req) ->
    case config:get("couchdb", "maintenance_mode") of
    "true" ->
        send_json(Req, 404, {[{status, maintenance_mode}]});
    "nolb" ->
        send_json(Req, 404, {[{status, nolb}]});
    _ ->
        try
            fabric2_db:list_dbs([{limit, 0}]),
            send_json(Req, 200, {[{status, ok}]})
        catch error:{timeout, _} ->
            send_json(Req, 404, {[{status, backend_unavailable}]})
        end
    end;

handle_up_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

get_docroot() ->
    % if the env var isn’t set, let’s not throw an error, but
    % assume the current working dir is what we want
    os:getenv("COUCHDB_FAUXTON_DOCROOT", "").
