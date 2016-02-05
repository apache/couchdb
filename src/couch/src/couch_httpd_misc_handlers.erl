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

-module(couch_httpd_misc_handlers).

-export([handle_welcome_req/2,handle_favicon_req/2,handle_utils_dir_req/2,
    handle_all_dbs_req/1,handle_restart_req/1,
    handle_uuids_req/1,handle_config_req/1,
    handle_task_status_req/1, handle_file_req/2]).


-include_lib("couch/include/couch_db.hrl").

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, send_error/4]).

% httpd global handlers

handle_welcome_req(#httpd{method='GET'}=Req, WelcomeMessage) ->
    send_json(Req, {[
        {couchdb, WelcomeMessage},
        {uuid, couch_server:get_uuid()},
        {version, list_to_binary(couch_server:get_version())}
        ] ++ case config:get("vendor") of
        [] ->
            [];
        Properties ->
            [{vendor, {[{?l2b(K), ?l2b(V)} || {K, V} <- Properties]}}]
        end
    });
handle_welcome_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_favicon_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    {{Year,Month,Day},Time} = erlang:universaltime(),
    OneYearFromNow = {{Year+1,Month,Day},Time},
    CachingHeaders = [
        %favicon should expire a year from now
        {"Cache-Control", "public, max-age=31536000"},
        {"Expires", couch_util:rfc1123_date(OneYearFromNow)}
    ],
    couch_httpd:serve_file(Req, "favicon.ico", DocumentRoot, CachingHeaders);

handle_favicon_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_file_req(#httpd{method='GET'}=Req, Document) ->
    couch_httpd:serve_file(Req, filename:basename(Document), filename:dirname(Document));

handle_file_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_utils_dir_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    "/" ++ UrlPath = couch_httpd:path(Req),
    case couch_httpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        % GET /_utils/path or GET /_utils/
        CachingHeaders = [{"Cache-Control", "private, must-revalidate"}],
        EnableCsp = config:get("csp", "enable", "false"),
        Headers = maybe_add_csp_headers(CachingHeaders, EnableCsp),
        couch_httpd:serve_file(Req, RelativePath, DocumentRoot, Headers);
    {_ActionKey, "", _RelativePath} ->
        % GET /_utils
        RedirectPath = couch_httpd:path(Req) ++ "/",
        couch_httpd:send_redirect(Req, RedirectPath)
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
    {ok, DbNames} = couch_server:all_databases(),
    send_json(Req, DbNames);
handle_all_dbs_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").


handle_task_status_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    % convert the list of prop lists to a list of json objects
    send_json(Req, [{Props} || Props <- couch_task_status:all()]);
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").


handle_restart_req(#httpd{method='GET', path_parts=[_, <<"token">>]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Token = case application:get_env(couch, instance_token) of
        {ok, Tok} ->
            Tok;
        _ ->
            Tok = erlang:phash2(make_ref()),
            application:set_env(couch, instance_token, Tok),
            Tok
    end,
    send_json(Req, 200, {[{token, Token}]});
handle_restart_req(#httpd{method='POST'}=Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = couch_httpd:verify_is_server_admin(Req),
    Result = send_json(Req, 202, {[{ok, true}]}),
    couch:restart(),
    Result;
handle_restart_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_uuids_req(#httpd{method='GET'}=Req) ->
    Max = list_to_integer(config:get("uuids","max_count","1000")),
    Count = try list_to_integer(couch_httpd:qs_value(Req, "count", "1")) of
        N when N > Max ->
            throw({bad_request, <<"count parameter too large">>});
        N when N < 0 ->
            throw({bad_request, <<"count must be a positive integer">>});
        N -> N
    catch
        error:badarg ->
            throw({bad_request, <<"count must be a positive integer">>})
    end,
    UUIDs = [couch_uuids:new() || _ <- lists:seq(1, Count)],
    Etag = couch_httpd:make_etag(UUIDs),
    couch_httpd:etag_respond(Req, Etag, fun() ->
        CacheBustingHeaders = [
            {"Date", couch_util:rfc1123_date()},
            {"Cache-Control", "no-cache"},
            % Past date, ON PURPOSE!
            {"Expires", "Mon, 01 Jan 1990 00:00:00 GMT"},
            {"Pragma", "no-cache"},
            {"ETag", Etag}
        ],
        send_json(Req, 200, CacheBustingHeaders, {[{<<"uuids">>, UUIDs}]})
    end);
handle_uuids_req(Req) ->
    send_method_not_allowed(Req, "GET").


% Config request handler


% GET /_config/
% GET /_config
handle_config_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Grouped = lists:foldl(fun({{Section, Key}, Value}, Acc) ->
        case dict:is_key(Section, Acc) of
        true ->
            dict:append(Section, {list_to_binary(Key), list_to_binary(Value)}, Acc);
        false ->
            dict:store(Section, [{list_to_binary(Key), list_to_binary(Value)}], Acc)
        end
    end, dict:new(), config:all()),
    KVs = dict:fold(fun(Section, Values, Acc) ->
        [{list_to_binary(Section), {Values}} | Acc]
    end, [], Grouped),
    send_json(Req, 200, {KVs});
% GET /_config/Section
handle_config_req(#httpd{method='GET', path_parts=[_,Section]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    KVs = [{list_to_binary(Key), list_to_binary(Value)}
            || {Key, Value} <- config:get(Section)],
    send_json(Req, 200, {KVs});
% GET /_config/Section/Key
handle_config_req(#httpd{method='GET', path_parts=[_, Section, Key]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    case config:get(Section, Key, undefined) of
    undefined ->
        throw({not_found, unknown_config_value});
    Value ->
        send_json(Req, 200, list_to_binary(Value))
    end;
% POST /_config/_reload - Flushes unpersisted config values from RAM
handle_config_req(#httpd{method='POST', path_parts=[_, <<"_reload">>]}=Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    _ = couch_httpd:body(Req),
    ok = couch_httpd:verify_is_server_admin(Req),
    ok = config:reload(),
    send_json(Req, 200, {[{ok, true}]});
% PUT or DELETE /_config/Section/Key
handle_config_req(#httpd{method=Method, path_parts=[_, Section, Key]}=Req)
      when (Method == 'PUT') or (Method == 'DELETE') ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Persist = couch_httpd:header_value(Req, "X-Couch-Persist") /= "false",
    case config:get("httpd", "config_whitelist", undefined) of
        undefined ->
            % No whitelist; allow all changes.
            handle_approved_config_req(Req, Persist);
        WhitelistValue ->
            % Provide a failsafe to protect against inadvertently locking
            % onesself out of the config by supplying a syntactically-incorrect
            % Erlang term. To intentionally lock down the whitelist, supply a
            % well-formed list which does not include the whitelist config
            % variable itself.
            FallbackWhitelist = [{<<"httpd">>, <<"config_whitelist">>}],

            Whitelist = case couch_util:parse_term(WhitelistValue) of
                {ok, Value} when is_list(Value) ->
                    Value;
                {ok, _NonListValue} ->
                    FallbackWhitelist;
                {error, _} ->
                    [{WhitelistSection, WhitelistKey}] = FallbackWhitelist,
                    couch_log:error("Only whitelisting ~s/~s due to error"
                                    " parsing: ~p",
                                    [WhitelistSection, WhitelistKey,
                                     WhitelistValue]),
                    FallbackWhitelist
            end,

            IsRequestedKeyVal = fun(Element) ->
                case Element of
                    {A, B} ->
                        % For readability, tuples may be used instead of binaries
                        % in the whitelist.
                        case {couch_util:to_binary(A), couch_util:to_binary(B)} of
                            {Section, Key} ->
                                true;
                            {Section, <<"*">>} ->
                                true;
                            _Else ->
                                false
                        end;
                    _Else ->
                        false
                end
            end,

            case lists:any(IsRequestedKeyVal, Whitelist) of
                true ->
                    % Allow modifying this whitelisted variable.
                    handle_approved_config_req(Req, Persist);
                _NotWhitelisted ->
                    % Disallow modifying this non-whitelisted variable.
                    send_error(Req, 400, <<"modification_not_allowed">>,
                               ?l2b("This config variable is read-only"))
            end
    end;
handle_config_req(Req) ->
    send_method_not_allowed(Req, "GET,PUT,POST,DELETE").

% PUT /_config/Section/Key
% "value"
handle_approved_config_req(Req, Persist) ->
    Query = couch_httpd:qs(Req),
    UseRawValue = case lists:keyfind("raw", 1, Query) of
    false            -> false; % Not specified
    {"raw", ""}      -> false; % Specified with no value, i.e. "?raw" and "?raw="
    {"raw", "false"} -> false;
    {"raw", "true"}  -> true;
    {"raw", InvalidValue} -> InvalidValue
    end,
    handle_approved_config_req(Req, Persist, UseRawValue).

handle_approved_config_req(#httpd{method='PUT', path_parts=[_, Section, Key]}=Req,
                           Persist, UseRawValue)
        when UseRawValue =:= false orelse UseRawValue =:= true ->
    RawValue = couch_httpd:json_body(Req),
    Value = case UseRawValue of
    true ->
        % Client requests no change to the provided value.
        RawValue;
    false ->
        % Pre-process the value as necessary.
        case Section of
        <<"admins">> ->
            couch_passwords:hash_admin_password(RawValue);
        _ ->
            RawValue
        end
    end,
    OldValue = config:get(Section, Key, ""),
    case config:set(Section, Key, ?b2l(Value), Persist) of
    ok ->
        send_json(Req, 200, list_to_binary(OldValue));
    Error ->
        throw(Error)
    end;

handle_approved_config_req(#httpd{method='PUT'}=Req, _Persist, UseRawValue) ->
    Err = io_lib:format("Bad value for 'raw' option: ~s", [UseRawValue]),
    send_json(Req, 400, {[{error, ?l2b(Err)}]});

% DELETE /_config/Section/Key
handle_approved_config_req(#httpd{method='DELETE',path_parts=[_,Section,Key]}=Req,
                           Persist, _UseRawValue) ->
    case config:get(Section, Key, undefined) of
    undefined ->
        throw({not_found, unknown_config_value});
    OldValue ->
        config:delete(Section, Key, Persist),
        send_json(Req, 200, list_to_binary(OldValue))
    end.

