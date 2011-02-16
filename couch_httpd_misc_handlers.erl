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
    handle_uuids_req/1,handle_config_req/1,handle_log_req/1,
    handle_task_status_req/1]).

-export([increment_update_seq_req/2]).


-include("couch_db.hrl").

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, send_error/4]).

% httpd global handlers

handle_welcome_req(#httpd{method='GET'}=Req, WelcomeMessage) ->
    send_json(Req, {[
        {couchdb, WelcomeMessage},
        {version, list_to_binary(couch_server:get_version())}
    ]});
handle_welcome_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_favicon_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    {{Year,Month,Day},Time} = erlang:localtime(),
    OneYearFromNow = {{Year+1,Month,Day},Time},
    CachingHeaders = [
        %favicon should expire a year from now
        {"Cache-Control", "public, max-age=31536000"},
        {"Expires", httpd_util:rfc1123_date(OneYearFromNow)}
    ],
    couch_httpd:serve_file(Req, "favicon.ico", DocumentRoot, CachingHeaders);

handle_favicon_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_utils_dir_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    "/" ++ UrlPath = couch_httpd:path(Req),
    case couch_httpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        % GET /_utils/path or GET /_utils/
        couch_httpd:serve_file(Req, RelativePath, DocumentRoot);
    {_ActionKey, "", _RelativePath} ->
        % GET /_utils
        RedirectPath = couch_httpd:path(Req) ++ "/",
        couch_httpd:send_redirect(Req, RedirectPath)
    end;
handle_utils_dir_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

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


handle_restart_req(#httpd{method='POST'}=Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = couch_httpd:verify_is_server_admin(Req),
    couch_server_sup:restart_core_server(),
    send_json(Req, 200, {[{ok, true}]});
handle_restart_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_uuids_req(#httpd{method='GET'}=Req) ->
    Count = list_to_integer(couch_httpd:qs_value(Req, "count", "1")),
    UUIDs = [couch_uuids:new() || _ <- lists:seq(1, Count)],
    Etag = couch_httpd:make_etag(UUIDs),
    couch_httpd:etag_respond(Req, Etag, fun() ->
        CacheBustingHeaders = [
            {"Date", httpd_util:rfc1123_date()},
            {"Cache-Control", "no-cache"},
            % Past date, ON PURPOSE!
            {"Expires", "Fri, 01 Jan 1990 00:00:00 GMT"},
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
    end, dict:new(), couch_config:all()),
    KVs = dict:fold(fun(Section, Values, Acc) ->
        [{list_to_binary(Section), {Values}} | Acc]
    end, [], Grouped),
    send_json(Req, 200, {KVs});
% GET /_config/Section
handle_config_req(#httpd{method='GET', path_parts=[_,Section]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    KVs = [{list_to_binary(Key), list_to_binary(Value)}
            || {Key, Value} <- couch_config:get(Section)],
    send_json(Req, 200, {KVs});
% GET /_config/Section/Key
handle_config_req(#httpd{method='GET', path_parts=[_, Section, Key]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    case couch_config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    Value ->
        send_json(Req, 200, list_to_binary(Value))
    end;
% PUT or DELETE /_config/Section/Key
handle_config_req(#httpd{method=Method, path_parts=[_, Section, Key]}=Req)
      when (Method == 'PUT') or (Method == 'DELETE') ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Persist = couch_httpd:header_value(Req, "X-Couch-Persist") /= "false",
    case couch_config:get(<<"httpd">>, <<"config_whitelist">>, null) of
        null ->
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
                    ?LOG_ERROR("Only whitelisting ~s/~s due to error parsing: ~p",
                               [WhitelistSection, WhitelistKey, WhitelistValue]),
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
    send_method_not_allowed(Req, "GET,PUT,DELETE").

% PUT /_config/Section/Key
% "value"
handle_approved_config_req(#httpd{method='PUT', path_parts=[_, Section, Key]}=Req, Persist) ->
    Value = couch_httpd:json_body(Req),
    OldValue = couch_config:get(Section, Key, ""),
    case couch_config:set(Section, Key, ?b2l(Value), Persist) of
    ok ->
        send_json(Req, 200, list_to_binary(OldValue));
    Error ->
        throw(Error)
    end;
% DELETE /_config/Section/Key
handle_approved_config_req(#httpd{method='DELETE',path_parts=[_,Section,Key]}=Req, Persist) ->
    case couch_config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    OldValue ->
        couch_config:delete(Section, Key, Persist),
        send_json(Req, 200, list_to_binary(OldValue))
    end.


% httpd db handlers

increment_update_seq_req(#httpd{method='POST'}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {ok, NewSeq} = couch_db:increment_update_seq(Db),
    send_json(Req, {[{ok, true},
        {update_seq, NewSeq}
    ]});
increment_update_seq_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

% httpd log handlers

handle_log_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Bytes = list_to_integer(couch_httpd:qs_value(Req, "bytes", "1000")),
    Offset = list_to_integer(couch_httpd:qs_value(Req, "offset", "0")),
    Chunk = couch_log:read(Bytes, Offset),
    {ok, Resp} = start_chunked_response(Req, 200, [
        % send a plaintext response
        {"Content-Type", "text/plain; charset=utf-8"},
        {"Content-Length", integer_to_list(length(Chunk))}
    ]),
    send_chunk(Resp, Chunk),
    last_chunk(Resp);
handle_log_req(Req) ->
    send_method_not_allowed(Req, "GET").


