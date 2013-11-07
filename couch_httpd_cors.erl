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

%% @doc module to handle Cross-Origin Resource Sharing
%%
%% This module handles CORS requests and preflight request for
%% CouchDB. The configuration is done in the ini file.
%%
%% This implements http://www.w3.org/TR/cors/


-module(couch_httpd_cors).

-include("couch_db.hrl").

-export([is_preflight_request/1, cors_headers/2]).

-define(SUPPORTED_HEADERS, "Accept, Accept-Language, Content-Type," ++
        "Expires, Last-Modified, Pragma, Origin, Content-Length," ++
        "If-Match, Destination, X-Requested-With, " ++
        "X-Http-Method-Override, Content-Range").

-define(SUPPORTED_METHODS, "GET, HEAD, POST, PUT, DELETE," ++
        "TRACE, CONNECT, COPY, OPTIONS").

% as defined in http://www.w3.org/TR/cors/#terminology
-define(SIMPLE_HEADERS, ["Cache-Control", "Content-Language",
        "Content-Type", "Expires", "Last-Modified", "Pragma"]).
-define(ALLOWED_HEADERS, lists:sort(["Server", "Etag",
        "Accept-Ranges" | ?SIMPLE_HEADERS])).
-define(SIMPLE_CONTENT_TYPE_VALUES, ["application/x-www-form-urlencoded",
        "multipart/form-data", "text/plain"]).

% TODO: - pick a sane default
-define(CORS_DEFAULT_MAX_AGE, 12345).

%% is_preflight_request/1

% http://www.w3.org/TR/cors/#resource-preflight-requests

is_preflight_request(#httpd{method=Method}=Req) when Method /= 'OPTIONS' ->
    Req;
is_preflight_request(Req) ->
    EnableCors = enable_cors(),
    is_preflight_request(Req, EnableCors).

is_preflight_request(Req, false) ->
    Req;
is_preflight_request(#httpd{mochi_req=MochiReq}=Req, true) ->
    case preflight_request(MochiReq) of
    {ok, PreflightHeaders} ->
        send_preflight_response(Req, PreflightHeaders);
    _ ->
        Req
    end.


preflight_request(MochiReq) ->
    Origin = MochiReq:get_header_value("Origin"),
    preflight_request(MochiReq, Origin).

preflight_request(MochiReq, undefined) ->
    % If the Origin header is not present terminate this set of
    % steps. The request is outside the scope of this specification.
    % http://www.w3.org/TR/cors/#resource-preflight-requests
    MochiReq;
preflight_request(MochiReq, Origin) ->
    Host = couch_httpd_vhost:host(MochiReq),
    AcceptedOrigins = get_accepted_origins(Host),
    AcceptAll = lists:member("*", AcceptedOrigins),

    HandlerFun = fun() ->
        OriginList = couch_util:to_list(Origin),
        handle_preflight_request(OriginList, Host, MochiReq)
    end,

    case AcceptAll of
    true ->
        % Always matching is acceptable since the list of
        % origins can be unbounded.
        % http://www.w3.org/TR/cors/#resource-preflight-requests
        HandlerFun();
    false ->
        case lists:member(Origin, AcceptedOrigins) of
        % The Origin header can only contain a single origin as
        % the user agent will not follow redirects.
        % http://www.w3.org/TR/cors/#resource-preflight-requests
        % TODO: Square against multi origin thinger in Security Considerations
        true ->
            HandlerFun();
        false ->
            % If the value of the Origin header is not a
            % case-sensitive match for any of the values
            % in list of origins do not set any additional
            % headers and terminate this set of steps.
            % http://www.w3.org/TR/cors/#resource-preflight-requests
            false
        end
    end.


handle_preflight_request(Origin, Host, MochiReq) ->
    %% get supported methods
    SupportedMethods = split_list(cors_config(Host, "methods",
                                              ?SUPPORTED_METHODS)),

    % get supported headers
    AllSupportedHeaders = split_list(cors_config(Host, "headers",
                                                 ?SUPPORTED_HEADERS)),

    SupportedHeaders = [string:to_lower(H) || H <- AllSupportedHeaders],

    % get max age
    MaxAge = cors_config(Host, "max_age", ?CORS_DEFAULT_MAX_AGE),

    PreflightHeaders0 = maybe_add_credentials(Origin, Host, [
        {"Access-Control-Allow-Origin", Origin},
        {"Access-Control-Max-Age", MaxAge},
        {"Access-Control-Allow-Methods",
            string:join(SupportedMethods, ", ")}]),

    case MochiReq:get_header_value("Access-Control-Request-Method") of
    undefined ->
        % If there is no Access-Control-Request-Method header
        % or if parsing failed, do not set any additional headers
        % and terminate this set of steps. The request is outside
        % the scope of this specification.
        % http://www.w3.org/TR/cors/#resource-preflight-requests
        {ok, PreflightHeaders0};
    Method ->
        case lists:member(Method, SupportedMethods) of
        true ->
            % method ok , check headers
            AccessHeaders = MochiReq:get_header_value(
                    "Access-Control-Request-Headers"),
            {FinalReqHeaders, ReqHeaders} = case AccessHeaders of
                undefined -> {"", []};
                Headers ->
                    % transform header list in something we
                    % could check. make sure everything is a
                    % list
                    RH = [string:to_lower(H)
                          || H <- split_headers(Headers)],
                    {Headers, RH}
            end,
            % check if headers are supported
            case ReqHeaders -- SupportedHeaders of
            [] ->
                PreflightHeaders = PreflightHeaders0 ++
                                   [{"Access-Control-Allow-Headers",
                                     FinalReqHeaders}],
                {ok, PreflightHeaders};
            _ ->
                false
            end;
        false ->
        % If method is not a case-sensitive match for any of
        % the values in list of methods do not set any additional
        % headers and terminate this set of steps.
        % http://www.w3.org/TR/cors/#resource-preflight-requests
            false
        end
    end.


send_preflight_response(#httpd{mochi_req=MochiReq}=Req, Headers) ->
    couch_httpd:log_request(Req, 204),
    couch_stats_collector:increment({httpd_status_codes, 204}),
    Headers1 = couch_httpd:http_1_0_keep_alive(MochiReq, Headers),
    Headers2 = Headers1 ++ couch_httpd:server_header() ++
               couch_httpd_auth:cookie_auth_header(Req, Headers1),
    {ok, MochiReq:respond({204, Headers2, <<>>})}.


% cors_headers/1

cors_headers(MochiReq, RequestHeaders) ->
    EnableCors = enable_cors(),
    CorsHeaders = do_cors_headers(MochiReq, EnableCors),
    maybe_apply_cors_headers(CorsHeaders, RequestHeaders).

do_cors_headers(#httpd{mochi_req=MochiReq}, true) ->
    Host = couch_httpd_vhost:host(MochiReq),
    AcceptedOrigins = get_accepted_origins(Host),
    case MochiReq:get_header_value("Origin") of
    undefined ->
        % If the Origin header is not present terminate
        % this set of steps. The request is outside the scope
        % of this specification.
        % http://www.w3.org/TR/cors/#resource-processing-model
        [];
    Origin ->
        handle_cors_headers(couch_util:to_list(Origin),
                            Host, AcceptedOrigins)
    end;
do_cors_headers(_MochiReq, false) ->
    [].

maybe_apply_cors_headers([], RequestHeaders) ->
    RequestHeaders;
maybe_apply_cors_headers(CorsHeaders, RequestHeaders0) ->
    % for each RequestHeader that isn't in SimpleHeaders,
    % (or Content-Type with SIMPLE_CONTENT_TYPE_VALUES)
    % append to Access-Control-Expose-Headers
    % return: RequestHeaders ++ CorsHeaders ++ ACEH

    RequestHeaders = [K || {K,_V} <- RequestHeaders0],
    ExposedHeaders0 = reduce_headers(RequestHeaders, ?ALLOWED_HEADERS),

    % here we may have not moved Content-Type into ExposedHeaders,
    % now we need to check whether the Content-Type valus is
    % in ?SIMPLE_CONTENT_TYPE_VALUES and if it isn’t add Content-
    % Type to to ExposedHeaders
    ContentType =  proplists:get_value("Content-Type", RequestHeaders0),
    IncludeContentType = case ContentType of
    undefined ->
        false;
    _ ->
        ContentType_ = string:to_lower(ContentType),
        lists:member(ContentType_, ?SIMPLE_CONTENT_TYPE_VALUES)
    end,
    ExposedHeaders = case IncludeContentType of
    false ->
        lists:umerge(ExposedHeaders0, ["Content-Type"]);
    true ->
        ExposedHeaders0
    end,
    CorsHeaders
    ++ RequestHeaders0
    ++ [{"Access-Control-Expose-Headers",
            string:join(ExposedHeaders, ", ")}].


reduce_headers(A, B) ->
    reduce_headers0(A, B, []).

reduce_headers0([], _B, Result) ->
    lists:sort(Result);
reduce_headers0([ElmA|RestA], B, Result) ->
    R = case member_nocase(ElmA, B) of
    false -> Result;
    _Else -> [ElmA | Result]
    end,
    reduce_headers0(RestA, B, R).

member_nocase(ElmA, List) ->
    lists:any(fun(ElmB) ->
        string:to_lower(ElmA) =:= string:to_lower(ElmB)
    end, List).

handle_cors_headers(_Origin, _Host, []) ->
    [];
handle_cors_headers(Origin, Host, AcceptedOrigins) ->
    AcceptAll = lists:member("*", AcceptedOrigins),
    case {AcceptAll, lists:member(Origin, AcceptedOrigins)} of
    {true, _} ->
        make_cors_header(Origin, Host);
    {false, true}  ->
        make_cors_header(Origin, Host);
    _ ->
        % If the value of the Origin header is not a
        % case-sensitive match for any of the values
        % in list of origins, do not set any additional
        % headers and terminate this set of steps.
        % http://www.w3.org/TR/cors/#resource-requests
        []
    end.


make_cors_header(Origin, Host) ->
    Headers = [{"Access-Control-Allow-Origin", Origin}],
    maybe_add_credentials(Origin, Host, Headers).


%% util

maybe_add_credentials(Origin, Host, Headers) ->
    maybe_add_credentials(Headers, allow_credentials(Origin, Host)).

maybe_add_credentials(Headers, false) ->
    Headers;
maybe_add_credentials(Headers, true) ->
    Headers ++ [{"Access-Control-Allow-Credentials", "true"}].


allow_credentials("*", _Host) ->
    false;
allow_credentials(_Origin, Host) ->
    Default = get_bool_config("cors", "credentials", false),
    get_bool_config(cors_section(Host), "credentials", Default).



cors_config(Host, Key, Default) ->
    couch_config:get(cors_section(Host), Key,
                     couch_config:get("cors", Key, Default)).

cors_section(Host0) ->
    {Host, _Port} = split_host_port(Host0),
    "cors:" ++ Host.

enable_cors() ->
    get_bool_config("httpd", "enable_cors", false).

get_bool_config(Section, Key, Default) ->
    case couch_config:get(Section, Key) of
    undefined ->
        Default;
    "true" ->
        true;
    "false" ->
        false
    end.

get_accepted_origins(Host) ->
    split_list(cors_config(Host, "origins", [])).

split_list(S) ->
    re:split(S, "\\s*,\\s*", [trim, {return, list}]).

split_headers(H) ->
    re:split(H, ",\\s*", [{return,list}, trim]).

split_host_port(HostAsString) ->
    % split at semicolon ":"
    Split = string:rchr(HostAsString, $:),
    split_host_port(HostAsString, Split).

split_host_port(HostAsString, 0) ->
    % no semicolon
    {HostAsString, '*'};
split_host_port(HostAsString, N) ->
    HostPart = string:substr(HostAsString, 1, N-1),
    % parse out port
    % is there a nicer way?
    case (catch erlang:list_to_integer(string:substr(HostAsString,
                    N+1, length(HostAsString)))) of
    {'EXIT', _} ->
        {HostAsString, '*'};
    Port ->
        {HostPart, Port}
    end.
