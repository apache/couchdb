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

-module(chttpd_cors).


-export([
    maybe_handle_preflight_request/1,
    maybe_handle_preflight_request/2,
    headers/2,
    headers/4
]).
-export([
    is_cors_enabled/1,
    get_cors_config/1
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("chttpd/include/chttpd_cors.hrl").


%% http://www.w3.org/TR/cors/#resource-preflight-requests

maybe_handle_preflight_request(#httpd{method=Method}) when Method /= 'OPTIONS' ->
    not_preflight;
maybe_handle_preflight_request(Req) ->
    case maybe_handle_preflight_request(Req, get_cors_config(Req)) of
        not_preflight ->
            not_preflight;
        {ok, PreflightHeaders} ->
            chttpd:send_response(Req, 204, PreflightHeaders, <<>>)
    end.


maybe_handle_preflight_request(#httpd{}=Req, Config) ->
    case is_cors_enabled(Config) of
        true ->
            case preflight_request(Req, Config) of
                {ok, PreflightHeaders} ->
                    {ok, PreflightHeaders};
                not_preflight ->
                    not_preflight;
                UnknownError ->
                    couch_log:error(
                        "Unknown response of chttpd_cors:preflight_request(~p): ~p",
                        [Req, UnknownError]
                    ),
                    not_preflight
            end;
        false ->
            not_preflight
    end.


preflight_request(Req, Config) ->
    case get_origin(Req) of
        undefined ->
            %% If the Origin header is not present terminate this set of
            %% steps. The request is outside the scope of this specification.
            %% http://www.w3.org/TR/cors/#resource-preflight-requests
            not_preflight;
        Origin ->
            AcceptedOrigins = get_accepted_origins(Req, Config),
            AcceptAll = lists:member(<<"*">>, AcceptedOrigins),

            HandlerFun = fun() ->
                handle_preflight_request(Req, Config, Origin)
            end,

            %% We either need to accept all origins or have it listed
            %% in our origins. Origin can only contain a single origin
            %% as the user agent will not follow redirects [1]. If the
            %% value of the Origin header is not a case-sensitive
            %% match for any of the values in list of origins do not
            %% set any additional headers and terminate this set
            %% of steps [1].
            %%
            %% [1]: http://www.w3.org/TR/cors/#resource-preflight-requests
            %%
            %% TODO: Square against multi origin Security Considerations and the
            %% Vary header
            %%
            case AcceptAll orelse lists:member(Origin, AcceptedOrigins) of
                true -> HandlerFun();
                false -> not_preflight
            end
    end.


handle_preflight_request(Req, Config, Origin) ->
    case chttpd:header_value(Req, "Access-Control-Request-Method") of
    undefined ->
        %% If there is no Access-Control-Request-Method header
        %% or if parsing failed, do not set any additional headers
        %% and terminate this set of steps. The request is outside
        %% the scope of this specification.
        %% http://www.w3.org/TR/cors/#resource-preflight-requests
        not_preflight;
    Method ->
        SupportedMethods = get_origin_config(Config, Origin,
                <<"allow_methods">>, ?SUPPORTED_METHODS),

        SupportedHeaders = get_origin_config(Config, Origin,
                <<"allow_headers">>, ?SUPPORTED_HEADERS),


        %% get max age
        MaxAge = couch_util:get_value("max_age", Config, ?CORS_DEFAULT_MAX_AGE),

        PreflightHeaders0 = maybe_add_credentials(Config, Origin, [
            {"Access-Control-Allow-Origin", binary_to_list(Origin)},
            {"Access-Control-Max-Age", MaxAge},
            {"Access-Control-Allow-Methods",
                string:join(SupportedMethods, ", ")}]),

        case lists:member(Method, SupportedMethods) of
            true ->
                %% method ok , check headers
                AccessHeaders = chttpd:header_value(Req,
                    "Access-Control-Request-Headers"),
                {FinalReqHeaders, ReqHeaders} = case AccessHeaders of
                    undefined -> {"", []};
                    Headers ->
                        %% transform header list in something we
                        %% could check. make sure everything is a
                        %% list
                        RH = [to_lower(H)
                              || H <- split_headers(Headers)],
                        {Headers, RH}
                end,
                %% check if headers are supported
                case ReqHeaders -- SupportedHeaders of
                [] ->
                    PreflightHeaders = PreflightHeaders0 ++
                                       [{"Access-Control-Allow-Headers",
                                         FinalReqHeaders}],
                    {ok, PreflightHeaders};
                _ ->
                    not_preflight
                end;
            false ->
            %% If method is not a case-sensitive match for any of
            %% the values in list of methods do not set any additional
            %% headers and terminate this set of steps.
            %% http://www.w3.org/TR/cors/#resource-preflight-requests
            not_preflight
        end
    end.


headers(Req, RequestHeaders) ->
    case get_origin(Req) of
        undefined ->
            %% If the Origin header is not present terminate
            %% this set of steps. The request is outside the scope
            %% of this specification.
            %% http://www.w3.org/TR/cors/#resource-processing-model
            RequestHeaders;
        Origin ->
            headers(Req, RequestHeaders, Origin, get_cors_config(Req))
    end.


headers(_Req, RequestHeaders, undefined, _Config) ->
    RequestHeaders;
headers(Req, RequestHeaders, Origin, Config) when is_list(Origin) ->
    headers(Req, RequestHeaders, ?l2b(string:to_lower(Origin)), Config);
headers(Req, RequestHeaders, Origin, Config) ->
    case is_cors_enabled(Config) of
        true ->
            AcceptedOrigins = get_accepted_origins(Req, Config),
            CorsHeaders = handle_headers(Config, Origin, AcceptedOrigins),
            ExposedCouchHeaders = couch_util:get_value(
                <<"exposed_headers">>, Config, ?COUCH_HEADERS),
            maybe_apply_headers(CorsHeaders, RequestHeaders, ExposedCouchHeaders);
        false ->
            RequestHeaders
    end.


maybe_apply_headers([], RequestHeaders, _ExposedCouchHeaders) ->
    RequestHeaders;
maybe_apply_headers(CorsHeaders, RequestHeaders, ExposedCouchHeaders) ->
    %% Find all non ?SIMPLE_HEADERS and and non ?SIMPLE_CONTENT_TYPE_VALUES,
    %% expose those through Access-Control-Expose-Headers, allowing
    %% the client to access them in the browser. Also append in
    %% ?COUCH_HEADERS, as further headers may be added later that
    %% need to be exposed.
    %% return: RequestHeaders ++ CorsHeaders ++ ACEH

    ExposedHeaders0 = simple_headers([K || {K,_V} <- RequestHeaders]),

    %% If Content-Type is not in ExposedHeaders, and the Content-Type
    %% is not a member of ?SIMPLE_CONTENT_TYPE_VALUES, then add it
    %% into the list of ExposedHeaders
    ContentType = proplists:get_value("content-type", ExposedHeaders0),
    IncludeContentType = case ContentType of
        undefined ->
            false;
        _ ->
            lists:member(string:to_lower(ContentType), ?SIMPLE_CONTENT_TYPE_VALUES)
        end,
    ExposedHeaders = case IncludeContentType of
        false ->
            ["content-type" | lists:delete("content-type", ExposedHeaders0)];
        true ->
            ExposedHeaders0
        end,

    %% ExposedCouchHeaders may get added later, so expose them by default
    ACEH = [{"Access-Control-Expose-Headers",
        string:join(ExposedHeaders ++ ExposedCouchHeaders, ", ")}],
    CorsHeaders ++ RequestHeaders ++ ACEH.


simple_headers(Headers) ->
    LCHeaders = [to_lower(H) || H <- Headers],
    lists:filter(fun(H) -> lists:member(H, ?SIMPLE_HEADERS) end, LCHeaders).

to_lower(String) when is_binary(String) ->
    to_lower(?b2l(String));
to_lower(String) ->
    string:to_lower(String).

handle_headers(_Config, _Origin, []) ->
    [];
handle_headers(Config, Origin, AcceptedOrigins) ->
    AcceptAll = lists:member(<<"*">>, AcceptedOrigins),
    case AcceptAll orelse lists:member(Origin, AcceptedOrigins) of
    true ->
        make_cors_header(Config, Origin);
    false ->
        %% If the value of the Origin header is not a
        %% case-sensitive match for any of the values
        %% in list of origins, do not set any additional
        %% headers and terminate this set of steps.
        %% http://www.w3.org/TR/cors/#resource-requests
        []
    end.


make_cors_header(Config, Origin) ->
    Headers = [{"Access-Control-Allow-Origin", binary_to_list(Origin)}],
    maybe_add_credentials(Config, Origin, Headers).


%% util


maybe_add_credentials(Config, Origin, Headers) ->
    case allow_credentials(Config, Origin) of
        false ->
            Headers;
        true ->
            Headers ++ [{"Access-Control-Allow-Credentials", "true"}]
    end.


allow_credentials(_Config, <<"*">>) ->
    false;
allow_credentials(Config, Origin) ->
    get_origin_config(Config, Origin, <<"allow_credentials">>,
        ?CORS_DEFAULT_ALLOW_CREDENTIALS).

get_cors_config(#httpd{cors_config = undefined}) ->
    EnableCors = config:get("httpd", "enable_cors", "false") =:= "true",
    AllowCredentials = config:get("cors", "credentials", "false") =:= "true",
    AllowHeaders = case config:get("cors", "headers", undefined) of
        undefined ->
            ?SUPPORTED_HEADERS;
        AllowHeaders0 ->
            [to_lower(H) || H <- split_list(AllowHeaders0)]
    end,
    AllowMethods = case config:get("cors", "methods", undefined) of
        undefined ->
            ?SUPPORTED_METHODS;
        AllowMethods0 ->
            split_list(AllowMethods0)
    end,
    ExposedHeaders = case config:get("cors", "exposed_headers", undefined) of
        undefined ->
            ?COUCH_HEADERS;
        ExposedHeaders0 ->
            [to_lower(H) || H <- split_list(ExposedHeaders0)]
    end,
    Origins0 = binary_split_list(config:get("cors", "origins", [])),
    Origins = [{O, {[]}} || O <- Origins0],
    [
        {<<"enable_cors">>, EnableCors},
        {<<"allow_credentials">>, AllowCredentials},
        {<<"allow_methods">>, AllowMethods},
        {<<"allow_headers">>, AllowHeaders},
        {<<"exposed_headers">>, ExposedHeaders},
        {<<"origins">>, {Origins}}
    ];
get_cors_config(#httpd{cors_config = Config}) ->
    Config.

is_cors_enabled(Config) ->
    case get(disable_couch_httpd_cors) of
        undefined ->
            put(disable_couch_httpd_cors, true);
        _ ->
            ok
    end,
    couch_util:get_value(<<"enable_cors">>, Config, false).


%% Get a list of {Origin, OriginConfig} tuples
%% ie: get_origin_configs(Config) ->
%% [
%%     {<<"http://foo.com">>,
%%         {
%%             [
%%                 {<<"allow_credentials">>, true},
%%                 {<<"allow_methods">>, [<<"POST">>]}
%%             ]
%%         }
%%     },
%%     {<<"http://baz.com">>, {[]}}
%% ]
get_origin_configs(Config) ->
    {Origins} = couch_util:get_value(<<"origins">>, Config, {[]}),
    Origins.


%% Get config for an individual Origin
%% ie: get_origin_config(Config, <<"http://foo.com">>) ->
%% [
%%     {<<"allow_credentials">>, true},
%%     {<<"allow_methods">>, [<<"POST">>]}
%% ]
get_origin_config(Config, Origin) ->
    OriginConfigs = get_origin_configs(Config),
    {OriginConfig} = couch_util:get_value(Origin, OriginConfigs, {[]}),
    OriginConfig.


%% Get config of a single key for an individual Origin
%% ie: get_origin_config(Config, <<"http://foo.com">>, <<"allow_methods">>, [])
%% [<<"POST">>]
get_origin_config(Config, Origin, Key, Default) ->
    OriginConfig = get_origin_config(Config, Origin),
    couch_util:get_value(Key, OriginConfig,
        couch_util:get_value(Key, Config, Default)).


get_origin(Req) ->
    case chttpd:header_value(Req, "Origin") of
        undefined ->
            undefined;
        Origin ->
            ?l2b(to_lower(Origin))
    end.


get_accepted_origins(_Req, Config) ->
    lists:map(fun({K,_V}) -> K end, get_origin_configs(Config)).


split_list(S) ->
    re:split(S, "\\s*,\\s*", [trim, {return, list}]).


binary_split_list(S) ->
    [list_to_binary(E) || E <- split_list(S)].


split_headers(H) ->
    re:split(H, ",\\s*", [{return,list}, trim]).
