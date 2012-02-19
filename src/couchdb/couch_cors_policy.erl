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

-module(couch_cors_policy).
-export([global_config/0, headers/1, headers/2, headers/3]).

% For the test suite.
-export([origins_config/3]).

-include("couch_db.hrl").

-define(DEFAULT_CORS_POLICY,
        [ {<<"allow_credentials">>, false}
        , {<<"max_age">>, 4 * 60 * 60}
        , {<<"allow_methods">>, <<"GET, HEAD, POST">>}
        , {<<"allow_headers">>,
            <<"Content-Length, If-Match, Destination"
            , ", X-HTTP-Method-Override"
            , ", X-Requested-With" % For jQuery v1.5.1
            >>}
        ]).


headers({_Method, _Headers}=Req) ->
    headers([], Req).

headers(DbConfig, {_Method, _Headers}=Req) ->
    headers(global_config(), DbConfig, Req).

headers(Global, Local, {Method, Headers})
        when is_list(Global) andalso is_list(Local) andalso is_list(Headers)
        andalso (is_atom(Method) orelse is_binary(Method)) ->
    {Httpd} = couch_util:get_value(<<"httpd">>, Global, {[]}),
    Enabled = couch_util:get_value(<<"cors_enabled">>, Httpd, false),
    Result = case Enabled of
        true ->
            % Normalize the headers to always use string key names.
            StrHeaders = [ {couch_util:to_list(Key), Val} || {Key, Val} <- Headers ],
            Req = {Method, StrHeaders},
            case lists:keyfind("Origin", 1, Headers) of
                false ->
                    % s. 5.1(1) and s. 5.2(1) If the Origin header is not
                    % present terminate this set of steps. The request is
                    % outside the scope of this specification.
                    ?LOG_DEBUG("Not a CORS request", []),
                    [];
                {"Origin", Origin} ->
                    headers(Global, Local, Req, Origin)
            end;
        _ ->
            []
    end,

    ?LOG_DEBUG("CORS headers: ~p", [Result]),
    Result.


headers(Global, Local, {ReqMethod, _ReqHeaders}=Req, OriginValue) ->
    Policies = origins_config(Global, Local, Req),

    % s. 5 ...each resource is bound to the following:
    % * A list of origins consisting of zero or more origins that are allowed
    %   to access this resource.
    % * A list of methods consisting of zero or more methods that are
    %   supported by the resource.
    % * A list of headers consisting of zero or more header field names that
    %   are supported by the resource.
    % * A supports credentials flag that indicates whether the resource
    %   supports user credentials in the request. [true/false]
    Origins = list_of_origins(Policies, Req),
    Methods = list_of_methods(Policies, Req),
    Headers = list_of_headers(Policies, Req),
    Creds = supports_credentials(Policies, Req),

    case ReqMethod of
        'OPTIONS' ->
            % s. 5.2. Preflight Request; also s. 6.1.5(1)
            preflight(OriginValue, Origins, Methods, Headers, Creds, Req);
        _ ->
            % s. 5.1. Simple X-O Request, Actual Request, and Redirects.
            actual(OriginValue, Origins, Methods, Headers, Creds, Req)
    end.

preflight(OriginVal, OkOrigins, OkMethods, OkHeaders, OkCreds, Req) ->
    % Note that s. 5.1(2) (actual requests) requires splitting the Origin
    % header, and AFAICT all tokens must match the list of origins. But
    % s. 5.2.2 (preflight requests) implies that the Origin header will contain
    % only one token.  Assume that the "source origin" (s. 6.1) is the first in
    % the list?
    SourceOrigin = lists:nth(1, string:tokens(OriginVal, " ")),
    [].

actual(OriginVal, OkOrigins, _OkMethods, _OkHeaders, OkCreds, _Req) ->
    % s. 5.1(2) Split the value of the Origin header on the U+0020 SPACE
    % character and if any of the resulting tokens is not a case-sensitive
    % match for any of the values in [OkOrigins] do not set any additional
    % headers and terminate...
    Origins = [ ?l2b(O) || O <- string:tokens(OriginVal, " ") ],
    GoodOrigin = fun(Origin) ->
        lists:any(fun(OkOrigin) ->
            OkOrigin == <<"*">> orelse OkOrigin == Origin
        end, OkOrigins)
    end,

    case lists:all(GoodOrigin, Origins) of
        false ->
            ?LOG_DEBUG("Origin ~p not allowed: ~p", [Origins, OkOrigins]),
            [];
        true ->
            % s. 5.1(3) If the resource supports credentials add a single
            % A-C-A-Origin header, with the value of the origin header as
            % value, and add a single A-C-A-Credentials header with the literal
            % string "true" as value.
            %
            % Otherwise, add a single A-C-A-Origin header with either the value
            % of the Origin header or the literal string "*" as value.
            Allow = case OkCreds of
                true -> [ {"Access-Control-Allow-Origin", OriginVal}
                        , {"Access-Control-Allow-Credentials", "true"}
                        ];
                false -> [ {"Access-Control-Allow-Origin", OriginVal} ]
            end,

            % s. 5.1(4) If the resource wants to expose more than just simple
            % response headers to the API of the CORS API specification add one
            % or more Access-Control-Expose-Headers headers, with as values the
            % filed names of the additional headers to expose.
            CouchHeaders = [ "Server", "Date"
                           , "Content-Length"
                           , "ETag", "Age"
                           , "Connection" % ?
                           % Any others?
                           ],

            % TODO: Merged with the configured expose_headers?
            Expose = [{"Access-Control-Expose-Headers",
                      string:join(CouchHeaders, ",")}],

            % Interestingly, the spec does not confirm policy for actual
            % requests. This function ignores methods, headers, and the request
            % object.  Of course, disabling CORS on CouchDB, or removing an
            % origin from the config would have immediate effect. Perhaps a
            % future feature could detect minor changes to the CORS policy and
            % purge the cache as mentioned in the 5.1(4) note.
            Allow ++ Expose
    end.

list_of_origins(Config, _Req) ->
    Keys = [ Key || {Key, _Val} <- Config ].

list_of_methods(Config, Req) ->
    [].
list_of_headers(Config, Req) ->
    [].
supports_credentials(Config, Req) ->
    false.

origins_config(Global, Local, {Method, Headers}) ->
    % Identify the "origins" configuration object which applies to this
    % request. The local (i.e.  _security object) config takes precidence.
    {Httpd} = couch_util:get_value(<<"httpd">>, Global, {[]}),
    XHost = couch_util:get_value(<<"x_forwarded_host">>, Httpd,
                                 "X-Forwarded-Host"),
    VHost = case lists:keyfind(XHost, 1, Headers) of
        false ->
            case lists:keyfind("Host", 1, Headers) of
                false -> "";
                {"Host", HostValue} -> ?l2b(HostValue)
            end;
        {XHost, ForwardedValue} -> ?l2b(ForwardedValue)
    end,

    {GlobalHosts} = couch_util:get_value(<<"origins">>, Global, {[]}),
    {LocalHosts} = couch_util:get_value(<<"origins">>, Local, {[]}),
    Origins = case couch_util:get_value(VHost, LocalHosts) of
        {LocalObj} ->
            ?LOG_DEBUG("Local origin list: ~s", [VHost]),
            LocalObj;
        _ ->
            ?LOG_DEBUG("No local origin list: ~s", [VHost]),
            case couch_util:get_value(VHost, GlobalHosts) of
                {GlobalObj} ->
                    ?LOG_DEBUG("Global origin list: ~s", [VHost]),
                    GlobalObj;
                _ ->
                    ?LOG_DEBUG("No global origin list: ~s", [VHost]),
                    []
            end
    end,

    origins_config(Origins).

origins_config(BaseOrigins) ->
    % Normalize the config object for origins, apply defaults, etc. If no
    % origins are specified, provide a default wildcard entry.
    Defaulted = fun(Policy) ->
        lists:foldl(fun({Key, Val}, State) ->
            lists:keyreplace(Key, 1, State, {Key, Val})
        end, ?DEFAULT_CORS_POLICY, Policy)
    end,

    Origins = case BaseOrigins of
        [] -> [ {<<"*">>, {[]}} ];
        _ -> BaseOrigins
    end,

    [ {Key, {Defaulted(Policy)}} || {Key, {Policy}} <- Origins ].

global_config() ->
    % Return the globally-configured CORS settings in a format identical
    % to that in the _security object.
    %
    % E.g., example.com allows origins http://origin.com and https://origin.com
    % { "origins":
    %   { "example.com":
    %     { "http://origin.com": {"max_age":1234, "allow_methods":"GET"}
    %     , "https://origin.com": {"allow_methods": "PUT, POST"}
    %     }
    %   }
    % }
    XHost = couch_config:get("httpd", "x_forwarded_host", "X-Forwarded-Host"),
    Enabled = couch_config:get("httpd", "cors_enabled", "false"),
    Origins = global_config(origins),

    [ {<<"httpd">>,
        {[ {<<"cors_enabled">>, couch_util:to_existing_atom(Enabled)}
         , {<<"x_forwarded_host">>, ?l2b(XHost)}
        ]} }
    , {<<"origins">>, {Origins}}
    ].

global_config(origins) ->
    % Return the .origins object of the config.  Map each domain (vhost) to the
    % origins it supports. Each supported origin is itself an object indicating
    % allowed headers, max age, etc.
    OriginsSection = couch_config:get("origins"),
    lists:foldl(fun({Key, Val}, State) ->
        Domain = ?l2b(Key),
        Origins = re:split(Val, ",\\s*"),
        DomainObj = lists:foldl(fun(Origin, DomainState) ->
            Policy = binary_section(Origin),
            lists:keystore(Origin, 1, DomainState, {Origin, {Policy}})
        end, [], Origins),
        lists:keystore(Domain, 1, State, {Domain, {DomainObj}})
    end, [], OriginsSection).


binary_section(Section) ->
    % Return a config section, with all strings converted to binaries.
    SectionStr = couch_config:get(Section),
    BoolOrBinary = fun("true") -> true;
                      ("false") -> false;
                      (Val) -> ?l2b(Val)
    end,
    [ {?l2b(Key), BoolOrBinary(Val)} || {Key, Val} <- SectionStr ].

% vim: sts=4 sw=4 et
