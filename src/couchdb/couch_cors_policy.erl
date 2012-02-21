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
-export([origins_config/3, policy_for_request/2]).

-include("couch_db.hrl").

-define(DEFAULT_CORS_POLICY,
        [ {<<"allow_credentials">>, false}
        , {<<"max_age">>, 4 * 60 * 60}
        , {<<"allow_methods">>, <<"GET, HEAD, POST">>}
        , {<<"allow_headers">>,
            <<"Origin, If-Match, Destination"
            , ", Accept, Content-Length, Content-Type"
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
    _No when _No =/= true ->
        [];
    true ->
        case request_origin(Headers) of
        undefined ->
            % s. 5.1(1) and s. 5.2(1) If the Origin header is not present
            % terminate this set of steps. The request is outside the scope of
            % this specification.
            %?LOG_DEBUG("Not a CORS request", []),
            [];
        Origins ->
            headers(Global, Local, {Method, Headers}, Origins)
        end
    end,

    ?LOG_DEBUG("CORS headers: ~p", [Result]),
    Result.


headers(Global, Local, {ReqMethod, _ReqHeaders}=Req, Origins) ->
    Policies = origins_config(Global, Local, Req),
    Policy = policy_for_request(Policies, Req),

    case ReqMethod of
        'OPTIONS' ->
            % s. 5.2. Preflight Request; also s. 6.1.5(1)
            preflight(Policy, Req);
        _ ->
            % s. 5.1. Simple X-O Request, Actual Request, and Redirects.
            actual(Policy, Req)
    end.

preflight(Policy, {_Method, ReqHeaders}) ->
    ACRMethod = "Access-Control-Request-Method",
    ACRHeaders = "Access-Control-Request-Headers",
    {OkOrigins, OkMethods, OkHeaders, OkCreds} = Policy,

    % XXX: s. 5.1(2) (actual requests) requires splitting the Origin
    % header, and AFAICT all tokens must match the list of origins. But
    % s. 5.2.2 (preflight requests) implies that the Origin header will contain
    % only one token. So is the "source origin" (s. 6.1) the first in
    % the list, or the whole header value?
    SourceOrigin = request_origin(ReqHeaders),

    % s. 5.2(2) If the value of the Origin header is not a case-sensitive match
    % for any of the values in [OkOrigins] do not set any additional headers
    % and terminate this set of steps.
    IsThisOrigin = fun(OkOrigin) -> OkOrigin =:= SourceOrigin end,
    case lists:any(IsThisOrigin, OkOrigins) of
    false ->
        ?LOG_DEBUG("Bad preflight origin: ~p vs. (~p)",
                   [SourceOrigin, OkOrigins]),
        [];
    true ->
        % s. 5.2(3) Let [RequestedMethod] be the value as result of parsing the
        % A-C-R-Method header.  If there is no A-C-R-Method header or if parsing
        % failed, do not set any additional headers and terminate this set of
        % steps.
        case couch_util:get_value(ACRMethod, ReqHeaders) of
        undefined ->
            ?LOG_DEBUG("No method for preflight: ~p", SourceOrigin),
            [];
        RequestedMethodStr ->
            RequestedMethod = couch_util:to_binary(RequestedMethodStr),

            % s. 5.2(4) Let [RequestedHeaders] be the values as result of
            % parsing the A-C-R-Headers headers.  If there are no A-C-R-Headers
            % headers let [RequestedHeaders] be the empty list.
            RequestedHeaders =
                case couch_util:get_value(ACRHeaders, ReqHeaders) of
                    undefined -> [];
                    HeaderList -> comma_split(string:to_lower(HeaderList))
                end,

            ?LOG_DEBUG("Origin ~s requests ~s with ~s", [SourceOrigin,
                    RequestedMethod, comma_join(RequestedHeaders)]),

            preflight(SourceOrigin, OkCreds, {RequestedMethod, OkMethods},
                      {RequestedHeaders, OkHeaders})
        end
    end.

preflight(Origin, OkCreds, {Method, OkMethods}, {Headers, OkHeaders}) ->
    % s. 5.2(5) If method is not a case-sensitive match for any of the values
    % in list of methods do not set any additional headers and terminate this
    % set of steps.
    IsTheMethod = fun(Candidate) -> Candidate =:= Method end,
    case lists:any(IsTheMethod, OkMethods) of
    false ->
        ?LOG_DEBUG("Decline preflight method from ~s: ~s",
                   [Origin, Method]),
        [];
    true ->
        % s. 5.2(6) If any of the header field-names is not a ASCII
        % case-insensitive match for any of the values in list of headers
        % do not set any additional headers and terminate this set of
        % steps.
        BadHeaders = lists:foldl(fun(Header, State) ->
            IsThisHeader = fun(Candidate) -> Candidate =:= Header end,
            case lists:any(IsThisHeader, OkHeaders) of
                true -> State;
                false -> [Header | State]
            end
        end, [], Headers),

        case BadHeaders of
        [ _ | _Rest ] ->
            ?LOG_DEBUG("Bad preflight headers (~p): ~s",
                       [Origin, comma_join(BadHeaders)]),
            [];
        [] ->
            % s. 5.2(7) If the resource supports credentials add a
            % single A-C-A-Origin header, with the value of the Origin
            % header as value, and add a single A-C-A-Credentials
            % header with the case-sensitive string "true" as value.
            % Otherwise, add a single A-C-A-Origin header, with either
            % the value of the Origin header or the string "*" as
            % value.
            CredsHeaders = case OkCreds of
            true ->
                [{"Access-Control-Allow-Origin", Origin},
                 {"Access-Control-Allow-Credentials", "true"}];
            false ->
                [{"Access-Control-Allow-Origin", Origin}]
            end,

            % s. 5.2(8) Optionally add a single A-C-Max-Age header with as
            % value the amount of seconds the user agent is allowed to
            % cache the result of the request.
            %
            % TODO
            MaxAgeHeaders = [],

            % s. 5.2(9) Add one or more Access-Control-Allow-Methods
            % headers consisting of (a subset of) the list of methods.
            MethodHeaders = [{"Access-Control-Allow-Methods",
                              comma_join(OkMethods)}],

            % s. 5.2(10) Add one or more A-C-A-Headers headers consisting
            % of (a subset of) the list of headers.
            HeadersHeaders = [{"Access-Control-Allow-Headers",
                               comma_join(OkHeaders)}],

            ?LOG_DEBUG("Good preflight ~s: ~s", [Method, Origin]),
            CredsHeaders ++ MaxAgeHeaders ++ MethodHeaders
                         ++ HeadersHeaders
        end
    end.

actual(Policy, Req) ->
    {OkOrigins, OkMethods, OkHeaders, OkCreds} = Policy,

    % s. 5.1(2) Split the value of the Origin header on the U+0020 SPACE
    % character and if any of the resulting tokens is not a case-sensitive
    % match for any of the values in [OkOrigins] do not set any additional
    % headers and terminate...
    Origins = request_origins(Req),
    GoodOrigin = fun(Origin) ->
        lists:any(fun(OkOrigin) ->
            OkOrigin == <<"*">> orelse OkOrigin == Origin
        end, OkOrigins)
    end,

    case lists:all(GoodOrigin, Origins) of
    false ->
        ?LOG_DEBUG("Origin not allowed: ~s", [comma_join(Origins)]),
        [];
    true ->
        % s. 5.1(3) If the resource supports credentials add a single
        % A-C-A-Origin header, with the value of the origin header as value,
        % and add a single A-C-A-Credentials header with the literal string
        % "true" as value.
        %
        % Otherwise, add a single A-C-A-Origin header with either the value of
        % the Origin header or the literal string "*" as value.
        OriginVal = request_origin(Req),
        Allow = case OkCreds of
            true -> [ {"Access-Control-Allow-Origin", OriginVal}
                    , {"Access-Control-Allow-Credentials", "true"}
                    ];
            false -> [ {"Access-Control-Allow-Origin", OriginVal} ]
        end,

        % s. 5.1(4) If the resource wants to expose more than just simple
        % response headers to the API of the CORS API specification add one or
        % more Access-Control-Expose-Headers headers, with as values the filed
        % names of the additional headers to expose.
        CouchHeaders = [ "Server", "Date"
                       , "Content-Length"
                       , "ETag", "Age"
                       , "Connection" % ?
                       % Any others?
                       ],

        % TODO: Merged with the configured expose_headers?
        Expose = [{"Access-Control-Expose-Headers", comma_join(CouchHeaders)}],

        % Interestingly, the spec does not confirm policy for actual requests.
        % This function ignores methods, headers, and the request object.  Of
        % course, disabling CORS on CouchDB, or removing an origin from the
        % config would have immediate effect. Perhaps a future feature could
        % detect minor changes to the CORS policy and purge the cache as
        % mentioned in the 5.1(4) note.
        Allow ++ Expose
    end.

policy_for_request(Policies, {_Method, ReqHeaders}) ->
    % s. 5 ...each resource is bound to the following:
    % * A list of origins consisting of zero or more origins that are allowed
    %   to access this resource.
    % * A list of methods consisting of zero or more methods that are
    %   supported by the resource.
    % * A list of headers consisting of zero or more header field names that
    %   are supported by the resource.
    % * A supports credentials flag that indicates whether the resource
    %   supports user credentials in the request. [true/false]
    %
    % The spec indicates that each resources has one static policy. However
    % CouchDB allows a per-origin policy. This function merges each origin's
    % policy into one single policy appropriate to the request.
    ReqOrigins = request_origins(ReqHeaders),

    % Check() (passed to foldl) looks up the policy for a given origin and
    % mixes it in to the final policy.
    Check = fun(ReqOrigin, {OkOrigins, OkMethods, OkHeaders, OkCreds}=State) ->
        {Policy} = case couch_util:get_value(ReqOrigin, Policies) of
            undefined -> couch_util:get_value(<<"*">>, Policies);
            MatchedPolicy -> MatchedPolicy
        end,

        Origins1 = sets:add_element(ReqOrigin, OkOrigins),

        CredsPol = couch_util:get_value(<<"allow_credentials">>, Policy),
        Creds1 = OkCreds andalso CredsPol,

        MethodPol = sets:from_list(comma_split(
                couch_util:get_value(<<"allow_methods">>, Policy))),
        Methods1 = case sets:size(OkMethods) of
            0 -> MethodPol;
            _ -> sets:intersection(OkMethods, MethodPol)
        end,

        HeaderPol = sets:from_list(comma_split(
                couch_util:get_value(<<"allow_headers">>, Policy))),
        Headers1 = case sets:size(OkHeaders) of
            0 -> HeaderPol;
            _ -> sets:intersection(OkHeaders, HeaderPol)
        end,

        {Origins1, Methods1, Headers1, Creds1}
    end,

    %         origins   , methods   , headers   , allow_credentials
    State0 = {sets:new(), sets:new(), sets:new(), false},
    {OkOrigins, OkMethods, OkHeaders, OkCreds} =
        lists:foldl(Check, State0, ReqOrigins),

    {sets:to_list(OkOrigins),
     sets:to_list(OkMethods),
     sets:to_list(OkHeaders),
     OkCreds}.

origins_config(Global, Local, {_Method, Headers}) ->
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
        Policy1 = lists:foldl(fun({Key, Val}, State) ->
            lists:keystore(Key, 1, State, {Key, Val})
        end, ?DEFAULT_CORS_POLICY, Policy),

        % Convert the headers to lower case.
        {<<"allow_headers">>, Headers} =
            lists:keyfind(<<"allow_headers">>, 1, Policy1),
        LowerHeaders = string:to_lower(?b2l(Headers)),
        lists:keystore(<<"allow_headers">>, 1, Policy1,
                       {<<"allow_headers">>, ?l2b(LowerHeaders)})
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
        Origins = comma_split(Val),
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



% Return the "Origin" header value.
request_origin({_Method, Headers}) ->
    request_origin(Headers);
request_origin([]) ->
    undefined;
request_origin([{Key, Val} | Rest]) ->
    case couch_util:to_list(Key) of
        "Origin" -> couch_util:to_binary(Val);
        _ -> request_origin(Rest)
    end.

request_origins(Req) ->
    case request_origin(Req) of
        undefined -> undefined;
        Origins -> re:split(Origins, "\s+")
    end.

comma_split(Str) ->
    re:split(Str, ",\\s*").

comma_join(List) ->
    StrList = [ couch_util:to_list(E) || E <- List ],
    string:join(StrList, ",").

% vim: sts=4 sw=4 et
