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
-export([global_config/0, check/2, check/3]).

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


check(DbConfig, #httpd{}=Req) ->
    check(global_config(), DbConfig, Req).

check(Global, Local, #httpd{}=Req)
        when is_list(Global) andalso is_list(Local) ->
    {Httpd} = couch_util:get_value(<<"httpd">>, Global, {[]}),
    Enabled = couch_util:get_value(<<"cors_enabled">>, Httpd, false),
    case Enabled of
        true -> [];
        _ -> false
    end.


origins_config(Global, Local, Req) ->
    % Identify the "origins" configuration object which applies to this
    % request. The local (i.e.  _security object) config takes precidence.
    {Httpd} = couch_util:get_value(<<"httpd">>, Global, {[]}),
    XHost = couch_util:get_value(<<"x_forwarded_host">>, Httpd,
                                 "X-Forwarded-Host"),
    VHost = case couch_httpd:header_value(Req, XHost) of
        undefined ->
            case couch_httpd:header_value(Req, "Host") of
                undefined -> "";
                HostValue -> ?l2b(HostValue)
            end;
        ForwardedValue -> ?l2b(ForwardedValue)
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
