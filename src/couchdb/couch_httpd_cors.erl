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

%% the cors utilities 
%%
%% CORS processing is done by adding CORS headers to the cors_headers
%% variable in the process registry. Then headers are added to the final
%% response headers while processing the response using the cors_headers
%% function.
%%
%% Process:
%% 1. set default cors headers using set_default_headers when couchdb
%%    start to process the request
%% 2. on OPTION method, check asked capabilities and eventually return
%%    preflight headers. At this steps CORS headers can be [] (capability not
%%    handled) or the list of preflight headers like the spec require.
%%    preflight headers are set in preflight_headers method.
%% 3. on database request , we check origin header in a list of origin. 
%%    Eventually we reset cors headers to [] if the origin isn't
%%    supported. Db origins are added to teh "origin" member of the
%%    security object. db_check_origin function is used to check them.

-module(couch_httpd_cors).

-include("couch_db.hrl").

-define(SUPPORTED_HEADERS, [
            %% simple headers
            "Accept", 
            "Accept-Language", 
            "Content-Type",
            "Expires", 
            "Last-Modified", 
            "Pragma", 
            "Origin",
            %% couchdb headers
            "Content-Length", 
            "If-Match", 
            "Destination",
            "X-Requested-With", 
            "X-Http-Method-Override", 
            "Content-Range"]).

-export([set_default_headers/1, headers/0,
         preflight_headers/1, preflight_headers/2,
         db_check_origin/2]).

set_default_headers(MochiReq) ->
    case MochiReq:get_header_value("Origin") of
    undefined -> 
        erlang:put(cors_headers, []);
    Origin ->
        DefaultHeaders = [{"Access-Control-Allow-Origin", Origin},
                          {"Access-Control-Allow-Credentials", "true"}],
        erlang:put(cors_headers, DefaultHeaders)
    end.

headers() ->
    erlang:get(cors_headers).

split_origin(Origin) ->
    {Scheme, Netloc, _, _, _} = mochiweb_util:urlsplit(Origin),
    {string:to_lower(Scheme), string:to_lower(Netloc)}.

preflight_headers(MochiReq) ->
    preflight_headers(MochiReq, [<<"*">>]).

preflight_headers(#httpd{mochi_req=MochiReq}, AcceptedOrigins) ->
    preflight_headers(MochiReq, AcceptedOrigins);
preflight_headers(MochiReq, AcceptedOrigins) ->
    SupportedMethods = ["GET", "HEAD", "POST", "PUT",
            "DELETE", "TRACE", "CONNECT", "COPY", "OPTIONS"],

    %% get custom headers
    CustomHeaders = re:split(couch_config:get("cors",
            "headers",""), "\\s*,\\s*",[{return, list}]),

    %% build list of headers to test
    AllSupportedHeaders = ?SUPPORTED_HEADERS ++ CustomHeaders,
    SupportedHeaders = [string:to_lower(H) || H <- AllSupportedHeaders],
    
    %% get max age
    MaxAge = list_to_integer(
        couch_config:get("cors", "max_age", "1000")
    ),

    %% reset cors_headers
    erlang:put(cors_headers, []),

    case MochiReq:get_header_value("Origin") of
    undefined -> ok;
    Origin ->
        %% if origin validate it against accepted origin
        case check_origin(AcceptedOrigins, split_origin(Origin)) of
        error -> ok; %% don't set any preflight header
        _Origin1 ->
            ?LOG_DEBUG("check preflight cors request", []), 

            PreflightHeaders0 = [
                {"Access-Control-Allow-Origin", Origin},
                {"Access-Control-Allow-Credentials", "true"},
                {"Access-Control-Max-Age", MaxAge},
                {"Access-Control-Allow-Methods", string:join(SupportedMethods, ", ")}
            ],

            %% now check the reqquested method
            case MochiReq:get_header_value("Access-Control-Request-Method") of
            undefined ->
                erlang:put(cors_headers, PreflightHeaders0);
            Method ->
                case lists:member(Method, SupportedMethods) of
                true ->
                    %% method ok , check headers
                    {FinalReqHeaders, ReqHeaders} = case MochiReq:get_header_value(
                            "Access-Control-Request-Headers") of
                        undefined -> {"", []};
                        Headers ->
                            %% transform header list in something we
                            %% could check. make sure everything is a
                            %% list 

                            RH = [string:to_lower(H) || H <- re:split(Headers, ",\\s*",
                                [{return,list},trim])],
                            {Headers, RH}
                    end,

                    %% check if headers are supported
                    case ReqHeaders -- SupportedHeaders of
                    [] ->
                        PreflightHeaders = PreflightHeaders0 ++ 
                            [{"Access-Control-Allow-Headers", FinalReqHeaders}],
                        erlang:put(cors_headers, PreflightHeaders);
                    _ -> ok
                    end;
                false -> ok
                end
            end
        end
    end.

check_origin([], _SO) ->
    error;
check_origin([<<"*">>|_], _SO) ->
    "*";
check_origin([A0|R], SO) ->
    A = couch_util:to_list(A0),
    SA = split_origin(A),
    
    if SO == SA -> A;
        true ->check_origin(R, SO)
    end.

db_check_origin(#httpd{mochi_req=MochiReq}, Db) ->
    {SecProps} = couch_db:get_security(Db),
    AcceptedOrigins = couch_util:get_value(<<"origins">>, SecProps, [<<"*">>]),
    case MochiReq:get_header_value("Origin") of
    undefined -> ok;
    Origin ->
        case check_origin(AcceptedOrigins, split_origin(Origin)) of
        error ->
            %% reset cors_headers
            erlang:put(cors_headers, []);
        _Origin1 ->
            CorsHeaders = [{"Access-Control-Allow-Origin", Origin},
                           {"Access-Control-Allow-Credentials", "true"}],
            erlang:put(cors_headers, CorsHeaders)
        end
    end,
    ok.
