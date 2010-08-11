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

-module(couch_httpd_stats_handlers).
-include("couch_db.hrl").

-export([handle_stats_req/1]).
-import(couch_httpd, [
    send_json/2, send_json/3, send_json/4, send_method_not_allowed/2,
    start_json_response/2, send_chunk/2, end_json_response/1,
    start_chunked_response/3, send_error/4
]).

handle_stats_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    flush(Req),
    send_json(Req, couch_stats_aggregator:all(range(Req)));

handle_stats_req(#httpd{method='GET', path_parts=[_, _Mod]}) ->
    throw({bad_request, <<"Stat names must have exactly to parts.">>});

handle_stats_req(#httpd{method='GET', path_parts=[_, Mod, Key]}=Req) ->
    flush(Req),
    Stats = couch_stats_aggregator:get_json({list_to_atom(binary_to_list(Mod)),
        list_to_atom(binary_to_list(Key))}, range(Req)),
    send_json(Req, {[{Mod, {[{Key, Stats}]}}]});

handle_stats_req(#httpd{method='GET', path_parts=[_, _Mod, _Key | _Extra]}) ->
    throw({bad_request, <<"Stat names must have exactly two parts.">>});

handle_stats_req(Req) ->
    send_method_not_allowed(Req, "GET").

range(Req) ->
    case couch_util:get_value("range", couch_httpd:qs(Req)) of
        undefined ->
            0;
        Value ->
            list_to_integer(Value)
    end.

flush(Req) ->
    case couch_util:get_value("flush", couch_httpd:qs(Req)) of
        "true" ->
            couch_stats_aggregator:collect_sample();
        _Else ->
            ok
    end.
