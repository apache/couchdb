% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_stats_handlers).
-include("couch_db.hrl").
-include("couch_stats.hrl").

-export([handle_stats_req/1]).
-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1,
    start_chunked_response/3, send_error/4]).

-define(b2a(V), list_to_atom(binary_to_list(V))).

handle_stats_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    send_json(Req, couch_stats_aggregator:all());

handle_stats_req(#httpd{method='GET', path_parts=[_Stats, Module, Key]}=Req) ->
    Time = parse_stats_query(Req),
    Stats = couch_stats_aggregator:get_json({?b2a(Module), ?b2a(Key)}, Time),
    Response = {[{Module, {[{Key, Stats}]}}]},
    send_json(Req, Response);

handle_stats_req(Req) ->
    send_method_not_allowed(Req, "GET").

parse_stats_query(Req) ->
    case couch_httpd:qs(Req) of
        [{"range", Time}] -> list_to_atom(Time);
        _ -> '0'
    end.
