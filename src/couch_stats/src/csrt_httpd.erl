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

-module(csrt_httpd).
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

-export([handle_resource_status_req/1]).

-import(
    chttpd,
    [
        send_json/2, send_json/3,
        send_method_not_allowed/2
    ]
).

rpc_to_json({Resp, _Errors, _Nodes}) ->
    #{<<"results">> => resp_to_json(Resp, #{}), <<"errors">> => [], <<"bad_nodes">> => []}.

resp_to_json([{N, R} | Rest], Acc) ->
    resp_to_json(Rest, maps:put(atom_to_binary(N), R, Acc));
resp_to_json([], Acc) ->
    Acc.

% handle_resource_status_req(#httpd{method = 'GET', path_parts = [<<"_active_resources">>]} = Req) ->
%     ok = chttpd:verify_is_server_admin(Req),
%     %% TODO: incorporate Bad responses
%     Resp = rpc_to_json(csrt:rpc(active, [json])),
%     send_json(Req, Resp);
handle_resource_status_req(#httpd{method = 'POST', path_parts = [<<"_active_resources">>, <<"_match">>, MatcherName]} = Req) ->
    chttpd:validate_ctype(Req, "application/json"),
    {JsonProps} = chttpd:json_body_obj(Req),
    GroupBy = couch_util:get_value(<<"group_by">>, JsonProps),
    SortBy = couch_util:get_value(<<"sort_by">>, JsonProps),
    CountBy = couch_util:get_value(<<"count_by">>, JsonProps),

    case {GroupBy, SortBy, CountBy} of
        {undefined, undefined, {Query}} ->
            handle_count_by(Req, MatcherName, Query);
        {undefined, {Query}, undefined} ->
            handle_sort_by(Req, MatcherName, Query);
        {{Query}, undefined, undefined} ->
            handle_group_by(Req, MatcherName, Query);
        {_, _, _} ->
            throw({bad_request, <<"Multiple aggregations are not supported">>})
    end;
handle_resource_status_req(#httpd{path_parts = [<<"_active_resources">>]} = Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    send_method_not_allowed(Req, "GET,HEAD");

handle_resource_status_req(Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    send_method_not_allowed(Req, "GET,HEAD,POST").

handle_count_by(Req, MatcherName, CountBy) ->
    AggregationKeys = couch_util:get_value(<<"aggregate_keys">>, CountBy),
    case csrt:query_count_by(MatcherName, AggregationKeys) of
        {ok, Map} ->
            send_json(Req, {aggregation_result_to_json(Map)});
        Else ->
            %% TODO handle error
            throw({bad_request, Else})
    end.

handle_sort_by(Req, MatcherName, SortBy) ->
    AggregationKeys = couch_util:get_value(<<"aggregate_keys">>, SortBy),
    CounterKey = couch_util:get_value(<<"counter_key">>, SortBy),
    case csrt:query_sort_by(MatcherName, AggregationKeys, CounterKey) of
        {ok, Map} ->
            send_json(Req, {aggregation_result_to_json(Map)});
        Else ->
            %% TODO handle error
            throw({bad_request, Else})
    end.

handle_group_by(Req, MatcherName, GroupBy) ->
    AggregationKeys = couch_util:get_value(<<"aggregate_keys">>, GroupBy),
    CounterKey = couch_util:get_value(<<"counter_key">>, GroupBy),
    case csrt:query_group_by(MatcherName, AggregationKeys, CounterKey) of
        {ok, Map} ->
            send_json(Req, {aggregation_result_to_json(Map)});
        Else ->
            %% TODO handle error
            throw({bad_request, Else})
    end.

aggregation_result_to_json(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> [{key_to_string(K), V} | Acc] end, [], Map).

key_to_string(Key) when is_tuple(Key) ->
    list_to_binary(string:join([atom_to_list(K) || K <- tuple_to_list(Key)], ","));
key_to_string(Key) when is_atom(Key) ->
    atom_to_binary(Key).
