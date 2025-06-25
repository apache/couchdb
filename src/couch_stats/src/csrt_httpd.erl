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
handle_resource_status_req(
    #httpd{method = 'POST', path_parts = [<<"_active_resources">>, <<"_match">>, MatcherName]} = Req
) ->
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
    AggregationKey = parse_key(AggregationKeys),
    case csrt_query:count_by(matching(MatcherName), AggregationKey) of
        {ok, Map} ->
            send_json(Req, {aggregation_result_to_json(Map)});
        Else ->
            %% TODO handle error
            throw({bad_request, Else})
    end.

handle_sort_by(Req, MatcherName, SortBy) ->
    AggregationKeys = couch_util:get_value(<<"aggregate_keys">>, SortBy),
    CounterKey = couch_util:get_value(<<"counter_key">>, SortBy),
    AggregationKey = parse_key(AggregationKeys),
    ValueKey = parse_key(CounterKey),
    case csrt_query:sort_by(matching(MatcherName), AggregationKey, ValueKey) of
        {ok, Map} ->
            send_json(Req, {aggregation_result_to_json(Map)});
        Else ->
            %% TODO handle error
            throw({bad_request, Else})
    end.

handle_group_by(Req, MatcherName, GroupBy) ->
    AggregationKeys = couch_util:get_value(<<"aggregate_keys">>, GroupBy),
    CounterKey = couch_util:get_value(<<"counter_key">>, GroupBy),
    AggregationKey = parse_key(AggregationKeys),
    ValueKey = parse_key(CounterKey),
    case csrt_query:group_by(matching(MatcherName), AggregationKey, ValueKey) of
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

matching(MatcherName) ->
    case csrt_logger:get_matcher(binary_to_list(MatcherName)) of
        undefined ->
            throw({bad_request, <<"unknown matcher '", MatcherName/binary, "'">>});
        Matcher ->
            Matcher
    end.

% extract one of the predefined matchers
%   - docs_read
%   - rows_read
%   - docs_written
%   - worker_changes_processed
%   - ioq_calls
query_matcher(MatcherName, AggregationKey, CounterKey) ->
    case csrt_logger:get_matcher(binary_to_list(MatcherName)) of
        undefined ->
            {error, <<"unknown matcher '", MatcherName/binary, "'">>};
        Matcher ->
            csrt_query:query_matcher(Matcher, AggregationKey, CounterKey)
    end.

-spec to_key(BinKey :: binary() | string()) ->
    Key ::
        rctx_field()
        | throw({bad_request, Reason :: binary()}).

to_key(<<"pid_ref">>) -> pid_ref;
to_key(<<"nonce">>) -> nonce;
to_key(<<"type">>) -> type;
to_key(<<"dbname">>) -> dbname;
to_key(<<"username">>) -> username;
to_key(<<"db_open">>) -> db_open;
to_key(<<"docs_read">>) -> docs_read;
to_key(<<"rows_read">>) -> rows_read;
to_key(<<"changes_returned">>) -> changes_returned;
to_key(<<"ioq_calls">>) -> ioq_calls;
to_key(<<"js_filter">>) -> js_filter;
to_key(<<"js_filtered_docs">>) -> js_filtered_docs;
to_key(<<"get_kv_node">>) -> get_kv_node;
to_key(<<"get_kp_node">>) -> get_kp_node;
to_key(Other) when is_binary(Other) -> throw({bad_request, <<"Invalid key '", Other/binary, "'">>}).

-spec parse_key(Keys :: binary() | [binary()]) ->
    [rctx_field()]
    | throw({bad_request, Reason :: binary()}).

parse_key(Keys) when is_list(Keys) ->
    parse_key(Keys, []);
parse_key(BinKey) when is_binary(BinKey) ->
    to_key(BinKey);
parse_key(undefined) ->
    undefined.

parse_key([BinKey | Rest], Keys) ->
    parse_key(Rest, [to_key(BinKey) | Keys]);
parse_key([], Keys) ->
    lists:reverse(Keys).
