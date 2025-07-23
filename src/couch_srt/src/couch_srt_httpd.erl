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

-module(couch_srt_httpd).
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_srt.hrl").

-export([handle_resource_status_req/1]).

-import(
    chttpd,
    [
        send_json/2, send_json/3,
        send_method_not_allowed/2
    ]
).

handle_resource_status_req(
    #httpd{method = 'POST', path_parts = [<<"_active_resources">>, <<"_match">>, MatcherNameBin]} =
        Req
) ->
    chttpd:validate_ctype(Req, "application/json"),
    {JsonProps} = chttpd:json_body_obj(Req),
    GroupBy = couch_util:get_value(<<"group_by">>, JsonProps),
    SortBy = couch_util:get_value(<<"sort_by">>, JsonProps),
    CountBy = couch_util:get_value(<<"count_by">>, JsonProps),
    MatcherName = binary_to_list(MatcherNameBin),
    {AggregationKeys, Query} =
        case {GroupBy, SortBy, CountBy} of
            {undefined, undefined, {Props}} ->
                Keys = couch_util:get_value(<<"aggregate_keys">>, Props),
                {Keys, couch_srt:query([couch_srt:from(MatcherName), couch_srt:count_by(Keys)])};
            {undefined, {Props}, undefined} ->
                Keys = couch_util:get_value(<<"aggregate_keys">>, Props),
                CounterKey = couch_util:get_value(<<"counter_key">>, Props),
                {Keys, couch_srt:query([couch_srt:from(MatcherName), couch_srt:sort_by(Keys, CounterKey)])};
            {{Props}, undefined, undefined} ->
                Keys = couch_util:get_value(<<"aggregate_keys">>, Props),
                CounterKey = couch_util:get_value(<<"counter_key">>, Props),
                {Keys, couch_srt:query([couch_srt:from(MatcherName), couch_srt:group_by(Keys, CounterKey)])};
            {_, _, _} ->
                throw({bad_request, <<"Multiple aggregations are not supported">>})
        end,
    case Query of
        {error, Reason} ->
            send_error(Req, Reason);
        Q ->
            JSON = to_json(AggregationKeys, couch_srt:rpc_run(Q)),
            send_json(Req, JSON)
    end;
handle_resource_status_req(#httpd{path_parts = [<<"_active_resources">>]} = Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    send_method_not_allowed(Req, "GET,HEAD");
handle_resource_status_req(Req) ->
    ok = chttpd:verify_is_server_admin(Req),
    send_method_not_allowed(Req, "GET,HEAD,POST").

to_json(AggregationKeys, Results) ->
    lists:map(fun(E) -> node_reply_to_json(AggregationKeys, E) end, Results).

node_reply_to_json(_AggregationKeys, #{node := Node, result := none, errors := Errors}) ->
    #{
        node => atom_to_binary(Node),
        result => none,
        errors => lists:map(fun erlang:atom_to_list/1, Errors)
    };
node_reply_to_json(AggregationKeys, #{node := Node, result := Result, errors := Errors}) ->
    #{
        node => atom_to_binary(Node),
        result => aggregation_result_to_json(AggregationKeys, Result),
        errors => lists:map(fun erlang:atom_to_list/1, Errors)
    }.

encode_key(AggregationKeys, Key) ->
    maps:from_list(lists:zip(AggregationKeys, tuple_to_list(Key))).

-spec aggregation_result_to_json(AggregationKeys :: binary() | [binary()], Map :: query_result()) ->
    json_spec(#{
        value => non_neg_integer(),
        key => #{
            username => string(),
            dbname => string()
        }
    }).

aggregation_result_to_json(AggregationKeys, Map) when
    is_map(Map) andalso is_list(AggregationKeys)
->
    maps:fold(
        fun(K, V, Acc) ->
            [
                #{
                    value => V,
                    key => encode_key(AggregationKeys, K)
                }
                | Acc
            ]
        end,
        [],
        Map
    );
aggregation_result_to_json(AggregationKey, Map) when
    is_map(Map) andalso is_binary(AggregationKey)
->
    maps:fold(
        fun(K, V, Acc) ->
            [
                #{value => V, key => #{AggregationKey => K}} | Acc
            ]
        end,
        [],
        Map
    );
aggregation_result_to_json(AggregationKeys, Ordered) when
    is_list(Ordered) andalso is_list(AggregationKeys)
->
    lists:map(
        fun({K, V}) ->
            #{
                value => V,
                key => encode_key(AggregationKeys, K)
            }
        end,
        Ordered
    );
aggregation_result_to_json(AggregationKey, Ordered) when
    is_list(Ordered) andalso is_binary(AggregationKey)
->
    lists:map(
        fun({K, V}) ->
            #{value => V, key => #{AggregationKey => K}}
        end,
        Ordered
    ).

send_error(Req, [{unknown_matcher, Matcher} | _]) ->
    MatcherBin = list_to_binary(Matcher),
    chttpd:send_error(Req, {bad_request, <<"Unknown matcher '", MatcherBin/binary, "'">>});
send_error(Req, [{invalid_key, FieldName} | _]) ->
    chttpd:send_error(Req, {bad_request, <<"Unknown field name '", FieldName/binary, "'">>});
send_error(Req, [Reason | _]) ->
    chttpd:send_error(Req, {error, Reason}).
