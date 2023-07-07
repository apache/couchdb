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

-module(couch_prometheus_util).

-export([
    couch_to_prom/3,
    to_bin/1,
    to_prom/4,
    to_prom/2,
    to_prom_summary/2
]).

couch_to_prom([couch_log, level, alert], Info, _All) ->
    to_prom(couch_log_requests_total, counter, "number of logged messages", {
        [{level, alert}], val(Info)
    });
couch_to_prom([couch_log, level, Level], Info, _All) ->
    to_prom(couch_log_requests_total, {[{level, Level}], val(Info)});
couch_to_prom([couch_replicator, checkpoints, failure], Info, _All) ->
    to_prom(couch_replicator_checkpoints_failure_total, counter, desc(Info), val(Info));
couch_to_prom([couch_replicator, checkpoints, success], Info, All) ->
    Total = val(Info) + val([couch_replicator, checkpoints, failure], All),
    to_prom(couch_replicator_checkpoints_total, counter, "number of checkpoint saves", Total);
couch_to_prom([couch_replicator, responses, failure], Info, _All) ->
    to_prom(couch_replicator_responses_failure_total, counter, desc(Info), val(Info));
couch_to_prom([couch_replicator, responses, success], Info, All) ->
    Total = val(Info) + val([couch_replicator, responses, failure], All),
    to_prom(
        couch_replicator_responses_total,
        counter,
        "number of HTTP responses received by the replicator",
        Total
    );
couch_to_prom([couch_replicator, stream_responses, failure], Info, _All) ->
    to_prom(couch_replicator_stream_responses_failure_total, counter, desc(Info), val(Info));
couch_to_prom([couch_replicator, stream_responses, success], Info, All) ->
    Total = val(Info) + val([couch_replicator, stream_responses, failure], All),
    to_prom(
        couch_replicator_stream_responses_total,
        counter,
        "number of streaming HTTP responses received by the replicator",
        Total
    );
couch_to_prom([couchdb, auth_cache_hits], Info, All) ->
    Total = val(Info) + val([couchdb, auth_cache_misses], All),
    to_prom(auth_cache_requests_total, counter, "number of authentication cache requests", Total);
couch_to_prom([couchdb, auth_cache_misses], Info, _All) ->
    to_prom(auth_cache_misses_total, counter, desc(Info), val(Info));
% force a # TYPE and # HELP definition for httpd_request_methods
couch_to_prom([couchdb, httpd_request_methods, 'COPY'], Info, _All) ->
    to_prom(httpd_request_methods, counter, "number of HTTP requests by method", {
        [{method, 'COPY'}], val(Info)
    });
couch_to_prom([couchdb, httpd_request_methods, Method], Info, _All) ->
    to_prom(httpd_request_methods, {[{method, Method}], val(Info)});
% force a # TYPE and # HELP definition for httpd_status_codes
couch_to_prom([couchdb, httpd_status_codes, 200], Info, _All) ->
    to_prom(httpd_status_codes, counter, "number of HTTP responses by status code", {
        [{code, 200}], val(Info)
    });
couch_to_prom([couchdb, httpd_status_codes, Code], Info, _All) ->
    to_prom(httpd_status_codes, {[{code, Code}], val(Info)});
% Convert to gauge in prometheus type. This is required because
% prometheus assumes that counters are cumulative and should be
% rated by default, whereas folsom (the library CouchDB uses for
% metrics) allows counters to be decremented as well. Folsom supports
% gauges but does not track their state to allow increment/decrement.
% Basically, anywhere we use couch_stats:decrement_count we should
% be converting to a prometheus gauge.
couch_to_prom([couchdb, open_databases], Info, _All) ->
    to_prom(open_databases, gauge, desc(Info), val(Info));
couch_to_prom([couchdb, open_os_files], Info, _All) ->
    to_prom(open_os_files, gauge, desc(Info), val(Info));
couch_to_prom([couchdb, httpd, clients_requesting_changes], Info, _All) ->
    to_prom(httpd_clients_requesting_changes, gauge, desc(Info), val(Info));
couch_to_prom([ddoc_cache, hit], Info, All) ->
    Total = val(Info) + val([ddoc_cache, miss], All),
    to_prom(ddoc_cache_requests_total, counter, "number of design doc cache requests", Total);
couch_to_prom([ddoc_cache, miss], Info, _All) ->
    to_prom(ddoc_cache_requests_failures_total, counter, desc(Info), val(Info));
couch_to_prom([ddoc_cache, recovery], Info, _All) ->
    to_prom(ddoc_cache_requests_recovery_total, counter, desc(Info), val(Info));
couch_to_prom([fabric, read_repairs, failure], Info, _All) ->
    to_prom(fabric_read_repairs_failures_total, counter, desc(Info), val(Info));
couch_to_prom([fabric, read_repairs, success], Info, All) ->
    Total = val(Info) + val([fabric, read_repairs, failure], All),
    to_prom(fabric_read_repairs_total, counter, "number of fabric read repairs", Total);
couch_to_prom([rexi, streams, timeout, init_stream], Info, _All) ->
    to_prom(rexi_streams_timeout_total, counter, "number of rexi stream timeouts", {
        [{stage, init_stream}], val(Info)
    });
couch_to_prom([rexi_streams, timeout, Stage], Info, _All) ->
    to_prom(rexi_streams_timeout_total, {[{stage, Stage}], val(Info)});
couch_to_prom([couchdb | Rest], Info, All) ->
    couch_to_prom(Rest, Info, All);
couch_to_prom(Path, Info, _All) ->
    case lists:keyfind(type, 1, Info) of
        {type, counter} ->
            Metric = counter_metric(Path),
            to_prom(Metric, counter, desc(Info), val(Info));
        {type, gauge} ->
            to_prom(path_to_name(Path), gauge, desc(Info), val(Info));
        {type, histogram} ->
            to_prom_summary(Path, Info)
    end.

type_def(Metric, Type, Desc) ->
    Name = to_prom_name(Metric),
    [
        to_bin(io_lib:format("\n# HELP ~s ~s\r", [Name, Desc])),
        to_bin(io_lib:format("# TYPE ~s ~s", [Name, Type]))
    ].

% support creating a metric series with multiple label/values.
% Instances is of the form [{[{LabelName, LabelValue}], Value}, ...]
to_prom(_Metric, _Type, _Desc, []) ->
    [];
to_prom(Metric, Type, Desc, Instances) when is_list(Instances) ->
    TypeStr = type_def(Metric, Type, Desc),
    [TypeStr] ++ lists:flatmap(fun(Inst) -> to_prom(Metric, Inst) end, Instances);
to_prom(Metric, Type, Desc, Data) ->
    to_prom(Metric, Type, Desc, [Data]).

to_prom(Metric, Instances) when is_list(Instances) ->
    lists:flatmap(fun(Inst) -> to_prom(Metric, Inst) end, Instances);
to_prom(Metric, {Labels, Value}) ->
    LabelParts = lists:map(
        fun({K, V}) ->
            lists:flatten(io_lib:format("~s=\"~s\"", [to_bin(K), to_bin(V)]))
        end,
        Labels
    ),
    MetricStr =
        case length(LabelParts) > 0 of
            true ->
                LabelStr = string:join(LabelParts, ", "),
                lists:flatten(io_lib:format("~s{~s}", [to_prom_name(Metric), LabelStr]));
            false ->
                lists:flatten(io_lib:format("~s", [to_prom_name(Metric)]))
        end,
    [to_bin(io_lib:format("~s ~p", [MetricStr, Value]))];
to_prom(Metric, Value) ->
    [to_bin(io_lib:format("~s ~p", [to_prom_name(Metric), Value]))].

to_prom_summary(Path, Info) ->
    Metric = path_to_name(Path ++ ["seconds"]),
    {value, Value} = lists:keyfind(value, 1, Info),
    {arithmetic_mean, Mean} = lists:keyfind(arithmetic_mean, 1, Value),
    {percentile, Percentiles} = lists:keyfind(percentile, 1, Value),
    {n, Count} = lists:keyfind(n, 1, Value),
    Quantiles = lists:map(
        fun({Perc, Val0}) ->
            % Prometheus uses seconds, so we need to convert milliseconds to seconds
            Val = Val0 / 1000,
            case Perc of
                50 -> {[{quantile, <<"0.5">>}], Val};
                75 -> {[{quantile, <<"0.75">>}], Val};
                90 -> {[{quantile, <<"0.9">>}], Val};
                95 -> {[{quantile, <<"0.95">>}], Val};
                99 -> {[{quantile, <<"0.99">>}], Val};
                999 -> {[{quantile, <<"0.999">>}], Val}
            end
        end,
        Percentiles
    ),
    SumMetric = path_to_name(Path ++ ["seconds", "sum"]),
    SumStat = to_prom(SumMetric, Count * Mean),
    CountMetric = path_to_name(Path ++ ["seconds", "count"]),
    CountStat = to_prom(CountMetric, Count),
    to_prom(Metric, summary, desc(Info), Quantiles) ++ [SumStat, CountStat].

to_prom_name(Metric) ->
    to_bin(io_lib:format("couchdb_~s", [Metric])).

path_to_name(Path) ->
    Parts = lists:map(
        fun(Part) ->
            io_lib:format("~s", [Part])
        end,
        Path
    ),
    string:join(Parts, "_").

counter_metric(Path) ->
    Name = path_to_name(Path),
    case lists:suffix("_total", Name) of
        true -> to_bin(Name);
        _ -> to_bin(io_lib:format("~s_total", [Name]))
    end.

to_bin(Data) when is_list(Data) ->
    iolist_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
    atom_to_binary(Data, utf8);
to_bin(Data) when is_integer(Data) ->
    integer_to_binary(Data);
to_bin(Data) when is_binary(Data) ->
    Data.

val(Data) ->
    {value, V} = lists:keyfind(value, 1, Data),
    V.

val(Key, Stats) ->
    {Key, Data} = lists:keyfind(Key, 1, Stats),
    val(Data).

desc(Info) ->
    {desc, V} = lists:keyfind(desc, 1, Info),
    V.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

to_prom_counter_test() ->
    [
        ?assertEqual(
            <<"couchdb_ddoc_cache 10">>,
            test_to_prom_output(ddoc_cache, counter, "size of ddoc cache", 10)
        ),
        ?assertEqual(
            <<"couchdb_httpd_status_codes{code=\"200\"} 3">>,
            test_to_prom_output(httpd_status_codes, counter, "HTTP request status by code", {
                [{code, 200}], 3
            })
        )
    ].

to_prom_gauge_test() ->
    ?assertEqual(
        <<"couchdb_temperature_celsius 36">>,
        test_to_prom_output(temperature_celsius, gauge, "temp", 36)
    ).

to_prom_summary_test() ->
    ?assertEqual(
        <<"couchdb_mango_query_time_seconds{quantile=\"0.75\"} 4.5">>,
        test_to_prom_summary_output([mango_query_time], [
            {value, [
                {min, 0.0},
                {max, 0.0},
                {arithmetic_mean, 0.0},
                {geometric_mean, 0.0},
                {harmonic_mean, 0.0},
                {median, 0.0},
                {variance, 0.0},
                {standard_deviation, 0.0},
                {skewness, 0.0},
                {kurtosis, 0.0},
                {percentile, [
                    {50, 0.0},
                    {75, 4500},
                    {90, 0.0},
                    {95, 0.0},
                    {99, 0.0},
                    {999, 0.0}
                ]},
                {histogram, [
                    {0, 0}
                ]},
                {n, 0}
            ]},
            {type, histogram},
            {desc, <<"length of time processing a mango query">>}
        ])
    ).

counter_metric_test_() ->
    [
        ?_assertEqual(
            <<"document_purges_total">>,
            counter_metric([document_purges, total])
        ),
        ?_assertEqual(
            <<"document_purges_total">>,
            counter_metric([document_purges])
        )
    ].

test_to_prom_output(Metric, Type, Desc, Val) ->
    Out = to_prom(Metric, Type, Desc, Val),
    lists:nth(2, Out).

test_to_prom_summary_output(Metric, Info) ->
    Out = to_prom_summary(Metric, Info),
    lists:nth(3, Out).

-endif.
