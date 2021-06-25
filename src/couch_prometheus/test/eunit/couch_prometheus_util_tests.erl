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

-module(couch_prometheus_util_tests).

-include_lib("couch/include/couch_eunit.hrl").

-import(couch_prometheus_util, [
    to_prom/3,
    to_prom_summary/2
]).

couch_prometheus_util_test_() ->
    [
        ?_assertEqual(
            <<"couchdb_ddoc_cache 10">>,
            test_to_prom_output(ddoc_cache, counter, 10)
        ),
        ?_assertEqual(
            <<"couchdb_httpd_status_codes{code=\"200\"} 3">>,
            test_to_prom_output(httpd_status_codes, counter, {[{code, 200}], 3})
        ),
        ?_assertEqual(
            <<"couchdb_temperature_celsius 36">>,
            test_to_prom_output(temperature_celsius, gauge, 36)
        ),
        ?_assertEqual(
            <<"couchdb_mango_query_time_seconds{quantile=\"0.75\"} 4.5">>,
            test_to_prom_sum_output([mango_query_time], [
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
        )
    ].

test_to_prom_output(Metric, Type, Val) ->
    Out = to_prom(Metric, Type, Val),
    lists:nth(2, Out).

test_to_prom_sum_output(Metric, Info) ->
    Out = to_prom_summary(Metric, Info),
    lists:nth(3, Out).
