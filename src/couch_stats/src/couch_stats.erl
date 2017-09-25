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

-module(couch_stats).

-export([
    start/0,
    stop/0,
    fetch/0,
    reload/0,
    sample/1,
    new/2,
    delete/1,
    list/0,
    increment_counter/1,
    increment_counter/2,
    decrement_counter/1,
    decrement_counter/2,
    update_histogram/2,
    update_gauge/2
]).


-include("couch_stats.hrl").


-type response() :: ok | {error, unknown_metric}.
-type stat() :: {any(), [{atom(), any()}]}.

start() ->
    application:start(couch_stats).

stop() ->
    application:stop(couch_stats).

fetch() ->
    couch_stats_aggregator:fetch().

reload() ->
    couch_stats_aggregator:reload().

-spec sample(any()) -> stat().
sample(Name) ->
    [{Name, Info}] = folsom_metrics:get_metric_info(Name),
    sample_type(Name, proplists:get_value(type, Info)).

-spec new(atom(), any()) -> ok | {error, metric_exists | unsupported_type}.
new(counter, Name) ->
    case folsom_metrics:new_counter(Name) of
        ok -> ok;
        {error, Name, metric_already_exists} -> {error, metric_exists}
    end;
new(histogram, Name) ->
    Time = config:get_integer("stats", "interval", ?DEFAULT_INTERVAL),
    case folsom_metrics:new_histogram(Name, slide_uniform, {Time, 1024}) of
        ok -> ok;
        {error, Name, metric_already_exists} -> {error, metric_exists}
    end;
new(gauge, Name) ->
    case folsom_metrics:new_gauge(Name) of
        ok -> ok;
        {error, Name, metric_already_exists} -> {error, metric_exists}
    end;
new(_, _) ->
    {error, unsupported_type}.

delete(Name) ->
    folsom_metrics:delete_metric(Name).

list() ->
    folsom_metrics:get_metrics_info().

-spec increment_counter(any()) -> response().
increment_counter(Name) ->
    notify_existing_metric(Name, {inc, 1}, counter).

-spec increment_counter(any(), pos_integer()) -> response().
increment_counter(Name, Value) ->
    notify_existing_metric(Name, {inc, Value}, counter).

-spec decrement_counter(any()) -> response().
decrement_counter(Name) ->
    notify_existing_metric(Name, {dec, 1}, counter).

-spec decrement_counter(any(), pos_integer()) -> response().
decrement_counter(Name, Value) ->
    notify_existing_metric(Name, {dec, Value}, counter).

-spec update_histogram(any(), number()) -> response();
                      (any(), function()) -> any().
update_histogram(Name, Fun) when is_function(Fun, 0) ->
    Begin = os:timestamp(),
    Result = Fun(),
    Duration = timer:now_diff(os:timestamp(), Begin) div 1000,
    case notify_existing_metric(Name, Duration, histogram) of
        ok ->
            Result;
        {error, unknown_metric} ->
            throw({unknown_metric, Name})
    end;
update_histogram(Name, Value) when is_number(Value) ->
    notify_existing_metric(Name, Value, histogram).

-spec update_gauge(any(), number()) -> response().
update_gauge(Name, Value) ->
    notify_existing_metric(Name, Value, gauge).

-spec notify_existing_metric(any(), any(), any()) -> response().
notify_existing_metric(Name, Op, Type) ->
    try
        ok = folsom_metrics:notify_existing_metric(Name, Op, Type)
    catch _:_ ->
        couch_log:notice("unknown metric: ~p", [Name]),
        {error, unknown_metric}
    end.

-spec sample_type(any(), atom()) -> stat().
sample_type(Name, histogram) ->
    folsom_metrics:get_histogram_statistics(Name);
sample_type(Name, _) ->
    folsom_metrics:get_metric_value(Name).
