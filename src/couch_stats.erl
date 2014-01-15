-module(couch_stats).

-export([
    start/0,
    stop/0,
    fetch/0,
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

-type response() :: ok | {error, unknown_metric}.
-type stat() :: {any(), [{atom(), any()}]}.

start() ->
    application:start(couch_stats).

stop() ->
    application:stop(couch_stats).

fetch() ->
    couch_stats_stats_collector:fetch().

-spec sample(any()) -> stat().
sample(Name) ->
    [{Name, Info}] = folsom_metrics:get_metric_info(Name),
    sample_type(Name, proplists:get_value(type, Info)).

-spec new(atom(), any()) -> ok | {error, metric_exists | unsupported_type}.
new(counter, Name) ->
    case folsom_metrics:new_counter(Name) of
        ok -> ok;
        {error, Name, metric_exists} -> {error, metric_exists}
    end;
new(histogram, Name) ->
    {ok, Time} = application:get_env(couch_stats, collection_interval),
    case folsom_metrics:new_histogram(Name, slide_uniform, {Time, 1024}) of
        ok -> ok;
        {error, Name, metric_exists} -> {error, metric_exists}
    end;
new(gauge, Name) ->
    case folsom_metrics:new_gauge(Name) of
        ok -> ok;
        {error, Name, metric_exists} -> {error, metric_exists}
    end;
new(_, _) ->
    {error, unsupported_type}.

delete(Name) ->
    folsom_metrics:delete_metric(Name).

list() ->
    folsom_metrics:get_metrics_info().

-spec increment_counter(any()) -> response().
increment_counter(Name) ->
    notify(Name, {inc, 1}).

-spec increment_counter(any(), pos_integer()) -> response().
increment_counter(Name, Value) ->
    notify(Name, {inc, Value}).

-spec decrement_counter(any()) -> response().
decrement_counter(Name) ->
    notify(Name, {dec, 1}).

-spec decrement_counter(any(), pos_integer()) -> response().
decrement_counter(Name, Value) ->
    notify(Name, {dec, Value}).

-spec update_histogram(any(), number()) -> response().
update_histogram(Name, Value) ->
    notify(Name, Value).

-spec update_gauge(any(), number()) -> response().
update_gauge(Name, Value) ->
    notify(Name, Value).

-spec notify(any(), any()) -> response().
notify(Name, Op) ->
    case folsom_metrics:notify(Name, Op) of
        ok -> ok;
        _ -> {error, unknown_metric}
    end.

-spec sample_type(any(), atom()) -> stat().
sample_type(Name, histogram) ->
    folsom_metrics:get_histogram_statistics(Name);
sample_type(Name, _) ->
    folsom_metrics:get_metric_value(Name).
