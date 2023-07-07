# couch_stats

couch_stats is a simple statistics collection app for Erlang applications. It
uses https://www.erlang.org/doc/man/counters.html to implement counters,
gauges and histograms. By default histograms record 10 seconds worth of data,
with a granularity of 1 second.

Stats can be fetched with `couch_stats:fetch()`. That returns the current
values of all the counters and gauges as well as the histogram statistics for
the last 10 seconds.

## Adding a metric

1. Write a stat description file. See `priv/descriptions.cfg for an example.
  * The metric name should be of type `[atom()]`.
  * The type should be one of `counter`, `gauge`, or `histogram`.

  If you don't add your metric to a description file, your metric will be
  accessible via `couch_stats:sample/1`, but it won't be read by the stats
  collector and therefore won't be available to HTTP `_stats` requests, etc.

2. Tell couch_stats to use your description file via application configuration.

3. Instrument your code with the helper functions in `couch_stats.erl`.
