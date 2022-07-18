# couch_stats

couch_stats is a simple statistics collection app for Erlang applications. Its
core API is a thin wrapper around a stat storage library (currently Folsom,) but
abstracting over that library provides several benefits:

* All references to stat storage are in one place, so it's easy to swap
  the module out.

* Some common patterns, such as tying a process's lifetime to a counter value,
  are straightforward to support.

* Configuration can be managed in a single place - for example, it's much easier
  to ensure that all histogram metrics use a 10-second sliding window if those
  metrics are instantiated/configured centrally.

## Adding a metric

1. Write a stat description file. See `priv/descriptions.cfg for an example.
  * The metric name should be of type `[atom()]`.
  * The type should be one of `counter`, `gauge`, or `histogram`.

  If you don't add your metric to a description file, your metric will be
  accessible via `couch_stats:sample/1`, but it won't be read by the stats
  collector and therefore won't be available to HTTP `_stats` requests, etc.

2. Tell couch_stats to use your description file via application configuration.

2. Instrument your code with the helper functions in `couch_stats.erl`.
