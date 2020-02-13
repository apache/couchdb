CouchDB Views
=====

This is the new application that builds and runs Map/reduce views against FoundationDB.
Currently only map indexes are supported and it will always return the full index.

Code layout:

* `couch_views` - Main entry point to query a view
* `couch_views_reader` - Reads from the index for queries
* `couch_views_indexer` - `couch_jobs` worker that builds an index from the changes feed.
* `couch_vews_jobs` - `couch_views` interactions with `couch_jobs`. It handles adding index jobs and subscribes to jobs.
* `couch_views_fdb` - Maps view operations to FoundationDB logic.
* `couch_views_encoding` - Encodes view keys that are byte comparable following CouchDB view sort order.
* `couch_views_server` - Spawns `couch_views_indexer` workers to handle index update jobs.

# Configuration

## Configuring rate limiter

Here is the example of configuration used in `couch_view` application:

```
[couch_rate.views]
limiter = couch_rate_limiter
opts = #{budget => 100, target => 2500, window => 60000, sensitivity => 1000}
```

Supported fields in `opts`:

* `budget` - the initial value for estimated batch size
* `target` - the amount in msec which we try to maintain for batch processing time
* `window` - time interval for contention detector
* `sensitivity` - minimal interval within the `window`

Unsupported fields in `opts` (if you really know what you are doing):

* `window_size` - how many batches to consider in contention detector
* `timer` - this is used for testing to fast forward time `fun() -> current_time_in_ms() end`
* `target` - the amount in msec which we try to maintain for batch processing time
* `underload_threshold` - a threshold below which we would try to increase the budget
* `overload_threshold` - a threshold above which we would start decreasing the budget
* `delay_threshold` - a threshold above which we would start introducing delays between batches
* `multiplicative_factor` - determines how fast we are going to decrease budget (must be in (0..1) range)
* `regular_delay` - delay between batches when there is no overload
* `congested_delay` - delay between batches when there is an overload
* `initial_budget` - initial value for budget to start with

