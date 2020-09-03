CouchDB Views
=====

This is the new application that builds and runs Map/reduce views against FoundationDB.
Currently only map indexes are supported and it will always return the full index.

Code layout:

* `couch_views` - Main entry point to query a view
* `couch_views_batch` - Dynamically determine optimal batch sizes for view indexers.
* `couch_views_batch_impl` - Default implementation for optimizing batch sizes.
* `couch_views_encoding` - Encodes view keys that are byte comparable following CouchDB view sort order.
* `couch_views_fdb` - Maps view operations to FoundationDB logic.
* `couch_views_http` - View specific helpers for chttpd
* `couch_views_indexer` - `couch_jobs` worker that builds an index from the changes feed.
* `couch_views_reader` - Reads from the index for queries
* `couch_vews_jobs` - `couch_views` interactions with `couch_jobs`. It handles adding index jobs and subscribes to jobs.
* `couch_views_server` - Spawns `couch_views_indexer` workers to handle index update jobs.
* `couch_views_updater` - Update interactive indexes during doc update transactions
* `couch_views_util` - Various utility functions

# Configuration

; Batch size sensing parameters
; batch_initial_size = 100 ; Initial batch size in number of documents
; batch_search_increment = 500 ; Size change when searching for the threshold
; batch_sense_increment = 100 ; Size change increment after hitting a threshold
; batch_max_tx_size_bytes = 9000000 ; Maximum transaction size in bytes
; batch_max_tx_time_msec = 4500 ; Maximum transaction time in milliseconds
; batch_thresold_penalty = 0.2 ; Amount to reduce batch size when crossing a threshold

The default batch size sensing parameters are fairly straight forward. These
values can be tweaked in the config if desired. If you find that you need to
tweak these values for any reason please open an issue on GitHub reporting your
experience in case we need to adjust them for common cases.
