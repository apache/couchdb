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
