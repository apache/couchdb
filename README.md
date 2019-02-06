ken
===

Ken builds views and search indexes. Automatically.

#### Overview

When the couch\_db\_update event is triggered with an `updated` event, ken will spawn indexing jobs for view groups and search indexes (one job per view group shard or search index shard). If a `deleted` event is triggered, all jobs associated with the corresponding database shard will be removed.

#### Testing

Testing for ken expected to be executed from the top level `couchdb` repo as a part of `make check` run. The isolated ken test could be ran as `rebar eunit apps=ken verbose=1` from the `couchdb`'s root directory.
