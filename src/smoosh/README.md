Smoosh
======

Smoosh is CouchDB's auto-compaction daemon. It is notified when
databases and views are updated and may then elect to enqueue them for
compaction.

API
---

All API functions are in smoosh.erl and only the exported functions in
this module should be called from outside of the smoosh application.

Additionally, smoosh responds to config changes dynamically and these
changes are the principal means of interacting with smoosh.

Top-Level Settings
------------------

The main settings one interacts with are:

<dl>
<dt>db_channels<dd>A comma-separated list of channel names for
databases.
<dt>view_channels<dd>A comma-separated list of channel names for
views.
<dt>staleness<dd>The number of minutes that the (expensive) priority
calculation can be stale for before it is recalculated. Defaults to 5.
</dl>

Sometimes it's necessary to use the following:

<dl>
<dt>cleanup_index_files</dt><dd>Whether smoosh cleans up the files
for indexes that have been deleted. Defaults to false and probably
shouldn't be changed unless the cluster is running low on disk space,
and only after considering the ramifications.</dd>
<dt>wait_secs</dt><dd>The time a channel waits before starting compactions 
to allow time to observe the system and make a smarter decision about what 
to compact first. Hardly ever changed from the default. Default 30 (seconds).
</dd>
</dl>

Channel Settings
----------------

A channel has several important settings that control runtime
behavior.

<dl>
<dt>capacity<dd>The maximum number of items the channel can hold (lowest priority item is removed to make room for new items). Defaults to 9999.
<dt>concurrency<dd>The maximum number of jobs that can run concurrently. Defaults to 1.
<dt>max_priority<dd>The item must have a priority lower than this to be enqueued. Defaults to infinity.
<dt>max_size<dd>The item must be no larger than this many bytes in length to be enqueued. Defaults to infinity.
<dt>min_priority<dd>The item must have a priority at least this high to be enqueued. Defaults to 5.0 for ratio and 16 mb for slack.
<dt>min_changes<dd>The minimum number of changes since last compaction before the item will be enqueued. Defaults to 0. Currently only works for databases.
<dt>min_size<dd>The item must be at least this many bytes in length to be enqueued. Defaults to 1mb (1048576 bytes).
<dt>priority<dd>The method used to calculate priority. Can be ratio (calculated as disk_size/data_size) or slack (calculated as disk_size-data_size). Defaults to ratio.
</dl>

Structure
---------

Smoosh consists of a central gen_server (smoosh_server) which manages
a number of subordinate smoosh_channel gen_servers. This is not
properly managed by OTP yet.

Compaction Scheduling Algorithm
-------------------------------

Smoosh decides whether to compact a database or view by evaluating the
item against the selection criteria of each _channel_ in the order
they are configured. By default there are two channels for databases
("ratio_dbs" and "slack_dbs"), and two channels for views ("ratio_views"
and "slack_views")

Smoosh will enqueue the new item to the first channel that accepts
it. If none accept it, the item is not enqueued for compaction.

Notes on the data_size value
----------------------------

Every database and view shard has a data_size value. In CouchDB this
accurately reflects the post-compaction file size. In DbCore, it is
the size of the file that we bill for. It excludes the b+tree and
database footer overhead. We also bill customers for the uncompressed
size of their documents, though we store them compressed on disk.
These two systems were developed independently (ours predates
CouchDB's) and DbCore only calculates the billing size value.

Because of the way our data_size is currently calculated, it can
sometimes be necessary to enqueue databases and views with very low
ratios. Due to this, it is also currently impossible to tell how
optimally compacted a cluster is.

Example config commands
-----------------------

Change the set of database channels;

    config:set("smoosh", "db_channels", "small_dbs,medium_dbs,large_dbs").

Change the set of database channels on all live nodes in the cluster;

    rpc:multicall(config, set, ["smoosh", "db_channels", "small_dbs,medium_dbs,large_dbs"]).

Change the concurrency of the ratio_dbs database channel to 2

    config:set("smoosh.ratio_dbs", "concurrency", "2").

Change it on all live nodes in the cluster;

    rpc:multicall(config, set, ["smoosh.ratio_dbs", "concurrency", "2"]).

Example API commands
--------------------

smoosh:status()

This prints the state of each channel; how many jobs they are
currently running and how many jobs are enqueued (as well as the
lowest and highest priority of those enqueued items). The idea is to
provide, at a glance, sufficient insight into smoosh that an operator
can assess whether smoosh is adequately targeting the reclaimable
space in the cluster. In general, a healthy status output will have
items in the ratio_dbs and ratio_views channels. Owing to the default
settings, the slack_dbs and slack_views will almost certainly have
items in them. Historically, we've not found that the slack channels,
on their own, are particularly adept at keeping things well compacted.

smoosh:enqueue_all_dbs(), smoosh:enqueue_all_views()

These functions do just what they say but should not generally need to
be called, smoosh is supposed to be autonomous. Call them if you get
alerted to a disk space issue, they might well help. If they do, that
indicates a bug in smoosh as it should already have enqueued eligible
shards once they met the configured settings.



