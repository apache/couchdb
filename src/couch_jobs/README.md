CouchDB Jobs Application
========================

Run background jobs in CouchDB

Design (RFC) discussion: https://github.com/apache/couchdb-documentation/pull/409/files

This is a description of some of the modules:

 * `couch_jobs`: The main API module. It contains functions for creating,
   accepting, executing, and monitoring jobs. A common pattern in this module
   is to get a jobs transaction object (named `JTx` throughout the code), then
   start a transaction and call a bunch of functions from `couch_jobs_fdb` in
   that transaction.

 * `couch_jobs_fdb`: This is a layer that talks to FDB. There is a lot of tuple
   packing and unpacking, reading ranges and also managing transaction objects.

 * `couch_jobs_pending`: This module implements the pending jobs queue. These
   functions could all go in `couch_jobs_fdb` but the implemention was fairly
   self-contained, with its own private helper functions, so it made sense to
   move to a separate module.

 * `couch_jobs_activity_monitor`: Here is where the "activity monitor"
   functionality is implemented. That's done with a `gen_server` instance
   running for each type. This `gen_server` periodically check if there are
   inactive jobs for its type, and if they are, it re-enqueues them. If the
   timeout value changes, then it skips the pending check, until the new
   timeout expires.

 * `couch_jobs_activity_monitor_sup` : This is a simple one-for-one supervisor
   to spawn `couch_jobs_activity_monitor` instances for each type.

 * `couch_jobs_type_monitor` : This is a helper process meant to be
   `spawn_link`-ed from a parent `gen_server`. It then monitors activity for a
   particular job type. If any jobs of that type have an update it notifies the
   parent process.

 * `couch_jobs_notifier`: Is responsible for subscriptions. Just like
   with activity monitor there is a `gen_server` instance running per
   each type. It uses a linked `couch_jobs_type_monitor` process to wait for
   any job updates. When an update notification arrives, it can efficiently
   find out if any active jobs have been updated, by reading the `(?JOBS,
   ?ACTIVITY, Type, Sequence)` range. That should account for the bulk of
   changes. The jobs that are not active anymore, are queried individually.
   Subscriptions are managed in an ordered set ETS table.

 * `couch_jobs_notifier_sup`: A simple one-for-one supervisor to spawn
   `couch_jobs_notifier` processes for each type.

 * `couch_jobs_server`: This is a `gen_server` which keeps track of job
   types. It then starts or stops activity monitors and notifiers for each
   type. To do that it queries the ` (?JOBS, ?ACTIVITY_TIMEOUT)` periodically.

 * `couch_jobs_sup`: This is the main application supervisor. The restart
   strategy is `rest_for_one`, meaning that a when a child restarts, the
   sibling following it will restart. One interesting entry there is the first
   child which is used just to create an ETS table used by `couch_jobs_fdb` to
   cache transaction object (`JTx` mentioned above). That child calls
   `init_cache/0`, where it creates the ETS then returns with `ignore` so it
   doesn't actually spawn a process. The ETS table will be owned by the
   supervisor process.
