Developer Oriented Replicator Description
=========================================

This description of scheduling replicator's functionality is mainly geared to
CouchDB developers. It dives a bit into the internal and explains how
everything is connected together.

A natural place to start is the top application supervisor:
`couch_replicator_sup`. It's a `rest_for_one` so if a child process terminates,
the rest of the children in the hierarchy following it are also terminated.
This structure implies a useful constraint -- children lower in the list can
safely call their siblings which are higher in the list.

A description of each child:

 * `couch_replication_event`: Starts a gen_event publication bus to handle some
    replication related events. This used for example, to publish cluster
    membership changes by the `couch_replicator_clustering` process. But is
    also used in replication tests to monitor for replication events.
    Notification is performed via the `couch_replicator_notifier:notify/1`
    function. It's the first (left-most) child because
    `couch_replicator_clustering` uses it.

 * `couch_replicator_clustering`: This module maintains cluster membership
    information for the replication application and provides functions to check
    ownership of replication jobs. A cluster membership change is published via
    the `gen_event` event server named `couch_replication_event` as previously
    covered. Published events are `{cluster, stable}` when cluster membership
    has stabilized, that it, no node membership changes in a given period, and
    `{cluster, unstable}` which indicates there was a recent change to the
    cluster membership and now it's considered unstable. Listeners for cluster
    membership change include `couch_replicator_doc_processor` and
    `couch_replicator_db_changes`. When doc processor gets an `{cluster,
    stable}` event it will remove all the replication jobs not belonging to the
    current node. When `couch_replicator_db_chanages` gets a `{cluster,
    stable}` event, it will restart the `couch_multidb_changes` process it
    controls, which will launch an new scan of all the replicator databases.

  * `couch_replicator_connection`: Maintains a global replication connection
    pool. It allows reusing connections across replication tasks. The Main
    interface is `acquire/1` and `release/1`. The general idea is once a
    connection is established, it is kept around for
    `replicator.connection_close_interval` milliseconds in case another
    replication task wants to re-use it. It is worth pointing out how linking
    and monitoring is handled: Workers are linked to the connection pool when
    they are created. If they crash, the connection pool will receive an 'EXIT'
    event and clean up after the worker. The connection pool also monitors
    owners (by monitoring the `Pid` from the `From` argument in the call to
    `acquire/1`) and cleans up if owner dies, and the pool receives a 'DOWN'
    message. Another interesting thing is that connection establishment
    (creation) happens in the owner process so the pool is not blocked on it.

 * `couch_replicator_rate_limiter` : Implements a rate limiter to handle
    connection throttling from sources or targets where requests return 429
    error codes. Uses the Additive Increase / Multiplicative Decrease feedback
    control algorithm to converge on the channel capacity. Implemented using a
    16-way sharded ETS table to maintain connection state. The table sharding
    code is split out to `couch_replicator_rate_limiter_tables` module. The
    purpose of the module it so maintain and continually estimate sleep
    intervals for each connection represented as a `{Method, Url}` pair. The
    interval is updated accordingly on each call to `failure/1` or `success/1`
    calls. For a successful request, a client should call `success/1`. Whenever
    a 429 response is received the client should call `failure/1`. When no
    failures are happening the code is ensuring the ETS tables are empty in
    order to have a lower impact on a running system.

 * `couch_replicator_scheduler` : This is the core component of the scheduling
    replicator. It's main task is to switch between replication jobs, by
    stopping some and starting others to ensure all of them make progress.
    Replication jobs which fail are penalized using an exponential backoff.
    That is, each consecutive failure will double the time penalty. This frees
    up system resources for more useful work than just continuously trying to
    run the same subset of failing jobs.

    The main API function is `add_job/1`. Its argument is an instance of the
    `#rep{}` record, which could be the result of a document update from a
    `_replicator` db or the result of a POST to `_replicate` endpoint.

    Each job internally is represented by the `#job{}` record. It contains the
    original `#rep{}` but also, maintains an event history. The history is a
    sequence of past events for each job. These are timestamped and ordered
    such that the most recent event is at the head. History length is limited
    based on the `replicator.max_history` configuration value. The default is
    20 entries. History events types are:

    * `added` : job was just added to the scheduler. This is the first event.
    * `started` : job was started. This was an attempt to run the job.
    * `stopped` : job was stopped by the scheduler.
    * `crashed` : job has crashed (instead of stopping cleanly).

    The core of the scheduling algorithm is the `reschedule/1` function. This
    function is called every `replicator.interval` milliseconds (default is
    60000 i.e. a minute). During each call the scheduler will try to stop some
    jobs, start some new ones and will also try to keep the maximum number of
    jobs running less than `replicator.max_jobs` (deafult 500). So the
    functions does these operations (actual code paste):

    ```
    Running = running_job_count(),
    Pending = pending_job_count(),
    stop_excess_jobs(State, Running),
    start_pending_jobs(State, Running, Pending),
    rotate_jobs(State, Running, Pending),
    update_running_jobs_stats(State#state.stats_pid)
    ```

    `Running` is the total number of currently runnig jobs. `Pending` is the
    total number of jobs waiting to be run. `stop_excess_jobs` will stop any
    exceeding the `replicator.max_jobs` configured limit. This code takes
    effect if user reduces the `max_jobs` configuration value.
    `start_pending_jobs` will start any jobs if there is more room available.
    This will take effect on startup or when user increases the `max_jobs`
    configuration value. `rotate_jobs` is where all the action happens. The
    scheduler picks `replicator.max_churn` running jobs to stop and then picks
    the same number of pending jobs to start. The default value of `max_churn`
    is 20. So by default every minute, 20 running jobs are stopped, and 20 new
    pending jobs are started.

    Before moving on it is worth pointing out that scheduler treats continuous
    and non-continuous replications differently. Normal (non-continuous)
    replications once started will be allowed to run to completion. That
    behavior is to preserve their semantics of replicating a snapshot of the
    source database to the target. For example if new documents are added to
    the source after the replication are started, those updates should not show
    up on the target database. Stopping and restarting a normal replication
    would violate that constraint. The only exception to the rule is the user
    explicitly reduces `replicator.max_jobs` configuration value. Even then
    scheduler will first attempt to stop as many continuous jobs as possible
    and only if it has no choice left will it stop normal jobs.

    Keeping that in mind and going back to the scheduling algorithm, the next
    interesting part is how the scheduler picks which jobs to stop and which
    ones to start:

    * Stopping: When picking jobs to stop the cheduler will pick longest
      running continuous jobs first. The sorting callback function to get the
      longest running jobs is unsurprisingly called `longest_running/2`. To
      pick the longest running jobs it looks at the most recent `started`
      event. After it gets a sorted list by longest running, it simply picks
      first few depending on the value of `max_churn` using `lists:sublist/2`.
      Then those jobs are stopped.

    * Starting: When starting the scheduler will pick the jobs which have been
      waiting the longest. Surprisingly, in this case it also looks at the
      `started` timestamp and picks the jobs which have the oldest `started`
      timestamp. If there are 3 jobs, A[started=10], B[started=7],
      C[started=9], then B will be picked first, then C then A. This ensures
      that jobs are not starved, which is a classic scheduling pitfall.

    In the code, the list of pending jobs is picked slightly differently than
    how the list of running jobs is picked. `pending_jobs/1` uses `ets:foldl`
    to iterate over all the pending jobs. As it iterates it tries to keep only
    up to `max_churn` oldest items in the accumulator. The reason this is done
    is that there could be a very large number of pending jobs and loading them
    all in a list (making a copy from ETS) and then sorting it can be quite
    expensive performance-wise. The tricky part of the iteration is happening
    in `pending_maybe_replace/2`. A `gb_sets` ordered set is used to keep top-N
    longest waiting jobs so far. The code has a comment with a helpful example
    on how this algorithm works.

    The last part is how the scheduler treats jobs which keep crashing. If a
    job is started but then crashes then that job is considered unhealthy. The
    main idea is to penalize such jobs such that they are forced to wait an
    exponentially larger amount of time with each consecutive crash. A central
    part to this algorithm is determining what forms a sequence of consecutive
    crashes. If a job starts then quickly crashes, and after next start it
    crashes again, then that would become a sequence of 2 consecutive crashes.
    The penalty then would be calcualted by `backoff_micros/1` function where
    the consecutive crash count would end up as the exponent. However for
    practical concerns there is also maximum penalty specified and that's the
    equivalent of 10 consecutive crashes. Timewise it ends up being about 8
    hours. That means even a job which keep crashing will still get a chance to
    retry once in 8 hours.

    There is subtlety when calculating consecutive crashes and that is deciding
    when the sequence stops. That is, figuring out when a job becomes healthy
    again. The scheduler considers a job healthy again if it started and hasn't
    crashed in a while. The "in a while" part is a configuration parameter
    `replicator.health_threshold` defaulting to 2 minutes. This means if job
    has been crashing, for example 5 times in a row, but then on the 6th
    attempt it started and ran for more than 2 minutes then it is considered
    healthy again. The next time it crashes its sequence of consecutive crashes
    will restart at 1.

 * `couch_replicator_scheduler_sup`: This module is a supervisor for running
   replication tasks. The most interesting thing about it is perhaps that it is
   not used to restart children. The scheduler handles restarts and error
   handling backoffs.

 * `couch_replicator_doc_processor`: The doc procesoor component is in charge
   of processing replication document updates, turning them into replication
   jobs and adding those jobs to the scheduler. Unfortunately the only reason
   there is even a `couch_replicator_doc_processor` gen_server, instead of
   replication documents being turned to jobs and inserted into the scheduler
   directly, is because of one corner case -- filtered replications using
   custom (Javascript mostly) filters. More about this later. It is better to
   start with how updates flow through the doc processor:

   Document updates come via the `db_change/3` callback from
   `couch_multidb_changes`, then go to the `process_change/2` function.

   In `process_change/2` a few decisions are made regarding how to proceed. The
   first is "ownership" check. That is a check if the replication document
   belongs on the current node. If not, then it is ignored. In a cluster, in
   general there would be N copies of a document change and we only want to run
   the replication once. Another check is to see if the update has arrived
   during a time when the cluster is considered "unstable". If so, it is
   ignored, because soon enough a rescan will be launched and all the documents
   will be reprocessed anyway. Another noteworthy thing in `process_change/2`
   is handling of upgrades from the previous version of the replicator when
   transient states were written to the documents. Two such states were
   `triggered` and `error`. Both of those states are removed from the document
   then then update proceeds in the regular fashion. `failed` documents are
   also ignored here. `failed` is a terminal state which indicates the document
   was somehow unsuitable to become a replication job (it was malforemd or a
   duplicate). Otherwise the state update proceeds to `process_updated/2`.

   `process_updated/2` is where replication document updates are parsed and
   translated to `#rep{}` records. The interesting part here is that the
   replication ID isn't calculated yet. Unsurprisingly the parsing function
   used is called `parse_rep_doc_without_id/1`. Also note that up until now
   everything is still running in the context of the `db_change/3` callback.
   After replication filter type is determined the update gets passed to the
   `couch_replicator_doc_processor` gen_server.

   The `couch_replicator_doc_processor` gen_server's main role is to try to
   calculate replication IDs for each `#rep{}` record passed to it, then add
   that as a scheduler job. As noted before, `#rep{}` records parsed up until
   this point lack a replication ID. The reason is replication ID calculation
   includes a hash of the filter code. And because user defined replication
   filters live in the source DB, which most likely involves a remote network
   fetch there is a possibility of blocking and a need to handle various
   network failures and retries. Because of that `replication_doc_processor`
   dispatches all of that blocking and retrying to a separate `worker` process
   (`couch_replicator_doc_processor_worker` module).

   `couch_replicator_doc_processor_worker` is where replication IDs are
   calculated for each individual doc update. There are two separate modules
   which contain utilities related to replication ID calculation:
   `couch_replicator_ids` and `couch_replicator_filters`. The first one
   contains ID calculation algorithms and the second one knows how to parse and
   fetch user filters from a remote source DB. One interesting thing about the
   worker is that it is time-bounded and is guaranteed to not be stuck forever.
   That's why it spawns an extra process with `spawn_monitor`, just so it can
   do an `after` clause in receive and bound the maximum time this worker will
   take.

   A doc processor worker will either succeed or fail but never block for too
   long. Success and failure are returned as exit values. Those are handled in
   the `worker_returned/3` doc processor clauses. The most common pattern is
   that a worker is spawned to add a replication job, it does so and returns a
   `{ok, ReplicationID}` value in `worker_returned`.

   In case of a filtered replication with custom user code there are two case to
   consider:

     1. Filter fetching code has failed. In that case worker returns an error.
        But because the error could be a transient network error, another
        worker is started to try again. It could fail and return an error
        again, then another one is started and so on. However each consecutive
        worker will do an exponential backoff, not unlike the scheduler code.
        `error_backoff/1` is where the backoff period is calculated.
        Consecutive errors are held in the `errcnt` field in the ETS table.

     2. Fetchig filter code succeeds, replication ID is calculated and job is
        added to the scheduler. However, because this is a filtered replication
        the source database could get an updated filter. Which means
        replication ID could change again. So the worker is spawned to
        periodically check the filter and see if it changed. In other words doc
        processor will do the work of checking for filtered replications, get
        an updated filter and will then refresh the replication job (remove the
        old one and add a new one with a different ID). The filter checking
        interval is determined by the `filter_backoff` function. An unusual
        thing about that function is it calculates the period based on the size
        of the ETS table. The idea there is for a few replications in a
        cluster, it's ok to check filter changes often. But when there are lots
        of replications running, having each one checking their filter often is
        not a good idea.

 * `couch_replicator`: This is an unusual but useful pattern. This child is not
   an actual process but a one-time call to the
   `couch_replicator:ensure_rep_db_exists/0` function, executed by the
   supervisor in the correct order (and monitored for crashes). This ensures
   the local replicator db exists, then returns `ignore`. This pattern is
   useful for doing setup-like things at the top level and in the correct order
   regdaring the rest of the children in the supervisor.

 * `couch_replicator_db_changes`: This process specializes and configures
   `couch_multidb_changes` so that it looks for `_replicator` suffixed shards
   and makes sure to restart it when node membership changes.


