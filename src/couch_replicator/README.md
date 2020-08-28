Developer Oriented Replicator Description
=========================================

This description of scheduling replicator's functionality is mainly geared to
CouchDB developers. It dives a bit into the internal and explains how
everything is connected together. A higher level overview is available in the
[RFC](https://github.com/apache/couchdb-documentation/pull/581). This
documention assumes the audience is familiar with that description as well as
with the [Couch Jobs
RFC](https://github.com/apache/couchdb-documentation/blob/master/rfcs/007-background-jobs.md)
as well as with the [Node Types
RFC](https://github.com/apache/couchdb-documentation/blob/master/rfcs/013-node-types.md).

A natural place to start is the top application supervisor:
`couch_replicator_sup`. The set of children in the supervisor is split into
`frontend` and `backend`. The `frontend` set is started on nodes which have the
`api_frontend` node type label set to `true`, and `backend` ones are started on
nodes which have the `replication` label set to `true`. The same node could
have both them set to `true`, and it could act as a replication front and
backend node. However, it is not guaranteed that jobs which are created by the
frontend part will necessarily run on the backend on the same node.


Frontend Description
--

The "frontend" consists of the parts which handle HTTP requests and monitor
`_replicator` databases for changes and then create `couch_jobs` replication
job records. Some of the modules involved in this are:

 * `couch_replicator` : Contains the main API "entry" point into the
   `couch_replicator` application. The `replicate/2` function creates transient
   replication jobs. `after_db_create/2`, `after_db_delete/2`,
   `after_doc_write/6` functions are called from `couch_epi` callbacks to
   create replication jobs from `_replicator` db events. Eventually they all
   call `couch_replicator_jobs:add_job/3` to create a `couch_jobs` replication
   job. Before the job is created, either the HTTP request body or the
   `_replicator` doc body is parsed into a `Rep` map object. An important
   property of this object is that it can be serialized to JSON and
   deserialized from JSON. This object is saved in the `?REP` field of the
   replication `couch_jobs` job data. Besides creating replication job
   `couch_replicator` is also responsible for handling `_scheduler/jobs` and
   `_scheduler/docs` monitoring API response. That happens in the `jobs/0`,
   `job/1`, `docs/` and `doc/2` function.

Backend Description
--

The "backend" consists of parts which run replication jobs, update their state,
and handle rescheduling on intermettent errors. All the job activity on these
nodes is ultumately driven from `couch_jobs` acceptors which wait in
`couch_jobs:accept/2` for replication jobs.

 * `couch_replicator_job_server` : A singleton process in charge of which
   spawning and keeping track of `couch_replicator_job` processes. It ensures
   there is a limited number of replication jobs running on each node. It
   periodically accepts new jobs and stopping the oldest running ones in order
   to give other pending jobs a chance to run. It runs this logic in the
   `reschedule/1` function. That function is called with a frequency defined by
   the `interval_sec` configuration setting. The other pramers which determine
   how jobs start and stop are `max_jobs` and `max_churn`. The node will try to
   limit running up to `max_jobs` job on average with periodic spikes of up to
   `max_jobs + max_churn` job at a time, and it will try not to start more than
   `max_churn` number of job during each rescheduling cycle.

 * `couch_replicator_connection`: Maintains a global replication connection
    pool. It allows reusing connections across replication tasks. The main
    interface is `acquire/1` and `release/1`. The general idea is once a
    connection is established, it is kept around for
    `replicator.connection_close_interval` milliseconds in case another
    replication task wants to re-use it. It is worth pointing out how linking
    and monitoring is handled: workers are linked to the connection pool when
    they are created. If they crash, the connection pool will receive an 'EXIT'
    event and clean up after the worker. The connection pool also monitors
    owners (by monitoring the `Pid` from the `From` argument in the call to
    `acquire/1`) and cleans up if owner dies, and the pool receives a 'DOWN'
    message. Another interesting thing is that connection establishment
    (creation) happens in the owner process so the pool is not blocked on it.

 * `couch_replicator_rate_limiter`: Implements a rate limiter to handle
    connection throttling from sources or targets where requests return 429
    error codes. Uses the Additive Increase / Multiplicative Decrease feedback
    control algorithm to converge on the channel capacity. Implemented using a
    16-way sharded ETS table to maintain connection state. The table sharding
    code is split out to `couch_replicator_rate_limiter_tables` module. The
    purpose of the module it to maintain and continually estimate sleep
    intervals for each connection represented as a `{Method, Url}` pair. The
    interval is updated accordingly on each call to `failure/1` or `success/1`
    calls. For a successful request, a client should call `success/1`. Whenever
    a 429 response is received the client should call `failure/1`. When no
    failures are happening the code ensures the ETS tables are empty in order
    to have a lower impact on a running system.



