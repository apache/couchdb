---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Replicator Implementation On FDB'
labels: rfc, discussion
assignees: 'vatamane@apache.org'

---

# Introduction

This document describes the design of the replicator application for CouchDB
4.x. The replicator will rely on `couch_jobs` for centralized scheduling and
monitoring of replication jobs.

## Abstract

Replication jobs can be created from documents in `_replicator` databases, or
by `POST`-ing requests to the HTTP `/_replicate` endpoint. Previously, in
CouchDB <= 3.x, replication jobs were mapped to individual cluster nodes and a
scheduler component would run up to `max_jobs` number of jobs at a time on each
node. The new design proposes using `couch_jobs`, as described in the
[Background Jobs
RFC](https://github.com/apache/couchdb-documentation/blob/main/rfcs/007-background-jobs.md),
to have a central, FDB-based queue of replication jobs. `couch_jobs`
application will manage job scheduling and coordination. The new design also
proposes using heterogeneous node types as defined in the [Node Types
RFC](https://github.com/apache/couchdb-documentation/blob/main/rfcs/013-node-types.md)
such that replication jobs will be created only on `api_frontend` nodes and run
only on `replication` nodes.

## Requirements Language

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be
interpreted as described in [RFC
2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

`_replicator` databases : A database that is either named `_replicator` or ends
with the `/_replicator` suffix.

`transient` replications : Replication jobs created by `POST`-ing to the
`/_replicate` endpoint.

`persistent` replications : Replication jobs defined in document in a
`_replicator` database.

`continuous` replications : Replication jobs created with the `"continuous":
true` parameter. These jobs will try to run continuously until the user removes
them. They may be temporarily paused to allow other jobs to make progress.

`one-shot` replications : Replication jobs which are not `continuous`. If the
`"continuous":true` parameter is not specified, by default, replication jobs
will be `one-shot`. These jobs will try to run until they reach the end of the
changes feed, then stop.

`api_frontend node` : Database node which has the `api_frontend` type set to
`true` as described in
[RFC](https://github.com/apache/couchdb-documentation/blob/main/rfcs/013-node-types.md).
Replication jobs can be only be created on these nodes.

`replication node` : Database node which has the `replication` type set to
`true` as described in
[RFC](https://github.com/apache/couchdb-documentation/blob/main/rfcs/013-node-types.md).
Replication jobs can only be run on these nodes.

`filtered` replications: Replications with a user-defined filter on the source
endpoint to filter its changes feed.

`replication_id` : An ID defined by replication jobs, which is a hash of
replication parameters that affect the result of the replication. These may
include source and target endpoint URLs, as well as a filter function specified
in a design document on the source endpoint.

`job_id` : A replication job ID derived from the database and document IDs for
persistent replications, and from source, target endpoint, user name and some
options for transient replications. Computing a `job_id`, unlike a
`replication_id`, doesn't require making any network requests. A filtered
replication with a given `job_id` during its lifetime may change its
`replication_id` multiple times when filter contents changes on the source.

`max_jobs` : Configuration parameter which specifies up to how many replication
jobs to run on each `replication` node.

`max_churn` : Configuration parameter which specifies a limit of how many new
jobs to spawn during each rescheduling interval.

`min_backoff_penalty` : Configuration parameter specifying the minimum (the
base) penalty applied to jobs which crash repeatedly.

`max_backoff_penalty` : Configuration parameter specifying the maximum penalty
applied to jobs which crash repeatedly.

---

# Detailed Description

Replication job creation and scheduling works roughly as follows:

 1) `Persistent` and `transient` jobs both start by creating or updating a
 `couch_jobs` record in a separate replication key-space on `api_frontend`
 nodes. Persistent jobs are driven by the `couch_epi` callback mechanism which
 notifies `couch_replicator` application when documents in `_replicator` DBs
 are updated, or when `_replicator` DBs are created and deleted. Transient jobs
 are created from the `_replicate` HTTP handler directly. Newly created jobs
 are in a `pending` state.

 2) Each `replication` node spawns some acceptor processes which wait in
 `couch_jobs:accept/2` call for jobs. It will accept only jobs which are
 scheduled to run at a time less or equal to the current time.

 3) After a job is accepted, its state is updated to `running`, and then, a
 gen_server process monitoring these replication jobs will spawn another
 acceptor. That happens until the `max_jobs` limit is reached.

 4) The same monitoring gen_server will periodically check if there are any
 pending jobs in the queue and, if there are, spawn up to some `max_churn`
 number of new acceptors. These acceptors may start new jobs and, if they do,
 for each one of them, the oldest running job will be stopped and re-enqueued
 as `pending`. This in large follows the logic from the replication scheduler
 in CouchDB <= 3.x except that is uses `couch_jobs` as the central queuing and
 scheduling mechanism.

 5) After the job is marked as `running`, it computes its `replication_id`,
 initializes an internal replication state record from job's data object, and
 starts replicating. Underneath this level the logic is identical to what's
 already happening in CouchDB <= 3.x and so it is not described further in this
 document.

 6) As jobs run, they periodically checkpoint, and when they do that, they also
 recompute their `replication_id`. In the case of filtered replications the
 `replication_id` may change, and if so, that job is stopped and re-enqueued as
 `pending`. Also, during checkpointing the job's data value is updated with
 stats such that the job stays active and doesn't get re-enqueued by the
 `couch_jobs` activity monitor.

 7) If the job crashes, it will reschedule itself in `gen_server:terminate/2`
 via `couch_jobs:resubmit/3` call to run again at some future time, defined
 roughly as `now + max(min_backoff_penalty * 2^consecutive_errors,
 max_backoff_penalty)`. If a job starts and successfully runs for some
 predefined period of time without crashing, it is considered to be `"healed"`
 and its `consecutive_errors` count is reset to 0.

 8) If the node where replication job runs crashes, or the job is manually
 killed via `exit(Pid, kill)`, `couch_jobs` activity monitor will automatically
 re-enqueue the job as `pending`.

## Replicator Job States

### Description

The set of replication job states is defined as:

 * `pending` : A job is marked as `pending` in these cases:
    - As soon as a job is created from an `api_frontend` node
    - When it stopped to let other replication jobs run
    - When a filtered replication's `replication_id` changes

 * `running` : Set when a job is accepted by the `couch_jobs:accept/2`
   call. This generally means the job is actually running on a node,
   however, in cases when a node crashes, the job may show as
   `running` on that node until `couch_jobs` activity monitor
   re-enqueues the job, and it starts running on another node.

 * `crashing` : The job was running, but then crashed with an intermittent
   error. Job's data has an error count which is incremented, and then a
   backoff penalty is computed and the job is rescheduled to try again at some
   point in the future.

 * `completed` : One-Shot replications which have completed

 * `failed` : This can happen when:
    - A replication job could not be parsed from a replication document. For
      example, if the user has not specified a `"source"` field.
    - A transient replication job crashes. Transient jobs don't get rescheduled
      to run again after they crash.
    - There already is another persistent replication job running or pending
      with the same `replication_id`.

### State Differences From CouchDB <= 3.x

The set of states is slightly different than the ones from before. There are
now fewer states as some of them have been combined together:

 * `initializing` was combined with `pending`

 * `error` was combined with `crashing`

### Mapping Between couch_jobs States and Replication States

`couch_jobs` application has its own set of state definitions and they map to
replicator states like so:

 | Replicator States| `couch_jobs` States
 | ---              | :--
 | pending          | pending
 | running          | running
 | crashing         | pending
 | completed        | finished
 | failed           | finished

### State Transition Diagram

Jobs start in the `pending` state, after either a `_replicator` db doc
update, or a POST to the `/_replicate` endpoint. Continuous jobs, will
normally toggle between `pending` and `running` states. One-Shot jobs
may toggle between `pending` and running a few times and then end up
in `completed`.

```
_replicator doc       +-------+
POST /_replicate ---->+pending|
                      +-------+
                          ^
                          |
                          |
                          v
                      +---+---+      +--------+
            +---------+running+<---->|crashing|
            |         +---+---+      +--------+
            |             |
            |             |
            v             v
        +------+     +---------+
        |failed|     |completed|
        +------+     +---------+
```


## Replication ID Collisions

Multiple replication jobs may specify replications which map to the same
`replication_id`. To handle these collisions there is an FDB subspace `(...,
LayerPrefix, ?REPLICATION_IDS, replication_id) -> job_id` to keep track of
them. After the `replication_id` is computed, each replication job checks if
there is already another job pending or running with the same `replication_id`.
If the other job is transient, then the current job will reschedule itself as
`crashing`. If the other job is persistent, the current job will fail
permanently as `failed`.

## Replication Parameter Validation

`_replicator` documents in CouchDB <= 3.x were parsed and validated in a
two-step process:

  1) In a validate-doc-update (VDU) javascript function from a programmatically
  inserted _design document. This validation happened when the document was
  updated, and performed some rough checks on field names and value types. If
  this validation failed, the document update operation was rejected.

  2) Inside replicator's Erlang code when it was translated to an internal
 record used by the replication application. This validation was more thorough
 but didn't have very friendly error messages. If validation failed here, the
 job would be marked as `failed`.

For CouchDB 4.x the proposal is to use only the Erlang parser. It would be
called from the `before_doc_update` callback. This is a callback which runs
before every document update. If validation fails there it would reject the
document update operation. This should reduce code duplication and also provide
better feedback to the users directly when they update the `_replicator`
documents.

## Transient Job Behavior

In CouchDB <= 3.x transient replication jobs ran in memory on a particular node
in the cluster. If the node where the replication job ran crashes, the job
would simply disappear without a trace. It was up to the user to periodically
monitor the job status and re-create the job. In the current design,
`transient` jobs are persisted to FDB as `couch_jobs` records, and so would
survive node restarts. Also after transient jobs complete or failed,
they used to disappear immediately. This design proposes keeping them around
for a configurable emount of time to allow users to retrieve their status via
`_scheduler/jobs/$id` API.

## Monitoring Endpoints

`_active_tasks`, `_scheduler/jobs` and `_scheduler/docs` endpoint are handled
by traversing the replication job's data using a new `couch_jobs:fold_jobs/4`
API function to retrieve each job's data. `_active_tasks` implementation
already works that way and `_scheduler/*` endpoint will work similarly.

## Replication Documents Not Updated For Transient Errors

Configuration
[option](https://docs.couchdb.org/en/latest/replication/replicator.html?highlight=update_docs#compatibility-mode)
`[replicator] update_docs = false` was introduced with the scheduling
replicator in a 2.x release. It controls whether to update replication
documents with transient states like `triggered` and `error`. It defaulted to
`false` and was mainly for compatibility with older monitoring user scripts.
That behavior now becomes hard-coded such that replication documents are only
updated with terminal states of `failed` and `completed`. Users should use
`_scheduler/docs` API to check for completion status instead.


# Advantages and Disadvantages

Advantages:

 * Simplicity: re-using `couch_jobs` means having a lot less code to maintain
   in `couch_replicator`. In the draft implementation there are about 3000
   lines of code saved compared to the replicator application in CouchDB 3.x

 * Simpler endpoint and monitoring implementation

 * Fewer replication job states to keep track of

 * Transient replications can survive node crashes and restarts

 * Simplified and improved validation logic

 * Using node types allows tightening firewall rules such that only
   `replication` nodes are the ones which may make arbitrary requests outside
   the cluster, and `frontend_api` nodes are the only ones that may accept
   incoming connections.

Disadvantages:

 * Behavior changes for transient jobs

 * Centralized job queue might mean handling some number of conflicts generated
   in the FDB backend when jobs are accepted. These are mitigated using the
   `startup_jitter` configuration parameter and a configurable number of max
   acceptors per node.

 * In monitoring API responses, `running` job state might not immediately
   reflect the running process state on the replication node. If the node
   crashes, it might take up to a minute or two until the job is re-enqueued by
   the `couch_jobs` activity monitor.

# Key Changes

 * Behavior changes for transient jobs

 * A delay in `running` state as reflected in monitoring API responses

 * `[replicator] update_docs = false` configuration option becomes hard-coded

## Applications and Modules affected

 * couch_jobs : New APIs to fold jobs and get pending count job estimate

 * fabric2_db : Adding EPI db create/delete callbacks

 * couch_replicator :
    - Remove `couch_replicator_scheduler*` modules
    - Remove `couch_replicator_doc_processor_*` modules
    - `couch_replicator` : job creation and a general API entry-point for
      couch_replicator.
    - `couch_replicator_job` : runs each replication job
    - `couch_replicator_job_server` : replication job monitoring gen_server
    - `couch_replicator_parse` : parses replication document and HTTP
      `_replicate` POST bodies

## HTTP API additions

N/A

## HTTP API deprecations

N/A

# Security Considerations

Ability to confine replication jobs to run on `replication` nodes improves the
security posture. It is possible to set up firewall rules which allow egress
traffic sent out only from those nodes.

# References

* [Background Jobs RFC](https://github.com/apache/couchdb-documentation/blob/main/rfcs/007-background-jobs.md)

* [Node Types RFC](https://github.com/apache/couchdb-documentation/blob/main/rfcs/013-node-types.md)

* [CouchDB 3.x replicator implementation](https://github.com/apache/couchdb/blob/3.x/src/couch_replicator/README.md)

# Co-authors

 * @davisp

# Acknowledgements

 * @davisp
