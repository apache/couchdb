---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Background jobs with FoundationDB'
labels: rfc, discussion
assignees: ''

---

[NOTE]: # ( ^^ Provide a general summary of the RFC in the title above. ^^ )

# Introduction

This document describes a data model, implementation, and an API for running
CouchDB background jobs with FoundationDB.

## Abstract

CouchDB background jobs are used for things like index building, replication
and couch-peruser processing. We present a generalized model which allows
creation, running, and monitoring of these jobs.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).


## General Concepts

In the discussion below a job is considered to be an abstract unit of work. It
is identified by a `JobId` and has a `JobType`. Client code creates a job which
is then is executed by a job processor. A job processor is language-specific
execution unit that runs the job. It could be an Erlang process, a thread, or
just a function.

The API used to create jobs is called the `Job Creation API` and the API used
by the job processors to run jobs is called the `Job Processing API`.

### Job States

Jobs in the system can be in 3 states. After a job is added and
is waiting to run, the job is considered to be `pending`. A job executed by
a job processor is considered to be `running`. When a job is neither `running`,
nor `pending`, it is considered to be `finished`. This is the state transition
diagram:

```
         +------------>+
         |             |
         |             v
 -->[PENDING]     [RUNNING]--->[FINISHED]
         ^             |           |
         |             v           |
         +-------------+<----------+
```



### Typical API Usage

The general pattern of using this API might look like:

  * Job creators:
    - Call `add/4,5` to add a job
    - Call `remove/3` to remove it

  * Job processors:
    - Call `accept/1,2` and wait until it gets a job to process.
    - Periodically call `update/2,3` to prevent the job from being re-enqueued
      due to idleness.
    - When done running a job, call `finish/2,3`


### Job Creation API

```
add(Tx, Type, JobId, JobData[, ScheduledTime]) -> ok | {error, Error}
```
 - Add a job to be executed by a job processor
   - `JobData` is map with a job type-specific data in it. It MAY contain any
     data as long as it can be properly encoded as JSON.
   - `ScheduledTime` is an optional parameter to schedule the job to be executed
     at a later time. The format is an integer seconds since UNIX epoch.
   - If the job with the same `JobId` exists:
      * If it is `pending`, then the `ScheduledTime` is updated.
      * If it is `running` then the job is flagged to be resubmitted when it finishes running.
      * If it is `finished` then it will be re-enqueued as `pending`

```
remove(Tx, Type, JobId) -> ok | {error, Error}
```
 - Remove a job. If it is running, it will be stopped.

```
get_job_data(Job) -> {ok, JobData} | {error, Error}
```
 - Get `JobData` associated with the job.

```
get_job_state(Job) -> {ok, pending | running | finished} | {error, Error}
```
 - Get the job's state.

```
set_type_timeout(Type, TimeoutSec) -> ok
```

 - Set the activity timeout for a job type. This function needs to be called
   once for each job type before any job of that type is added.

```
get_type_timeout(Type)  -> {ok, TimeoutSec} | {error, Error}
```

 - Get the type timeout for a job type.

```
subscribe(Type, JobId) -> {ok, SubscriptionId, JobState}
```

 - Subscribe to receive job state updates. Notifications can be received using
 the `wait/2,3` calls.

```
unsubscribe(SubscriptionId) -> ok
```
 - Unsubscribe from receiving job state updates.

```
wait(SubscriptionId, Timeout) -> {Type, JobId, JobState} | timeout
wait([SubscriptionId], Timeout) -> {Type, JobId, JobState} | timeout

```
 - Receive subscription notification updates from one or more subscriptions.

```
wait(SubscriptionId, Type, Timeout) -> {Type, JobId, JobState} | timeout
wait([SubscriptionId], Type, Timeout) -> {Type, JobId, JobState} | timeout

```
 - Receive subscription notification updates for one particular state only.
   Updates for any other state will be ignored. This function can be used, for
   example, to wait until a job has finished running.


### Job Processing API

```
accept(Type[, OptionsMap]) -> {ok, Job} | {error, Error}
```

 - Get a `pending` job and start running it. `OptionsMap` is a map that MAY
   have these parameters:
    * `no_schedule` = `true` | `false` Use a more optimized dequeueing strategy
      if time-based scheduling is not used and job IDs are known to start with
      a random looking (UUID-like) prefix.
    * `max_sched_time` = `SecondsSinceEpoch` : Only accept jobs which have been
      scheduled before or at `SecondsSinceEpoch` UNIX time.
    * `timeout` = `TimeoutMSec` : Maximum timeout to wait when there are no
      pending jobs available. `0` means don't wait at all and return `{error,
      not_found}` immediately, effectively making `accept/1,2` non-blocking.


```
update(Tx, Job[, JobData]) -> {ok, Job} | {error, halt | Error}

```
 - This MAY be called to update a job's `JobData`. It MUST be called at least
   as often as the configured timeout value for the job’s type. Not doing this
   will result in the job being re-enqueued. If `halt` is returned, the job
   processor MUST stop running the job. Job processors MUST call `update/2,3`
   in any write transactions it performs in order to guarantee mutual exclusion
   that at most one job processor is executing a particular job at a time.

```
finish(Tx, Job[, JobData]) -> ok | {error, halt | Error}
```
 - Called by the job processor when it has finished running the job. The
   `JobData` parameter MAY contain a final result. If `halt` is returned, it
   means that the `JobData` value wasn't updated. Job processors MUST call
   `update/2,3` or `finish/2,3` in any write transactions it performs in order
   to guarantee mutual exclusion that at most one job processor is executing a
   particular job at a time.

```
resubmit(Tx, Job[, ScheduledTime]) -> {ok, Job} | {error, Error}
```
 - Mark the job for resubmission. The job won't be re-enqueued until
   `finish/2,3` is called.

```
is_resubmitted(Job) -> true | false
```
 - Check if the job object was marked for resubmission. The job processor MAY
   call this function on the `Job` object that gets returned from the
   `update/2,3` function to determine if job creator had requested the job to
   be resubmitted. The job won't actually be re-enqueued until `finish/2,3`
   function is called.

# Framework Implementation Details

This section discusses how some of the framework functionality is implemented.

All the coordination between job creation and job processing is done via
FoundationDB. There is a top level `"couch_jobs"` subspace. All the subspaces
mentioned below will be under this subspace.

Each job managed by the framework will have an entry in the main `jobs table`.
Pending jobs are added to a `pending queue` subspace. When they are
accepted by a jobs processor, the jobs are removed from the pending queue and added
to the `active jobs` subspace.

Job states referenced in the API section are essentially defined based on the
presence in any of these subspaces:

 * If a job is in the `pending queue` it is considered `pending`
 * If a job is in the `active jobs` subspace, then it is `running`
 * If a job is not `pending` or `running` then it is considered `finished`

### Activity Monitor

Job processors may suddenly crash and stop running their jobs. In that case the
framework will automatically make those jobs `pending` after a timeout. That
ensures the jobs continue to make progress. To avoid getting re-enqueued as
`pending` due the timeout, each job processor must periodically call the
`update/2,3` function. That functionality is implemented by the `activity
monitor`. It periodically watches a per-type versionstamp-ed key, then scans
`active jobs` subspace for any `running` jobs which haven't updated their
entries during the timeout period.

### Subscription Notifications

Subscription notifications are managed separately for each job type. They use
a per-type versionstamp-ed watch to monitor which jobs have updated since
the last time it delivered notifications to the subscribers.

### Data Model

 * `("couch_jobs", "data", Type, JobId) = (Sequence, JobLock, ScheduledTime, Resubmit, JobData)`
 * `("couch_jobs", "pending", Type, ScheduledTime, JobId) = ""`
 * `("couch_jobs", "watches_pending", Type) = Sequence`
 * `("couch_jobs", "watches_activity", Type) = Sequence`
 * `("couch_jobs", "activity_timeout", Type) = ActivityTimeout`
 * `("couch_jobs", "activity", Type, Sequence) = JobId`


### Job Lifecycle Implementation

This section describes how the framework implements some of the API functions.

 - `add/4,5` :
   * Add the new job to the main jobs table.
   * If a job with the same `JobId` exists, resubmit the job.
   * Update `"pending"` watch for the type with a new versionstamp and bump its
     counter.
   * `JobLock` is set to `null`.

 - `remove/3` :
   * Job is removed from the main jobs table.
   * Job processor during the next `update/2,3` call will get a `halt` error
     and know to stop running the job.

 - `accept/1,2` :
   * Generate a unique `JobLock` UUID.
   * Attempt to dequeue the item from the pending queue, then assign it the
     `JobLock` in the jobs table.
   * Create an entry in the `"activity"` subspace.
   * If there are no pending jobs, get a watch for the `"pending"` queue and
     wait until it fires, then try again.

 - `update/2,3`:
   * If job is missing from the main jobs table return `halt`.
   * Check if `JobLock` matches, otherwise return `halt`.
   * Delete old `"activity"` sequence entry.
   * Maybe update `JobData`.
   * Create a new `"activity"` sequence entry and in main job table.
   * Update `"watches"` sequence for that job type.

 - `finish/2,3`:
   * If job is missing from the main jobs table return `halt`.
   * Check if `JobLock` matches, otherwise returns `halt`.
   * Delete old `"activity"` sequence entry.
   * If `Resubmit` field is `true`, re-enqueue the job, and set `Resubmit` to `false`.
   * Set job table's `JobLock` to `null`

 - `resubmit/2,3`:
   * Set the `Resubmit` field to `true`.
   * The job will be re-enqueued when `finish/2,3` is called.


# Advantages and Disadvantages

The main advantage is having a central way to coordinate batch processing
across a cluster, with a single, unified API.


## Possible Future Extensions

Since all job keys and values are just FDB tuples and JSON encoded objects, in
the future it might be possible to accept external jobs, not just jobs defined
by the CouchDB internals. Also, since workers could be written in any language
as long as they can talk to the FDB cluster, and follow the behavior describes
in the design, it opens the possibility to have custom (user defined) workers
of different types. But that is out of scope in the current RFC discussion.

# Key Changes

 - New job execution framework
 - A single global job queue for each job type
 - An activity monitor to ensure jobs continue to make progress

## Applications and Modules Affected

Replication, indexing, couch-peruser

## HTTP API Additions

None. However, in the future, it might be useful to have an API to query and
monitor the state of all the queues and workers.

## HTTP API Deprecations

None have been identified.

# Security Considerations

None have been identified.

# References

[Original mailing list discussion](https://lists.apache.org/thread.html/9338bd50f39d7fdec68d7ab2441c055c166041bd84b403644f662735@%3Cdev.couchdb.apache.org%3E)

# Co-authors
  - @davisp

# Acknowledgments
 - @davisp
 - @kocolosk
 - @garrensmith
 - @rnewson
 - @mikerhodes
 - @sansato
