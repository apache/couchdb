---
name: Formal RFC
about: Submit a formal Request For Comments for consideration by the team.
title: 'Fair Share Job Scheduling for CouchDB 3.x Replicator'
labels: rfc, discussion
assignees: 'vatamane@apache.org'

---

# Introduction

This document describes an improvement to the CouchDB 3.x replicator to
introduce fair resource sharing between replication jobs in different
_replicator databases.

## Abstract

Currently CouchDB replicator 3.x schedules jobs without any regard to what
database they originated from. If there are multiple `_replicator` dbs then
replication jobs from dbs with most jobs will consume most of the scheduler's
resources. The proposal is to implement a fair sharing scheme as described in
[A Fair Share Scheduler][2] paper by Judy Kay and Piers Lauder. It would allow
sharing replication scheduler resources fairly amongst `_replicator` dbs.

The idea was originally discussed on the [couchdb-dev][1] mailing list and the
use of the Fair Share algorithm suggested by Joan Touzet.

## Requirements Language

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be
interpreted as described in [RFC
2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

`_replicator` databases : A database that is either named `_replicator` or ends
with the `/_replicator` suffix.

`shares` : An abstract representation of entitlement to run on the replication
scheduler.

`usage` : A measure of resource usage by jobs from a particular `_replicator`
db. For the scheduling replicator this will be the total time spent running.

`continuous` replications : Replication jobs created with the `"continuous":
true` parameter. These jobs will try to run continuously until the user removes
them. They may be temporarily paused to allow other jobs to make progress.

`one-shot` replications : Replication jobs which are not `continuous`. If the
`"continuous":true` parameter is not specified, by default, replication jobs
will be `one-shot`. These jobs will try to run until they reach the end of the
changes feed, then stop.

`job priority` : A job attribute which indicates the likelihood of the job
being executed before other jobs. Following the convention in the "Fair Share"
paper, jobs with a lower priority value are at the front of the pending queue,
and get executed first.

`max_jobs` : Configuration parameter which specifies up to how many replication
jobs to run on each `replication` node.

`max_churn` : Configuration parameter which specifies a limit of how many new
jobs to spawn during each rescheduling interval.

---

# Detailed Description

The general idea behind the algorithm is to continuously monitor
per-`_replicator` jobs statistics and update each job's priorities in
proportion to the usage from all the jobs in the same `_replicator` db. To make
sure all jobs eventually get a chance to run and do not starve, all the
priorities are continuously boosted, such that jobs which haven't run for a
while, and maybe be starved, will eventually get a chance to run.

The algorithm has 3 basic components that can run mostly independently from
each other:

1) Keep track of `usage` for each `_replicator` db . In the paper this part is
called "user-level scheduling". As jobs run, they send reports to this
component. Those reports are accumulated for one period, then rolled up when
the period ends. There is also a decay coefficient applied to account for
recent historical usage (this is called `K1` in the paper). This ensures in
absence of jobs running from a particular `_replicator` db, the usage would
drops to 0 and the whole entry is removed from the table table altogether.

 Every `UsageUpdateInterval` seconds (called `t1` in the paper):
   For each `Db`:
     ```
     DecayCoeff = get_usage_decay_coefficient(0.5)
     AccumulatedUsage = get_accumulated_usage(Db),
     update_usage(Db, usage(Db) * DecayCoeff + AccumulatedUsage)
     reset_accumulated_usage(Db)
     ```

2) Uniformly decay all process priorities. Periodically lower the priority
values, and thus boost the priority, of all the pending and running jobs in the
system. The paper in this step applies a per-process "nice" value, which is
skipped in the initial proposal. It could be added later if needed.

 Every `UniformPriorityBoostInterval` seconds (called `t2` in the paper):
   For each `Job`:
     ```
     DecayCoeff = get_uniform_decay_coefficient(0.75),
     Job#job.priority = Job#job.priority * DecayCoeff
     ```

[note]: If jobs were scheduled to run at an absolute future time (a deadline) this step could be avoided. Then, the effect of all the jobs needing to periodically move to the front of the queue would be accomplished instead by the current time (i.e. `now()`) moving head along the time-line.

3) Adjust running process priority in proportion to the shares used by all the
jobs in the same db:

 Every `RunningPriorityReduceInterval` seconds (called `t3` in the paper):
   For each `Job`:
     ```
     Db = Job#job.db,
     SharesSq = shares(Db) * shares(Db),
     Job#job.priority = Job#job.priority + (usage(Db) * pending(Db)) / SharesSq
     ```

### How Jobs Start and Stop

During each rescheduling cycle, `max_churn` running jobs from the back of the
queue are stopped and `max_churn` jobs from the front of the pending queue are
started. This part is not modified from the existing scheduling algorithm,
except now, the jobs would be ordered by their `priority` value before being
ordered by their last start time.

In addition, `one-shot` replication jobs would still be skipped when stopping
and we'd let them run in order to maintain traditional replication semantics
just like before.

When picking the jobs to run exclude jobs which have been exponentially backed
off due to repeated errors. This part is unmodified and from the original
scheduler.

### Configuration

The decay coefficients and interval times for each of the 3 parts of the algorithm would be configurable in the `[replicator]` config section.

Per-`_replicator` db shares would be configurable in the `[replicator.shares]` section as:

```
[replicator.shares]
$prefix/_replicator = $numshares
```

By default each db is assigned 100 shares. Then higher number of shares should
then indicated a larger proportion of scheduler resources allocated to that db.
A lower number would get proportionally less shares.

For example:

```
[replicator.shares]

; This is the default
; _replicator = 100

high/_replicator = 200
low/_replicator = 50
```

# Advantages and Disadvantages

Advantages:

  * Allow a fair share of resources between multiple `_replicator` db instances

  * Can boost or lower the priority of some replication jobs by adjusting the
    shares assigned to that database instance.

Disadvantages:

  * Adds more complexity to the scheduler

# Key Changes

 * Modifies replication scheduler

## Applications and Modules affected

 * `couch_replicator` application

## HTTP API additions

N/A

## HTTP API deprecations

N/A

# Security Considerations

None

# References

* [1]: https://lists.apache.org/thread.html/rebba9a43bfdf9696f2ce974b0fc7550a631c7b835e4c14e51cd27a87%40%3Cdev.couchdb.apache.org%3E "couchdb-dev"

* [2]: https://proteusmaster.urcf.drexel.edu/urcfwiki/images/KayLauderFairShare.pdf "Fair Share Scheduler"

# Co-authors

 * Joan Touzet (@wohali)

# Acknowledgments

 * Joan Touzet (@wohali)
