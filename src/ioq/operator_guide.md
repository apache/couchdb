# An operator's guide to IOQ

IOQ handles the prioritisation of IO operations in the database. It has
two main responsibilities:

 1. Providing configurable prioritisation of interactive requests and
    background requests such as compaction and internal replication.
 2. Providing equal prioritisation for interactive requests by backend/database.

From an operational perspective point 1 is of most interest as it provides a set
of levers that can be pulled to change the behaviour of the cluster in favour
of particular workloads or operational concerns.

## Basic overview

From an operational point-of-view, IOQ carries out two fundamental operations:

 1. Enqueueing requests into one of a number of available channels.
 2. Selecting and submitting a request from the available channels according
    to configured priorities.

IOQ categorises IO requests by class and by priority. The class of a request
dictates the channel into which it will be enqueued and the priority influences
the probability that a given request will be dequeued and executed.

The following table lists the IOQ classes and the corresponding priorities. Note
that the mapping of IOQ classes to class priorities is not 1:1.

```
|---------------+---------------+--------------------------------------------|
| IOQ class     | IOQ priority  | Description                                |
|---------------+---------------+--------------------------------------------|
| interactive   | reads, writes | IO requests related to requests made by    |
|               |               | users via the http layer.                  |
|               |               |                                            |
| db_update     | writes        | Interactive IO requests which are database |
|               |               | write operations.                          |
|               |               |                                            |
| view_update   | views         | IO requests related to view index builds.  |
|               |               |                                            |
| db_compact    | compaction    | IO requests related to database            |
|               |               | compactions.                               |
|               |               |                                            |
| view_compact  | compaction    | IO requests related to view compactions.   |
|               |               |                                            |
| internal_repl | replication   | IO requests related to internal            |
|               |               | replication.                               |
|               |               |                                            |
| low           | low           | IO requests related to requests made by    |
|               |               | users via the http layer where the         |
|               |               | "x-cloudant-priority: low" header is set.  |
|               |               |                                            |
| other         | undefined     | IO requests that do not fit any of the     |
|               |               | above classes. This includes search IO     |
|               |               | requests.                                  |
|---------------+---------------+--------------------------------------------|
```

## Internals

To understand the relationship between the IOQ classes and the IOQ priorities
it is helpful to understand the channels into which IO requests are enqueued.

IOQ uses the following four channels:

 - `Compaction`
 - `Internal replication`
 - `Low`
 - `Customer`

The `Customer` channel is effectively a meta-channel where each item in the
queue represents a backend/dbname combination that consists of a further three
channels:

 - `Interactive`
 - `DB update`
 - `View update`

Requests are enqueued according to the following scheme:

 - Requests with class `internal_repl`, `low`, `db_compact` or `view_compact`
   are enqueued into `Internal replication`, `Low` or `Compaction` channels
   respectively.
 - Requests with class `interactive`, `db_update` or `view_update` are enqueued
   into the `Interactive`, `DB update` or `View update` channel of the relevant
   `Customer` channel for the backend/database combination.
 - Requests with class `other` are enqueued into the `Interactive` queue of a
   `Customer` channel reserved for `other` IOQ requests.

Requests are submitted as follows:

 - The next item is selected from either the `Compaction`,
   `Internal replication`, `Low` or `Customer` channel according to the
   configured priorities (`compaction`, `replication`, `low` and `customer`).
 - If the item is obtained from the `Compaction`, `Internal replication` or
   `Low` channels then the request is submitted for execution.
 - If the item is obtained from the `Customer` channel then the request is
   selected from either the `Interactive`, `DB update` or `View update` channel
   according to the configured priorities (`reads`, `writes`, and `views`).

## Configuration

Unless there is prior knowledge of the IOQ configuration required to support the
intended workload of a cluster on a given hardware specification it is
recommended that IOQ is initially left with the default configuration values. As
more becomes known about the behaviour of a cluster under load the IOQ settings
can be tuned to provide optimal performance for the production workload.

Note that tuning IOQ is not the answer to all performance problems and there are
a finite number of gains to be had (possibly zero). You should also be
considering the total load on the cluster, the capabilities of the underlying
hardware and the usage patterns and design of the applications which sit on top
of the data layer.

### Priorities

IOQ ships with a default configuration which gives interactive reads/writes and
view builds a high priority (`1.0`) and the background requests a much lower
priority (`0.001` for compaction and `0.0001` for replication and low).

You can set the priorities to other values using the config app in a remsh as
follows:

    config:set("ioq", "views", "0.5", "FBXXXXX reduce views IOQ priority").

To return to the default value just delete the configuration value:

    config:delete("ioq", "views", "FBXXXXX revert to default priority").

The following sections describe typical situations where tuning IOQ priorities
might be appropriate.

#### Internal replication backlog

If cluster nodes are frequently exhibiting an internal replication backlog
then it might be worth increasing the `replication` priority.

A backlog can be confirmed by checking the following graphite target:

    net.cloudant.cluster001.db*.erlang.internal_replication_jobs

If this value is consistently elevated by more than a few hundred changes then
try increasing the `replication` IOQ priority:

    config:set("ioq", "replication", "0.5", "FBXXXXX speed up internal replication").

If this has been effective you should notice a change in the rate at which the
metric decreases. It is worth experimenting with values as high as `1.0` however
you will need to keep an eye on HTTP request latencies to make sure there is no
adverse impact on other aspects of cluster performance.

#### Compactions not completing quickly enough

If disk usage is rising on cluster nodes and there is a corresponding backlog
in compaction work then it might be worth increasing the `compaction` priority.

Check the volume of pending changes for ongoing compaction jobs in graphite:

    net.cloudant.cluster001.db1.dbcore.active_tasks.changes_pending.*compaction

Increase the priority for `compaction`:

    config:set("ioq", "compaction", "0.5", "FBXXXXX speed up compaction").

Now monitor the changes_pending metrics to see if the rate at which changes are
being processed has increased.

The notes in previous section apply here - experiment with values as high as
"1.0" if necessary and keep a close eye on cluster performance whilst you
do so.

#### Interactive requests and views competing for IO resource

Metrics might show that read/write performance worsens when views are building
or conversely that view build performance slows when read/write load increases.
If the performance requirements of the cluster are such that a particular
type of request is more critical to the application it supports then it might be
worth reducing the other IOQ priorities, for example:

    config:set("ioq", "views", "0.1", "FBXXXXX attempt to improve read/write performance").

### Concurrency

The concurrency defines the total number of concurrent IO operations allowed by
IOQ. The default value is `20` however it can be worth increasing if the
answer to the following questions is yes:

 1. Either `net.cloudant.cluster001.db1.erlang.io_queue.active_requests` or
    `net.cloudant.cluster001.db1.couchdb.io_queue.latency` is consistently
    elevated.

 2. Disk utilisation is significantly less than 100%.

If performance is being impacted by request waiting in the queues then it is
worth bumping IOQ concurrency (sensible values to try are `30`, `50` and `100`)
and observing the resulting effect.

Note that increasing this value beyond a certain point can result in the disks
being overloaded and overall performance degradation. The exact point depends
on the cluster workload and hardware so it is very important to monitor the
cluster when making changes here.

### Bypasses

In extreme cases it is possible that IOQ itself is the bottleneck for certain
request classes. If this is case then you can bypass IOQ for that request
class altogether, e.g. for interactive requests:

    config:set("ioq.bypass", "interactive", "true", "FBXXXXX attempt to improve interactive performance").

Note that bypasses are set for IOQ *classes* not IOQ priorities. This means if
you wanted to bypass all compaction requests you would need to set a bypass for
`db_compact` and `view_compact`.

The following warnings should be heeded when considering setting an IOQ bypass:

 - Other request classes will continue to be routed through IOQ so will not
   be able to compete with the bypassed requests. You should therefore monitor
   the cluster carefully to determine that overall performance is acceptable.
   Keep a close eye on compaction in particular (unless it is being bypassed)
   as if the rate of compaction slows too much the disk may start filling up.

 - The bypass effectively shifts the bottleneck to another part of the system
   which is typically evident in `couch_file` and `couch_db_updater` message
   queue backups.

 - Disk performance may also become saturated which could lead to various
   resulting performance degradations.

A good rule of thumb is to avoid IOQ bypasses altogether unless the customer
is in immediate pain.
