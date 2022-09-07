---
name: Shard Splitting
about: Introduce Shard Splitting to CouchDB
title: 'Shard Splitting'
labels: rfc, discussion
assignees: '@nickva'

---

# Introduction

This RFC proposes adding the capability to split shards to CouchDB. The API and
the internals will also allow for other operations on shards in the future such
as merging or rebalancing.

## Abstract

Since CouchDB 2.0 clustered databases have had a fixed Q value defined at
creation. This often requires users to predict database usage ahead of time
which can be hard to do. A too low of a value might result in large shards,
slower performance, and needing more disk space to do compactions.

It would be nice to start with a low Q initially, for example Q=1, and as
usage grows to be able to split some shards that grow too big. Especially
with partitioned queries being available there will be a higher chance
of having uneven sized shards and so it would be beneficial to split the
larger ones to even out the size distribution across the cluster.

## Requirements Language

[NOTE]: # ( Do not alter the section below. Follow its instructions. )

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC 2119](https://www.rfc-editor.org/rfc/rfc2119.txt).

## Terminology

*resharding* : Manipulating CouchDB shards. Could be splitting, merging,
rebalancing or other operations. This will be used as the top-level API
endpoint name with the idea that in the future different types of shard
manipulation jobs would be added.

---

# Detailed Description

From the user's perspective there would be a new HTTP API endpoint -
`_reshard/*`. A POST request to `_reshard/jobs/` would start resharding jobs.
Initially these will be of just one "type":"split" but in the future other
types could be added.

Users would then be able to monitor the state of these jobs to inspect their
progress, see when they completed or failed.

The API should be designed to be consistent with `_scheduler/jobs` as much as
possible since that is another recent CouchDB's API exposing an internal jobs
list.

Most of the code implementing this would live in the mem3 application with some
lower level components in the *couch* application. There will be a new child in
the *mem3_sup* supervisor responsible for resharding called *mem3_reshard_sup*.
It will have a *mem3_reshard* manager process which should have an Erlang API
for starting jobs, stopping jobs, removing them, and inspecting their state.
Individual jobs would be instances of a gen_server defined in
*mem3_reshard_job* module. There will be simple-one-for-one supervisor under
*mem3_reshard_sup* named *mem3_reshard_job_sup* to keep track of
*mem3_reshard_job* children .

An individual shard splitting job will follow roughly these steps in order:

- **Create targets**. Targets are created. Some target properties should match
  the source. This means matching the PSE engine if source uses a custom one.
  If source is partitioned, targets should be partitioned as well, etc.

- **Initial bulk copy.** After the targets are created, copy all the document
  in the source shard to the targets. This operation should be as optimized as
  possible as it could potentially copy tens of GBs of data. For this reason
  this piece of code will be closer the what the compactor does.

- **Build indices**. The source shard might have had up-to-date indices and so
  it is beneficial for the split version to have them as well. Here we'd
  inspect all `_design` docs and rebuild all the known indices. After this step
  there will be a "topoff" step to replicate any change that might have
  occurred on the source while the indices were built.

- **Update shard map**. Here the global shard map is updated to remove the old
  source shard and replace it with the targets. There will be a corresponding
  entry added into the shard's document `changelog entry` indicating that a
  split happened. To avoid conflicts being generated when multiple copies of a
  range finish splitting and race to update the shard map. All shard map
  updates will be routes through one consistently picked node (lowest in the
  list connected nodes when they are sorted). After shard map is updated. There
  will be another topoff replication job to bring in changes from the source
  shard to the targets that might have occurred while the shard map was
  updating.

- **Delete source shard**

This progression of split states will be visible when inspecting a job's status
as well as in the history in the `detail` field of each event.


# Advantages and Disadvantages

Main advantage is to dynamically change shard size distribution on a cluster in
response to changing user requirements without having to delete and recreate
databases.

One disadvantage is that it might break some basic constraints about all copies
of a shard range being the same size. A user could choose to split for example
a shard copy 00..-ff... on node1 only so on node2 and node3 the copy will be
00-..ff.. but on node1 there will now be 00-..7f.. and 80-ff... External
tooling inspecting $db/_shards endpoint might need to be updated to handle this
scenario. A mitigating factor here is that resharding in the current proposal
is not automatic it is an operation triggered manually by the users.

# Key Changes

The main change is the ability to split shard via the `_reshard/*` HTTP API

## Applications and Modules affected

Most of the changes will be in the *mem3* application with some changes in the *couch* application as well.

## HTTP API additions

`* GET /_reshard`

Top level summary. Besides the new _reshard endpoint, there `reason` and the stats are more detailed.

Returns

```
{
    "completed": 3,
    "failed": 4,
    "running": 0,
    "state": "stopped",
    "state_reason": "Manual rebalancing",
    "stopped": 0,
    "total": 7
}
```

* `PUT /_reshard/state`

Start or stop global rebalacing.

Body
```
{
    "state": "stopped",
    "reason": "Manual rebalancing"
}
```

Returns

```
{
    "ok": true
}
```

* `GET /_reshard/state`

Return global resharding state and reason.

```
{
    "reason": "Manual rebalancing",
    "state": "stopped"
}
```

* `GET /_reshard/jobs`

Get the state of all the resharding jobs on the cluster. Now we have a detailed
state transition history which looks similar what _scheduler/jobs have.

```
{
    "jobs": [
        {
            "history": [
                {
                    "detail": null,
                    "timestamp": "2019-02-06T22:28:06Z",
                    "type": "new"
                },
                ...
                {
                    "detail": null,
                    "timestamp": "2019-02-06T22:28:10Z",
                    "type": "completed"
                }
            ],
            "id": "001-0a308ef9f7bd24bd4887d6e619682a6d3bb3d0fd94625866c5216ec1167b4e23",
            "job_state": "completed",
            "node": "node1@127.0.0.1",
            "source": "shards/00000000-ffffffff/db1.1549492084",
            "split_state": "completed",
            "start_time": "2019-02-06T22:28:06Z",
            "state_info": {},
            "target": [
                "shards/00000000-7fffffff/db1.1549492084",
                "shards/80000000-ffffffff/db1.1549492084"
            ],
            "type": "split",
            "update_time": "2019-02-06T22:28:10Z"
        },
        {
           ....
        },
   ],
   "offset": 0,
   "total_rows": 7
}
```

* `POST /_reshard/jobs`

Create a new resharding job. This can now take other parameters and can split multiple ranges.

To split one shard on a particular node

```
{
    "type": "split",
    "shard": "shards/80000000-bfffffff/db1.1549492084"
    "node": "node1@127.0.0.1"
}
```

To split a particular range on all nodes:

```
{
     "type": "split",
     "db" : "db1",
     "range" : "80000000-bfffffff"
}
```

To split a range on just one node:

```
{
     "type": "split",
     "db" : "db1",
     "range" : "80000000-bfffffff",
     "node": "node1@127.0.0.1"
}
```

To split all ranges of a db on one node:

```
{
     "type": "split",
     "db" : "db1",
     "node": "node1@127.0.0.1"
}
```

Result now may contain multiple job IDs

```
[
    {
        "id": "001-d457a4ea82877a26abbcbcc0e01c4b0070027e72b5bf0c4ff9c89eec2da9e790",
        "node": "node1@127.0.0.1",
        "ok": true,
        "shard": "shards/80000000-bfffffff/db1.1549986514"
    },
    {
        "id": "001-7c1d20d2f7ef89f6416448379696a2cc98420e3e7855fdb21537d394dbc9b35f",
        "node": "node1@127.0.0.1",
        "ok": true,
        "shard": "shards/c0000000-ffffffff/db1.1549986514"
    }
]
```

* `GET /_reshard/jobs/$jobid`

Get just one job by its ID

```
{
    "history": [
        {
            "detail": null,
            "timestamp": "2019-02-12T16:55:41Z",
            "type": "new"
        },
        {
            "detail": "Shard splitting disabled",
            "timestamp": "2019-02-12T16:55:41Z",
            "type": "stopped"
        }
    ],
    "id": "001-d457a4ea82877a26abbcbcc0e01c4b0070027e72b5bf0c4ff9c89eec2da9e790",
    "job_state": "stopped",
    "node": "node1@127.0.0.1",
    "source": "shards/80000000-bfffffff/db1.1549986514",
    "split_state": "new",
    "start_time": "1970-01-01T00:00:00Z",
    "state_info": {
        "reason": "Shard splitting disabled"
    },
    "target": [
        "shards/80000000-9fffffff/db1.1549986514",
        "shards/a0000000-bfffffff/db1.1549986514"
    ],
    "type": "split",
    "update_time": "2019-02-12T16:55:41Z"
}
```

* `GET /_reshard/jobs/$jobid/state`

Get the running state of a particular job only

```
{
    "reason": "Shard splitting disabled",
    "state": "stopped"
}
```

* `PUT /_reshard/jobs/$jobid/state`

Stop or resume a particular job

Request body

```
{
     "state": "stopped",
     "reason": "Pause this job for now"
}
```


## HTTP API deprecations

None

# Security Considerations

None.

# References

Original RFC-as-an-issue:

https://github.com/apache/couchdb/issues/1920

Most of the discussion regarding this has happened on the `@dev` mailing list:

https://mail-archives.apache.org/mod_mbox/couchdb-dev/201901.mbox/%3CCAJd%3D5Hbs%2BNwrt0%3Dz%2BGN68JPU5yHUea0xGRFtyow79TmjGN-_Sg%40mail.gmail.com%3E

https://mail-archives.apache.org/mod_mbox/couchdb-dev/201902.mbox/%3CCAJd%3D5HaX12-fk2Lo8OgddQryZaj5KRa1GLN3P9LdYBQ5MT0Xew%40mail.gmail.com%3E


# Acknowledgments

@davisp @kocolosk : Collaborated on the initial idea and design

@mikerhodes @wohali @janl @iilyak : Additionally collaborated on API design
