# An operator's guide to smoosh

Smoosh is the auto-compactor for the databases. It automatically selects and
processes the compacting of database shards on each node.

## Smoosh Channels

Smoosh works using the concept of channels. A channel is essentially a queue of pending
compactions. There are separate sets of channels for database and view compactions. Each
channel is assigned a configuration which defines whether a compaction ends up in
the channel's queue and how compactions are prioritised within that queue.

Smoosh takes each channel and works through the compactions queued in each in priority
order. Each channel is processed concurrently, so the priority levels only matter within
a given channel.

Finally, each channel has an assigned number of active compactions, which defines how
many compactions happen for that channel in parallel. For example, a cluster with
a lot of database churn but few views might require more active compactions to the
database channel(s).

It's important to remember that a channel is local to a dbcore node, that is
each node maintains and processes an independent set of compactions.

### Channel configuration options

#### Channel types

Each channel has a basic type for the algorithm it uses to select pending
compactions for its queue and how it prioritises them.

The two queue types are:

* **ratio**: this uses the ratio `total_bytes / user_bytes` as its driving
calculation. The result _X_ must be greater than some configurable value _Y_ for a
compaction to be added to the queue. Compactions are then prioritised for
higher values of _X_.

* **slack**: this uses `total_bytes - user_bytes` as its driving calculation.
The result _X_ must be greater than some configurable value _Y_ for a compaction
to be added to the queue. Compactions are prioritised for higher values of _X_.

In both cases, _Y_ is set using the `min_priority` configuration variable. The
calculation of _X_ is described in [Priority calculation](#priority-calculation), below.

Both algorithms operate on two main measures:

* **user_bytes**: this is the amount of data the user has in the file. It
doesn't include storage overhead: old revisions, on-disk btree structure and
so on.

* **total_bytes**: the size of the file on disk.

Channel type is set using the `priority` configuration setting.

#### Further configuration options

Beyond its basic type, there are several other configuration options which
can be applied to a queue.

*All options MUST be set as strings.* See the [smoosh readme][srconfig] for
all settings and their defaults.

#### Priority calculation

The algorithm type and certain configuration options feed into the priority
calculation.

The priority is calculated when a compaction is enqueued. As each channel
has a different configuration, each channel will end up with a different
priority value. The enqueue code checks each channel in turn to see whether the
compaction passes its configured priority threshold (`min_priority`). Once
a channel is found that can accept the compaction, the compaction is added
to that channel's queue and the enqueue process stops. Therefore the
ordering of channels has a bearing in what channel a compaction ends up in.

If you want to follow this along, the call order is all in `smoosh_server`,
`enqueue_request -> find_channel -> get_priority`.

The priority calculation is probably the easiest way to understand the effects
of configuration variables. It's defined in `smoosh_server#get_priority/3`,
currently [here][ss].

[ss]: https://github.com/apache/couchdb-smoosh/blob/master/src/smoosh_server.erl#L277
[srconfig]: https://github.com/apache/couchdb-smoosh#channel-settings

#### Background Detail

`user_bytes` is called `data_size` in `db_info` blocks. It is the total of all bytes
that are used to store docs and their attachments.

Since `.couch` files are append only, every update adds data to the file. When
you update a btree, a new leaf node is written and all the nodes back up the
root. In this update, old data is never overwritten and these parts of the
file are no longer live; this includes old btree nodes and document bodies.
Compaction takes this file and writes a new file that only contains live data.

`total_data` is the number of bytes in the file as reported by `ls -al filename`.

#### Flaws

An important flaw in this calculation is that `total_data` takes into account
the compression of data on disk, whereas `user_bytes` does not. This can give
unexpected results to calculations, as the values are not directly comparable.

However, it's the best measure we currently have.

[Even more info](https://github.com/apache/couchdb-smoosh#notes-on-the-data_size-value).


### Defining a channel

Defining a channel is done via normal dbcore configuration, with some
convention as to the parameter names.

Channel configuration is defined using `smoosh.channel_name` top level config
options. Defining a channel is just setting the various options you want
for the channel, then bringing it into smoosh's sets of active channels by
adding it to either `db_channels` or `view_channels`.

This means that smoosh channels can be defined either for a single node or
globally across a cluster, by setting the configuration either globally or
locally. In the example, we set up a new global channel.

It's important to choose good channel names. There are some conventional ones:

* `ratio_dbs`: a ratio channel for dbs, usually using the default settings.
* `slack_dbs`: a slack channel for dbs, usually using the default settings.
* `ratio_views`: a ratio channel for views, usually using the default settings.
* `slack_views`: a slack channel for views, usually using the default settings.

These four are defined by default if there are no others set ([source][source1]).

[source1]: https://github.com/apache/couchdb-smoosh/blob/master/src/smoosh_server.erl#L75

And some standard names for ones we often have to add:

* `big_dbs`: a ratio channel for only enqueuing large database shards. What
  _large_ means is very workload specific.

Channels have certain defaults for their configuration, defined in the
[smoosh readme][srconfig]. It's only neccessary to set up how this channel
differs from those defaults. Below, we just need to set the `min_size` and
`concurrency` settings, and allow the `priority` to default to `ratio`
along with the other defaults.

```bash
# Define the new channel
(couchdb@db1.foo.bar)3> rpc:multicall(config, set, ["smoosh.big_dbs", "min_size", "20000000000"]).
{[ok,ok,ok],[]}
(couchdb@db1.foo.bar)3> rpc:multicall(config, set, ["smoosh.big_dbs", "concurrency", "2"]).
{[ok,ok,ok],[]}

# Add the channel to the db_channels set -- note we need to get the original
# value first so we can add the new one to the existing list!
(couchdb@db1.foo.bar)5> rpc:multicall(config, get, ["smoosh", "db_channels"]).
{["ratio_dbs","ratio_dbs","ratio_dbs"],[]}
(couchdb@db1.foo.bar)6> rpc:multicall(config, set, ["smoosh", "db_channels", "ratio_dbs,big_dbs"]).
{[ok,ok,ok],[]}
```

### Viewing active channels

```bash
(couchdb@db3.foo.bar)3> rpc:multicall(config, get, ["smoosh", "db_channels"]).
{["ratio_dbs,big_dbs","ratio_dbs,big_dbs","ratio_dbs,big_dbs"],[]}
(couchdb@db3.foo.bar)4> rpc:multicall(config, get, ["smoosh", "view_channels"]).
{["ratio_views","ratio_views","ratio_views"],[]}
```

### Removing a channel

```bash
# Remove it from the active set
(couchdb@db1.foo.bar)5> rpc:multicall(config, get, ["smoosh", "db_channels"]).
{["ratio_dbs,big_dbs", "ratio_dbs,big_dbs", "ratio_dbs,big_dbs"],[]}
(couchdb@db1.foo.bar)6> rpc:multicall(config, set, ["smoosh", "db_channels", "ratio_dbs"]).
{[ok,ok,ok],[]}

# Delete the config -- you need to do each value
(couchdb@db1.foo.bar)3> rpc:multicall(config, delete, ["smoosh.big_dbs", "concurrency"]).
{[ok,ok,ok],[]}
(couchdb@db1.foo.bar)3> rpc:multicall(config, delete, ["smoosh.big_dbs", "min_size"]).
{[ok,ok,ok],[]}
```

### Getting channel configuration

As far as I know, you have to get each setting separately:

```
(couchdb@db1.foo.bar)1> rpc:multicall(config, get, ["smoosh.big_dbs", "concurrency"]).
{["2","2","2"],[]}

```

### Setting channel configuration

The same as defining a channel, you just need to set the new value:

```
(couchdb@db1.foo.bar)2> rpc:multicall(config, set, ["smoosh.ratio_dbs", "concurrency", "1"]).
{[ok,ok,ok],[]}
```

It sometimes takes a little while to take affect.



## Standard operating procedures

There are a few standard things that operators often have to do when responding
to pages.

In addition to the below, in some circumstances it's useful to define new
channels with certain properties (`big_dbs` is a common one) if smoosh isn't
selecting and prioritising compactions that well.

### Checking smoosh's status

You can see the queued items for each channel by going into `remsh` on a node
and using:

```
> smoosh:status().
{ok,[{"ratio_dbs",
      [{active,1},
       {starting,0},
       {waiting,[{size,522},
                 {min,{5.001569007970237,{1378,394651,323864}}},
                 {max,{981756.5441159063,{1380,370286,655752}}}]}]},
     {"slack_views",
      [{active,1},
       {starting,0},
       {waiting,[{size,819},
                 {min,{16839814,{1375,978920,326458}}},
                 {max,{1541336279,{1380,370205,709896}}}]}]},
     {"slack_dbs",
      [{active,1},
       {starting,0},
       {waiting,[{size,286},
                 {min,{19004944,{1380,295245,887295}}},
                 {max,{48770817098,{1380,370185,876596}}}]}]},
     {"ratio_views",
      [{active,1},
       {starting,0},
       {waiting,[{size,639},
                 {min,{5.0126340031149335,{1380,186581,445489}}},
                 {max,{10275.555632057285,{1380,370411,421477}}}]}]}]}
```

This gives you the node-local status for each queue.

Under each channel there is some information about the channel:

* `active`: number of current compactions in the channel.
* `starting`: number of compactions starting-up.
* `waiting`: number of queued compactions.
  * `min` and `max` give an idea of the queued jobs' effectiveness. The values
    for these are obviously dependent on whether the queue is ratio or slack.

For ratio queues, the default minimum for smoosh to enqueue a compaction is 5. In
the example above, we can guess that 981,756 is quite high. This could be a
small database, however, so it doesn't necessarily mean useful compactions
from the point of view of reclaiming disk space.

For this example, we can see that there are quite a lot of queued compactions,
but we don't know which would be most effective to run to reclaim disk space.
It's also worth noting that the waiting queue sizes are only meaningful
related to other factors on the cluster (e.g., db number and size).


### Smoosh IOQ priority

This is a global setting which affects all channels. Increasing it allows each
active compaction to (hopefully) proceed faster as the compaction work is of
a higher priority relative to other jobs. Decreasing it (hopefully) has the
converse effect.

By this point you'll [know whether smoosh is backing up](#checking-smooshs-status).
If it's falling behind (big queues), try increasing compaction priority.

Smoosh's IOQ priority is controlled via the `ioq` -> `compaction` queue.

```
> rpc:multicall(config, get, ["ioq", "compaction"]).
{[undefined,undefined,undefined],[]}

```

Priority by convention runs 0 to 1, though the priority can be any positive
number. The default for compaction is 0.01; pretty low.

If it looks like smoosh has a bunch of work that it's not getting
through, priority can be increased. However, be careful that this
doesn't adversely impact the customer experience. If it will, and
it's urgent, at least drop them a warning.

```
> rpc:multicall(config, set, ["ioq", "compaction", "0.5"]).
{[ok,ok,ok],[]}
```

In general, this should be a temporary measure. For some clusters,
a change from the default may be required to help smoosh keep up
with particular workloads.

### Granting specific channels more workers

Giving smoosh a higher concurrency for a given channel can allow a backlog
in that channel to catch up.

Again, some clusters run best with specific channels having more workers.

From [assessing disk space](#assess-the-space-on-the-disk), you should
know whether the biggest offenders are db or view files. From this,
you can infer whether it's worth giving a specific smoosh channel a
higher concurrency.

The current setting can be seen for a channel like so:

```
> rpc:multicall(config, get, ["smoosh.ratio_dbs", "concurrency"]).
{["2","2","2"], []}
```

`undefined` means the default is used.

If we knew that disk space for DBs was the major user of disk space, we might
want to increase a `_dbs` channel. Experience shows `ratio_dbs` is often best
but evaluate this based on the current status.

If we want to increase the ratio_dbs setting:

```
> rpc:multicall(config, set, ["smoosh.ratio_dbs", "concurrency", "2"]).
{[ok,ok,ok],[]}
```

### Suspending smoosh

If smoosh itself is causing issues, it's possible to suspend its operation.
This differs from either `application:stop(smoosh).` or setting all channel's
concurrency to zero because it both pauses on going compactions and maintains
the channel queues intact.

If, for example, a node's compactions are causing disk space issues, smoosh
could be suspended while working out which channel is causing the problem. For
example, a big_dbs channel might be creating huge compaction-in-progress
files if there's not much in the shard to compact away.

It's therefore useful to use when testing to see if smoosh is causing a
problem.

```
# suspend
smoosh:suspend().

# resume a suspended smoosh
smoosh:resume().
```

Suspend is currently pretty literal: `erlang:suspend_process(Pid, [unless_suspending])`
is called for each compaction process in each channel. `resume_process` is called
for resume.

### Restarting Smoosh

Restarting Smoosh is a long shot and is a brute force approach in the hope that
when Smoosh rescans the DBs that it makes the right decisions. If required to take
this step contact rnewson or davisp so that they can inspect Smoosh and see the bug.

```
> exit(whereis(smoosh_server), kill), smoosh:enqueue_all_dbs(), smoosh:enqueue_all_views().
```
