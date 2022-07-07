# IOQ2 Overview

IOQ2 is a replacement for the original IOQ, with the core motivation to create
a faster IOQ that eliminates the need for IOQ bypasses. This is achieved with
two primary approaches:

  1. Faster data structures
  2. More processes

IOQ2 also provides a more capable configuration system allowing
(de)prioritization at the class, user, and shard/class levels. This means you
can do things like bumping up global compaction priority, deprioritizing a
problematic MT user, and bumping up view build priority on an individual shard.


## Faster Data Structures

One of the issues with IOQ1 is that it uses standard Erlang list and queue data
structures for doing high volume modifications to the request queues and
tracking of active requests. This quickly becomes a bottleneck, and results in
IOQ1 being roughly an order of magnitude slower than bypassing it, in high
throughput scenarios.

We work around this issue in IOQ2 by introducing the `hqueue` data structure,
which is an Erlang NIF wrapper around a simple mutable priority queue that only
does in place updates. This queue is minimal in functionality and only supports
floating point values as the priority sorting mechanism. The result is that
IOQ2 focuses on a multiplicative prioritization scheme using chained
multipliers allowing for different weights to classes, users, and shard/class
pairs. This prioritization scheme can easily be extended later on to include
additional attributes, such as CCM tier, or perhaps a feedback loop providing
prioritization on overall request volumes/cost per user.


## More Processes

The introduction of `hqueue` has a considerable impact on IOQ and more than
doubles the throughput of IOQ1, however, as mentioned above, IOQ1 bypasses can result in
an order of magnitude performance difference, so clearly faster data structures
are not sufficient to solve the problem. The main issue is that an Erlang
process can only go as fast a single CPU core will allow, so as we continually
add more CPU cores to production systems, single process IOQ becomes even more
of a problem.

Having all requests funnel through a single Erlang process will
inevitably become a bottleneck. That said, the entire point of IOQ is that it
*IS* a bottleneck! If there's no queued requests, then there's nothing to
prioritize, making the use of IOQ questionable at best. The balancing act here
is getting as close to IOQ bypass performance as we can while inducing enough
of a bottleneck to effectively be able to prioritize different types of work.

IOQ2 uses a set of IOQ2 processes to achieve similar levels of performance as
an IOQ bypass. It creates a set of named `ioq_server_$N` processes for each
Erlang scheduler in the VM. The caller requests are then dispatched to the
appropriate IOQ2 server based on the current scheduler of the caller. Overall
this works quite well and seems to be an effective way of ensuring sufficient
request volume to have a queue backlog to prioritize, while also spreading out
load as the Erlang spreads load out across more schedulers, as needed. There is
an experimental option to bind the IOQ2 pids to the relevant schedulers, but so
far this has not shown conclusive improvements during limited testing. Another
potential approach here is to randomize requests to the different IOQ2 pids.
More fine grained testing with a sharp eye on latency distributions would be
useful here.


# The (h)queue

The new queue in IOQ2 is the `hqueue` NIF, which is a mutable heap based
max priority queue that prioritizes on a single floating point value. What this
means in practice is that every request gets a numeric floating point priority
value, and is then thrown into the queue until is is popped as the max value.
The result is a priority queue data structure that inserts new items in
O(log(N)) and also extracts the maximum item in O(log(N)), resulting in a very
performant data structure for the application at hand.


## Sample Prioritizations

The current prioritization scheme is a simple set of multipliers for the
various dimensions. Currently there are three dimensions:

  * Class: eg `interactive`, `db_compact`, `view_update`, etc
  * User: the user making the request, eg `<<"foo">>`
  * Shard/Class pair: the shard and class for the request
    - eg `{<<"shards/00000000-1fffffff/foo">>, interactive}`
    - this allows for things like increased compaction priority on an
      individual shard outside of the global class multipliers

Behind the scenes, this basically works as follows:

```erlang
prioritize_request(Req) ->
  UserPriority = user_priority(Req),
  ClassPriority = class_priority(Req),
  ShardClassPriority = shard_class_priority(Req),

  UserPriority * ClassPriority * ShardClassPriority.
```

With the default priority being the identity priority, 1.0, so in the case
where no values are defined the multiplier above would be 1.0 * 1.0 * 1.0. The
default class priorities are currently defined as follows in `ioq.hrl`:


### Default Class Priorities

```erlang
-define(DEFAULT_CLASS_PRIORITIES, [
    {customer, 1.0},
    {internal_repl, 0.001},
    {view_compact, 0.0001},
    {db_compact, 0.0001},
    {low, 0.0001},
    {db_meta, 1.0},

    {db_update, 1.0},
    {view_update, 1.0},
    {other, 1.0},
    {interactive, 1.0}
]).
```


## Handling Priority Starvation

One potential problem with the simple floating point based priority queue is
that lower priority items can become starved given a constant volume of higher
priority requests. We want to ensure that requests can't get permanently
starved in this manner, and that work progresses on all fronts in a timely
fashion. IOQ1 handles this issue by ensuring there's always a random chance low
priority work will be executed.

In IOQ2 this issue is handled by way of an auto scaling elevator on the
priority values. What this means is that every `N` requests, IOQ2 will scale
the existing queued items by a configurable scaling factor. The idea is that
you automatically increase the priority of all queued items, and if you do that
enough times then lower priority items will eventually bubble up to the top.
Behind the scenes hqueue is an array based heap so we can easily run through
the array and update the priority of each item. By scaling the priority of each
item linearly, we preserve the loop invariant sorted order of the elements in
the heap and can accomplish this without needing to resort the heap.

The default scale factor is currently `2.0`, and the default `N` value for how
often to scale is currently every `1000` requests. Both of these values are
config knobs. In general these values seem _ok_, but they're not particularly
scientific, so we'll want to keep an eye on them over time in a variety of
workloads.


## Intentional Priority Starvation

Initially, IOQ2 and hqueue required all priorities to be greater than zero, but
this has been switched to be greater than or equal to zero. The motivation here
is that 0.0 has the cute property of propagating through any multipliers. This
means a zero value for any of the dimensions will make the other dimensions
irrelevant. But what's even more interesting is that zero priority values skip
the auto scaling elevator and will forever be stuck at zero, which provides a
way to intentionally starve particular work types, or at the very least to
ensure that it will never be selected unless there is no other work to do. This
is especially useful for black balling problematic MT users, or marking a
particular database as background only work.


---


# IOQ2 Operations Guide

IOQ2 comes with a feature toggle, and is disabled by default. You can enable it
with:


## Enable IOQ2

```erlang
ioq_config:set_enabled(true, "Enabling IOQ2").
```

You can verify it's enabled by checking:

```erlang
ioq:ioq2_enabled().
```


## Metrics Dashboard

IOQ2 has a dedicated dashboard page on `metrics.cloudant.com` with the
expected tab name of `IOQ2`.


---


## Choosing Good Priority Values

Choosing good priority values is going to be a crucial part of tuning IOQ2.
Unfortunately this is not particularly obvious nor necessarily easy, and it will
take some experimentation under different workloads to begin establishing some
best practices. Hopefully folks can start updating this section with some
useful tips and tricks for different workloads. Below there's more
documentation on how to validate the priority configs for different request
types. The primary motivation for adding that logic was to help facilitate
experimentation of different configuration options, and to aid in understanding
how the different configurations impact request prioritization.

It will be useful to keep in mind the ranges of priority values. By default all
interactive/view_update/db_update/other class requests have a priority of
`1.0`, and assuming no user specific or shard specific configs, those requests
will have a priority of `1.0`. Similarly, standard background tasks like
compaction and internal replication have a default priority of `0.0001`. So
primary database operations by default have a thousand fold prioritization over
background tasks. The default bounds of prioritization are from `0.0` to
`10000.0`, so you have a decent field to experiment with, and the upper bound
can be configured higher or lower as desired.

It's also important to remember the auto scaling elevator logic for prioritized
requests. Every `N` requests all currently queued requests have their
priorities linearly scaled, so after sufficiently long time in the queue, all
requests (with non `0.0` priorities) will eventually become the top priority
(assuming constant priorities coming in). The scaling factor can be configured
as well as how often to do the scaling.

So let's look at some real world scenarios where you would want to change
prioritization. Let's start with a simple one, what to do when a node is at 95%
disk space? This is an easy one! just blast compaction priority. Unlike IOQ1,
IOQ2 does not differeniate between request types in terms of selecting next
work, it's strictly based on the priority multiplier, so you can completely
prioritize compaction traffic over standard database operations, potentially to
the detriment of standard operations performance. So if you set the compaction
priority multiplier to 10000 you'll prioritize compaction work above everything
else (assuming default priorities elsewhere). This means that as long as there
is compaction jobs in the queue those will be handled before anything else.
This should be a significant win for prioritizing compaction in low disk space
scenarios.

Now, let's look at a similar, albeit more complicated scenario. Disk space is
above 85%, and you want to get out ahead of the compaction curve without
severely impacting cluster performance for standard operations. Cranking
compaction to the limit will get the job done, but it will also potentially
induce performance issues for the customer and could starve normal requests.
Here you would want to experiment a bit with gradually bumping up the
compaction priority. Try going from the `0.0001` default to `0.001` and see how
much that increases compaction throughput. Still not enough? try `0.01` and
repeat. Then on to `0.1` and maybe even on to `1.0` to make compaction priority
level with normal operations.

One other thing to keep in mind here is that these prioritizations are also
dependent on the overall request volumes. If you've got 10k pending db_update
requests, and only 5 pending compaction requests, then cranking compaction
priority is not going to have a massive negative impact on db_update
throughput. Similarly, if you've only got one compaction job running, you'll
run into diminishing returns for how effectively you can prioritize compaction
as there's insufficient request volume to prioritize. You'll need to experiment
with increasing Smoosh concurrency to get more jobs running to have more queued
items to prioritize.


## Setting Priority Values

IOQ2 contains utility functions for setting all configuration related values;
you should *not* need to use `config:set` for any IOQ2 related configuration
changes. In addition to the various config tunables, there are builtin helpers
to assist with setting appropriate priorities. All priority values should be
floating point values, and the described config helpers will prevent you from
using non floating point values. All of the config helpers here expect a
`Reason` value to log changes as per standard auditing rules.

*NOTE* the shard priorities do not include the shard suffix in the config names
to preserve priorities between cycled dbs, so if for whatever reason you
manually set shard priorities, make sure you use `filename:rootname(ShardName)`
to drop the suffix so that your config options work as expected.


### Setting Class Priority Values

Class specific priority multipliers can be set as demonstrated below. The class
name should be an Erlang atom, and the value should be a float. For example:

```erlang
ioq_config:set_class_config(interactive, 3.2, "Insightful motivation").
ioq_config:set_class_config(db_compact, 0.5, "Moar compactions").
```


### Setting Shard/Class Specific Priority Values

You can prioritize on Shard/Class pairs, there is no shard wide prioritization
so you'll need to set each class as appropriate. This function takes a
`#shard{}` record as returned by `mem3:shards`.

```erlang
Shard = hd(mem3:shards(DbName)),
ioq_config:set_shard_config(Shard, db_update, 2.3, "Prioritize db updates").
```

You _could_ call `set_shard_config` for every shard for a given database, but
there's a helper for that as well:


### Setting Shard/Class Priority for all Shards in a Database

There's a helper function for setting a class priority on all shards for a
given database name. Similarly to the shard/class configs, you have to specify
each class priority individually. You can use it as follows:

```erlang
ioq_config:set_db_config(<<"foo/bar">>, view_update, 0.8, "Build that view").
```

This is roughly equivalent to:

```erlang
[set_shard_config(S, Class, Value, Reason) || S <- mem3:shards(DbName)].
```


### Setting User Specific Priority Values

You can set a global multiplier for a particular user to increase or decrease
the priority of all of their requests. Here's an example:

```erlang
ioq_config:set_user_config(<<"foo">>, 3.7, "Priority user").
```

The `set_user_config` currently does not validate that the user exists, so
you'll want to validate you set the config for the appropriate user.


### Verifying Expected Prioritization Matches up with Reality

Ok great, so you've just used the handy helpers to set varius priority values,
but how do you verify it did what you expect? How do you test to see that the
multipliers result in a prioritization in the desired range? The prioritization
logic is self contained and can easily be experimented with. You have two
options, either with the `check_priority/3` function, or by using the
`prioritize` function directly. The `check_priority` function is the simple
approach, and precludes you from having to build up the relevant priority data
structures. It can be used as follows:

```erlang
User = <<"foo">>,
DbName = <<"foo/bar">>,
Shard = hd(mem3:shards(DbName)),
ioq_config:check_priority(internal_repl, User, Shard).
```

That will return the floating point prioritization value for that request. You
can also experiment with your own config options directly by way of using the
`ioq_config:prioritize` function. To demonstrate an example of using this,
here is the source code for the `check_priority` function used above:

```erlang
-spec check_priority(atom(), binary(), binary()) -> float().
check_priority(Class, User, Shard0) ->
    {ok, ClassP} = build_class_priorities(),
    {ok, UserP} = build_user_priorities(),
    {ok, ShardP} = build_shard_priorities(),

    Shard = filename:rootname(Shard0),
    Req = #ioq_request{
        user = User,
        shard = Shard,
        class = Class
    },

    prioritize(Req, ClassP, UserP, ShardP).
```

The `build_*_priorities()` functions are all exported from the `ioq_config`
module and are directly usable for easy testing. You can also see the full list
of priority values from those priority data structures like so:

```erlang
(node1@127.0.0.1)14> khash:to_list(ShardP).
[{{<<"shards/00000000-1fffffff/foo">>,interactive},1.0e3},
 {{<<"shards/00000000-1fffffff/foo/pizza_db">>,db_update},
   1.5}]
```


---


## Other Configuration Knobs

There are a number of other configuration knobs availabe and they're detailed
below.


### IOQ2 Concurrency

We'll start out with one of the more awkward configuration tunables:
concurrency! This option is awkward because it fundamentally changes the
dynamics of IOQ, both IOQ1 and IOQ2. If concurrency is higher than the number
of parallel requests, then you'll never actually prioritize things and using
IOQ is a waste. If it's too low then you'll overly bottleneck the system and
cause a backup of requests.

This awkwardness is further compounded by the fact that IOQ2 is inherently
concurrent in that it has one IOQ2 pid per scheduler, so you must exercise
caution with setting the concurrency value! This value is propagated to every
IOQ2 pid, so setting concurrency is essentially multiplicative, and total
concurrency will be `concurrency * num_schedulers`. Most of our newer systems
now have 48 cores, and we have have systems with 56 cores now, so setting IOQ2
concurrency to 5 could result in a total concurrency of 250+!!! The
`ioq:get_disk_concurrency()` function (which calls
`ioq_server2:get_concurrency()` when IOQ2 is enabled) will aggregate these
concurrency values together, giving you the full total, so that's useful to
double check.

Interestingly enough, the best concurrency value found so far through emperical
means is concurrency=1 per IOQ2 pid! This is the current default, and so on
machines with 48 cores we end up with a total concurrency of 49. So far the
default of one has been fairly effective, but given sufficient volume of
requests it might be worthwhile to bump it up. Start small and trying bumping
it up to two, or maybe three. For example:

```erlang
ioq_config:set_concurrency(2, "Bumping concurrency").
```

*NOTE* if you do feel the need to update concurrency, please do notify
@chewbranca afterwards so we can observe this in more workloads.


### IOQ2 Resize Limit

The resize limit value controls how many requests to handle before triggering
the auto scaling elevator logic described above. This defaults to 1000, and can
be changed with:

```erlang
ioq_config:set_resize_limit(5000, "Huge resize limit test").
```

*NOTE* if you do feel the need to update resize_limit, please do notify
@chewbranca afterwards so we can observe this in more workloads.


### IOQ2 Scale Factor

The scale factor defines the multiplier to use during auto scaling when the
resize limit is hit. This currently defaults to two, which means every ten
auto scale iterations you'll have increased the priority one thousand fold for
any requests that have been in the queue for all ten cycles. This value may or
may not be too aggressive. Setting this to one essentially eliminates the auto
scaling elevator logic entirely, which is not really recommended. You can
update it as follows:

```erlang
ioq_config:set_scale_factor(1.7, "Modifying scale factor").
```

*NOTE* if you do feel the need to update scale_factor, please do notify
@chewbranca afterwards so we can observe this in more workloads.


### IOQ2 Max Priority

Max priority establishes an upper bound on the priority values. It currently
defaults to 10000.0. There is also an implicit lower bound on priority values
of 0.0. Depending on how wild you go with the multipliers, it might be useful
to increase this value, which can be done with the following:

```erlang
ioq_config:set_max_priority(55555.0, "Expand priority space").
```

### IOQ2 Dedupe

Both IOQ1 and IOQ2 have a dedupe feature that will avoid performing the same
read multiple times in parallel. In IOQ1 this operation scanned through lists
and could become a considerable resource hog. In IOQ2 this is a simple khash
lookup and should not be a problem. You should *not* need to ever disable this.
For whatever reason if you need to, you can do so with:

```erlang
ioq_config:set_dedupe(false, "Disable dedupe test").
```

*NOTE* if you do feel the need to update dedupe, please do notify
@chewbranca afterwards so we can observe this in more workloads.


### IOQ2 Bypass

IOQ2 has the same bypass logic as IOQ1, however, the whole point of IOQ2 is to
make a sufficiently performant IOQ that bypasses are *not* necessary. This
functionality was included in IOQ2 as a backup in case max throughput is
essential and unreachable with IOQ2. You can set it in the standard manner, but
in the `ioq2.bypass` namespace as follows:

```erlang
ioq_config:set_bypass(interactive, true, "Bypass interactive channel").
```

*NOTE* if you do feel the need to bypass IOQ2, please do notify
@chewbranca afterwards so we can observe this in more workloads. Yes, this
NOTE blurb is in a number of config descriptions, but _please_ do notify
@chewbranca if you feel the need to bypass anything.


### IOQ2 Dispatch Strategy

IOQ2 utilizes many processes to achieve the desired throughput and performance.
There are several different dispatch strategies for determining how requests are
funneled through these IOQ pids, and a `single_server` fallback in the event
only a single IOQ2 server is desired. Changing dispatch strategies is a *safe*
operation to perform, all the pids already exist and it will just toggle which
to go through. All active requests will continue to go through the IOQ2 pid they
were initially handled by, and any new requests will go through the specified
IOQ2. The four current dispatch strategies are:

  * "server_per_scheduler"
  * "fd_hash"
  * "random"
  * "single_server"


### Server per Scheduler Dispatch Strategy

```erlang
ioq_config:set_dispatch_strategy("server_per_scheduler", "Changing dispatch)).
```

This is the default dispatch strategy. IOQ2 creates `N` `ioq_server2` pids, where
`N` is the number of Erlang VM schedulers on the current system, which defaults
to the number of CPU Cores. This dispatch strategy uses the the current
scheduler of the caller process to determine which IOQ2 server to use. This has
the nice property of automatically distributing work out across IOQ2 servers
based on how the Erlang VM is spreading out work across the schedulers. In
practice this works pretty well and seems reasonable at a high level, but it may
or may not be optimal for all workloads, which is why we have multiple dispatch
strategies.


### FD Hash Scheduler Dispatch Strategy

```erlang
ioq_config:set_dispatch_strategy("fd_hash", "Changing dispatch)).
```

The `fd_hash` dispatch strategy hashes on the couch_file pid the request has as
a destination, and then ensures that all requests to the same couch_file pid go
through a single IOQ2 pid. This provides the most control over prioritization of
requests to individual shards, as _all_ requests to that shard will go through
the single IOQ2 pid, providing global prioritization rather than localized by
IOQ2 pid. This can be useful when dealing with overloaded couch_file pids where
you want to minimize and focus work send to those pids. Also, by funneling all
reuqests to the same shard through the same IOQ2 pid, this increases the
opportunity for deduping requests, which can be significant. This dispatch
strategy can result in uneven distribution of work across IOQ2 pids, so it's not
appropriate for all situations, but for many dedicated clusters this could be an
ideal dispatch strategy.


### Random Dispatch Strategy


```erlang
ioq_config:set_dispatch_strategy("random", "Changing dispatch)).
```

The `random` dispatch strategy just randomly selects one of the IOQ2 pids to
send the request to. This dispatch strategy uses a random normal distribution
and should result in roughly even work distributed across all IOQ2 pids. This is
not the default strategy because if there's less concurrent requests active in
the system than total IOQ2 pids, there will not actually be any prioritization
taking place, in which case the `server_per_scheduler` dispatch strategy should
be preferred as it will reduce the number of IOQ2 pids in use as a function of
how much work is on the system.


### Single Server Dispatch Strategy

```erlang
ioq_config:set_dispatch_strategy("random", "Changing dispatch)).
```

This is a fallback dispatch strategy that may or may not be removed at some
point. This utilizes a single IOQ2 pid for _all_ requests, eliminating the
benefits of parallel IOQ2 pids and inevitably resulting in IOQ2 becoming a
bottleneck in the same way as IOQ1, albeit a faster bottleneck.


---


## Finding the pids

The IOQ2 pids per scheduler have registered names of the form `ioq_server_$N`
where `$N` is the scheduler id, starting from 1. You can get a list of all the
IOQ2 pids on the current system with the following:

```erlang
(node1@127.0.0.1)10> ioq_sup:ioq_server_pids().
[ioq_server_1,ioq_server_2]
```


## ioq_config:ioq_classes

You can see the proper names for all registered IOQ classes with the following:

```erlang
(node1@127.0.0.1)15> ioq_config:ioq_classes().
[customer,internal_repl,view_compact,db_compact,low,db_meta,
 db_update,view_update,other,interactive]
```


### Dynamic IOQ classes

To support 3rd party IO channels for things like search/geo/cache/etc, you can
manually set a config priority for the desired class, and then it will be
picked up by `ioq_config:is_valid_class/1`. Because the `ioq_config:set_*`
setters depend on `is_valid_class`, you must manually define the priority
initially, for example with `config:set("ioq.classes", "search", "1.0").`.
Afterwards, you'll be able to utilize the setters as expected.


## ioq_server2:get_state

You can see a human readable representation of the IOQ2 server state with the
following block of code. The output is "human readable" in that the khash and
hqueue data structures have been transformed into lists so the contents can be
viewed. This fetches the state of the `ioq_server_1` pid. If you want a
different pid you'll need to manually `gen_server:call` into it.

```erlang
(node1@127.0.0.1)16> ioq_server2:get_state().
{state,[],[],[],1,0,
       [{view_update,1.0},
        {view_compact,0.0001},
        {db_compact,0.0001},
        {low,0.0001},
        {db_update,1.0},
        {customer,1.0},
        {internal_repl,0.001},
        {interactive,1.0},
        {other,1.0},
        {db_meta,1.0}],
       [],
       [{{<<"shards/00000000-1fffffff/foo">>,interactive},1.0e3},
        {{<<"shards/00000000-1fffffff/foo/pizza_db">>,db_update},
         1.5}],
       2.0,true,1000,1,ioq_server_1,0,normal,1.0e4}
```

If you want to have the pretty printed version and be able to fetch the fields
directly, you'll need to include the `ioq_server2` records, for example:

```erlang
(node1@127.0.0.1)17> rr(ioq_server2), ioq_server2:get_state().
#state{reqs = [],waiters = [],queue = [],concurrency = 1,
       iterations = 0,
       class_p = [{view_update,1.0},
                  {view_compact,0.0001},
                  {db_compact,0.0001},
                  {low,0.0001},
                  {db_update,1.0},
                  {customer,1.0},
                  {internal_repl,0.001},
                  {interactive,1.0},
                  {other,1.0},
                  {db_meta,1.0}],
       user_p = [],
       shard_p = [{{<<"shards/00000000-1fffffff/foo">>,interactive},
                   1.0e3},
                  {{<<"shards/00000000-1fffffff/foo/pizza_db">>,db_update},
                   1.5}],
       scale_factor = 2.0,dedupe = true,resize_limit = 1000,
       next_key = 1,server_name = ioq_server_1,scheduler_id = 1,
       collect_stats = normal,max_priority = 1.0e4}
```

To fetch the server state from a particular IOQ2 pid, you can do so with the
following:

```erlang
(node1@127.0.0.1)18> gen_server:call(ioq_server_2, get_state).
#state{reqs = [],waiters = [],queue = [],concurrency = 1,
       iterations = 0,
       class_p = [{view_update,1.0},
                  {view_compact,0.0001},
                  {db_compact,0.0001},
                  {low,0.0001},
                  {db_update,1.0},
                  {customer,1.0},
                  {internal_repl,0.001},
                  {interactive,1.0},
                  {other,1.0},
                  {db_meta,1.0}],
       user_p = [],
       shard_p = [{{<<"shards/00000000-1fffffff/foo">>,interactive},
                   1.0e3},
                  {{<<"shards/00000000-1fffffff/foo/pizza_db">>,db_update},
                   1.5}],
       scale_factor = 2.0,dedupe = true,resize_limit = 1000,
       next_key = 1,server_name = ioq_server_2,scheduler_id = 2,
       collect_stats = normal,max_priority = 1.0e4}
```


---


## Gotchas

Some miscellaneous "gotchas" to be aware of.


### Shard Class Configs Drop Suffixes

By default the shard names include the db file suffix which is the timestamp of
creation time. These suffixes must be dropped from the config entries,
otherwise they will not be picked up in the IOQ2 config. For instance, here's
what the default name looks like, followed by properly truncating the suffix:

```erlang
(node1@127.0.0.1)21> S#shard.name.
<<"shards/00000000-1fffffff/foo.1503945430">>
(node1@127.0.0.1)22> filename:rootname(S#shard.name).
<<"shards/00000000-1fffffff/foo">>
```


### Shard Configs Persist Through Db Cycles, but not Reshards

In the `Shard Class Configs Drop Suffixes` "gotcha" above, you'll see that
suffixes are not an allowed part of the shard config keys. The motivation here
is to allow for database configs persisting through cycles, eg deleting and
recreating a database will preserve the config.

*HOWEVER*, if you delete the database and recreate it with a new sharding
factor, you'll end up with a completely different set of shards and the old
configs will no longer map over and it will essentially reset to the defaults.
This should be obvious given that the shard configs are keyed on the full shard
name, so switching said shard name will result in a different config key.
You'll need to manually reset the configs with the appropriate new shards once
the database has been recreated or resharded.


### Non integer/float Priority Configs are Ignored

If you set a config value that is not an integer or floating point value, that
configuration will be silently ignored and replaced with the default value of
`1.0`. Check out the docs above about how to verify config expectations. The
`ioq_config` helpers described above will only allow you to set configs with
floating point values, so if you only use those this should never be a problem.
However if you manually set the config values you might run into this. The
IOQ2 config will attempt to convert integers to floats, but any other values
will be ignored.


### Only a subset of metrics reducers are enabled

The https://metrics.cloudant.com tabs are predominantly powerd by Centinela
reducers that aggregate the node specific metrics into an individual global
metric. Due to the current precarious state of the metrics stack, the IOQ2 work
has been cautious with the introduction of new metrics. Only the reduced
metrics currently enabled on the IOQ2 tab have reducers enabled. If you need
additional reducers you'll need to add them. For example, the iowait metrics
are reduced on median but not P99.9:

```
(chewbranca)-(jobs:0)-(~)
(! 14529)-> acurl
https://cloudant.cloudant.com/metrics/couchdb.io_queue2.iowait.percentile.50
{"_id":"couchdb.io_queue2.iowait.percentile.50","_rev":"1-e0ad9472a61b9a46ace4c9852ce63a36","reduce":true,"reducers":["mean"]}

(chewbranca)-(jobs:0)-(~)
(! 14530)-> acurl
https://cloudant.cloudant.com/metrics/couchdb.io_queue2.iowait.percentile.999
{"error":"not_found","reason":"missing"}
```


### IOQ2 Concurrency is Multiplicative

This is covered in depth in the concurrency sections above, but this is an
important enough point to warrant calling it out here as well. With IOQ1
concurrency is singular for the one IOQ1 pid, but in IOQ2 concurrency is set
for *every* IOQ2 pid. This means you should realistically never set concurrency
above single digits for IOQ2. Setting concurrency to five on a system with 56
CPU cores will result in a total concurrency of over 250, which is probably not
productive.


### IOQ2 Can Only Prioritize Work when there's a Backlog of Work

As mentioned above, IOQ is inherently a bottleneck, otherwise it isn't actually
able to prioritize any work. On a similar note, if there's an insufficient
volume of a particular request type, you won't be able to significantly
influence the throughput of that type. For instance, you can bump compaction
priority to the moon but if there's only one compaction job you're not going to
make a significant difference to the volume of compaction requests.

The other side of this is that IOQ2 is only effective at prioritizing work when
there's a variety of work types. If you've got a problematic MT user that you
want to back burner, you can set the user priority to `0.0` and all their
requests will be prioritized at `0.0` and will never benefit from the auto
scaling elevator. That said, if there are _no_ other users making requests to
the system, then that user's requests will still be chugging along as fast as
they come in. IOQ2 is *NOT* a rate limiting system, it's a prioritization
system that prioritizes requests relative to all other pending requests.
Without sufficient work it's essentially just a pass through.


---


## Request for feedback

IOQ and IOQ2 are complicated beasts, and there's a lot of tunable knobs here.
It is expected that we'll need to experiment with different levels for
different workloads, so please do be diligent about informing @chewbranca of
situations where you've had to change the configuration options above that
request notifications. Any other thoughts/comments/suggestions welcome as well.
Similarly, feedback is welcome on the IOQ2 metrics tab as well.
