# couch_srt: Couch Stats Resource Tracker aka CSRT

The `couch_srt` app introduces the Couch Stats Resource Tracker, aka CSRT for
short. CSRT is a real time stats tracking system that tracks the quantity of
resources induced at the process level in a live queryable manner, while also
generating process lifetime reports containing statistics on the total resource
load of a request, as a function of CouchDB operations like dbs/docs opened,
view and changes rows read, changes returned vs processed, Javascript filter
usage, request duration, and more. This system is a paradigm shift in CouchDB
visibility and introspection, allowing for expressive real time querying
capabilities to introspect, understand, and aggregate CouchDB internal resource
usage, as well as powerful filtering facilities for conditionally generating
reports on "heavy usage" requests or "long/slow" requests. CSRT also extends
`recon:proc_window` with `couch_srt:proc_window` allowing for the same style of
battle hardened introspection with Recon's excellent `proc_window`, but with the
sample window over any of the CSRT tracked CouchDB stats!

CSRT does this by piggy-backing off of the existing metrics tracked by way of
`couch_stats:increment_counter` at the time when the local process induces those
metrics inc calls, and then CSRT updates an ets entry containing the context
information for the local process, such that global aggregate queries can be
performed against the ets table as well as the generation of the process
resource usage reports at the conclusions of the process's lifecyle.The ability
to do aggregate querying in realtime in addition to the process lifecycle
reports for post facto analysis over time, is a cornerstone of CSRT that is the
result of a series of iterations until a robust and scalable aproach was built.

The real time querying is achieved by way of a global ets table with
`read_concurrency`, `write_concurrency`, and `decentralized_counters` enabled.
Great care was taken to ensure that _zero_ concurrent writes to the same key
occure in this model, and this entire system is predicated on the fact that
incremental updates to `ets:update_counters` provides *really* fast and
efficient updates in an atomic and isolated fashion when coupled with
decentralized counters and write concurrency. Each process that calls
`couch_stats:increment_counter` tracks their local context in CSRT as well, with
zero concurrent writes from any other processes. Outside of the context setup
and teardown logic, _only_ operations to `ets:update_counter` are performed, one
per process invocation of `couch_stats:increment_counter`, and one for
coordinators to update worker deltas in a single batch, resulting in a 1:1 ratio
of ets calls to real time stats updates for the primary workloads.

The primary achievement of CSRT is the core framework iself for concurrent
process local stats tracking and real time RPC delta accumulation in a scalable
manner that allows for real time aggregate querying and process lifecycle
reports. This took several versions to find a scalable and robust approach that
induced minimal impact on maximum system throughput. Now that the framework is
in place, it can be extended to track any further desired process local uses of
`couch_stats:increment_counter`. That said, the currently selected set of stats
to track was heavily influenced by the challenges in reotractively understanding
the quantity of resources induced by a query like `/db/_changes?since=$SEQ`, or
similarly, `/db/_find`.

CSRT started as an extension of the Mango execution stats logic to `_changes`
feeds to get proper visibility into quantity of docs read and filtered per
changes request, but then the focus inverted with the realization that we should
instead use the existing stats tracking mechanisms that have already been deemed
critical information to track, which then also allows for the real time tracking
and aggregate query capabilities. The Mango execution stats can be ported into
CSRT itself and just become one subset of the stats tracked as a whole, and
similarly, any additional desired stats tracking can be easily added and will
be picked up in the RPC deltas and process lifetime reports.

## A Simple Example

Given a databse `foo` with 11k documents containg a `doc.value` field that is an
integer value which can be filtered in a design doc by way of even and odd. If
we instantiate a series of while loops in parallel making requests of the form:

> GET /foo/_changes?filter=bar/even&include_docs=true

We can generate a good chunk of load on a local laptop dev setup, resulting in
requests that take a few seconds to load through the changes feed, fetch all 11k
docs, and then funnel them through the Javascript engine to filter for even
valued docs; this allows us time to query these heavier requests live and see
them in progress with the real time stats tracking and querying capabilities of
CSRT.

For example, let's use `couch_srt:proc_window/3` as one would do with
`recon:proc_window/3` to get an idea of the heavy active processes on the
system:

```
(node1@127.0.0.1)2> rp([{PR, couch_srt:to_json(couch_srt:get_resource(PR))} || {PR, _, _} <- couch_srt:proc_window(ioq_calls, 3, 1000)]).
[{{<0.5090.0>,#Ref<0.2277656623.605290499.37969>},
  #{changes_returned => 3962,db_open => 10,dbname => <<"foo">>,
    docs_read => 7917,docs_written => 0,get_kp_node => 54,
    get_kv_node => 1241,ioq_calls => 15834,js_filter => 7917,
    js_filtered_docs => 7917,nonce => <<"cc5a814ceb">>,
    pid_ref =>
        <<"<0.5090.0>:#Ref<0.2277656623.605290499.37969>">>,
    rows_read => 7917,
    started_at => <<"2025-07-21T17:25:08.784z">>,
    type =>
        <<"coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes">>,
    updated_at => <<"2025-07-21T17:25:13.051z">>,
    username => <<"adm">>}},
 {{<0.5087.0>,#Ref<0.2277656623.606601217.92191>},
  #{changes_returned => 4310,db_open => 10,dbname => <<"foo">>,
    docs_read => 8624,docs_written => 0,get_kp_node => 58,
    get_kv_node => 1358,ioq_calls => 17248,js_filter => 8624,
    js_filtered_docs => 8624,nonce => <<"0e625c723a">>,
    pid_ref =>
        <<"<0.5087.0>:#Ref<0.2277656623.606601217.92191>">>,
    rows_read => 8624,
    started_at => <<"2025-07-21T17:25:08.424z">>,
    type =>
        <<"coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes">>,
    updated_at => <<"2025-07-21T17:25:13.051z">>,
    username => <<"adm">>}},
 {{<0.5086.0>,#Ref<0.2277656623.605290499.27728>},
  #{changes_returned => 4285,db_open => 10,dbname => <<"foo">>,
    docs_read => 8569,docs_written => 0,get_kp_node => 57,
    get_kv_node => 1349,ioq_calls => 17138,js_filter => 8569,
    js_filtered_docs => 8569,nonce => <<"962cda1645">>,
    pid_ref =>
        <<"<0.5086.0>:#Ref<0.2277656623.605290499.27728>">>,
    rows_read => 8569,
    started_at => <<"2025-07-21T17:25:08.406z">>,
    type =>
        <<"coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes">>,
    updated_at => <<"2025-07-21T17:25:13.051z">>,
    username => <<"adm">>}}]
ok
```

This shows us the top 3 most active processes (being tracked in CSRT) over the
next 1000 milliseconds, sorted by number of `ioq_calls` induced! All of three
of these processes are incurring heavy usage, reading many thousands of docs
with 15k+ IOQ calls and heavy JS filter usage, exactly the types of requests
you want to be alerted to. CSRT's proc window logic is built on top of Recon's,
which doesn't return the process info itself, so you'll need to fetch the
process status with `couch_srt:get_resource/1` and then pretty print it with
`couch_srt:to_json/1`.

The output above is a real time snapshot of the live running system and shows
processes actively inducing additional resource usage, so these CSRT context
values are just a time snapshot of where that process was at, as of the
`updated_at` timestamp. We can reference the nonce value to search through the
report logs for a final report, assuming the given context ended up using
sufficient resources to trigger a logger matcher lifetime report. The above
changes requests were induced specifically to induce reports as well, so
unsurprisingly we have reports for all three.

However, I want to first show the existing visibility into these changes
requests exposed by the raw HTTP logs to highlight the impact of the CSRT
reports and new visibility into request workloads exposed.

First, let's look at the existing HTTP logs for those 3 requests:

```
(chewbranca)-(jobs:1)-(~/src/couchdb_csrt_v3)
(! 9872)-> grep 'cc5a814ceb\|0e625c723a\|962cda1645' ./dev/logs/node1.log | grep -v '^\[report]'
[notice] 2025-07-21T17:25:14.520641Z node1@127.0.0.1 <0.5087.0> 0e625c723a localhost:15984 127.0.0.1 adm GET /foo/_changes?filter=bar/even&asdf=fdsa&include_docs=true 200 ok 6096
[notice] 2025-07-21T17:25:14.521417Z node1@127.0.0.1 <0.5086.0> 962cda1645 localhost:15984 127.0.0.1 adm GET /foo/_changes?filter=bar/even&asdf=fdsa&include_docs=true 200 ok 6115
[notice] 2025-07-21T17:25:14.844317Z node1@127.0.0.1 <0.5090.0> cc5a814ceb localhost:15984 127.0.0.1 adm GET /foo/_changes?filter=bar/even&asdf=fdsa&include_docs=true 200 ok 6059
```

So we see the requests were made, and we can see it's doing `include_docs=true`
as well as using a customer filter, both obvious indications that this is a
potentially heavier request, however, we don't know if database foo had a
thousand docs or a billion docs, whether those docs were small or large, nor any
indication of the computational complexity of the reference filter function.
This makes it challenging to retroactively correlate heavy resource usage at a
hardware level with the underlying requests that induced those workloads,
especially if the heavy requests are an inconspicuous subset of the full
database workload.

CSRT resolves this by providing a real time querying system to find the active
heavy proceses, live, as well as a process lifecyle reporting engine providing
detailed analysis of the workloads induced by the request.

Let's assume we had the default IOQ logger matcher enabled, with the default
configuration of logging any requests inducing more than 10k IOQ calls, which
would catch all three of our requests above, even though they're all still
going. As a result, we generate process lifecylce reports for all three of those
requests, as we can see:

```
(chewbranca)-(jobs:1)-(~/src/couchdb_csrt_v3)
(! 9873)-> grep 'cc5a814ceb\|0e625c723a\|962cda1645' ./dev/logs/node1.log | grep '^\[report]'
[report] 2025-07-21T17:25:14.520787Z node1@127.0.0.1 <0.5174.0> -------- [csrt-pid-usage-lifetime changes_returned=5500 db_open=10 dbname="foo" docs_read=11001 get_kp_node=72 get_kv_node=1754 ioq_calls=22002 js_filter=11001 js_filtered_docs=11001 nonce="0e625c723a" pid_ref="<0.5087.0>:#Ref<0.2277656623.606601217.92191>" rows_read=11001 started_at="2025-07-21T17:25:08.424z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-21T17:25:14.520z" username="adm"]
[report] 2025-07-21T17:25:14.521578Z node1@127.0.0.1 <0.5155.0> -------- [csrt-pid-usage-lifetime changes_returned=5500 db_open=10 dbname="foo" docs_read=11001 get_kp_node=72 get_kv_node=1754 ioq_calls=22002 js_filter=11001 js_filtered_docs=11001 nonce="962cda1645" pid_ref="<0.5086.0>:#Ref<0.2277656623.605290499.27728>" rows_read=11001 started_at="2025-07-21T17:25:08.406z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-21T17:25:14.521z" username="adm"]
[report] 2025-07-21T17:25:14.844436Z node1@127.0.0.1 <0.5213.0> -------- [csrt-pid-usage-lifetime changes_returned=5500 db_open=10 dbname="foo" docs_read=11001 get_kp_node=72 get_kv_node=1754 ioq_calls=22002 js_filter=11001 js_filtered_docs=11001 nonce="cc5a814ceb" pid_ref="<0.5090.0>:#Ref<0.2277656623.605290499.37969>" rows_read=11001 started_at="2025-07-21T17:25:08.784z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-21T17:25:14.844z" username="adm"]
```

We find the process lifecycle reports for the requests with the three grep'ed on
nonces, and we can see they all read the 11k core documents, plus the one design
document, JS filtered all 11,001 docs, and then only returned the 5500 doc's
containing an even `doc.value` field.

This also shows the discrepancy between the quantity of induced resource usage
to actually generate a request, relative to the magnitude of the data returned.
All of our `doc.value` fields were positive integers, if we had a filter
function searching for negative `doc.value` results, we would have found none,
resulting in `changes_returned=0`, but we would have still induced the 11,001
doc loads and Javascript filter calls.

CSRT is specifically built to automatically find and report these types of
workload discrepancies and in general to help highlight where individual HTTP
requests use drastically more resources than the median workloads.

See the dedicated proc window documentation section further down for more info.

## Additional Overview and Examples

The query and HTTP API's are well documented and tested (h/t @iilyak) and
provide an excellent overview of the interaction patterns and query capabilities
of CSRT. Those can be found at:

* `couch_srt_query.erl` "Query API functions"
  - https://github.com/apache/couchdb/blob/93bc894380056ccca1f77415454e991c4d914249/src/couch_stats/src/couch_srt_query.erl#L319-L674
  - the above highlighted functions are well tested, typespec'ed, and have
    auxilary documentation and examples, an excellent resource
* the `couch_srt_query_tests.erl` Eunit tests are an excellent overview of utilizing
  the `couch_srt_query:` API from Erlang to find, filter, and aggregate CSRT real
  time contexts
  - https://github.com/apache/couchdb/blob/93bc894380056ccca1f77415454e991c4d914249/src/couch_stats/test/eunit/couch_srt_query_tests.erl
* similarly, the `couch_srt_httpd_tests.erl` Eunit tests are an excellent overview of
  performing the same style `couch_srt_query:` queries, but through the HTTP API
  - https://github.com/apache/couchdb/blob/93bc894380056ccca1f77415454e991c4d914249/src/couch_stats/test/eunit/couch_srt_httpd_tests.erl
* Additionally there's the `couch_srt_logger_tests.erl` Eunit tests which demonstrate
  the different default logger matchers in action
  - https://github.com/apache/couchdb/blob/93bc894380056ccca1f77415454e991c4d914249/src/couch_stats/test/eunit/couch_srt_logger_tests.erl

# CSRT Config Keys

## -define(CSRT, "csrt").

> config:get("csrt").

Primary CSRT config namespace: contains core settings for enabling different
layers of functionality in CSRT, along with global config settings for limiting
data volume generation.

## -define(CSRT_MATCHERS_ENABLED, "csrt_logger.matchers_enabled").

> config:get("csrt_logger.matchers_enabled").

Config toggles for enabling specific builtin logger matchers, see the dedicated
section below on `# CSRT Default Matchers`.

## -define(CSRT_MATCHERS_THRESHOLD, "csrt_logger.matchers_threshold").

> config:get("csrt_logger.matchers_threshold").

Config settings for defining the primary `Threshold` value of the builtin logger
matchers, see the dedicated section below on `# CSRT Default Matchers`.

## -define(CSRT_MATCHERS_DBNAMES, "csrt_logger.dbnames_io").

> config:get("csrt_logger.matchers_enabled").

Config section for setting `$db_name = $threshold` resulting in instantiating a
"dbnames_io" logger matcher for each `$db_name` that will generate a CSRT
lifecycle report for any contexts that that induced more operations on _any_ one
field of `ioq_calls|get_kv_node|get_kp_node|docs_read|rows_read` that is greater
than `$threshold` and is on database `$db_name`.

This is basically a simple matcher for finding heavy IO requests on a particular
database, in a manner amenable to key/value pair specifications in this .ini
file until a more sophisticated declarative model exists. In particular, it's
not easy to sequentially generate matchspecs by way `ets:fun2ms/1`, and so an
alternative mechanism for either dynamically assembling an `#rctx{}` to match
against or generating the raw matchspecs themselves is warranted.

# CSRT Code Markers

## -define(CSRT_ETS, csrt_server).

This is the reference to the CSRT ets table, it's managed by `csrt_server` so
that's where the name originates from.

## -define(MATCHERS_KEY, {csrt_logger, all_csrt_matchers}).

This marker is where the active matchers are written to in `persisten_term` for
concurrently and parallelly and accessing the logger matchers in the CSRT
tracker processes for lifecycle reporting.

# CSRT Process Dictionary Markers

## -define(PID_REF, {csrt, pid_ref}).

This marker is for the core storing the core `PidRef` identifier. The key idea
here is that a lifecycle is a context lifecycle is contained to within the given
`PidRef`, meaning that a `Pid` can instantiate different CSRT lifecycles and
pass those to different workers.

This is specifically necessary for long running processes that need to handle
many CSRT context lifecycles over the course of that individual process's
lifecycle independent. In practice, this is immediately needed for the actual
coordinator lifecycle tracking, as `chttpd` uses a worker pool of http request
handlers that can be re-used, so we need a way to create a CSRT lifecycle
corresponding to the given request currently being serviced. This is also
intended to be used in other long running processes, like IOQ or `couch_js` pids
such that we can track the specific context inducing the operations on the
`couch_file` pid or indexer or replicator or whatever.

Worker processes have a more clear cut lifecycle, but either style of process
can be exit'ed in a manner that skips the ability to do cleanup operations, so
additionally there's a dedicated tracker process spawned to monitor the process
that induced the CSRT context such that we can do the dynamic logger matching
directly in these tracker processes and also we can properly cleanup the ets
entries even if the Pid crashes.

## -define(TRACKER_PID, {csrt, tracker}).

A handle to the spawned tracker process that does cleanup and logger matching
reports at the end of the process lifecycle. We store a reference to the tracker
pid so that for explicit context destruction, like in `chttpd` workers after a
request has been serviced, we can update stop the tracker and perform the
expected cleanup directly.

## -define(DELTA_TA, {csrt, delta_ta}).

This stores our last delta snapshot to track progress since the last incremental
streaming of stats back to the coordinator process. This will be updated after
the next delta is made with the latest value. Eg this stores `T0` so we can do
`T1 = get_resource()` `make_delta(T0, T1)` and then we save `T1` as the new `T0`
for use in our next delta.

## -define(LAST_UPDATED, {csrt, last_updated}).

This stores the integer corresponding to the `erlang:monotonic_time()` value of
the most recent `updated_at` value. Basically this lets us utilize a pdict
value to be able to turn `update_at` tracking into an incremental operation that
can be chained in the existing atomic `ets:update_counter` and
`ets:update_element` calls.

The issue being that our updates are of the form `+2 to ioq_calls for $pid_ref`,
which ets does atomically in a guaranteed `atomic` and `isolated` manner. The
strict use of the atomic operations for tracking these values is why this
system works effeciently at scale. This means that we can increment counters on
all of the stats counter fields in a batch, very quickly, but for tracking
`updated_at` timestamps we'd need to either do an extra ets call to get the last
`updated_at` value, or do an extra ets call to `ets:update_element` to set the
`updated_at` value to `couch_srt_util:tnow()`. The core problem with this is that the
batch inc operation is essentially the only write operation performed after the
initial context setting of dbname/handler/etc; this means that we'd literally
double the number of ets calls induced to track CSRT updates, just for tracking
the `updated_at`. So instead, we rely on the fact that the local process
corresponding to `$pid_ref` is the _only_ process doing updates so we know the
last `updated_at` value will be the last time this process updated the data. So
we track that value in the pdict and then take a delta between `tnow()` and
`updated_at`, and then `updated_at` becomes a value we can sneak into the other
integer counter updates we're already performing!

# Primary Config Toggles

# CSRT (?CSRT="csrt") Config Settings

## config:get(?CSRT, "enable", false).

Core enablement toggle for CSRT, defaults to false. Enabling this setting
intiates local CSRT stats collection as well as shipping deltas in RPC
responses to accumulate in the coordinator.

This does _not_ trigger the new RPC spawn metrics, and it does not enable
reporting for any of the rctx types.

*NOTE*: you *MUST* have all nodes in the cluster running a CSRT aware CouchDB
_before_ you enable it on any node, otherwise the old version nodes won't know
how to handle the new RPC formats including an embedded Delta payload.

## config:get(?CSRT, "enable_init_p", false).

Enablement of tracking new metric counters for different `fabric_rpc` operations
spawned by way of `rexi_server:init_p/3`. This is the primary mechanism for
inducing database RPC operations within CouchDB, and these init_p metrics aim to
provide node lever understandings of the workloads being induced by other
coordinator proceses. This is especially relevant for databases on subsets of a
cluster resulting in non-uniform workloads, these metrics are tailored to
provide insight into what work is being spawned on each node in the cluster as a
function of time.

## config:get(?CSRT, "enable_reporting", false).

This is the primary toggle for enabling CSRT process lifetime reports containing
detailed information about the quantity of work induced by the given
request/worker/etc. This is the top level toggle for enabling _any_ reporting,
and there also exists `config:get(?CSRT, "enable_rpc_reporting", false).` to
disable the reporting of any individual RPC workers, leaving the coordinator
responsible of generating a report with the accumulated deltas.

## config:get(?CSRT, "enable_rpc_reporting", false).

This enables the possibility of RPC workers generating reports. They still need
to hit the configured thresholds to induce a report, but this will generate CSRT
process lifetime reports for individual RPC workers that trigger the configured
logger thresholds. This allows for quantifying per node resource usage when
desired, as otherwise the reports are at the http request level and don't
provide per node stats.

The key idea here is that having RPC level CSRT process lifetime reporting is
incredibly useful, but can also generate large quantities of data. For example,
a view query on a Q=64 database will stream results from 64 shard replicas,
resulting in at least 64 RPC reports, plus any that might have been generated
from RPC workers that "lost" the race for shard replica. This is very useful,
but a lot of data given the verbose nature of funneling it through the RSyslog
reports, however, the ability to write directly to something like ClickHouse or
another columnar store would be great.

Until there's an efficient storage mechanism to stream the results to, the
rsyslog entries work great and are very practical, but care must be taken to
not generate too much data for aggregate queries as they generate at least `Qx`
more report than an individual report per http request from the coordinator.
This setting exists as a way to either a) utilize the logger matcher configured
thresholds to allow for _any_ rctx's to be recorded when they induce heavy
operations, either Coordinator or RPC worker; or b) to _only_ log workloads at
the coordinator level.

NOTE: this setting exists because we lack an expressive enough config
declaration to easily chain the matchspec constructions as `ets:fun2ms/1` is a
special compile time parse transform macro that requires the fully definition to
be specified directly, it cannot be iteractively constructed. That said, you
_can_ register matchers through remsh with more specific and fine grained
pattern matching, and a more expressive system for defining matchers are being
explored.

## config:get_boolean(?CSRT, "should_truncate_reports", true)

Enables truncation of the CSRT process lifetime reports to not include any
fields that are zero at the end of process lifetime, eg don't include
`js_filter=0` in the report if the request did not induce Javascript filtering.

This can be disabled if you really care about consistent fields in the report
logs, but this is a log space saving mechanism, similar to disabling RPC
reporting by default, as its a simple way to reduce overall volume

## config:get(?CSRT, "randomize_testing", true).

This is a `make eunit` only feature toggle that will induce randomness into the
cluster's `couch_srt:is_enabled()` state, specifically to utilize the test suite to
exercise edge case scenarios and failures when CSRT is only conditionally
enabled, ensuring that it gracefuly and robustly handles errors without fallout
to the underlying http clients.

The idea here is to introduce randomness into whether CSRT is enabled across all
the nodes to simulate clusters with heterogeneous CSRT enablement and also to
ensure that CSRT works properly when toggled on/off wihout causing any
unexpected fallout to the client requests.

This is a config toggle specifically so that the actual CSRT tests can disable
it for making accurate assertions about resource usage traacking, and is not
intended to be used directly.

## config:get_integer(?CSRT, "query_limit", ?QUERY_LIMIT)

Limit the quantity of rows that can be loaded in an http query.

# CSRT Logger Matcher Enablement and Thresholds

There are currently six builtin default loggers designed to make it easy to do
filtering on heavy resource usage inducing and long running requests. These are
designed as a simple baseline of useful matchers, declared in a manner amenable
to `default.ini` based constructs. More expressive matcher declarations are
being explored, and matchers of arbitrary complexity can be registered directly
through remsh. The default matchers are all designed around an integer config
Threshold that triggers on a specific field, eg docs read, or on a delta of
fields for long requests and changes requests that process many rows but return
few.

The current default matchers are:

  * docs_read: match all requests reading more than N docs
  * rows_read: match all requests reading more than N rows
  * docs_written: match all requests writing more than N docs
  * long_reqs: match all requests lasting more than N milliseconds
  * changes_processed: match all changes requests that returned at least N rows
    less than was necessarily loaded to complete the request (eg find heavy
    filtered changes requests reading many rows but returning few).
  * ioq_calls: match all requests inducing more than N ioq_calls

Each of the default matchers has an enablement setting in
`config:get(?CSRT_MATCHERS_ENABLED, Name)` for toggling enablement of it, and a
corresponding threshold value setting in `config:get(?CSRT_MATCHERS_THRESHOLD,
Name)` that is an integer value corresponding to the specific nature of that
matcher.

## CSRT Logger Matcher Enablement (?CSRT_MATCHERS_ENABLED)

> -define(CSRT_MATCHERS_THRESHOLD, "csrt_logger.matchers_enabled").

### config:get_boolean(?CSRT_MATCHERS_ENABLED, "docs_read", false)

Enable the `docs_read` builtin matcher, with a default `Threshold=1000`, such
that any request that reads more than `Threshold` docs will generate a CSRT
process lifetime report with a summary of its resouce consumption.

This is different from the `rows_read` filter in that a view with `?limit=1000`
will read 1000 rows, but the same request with `?include_docs=true` will also
induce an additional 1000 docs read.

### config:get_boolean(?CSRT_MATCHERS_ENABLED, "rows_read", false)

Enable the `rows_read` builtin matcher, with a default `Threshold=1000`, such
that any request that reads more than `Threshold` rows will generate a CSRT
process lifetime report with a summary of its resouce consumption.

This is different from the `docs_read` filter so that we can distinguish between
heavy view requests with lots of rows or heavy requests with lots of docs.

### config:get_boolean(?CSRT_MATCHERS_ENABLED, "docs_written", false)

Enable the `docs_written` builtin matcher, with a default `Threshold=500`, such
that any request that writtens more than `Threshold` docs will generate a CSRT
process lifetime report with a summary of its resouce consumption.

### config:get_boolean(?CSRT_MATCHERS_ENABLED, "ioq_calls", false)

Enable the `ioq_calls` builtin matcher, with a default `Threshold=10000`, such
that any request that induces more than `Threshold` IOQ calls will generate a
CSRT process lifetime report with a summary of its resouce consumption.

### config:get_boolean(?CSRT_MATCHERS_ENABLED, "long_reqs", false)

Enable the `long_reqs` builtin matcher, with a default `Threshold=60000`, such
that any request where the the last CSRT rctx `updated_at` timestamp is at least
`Threshold` milliseconds grather than the `started_at timestamp` will generate a
CSRT process lifetime report with a summary of its resource consumption.

## CSRT Logger Matcher Threshold (?CSRT_MATCHERS_THRESHOLD)

> -define(CSRT_MATCHERS_THRESHOLD, "csrt_logger.matchers_threshold").

### config:get_integer(?CSRT_MATCHERS_THRESHOLD, "docs_read", 1000)

Threshold for `docs_read` logger matcher, defaults to `1000` docs read.

### config:get_integer(?CSRT_MATCHERS_THRESHOLD, "rows_read", 1000)

Threshold for `rows_read` logger matcher, defaults to `1000` rows read.

### config:get_integer(?CSRT_MATCHERS_THRESHOLD, "docs_written", 500)

Threshold for `docs_written` logger matcher, defaults to `500` docs written.

### config:get_integer(?CSRT_MATCHERS_THRESHOLD, "ioq_calls", 10000)

Threshold for `ioq_calls` logger matcher, defaults to `10000` IOQ calls made.

### config:get_integer(?CSRT_MATCHERS_THRESHOLD, "long_reqs", 60000)

Threshold for `long_reqs` logger matcher, defaults to `60000` milliseconds.

# Core CSRT API

The `csrt(.erl)` module is the primary entry point into CSRT, containing API
functionality for tracking the lifecycle of processes, inducing metric tracking
over that lifecycle, and also a variety of functions for aggregate querying.

It's worth noting that the CSRT context tracking functions are specifically
designed to not `throw` and be safe in the event of unexpected CSRT failures or
edge cases. The aggregate query API has some callers that will actually throw,
but aside from this core CSRT operations will not bubble up exceptions, and will
either return the error value, or catch the error and move on rather than
chaining further errors.

## PidRef API

These are functions are CRUD operations around creating and storing the CSRT
`PidRef` handle.

```
-export([
    destroy_pid_ref/0,
    destroy_pid_ref/1,
    create_pid_ref/0,
    get_pid_ref/0,
    get_pid_ref/1,
    set_pid_ref/1
]).
```

## Context Lifecycle API

These are the CRUD functions for handling a CSRT context lifecycle, where a
lifecycle context is created in a `chttpd` coordinator process by way of
`couch_srt:create_coordinator_context/2`, or in `rexi_server:init_p` by way of
`couch_srt:create_worker_context/3`. Additional functions are exposed for setting
context specific info like username/dbname/handler. `get_resource` fetches the
context being tracked corresponding to the given `PidRef`.

```
-export([
    create_context/2,
    create_coordinator_context/2,
    create_worker_context/3,
    destroy_context/0,
    destroy_context/1,
    get_resource/0,
    get_resource/1,
    set_context_dbname/1,
    set_context_dbname/2,
    set_context_handler_fun/1,
    set_context_handler_fun/2,
    set_context_username/1,
    set_context_username/2
]).
```

## Public API

The "Public" or miscellaneous API for lack of a better name. These are various
functions exposed for wider use and/or testing purposes.

```
-export([
    clear_pdict_markers/0,
    do_report/2,
    is_enabled/0,
    is_enabled_init_p/0,
    maybe_report/2,
    to_json/1
]).
```

## Stats Collection API

This is the stats collection API utilized by way of
`couch_stats:increment_counter` to do local process tracking, and also in `rexi`
to adding and extracting delta contexts and then accumulating those values.

NOTE: `make_delta/0` is a "destructive" operation that will induce a new delta
by way of the last local pdict's rctx delta snapshot, and then update to the
most recent version. Two individual rctx snapshots for a PidRef can safely
generate an actual delta by way of `couch_srt_util:rctx_delta/2`.

```
-export([
    accumulate_delta/1,
    add_delta/2,
    docs_written/1,
    extract_delta/1,
    get_delta/0,
    inc/1,
    inc/2,
    ioq_called/0,
    js_filtered/1,
    make_delta/0,
    rctx_delta/2,
    maybe_add_delta/1,
    maybe_add_delta/2,
    maybe_inc/2,
    should_track_init_p/1
]).
```

## Query API

See the `Additional Overview and Examples` section above for more details.

```
% Aggregate Query API
-export([
    active/0,
    active/1,
    active_coordinators/0,
    active_coordinators/1,
    active_workers/0,
    active_workers/1,
    find_by_nonce/1,
    find_by_pid/1,
    find_by_pidref/1,
    find_workers_by_pidref/1,
    query_matcher/1,
    query_matcher/2
]).

-export([
    query/1,
    from/1,
    group_by/1,
    group_by/2,
    sort_by/1,
    sort_by/2,
    count_by/1,
    options/1,
    unlimited/0,
    with_limit/1,

    run/1,
    unsafe_run/1
]).
```

## couch_srt:proc_window/3 -- Recon API Ports of https://github.com/ferd/recon/releases/tag/2.5.6

This is a "port" of `recon:proc_window` to `couch_srt:proc_window`, allowing for
`proc_window` style aggregations/sorting/filtering but with the stats fields
collected by CSRT! This is also a direct port of `recon:proc_window` in that it
utilizes the same underlying logic and effecient internal data structures as
`recon:proc_window`, but rather only changes the Sample function:

```erlang
%% This is a recon:proc_window/3 [1] port with the same core logic but
%% recon_lib:proc_attrs/1 replaced with pid_ref_attrs/1, and returning on
%% pid_ref() rather than pid().
%% [1] https://github.com/ferd/recon/blob/c2a76855be3a226a3148c0dfc21ce000b6186ef8/src/recon.erl#L268-L300
-spec proc_window(AttrName, Num, Time) -> term() | throw(any()) when
    AttrName :: rctx_field(), Num :: non_neg_integer(), Time :: pos_integer().
proc_window(AttrName, Num, Time) ->
    Sample = fun() -> pid_ref_attrs(AttrName) end,
    {First, Last} = recon_lib:sample(Time, Sample),
    recon_lib:sublist_top_n_attrs(recon_lib:sliding_window(First, Last), Num).
```

In particular, our change is `Sample = fun() -> pid_ref_attrs(AttrName) end,`,
and in fact, if recon upstream parameterized the option of `AttrName` or
`SampleFunction`, this could be reimplemented as:

```erlang
%% couch_srt:proc_window
proc_window(AttrName, Num, Time) ->
    Sample = fun() -> pid_ref_attrs(AttrName) end,
    recon:proc_window(Sample, Num, Time).
```

This implementation is being highlighted here because `recon:proc_window/3` is
battle hardened and `recon_lib:sliding_window` uses an effecient internal data
structure for storing the two samples that has been proven to work in production
systems with millions of active processes, so swapping the `Sample` function
with a CSRT version allows us to utilize the production grade recon
functionality, but extended out to the particular CouchDB statistics we're
esepecially interested in.

And on a fun note: any further stats tracking fields added to CSRT tracking will
automatically work with this too.


```
-export([
    pid_ref_attrs/1,
    pid_ref_matchspec/1,
    proc_window/3
]).
```

<hr />

# Core types and Maybe types

Before we look at the `#rctx{}` record fields, lets examine the core datatypes
defined by CSRT for use in Dialyzer typespecs. There are more, but these are the
essentials and demonstrate the "maybe" typespec approach utilized in CSRT.

Let's say we have a `-type foo() :: #foo{}` and `-type maybe_foo() :: foo() |
undefined`, we then can construct functions of the form `-spec get_foo(id()) ->
maybe_foo()` and then we can use Dialyzer to statically assert all callers of
`get_foo/1` handle the `maybe_foo()` data type rather than just `foo()` and
ensure that all subsequent callers do as well.

This approach of `-spec maybe_<Type> :: <Type> | undefined` is utilized
throughout CSRT and has greatly aided in the development, refactoring, and
static analysis of this system. Here's a useful snippet for running Dialyzer
while hacking on CSRT:

> make && time make dialyze apps=couch_stats

```erlang
-type pid_ref() :: {pid(), reference()}.
-type maybe_pid_ref() :: pid_ref() | undefined.

-type coordinator_rctx() :: #rctx{type :: coordinator()}.
-type rpc_worker_rctx() :: #rctx{type :: rpc_worker()}.
-type rctx() :: #rctx{} | coordinator_rctx() | rpc_worker_rctx().
-type rctxs() :: [#rctx{}] | [].
-type maybe_rctx() :: rctx() | undefined.
```

Above we have the core `pid_ref()` data type, which is just a tuple with a
`pid()` and a `reference()`, and naturally, `maybe_pid_ref()` handles the
optional presence of a `pid_ref()`, allowing for our APIs like
`couch_srt:get_resource(maybe_pidref())` to handle ambiguity of the presence of a
`pid_ref()`.

We define our core `rctx()` data type as an empty `#rctx{}`, or the more
specific `coordinator_rctx()` or `rpc_worker_rctx()` such that we can be
specific about the `rctx()` type in functions that need to distinguish. And then
as expected, we have the notion of `maybe_rctx()`.

# #rctx{}

This is the core data structure utilized to track a CSRT context for a
coordinator or rpc_worker process, represented by the `#rctx{}` record, and
stored in the `?CSRT_ETS` table keyed on `{keypos, #rctx.pid_ref}`.

The Metadata fields store labeling data for the given process being tracked,
such as started_at and updated_at timings, the primary `pid_ref` id key, the
type of the process context, and some additional information like username,
dbname, and the nonce of the coordinator request.

The Stats Counters fields are `non_neg_integer()` monotonically increasing
counters corresponding to the `couch_stats` metrics counters we're interested in
tracking at a process level cardinality. The use of these purely integer counter
fields represented by a record represented in an ets table is the cornerstone of
CSRT and why its able to operate at high throughput and high concurrency, as
`ets:update_counter/{3,4}` take increment operations to be performed atomically
and in isolation, in a manner in which does not require fetching and loading the
data directly. We then take care to batch the accumulation of delta updates into
a single `update_counter` call and even sneak in the `updated_at` tracking as a
integer counter update without inducing an extra ets call.

NOTE: the typespec's for these fields include `'_'` atoms as possible types as
that is the matchspec wildcard any of the fields can be set to when using an
existing `#rctx{}` record to search with.


```erlang
-record(rctx, {
    %% Metadata
    started_at = couch_srt_util:tnow() :: integer() | '_',
    %% NOTE: updated_at must be after started_at to preserve time congruity
    updated_at = couch_srt_util:tnow() :: integer() | '_',
    pid_ref :: maybe_pid_ref() | {'_', '_'} | '_',
    nonce :: nonce() | undefined | '_',
    type :: rctx_type() | undefined | '_',
    dbname :: dbname() | undefined | '_',
    username :: username() | undefined | '_',

    %% Stats Counters
    db_open = 0 :: non_neg_integer() | '_',
    docs_read = 0 :: non_neg_integer() | '_',
    docs_written = 0 :: non_neg_integer() | '_',
    rows_read = 0 :: non_neg_integer() | '_',
    changes_returned = 0 :: non_neg_integer() | '_',
    ioq_calls = 0 :: non_neg_integer() | '_',
    js_filter = 0 :: non_neg_integer() | '_',
    js_filtered_docs = 0 :: non_neg_integer() | '_',
    get_kv_node = 0 :: non_neg_integer() | '_',
    get_kp_node = 0 :: non_neg_integer() | '_'
    %% "Example to extend CSRT"
    %%write_kv_node = 0 :: non_neg_integer() | '_',
    %%write_kp_node = 0 :: non_neg_integer() | '_'
}).
```

## Metadata

We use `couch_srt_util:tnow()` for time tracking, which is a `native` format
`erlang:monotonic_time()` integer, which, noteably, _can_ be and is often a
negative value. You must either take a delta or convert the time to get into a
useable format, as one might suspect by the use of `native`.

We make use of `erlang:mononotic_time/0` as per the recommendation in
https://www.erlang.org/doc/apps/erts/time_correction.html#how-to-work-with-the-new-api
for the suggested way to `Measure Elasped Time`, as quoted:

```
Take time stamps with erlang:monotonic_time/0 and calculate the time difference
using ordinary subtraction. The result is in native time unit. If you want to
convert the result to another time unit, you can use erlang:convert_time_unit/3.

An easier way to do this is to use erlang:monotonic_time/1 with the desired time
unit. However, you can then lose accuracy and precision.
```

So our `couch_srt_util:tnow/0` is implemented as the following, and we store
timestamps in `native` format as long as possible to avoid precision loss at
higher units of time, eg 300 microseconds is zero milliseconds.

```
-spec tnow() -> integer().
tnow() ->
    erlang:monotonic_time().
```

We store timestamps in the node's local erlang representation of time,
specifically to be able to effeciently do time deltas, and then we track time
deltas from the local node's perspective to not send timestamps across the wire.
We then utilize `calendar:system_time_to_rfc3339` to convert the local node's
native time representation to its corresponding time format when we generate the
process life cycle reports or send an http response.

NOTE: because we do an inline definition and assignment of the
`#rctx.started_at` and `#rctx.updated_at` fields to `couch_srt_util:tnow()`, we
_must_ declare `#rctx.updated_at` *after* `#rctx.started_at` to avoid
fundamental time incongruenties.

### #rctx.started_at = couch_srt_util:tnow() :: integer() | '_',

A static value corresponding to the local node's Erlang monotonic_time at which
this context was created.

### #rctx.updated_at = couch_srt_util:tnow() :: integer() | '_',

A dynamic value corresponding to the local node's Erlang monotonic_time at which
this context was updated. Note: unlike `#rctx.started_at`, this value will
update over time, and in the process lifecycle reports the `#rctx.updated_at`
value corresponds to the point at which the context was destroyed, allowing for
calculation of the total duration of the request/context.

### #rctx.pid_ref :: maybe_pid_ref() | {'_', '_'} | '_',

The primary identifier used to track the resources consumed by a given `pid()`
for a specific context identified with a `make_ref()`, and combined together as
unit as a given `pid()`, eg the `chttpd` worker pool, can have many contexts
over time.

### #rctx.nonce :: nonce() | undefined | '_',

The `Nonce` value of the http request being serviced by the `coordinator_rctx()`
used as the primary grouping identifier of workers across the cluster, as the
`Nonce` is funneled through `rexi_server`.

### #rctx.type :: rctx_type() | undefined | '_',

A subtype classifier for the `#rctx{}` contexts, right now only supporting
`#rpc_worker{}` and `#coordinator{}`, but CSRT was designed to accomodate
additional context types like `#view_indexer{}`, `#search_indexer{}`,
`#replicator{}`, `#compactor{}`, `#etc{}`.

### #rctx.dbname :: dbname() | undefined | '_',

The database name, filled in at some point after the initial context creation by
way of `couch_srt:set_context_dbname/{1,2}`.

### #rctx.username :: username() | undefined | '_',

The requester's username, filled in at some point after the initial context
creation by way of `couch_srt:set_context_username/{1,2}`.

## Stats Counters

All of these stats counters are stricly `non_neg_integer()` counter values that
are monotonically increasing, as we only induce positive counter increment calls
in CSRT. Not all of these values will be nonzero, eg if the context doesn't
induce Javascript filtering of documents, it won't inc the `#rctx.js_filter`
field. The `"should_truncate_reports"` config value described in this document
will conditionally exclude the zero valued fields from being included in the
process life cycle report.

### #rctx.db_open = 0 :: non_neg_integer() | '_',

> Tracking `couch_stats:increment_counter([couchdb, couch_server, open])

The number of `couch_server:open/2` invocations induced by this context.

### #rctx.docs_read = 0 :: non_neg_integer() | '_',

> Tracking `couch_stats:increment_counter([couchdb, database_reads])

The number of `couch_db:open_doc/3` invocations induced by this context.

### #rctx.docs_written = 0 :: non_neg_integer() | '_',

A phony metric counting docs written by the context, induced by
`couch_srt:docs_written(length(Docs0)),` in `fabric_rpc:update_docs/3` as a way to
count the magnitude of docs written, as the actual document writes happen in the
`#db.main_pid` `couch_db_updater` pid and subprocess tracking is not yet
supported in CSRT.

This can be replaced with direct counting  once passthrough contexts work.

### #rctx.rows_read = 0 :: non_neg_integer() | '_',

> Tracking `couch_stats:increment_counter([fabric_rpc, changes, processed])
> also Tracking `couch_stats:increment_counter([fabric_rpc, view, rows_read])

A value tracking multiple possible metrics corresponding to rows streamed in
aggregate operations. This is used for view_rows/changes_rows/all_docs/etc.

### #rctx.changes_returned = 0 :: non_neg_integer() | '_',

The number of `fabric_rpc:changes_row/2` invocations induced by this context,
specifically tracking the number of changes rows streamed back to the client
requeest, allowing for distinguishing between the number of changes processed to
fulfill a request versus the number actually returned in the http response.

### #rctx.ioq_calls = 0 :: non_neg_integer() | '_',

A phony metric counting invocations of `ioq:call/3` induced by this context. As
with `#rctx.docs_written`, we need a proxy metric to reperesent these calls
until CSRT context passing is supported so that the `ioq_server` pid and return
its own delta back to the worker pid.

### #rctx.js_filter = 0 :: non_neg_integer() | '_',

A phony metric counting the number of `couch_query_servers:filter_docs_int/5`
(eg ddoc_prompt) invocations induced by this context. This is called by way of
`couch_srt:js_filtered(length(JsonDocs))` which both increments `js_filter` by 1, and
`js_filtered_docs` by the length of the docs so we can track magnitude of docs
and doc revs being filtered.

### #rctx.js_filtered_docs = 0 :: non_neg_integer() | '_',

A phony metric counting the quantity of documents filtered by way of
`couch_query_servers:filter_docs_int/5` (eg ddoc_prompt) invocations induced by
this context. This is called by way of `couch_srt:js_filtered(length(JsonDocs))`
which both increments `#rctx.js_filter` by 1, and `#rctx.js_filtered_docs` by
the length of the docs so we can track magnitude of docs and doc revs being
filtered.

### #rctx.get_kv_node = 0 :: non_neg_integer() | '_',

This metric tracks the number of invocations to `couch_btree:get_node/2` in
which the `NodeType` returned by `couch_file:pread_term/2` is `kv_node`, instead
of `kp_node`.

This provides a mechanism to quantify the impact of document count and document
size as those values become larger in the logarithmic complexity btree
algorithms.  size on the logarithmic complexity btree algorithms as the database
btrees grow.

### #rctx.get_kp_node = 0 :: non_neg_integer() | '_'

This metric tracks the number of invocations to `couch_btree:get_node/2` in
which the `NodeType` returned by `couch_file:pread_term/2` is `kp_node`, instead
of `kv_node`.

This provides a mechanism to quantify the impact of document count and document
size as those values become larger in the logarithmic complexity btree
algorithms.  size on the logarithmic complexity btree algorithms as the database
btrees grow.

# Extending CSRT

There are documentation markers in the code highlighting where and how to extend
CSRT with additional stats to track. The currently selected stats are targeted
as a working demonstration of CSRT being able to highlight heavy usage changes
requests. CSRT has been designed to support extending out to all stats
collection and all resource usage inducing processes within CouchDB.

Grep for `'Example to extend CSRT'` to find the code points, eg:

> grep -ri 'Example to extend CSRT' src/
