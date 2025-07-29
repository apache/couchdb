.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

.. _csrt:

===================================
Couch Stats Resource Tracker (CSRT)
===================================

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
battle hardened introspection with Recon's excellent `proc_window`, but with
the sample window over any of the CSRT tracked CouchDB stats!

CSRT does this by piggy-backing off of the existing metrics tracked by way of
`couch_stats:increment_counter` at the time when the local process induces
those metrics inc calls, and then CSRT updates an ets entry containing the
context information for the local process, such that global aggregate queries
can be performed against the ets table as well as the generation of the process
resource usage reports at the conclusions of the process's lifecycle.The ability
to do aggregate querying in realtime in addition to the process lifecycle
reports for post facto analysis over time, is a cornerstone of CSRT that is the
result of a series of iterations until a robust and scalable approach was built.

The real time querying is achieved by way of a global ets table with
`read_concurrency`, `write_concurrency`, and `decentralized_counters` enabled.
Great care was taken to ensure that _zero_ concurrent writes to the same key
occur in this model, and this entire system is predicated on the fact that
incremental updates to `ets:update_counters` provides *really* fast and
efficient updates in an atomic and isolated fashion when coupled with
decentralized counters and write concurrency. Each process that calls
`couch_stats:increment_counter` tracks their local context in CSRT as well,
with zero concurrent writes from any other processes. Outside of the context
setup and teardown logic, _only_ operations to `ets:update_counter` are
performed, one per process invocation of `couch_stats:increment_counter`, and
one for coordinators to update worker deltas in a single batch, resulting in a
1:1 ratio of ets calls to real time stats updates for the primary workloads.

The primary achievement of CSRT is the core framework itself for concurrent
process local stats tracking and real time RPC delta accumulation in a scalable
manner that allows for real time aggregate querying and process lifecycle
reports. This took several versions to find a scalable and robust approach that
induced minimal impact on maximum system throughput. Now that the framework is
in place, it can be extended to track any further desired process local uses of
`couch_stats:increment_counter`. That said, the currently selected set of stats
to track was heavily influenced by the challenges in retroactively
understanding the quantity of resources induced by a query like
`/db/_changes?since=$SEQ`, or similarly, `/db/_find`.

CSRT started as an extension of the Mango execution stats logic to `_changes`
feeds to get proper visibility into quantity of docs read and filtered per
changes request, but then the focus inverted with the realization that we
should instead use the existing stats tracking mechanisms that have already
been deemed critical information to track, which then also allows for the real
time tracking and aggregate query capabilities. The Mango execution stats can
be ported into CSRT itself and just become one subset of the stats tracked as a
whole, and similarly, any additional desired stats tracking can be easily added
and will be picked up in the RPC deltas and process lifetime reports.

.. seealso::

    :doc:`/config/csrt`

CSRT Overview
-------------

    When an incoming HTTP request is is handled by the CouchDB `chttpd` worker
    pool, that worker process instantiates a CSRT `coordinator` context to
    track both the resources induced by local coordinator process, as well as
    to aggregate the resources induced by the remote RPC worker processes
    needed to fulfill the given request; each of those workers instantiates its
    own `rpc_worker` context to do local tracking and forward the deltas back
    to the coordinator.

    These contexts are represented internally by the `#rctx{}` record and are
    often referred to as "rctx"'s for short.

    For a singular doc open request, eg `GET /foo/bar`, the coordinator process
    will query from the normal N=3 shard replicas containing that doc. The
    coordinator process spawns 3 RPC workers on the relevant nodes in the
    cluster containing those shard replicas, each of which creates a local
    rctx. This is a total of four processes spawned to fulfill the get doc HTTP
    request, each of which has a valid local rctx that can generate a process
    lifecycle report to provide detailed node level information of RPC workers.

    We must decide what to log. A simple approach is by way of enabling the
    `all_coordinators` matcher, this will create a process lifecycle report for
    every HTTP request made against the CouchDB cluster, providing detailed
    statistics about the total quantities of CouchDB resources induced to
    fulfill the request. This creates a 1:1 mapping of additional CouchDB CSRT
    reports generated with HTTP requests, as only the coordinators are logged
    and no RPC workers.

    Logging a CSRT report for each HTTP request provides a chronological view
    of work induced, at the cluster level, that can then be analyzed with
    various tools to visualize and understand what the cluster was doing and
    why. At a simpler level, these reports can be summed together over a time
    period on the relevant fields, and then we can utilize calculus to take
    the the derivative and understand the largest spikes in workload over time,
    or the integral to understand the total quantities of resources induced
    over time, eg docs read per second or how many total docs were read in a
    given time period, respectively.

    However, that's exactly what a tool like Graphite or Prometheus connected
    to CouchDB's existing stats facilities would provide, so what does CSRT
    accomplish? Specifically, the current CouchDB stats are at the node level,
    meaning we can see how many documents a particular node loaded as a
    function of time, or we can be aggregate the sum over all the nodes at the
    cluster level, giving us the total docs read as we did above, but CSRT goes
    beyond the node level and provides:

        * Real time tracking of those same existing stats, but at the HTTP
          coordinator and RPC worker process level

        * Performant HTTP and internal query APIs for advanced aggregations and
          sorting on top of efficient ETS match specs, done in a constrained
          manner so that queries don't overload the system yet still provide
          meaningful results

        * Dynamic "Logger Matcher" system for on the fly enabling powerful
          filtering of CouchDB internal activity for real time querying or
          generating logs for HTTP requests with detailed statistics about
          their CouchDB resource usage

    The RPC worker stats are tracked in real time to have their workloads
    queryable in real time, but also because they need to funnel the statistics
    back to the coordinator request so that we can generate a report for the
    HTTP request as a whole. This brings us back to the question of what to
    log, for our `GET /foo/bar` example above, that spawned four processes to
    fulfill the request, each of which instantiates a local CSRT context to
    track its usage. We could log reports for all three workers, and the
    coordinator, which would give us four total reports generated, which is
    maybe alright, but it's 4x more log lines compared to the singular HTTP
    entry.

    Taking that a step further, if we perform a view query against a `Q=64`
    database, that will create a `coordinator` rctx, as well as `Q * N = 64 * 3
    = 196` total `rpc_worker` rctxs, although 2/3rds of those workers will die
    out after losing the race for the shard range, but if we logged all rctx
    reports for all processes tracked, the singular HTTP view query against a
    Q=64 database would generate 196 RPC worker reports and 1 coordinator
    report!

    To generate 197 rsyslog report log entries for a singular HTTP request is a
    significant increase over the singular HTTP log entry normally generated,
    *however*, a Javascript filtered changes request from `since=0` on a `Q=64`
    billion+ doc database will takes many billions of rows read, docs read, and
    IOQ calls to fulfill, at which point, the 197 induced reports, even with
    2/3rds as noops, are suddenly inconsequential compared to the raw CPU and
    network/disk IO induced to fulfill that request.

    CSRT itself creates the real time tracking system that allows us to track
    process level CouchDB metrics in HTTP coordinators and RPC workers in a
    real time queryable manner while also shipping the RPC deltas back to the
    coordinators for aggregating. The data itself is able to be collected with
    minimal impact and at high throughput, but the act of logging and querying
    in real time is what becomes expensive.

    The balance in CSRT is how do we query and save the usage data efficiently
    with minimal impact on throughput and performance of CouchDB while still
    allowing for meaningful insights.

    .. note::

        The core stats collection of CSRT is highly performant after having
        gone through a number of iterations and performance testing to find a
        viable approach. One of the key *experimental* aspects of CSRT is our
        ability to map Logger Matcher configurations from `default.ini` into
        CSRT itself and generate a corresponding `ets:match_spec()`.  If we had
        a way of declaring a Logger Matcher in the ini file by way of Mango
        specs that is then able to be translated into a compiled match_spec,
        then we eliminate the need for the default matchers and toggles for RPC
        reporting.

    The CSRT Logger Matchers are a first pass pragmatic approach for being able
    to easily map useful filtering declarations into logs and HTTP query API
    for constraining the volume of data returned. For example, enabling the
    default `ioq_calls` matcher with a threshold of 10000 IOQ calls with non
    RPC reporting enabled will result in generating a CSRT lifecycle report for
    that HTTP request, but all of the enabled matchers are are exposed in the
    HTTP API as well, so you can query against the `ioq_calls` matcher and
    perform aggregations on top of those results, grouping by database name for
    instance, but the aggregations happen *on top* of the `ets:match_spec()`
    filtered rows, so the initial query filtering is performed as close to the
    internal ETS data storage as possible, and we can focus on workloads that
    are already established as significant, and *only* aggregate on top of
    those significant workloads. This is absolutely essential for being able to
    perform these types of queries and aggregations at scale with minimal
    impact on CouchDB.

    Furthermore, the heavy RPC workers that would normally be skipped in the
    logs, are still queryable through the HTTP API and query API, so they can
    still be introspected to diagnose ongoing cluster issues, without having to
    modify logging settings.

An example of the hidden data CSRT exposes
------------------------------------------

    Let's take a look at a more concrete example demonstrating where CSRT
    fundamentally changes our visibility into the internal activities of
    CouchDB. The following screenshot is from a benchmark against a real world
    cluster, but let's say this view is roughly keyed into a real workload's
    daily cyclic run, and we're trying to understand what CouchDB is doing and
    why, for the given workload. So here we have some time series graphs built
    from querying `GET /_node/_local/_system` and aggregated across the
    cluster, showing HTTP throughput, CouchDB operations, IOQ throughput,
    Erlang memory, and Erlang process count:

    .. figure:: ../../images/csrt-sample-workload.png
      :align: center
      :alt: Sample high throughput workload

    We can see a steady increase in processes over time, and a similar but
    logarithmic-ish increase in the CouchDB operations as well as in the IOQ
    calls, however, the HTTP throughput actually goes *down*, and we'd like to
    understand why that's happening. The benchmark above was gradually
    saturating CPU as a function of time, with near full CPU saturation at the
    end of the run, and the Erlang process count we know is at least largely
    relative to the normal cyclic workload of increase concurrency coming in,
    so as we run out of available CPU while also increasing the number of
    concurrent requests, those requests naturally get slower.

    That said, we can see this cluster tops out at a sustained one million IOQ
    operations per second, to fulfill maybe 1000 requests per second, which is
    a 1000x increase in IOQ calls relative to HTTP requests, a pretty
    substantial difference. The next question is obviously, well, what's
    actually inducing all of those operations, what HTTP requests are actually
    causing 1 million IOQ ops per second and 300k doc reads per second? Are
    these slow or problematic queries? If not, are we just hitting cluster
    capacity and need to expand? These types of questions are very difficult to
    make concrete conclusions upon with the existing data, and often isolated
    experiments and profiling are required to even begin to track down the
    heavy usage requests, especially when those heavy requests are a needle in
    a haystack. The above metrics visualization does a great job of informing
    us the system is *very* busy, but it's difficult to understand why. Even
    the CouchDB operations doc reads stats are difficult to correlate, as doc
    reads could be happening from views or all docs or changes or background
    operations, further clouding the connection of these metrics to the HTTP
    requests that induced them.

    This is where you would enable CSRT with the `ioq_calls` matcher, and maybe
    the `docs_read` matcher, allowing you to query live and track down what
    requests are generating all of that load. Or better yet, if you'd already
    enabled CSRT and reporting on the default Logger Matchers, there'd be a
    logged report for each of the heavy requests using more than 10000 IOQ
    calls or 1000 docs read.  Perhaps only a few requests are hitting the 10000
    IOQ calls metric, so you'd like to lower the Threshold to 5000 IOQ calls, or
    maybe that wasn't sufficient so you dropped it down further to 1000. These
    Logger Matchers can be enabled dynamically as well as their Thresholds
    configured dynamically, and the CSRT logger will pickup on those changes
    and reload the matchers immediately, live, such that those new Thresholds
    apply to the process lifecycle logging reports as well as querying the
    `_active_resources` against the `ioq_calls` matcher.

    .. seealso::

        :config:option:`Enable CSRT here <csrt/enable>`

        :config:option:`Enable CSRT reporting here <csrt/enable_reporting>`

        :config:option:`Enable ioq_calls matcher here <csrt_logger.matchers_enabled/ioq_calls>`

        :config:option:`Set ioq_calls matcher Threshold here <csrt_logger.matchers_threshold/ioq_calls>`

    Once the Logger Matchers are enabled, reports can be generated automatically,
    and advanced query aggregations become available. In our heavy example cyclic
    workload above, the benchmark was a rampup view query run on a Q=64 database
    with `?group=false&include_docs=true` and no limit, specifically to do a full
    index database scan loading every single doc in the process, and then we spawn
    another HTTP worker performing those view requests every second, progressively
    overloading the system as you can see in the Erlang process count metrics. In
    the above workload case, it was fairly uniform in that the workload wasn't
    skewed by outliers, rather by a large parallel quantities of full database
    scans by way of view indexes.

Another example with csrt proc window
-------------------------------------

    Now let us continue with another example, this time demonstrating the use of
    the `csrt:proc_window/3` in a `remsh`, as one would do with `recon:proc_window/3`
    to get an idea of heavy active processes in the system. Normally one would
    run something like `recon:proc_window(reductions, 5, 5000).` to list the
    top 5 most active processes over the next five seconds, sorted by delta on
    the reductions count of that process. Essentially `recon:proc_window` takes
    a snapshot of the system at `T0` for the data you requested, waits 5000
    milliseconds, fetches a snapshot of the system at `T1`, then it performs a
    delta on T1 and T0, sorting and returning the top 5 results. Recon does
    this by way of a heavily optimized data structure allowing for minimal
    memory consumption of high Erlang process systems and efficient deltas.

    The `csrt:proc_window/3` functionality piggy backs off of
    `recon:proc_window`, and utilizes the same core data structures and delta
    sorting logic, but instead exposing sampling on `erlang:process_info/2`
    statistics, `csrt:proc_window` exposes the same logic on the CouchDB
    internal CSRT metrics, like `docs_read`, `ioq_calls`, `js_filter`, etc.

    .. note::

       The `csrt:proc_window/3` functionality is demonstrated in a `remsh` as it's
       not currently exposed by way of the HTTP API, but can now easily be built on
       the field extraction logic in `couch_srt_query` powering the HTTP API. This
       can be added readily, as it should map over well enough to the HTTP API.

    Now, given a database `foo` with 11k documents containing a `doc.value` field
    that is an integer value which can be filtered in a design doc by way of
    even and odd. If we instantiate a series of while loops in parallel making
    requests of the form::

        GET /foo/_changes?filter=bar/even&include_docs=true

    We can generate a good chunk of load on a local laptop dev setup, resulting
    in requests that take a few seconds to load through the changes feed, fetch
    all 11k docs, and then funnel them through the Javascript engine to filter
    for even valued docs; this allows us time to query these heavier requests
    live and see them in progress with the real time stats tracking and
    querying capabilities of CSRT.

    For example, let's use `couch_srt:proc_window/3` as one would do with
    `recon:proc_window/3` to get an idea of the heavy active processes on the
    system::

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

    This shows us the top 3 most active processes (being tracked in CSRT) over
    the next 1000 milliseconds, sorted by number of `ioq_calls` induced! All of
    three of these processes are incurring heavy usage, reading many thousands
    of docs with 15k+ IOQ calls and heavy JS filter usage, exactly the types of
    requests you want to be alerted to. CSRT's proc window logic is built on
    top of Recon's, which doesn't return the process info itself, so you'll
    need to fetch the process status with `couch_srt:get_resource/1` and then
    pretty print it with `couch_srt:to_json/1`.

    The output above is a real time snapshot of the live running system and
    shows processes actively inducing additional resource usage, so these CSRT
    context values are just a time snapshot of where that process was at, as of
    the `updated_at` timestamp. We can reference the nonce value to search
    through the report logs for a final report, assuming the given context
    ended up using sufficient resources to trigger a logger matcher lifetime
    report. The above changes requests were induced specifically to induce
    reports as well, so unsurprisingly we have reports for all three.

    However, I want to first show the existing visibility into these changes
    requests exposed by the raw HTTP logs to highlight the impact of the CSRT
    reports and new visibility into request workloads exposed.

    First, let's look at the existing HTTP logs for those 3 requests::

        (chewbranca)-(jobs:1)-(~/src/couchdb_csrt_v3)
        (! 9872)-> grep 'cc5a814ceb\|0e625c723a\|962cda1645' ./dev/logs/node1.log | grep -v '^\[report]'
        [notice] 2025-07-21T17:25:14.520641Z node1@127.0.0.1 <0.5087.0> 0e625c723a localhost:15984 127.0.0.1 adm GET /foo/_changes?filter=bar/even&asdf=fdsa&include_docs=true 200 ok 6096
        [notice] 2025-07-21T17:25:14.521417Z node1@127.0.0.1 <0.5086.0> 962cda1645 localhost:15984 127.0.0.1 adm GET /foo/_changes?filter=bar/even&asdf=fdsa&include_docs=true 200 ok 6115
        [notice] 2025-07-21T17:25:14.844317Z node1@127.0.0.1 <0.5090.0> cc5a814ceb localhost:15984 127.0.0.1 adm GET /foo/_changes?filter=bar/even&asdf=fdsa&include_docs=true 200 ok 6059

    So we see the requests were made, and we can see it's doing
    `include_docs=true` as well as using a customer filter, both obvious
    indications that this is a potentially heavier request, however, we don't
    know if database `foo` had a thousand docs or a billion docs, whether those
    docs were small or large, nor any indication of the computational
    complexity of the reference filter function.  This makes it challenging to
    retroactively correlate heavy resource usage at a hardware level with the
    underlying requests that induced those workloads, especially if the heavy
    requests are an inconspicuous subset of the full database workload.

    CSRT resolves this by providing a real time querying system to find the
    active heavy processes, live, as well as a process lifecycle reporting engine
    providing detailed analysis of the workloads induced by the request.

    Let's assume we had the default IOQ logger matcher enabled, with the
    default configuration of logging any requests inducing more than 10k IOQ
    calls, which would catch all three of our requests above, even though
    they're all still going. As a result, we generate process lifecycle reports
    for all three of those requests, as we can see::

        (chewbranca)-(jobs:1)-(~/src/couchdb_csrt_v3)
        (! 9873)-> grep 'cc5a814ceb\|0e625c723a\|962cda1645' ./dev/logs/node1.log | grep '^\[report]'
        [report] 2025-07-21T17:25:14.520787Z node1@127.0.0.1 <0.5174.0> -------- [csrt-pid-usage-lifetime changes_returned=5500 db_open=10 dbname="foo" docs_read=11001 get_kp_node=72 get_kv_node=1754 ioq_calls=22002 js_filter=11001 js_filtered_docs=11001 nonce="0e625c723a" pid_ref="<0.5087.0>:#Ref<0.2277656623.606601217.92191>" rows_read=11001 started_at="2025-07-21T17:25:08.424z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-21T17:25:14.520z" username="adm"]
        [report] 2025-07-21T17:25:14.521578Z node1@127.0.0.1 <0.5155.0> -------- [csrt-pid-usage-lifetime changes_returned=5500 db_open=10 dbname="foo" docs_read=11001 get_kp_node=72 get_kv_node=1754 ioq_calls=22002 js_filter=11001 js_filtered_docs=11001 nonce="962cda1645" pid_ref="<0.5086.0>:#Ref<0.2277656623.605290499.27728>" rows_read=11001 started_at="2025-07-21T17:25:08.406z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-21T17:25:14.521z" username="adm"]
        [report] 2025-07-21T17:25:14.844436Z node1@127.0.0.1 <0.5213.0> -------- [csrt-pid-usage-lifetime changes_returned=5500 db_open=10 dbname="foo" docs_read=11001 get_kp_node=72 get_kv_node=1754 ioq_calls=22002 js_filter=11001 js_filtered_docs=11001 nonce="cc5a814ceb" pid_ref="<0.5090.0>:#Ref<0.2277656623.605290499.37969>" rows_read=11001 started_at="2025-07-21T17:25:08.784z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-21T17:25:14.844z" username="adm"]

    We find the process lifecycle reports for the requests with the three
    grep'ed on nonces, and we can see they all read the 11k core documents,
    plus the one design document, JS filtered all 11,001 docs, and then only
    returned the 5500 doc's containing an even `doc.value` field.

    This also shows the discrepancy between the quantity of induced resource
    usage to actually generate a request, relative to the magnitude of the data
    returned.  All of our `doc.value` fields were positive integers, if we had
    a filter function searching for negative `doc.value` results, we would have
    found none, resulting in `changes_returned=0`, but we would have still
    induced the 11,001 doc loads and Javascript filter calls.

    CSRT is specifically built to automatically find and report these types of
    workload discrepancies and in general to help highlight where individual
    HTTP requests use drastically more resources than the median workloads.

Demonstration of expressiveness constraints in Logger Matchers and ini settings
-------------------------------------------------------------------------------

    Again, we run into expressiveness issues with default.ini mapping to
    `ets:match_spec()`, ideally we could just directly declare something like::

        [csrt_loggers]
        heavy_rpc_workers = ets:fun2ms(#rctx{type=#rpc_worker{}, ioq_calls=IC}=R) when IC > 1234 -> R end).
        heavy_coordinators = ets:fun2ms(#rctx{type=#coordinator{}, ioq_calls=IC}=R) when IC > 1234 -> R end).
        heavy_changes = ets:fun2ms(#rctx{type=#coordinator{mod='chttp_db', func='handle_changes_req'}, docs_read=DR}=R) when DR > 123456 -> R end).
        debug_foo_db = ets:fun2ms(fun(#rctx{type=#coordinator{}, dbname=(<<"foo">>)}) -> R end).
        debug_foo_db = ets:fun2ms(fun(#rctx{type=#coordinator{}, dbname=(<<"foo">>)}=R) -> R end)).
        debug_foo_db_shard_changes = ets:fun2ms(fun(#rctx{type=#rpc_worker{func=changes}, dbname=(<<"shards/00000000-7fffffff/foo.1753691445">>)}=R) -> R end)).

    Once we can express and persist Logger Matchers directly like that in the
    `ini` files, we'll no longer need the default matchers, as we'll be able to
    express any filter functions directly, on coordinators or RPC workers or a
    combination of both. Furthermore, once we can transform the static
    default.ini definitions of that form, we'll be able to do the same with the
    query interface, and we can `POST` those complex queries in and have a
    match spec dynamically generated and run directly against the ETS table.

    Now to highlight that this is _specifically_ an expressiveness problem,
    let's demonstrate how to actually register those matchers above dynamically
    by way of a `remsh`, and then see the report generation changes directly::

        (node1@127.0.0.1)35> rr(couch_srt_logger).
        [coordinator,rctx,rpc_worker,st]
        (node1@127.0.0.1)36> couch_srt_logger:register_matcher("debug_foo_db", ets:fun2ms(fun(#rctx{type=#coordinator{}, dbname=(<<"foo">>)}=R) -> R end)).
        ok

    In the logs we see the automatic Logger Matcher reload notice::

        [notice] 2025-07-28T08:35:41.576259Z node1@127.0.0.1 <0.251.0> -------- Initialized 3 CSRT Logger matchers

    And now, if we make an HTTP request to database `foo`, we'll automatically
    generate a CSRT process lifecycle report log for that request, without
    inducing additional report logging for requests to databases other than
    `foo`, for example, given `GET /foo`, we now get the following HTTP related
    logs::

        [notice] 2025-07-28T08:38:15.638529Z node1@127.0.0.1 <0.2114371.0> 203629c3b4 localhost:15984 127.0.0.1 adm GET /foo 200 ok 3
        [report] 2025-07-28T08:38:15.638659Z node1@127.0.0.1 <0.2114423.0> -------- [csrt-pid-usage-lifetime db_open=6 dbname="foo" nonce="203629c3b4" pid_ref="<0.2114371.0>:#Ref<0.3800414810.3105882114.258360>" started_at="2025-07-28T08:38:15.636z" type="coordinator-{chttpd_db:handle_request}:GET:/foo" updated_at="2025-07-28T08:38:15.638z" username="adm"]

    We can also create our `debug_foo_db_shard_changes` Logger Matcher declared
    above, but note that the RPC workers operate on local database shard names,
    not the higher level clustered database names from the coordinator's
    perspective. To match against specific database names in RPC workers, we'll
    need to match against the full shard name (eg
    `<<"shards/00000000-7fffffff/foo.1753691445">>` instead of `<<"foo">>`), as
    in our example above, like so::

        (node1@127.0.0.1)44> rr(couch_srt_logger).
        [coordinator,rctx,rpc_worker,st]
        (node1@127.0.0.1)45> couch_srt_logger:register_matcher("debug_foo_db_shard_changes", ets:fun2ms(fun(#rctx{type=#rpc_worker{func=changes}, dbname=(<<"shards/00000000-7fffffff/foo.1753691445">>)}=R) -> R end)).
        ok

    As before we get the loggers re-initialize message, but we don't see the
    RPC worker, only the top level coordinator report from our "debug_foo_db"
    Logger Matcher, what happened?::

        [notice] 2025-07-28T08:45:06.305788Z node1@127.0.0.1 <0.251.0> -------- Initialized 4 CSRT Logger matchers
        [notice] 2025-07-28T08:45:08.106879Z node1@127.0.0.1 <0.2124751.0> eff915deb7 localhost:15984 127.0.0.1 adm GET /foo/_changes 200 ok 110
        [report] 2025-07-28T08:45:08.106957Z node1@127.0.0.1 <0.2124806.0> -------- [csrt-pid-usage-lifetime changes_returned=6228 db_open=8 dbname="foo" get_kp_node=42 get_kv_node=1003 nonce="eff915deb7" pid_ref="<0.2124751.0>:#Ref<0.3800414810.3105882116.229072>" rows_read=6228 started_at="2025-07-28T08:45:07.997z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-28T08:45:08.106z" username="adm"]

    What happened is that Logger Matchers on `rpc_worker` rctxs are queryable
    but will not generate a report log unless the specific
    :config:option:`csrt/enable_rpc_reporting` setting is enabled! After doing
    so, we see the config set notice, followed by a report for the rpc worker
    on shard range OO-7F for db `foo`, as expected. Note that we get the RPC
    report before the HTTP log, as the worker completed before the coordinator
    that needed that worker to complete, completed, and similarly, the
    coordinator worker process logs the HTTP line prior to the CSRT coordinator
    context being cleaned up::

        [notice] 2025-07-28T08:45:44.584938Z node1@127.0.0.1 <0.146.0> -------- config: [csrt] enable_rpc_reporting set to true for reason nil
        [report] 2025-07-28T08:45:47.852469Z node1@127.0.0.1 <0.2125885.0> -------- [csrt-pid-usage-lifetime  changes_returned=3122 db_open=1 dbname="shards/00000000-7fffffff/foo.1753691445" get_kp_node=21 get_kv_nodat="2025-07-28T08:45:47.731z" type="rpc_worker-{<0.2125822.0>:#Ref<0.3800414810.3105882116.240929>}:fabric_rpc:changes" updated_at="2025-07-28T08:45:47.852z"]
        [notice] 2025-07-28T08:45:47.852524Z node1@127.0.0.1 <0.2125822.0> 7ea9ca7743 localhost:15984 127.0.0.1 adm GET /foo/_changes 200 ok 122
        [report] 2025-07-28T08:45:47.852602Z node1@127.0.0.1 <0.2125871.0> -------- [csrt-pid-usage-lifetime changes_returned=6228 db_open=11 dbname="foo" get_kp_node=42 get_kv_node=1003 nonce="7ea9ca7743" pid_ref="<0.2125822.0>:#Ref<0.3800414810.3105882116.240904>" rows_read=6228 started_at="2025-07-28T08:45:47.730z" type="coordinator-{chttpd_db:handle_changes_req}:GET:/foo/_changes" updated_at="2025-07-28T08:45:47.852z" username="adm"]

    .. note::

        It seems like that some creative pattern matches nestable within match
        specs are possible, perhaps something like
        `ets:fun2ms(fun(#rctx{dbname=<<"shards/", Range/17, "/foo", Timestamp/binary">>}) -> {Range, R} end).`
        allowing for matching on all `foo` db workers, and demoing extracting
        out the Range for run.
