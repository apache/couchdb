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

.. default-domain:: config
.. highlight:: ini

.. _config-csrt:

==========================================
Couch Stats Resource Tracker (CSRT) Config
==========================================

CSRT configuration options and overview.

.. seealso::

    :doc:`/csrt/index`

CSRT config
===========

This section contains the top level enablement and configuration options for CSRT.

.. config:section:: csrt :: CSRT Primary Configuration

    .. config:option:: enable :: Enable CSRT data collection and RPC deltas

        Core enablement toggle for CSRT, defaults to false. Enabling this
        setting initiates local CSRT stats collection as well as shipping deltas
        in RPC responses to accumulate in the coordinator.

        This does *not* trigger the new RPC spawn metrics, and it does not
        enable reporting for any of the rctx types.

        .. warning::

            You *MUST* have all nodes in the cluster running a CSRT aware
            CouchDB *before* you enable it on any node, otherwise the old
            version nodes won't know how to handle the new RPC formats
            including an embedded Delta payload.

        Top level CSRT enablement for local data collection and RPC deltas::

            [csrt]
            enable = false

    .. config:option:: enable_init_p :: Enable RPC spawn metric tracking

        Enablement of tracking new metric counters for different `fabric_rpc` operations
        spawned by way of `rexi_server:init_p/3`. This is the primary mechanism for
        inducing database RPC operations within CouchDB, and these init_p metrics aim to
        provide node level understandings of the workloads being induced by other
        coordinator processes. This is especially relevant for databases on subsets of a
        cluster resulting in non-uniform workloads, these metrics are tailored to
        provide insight into what work is being spawned on each node in the cluster as a
        function of time.

        Enablement for tracking counts of spawned RPC workers::

            [csrt]
            enable_init_p = false

    .. config:option:: enable_reporting :: Enable CSRT Process Lifecyle Reports

        This is the primary toggle for enabling CSRT process lifetime reports
        containing detailed information about the quantity of work induced by
        the given request/worker/etc. This is the top level toggle for enabling
        _any_ reporting, and there also exists
        :config:option:`csrt/enable_rpc_reporting` to disable the reporting of
        any individual RPC workers, leaving the coordinator responsible of
        generating a report with the accumulated deltas.

        .. note::

            Note that this setting toggles whether or not to generate process
            lifecycle reports, but no reports will be generated until logger
            matchers have been enabled that trigger a match on CSRT contexts
            that have surpassed the configured thresholds.

        Top level toggle for whether any process lifecycle reports are generated::

            [csrt]
            enable_reporting = false

    .. config:option:: enable_rpc_reporting :: Enable RPC process lifecyle reports

        This enables the possibility of RPC workers generating reports. They
        still need to hit the configured thresholds to induce a report, but
        this will generate CSRT process lifetime reports for individual RPC
        workers that trigger the configured logger thresholds. This allows for
        quantifying per node resource usage when desired, as otherwise the
        reports are at the http request level and don't provide per node stats.

        The key idea here is that having RPC level CSRT process lifetime
        reporting is incredibly useful, but can also generate large quantities
        of data. For example, a view query on a Q=64 database will stream
        results from 64 shard replicas, resulting in at least 64 RPC reports,
        plus any that might have been generated from RPC workers that "lost"
        the race for shard replica. This is very useful, but a lot of data
        given the verbose nature of funneling it through the RSyslog reports,
        however, the ability to write directly to something like ClickHouse or
        another columnar store would be great.

        Until there's an efficient storage mechanism to stream the results to,
        the rsyslog entries work great and are very practical, but care must be
        taken to not generate too much data for aggregate queries as they
        generate at least `Qx` more report than an individual report per http
        request from the coordinator.  This setting exists as a way to either
        a) utilize the logger matcher configured thresholds to allow for _any_
        rctx's to be recorded when they induce heavy operations, either
        Coordinator or RPC worker; or b) to _only_ log workloads at the
        coordinator level.

        .. note::

            This setting exists because we lack an expressive enough config
            declaration to easily chain the matchspec constructions as
            `ets:fun2ms/1` is a special compile time parse transform macro that
            requires the full definition to be specified directly, it cannot
            be interactively constructed. That said, you _can_ register matchers
            through `remsh` with more specific and fine grained pattern matching,
            and a more expressive system for defining matchers are being
            explored.

        .. warning::

            Enabling this setting *will* generate considerably more logs! Specifically, for aggregate queries and database operations, this will generate `Q` * `N` times more logs than a singular doc request taking only `N` inreacting with a singular shard range. See the note above about this being a temporary setting during the experimental stages of CSRT.

        Toggle to enable possibility of RPC process lifecycle reports::

            [csrt]
            enable_rpc_reporting = false

    .. config:option:: should_truncate_reports :: truncate zero values from lifecyle reports

        enables truncation of the csrt process lifetime reports to not include
        any fields that are zero at the end of process lifetime, eg don't
        include `js_filter=0` in the report if the request did not induce
        javascript filtering.

        this can be disabled if you really care about consistent fields in the
        report logs, but this is a log space saving mechanism, similar to
        disabling rpc reporting by default, as its a simple way to reduce
        overall volume

        Truncate zero values from process lifecycle reports, enabled by default:

            [csrt]
            should_truncate_reports = true

    .. config:option:: query_limit :: Maximum quantity of rows to return in CSRT query/http requests.

        Limit the quantity of rows that can be loaded in an http query.::

            [csrt]
            query_limit = 100

    .. config:option:: query_cardinality_limit :: Maximum quantity of rows to allow in CSRT query/http requests.

        Limit the quantity of rows that can be loaded in an http query.::

            [csrt]
            query_cardinality_limit = 10000

.. _csrt-logger-matcher-configuration:

CSRT Logger Matcher Configuration
=================================

There are currently eight builtin default logger matchers designed to make it
easy to do filtering on heavy resource usage inducing and long running
requests. These are designed as a simple baseline of useful matchers, declared
in a manner amenable to `default.ini` based constructs. More expressive matcher
declarations are being explored, and matchers of arbitrary complexity can be
registered directly through `remsh`. The default matchers are all designed around
an integer config Threshold that triggers on a specific field, eg docs read, or
on a delta of fields for long requests and changes requests that process many
rows but return few.

The current default matchers are:

  * `all_coordinators`: match all Coordinators handling HTTP requests

    * :config:option:`Enable <csrt_logger.matchers_enabled/all_coordinators>` | none

  * `all_rpc_workers`: match all RPC Worker handling internal requests

    * :config:option:`Enable <csrt_logger.matchers_enabled/all_rpc_workers>` | none

  * `docs_read`: match all requests reading more than N docs

    * :config:option:`Enable <csrt_logger.matchers_enabled/docs_read>` | :config:option:`Threshold <csrt_logger.matchers_threshold/docs_read>`

  * `rows_read`: match all requests reading more than N rows

    * :config:option:`Enable <csrt_logger.matchers_enabled/rows_read>` | :config:option:`Threshold <csrt_logger.matchers_threshold/rows_read>`

  * `docs_written`: match all requests writing more than N docs

    * :config:option:`Enable <csrt_logger.matchers_enabled/docs_written>` | :config:option:`Threshold <csrt_logger.matchers_threshold/docs_written>`

  * `ioq_calls`: match all requests inducing more than N ioq_calls

    * :config:option:`Enable <csrt_logger.matchers_enabled/ioq_calls>` | :config:option:`Threshold <csrt_logger.matchers_threshold/ioq_calls>`

  * `long_reqs`: match all requests lasting more than N milliseconds

    * :config:option:`Enable <csrt_logger.matchers_enabled/long_reqs>` | :config:option:`Threshold <csrt_logger.matchers_threshold/long_reqs>`

  * `changes_processed`: match all changes requests that returned at least N rows
    less than was necessarily loaded to complete the request (eg find heavy
    filtered changes requests reading many rows but returning few).

    * :config:option:`Enable <csrt_logger.matchers_enabled/changes_processed>` | :config:option:`Threshold <csrt_logger.matchers_threshold/changes_processed>`

Each of the default matchers has an enablement setting in
ref:`csrt-logger-matcher-configuration-enablement` for toggling enablement of
it, and all but the `all_coordinators` and `all_rpc_workers` matchers have a
corresponding threshold value setting in
:ref:`csrt-logger-matcher-configuration-threshold` that is an integer value
corresponding to the specific nature of that matcher.

.. seealso::

    :ref:`csrt-logger-matcher-configuration-enablement`

    :ref:`csrt-logger-matcher-configuration-threshold`

.. _csrt-logger-matcher-configuration-enablement:

CSRT Logger Matcher Enablement Configuration
--------------------------------------------

   These settings enable the default logger matchers, any can be enabled
   independently of each other, but none will generate reports unless the
   :config:option:`csrt/enable` and :config:option:`csrt/enable_reporting`
   settings are both true.

.. seealso::

    :ref:`csrt-logger-matcher-configuration`

    :ref:`csrt-logger-matcher-configuration-threshold`

.. config:section:: csrt_logger.matchers_enabled :: CSRT Logger Matcher Enablement

    .. config:option:: all_coordinators :: Enable all_coordinators CSRT Logger Matcher

        Enable the `all_coordinators` default matcher to match against all
        coordinators handling HTTP requests.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            all_coordinators = false

    .. config:option:: all_rpc_workers :: Enable all_rpc_workers default CSRT Logger Matcher

        Enable the `all_rpc_workers` default matcher to match against all
        RPC Workers handling internal CouchDB requests. This is predominantly
        induced by HTTP requests, but any internal systems flowing through
        `fabric_rpc` will be picked up as well, such as internal/external
        replication and anything that needs to load a document through the
        quorum system.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            all_rpc_workers = false

    .. config:option:: docs_read :: Enable docs_read default CSRT Logger Matcher

        Enable the `docs_read` builtin matcher, with a default
        `Threshold=1000`, such that any request that reads more than
        `Threshold` docs will generate a CSRT process lifetime report with a
        summary of its resource consumption.

        This is different from the `rows_read` filter in that a view with
        `?limit=1000` will read 1000 rows, but the same request with
        `?include_docs=true` will also induce an additional 1000 docs read.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            docs_read = false

        .. seealso::
            :config:option:`Set docs_read matcher Threshold <csrt_logger.matchers_threshold/docs_read>`

    .. config:option:: rows_read :: Enable rows_read default CSRT Logger Matcher

        Enable the `rows_read` builtin matcher, with a default
        `Threshold=1000`, such that any request that reads more than
        `Threshold` rows will generate a CSRT process lifetime report with a
        summary of its resource consumption.

        This is different from the `docs_read` filter so that we can
        distinguish between heavy view requests with lots of rows or heavy
        requests with lots of docs.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            rows_read = false

        .. seealso::
            :config:option:`Set rows_read matcher Threshold <csrt_logger.matchers_threshold/rows_read>`

    .. config:option:: docs_written :: Enable docs_written default CSRT Logger Matcher

        Enable the `docs_written` builtin matcher, with a default
        `Threshold=500`, such that any request that written more than
        `Threshold` docs will generate a CSRT process lifetime report with a
        summary of its resource consumption.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            docs_written = false

        .. seealso::
            :config:option:`Set docs_written matcher Threshold <csrt_logger.matchers_threshold/docs_written>`

    .. config:option:: ioq_calls :: Enable ioq_calls default CSRT Logger Matcher

        Enable the `ioq_calls` builtin matcher, with a default
        `Threshold=10000`, such that any request that induces more than
        `Threshold` IOQ calls will generate a CSRT process lifetime report with
        a summary of its resource consumption.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            ioq_calls = false

        .. seealso::
            :config:option:`Set ioq_calls matcher Threshold <csrt_logger.matchers_threshold/ioq_calls>`

    .. config:option:: long_reqs :: Enable long_reqs default CSRT Logger Matcher

        Enable the `long_reqs` builtin matcher, with a default
        `Threshold=60000`, such that any request where the the last CSRT rctx
        `updated_at` timestamp is at least `Threshold` milliseconds greater
        than the `started_at timestamp` will generate a CSRT process lifetime
        report with a summary of its resource consumption.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            long_reqs = false

        .. seealso::
            :config:option:`Set long_reqs matcher Threshold <csrt_logger.matchers_threshold/long_reqs>`

    .. config:option:: changes_processed :: Enable changes_processed default CSRT Logger Matcher

        Enable the `changes_processed` builtin matcher, with a default
        `Threshold=1000`, such that any request where the CSRT rctx `rows_read`
        field as at least `Threshold` greater than the rctx `changes_returned`
        field will generate a CSRT process lifetime report with a summary of
        its resource consumption.

        Enable the matcher::

            [csrt_logger.matchers_enabled]
            changes_processed = false

        .. seealso::
            :config:option:`Set changes_processed matcher Threshold <csrt_logger.matchers_threshold/changes_processed>`

.. _csrt-logger-matcher-configuration-threshold:

CSRT Logger Matcher Threshold Configuration
-------------------------------------------

   These settings control the Threshold configurations for the default
   matchers. These are scalar integer values that are used by all default
   matchers aside from `all_coordinators` and `all_rpc_workers`. See the top
   level config for more information and the enablement config for how to
   enable these matchers.

.. seealso::

    :ref:`csrt-logger-matcher-configuration`

    :ref:`csrt-logger-matcher-configuration-enablement`

.. config:section:: csrt_logger.matchers_threshold :: CSRT Logger Matcher Threshold

    .. config:option:: docs_read :: Set Threshold for docs_read CSRT Logger Matcher

        Threshold for `docs_read` logger matcher, defaults to `1000` docs read.

        Set the Threshold::

            [csrt_logger.matchers_threshold]
            docs_read = 1000

        .. seealso::
            :config:option:`Enable docs_read matcher <csrt_logger.matchers_enabled/docs_read>`

    .. config:option:: rows_read :: Set Threshold for rows_read default CSRT Logger Matcher

        Threshold for `rows_read` logger matcher, defaults to `1000` rows read.

        Set the Threshold::

            [csrt_logger.matchers_threshold]
            rows_read = 1000

        .. seealso::
            :config:option:`Enable rows_read matcher <csrt_logger.matchers_enabled/rows_read>`

    .. config:option:: docs_written :: Set Threshold for docs_written default CSRT Logger Matcher

        Threshold for `docs_written` logger matcher, defaults to `500` docs written.

        Set the Threshold::

            [csrt_logger.matchers_threshold]
            docs_written = 500

        .. seealso::
            :config:option:`Enable docs_written matcher <csrt_logger.matchers_enabled/docs_written>`

    .. config:option:: ioq_calls :: Set Threshold for ioq_calls default CSRT Logger Matcher

        Threshold for `ioq_calls` logger matcher, defaults to `10000` IOQ calls.

        Set the Threshold::

            [csrt_logger.matchers_threshold]
            ioq_calls = 10000

        .. seealso::
            :config:option:`Enable ioq_calls matcher <csrt_logger.matchers_enabled/ioq_calls>`

    .. config:option:: long_reqs :: Set Threshold for long_reqs default CSRT Logger Matcher

        Threshold for `long_reqs` logger matcher, defaults to `60000` milliseconds (1 minute).

        Set the Threshold::

            [csrt_logger.matchers_threshold]
            long_reqs = 60000

        .. seealso::
            :config:option:`Enable long_reqs matcher <csrt_logger.matchers_enabled/long_reqs>`

    .. config:option:: changes_processed :: Set Threshold for changes_processed default CSRT Logger Matcher

        Threshold for `changes_processed` logger matcher, defaults to `1000` changes processed.

        Set the Threshold::

            [csrt_logger.matchers_threshold]
            changes_processed = 1000

        .. seealso::
            :config:option:`Enable changes_processed matcher <csrt_logger.matchers_enabled/changes_processed>`

Recommendations
===============

CSRT is still experimental, and the Matcher Logger declaration syntax limits
the types of filtering and queries available, so caution should be exercised
when enabling any RPC reporting, and unless you have a specific need, you can
leave RPC reporting disabled.

Simple Approach: Log a CSRT report for all coordinators
-------------------------------------------------------

To enable process life cycle reporting for all HTTP requests, enable the `all_coordinators` matcher by way of :config:option:`csrt_logger.matchers_enabled/all_coordinators`, in addition to the normal CSRT enablements for tracking and reporting. This will result in a 1:1 mapping of HTTP requests to CSRT report logs, connected by the request `nonce`. For example::

    [csrt]
    enable = true
    enable_init_p = true
    enable_reporting = true

    [csrt_logger.matchers_enabled]
    all_coordinators = true

Custom Logger Matcher filtering
-------------------------------

The default loggers and logger thresholds have been configured to make it easy
to find significant requests that induce heavy resource usage, without
drastically increasing the data log volume. For example, logging only when
requests take more than a minute or induce more than 10000 IOQ calls are
examples of scenarios where you most likely want to be informed about those
significant requests as they're well outside of normal efficient database
queries, while constraining total log volume for non-significant requests. The
default logger matcher Thresholds provide a simple way to set high level
watermarks to automatically generate logged reports for further analysis.

Here's a recommended Sample configuration to enable the threshold based default
matchers, and utilize their default config values::

    [csrt]
    enable = true
    enable_init_p = true
    enable_reporting = true

    [csrt_logger.matchers_enabled]
    docs_read = true
    rows_read = true
    docs_written = true
    long_reqs = true
    changes_processed = true
    ioq_calls = true

That will enable CSRT stats collection and the new RPC stats, CSRT reporting, and the default Logger Matchers, although these can be individually enabled as well. Those Logger Matchers Threshold values can be configured, as follows, with the commented out defaults shown::

    [csrt_logger.matchers_threshold]
    ;docs_read = 1000
    ;rows_read = 1000
    ;docs_written = 500
    ;long_reqs = 60000
    ;changes_processed = 1000
    ;ioq_calls = 10000

Change these values and enablements at your discretion. And if you really want
to enable RPC reporting, you can do so by way of
:config:option:`csrt/enable_rpc_reporting`, which will then use the same
configured Thresholds to match against those RPC workers, which, using the
default `ioq_calls` Threshold of 10000 would result in generating an
`rpc_worker` rctx report for any workers that generated more than 10000
ioq_calls, and similarly for the coordinator, which is a little awkward but it
provides a way to at least get node level reports generating when you really
need to see RPC worker resource usage at the node level. This is certainly
useful, but with Threshold configured low enough this will generate large
volumes of RPC worker reports, as described above, so cautious is warranted in
enabling RPC report logging with these filters::

    [csrt]
    enable = true
    enable_init_p = true
    enable_reporting = true
    enable_rpc_reporting = true

    [csrt_logger.matchers_enabled]
    docs_read = true
    rows_read = true
    docs_written = true
    long_reqs = true
    changes_processed = true
    ioq_calls = true

That said, if you really want to enable RPC reporting, you can do so by way of
:config:option:`csrt_logger.matchers_enabled/all_rpc_workers`, which combined
with :config:option:`csrt_logger.matchers_enabled/all_coordinators` will enable
logging for *ALL* coordinators and RPC workers, resulting in a report generated
for every CSRT tracked process lifecycle. This would be much better suited for
writing directly to a Vector store for post processing without any of the
verbose string labels. This also has the advantage of exposing both
`all_coordinators` and `all_rpc_workers` through the `/_active_resources`
interface, allowing for efficient querying and aggregating on either all
coordinators or RPC workers::

    [csrt]
    enable = true
    enable_init_p = true
    enable_reporting = true
    enable_rpc_reporting = true

    [csrt_logger.matchers_enabled]
    all_coordinators = true
    all_rpc_workers = true

.. note::

   Enabling :config:option:`csrt_logger.matchers_enabled/all_rpc_workers` while
   leaving :config:option:`csrt/enable_rpc_reporting` disabled will result in a
   pragmatic middle ground with no RPC reports being generated, yet the
   `all_rpc_workers` logger matcher being enabled for querying. We should
   probably extend the Logger Matchers logic to allow for specific Logger
   Matchers to only be utilized for the querying APIs, and allow for more
   stringent filters when decided to generate a lifecycle report.
