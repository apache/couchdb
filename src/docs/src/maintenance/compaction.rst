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

.. _compact:

==========
Compaction
==========

The `compaction` operation is a way to reduce disk space usage by removing
unused and old data from database or view index files. This operation is very
similar to the `vacuum` (`SQLite`_ ex.) operation available for other database
management systems.

.. _SQLite: http://www.sqlite.org/lang_vacuum.html

During compaction, CouchDB re-creates the database or view in a new file
with the ``.compact`` extension. As this requires roughly twice the disk storage,
CouchDB first checks for available disk space before proceeding.

When all actual data is successfully transferred to the newly compacted file,
CouchDB transparently swaps the compacted file into service, and removes the
old database or view file.

Since CouchDB 2.1.1, automated compaction is enabled by default, and is
described in the next section. It is still possible to trigger manual
compaction if desired or necessary. This is described in the subsequent
sections.

.. _compact/auto:

Automatic Compaction
====================

CouchDB's automatic compaction daemon, internally known as "smoosh", will
trigger compaction jobs for both databases and views based on configurable
thresholds for the sparseness of a file and the total amount of space that can
be recovered.

Channels
--------

Smoosh works using the concept of channels. A channel is essentially a queue of
pending compactions. There are separate sets of active channels for databases
and views. Each channel is assigned a configuration which defines whether a
compaction ends up in the channel's queue and how compactions are prioritized
within that queue.

Smoosh takes each channel and works through the compactions queued in each in
priority order. Each channel is processed concurrently, so the priority levels
only matter within a given channel. Each channel has an assigned number of
active compactions, which defines how many compactions happen for that channel
in parallel. For example, a cluster with a lot of database churn but few views
might require more active compactions in the database channel(s).

It's important to remember that a channel is local to a CouchDB node; that is,
each node maintains and processes an independent set of compactions. Channels
are defined as either "ratio" channels or "slack" channels, depending on the
type of algorithm used for prioritization:

-   Ratio: uses the ratio of sizes.file / sizes.active as its driving
    calculation. The result X must be greater than some configurable value Y for
    a compaction to be added to the queue. Compactions are then prioritised for
    higher values of X.

-   Slack: uses the difference of sizes.file - sizes.active as its driving
    calculation. The result X must be greater than some configurable value Y for
    a compaction to be added to the queue. Compactions are prioritised for
    higher values of X.

In both cases, Y is set using the ``min_priority`` configuration variable. CouchDB
ships with four channels pre-configured: one channel of each type for databases,
and another one for views.

Channel Configuration
---------------------

Channels are defined using ``[smoosh.<channel_name>]`` configuration blocks, and
activated by naming the channel in the ``db_channels`` or ``view_channels``
configuration setting in the ``[smoosh]`` block. The default configuration is

.. code-block:: ini

    [smoosh]
    db_channels = upgrade_dbs,ratio_dbs,slack_dbs
    view_channels = upgrade_views,ratio_views,slack_views

    [smoosh.ratio_dbs]
    priority = ratio
    min_priority = 2.0

    [smoosh.ratio_views]
    priority = ratio
    min_priority = 2.0

    [smoosh.slack_dbs]
    priority = slack
    min_priority = 536870912

    [smoosh.slack_views]
    priority = slack
    min_priority = 536870912

The "upgrade" channels are a special pair of channels that only check whether
the `disk_format_version` for the file matches the current version, and enqueue
the file for compaction (which has the side effect of upgrading the file format)
if that's not the case. There are several additional properties that can be
configured for each channel; these are documented in the :ref:`configuration API
<config/compactions>`

Scheduling Windows
------------------

Each compaction channel can be configured to run only during certain hours of
the day. The channel-specific `from`, `to`, and `strict_window` configuration
settings control this behavior. For example

.. code-block:: ini

    [smoosh.overnight_channel]
    from = 20:00
    to = 06:00
    strict_window = true

where `overnight_channel` is the name of the channel you want to configure.

Note: CouchDB determines time via the UTC (GMT) timezone, so these settings must be
expressed as UTC (GMT).

The ``strict_window`` setting will cause the compaction daemon to suspend all
active compactions in this channel when exiting the window, and resume them when
re-entering. If ``strict_window`` is left at its default of false, the active
compactions will be allowed to complete but no new compactions will be started.

Migration Guide
---------------

Previous versions of CouchDB shipped with a simpler compaction daemon. The
configuration system for the new daemon is not backwards-compatible with the old
one, so users with customized compaction configurations will need to port them
to the new setup. The old daemon's compaction rules configuration looked like

.. code-block:: ini

    [compaction_daemon]
    min_file_size = 131072
    check_interval = 3600
    snooze_period_ms = 3000

    [compactions]
    mydb = [{db_fragmentation, "70%"}, {view_fragmentation, "60%"}, {parallel_view_compaction, true}]
    _default = [{db_fragmentation, "50%"}, {view_fragmentation, "55%"}, {from, "20:00"}, {to, "06:00"}, {strict_window, true}]

Many of the elements of this configuration can be ported over to the new system.
Examining each in detail:

*   ``min_file_size`` is now configured on a per-channel basis using the
    min_size config setting.

*   ``db_fragmentation`` is equivalent to configuring a priority = ratio
    channel with min_priority set to 1.0 / (1 - db_fragmentation/100)
    and then listing that channel in the [smoosh] db_channels config
    setting.

*   ``view_fragmention`` is likewise equivalent to configuring a priority = ratio
    channel with min_priority set to 1.0 / (1 - view_fragmentation/100)
    and then listing that channel in the [smoosh] view_channels config
    setting.

*   ``from`` / ``to`` / ``strict_window``: each of these settings can be applied
    on a per-channel basis in the new daemon. The one behavior change is that
    the new daemon will suspend compactions upon exiting the allowed window
    instead of canceling them outright, and resume them when re-entering.

*   ``parallel_view_compaction``: each compaction channel has a concurrency
    setting that controls how many compactions will execute in parallel in that
    channel. The total parallelism is the sum of the concurrency settings of all
    active channels. This is a departure from the previous behavior, in which
    the daemon would only focus on one database and/or its views (depending on
    the value of this flag) at a time.

The ``check_interval`` and ``snooze_period_ms`` settings are obsolete in the
event-driven design of the new daemon. The new daemon does not support setting
database-specific thresholds as in the ``mydb`` setting above. Rather, channels
can be configured to focus on specific classes of files: large databases, small
view indexes, and so on. Most cases of named database compaction rules can be
expressed using properties of those databases and/or their associated views.

.. _compact/db:

Manual Database Compaction
==========================

Database compaction compresses the database file by removing unused file
sections created during updates. Old documents revisions are replaced with
small amount of metadata called `tombstone` which are used for conflicts
resolution during replication. The number of stored revisions
(and their `tombstones`) can be configured by using the :get:`_revs_limit
</{db}/_revs_limit>` URL endpoint.

Compaction can be manually triggered per database and runs as a background
task. To start it for specific database there is need to send HTTP
:post:`/{db}/_compact` sub-resource of the target database::

    curl -H "Content-Type: application/json" -X POST http://localhost:5984/my_db/_compact

On success, HTTP status :statuscode:`202` is returned immediately:

.. code-block:: http

    HTTP/1.1 202 Accepted
    Cache-Control: must-revalidate
    Content-Length: 12
    Content-Type: text/plain; charset=utf-8
    Date: Wed, 19 Jun 2013 09:43:52 GMT
    Server: CouchDB (Erlang/OTP)

.. code-block:: javascript

    {"ok":true}

Although the request body is not used you must still specify
:header:`Content-Type` header with :mimetype:`application/json` value
for the request. If you don't, you will be aware about with HTTP status
:statuscode:`415` response:

.. code-block:: http

    HTTP/1.1 415 Unsupported Media Type
    Cache-Control: must-revalidate
    Content-Length: 78
    Content-Type: application/json
    Date: Wed, 19 Jun 2013 09:43:44 GMT
    Server: CouchDB (Erlang/OTP)

    {"error":"bad_content_type","reason":"Content-Type must be application/json"}

When the compaction is successful started and running it is possible to get
information about it via :ref:`database information resource <api/db>`::

    curl http://localhost:5984/my_db

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 246
    Content-Type: application/json
    Date: Wed, 19 Jun 2013 16:51:20 GMT
    Server: CouchDB (Erlang/OTP)

    {
        "committed_update_seq": 76215,
        "compact_running": true,
        "db_name": "my_db",
        "disk_format_version": 6,
        "doc_count": 5091,
        "doc_del_count": 0,
        "instance_start_time": "0",
        "purge_seq": 0,
        "sizes": {
          "active": 3787996,
          "disk": 17703025,
          "external": 4763321
        },
        "update_seq": 76215
    }

Note that ``compact_running`` field is ``true`` indicating that compaction
is actually running. To track the compaction progress you may query the
:get:`_active_tasks </_active_tasks>` resource::

    curl http://localhost:5984/_active_tasks

.. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 175
    Content-Type: application/json
    Date: Wed, 19 Jun 2013 16:27:23 GMT
    Server: CouchDB (Erlang/OTP)

    [
        {
            "changes_done": 44461,
            "database": "my_db",
            "pid": "<0.218.0>",
            "progress": 58,
            "started_on": 1371659228,
            "total_changes": 76215,
            "type": "database_compaction",
            "updated_on": 1371659241
        }
    ]

.. _compact/views:

Manual View Compaction
======================

`Views` also need compaction. Unlike databases, views are compacted by groups
per `design document`. To start their compaction, send the HTTP
:post:`/{db}/_compact/{ddoc}` request::

    curl -H "Content-Type: application/json" -X POST http://localhost:5984/dbname/_compact/designname

.. code-block:: javascript

    {"ok":true}

This compacts the view index from the current version of the specified design
document. The HTTP response code is :statuscode:`202`
(like :ref:`compaction for databases <compact/db>`) and a compaction background
task will be created.

.. _compact/views/cleanup:

Views cleanup
-------------

View indexes on disk are named after their `MD5` hash of the view definition.
When you change a view, old indexes remain on disk. To clean up all outdated
view indexes (files named after the MD5 representation of views, that does not
exist anymore) you can trigger a :ref:`view cleanup <api/db/view_cleanup>`::

    curl -H "Content-Type: application/json" -X POST http://localhost:5984/dbname/_view_cleanup

.. code-block:: javascript

    {"ok":true}
