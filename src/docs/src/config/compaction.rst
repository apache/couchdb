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

==========
Compaction
==========

.. _config/database_compaction:

Database Compaction Options
===========================

.. config:section:: database_compaction :: Database Compaction Options

    .. config:option:: doc_buffer_size :: Documents buffer size

        Specifies the copy buffer's maximum size in bytes::

            [database_compaction]
            doc_buffer_size = 524288

    .. config:option:: checkpoint_after :: Checkpoint trigger

        Triggers a checkpoint after the specified amount of bytes were
        successfully copied to the compacted database::

            [database_compaction]
            checkpoint_after = 5242880

.. _config/view_compaction:

View Compaction Options
=======================

.. config:section:: view_compaction :: View Compaction Options

    .. config:option:: keyvalue_buffer_size :: Key-Values buffer size

        Specifies maximum copy buffer size in bytes used during compaction::

            [view_compaction]
            keyvalue_buffer_size = 2097152

.. _config/compactions:

Compaction Daemon
=================

CouchDB ships with an automated, event-driven daemon internally known as "smoosh" that
continuously re-prioritizes the database and secondary index files on each node and
automatically compacts the files that will recover the most free space according to the
following parameters.

.. config:section:: smoosh :: Compaction Daemon Rules

    .. config:option:: db_channels :: Active database channels

        A comma-delimited list of channels that are sent the names of database
        files when those files are updated. Each channel can choose whether to
        enqueue the database for compaction; once a channel has enqueued the
        database, no additional channel in the list will be given the
        opportunity to do so.

    .. config:option:: view_channels :: Active secondary index channels

        A comma-delimited list of channels that are sent the names of secondary
        index files when those files are updated. Each channel can choose
        whether to enqueue the index for compaction; once a channel has enqueued
        the index, no additional channel in the list will be given the
        opportunity to do so.

    .. config:option:: staleness :: Minimum time between priority calculations

        The number of minutes that the (expensive) priority calculation on an
        individual can be stale for before it is recalculated. Defaults to 5.

    .. config:option:: cleanup_index_files :: Automatically delete orphaned index files

        If set to true, the compaction daemon will delete the files for indexes
        that are no longer associated with any design document. Defaults to
        `false` and probably shouldn't be changed unless the node is running low
        on disk space, and only after considering the ramifications.

    .. config:option:: wait_secs :: Warmup period before triggering first compaction

        The time a channel waits before starting compactions to allow time to
        observe the system and make a smarter decision about what to compact
        first. Hardly ever changed from the default of 30 (seconds).

.. config:section:: smoosh.<channel> :: Per-channel configuration

The following settings control the resource allocation for a given compaction
channel.

    .. config:option:: capacity :: Maximum number of items

        The maximum number of items the channel can hold (lowest priority item
        is removed to make room for new items). Defaults to 9999.

    .. config:option:: concurrency :: Maximum number of concurrent jobs

        The maximum number of jobs that can run concurrently in this channel.
        Defaults to 1.

    .. config:option:: from :: Time window start

    .. config:option:: to :: Time window end

        The time period during which this channel is allowed to execute
        compactions. The value for each of these parameters must obey the format
        `HH:MM` with HH in [0..23] and MM in [0..59]. Each channel listed in the
        top-level daemon configuration continuously builds its priority queue
        regardless of the period defined here. The default is to allow the
        channel to execute compactions all the time.

    .. config:option:: strict_window :: Run compaction only within the time window

        If set to ``true``, any compaction that is still running after the end of
        the allowed perio will be suspended, and then resumed during the next
        window. It defaults to ``false``, in which case any running compactions
        will be allowed to finish, but no new ones will be started.

There are also several settings that collectively control whether a channel will
enqueue a file for compaction and how it prioritizes files within its queue:

    .. config:option:: max_priority :: Maximum priority of item to be enqueued

        Each item must have a priority lower than this to be enqueued. Defaults
        to infinity.

    .. config:option:: max_size :: Maximum size of item to be enqueued

        The item must be no larger than this many bytes in length to be
        enqueued. Defaults to infinity.

    .. config:option:: min_priority :: Minimum priority of item to be enqueued

        The item must have a priority at least this high to be enqueued.
        Defaults to 5.0 for ratio and 16 MB for slack.

    .. config:option:: min_changes :: Minimum number of changes of item to be enqueued

        The minimum number of changes since last compaction before the item will
        be enqueued. Defaults to 0. Currently only works for databases.

    .. config:option:: min_size :: Minimum size of item to be enqueued

        The item must be at least this many bytes in length to be enqueued.
        Defaults to 1mb (1048576 bytes).

    .. config:option:: priority :: Method for priority calculation

        The method used to calculate priority. Can be ratio (calculated as
        ``sizes.file/sizes.active``) or slack (calculated as ``sizes.file -
        sizes.active``). Defaults to ratio.
