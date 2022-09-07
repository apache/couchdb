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

.. _config/index_builds:

===================
Background Indexing
===================

Secondary indexes in CouchDB are not updated during document write operations. In order to
avoid high latencies when reading indexes following a large block of writes, CouchDB
automatically kicks off background jobs to keep secondary indexes "warm". The daemon
responsible for this process is internally known as "ken" and can be configured using the
following settings.

.. config:section:: ken :: Background Index Builds

    .. config:option:: batch_channels :: Steady-state build concurrency

        This setting controls the number of background view builds that can be running in
        parallel at any given time. The default is 20.

    .. config:option:: incremental_channels :: Additional slots for short jobs

        It is possible for all the slots in the normal build system to be occupied by
        long-running index rebuilds (e.g. if new design documents are posted to several
        databases simultaneously). In order to avoid already-built indexes from falling
        behind when this occurs, CouchDB will allow for a number of short background
        indexing jobs to run even when all slots are full. This setting controls how many
        additional short jobs are allowed to run concurrently with the main jobs. The
        default is 80.

    .. config:option:: max_incremental_updates :: Threshold defining a job as short

        CouchDB estimates whether an indexing job is "incremental" or not by looking at
        the difference in sequence numbers between the current index and the main
        database. If the difference is larger than the threshold defined here the
        background job will only be allowed to run in the main queue. Defaults to 1000.

.. config:section:: ken.ignore :: Auto-Indexing Blocklist

Entries in this configuration section can be used to tell the background indexer to skip
over specific database shard files. The key must be the exact name of the shard with the
``.couch`` suffix omitted, for example:

    .. code-block:: ini

        [ken.ignore]
        shards/00000000-1fffffff/mydb.1567719095 = true

    .. note::
        In case when you'd like to skip all views from a ddoc, you may add
        ``autoupdate: false`` to the ddoc. All views of that ddoc will then be skipped.

        More at :http:put:`/{db}/_design/{ddoc}`.
