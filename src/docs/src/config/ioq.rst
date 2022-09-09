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

.. _config/ioq:

========
IO Queue
========

CouchDB has an internal subsystem that can prioritize IO associated with certain
classes of operations. This subsystem can be configured to limit the resources
devoted to background operations like internal replication and compaction
according to the settings described below.

.. config:section:: ioq :: IO Queue Configuration

    .. config:option:: concurrency :: Number of in-flight IO requests

        Specifies the maximum number of concurrent in-flight IO requests that
        the queueing system will submit::

            [ioq]
            concurrency = 10

    .. config:option:: ratio :: Preference for selecting background over interactive IO

        The fraction of the time that a background IO request will be selected
        over an interactive IO request when both queues are non-empty::

            [ioq]
            ratio = 0.01

.. config:section:: ioq.bypass :: Bypass selected IO classes by setting these to true

    System administrators can choose to submit specific classes of IO directly
    to the underlying file descriptor or OS process, bypassing the queues
    altogether. Installing a bypass can yield higher throughput and lower
    latency, but relinquishes some control over prioritization. The following
    classes are recognized:

    .. config:option:: os_process

        Messages on their way to an external process (e.g., ``couchjs``).

    .. config:option:: read

        Disk IO fulfilling interactive read requests.

    .. config:option:: write

        Disk IO required to update a database.

    .. config:option:: view_update

        Disk IO required to update views and other secondary indexes.

    .. config:option:: shard_sync

        Disk IO issued by the background replication processes that fix any
        inconsistencies between shard copies.

    .. config:option:: compaction

        Disk IO issued by compaction jobs.

    Without any configuration CouchDB will enqueue all classes of IO. The
    default.ini configuration file that ships with CouchDB activates a bypass
    for each of the interactive IO classes and only background IO goes into the
    queueing system::

        [ioq.bypass]
        os_process = true
        read = true
        write = true
        view_update = true
        shard_sync = false
        compaction = false

Recommendations
===============

The default configuration protects against excessive IO from background
operations like compaction disrupting the latency of interactive operations,
while maximizing the overall IO throughput devoted to those interactive
requests. There are certain situations where this configuration could be
sub-optimal:

* An administrator may want to devote a larger portion of the overall IO
  bandwidth to compaction in order to stay ahead of the incoming write load. In
  this it may be necessary to disable the bypass for ``write`` (to help with
  database compaction) and/or ``view_update`` (to help with view index compaction)
  and then increase the ``ratio`` to give compaction a higher priority.

* A server with a large number of views that do not need to be comlpetely
  up-to-date may benefit from removing the bypass on ``view_update`` in order to
  optimize the latency for regular document read and write operations, and build
  the views during quieter periods.
