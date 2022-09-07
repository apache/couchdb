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

.. _replication:

===========
Replication
===========

Replication is an incremental one way process involving two databases
(a source and a destination).

The aim of replication is that at the end of the process, all active
documents in the source database are also in the destination database
and all documents that were deleted in the source database are also
deleted in the destination database (if they even existed).

The replication process only copies the last revision of a document, so all
previous revisions that were only in the source database are not copied to the
destination database.

.. toctree::
    :maxdepth: 2

    intro
    replicator
    conflicts
    protocol
