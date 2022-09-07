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

.. _cluster:

==================
Cluster Management
==================

As of CouchDB 2.0.0, CouchDB can be run in two different modes of operation:
    * Standalone: In this mode, CouchDB's clustering is unavailable.
      CouchDB's HTTP-based replication with other CouchDB installations remains available.
    * Cluster: A cluster of CouchDB installations internally replicate
      with each other via optimized network connections.
      This is intended to be used with servers that are in the same data center.
      This allows for database sharding to improve performance.

This section details the theory behind CouchDB clusters, and provides specific
operational instructions on node, database and shard management.

.. toctree::
    :maxdepth: 2

    theory
    nodes
    databases
    sharding
    purging
    tls_erlang_distribution
    troubleshooting
