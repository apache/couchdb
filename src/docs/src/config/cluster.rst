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

======================
Configuring Clustering
======================

.. _config/cluster:

Cluster Options
===============

.. config:section:: cluster :: Cluster Options

    .. config:option:: q :: Default number of shards for newly created database

    Sets the default number of shards for newly created databases. The
    default value, ``2``, splits a database into 2 separate partitions. ::

        [cluster]
        q = 2

    For systems with only a few, heavily accessed, large databases, or
    for servers with many CPU cores, consider increasing this value to
    ``4`` or ``8``.

    The value of ``q`` can also be overridden on a per-DB basis, at DB
    creation time.

    .. seealso::
        :http:put:`PUT /{db} </{db}>`

    .. config:option:: n :: Number of replicas of each document

    Sets the number of replicas of each document in a cluster. CouchDB will
    only place one replica per node in a cluster. When set up through the
    :ref:`Cluster Setup Wizard <cluster/setup/wizard>`, a standalone single
    node will have ``n = 1``, a two node cluster will have ``n = 2``, and
    any larger cluster will have ``n = 3``. It is recommended not to set
    ``n`` greater than ``3``. ::

        [cluster]
        n = 3

    .. config:option:: placement :: Sets the cluster-wide replica placement policy

    .. warning::

        Use of this option will **override** the ``n`` option for replica
        cardinality. Use with care.

    Sets the cluster-wide replica placement policy when creating new
    databases. The value must be a comma-delimited list of strings of the
    format ``zone_name:#``, where ``zone_name`` is a zone as specified in
    the ``nodes`` database and ``#`` is an integer indicating the number of
    replicas to place on nodes with a matching ``zone_name``.

    This parameter is not specified by default. ::

        [cluster]
        placement = metro-dc-a:2,metro-dc-b:1

    .. seealso::
        :ref:`cluster/databases/placement`

    .. config:option:: seedlist:: Optional, comma-delimited list of nodes that should \
        be contacted to join cluster

    An optional, comma-delimited list of node names that this node should
    contact in order to join a cluster. If a seedlist is configured the ``_up``
    endpoint will return a 404 until the node has successfully contacted at
    least one of the members of the seedlist and replicated an up-to-date copy
    of the ``_nodes``, ``_dbs``, and ``_users`` system databases.

        [cluster]
        seedlist = couchdb@node1.example.com,couchdb@node2.example.com

RPC Performance Tuning
======================

.. config:section:: rexi :: Internal RPC Tuning

    CouchDB uses distributed Erlang to communicate between nodes in a cluster.
    The ``rexi`` library provides an optimized RPC mechanism over this
    communication channel. There are a few configuration knobs for this system,
    although in general the defaults work well.

    .. config:option:: buffer_count :: Number of buffered messages before dropping

    The local RPC server will buffer messages if a remote node goes unavailable.
    This flag determines how many messages will be buffered before the local
    server starts dropping messages. Default value is ``2000``.

    .. config:option:: server_per_node :: Enable or disable one local `gen_server` \
        process per node

    By default, rexi will spawn one local gen_server process for each node in
    the cluster. Disabling this flag will cause CouchDB to use a single process
    for all RPC communication, which is not recommended in high throughput
    deployments.

    .. config:option:: stream_limit :: Number of send messages without waiting \
        for acknowledgement from the coordinator

        .. versionadded:: 3.0

    This flag comes into play during streaming operations like views and change
    feeds. It controls how many messages a remote worker process can send to a
    coordinator without waiting for an acknowledgement from the coordinator
    process. If this value is too large the coordinator can become overwhelmed
    by messages from the worker processes and actually deliver lower overall
    throughput to the client. In CouchDB 2.x this value was hard-coded to
    ``10``. In the 3.x series it is configurable and defaults to ``5``.
    Databases with a high ``q`` value are especially sensitive to this setting.
