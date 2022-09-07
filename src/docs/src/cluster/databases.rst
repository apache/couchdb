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

.. _cluster/databases:

===================
Database Management
===================

.. _cluster/databases/create:

Creating a database
===================

This will create a database with ``3`` replicas and ``8`` shards.

.. code-block:: bash

    curl -X PUT "http://xxx.xxx.xxx.xxx:5984/database-name?n=3&q=8" --user admin-user

The database is in ``data/shards``. Look around on all the nodes and you will
find all the parts.

If you do not specify ``n`` and ``q`` the default will be used. The default is
``3`` replicas and ``8`` shards.

.. _cluster/databases/delete:

Deleting a database
===================

.. code-block:: bash

    curl -X DELETE "http://xxx.xxx.xxx.xxx:5984/database-name --user admin-user

.. _cluster/databases/placement:

Placing a database on specific nodes
====================================

In BigCouch, the predecessor to CouchDB 2.0's clustering functionality, there
was the concept of zones. CouchDB 2.0 carries this forward with cluster
placement rules.

.. warning::

    Use of the ``placement`` argument will **override** the standard
    logic for shard replica cardinality (specified by ``[cluster] n``.)

First, each node must be labeled with a zone attribute. This defines which zone each node
is in. You do this by editing the node's document in the system ``_nodes`` database, which
is accessed node-local via the ``GET /_node/_local/_nodes/{node-name}`` endpoint.

Add a key value pair of the form:

.. code-block:: text

    "zone": "metro-dc-a"

Do this for all of the nodes in your cluster.

In your config file (``local.ini`` or ``default.ini``) on each node, define a
consistent cluster-wide setting like:

.. code-block:: text

    [cluster]
    placement = metro-dc-a:2,metro-dc-b:1

In this example, it will ensure that two replicas for a shard will be hosted
on nodes with the zone attribute set to ``metro-dc-a`` and one replica will
be hosted on a new with the zone attribute set to ``metro-dc-b``.

Note that you can also use this system to ensure certain nodes in the cluster
do not host *any* replicas for newly created databases, by giving them a zone
attribute that does not appear in the ``[cluster]`` placement string.
