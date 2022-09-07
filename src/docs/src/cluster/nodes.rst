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

.. _cluster/nodes:

===============
Node Management
===============

.. _cluster/nodes/add:

Adding a node
=============

Go to ``http://server1:5984/_membership`` to see the name of the node and all
the nodes it is connected to and knows about.

.. code-block:: text

    curl -X GET "http://xxx.xxx.xxx.xxx:5984/_membership" --user admin-user

.. code-block:: javascript

    {
        "all_nodes":[
            "node1@xxx.xxx.xxx.xxx"],
        "cluster_nodes":[
            "node1@xxx.xxx.xxx.xxx"]
    }

* ``all_nodes`` are all the nodes that this node knows about.
* ``cluster_nodes`` are the nodes that are connected to this node.

To add a node simply do:

.. code-block:: text

    curl -X PUT "http://xxx.xxx.xxx.xxx/_node/_local/_nodes/node2@yyy.yyy.yyy.yyy" -d {}

Now look at ``http://server1:5984/_membership`` again.

.. code-block:: javascript

    {
        "all_nodes":[
            "node1@xxx.xxx.xxx.xxx",
            "node2@yyy.yyy.yyy.yyy"
        ],
        "cluster_nodes":[
            "node1@xxx.xxx.xxx.xxx",
            "node2@yyy.yyy.yyy.yyy"
        ]
    }

And you have a 2 node cluster :)

``http://yyy.yyy.yyy.yyy:5984/_membership`` will show the same thing, so you
only have to add a node once.

.. _cluster/nodes/remove:

Removing a node
===============

Before you remove a node, make sure that you have moved all
:ref:`shards <cluster/sharding/move>` away from that node.

To remove ``node2`` from server ``yyy.yyy.yyy.yyy``, you need to first know the
revision of the document that signifies that node’s existence:

.. code-block:: text

    curl "http://xxx.xxx.xxx.xxx/_node/_local/_nodes/node2@yyy.yyy.yyy.yyy"
    {"_id":"node2@yyy.yyy.yyy.yyy","_rev":"1-967a00dff5e02add41820138abb3284d"}

With that ``_rev``, you can now proceed to delete the node document:

.. code-block:: text

    curl -X DELETE "http://xxx.xxx.xxx.xxx/_node/_local/_nodes/node2@yyy.yyy.yyy.yyy?rev=1-967a00dff5e02add41820138abb3284d"
