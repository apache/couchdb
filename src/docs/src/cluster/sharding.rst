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

.. _cluster/sharding:

================
Shard Management
================

.. _cluster/sharding/intro:

Introduction
------------

This document discusses how sharding works in CouchDB along with how to
safely add, move, remove, and create placement rules for shards and
shard replicas.

A `shard
<https://en.wikipedia.org/wiki/Shard_(database_architecture)>`__ is a
horizontal partition of data in a database. Partitioning data into
shards and distributing copies of each shard (called "shard replicas" or
just "replicas") to different nodes in a cluster gives the data greater
durability against node loss. CouchDB clusters automatically shard
databases and distribute the subsets of documents that compose each
shard among nodes. Modifying cluster membership and sharding behavior
must be done manually.

Shards and Replicas
~~~~~~~~~~~~~~~~~~~

How many shards and replicas each database has can be set at the global
level, or on a per-database basis. The relevant parameters are ``q`` and
``n``.

*q* is the number of database shards to maintain. *n* is the number of
copies of each document to distribute. The default value for ``n`` is ``3``,
and for ``q`` is ``2``. With ``q=2``, the database is split into 2 shards. With
``n=3``, the cluster distributes three replicas of each shard. Altogether,
that's 6 shard replicas for a single database.

In a 3-node cluster with ``q=8``, each node would receive 8 shards. In a 4-node
cluster, each node would receive 6 shards. We recommend in the general case
that the number of nodes in your cluster should be a multiple of ``n``, so that
shards are distributed evenly.

CouchDB nodes have a ``etc/default.ini`` file with a section named
`cluster <../config/cluster.html>`__ which looks like this:

::

    [cluster]
    q=2
    n=3

These settings specify the default sharding parameters for newly created
databases. These can be overridden in the ``etc/local.ini`` file by copying the
text above, and replacing the values with your new defaults.  The values can
also be set on a per-database basis by specifying the ``q`` and ``n`` query
parameters when the database is created. For example:

.. code-block:: bash

    $ curl -X PUT "$COUCH_URL:5984/database-name?q=4&n=2"

This creates a database that is split into 4 shards and 2 replicas,
yielding 8 shard replicas distributed throughout the cluster.

Quorum
~~~~~~

Depending on the size of the cluster, the number of shards per database,
and the number of shard replicas, not every node may have access to
every shard, but every node knows where all the replicas of each shard
can be found through CouchDB's internal shard map.

Each request that comes in to a CouchDB cluster is handled by any one
random coordinating node. This coordinating node proxies the request to
the other nodes that have the relevant data, which may or may not
include itself. The coordinating node sends a response to the client
once a `quorum
<https://en.wikipedia.org/wiki/Quorum_(distributed_computing)>`__ of
database nodes have responded; 2, by default. The default required size
of a quorum is equal to ``r=w=((n+1)/2)`` where ``r`` refers to the size
of a read quorum, ``w`` refers to the size of a write quorum, and ``n``
refers to the number of replicas of each shard. In a default cluster where
``n`` is 3, ``((n+1)/2)`` would be 2.

.. note::
    Each node in a cluster can be a coordinating node for any one
    request. There are no special roles for nodes inside the cluster.

The size of the required quorum can be configured at request time by
setting the ``r`` parameter for document and view reads, and the ``w``
parameter for document writes. For example, here is a request that
directs the coordinating node to send a response once at least two nodes
have responded:

.. code-block:: bash

    $ curl "$COUCH_URL:5984/{db}/{doc}?r=2"

Here is a similar example for writing a document:

.. code-block:: bash

    $ curl -X PUT "$COUCH_URL:5984/{db}/{doc}?w=2" -d '{...}'

Setting ``r`` or ``w`` to be equal to ``n`` (the number of replicas)
means you will only receive a response once all nodes with relevant
shards have responded or timed out, and as such this approach does not
guarantee `ACIDic consistency
<https://en.wikipedia.org/wiki/ACID#Consistency>`__. Setting ``r`` or
``w`` to 1 means you will receive a response after only one relevant
node has responded.

.. _cluster/sharding/examine:

Examining database shards
-------------------------

There are a few API endpoints that help you understand how a database
is sharded. Let's start by making a new database on a cluster, and putting
a couple of documents into it:

.. code-block:: bash

    $ curl -X PUT $COUCH_URL:5984/mydb
    {"ok":true}
    $ curl -X PUT $COUCH_URL:5984/mydb/joan -d '{"loves":"cats"}'
    {"ok":true,"id":"joan","rev":"1-cc240d66a894a7ee7ad3160e69f9051f"}
    $ curl -X PUT $COUCH_URL:5984/mydb/robert -d '{"loves":"dogs"}'
    {"ok":true,"id":"robert","rev":"1-4032b428c7574a85bc04f1f271be446e"}

First, the top level :ref:`api/db` endpoint will tell you what the sharding parameters
are for your database:

.. code-block:: bash

    $ curl -s $COUCH_URL:5984/db | jq .
    {
      "db_name": "mydb",
    ...
      "cluster": {
        "q": 8,
        "n": 3,
        "w": 2,
        "r": 2
      },
    ...
    }

So we know this database was created with 8 shards (``q=8``), and each
shard has 3 replicas (``n=3``) for a total of 24 shard replicas across
the nodes in the cluster.

Now, let's see how those shard replicas are placed on the cluster with
the :ref:`api/db/shards` endpoint:

.. code-block:: bash

    $ curl -s $COUCH_URL:5984/mydb/_shards | jq .
    {
      "shards": {
        "00000000-1fffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node4@127.0.0.1"
        ],
        "20000000-3fffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node3@127.0.0.1"
        ],
        "40000000-5fffffff": [
          "node2@127.0.0.1",
          "node3@127.0.0.1",
          "node4@127.0.0.1"
        ],
        "60000000-7fffffff": [
          "node1@127.0.0.1",
          "node3@127.0.0.1",
          "node4@127.0.0.1"
        ],
        "80000000-9fffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node4@127.0.0.1"
        ],
        "a0000000-bfffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node3@127.0.0.1"
        ],
        "c0000000-dfffffff": [
          "node2@127.0.0.1",
          "node3@127.0.0.1",
          "node4@127.0.0.1"
        ],
        "e0000000-ffffffff": [
          "node1@127.0.0.1",
          "node3@127.0.0.1",
          "node4@127.0.0.1"
        ]
      }
    }

Now we see that there are actually 4 nodes in this cluster, and CouchDB
has spread those 24 shard replicas evenly across all 4 nodes.

We can also see exactly which shard contains a given document with
the :ref:`api/db/shards/doc` endpoint:

.. code-block:: bash

    $ curl -s $COUCH_URL:5984/mydb/_shards/joan | jq .
    {
      "range": "e0000000-ffffffff",
      "nodes": [
        "node1@127.0.0.1",
        "node3@127.0.0.1",
        "node4@127.0.0.1"
      ]
    }
    $ curl -s $COUCH_URL:5984/mydb/_shards/robert | jq .
    {
      "range": "60000000-7fffffff",
      "nodes": [
        "node1@127.0.0.1",
        "node3@127.0.0.1",
        "node4@127.0.0.1"
      ]
    }

CouchDB shows us the specific shard into which each of the two sample
documents is mapped.

.. _cluster/sharding/move:

Moving a shard
--------------

When moving shards or performing other shard manipulations on the cluster, it
is advisable to stop all resharding jobs on the cluster. See
:ref:`cluster/sharding/stop_resharding` for more details.

This section describes how to manually place and replace shards. These
activities are critical steps when you determine your cluster is too big
or too small, and want to resize it successfully, or you have noticed
from server metrics that database/shard layout is non-optimal and you
have some "hot spots" that need resolving.

Consider a three-node cluster with q=8 and n=3. Each database has 24
shards, distributed across the three nodes. If you :ref:`add a fourth
node <cluster/nodes/add>` to the cluster, CouchDB will not redistribute
existing database shards to it. This leads to unbalanced load, as the
new node will only host shards for databases created after it joined the
cluster. To balance the distribution of shards from existing databases,
they must be moved manually.

Moving shards between nodes in a cluster involves the following steps:

0. :ref:`Ensure the target node has joined the cluster <cluster/nodes/add>`.
1. Copy the shard(s) and any secondary
   :ref:`index shard(s) onto the target node <cluster/sharding/copying>`.
2. :ref:`Set the target node to maintenance mode <cluster/sharding/mm>`.
3. Update cluster metadata
   :ref:`to reflect the new target shard(s) <cluster/sharding/add-shard>`.
4. Monitor internal replication
   :ref:`to ensure up-to-date shard(s) <cluster/sharding/verify>`.
5. :ref:`Clear the target node's maintenance mode <cluster/sharding/mm-2>`.
6. Update cluster metadata again
   :ref:`to remove the source shard(s)<cluster/sharding/remove-shard>`
7. Remove the shard file(s) and secondary index file(s)
   :ref:`from the source node <cluster/sharding/remove-shard-files>`.

.. _cluster/sharding/copying:

Copying shard files
~~~~~~~~~~~~~~~~~~~

.. note::
    Technically, copying database and secondary index
    shards is optional. If you proceed to the next step without
    performing this data copy, CouchDB will use internal replication
    to populate the newly added shard replicas. However, copying files
    is faster than internal replication, especially on a busy cluster,
    which is why we recommend performing this manual data copy first.

Shard files live in the ``data/shards`` directory of your CouchDB
install. Within those subdirectories are the shard files themselves. For
instance, for a ``q=8`` database called ``abc``, here is its database shard
files:

::

  data/shards/00000000-1fffffff/abc.1529362187.couch
  data/shards/20000000-3fffffff/abc.1529362187.couch
  data/shards/40000000-5fffffff/abc.1529362187.couch
  data/shards/60000000-7fffffff/abc.1529362187.couch
  data/shards/80000000-9fffffff/abc.1529362187.couch
  data/shards/a0000000-bfffffff/abc.1529362187.couch
  data/shards/c0000000-dfffffff/abc.1529362187.couch
  data/shards/e0000000-ffffffff/abc.1529362187.couch

Secondary indexes (including JavaScript views, Erlang views and Mango
indexes) are also sharded, and their shards should be moved to save the
new node the effort of rebuilding the view. View shards live in
``data/.shards``. For example:

::

  data/.shards
  data/.shards/e0000000-ffffffff/_replicator.1518451591_design
  data/.shards/e0000000-ffffffff/_replicator.1518451591_design/mrview
  data/.shards/e0000000-ffffffff/_replicator.1518451591_design/mrview/3e823c2a4383ac0c18d4e574135a5b08.view
  data/.shards/c0000000-dfffffff
  data/.shards/c0000000-dfffffff/_replicator.1518451591_design
  data/.shards/c0000000-dfffffff/_replicator.1518451591_design/mrview
  data/.shards/c0000000-dfffffff/_replicator.1518451591_design/mrview/3e823c2a4383ac0c18d4e574135a5b08.view
  ...

Since they are files, you can use ``cp``, ``rsync``,
``scp`` or other file-copying command to copy them from one node to
another. For example:

.. code-block:: bash

    # one one machine
    $ mkdir -p data/.shards/{range}
    $ mkdir -p data/shards/{range}
    # on the other
    $ scp {couch-dir}/data/.shards/{range}/{database}.{datecode}* \
      {node}:{couch-dir}/data/.shards/{range}/
    $ scp {couch-dir}/data/shards/{range}/{database}.{datecode}.couch \
      {node}:{couch-dir}/data/shards/{range}/

.. note::
    Remember to move view files before database files! If a view index
    is ahead of its database, the database will rebuild it from
    scratch.

.. _cluster/sharding/mm:

Set the target node to ``true`` maintenance mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before telling CouchDB about these new shards on the node, the node
must be put into maintenance mode. Maintenance mode instructs CouchDB to
return a ``404 Not Found`` response on the ``/_up`` endpoint, and
ensures it does not participate in normal interactive clustered requests
for its shards. A properly configured load balancer that uses ``GET
/_up`` to check the health of nodes will detect this 404 and remove the
node from circulation, preventing requests from being sent to that node.
For example, to configure HAProxy to use the ``/_up`` endpoint, use:

::

  http-check disable-on-404
  option httpchk GET /_up

If you do not set maintenance mode, or the load balancer ignores this
maintenance mode status, after the next step is performed the cluster
may return incorrect responses when consulting the node in question. You
don't want this! In the next steps, we will ensure that this shard is
up-to-date before allowing it to participate in end-user requests.

To enable maintenance mode:

.. code-block:: bash

    $ curl -X PUT -H "Content-type: application/json" \
        $COUCH_URL:5984/_node/{node-name}/_config/couchdb/maintenance_mode \
        -d "\"true\""

Then, verify that the node is in maintenance mode by performing a ``GET
/_up`` on that node's individual endpoint:

.. code-block:: bash

    $ curl -v $COUCH_URL/_up
    …
    < HTTP/1.1 404 Object Not Found
    …
    {"status":"maintenance_mode"}

Finally, check that your load balancer has removed the node from the
pool of available backend nodes.

.. _cluster/sharding/add-shard:

Updating cluster metadata to reflect the new target shard(s)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we need to tell CouchDB that the target node (which must already be
:ref:`joined to the cluster <cluster/nodes/add>`) should be hosting
shard replicas for a given database.

To update the cluster metadata, use the special ``/_dbs`` database,
which is an internal CouchDB database that maps databases to shards and
nodes. This database is automatically replicated between nodes. It is accessible
only through the special ``/_node/_local/_dbs`` endpoint.

First, retrieve the database's current metadata:

.. code-block:: bash

    $ curl http://localhost/_node/_local/_dbs/{name}
    {
      "_id": "{name}",
      "_rev": "1-e13fb7e79af3b3107ed62925058bfa3a",
      "shard_suffix": [46, 49, 53, 51, 48, 50, 51, 50, 53, 50, 54],
      "changelog": [
        ["add", "00000000-1fffffff", "node1@xxx.xxx.xxx.xxx"],
        ["add", "00000000-1fffffff", "node2@xxx.xxx.xxx.xxx"],
        ["add", "00000000-1fffffff", "node3@xxx.xxx.xxx.xxx"],
        …
      ],
      "by_node": {
        "node1@xxx.xxx.xxx.xxx": [
          "00000000-1fffffff",
          …
        ],
        …
      },
      "by_range": {
        "00000000-1fffffff": [
          "node1@xxx.xxx.xxx.xxx",
          "node2@xxx.xxx.xxx.xxx",
          "node3@xxx.xxx.xxx.xxx"
        ],
        …
      }
    }

Here is a brief anatomy of that document:

-  ``_id``: The name of the database.
-  ``_rev``: The current revision of the metadata.
-  ``shard_suffix``: A timestamp of the database's creation, marked as
   seconds after the Unix epoch mapped to the codepoints for ASCII
   numerals.
-  ``changelog``: History of the database's shards.
-  ``by_node``: List of shards on each node.
-  ``by_range``: On which nodes each shard is.

To reflect the shard move in the metadata, there are three steps:

1. Add appropriate changelog entries.
2. Update the ``by_node`` entries.
3. Update the ``by_range`` entries.

.. warning::
    Be very careful! Mistakes during this process can
    irreparably corrupt the cluster!

As of this writing, this process must be done manually.

To add a shard to a node, add entries like this to the database
metadata's ``changelog`` attribute:

.. code-block:: javascript

    ["add", "{range}", "{node-name}"]

The ``{range}`` is the specific shard range for the shard. The ``{node-name}``
should match the name and address of the node as displayed in ``GET
/_membership`` on the cluster.

.. note::
    When removing a shard from a node, specify ``remove`` instead of ``add``.

Once you have figured out the new changelog entries, you will need to
update the ``by_node`` and ``by_range`` to reflect who is storing what
shards. The data in the changelog entries and these attributes must
match. If they do not, the database may become corrupted.

Continuing our example, here is an updated version of the metadata above
that adds shards to an additional node called ``node4``:

.. code-block:: javascript

    {
      "_id": "{name}",
      "_rev": "1-e13fb7e79af3b3107ed62925058bfa3a",
      "shard_suffix": [46, 49, 53, 51, 48, 50, 51, 50, 53, 50, 54],
      "changelog": [
        ["add", "00000000-1fffffff", "node1@xxx.xxx.xxx.xxx"],
        ["add", "00000000-1fffffff", "node2@xxx.xxx.xxx.xxx"],
        ["add", "00000000-1fffffff", "node3@xxx.xxx.xxx.xxx"],
        ...
        ["add", "00000000-1fffffff", "node4@xxx.xxx.xxx.xxx"]
      ],
      "by_node": {
        "node1@xxx.xxx.xxx.xxx": [
          "00000000-1fffffff",
          ...
        ],
        ...
        "node4@xxx.xxx.xxx.xxx": [
          "00000000-1fffffff"
        ]
      },
      "by_range": {
        "00000000-1fffffff": [
          "node1@xxx.xxx.xxx.xxx",
          "node2@xxx.xxx.xxx.xxx",
          "node3@xxx.xxx.xxx.xxx",
          "node4@xxx.xxx.xxx.xxx"
        ],
        ...
      }
    }

Now you can ``PUT`` this new metadata:

.. code-block:: bash

    $ curl -X PUT http://localhost/_node/_local/_dbs/{name} -d '{...}'

.. _cluster/sharding/sync:

Forcing synchronization of the shard(s)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. versionadded:: 2.4.0

Whether you pre-copied shards to your new node or not, you can force
CouchDB to synchronize all replicas of all shards in a database with the
:ref:`api/db/sync_shards` endpoint:

.. code-block:: bash

    $ curl -X POST $COUCH_URL:5984/{db}/_sync_shards
    {"ok":true}

This starts the synchronization process. Note that this will put
additional load onto your cluster, which may affect performance.

It is also possible to force synchronization on a per-shard basis by
writing to a document that is stored within that shard.

.. note::

    Admins may want to bump their ``[mem3] sync_concurrency`` value to a
    larger figure for the duration of the shards sync.

.. _cluster/sharding/verify:

Monitor internal replication to ensure up-to-date shard(s)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After you complete the previous step, CouchDB will have started
synchronizing the shards. You can observe this happening by monitoring
the ``/_node/{node-name}/_system`` endpoint, which includes the
``internal_replication_jobs`` metric.

Once this metric has returned to the baseline from before you started
the shard sync, or is ``0``, the shard replica is ready to serve data
and we can bring the node out of maintenance mode.

.. _cluster/sharding/mm-2:

Clear the target node's maintenance mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can now let the node start servicing data requests by
putting ``"false"`` to the maintenance mode configuration endpoint, just
as in step 2.

Verify that the node is not in maintenance mode by performing a ``GET
/_up`` on that node's individual endpoint.

Finally, check that your load balancer has returned the node to the pool
of available backend nodes.

.. _cluster/sharding/remove-shard:

Update cluster metadata again to remove the source shard
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, remove the source shard from the shard map the same way that you
added the new target shard to the shard map in step 2. Be sure to add
the ``["remove", {range}, {source-shard}]`` entry to the end of the
changelog as well as modifying both the ``by_node`` and ``by_range`` sections of
the database metadata document.

.. _cluster/sharding/remove-shard-files:

Remove the shard and secondary index files from the source node
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, you can remove the source shard replica by deleting its file from the
command line on the source host, along with any view shard replicas:

.. code-block:: bash

    $ rm {couch-dir}/data/shards/{range}/{db}.{datecode}.couch
    $ rm -r {couch-dir}/data/.shards/{range}/{db}.{datecode}*

Congratulations! You have moved a database shard replica. By adding and removing
database shard replicas in this way, you can change the cluster's shard layout,
also known as a shard map.

Specifying database placement
-----------------------------

You can configure CouchDB to put shard replicas on certain nodes at
database creation time using placement rules.

.. warning::

    Use of the ``placement`` option will **override** the ``n`` option,
    both in the ``.ini`` file as well as when specified in a ``URL``.

First, each node must be labeled with a zone attribute. This defines which zone
each node is in. You do this by editing the node’s document in the special
``/_nodes`` database, which is accessed through the special node-local API
endpoint at ``/_node/_local/_nodes/{node-name}``. Add a key value pair of the
form:

::

    "zone": "{zone-name}"

Do this for all of the nodes in your cluster. For example:

.. code-block:: bash

    $ curl -X PUT http://localhost/_node/_local/_nodes/{node-name} \
        -d '{ \
            "_id": "{node-name}",
            "_rev": "{rev}",
            "zone": "{zone-name}"
            }'

In the local config file (``local.ini``) of each node, define a
consistent cluster-wide setting like:

::

    [cluster]
    placement = {zone-name-1}:2,{zone-name-2}:1

In this example, CouchDB will ensure that two replicas for a shard will
be hosted on nodes with the zone attribute set to ``{zone-name-1}`` and
one replica will be hosted on a new with the zone attribute set to
``{zone-name-2}``.

This approach is flexible, since you can also specify zones on a per-
database basis by specifying the placement setting as a query parameter
when the database is created, using the same syntax as the ini file:

.. code-block:: bash

    curl -X PUT $COUCH_URL:5984/{db}?zone={zone}

The ``placement`` argument may also be specified. Note that this *will*
override the logic that determines the number of created replicas!

Note that you can also use this system to ensure certain nodes in the
cluster do not host any replicas for newly created databases, by giving
them a zone attribute that does not appear in the ``[cluster]``
placement string.

.. _cluster/sharding/splitting_shards:

Splitting Shards
----------------

The :ref:`api/server/reshard` is an HTTP API for shard manipulation. Currently
it only supports shard splitting. To perform shard merging, refer to the manual
process outlined in the :ref:`cluster/sharding/merging_shards` section.

The main way to interact with :ref:`api/server/reshard` is to create resharding
jobs, monitor those jobs, wait until they complete, remove them, post new jobs,
and so on. What follows are a few steps one might take to use this API to split
shards.

At first, it's a good idea to call ``GET /_reshard`` to see a summary of
resharding on the cluster.

.. code-block:: bash

   $ curl -s $COUCH_URL:5984/_reshard | jq .
   {
     "state": "running",
     "state_reason": null,
     "completed": 3,
     "failed": 0,
     "running": 0,
     "stopped": 0,
     "total": 3
   }

Two important things to pay attention to are the total number of jobs and the state.

The ``state`` field indicates the state of resharding on the cluster. Normally
it would be ``running``, however, another user could have disabled resharding
temporarily. Then, the state would be ``stopped`` and hopefully, there would be
a reason or a comment in the value of the ``state_reason`` field. See
:ref:`cluster/sharding/stop_resharding` for more details.

The ``total`` number of jobs is important to keep an eye on because there is a
maximum number of resharding jobs per node, and creating new jobs after the
limit has been reached will result in an error. Before staring new jobs it's a
good idea to remove already completed jobs. See :ref:`reshard configuration
section <config/reshard>` for the default value of ``max_jobs`` parameter and
how to adjust if needed.

For example, to remove all the completed jobs run:

.. code-block:: bash

    $ for jobid in $(curl -s $COUCH_URL:5984/_reshard/jobs | jq -r '.jobs[] | select (.job_state=="completed") | .id'); do \
          curl -s -XDELETE $COUCH_URL:5984/_reshard/jobs/$jobid \
      done

Then it's a good idea to see what the db shard map looks like.

.. code-block:: bash

    $ curl -s $COUCH_URL:5984/db1/_shards | jq '.'
    {
      "shards": {
        "00000000-7fffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node3@127.0.0.1"
        ],
        "80000000-ffffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node3@127.0.0.1"
        ]
      }
    }

In this example we'll split all the copies of the ``00000000-7fffffff`` range.
The API allows a combination of parameters such as: splitting all
the ranges on all the nodes, all the ranges on just one node, or one particular
range on one particular node. These are specified via the ``db``,
``node`` and ``range`` job parameters.

To split all the copies of ``00000000-7fffffff`` we issue a request like this:

.. code-block:: bash

    $ curl -s -H "Content-type: application/json" -XPOST $COUCH_URL:5984/_reshard/jobs \
      -d '{"type": "split", "db":"db1", "range":"00000000-7fffffff"}' | jq '.'
    [
      {
        "ok": true,
        "id": "001-ef512cfb502a1c6079fe17e9dfd5d6a2befcc694a146de468b1ba5339ba1d134",
        "node": "node1@127.0.0.1",
        "shard": "shards/00000000-7fffffff/db1.1554242778"
      },
      {
        "ok": true,
        "id": "001-cec63704a7b33c6da8263211db9a5c74a1cb585d1b1a24eb946483e2075739ca",
        "node": "node2@127.0.0.1",
        "shard": "shards/00000000-7fffffff/db1.1554242778"
      },
      {
        "ok": true,
        "id": "001-fc72090c006d9b059d4acd99e3be9bb73e986d60ca3edede3cb74cc01ccd1456",
        "node": "node3@127.0.0.1",
        "shard": "shards/00000000-7fffffff/db1.1554242778"
      }
    ]

The request returned three jobs, one job for each of the three copies.

To check progress of these jobs use ``GET /_reshard/jobs`` or ``GET
/_reshard/jobs/{jobid}``.

Eventually, these jobs should complete and the shard map should look like this:

.. code-block:: bash

    $ curl -s $COUCH_URL:5984/db1/_shards | jq '.'
    {
      "shards": {
        "00000000-3fffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node3@127.0.0.1"
        ],
        "40000000-7fffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node3@127.0.0.1"
        ],
        "80000000-ffffffff": [
          "node1@127.0.0.1",
          "node2@127.0.0.1",
          "node3@127.0.0.1"
        ]
      }
    }

.. _cluster/sharding/stop_resharding:

Stopping Resharding Jobs
------------------------

Resharding at the cluster level could be stopped and then restarted. This can
be helpful to allow external tools which manipulate the shard map to avoid
interfering with resharding jobs. To stop all resharding jobs on a cluster
issue a ``PUT`` to ``/_reshard/state`` endpoint with the ``"state": "stopped"``
key and value. You can also specify an optional note or reason for stopping.

For example:

.. code-block:: bash

    $ curl -s -H "Content-type: application/json" \
      -XPUT $COUCH_URL:5984/_reshard/state \
      -d '{"state": "stopped", "reason":"Moving some shards"}'
    {"ok": true}

This state will then be reflected in the global summary:

.. code-block:: bash

   $ curl -s $COUCH_URL:5984/_reshard | jq .
   {
     "state": "stopped",
     "state_reason": "Moving some shards",
     "completed": 74,
     "failed": 0,
     "running": 0,
     "stopped": 0,
     "total": 74
   }

To restart, issue a ``PUT`` request like above with ``running`` as the state.
That should resume all the shard splitting jobs since their last checkpoint.

See the API reference for more details: :ref:`api/server/reshard`.

.. _cluster/sharding/merging_shards:

Merging Shards
--------------

The ``q`` value for a database can be set when the database is created or it
can be increased later by splitting some of the shards
:ref:`cluster/sharding/splitting_shards`. In order to decrease ``q`` and merge
some shards together, the database must be regenerated. Here are the steps:

1. If there are running shard splitting jobs on the cluster, stop them via the
   HTTP API :ref:`cluster/sharding/stop_resharding`.
2. Create a temporary database with the desired shard settings, by
   specifying the q value as a query parameter during the PUT
   operation.
3. Stop clients accessing the database.
4. Replicate the primary database to the temporary one. Multiple
   replications may be required if the primary database is under
   active use.
5. Delete the primary database. **Make sure nobody is using it!**
6. Recreate the primary database with the desired shard settings.
7. Clients can now access the database again.
8. Replicate the temporary back to the primary.
9. Delete the temporary database.

Once all steps have completed, the database can be used again. The
cluster will create and distribute its shards according to placement
rules automatically.

Downtime can be avoided in production if the client application(s) can
be instructed to use the new database instead of the old one, and a cut-
over is performed during a very brief outage window.
