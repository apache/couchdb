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

.. _partitioned-dbs:

=====================
Partitioned Databases
=====================

A partitioned database forms documents into logical partitions by using
a partition key. All documents are assigned to a partition, and many documents
are typically given the same partition key. The benefit of partitioned databases
is that secondary indices can be significantly more efficient when locating
matching documents since their entries are contained within their partition.
This means a given secondary index read will only scan a single partition
range instead of having to read from a copy of every shard.

As a means to introducing partitioned databases, we'll consider a motivating
use case to describe the benefits of this feature. For this example, we'll
consider a database that stores readings from a large network of soil
moisture sensors.

.. note::
    Before reading this document you should be familiar with the
    :ref:`theory <cluster/theory>` of :ref:`sharding <cluster/sharding>`
    in CouchDB.

Traditionally, a document in this database may have something like the
following structure:

.. code-block:: javascript

    {
        "_id": "sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
        "_rev":"1-14e8f3262b42498dbd5c672c9d461ff0",
        "sensor_id": "sensor-260",
        "location": [41.6171031, -93.7705674],
        "field_name": "Bob's Corn Field #5",
        "readings": [
            ["2019-01-21T00:00:00", 0.15],
            ["2019-01-21T06:00:00", 0.14],
            ["2019-01-21T12:00:00", 0.16],
            ["2019-01-21T18:00:00", 0.11]
        ]
    }

.. note::
    While this example uses IoT sensors, the main thing to consider is that
    there is a logical grouping of documents. Similar use cases might be
    documents grouped by user or scientific data grouped by experiment.

So we've got a bunch of sensors, all grouped by the field they monitor
along with their readouts for a given day (or other appropriate time period).

Along with our documents, we might expect to have two secondary indexes
for querying our database that might look something like:

.. code-block:: javascript

    function(doc) {
        if(doc._id.indexOf("sensor-reading-") != 0) {
            return;
        }
        for(var r in doc.readings) {
            emit([doc.sensor_id, r[0]], r[1])
        }
    }

and:

.. code-block:: javascript

    function(doc) {
        if(doc._id.indexOf("sensor-reading-") != 0) {
            return;
        }
        emit(doc.field_name, doc.sensor_id)
    }

With these two indexes defined, we can easily find all readings for a given
sensor, or list all sensors in a given field.

Unfortunately, in CouchDB, when we read from either of these indexes, it
requires finding a copy of every shard and asking for any documents related
to the particular sensor or field. This means that as our database scales
up the number of shards, every index request must perform more work,
which is unnecessary since we are only interested in a small number of documents.
Fortunately for you, dear reader, partitioned databases were created to solve
this precise problem.

What is a partition?
====================

In the previous section, we introduced a hypothetical database that contains
sensor readings from an IoT field monitoring service. In this particular
use case, it's quite logical to group all documents by their ``sensor_id``
field. In this case, we would call the ``sensor_id`` the partition key.

A good partition has two basic properties. First, it should have a high
cardinality. That is, a large partitioned database should have many more
partitions than documents in any single partition. A database that has
a single partition would be an anti-pattern for this feature. Secondly,
the amount of data per partition should be "small". The general
recommendation is to limit individual partitions to less than ten
gigabytes (10 GB) of data. Which, for the example of sensor documents,
equates to roughly 60,000 years of data.

.. note::

    The ``max_partition_size`` under CouchDB dictates the partition limit.
    The default value for this option is 10GiB but can be changed accordingly.
    Setting the value for this option to 0 disables the partition limit.

Why use partitions?
===================

The primary benefit of using partitioned databases is for the performance
of partitioned queries. Large databases with lots of documents often
have a similar pattern where there are groups of related documents that
are queried together.

By using partitions, we can execute queries against these individual groups
of documents more efficiently by placing the entire group within a specific
shard on disk. Thus, the view engine only has to consult one copy of the
given shard range when executing a query instead of executing
the query across all ``q`` shards in the database. This mean that you do
not have to wait for all ``q`` shards to respond, which is both
efficient and faster.

Partitions By Example
=====================

To create a partitioned database, we simply need to pass a query string
parameter:

.. code-block:: bash

    shell> curl -X PUT http://127.0.0.1:5984/my_new_db?partitioned=true
    {"ok":true}

To see that our database is partitioned, we can look at the database
information:

.. code-block:: bash

    shell> curl http://127.0.0.1:5984/my_new_db
    {
      "cluster": {
        "n": 3,
        "q": 8,
        "r": 2,
        "w": 2
      },
      "compact_running": false,
      "db_name": "my_new_db",
      "disk_format_version": 7,
      "doc_count": 0,
      "doc_del_count": 0,
      "instance_start_time": "0",
      "props": {
        "partitioned": true
      },
      "purge_seq": "0-g1AAAAFDeJzLYWBg4M...",
      "sizes": {
        "active": 0,
        "external": 0,
        "file": 66784
      },
      "update_seq": "0-g1AAAAFDeJzLYWBg4M..."
    }

You'll now see that the ``"props"`` member contains ``"partitioned": true``.

.. note::

    Every document in a partitioned database (except _design
    and _local documents) must have the format “partition:docid”.
    More specifically, the partition for a given document is
    everything before the first colon. The document id is everything
    after the first colon, which may include more colons.

.. note::

    System databases (such as _users) are *not* allowed to be partitioned. This is
    due to system databases already having their own incompatible
    requirements on document ids.

Now that we've created a partitioned database, it's time to add some documents.
Using our earlier example, we could do this as such:

.. code-block:: bash

    shell> cat doc.json
    {
        "_id": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
        "sensor_id": "sensor-260",
        "location": [41.6171031, -93.7705674],
        "field_name": "Bob's Corn Field #5",
        "readings": [
            ["2019-01-21T00:00:00", 0.15],
            ["2019-01-21T06:00:00", 0.14],
            ["2019-01-21T12:00:00", 0.16],
            ["2019-01-21T18:00:00", 0.11]
        ]
    }
    shell> $ curl -X POST -H "Content-Type: application/json" \
                http://127.0.0.1:5984/my_new_db -d @doc.json
    {
        "ok": true,
        "id": "sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
        "rev": "1-05ed6f7abf84250e213fcb847387f6f5"
    }

The only change required to the first example document is that we are now
including the partition name in the document id by prepending it to the
old id separated by a colon.

.. note::

    The partition name in the document id is not magical. Internally,
    the database is simply using only the partition for hashing
    the document to a given shard, instead of the entire document id.

Working with documents in a partitioned database is no different than
a non-partitioned database. All APIs are available, and existing client
code will all work seamlessly.

Now that we have created a document, we can get some info about the partition
containing the document:

.. code-block:: bash

    shell> curl http://127.0.0.1:5984/my_new_db/_partition/sensor-260
    {
      "db_name": "my_new_db",
      "doc_count": 1,
      "doc_del_count": 0,
      "partition": "sensor-260",
      "sizes": {
        "active": 244,
        "external": 347
      }
    }

And we can also list all documents in a partition:

.. code-block:: bash

    shell> curl http://127.0.0.1:5984/my_new_db/_partition/sensor-260/_all_docs
    {"total_rows": 1, "offset": 0, "rows":[
        {
            "id":"sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
            "key":"sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf",
            "value": {"rev": "1-05ed6f7abf84250e213fcb847387f6f5"}
        }
    ]}

Note that we can use all of the normal bells and whistles available to
``_all_docs`` requests. Accessing ``_all_docs`` through the
``/dbname/_partition/name/_all_docs`` endpoint is mostly a convenience
so that requests are guaranteed to be scoped to a given partition. Users
are free to use the normal ``/dbname/_all_docs`` to read documents from
multiple partitions. Both query styles have the same performance.

Next, we'll create a design document containing our index for
getting all readings from a given sensor. The map function is similar to
our earlier example except we've accounted for the change in the document
id.

.. code-block:: javascript

    function(doc) {
        if(doc._id.indexOf(":sensor-reading-") < 0) {
            return;
        }
        for(var r in doc.readings) {
            emit([doc.sensor_id, r[0]], r[1])
        }
    }

After uploading our design document, we can try out a partitioned query:

.. code-block:: bash

    shell> cat ddoc.json
    {
        "_id": "_design/sensor-readings",
        "views": {
            "by_sensor": {
                "map": "function(doc) { ... }"
            }
        }
    }
    shell> $ curl -X POST -H "Content-Type: application/json" http://127.0.0.1:5984/my_new_db -d @ddoc2.json
    {
        "ok": true,
        "id": "_design/all_sensors",
        "rev": "1-4a8188d80fab277fccf57bdd7154dec1"
    }
    shell> curl http://127.0.0.1:5984/my_new_db/_partition/sensor-260/_design/sensor-readings/_view/by_sensor
    {"total_rows":4,"offset":0,"rows":[
    {"id":"sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf","key":["sensor-260","0"],"value":null},
    {"id":"sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf","key":["sensor-260","1"],"value":null},
    {"id":"sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf","key":["sensor-260","2"],"value":null},
    {"id":"sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf","key":["sensor-260","3"],"value":null}
    ]}

Hooray! Our first partitioned query. For experienced users, that may not
be the most exciting development, given that the only things that have
changed are a slight tweak to the document id, and accessing views with
a slightly different path. However, for anyone who likes performance
improvements, it's actually a big deal. By knowing that the view results
are all located within the provided partition name, our partitioned
queries now perform nearly as fast as document lookups!

The last thing we'll look at is how to query data across multiple partitions.
For that, we'll implement the example sensors by field query from our
initial example. The map function will use the same update to account
for the new document id format, but is otherwise identical to the previous
version:

.. code-block:: javascript

    function(doc) {
        if(doc._id.indexOf(":sensor-reading-") < 0) {
            return;
        }
        emit(doc.field_name, doc.sensor_id)
    }

Next, we'll create a new design doc with this function. Be sure to notice
that the ``"options"`` member contains ``"partitioned": false``.

.. code-block:: bash

    shell> cat ddoc2.json
    {
      "_id": "_design/all_sensors",
      "options": {
        "partitioned": false
      },
      "views": {
        "by_field": {
          "map": "function(doc) { ... }"
        }
      }
    }
    shell> $ curl -X POST -H "Content-Type: application/json" http://127.0.0.1:5984/my_new_db -d @ddoc2.json
    {
        "ok": true,
        "id": "_design/all_sensors",
        "rev": "1-4a8188d80fab277fccf57bdd7154dec1"
    }

.. note::

    Design documents in a partitioned database default to being
    partitioned. Design documents that contain views for queries
    across multiple partitions must contain the ``"partitioned": false``
    member in the ``"options"`` object.

.. note::

    Design documents are either partitioned or global. They cannot
    contain a mix of partitioned and global indexes.

And to see a request showing us all sensors in a field, we would use a
request like:

.. code-block:: bash

    shell> curl -u adm:pass http://127.0.0.1:15984/my_new_db/_design/all_sensors/_view/by_field
    {"total_rows":1,"offset":0,"rows":[
    {"id":"sensor-260:sensor-reading-ca33c748-2d2c-4ed1-8abf-1bca4d9d03cf","key":"Bob's Corn Field #5","value":"sensor-260"}
    ]}

Notice that we're not using the ``/dbname/_partition/...`` path for global
queries. This is because global queries, by definition, do not cover individual
partitions. Other than having the ``"partitioned": false`` parameter in the
design document, global design documents and queries are identical in
behavior to design documents on non-partitioned databases.

.. warning::

    To be clear, this means that global queries perform identically to
    queries on non-partitioned databases. Only partitioned queries
    on a partitioned database benefit from the performance improvements.
