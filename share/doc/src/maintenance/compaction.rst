.. Licensed under the Apache License, Version 2.0 (the "License")you may not
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

.. _compact:

Compaction
==========

The `compaction` operation is the way to reduce disk space usage by removing
unused and old data from database or view index files. This operation is a very
similar to the `vacuum` (`SQLite`_ ex.) available for other database management
systems.

.. _SQLite: http://www.sqlite.org/lang_vacuum.html

During compaction of the `target` CouchDB creates new file with the ``.compact``
extension and transfers only actual data into. Because of this, CouchDB checks
first for the available disk space - it should be *twice greater* than the
compacted file's data.

When all actual data successful transferred to the `compacted` file CouchDB
*removes* the `target` replacing him with it.


.. _compact/db:

Database Compaction
-------------------

Database compaction compresses the database file by removing unused file
sections created during updates. Old documents revisions are replaced with
small amount of metadata called `tombstone` which are used for conflicts
resolution during replication. The number of stored revisions
(and their `tombstones`) can be configured by using the :http:get:`_revs_limit
</{db}/_revs_limit>` URL endpoint.

Compaction is manually triggered operation per database and runs as a background
task. To start it for specific database there is need to send HTTP
:http:post:`/{db}/_compact` sub-resource of the target database::

  curl -H "Content-Type: application/json" -X POST http://localhost:5984/my_db/_compact

On success, HTTP status :http:statuscode:`202` is returned immediately:

.. code-block:: http

  HTTP/1.1 202 Accepted
  Cache-Control: must-revalidate
  Content-Length: 12
  Content-Type: text/plain; charset=utf-8
  Date: Wed, 19 Jun 2013 09:43:52 GMT
  Server: CouchDB/1.3.0 (Erlang OTP/R15B03)

.. code-block:: javascript

  {"ok":true}

Although the request body is not used you must still specify
:http:header:`Content-Type` header with :mimetype:`application/json` value
for the request. If you don't, you will be aware about with HTTP status
:http:statuscode:`415` response:

.. code-block:: http

  HTTP/1.1 415 Unsupported Media Type
  Cache-Control: must-revalidate
  Content-Length: 78
  Content-Type: application/json
  Date: Wed, 19 Jun 2013 09:43:44 GMT
  Server: CouchDB/1.3.0 (Erlang OTP/R15B03)

  {"error":"bad_content_type","reason":"Content-Type must be application/json"}

When the compaction is successful started and running it is possible to get
information about it via :ref:`database information resource <api/db>`::

  curl http://localhost:5984/my_db

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 246
  Content-Type: application/json
  Date: Wed, 19 Jun 2013 16:51:20 GMT
  Server: CouchDB/1.3.0 (Erlang OTP/R15B03)

  {
      "committed_update_seq": 76215,
      "compact_running": true,
      "data_size": 3787996,
      "db_name": "my_db",
      "disk_format_version": 6,
      "disk_size": 17703025,
      "doc_count": 5091,
      "doc_del_count": 0,
      "instance_start_time": "1371660751878859",
      "purge_seq": 0,
      "update_seq": 76215
  }


Note that ``compaction_running`` field is ``true`` indicating that compaction
is actually running. To track the compaction progress you may query the
:http:get:`_active_tasks </_active_tasks>` resource::

  curl http://localhost:5984/my_db

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 175
  Content-Type: application/json
  Date: Wed, 19 Jun 2013 16:27:23 GMT
  Server: CouchDB/1.3.0 (Erlang OTP/R15B03)

  [
      {
          "changes_done": 44461,
          "database": "my_db",
          "pid": "<0.218.0>",
          "progress": 58,
          "started_on": 1371659228,
          "total_changes": 76215,
          "type": "database_compaction",
          "updated_on": 1371659241
      }
  ]


.. _compact/views:

Views Compaction
----------------

`Views` are also need compaction like databases, unlike databases views
are compacted by groups per `design document`. To start their compaction there
is need to send HTTP :http:post:`/{db}/_compact/{ddoc}` request::

  curl -H "Content-Type: application/json" -X POST http://localhost:5984/dbname/_compact/designname

.. code-block:: javascript

  {"ok":true}

This compacts the view index from the current version of the specified design
document. The HTTP response code is :http:statuscode:`202`
(like :ref:`compaction for databases <compact/db>`) and a compaction background
task will be created.


.. _compact/views/cleanup:

Views cleanup
^^^^^^^^^^^^^

View indexes on disk are named after their `MD5` hash of the view definition.
When you change a view, old indexes remain on disk. To clean up all outdated
view indexes (files named after the MD5 representation of views, that does not
exist anymore) you can trigger a :ref:`view cleanup <api/db/view_cleanup>`::

  curl -H "Content-Type: application/json" -X POST http://localhost:5984/dbname/_view_cleanup

.. code-block:: javascript

  {"ok":true}


.. _compact/auto:

Automatic Compaction
--------------------

While both :ref:`database <compact/db>` and :ref:`views <compact/views>`
compactions are required be manually triggered, it is also possible to configure
automatic compaction, so that compaction of databases and views is automatically
triggered based on various criteria. Automatic compaction is configured in
CouchDB's :ref:`configuration files <config/intro>`.

The :ref:`compaction daemon <config/daemons/compaction_daemon>` is responsible
for triggering the compaction. It is automatically started, but disabled by
default. The criteria for triggering the compactions is configured in the
:ref:`compactions <config/compactions>` section.
