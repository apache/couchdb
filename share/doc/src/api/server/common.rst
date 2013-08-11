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


.. _api/server/root:

``/``
=====

.. http:get:: /

  Accessing the root of a CouchDB instance returns meta information about
  the instance. The response is a JSON structure containing information
  about the server, including a welcome message and the version of the
  server.

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET / HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 179
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 06:33:33 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    {
        "couchdb": "Welcome",
        "uuid": "85fb71bf700c17267fef77535820e371",
        "vendor": {
            "name": "The Apache Software Foundation",
            "version": "1.3.1"
        },
        "version": "1.3.1"
    }


.. _api/server/active_tasks:

``/_active_tasks``
==================

.. http:get:: /_active_tasks

  List of running tasks, including the task type, name, status
  and process ID. The result is a JSON array of the currently running tasks,
  with each task being described with a single object. Depending on operation
  type set of response object fields might be different.

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json number changes_done: Processed changes
  :>json string database: Source database
  :>json string pid: Process ID
  :>json number progress: Current percentage progress
  :>json number started_on: Task start time as unix timestamp
  :>json string status: Task status message
  :>json string task: Task name
  :>json number total_changes: Total changes to process
  :>json string type: Operation Type
  :>json number updated_on: Unix timestamp of last operation update
  :code 200: Request completed successfully
  :code 401: Administrator's privileges required

  **Request**:

  .. code-block:: http

    GET /_active_tasks HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 1690
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 06:37:31 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    [
        {
            "changes_done": 64438,
            "database": "mailbox",
            "pid": "<0.12986.1>",
            "progress": 84,
            "started_on": 1376116576,
            "total_changes": 76215,
            "type": "database_compaction",
            "updated_on": 1376116619
        },
        {
            "changes_done": 14443,
            "database": "mailbox",
            "design_document": "c9753817b3ba7c674d92361f24f59b9f",
            "pid": "<0.10461.3>",
            "progress": 18,
            "started_on": 1376116621,
            "total_changes": 76215,
            "type": "indexer",
            "updated_on": 1376116650
        },
        {
            "changes_done": 5454,
            "database": "mailbox",
            "design_document": "_design/meta",
            "pid": "<0.6838.4>",
            "progress": 7,
            "started_on": 1376116632,
            "total_changes": 76215,
            "type": "indexer",
            "updated_on": 1376116651
        },
        {
            "checkpointed_source_seq": 68585,
            "continuous": false,
            "doc_id": null,
            "doc_write_failures": 0,
            "docs_read": 4524,
            "docs_written": 4524,
            "missing_revisions_found": 4524,
            "pid": "<0.1538.5>",
            "progress": 44,
            "replication_id": "9bc1727d74d49d9e157e260bb8bbd1d5",
            "revisions_checked": 4524,
            "source": "mailbox",
            "source_seq": 154419,
            "started_on": 1376116644,
            "target": "http://mailsrv:5984/mailbox",
            "type": "replication",
            "updated_on": 1376116651
        }
    ]


.. _api/server/all_dbs:

``/_all_dbs``
=============

.. http:get:: /_all_dbs

  Returns a list of all the databases in the CouchDB instance.

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET /_all_dbs HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 52
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 06:57:48 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    [
       "_users",
       "contacts",
       "docs",
       "invoices",
       "locations"
    ]


.. _api/server/db_updates:

``/_db_updates``
================

.. versionadded:: 1.4

.. http:get:: /_db_updates

  Returns a list of all database events in the CouchDB instance.

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :query string feed: - **longpoll**: Closes the connection after the first event.
    - **continuous**: Send a line of JSON per event. Keeps the socket open
      until ``timeout``.
    - **eventsource**: Like, ``continuous``, but sends the events in
      `EventSource <http://dev.w3.org/html5/eventsource/>`_ format.
  :query number timeout: Number of seconds until CouchDB closes the connection.
    Default is ``60``.
  :query boolean heartbeat: Whether CouchDB will send a newline character
    (``\n``) on ``timeout``. Default is ``true``.
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header Transfer-Encoding: ``chunked``
  :>json string db_name: Database name
  :>json boolean ok: Event operation status
  :>json string type: A database event is one of ``created``, ``updated``,
    ``deleted``
  :code 200: Request completed successfully.
  :code 401: Administrator's privileges required.

  **Request**:

  .. code-block:: http

    GET /_db_updates HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 07:02:41 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)
    Transfer-Encoding: chunked

    {
        "db_name": "mailbox",
        "ok": true,
        "type": "created"
    }


.. _api/server/log:

``/_log``
=========

.. http:get:: /_log

  Gets the CouchDB log, equivalent to accessing the local log file of the
  corresponding CouchDB instance.

  :<header Accept: - :mimetype:`text/plain`
  :query number bytes: Bytes to be returned. Default is ``1000``.
  :query number offset: Offset in bytes where the log tail should be started.
    Default is ``0``.
  :>header Content-Type: :mimetype:`text/plain; charset=utf-8`
  :>header Transfer-Encoding: ``chunked``
  :code 200: Request completed successfully.
  :code 401: Administrator's privileges required.

  **Request**:

  .. code-block:: http

    GET /_log HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: text

    [Wed, 27 Oct 2010 10:49:42 GMT] [info] [<0.23338.2>] 192.168.0.2 - - 'PUT' /authdb 401
    [Wed, 27 Oct 2010 11:02:19 GMT] [info] [<0.23428.2>] 192.168.0.116 - - 'GET' /recipes/FishStew 200
    [Wed, 27 Oct 2010 11:02:19 GMT] [info] [<0.23428.2>] 192.168.0.116 - - 'GET' /_session 200
    [Wed, 27 Oct 2010 11:02:19 GMT] [info] [<0.24199.2>] 192.168.0.116 - - 'GET' / 200
    [Wed, 27 Oct 2010 13:03:38 GMT] [info] [<0.24207.2>] 192.168.0.116 - - 'GET' /_log?offset=5 200

If you want to pick out specific parts of the log information you can
use the ``bytes`` argument, which specifies the number of bytes to be
returned, and ``offset``, which specifies where the reading of the log
should start, counted back from the end. For example, if you use the
following request:

.. code-block:: http

  GET /_log?bytes=500&offset=2000

Reading of the log will start at 2000 bytes from the end of the log, and
500 bytes will be shown.

**How bytes/offset works?**

CouchDB reads specified amount of ``bytes`` from the end of log file,
jumping to ``offset`` bytes towards the beginning of the file first:

.. code-block:: text

   Log File    FilePos
   ----------
  |          |  10
  |          |  20
  |          |  30
  |          |  40
  |          |  50
  |          |  60
  |          |  70 -- Bytes = 20  --
  |          |  80                 | Chunk
  |          |  90 -- Offset = 10 --
  |__________| 100



.. _api/server/replicate:

``/_replicate``
===============

.. http:post:: /_replicate

  Request, configure, or stop, a replication operation.

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :<json boolean cancel: Cancels the replication
  :<json boolean continuous: Configure the replication to be continuous
  :<json boolean create_target: Creates the target database.
    Required administrator's privileges on target server.
  :<json array doc_ids: Array of document IDs to be synchronized
  :<json string proxy: Address of a proxy server through which replication
    should occur
  :<json string source: Source database name or URL
  :<json string target: Target database name or URL
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json array history: Replication history (see below)
  :>json boolean ok: Replication status
  :>json number replication_id_version: Replication protocol version
  :>json string session_id: Unique session ID
  :>json number source_last_seq: Last sequence number read from source database
  :code 200: Replication request successfully completed
  :code 202: Continuous replication request has been accepted
  :code 400: Invalid JSON data
  :code 401: Administrator's privileges required
  :code 404: Either the source or target DB is not found or attempt to
    cancel unknown replication task
  :code 500: JSON specification was invalid

  The specification of the replication request is controlled through the
  JSON content of the request. The JSON should be an object with the
  fields defining the source, target and other options.

  The `Replication history` is an array of objects with following structure:

  :json number doc_write_failures: Number of document write failures
  :json number docs_read:  Number of documents read
  :json number docs_written:  Number of documents written to target
  :json number end_last_seq:  Last sequence number in changes stream
  :json string end_time:  Date/Time replication operation completed in
    :rfc:`2822` format
  :json number missing_checked:  Number of missing documents checked
  :json number missing_found:  Number of missing documents found
  :json number recorded_seq:  Last recorded sequence number
  :json string session_id:  Session ID for this replication operation
  :json number start_last_seq:  First sequence number in changes stream
  :json string start_time:  Date/Time replication operation started in
    :rfc:`2822` format

  **Request**

  .. code-block:: http

    POST /_replicate HTTP/1.1
    Accept: application/json
    Content-Length: 36
    Content-Type: application/json
    Host: localhost:5984

    {
        "source": "db_a",
        "target": "db_b"
    }

  **Response**

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 692
    Content-Type: application/json
    Date: Sun, 11 Aug 2013 20:38:50 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "history": [
            {
                "doc_write_failures": 0,
                "docs_read": 10,
                "docs_written": 10,
                "end_last_seq": 28,
                "end_time": "Sun, 11 Aug 2013 20:38:50 GMT",
                "missing_checked": 10,
                "missing_found": 10,
                "recorded_seq": 28,
                "session_id": "142a35854a08e205c47174d91b1f9628",
                "start_last_seq": 1,
                "start_time": "Sun, 11 Aug 2013 20:38:50 GMT"
            },
            {
                "doc_write_failures": 0,
                "docs_read": 1,
                "docs_written": 1,
                "end_last_seq": 1,
                "end_time": "Sat, 10 Aug 2013 15:41:54 GMT",
                "missing_checked": 1,
                "missing_found": 1,
                "recorded_seq": 1,
                "session_id": "6314f35c51de3ac408af79d6ee0c1a09",
                "start_last_seq": 0,
                "start_time": "Sat, 10 Aug 2013 15:41:54 GMT"
            }
        ],
        "ok": true,
        "replication_id_version": 3,
        "session_id": "142a35854a08e205c47174d91b1f9628",
        "source_last_seq": 28
    }


Replication Operation
---------------------

The aim of the replication is that at the end of the process, all active
documents on the source database are also in the destination database
and all documents that were deleted in the source databases are also
deleted (if they exist) on the destination database.

Replication can be described as either push or pull replication:

-  *Pull replication* is where the ``source`` is the remote CouchDB
   instance, and the ``target`` is the local database.

   Pull replication is the most useful solution to use if your source
   database has a permanent IP address, and your destination (local)
   database may have a dynamically assigned IP address (for example,
   through DHCP). This is particularly important if you are replicating
   to a mobile or other device from a central server.

-  *Push replication* is where the ``source`` is a local database, and
   ``target`` is a remote database.

Specifying the Source and Target Database
-----------------------------------------

You must use the URL specification of the CouchDB database if you want
to perform replication in either of the following two situations:

-  Replication with a remote database (i.e. another instance of CouchDB
   on the same host, or a different host)

-  Replication with a database that requires authentication

For example, to request replication between a database local to the
CouchDB instance to which you send the request, and a remote database
you might use the following request:

.. code-block:: http

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "source" : "recipes",
       "target" : "http://coucdb-remote:5984/recipes",
    }


In all cases, the requested databases in the ``source`` and ``target``
specification must exist. If they do not, an error will be returned
within the JSON object:

.. code-block:: javascript

    {
       "error" : "db_not_found"
       "reason" : "could not open http://couchdb-remote:5984/ol1ka/",
    }

You can create the target database (providing your user credentials
allow it) by adding the ``create_target`` field to the request object:

.. code-block:: http

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "create_target" : true
       "source" : "recipes",
       "target" : "http://couchdb-remote:5984/recipes",
    }

The ``create_target`` field is not destructive. If the database already
exists, the replication proceeds as normal.

Single Replication
------------------

You can request replication of a database so that the two databases can
be synchronized. By default, the replication process occurs one time and
synchronizes the two databases together. For example, you can request a
single synchronization between two databases by supplying the ``source``
and ``target`` fields within the request JSON content.

.. code-block:: http

    POST http://couchdb:5984/_replicate
    Accept: application/json
    Content-Type: application/json

    {
       "source" : "recipes",
       "target" : "recipes-snapshot",
    }

In the above example, the databases ``recipes`` and ``recipes-snapshot``
will be synchronized. These databases are local to the CouchDB instance
where the request was made. The response will be a JSON structure
containing the success (or failure) of the synchronization process, and
statistics about the process:

.. code-block:: javascript

    {
       "ok" : true,
       "history" : [
          {
             "docs_read" : 1000,
             "session_id" : "52c2370f5027043d286daca4de247db0",
             "recorded_seq" : 1000,
             "end_last_seq" : 1000,
             "doc_write_failures" : 0,
             "start_time" : "Thu, 28 Oct 2010 10:24:13 GMT",
             "start_last_seq" : 0,
             "end_time" : "Thu, 28 Oct 2010 10:24:14 GMT",
             "missing_checked" : 0,
             "docs_written" : 1000,
             "missing_found" : 1000
          }
       ],
       "session_id" : "52c2370f5027043d286daca4de247db0",
       "source_last_seq" : 1000
    }

Continuous Replication
----------------------

Synchronization of a database with the previously noted methods happens
only once, at the time the replicate request is made. To have the target
database permanently replicated from the source, you must set the
``continuous`` field of the JSON object within the request to true.

With continuous replication changes in the source database are
replicated to the target database in perpetuity until you specifically
request that replication ceases.

.. code-block:: http

    POST http://couchdb:5984/_replicate
    Accept: application/json
    Content-Type: application/json

    {
       "continuous" : true
       "source" : "recipes",
       "target" : "http://couchdb-remote:5984/recipes",
    }

Changes will be replicated between the two databases as long as a
network connection is available between the two instances.

.. note::
   Two keep two databases synchronized with each other, you need to set
   replication in both directions; that is, you must replicate from
   ``source`` to ``target``, and separately from ``target`` to
   ``source``.

Canceling Continuous Replication
--------------------------------

You can cancel continuous replication by adding the ``cancel`` field to
the JSON request object and setting the value to true. Note that the
structure of the request must be identical to the original for the
cancellation request to be honoured. For example, if you requested
continuous replication, the cancellation request must also contain the
``continuous`` field.

For example, the replication request:

.. code-block:: http

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "source" : "recipes",
       "target" : "http://couchdb-remote:5984/recipes",
       "create_target" : true,
       "continuous" : true
    }

Must be canceled using the request:

.. code-block:: http

    POST http://couchdb:5984/_replicate
    Accept: application/json
    Content-Type: application/json

    {
        "cancel" : true,
        "continuous" : true
        "create_target" : true,
        "source" : "recipes",
        "target" : "http://couchdb-remote:5984/recipes",
    }

Requesting cancellation of a replication that does not exist results in
a 404 error.


.. _api/server/restart:

``/_restart``
=============

.. http:post:: /_restart

  Restarts the CouchDB instance. You must be authenticated as a user with
  administration privileges for this to work.

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :code 202: Server goes to restart (there is no guarantee that it will be
    alive after)
  :code 401: Administrator's privileges required
  :code 415: Bad request`s :http:header:`Content-Type`

  **Request**:

  .. code-block:: http

    POST /_restart HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 202 Accepted
    Cache-Control: must-revalidate
    Content-Length: 12
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 11:33:50 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    {
        "ok": true
    }


.. _api/server/stats:

``/_stats``
===========

.. http:get:: /_stats

  The ``_stats`` resource returns a JSON object containing the statistics
  for the running server. The object is structured with top-level sections
  collating the statistics for a range of entries, with each individual
  statistic being easily identified, and the content of each statistic is
  self-describing

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET /_stats/couchdb/request_time HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 187
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 11:41:11 GMT
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    {
        "couchdb": {
            "request_time": {
                "current": 21.0,
                "description": "length of a request inside CouchDB without MochiWeb",
                "max": 19.0,
                "mean": 7.0,
                "min": 1.0,
                "stddev": 10.392,
                "sum": 21.0
            }
        }
    }


The fields provide the current, minimum and maximum, and a collection of
statistical means and quantities. The quantity in each case is not
defined, but the descriptions below provide

The statistics are divided into the following top-level sections:

``couchdb``
-----------

Describes statistics specific to the internals of CouchDB

+-------------------------+-------------------------------------------------------+----------------+
| Statistic ID            | Description                                           | Unit           |
+=========================+=======================================================+================+
| ``auth_cache_hits``     | Number of authentication cache hits                   | number         |
+-------------------------+-------------------------------------------------------+----------------+
| ``auth_cache_misses``   | Number of authentication cache misses                 | number         |
+-------------------------+-------------------------------------------------------+----------------+
| ``database_reads``      | Number of times a document was read from a database   | number         |
+-------------------------+-------------------------------------------------------+----------------+
| ``database_writes``     | Number of times a database was changed                | number         |
+-------------------------+-------------------------------------------------------+----------------+
| ``open_databases``      | Number of open databases                              | number         |
+-------------------------+-------------------------------------------------------+----------------+
| ``open_os_files``       | Number of file descriptors CouchDB has open           | number         |
+-------------------------+-------------------------------------------------------+----------------+
| ``request_time``        | Length of a request inside CouchDB without MochiWeb   | milliseconds   |
+-------------------------+-------------------------------------------------------+----------------+

``httpd_request_methods``
-------------------------

+----------------+----------------------------------+----------+
| Statistic ID   | Description                      | Unit     |
+================+==================================+==========+
| ``COPY``       | Number of HTTP COPY requests     | number   |
+----------------+----------------------------------+----------+
| ``DELETE``     | Number of HTTP DELETE requests   | number   |
+----------------+----------------------------------+----------+
| ``GET``        | Number of HTTP GET requests      | number   |
+----------------+----------------------------------+----------+
| ``HEAD``       | Number of HTTP HEAD requests     | number   |
+----------------+----------------------------------+----------+
| ``POST``       | Number of HTTP POST requests     | number   |
+----------------+----------------------------------+----------+
| ``PUT``        | Number of HTTP PUT requests      | number   |
+----------------+----------------------------------+----------+

``httpd_status_codes``
----------------------

+----------------+------------------------------------------------------+----------+
| Statistic ID   | Description                                          | Unit     |
+================+======================================================+==========+
| ``200``        | Number of HTTP 200 OK responses                      | number   |
+----------------+------------------------------------------------------+----------+
| ``201``        | Number of HTTP 201 Created responses                 | number   |
+----------------+------------------------------------------------------+----------+
| ``202``        | Number of HTTP 202 Accepted responses                | number   |
+----------------+------------------------------------------------------+----------+
| ``301``        | Number of HTTP 301 Moved Permanently responses       | number   |
+----------------+------------------------------------------------------+----------+
| ``304``        | Number of HTTP 304 Not Modified responses            | number   |
+----------------+------------------------------------------------------+----------+
| ``400``        | Number of HTTP 400 Bad Request responses             | number   |
+----------------+------------------------------------------------------+----------+
| ``401``        | Number of HTTP 401 Unauthorized responses            | number   |
+----------------+------------------------------------------------------+----------+
| ``403``        | Number of HTTP 403 Forbidden responses               | number   |
+----------------+------------------------------------------------------+----------+
| ``404``        | Number of HTTP 404 Not Found responses               | number   |
+----------------+------------------------------------------------------+----------+
| ``405``        | Number of HTTP 405 Method Not Allowed responses      | number   |
+----------------+------------------------------------------------------+----------+
| ``409``        | Number of HTTP 409 Conflict responses                | number   |
+----------------+------------------------------------------------------+----------+
| ``412``        | Number of HTTP 412 Precondition Failed responses     | number   |
+----------------+------------------------------------------------------+----------+
| ``500``        | Number of HTTP 500 Internal Server Error responses   | number   |
+----------------+------------------------------------------------------+----------+

``httpd``
---------

+----------------------------------+----------------------------------------------+----------+
| Statistic ID                     | Description                                  | Unit     |
+==================================+==============================================+==========+
| ``bulk_requests``                | Number of bulk requests                      | number   |
+----------------------------------+----------------------------------------------+----------+
| ``clients_requesting_changes``   | Number of clients for continuous _changes    | number   |
+----------------------------------+----------------------------------------------+----------+
| ``requests``                     | Number of HTTP requests                      | number   |
+----------------------------------+----------------------------------------------+----------+
| ``temporary_view_reads``         | Number of temporary view reads               | number   |
+----------------------------------+----------------------------------------------+----------+
| ``view_reads``                   | Number of view reads                         | number   |
+----------------------------------+----------------------------------------------+----------+

You can also access individual statistics by quoting the statistics
sections and statistic ID as part of the URL path. For example, to get
the ``request_time`` statistics, you can use:

.. code-block:: http

    GET /_stats/couchdb/request_time

This returns an entire statistics object, as with the full request, but
containing only the request individual statistic. Hence, the returned
structure is as follows:

.. code-block:: javascript

    {
       "couchdb" : {
          "request_time" : {
             "stddev" : 7454.305,
             "min" : 1,
             "max" : 34185,
             "current" : 34697.803,
             "mean" : 1652.276,
             "sum" : 34697.803,
             "description" : "length of a request inside CouchDB without MochiWeb"
          }
       }
    }


.. _api/server/utils:

``/_utils``
===========

.. http:get:: /_utils

  Accesses the built-in Futon administration interface for CouchDB.

  :>header Location: New URI location
  :code 301: Redirects to :http:get:`/_utils/`

.. http:get:: /_utils/

  :>header Content-Type: :mimetype:`text/html`
  :>header Last-Modified: Static files modification timestamp
  :code 200: Request completed successfully


.. _api/server/uuids:

``/_uuids``
===========

.. http:get:: /_uuids

  Requests one or more Universally Unique Identifiers (UUIDs) from the
  CouchDB instance. The response is a JSON object providing a list of
  UUIDs.

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :query number count: Number of UUIDs to return. Default is ``1``.
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Response hash
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET /_uuids?count=10 HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Content-Length: 362
    Content-Type: application/json
    Date: Sat, 10 Aug 2013 11:46:25 GMT
    ETag: "DGRWWQFLUDWN5MRKSLKQ425XV"
    Expires: Fri, 01 Jan 1990 00:00:00 GMT
    Pragma: no-cache
    Server: CouchDB/1.3.1 (Erlang OTP/R15B02)

    {
        "uuids": [
            "75480ca477454894678e22eec6002413",
            "75480ca477454894678e22eec600250b",
            "75480ca477454894678e22eec6002c41",
            "75480ca477454894678e22eec6003b90",
            "75480ca477454894678e22eec6003fca",
            "75480ca477454894678e22eec6004bef",
            "75480ca477454894678e22eec600528f",
            "75480ca477454894678e22eec6005e0b",
            "75480ca477454894678e22eec6006158",
            "75480ca477454894678e22eec6006161"
        ]
    }

The UUID type is determined by the :ref:`UUID algorithm <config/uuids/algorithm>`
setting in the CouchDB configuration.

The UUID type could be changed in anytime through
:ref:`Config API <api/config/section/key>`. For example, changing the UUID
type to ``random`` use next HTTP request:

.. code-block:: http

    PUT http://couchdb:5984/_config/uuids/algorithm
    Content-Type: application/json
    Accept: */*

    "random"

When obtaining a list of UUIDs you'll see the changes:

.. code-block:: javascript

    {
       "uuids" : [
          "031aad7b469956cf2826fcb2a9260492",
          "6ec875e15e6b385120938df18ee8e496",
          "cff9e881516483911aa2f0e98949092d",
          "b89d37509d39dd712546f9510d4a9271",
          "2e0dbf7f6c4ad716f21938a016e4e59f"
       ]
    }


.. _api/server/favicon:

``/favicon.ico``
================

.. http:get:: /favicon.ico

  Binary content for the `favicon.ico` site icon.

  :>header Content-Type: :mimetype:`image/x-icon`
  :code 200: Request completed successfully
  :code 404: The requested content could not be found
