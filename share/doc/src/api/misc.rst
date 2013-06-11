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

.. _api/misc:

=====================
Miscellaneous Methods
=====================

The CouchDB Miscellaneous interface provides the basic interface to a
CouchDB server for obtaining CouchDB information and getting and setting
configuration information.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /                       |  Get the welcome message and version      |
|        |                         |  information                              |
+--------+-------------------------+-------------------------------------------+
| GET    | /_active_tasks          |  Obtain a list of the tasks running in the|
|        |                         |  server                                   |
+--------+-------------------------+-------------------------------------------+
| GET    | /_all_dbs               |  Get a list of all the DBs                |
+--------+-------------------------+-------------------------------------------+
| GET    | /_db_updates            |  A feed of database events                |
+--------+-------------------------+-------------------------------------------+
| GET    | /_log                   |  Return the server log file               |
+--------+-------------------------+-------------------------------------------+
| POST   | /_replicate             |  Set or cancel replication                |
+--------+-------------------------+-------------------------------------------+
| POST   | /_restart               |  Restart the server                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_stats                 |  Return server statistics                 |
+--------+-------------------------+-------------------------------------------+
| GET    | /_utils                 |  CouchDB administration interface (Futon) |
+--------+-------------------------+-------------------------------------------+
| GET    | /_uuids                 |  Get generated UUIDs from the server      |
+--------+-------------------------+-------------------------------------------+
| GET    | /favicon.ico            |  Get the site icon                        |
+--------+-------------------------+-------------------------------------------+

.. _api/misc/root:
.. _api/misc/root.get:

``GET /``
=========

* **Method**: ``GET /``
* **Request**: None
* **Response**: Welcome message and version
* **Admin Privileges Required**: no
* **Return Codes**:

  * **200**:
    Request completed successfully.

Accessing the root of a CouchDB instance returns meta information about
the instance. The response is a JSON structure containing information
about the server, including a welcome message and the version of the
server.

.. code-block:: javascript

    {
       "couchdb" : "Welcome",
       "version" : "1.0.1"
    }

.. _api/misc/active_tasks:
.. _api/misc/active_tasks.get:

``GET /_active_tasks``
======================

* **Method**: ``GET /_active_tasks``
* **Request**: None
* **Response**: List of running tasks, including the task type, name, status
  and process ID
* **Admin Privileges Required**: yes
* **Return Codes**:

  * **200**:
    Request completed successfully.

You can obtain a list of active tasks by using the ``/_active_tasks``
URL. The result is a JSON array of the currently running tasks, with
each task being described with a single object. For example:

.. code-block:: javascript

    [
       {
        "pid" : "<0.11599.0>",
        "status" : "Copied 0 of 18369 changes (0%)",
        "task" : "recipes",
        "type" : "Database Compaction"
        }
    ]

The returned structure includes the following fields for each task:

* **tasks** [array]: Active Task

  * **pid**:Process ID
  * **status**: Task status message
  * **task**: Task name
  * **type**: Operation Type

For operation type, valid values include:

-  ``Database Compaction``

-  ``Replication``

-  ``View Group Compaction``

-  ``View Group Indexer``

.. _api/misc/all_dbs:
.. _api/misc/all_dbs.get:

``GET /_all_dbs``
=================

* **Method**: ``GET /_all_dbs``
* **Request**: None
* **Response**: JSON list of DBs
* **Admin Privileges Required**: no
* **Return Codes**:

  * **200**:
    Request completed successfully.

Returns a list of all the databases in the CouchDB instance. For
example:

.. code-block:: http

    GET http://couchdb:5984/_all_dbs
    Accept: application/json

The return is a JSON array:

.. code-block:: javascript

    [
       "_users",
       "contacts",
       "docs",
       "invoices",
       "locations"
    ]


.. _api/misc/db_updates:
.. _api/misc/db_updates.get:

``GET /_db_updates``
====================

* **Method**: ``GET /_db_updates``
* **Request**: None
* **Admin Privileges Required**: yes
* **Query Arguments**:

  * **Argument**: feed

    * **Description**: Format of the response feed
    * **Optional**: yes
    * **Type**: string
    * **Default**: longpoll
    * **Supported Values**:

      * **longpoll**: Closes the connection after the first event.
      * **continuous**: Send a line of JSON per event. Keeps the socket open until ``timeout``.
      * **eventsource**: Like, ``continuous``, but sends the events in EventSource format. See http://dev.w3.org/html5/eventsource/ for details,

  * **Argument**: timeout

    * **Description**: Number of seconds until CouchDB closes the connection.
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 60

  * **Argument**: heartbeat

    * **Description**: Whether CouchDB will send a newline character (``\n``) on ``timeout``.
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

* **Return Codes**:

  * **200**
    Request completed successfully.

Returns a list of all database events in the CouchDB instance.

A database event is one of `created`, `updated`, `deleted`.

For example:

.. code-block:: http

    GET http://couchdb:5984/_db_events?feed=continuous
    Accept: application/json

.. code-block:: javascript

    {"dbname":"my-database", "type":"created"}
    {"dbname":"my-database", "type":"updated"}
    {"dbname":"another-database", "type":"created"}
    {"dbname":"my-database", "type":"deleted"}
    {"dbname":"another-database", "type":"updated"}


.. _api/misc/log:
.. _api/misc/log.get:

``GET /_log``
=============

* **Method**: ``GET /_log``
* **Request**: None
* **Response**: Log content
* **Admin Privileges Required**: yes
* **Query Arguments**:

  * **Argument**: bytes

    * **Description**:  Bytes to be returned
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 1000

  * **Argument**: offset

    * **Description**:  Offset in bytes where the log tail should be started
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

* **Return Codes**:

  * **200**:
    Request completed successfully.

Gets the CouchDB log, equivalent to accessing the local log file of the
corresponding CouchDB instance.

When you request the log, the response is returned as plain (UTF-8)
text, with an HTTP ``Content-type`` header as ``text/plain``.

For example, the request:

.. code-block:: http

    GET http://couchdb:5984/_log
    Accept: */*

The raw text is returned:

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

.. _api/misc/replicate:
.. _api/misc/replicate.post:

``POST /_replicate``
====================

.. todo:: POST /_replicate :: what response is?

* **Method**: ``POST /_replicate``
* **Request**: Replication specification
* **Response**: TBD
* **Admin Privileges Required**: yes
* **Query Arguments**:

  * **Argument**: bytes

    * **Description**:  Bytes to be returned
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 1000

  * **Argument**: offset

    * **Description**:  Offset in bytes where the log tail should be started
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

* **Return Codes**:

  * **200**:
    Replication request successfully completed
  * **202**:
    Continuous replication request has been accepted
  * **404**:
    Either the source or target DB is not found
  * **500**:
    JSON specification was invalid

Request, configure, or stop, a replication operation.

The specification of the replication request is controlled through the
JSON content of the request. The JSON should be an object with the
fields defining the source, target and other options. The fields of the
JSON request are shown in the table below:

* **cancel (optional)**:  Cancels the replication
* **continuous (optional)**:  Configure the replication to be continuous
* **create_target (optional)**:  Creates the target database
* **doc_ids (optional)**:  Array of document IDs to be synchronized
* **proxy (optional)**:  Address of a proxy server through which replication
  should occur
* **source**:  Source database name or URL
* **target**:  Target database name or URL

Replication Operation
---------------------

The aim of the replication is that at the end of the process, all active
documents on the source database are also in the destination database
and all documents that were deleted in the source databases are also
deleted (if they exist) on the destination database.

Replication can be described as either push or pull replication:

-  *Pull replication* is where the ``source`` is the remote CouchDB
   instance, and the ``destination`` is the local database.

   Pull replication is the most useful solution to use if your source
   database has a permanent IP address, and your destination (local)
   database may have a dynamically assigned IP address (for example,
   through DHCP). This is particularly important if you are replicating
   to a mobile or other device from a central server.

-  *Push replication* is where the ``source`` is a local database, and
   ``destination`` is a remote database.

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
    Content-Type: application/json
    Accept: application/json

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

The structure defines the replication status, as described in the table
below:

* **history [array]**:  Replication History

  * **doc_write_failures**:  Number of document write failures
  * **docs_read**:  Number of documents read
  * **docs_written**:  Number of documents written to target
  * **end_last_seq**:  Last sequence number in changes stream
  * **end_time**:  Date/Time replication operation completed
  * **missing_checked**:  Number of missing documents checked
  * **missing_found**:  Number of missing documents found
  * **recorded_seq**:  Last recorded sequence number
  * **session_id**:  Session ID for this replication operation
  * **start_last_seq**:  First sequence number in changes stream
  * **start_time**:  Date/Time replication operation started

* **ok**:  Replication status
* **session_id**:  Unique session ID
* **source_last_seq**:  Last sequence number read from source database

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
    Content-Type: application/json
    Accept: application/json

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
   ``databasea`` to ``databaseb``, and separately from ``databaseb`` to
   ``databasea``.

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
    Content-Type: application/json
    Accept: application/json

    {
        "cancel" : true,
        "continuous" : true
        "create_target" : true,
        "source" : "recipes",
        "target" : "http://couchdb-remote:5984/recipes",
    }

Requesting cancellation of a replication that does not exist results in
a 404 error.

.. _api/misc/restart:
.. _api/misc/restart.post:

``POST /_restart``
==================

* **Method**: ``POST /_restart``
* **Request**: None
* **Response**: JSON status message
* **Admin Privileges Required**: yes
* **HTTP Headers**:

  * **Header**: ``Content-Type``

    * **Description**: Request content type
    * **Optional**: no
    * **Value**: :mimetype:`application/json`

* **Return Codes**:

  * **200**:
    Replication request successfully completed

Restarts the CouchDB instance. You must be authenticated as a user with
administration privileges for this to work.

For example:

.. code-block:: http

    POST http://admin:password@couchdb:5984/_restart

The return value (if the server has not already restarted) is a JSON
status object indicating that the request has been received:

.. code-block:: javascript

    {
       "ok" : true,
    }

If the server has already restarted, the header may be returned, but no
actual data is contained in the response.

.. _api/misc/stats:
.. _api/misc/stats.get:

``GET /_stats``
===============

* **Method**: ``GET /_stats``
* **Request**: None
* **Response**: Server statistics
* **Admin Privileges Required**: no
* **Return Codes**:

  * **200**:
    Request completed successfully.

The ``_stats`` method returns a JSON object containing the statistics
for the running server. The object is structured with top-level sections
collating the statistics for a range of entries, with each individual
statistic being easily identified, and the content of each statistic is
self-describing. For example, the request time statistics, within the
``couchdb`` section are structured as follows:

.. code-block:: javascript

    {
       "couchdb" : {
    ...
          "request_time" : {
             "stddev" : "27.509",
             "min" : "0.333333333333333",
             "max" : "152",
             "current" : "400.976",
             "mean" : "10.837",
             "sum" : "400.976",
             "description" : "length of a request inside CouchDB without MochiWeb"
          },
    ...
        }
    }


The fields provide the current, minimum and maximum, and a collection of
statistical means and quantities. The quantity in each case is not
defined, but the descriptions below provide

The statistics are divided into the following top-level sections:

-  ``couchdb``: Describes statistics specific to the internals of CouchDB.

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

-  ``httpd_request_methods``

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

-  ``httpd_status_codes``

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

-  ``httpd``

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

.. _api/misc/utils:
.. _api/misc/utils.get:

``GET /_utils``
===============

* **Method**: ``GET /_utils``
* **Request**: None
* **Response**: Administration interface
* **Admin Privileges Required**: no

Accesses the built-in Futon administration interface for CouchDB.

.. _api/misc/uuids:
.. _api/misc/uuids.get:

``GET /_uuids``
===============

* **Method**: ``GET /_uuids``
* **Request**: None
* **Response**: List of UUIDs
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: count

    * **Description**:  Number of UUIDs to return
    * **Optional**: yes
    * **Type**: numeric

* **Return Codes**:

  * **200**:
    Request completed successfully.

Requests one or more Universally Unique Identifiers (UUIDs) from the
CouchDB instance. The response is a JSON object providing a list of
UUIDs. For example:

.. code-block:: javascript

    {
       "uuids" : [
          "7e4b5a14b22ec1cf8e58b9cdd0000da3"
       ]
    }

You can use the ``count`` argument to specify the number of UUIDs to be
returned. For example:

.. code-block:: http

    GET http://couchdb:5984/_uuids?count=5

Returns:

.. code-block:: javascript

    {
       "uuids" : [
          "c9df0cdf4442f993fc5570225b405a80",
          "c9df0cdf4442f993fc5570225b405bd2",
          "c9df0cdf4442f993fc5570225b405e42",
          "c9df0cdf4442f993fc5570225b4061a0",
          "c9df0cdf4442f993fc5570225b406a20"
       ]
    }

The UUID type is determined by the :ref:`UUID algorithm <config/uuids/algorithm>`
setting in the CouchDB configuration.

The UUID type could be changed in anytime through
:ref:`Config API <api/config/section/key.put>`. For example, changing the UUID
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


.. _api/misc/favicon:
.. _api/misc/favicon.get:

``GET /favicon.ico``
====================

* **Method**: ``GET /favicon.ico``
* **Request**: None
* **Response**: Binary content for the `favicon.ico` site icon
* **Admin Privileges Required**: no
* **Return Codes**:

  * **200**:
    Request completed successfully.
  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Returns the site icon. The return ``Content-Type`` header is
:mimetype:`image/x-icon`, and the content stream is the image data.
