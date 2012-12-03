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

.. _api-db:

================
Database Methods
================

The Database methods provide an interface to an entire database withing
CouchDB. These are database, rather than document, level requests.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /db                     | Returns database information              |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db                     | Create a new database                     |
+--------+-------------------------+-------------------------------------------+
| DELETE | /db                     | Delete an existing database               |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_all_docs           | Returns a built-in view of all documents  |
|        |                         | in this database                          |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_all_docs           | Returns certain rows from the built-in    |
|        |                         | view of all documents                     |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_bulk_docs          | Insert multiple documents in to the       |
|        |                         | database in a single request              |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_changes            | Returns changes for the given database    |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_compact            | Starts a compaction for the database      |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_compact/design-doc | Starts a compaction for all the views in  |
|        |                         | the selected design document              |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_ensure_full_commit | Makes sure all uncommitted changes are    |
|        |                         | written and synchronized to the disk      |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_missing_revs       | Given a list of document revisions,       |
|        |                         | returns the document revisions that do not|
|        |                         | exist in the database                     |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_purge              | Purge some historical documents entirely  |
|        |                         | from database history                     |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_revs_diff          | Given a list of document revisions,       |
|        |                         | returns differences between the given     |
|        |                         | revisions and ones that are in the        |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_revs_limit         | Gets the limit of historical revisions to |
|        |                         | store for a single document in the        |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/_revs_limit         | Sets the limit of historical revisions to |
|        |                         | store for a single document in the        |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| GET    | /db/_security           | Returns the special security object for   |
|        |                         | the database                              |
+--------+-------------------------+-------------------------------------------+
| PUT    | /db/_security           | Sets the special security object for the  |
|        |                         | database                                  |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_temp_view          | Execute a given view function for all     |
|        |                         | documents and return the result           |
+--------+-------------------------+-------------------------------------------+
| POST   | /db/_view_cleanup       | Removes view files that are not used by   |
|        |                         | any design document                       |
+--------+-------------------------+-------------------------------------------+

For all the database methods, the database name within the URL path
should be the database name that you wish to perform the operation on.
For example, to obtain the meta information for the database
``recipes``, you would use the HTTP request:

.. code-block:: http

    GET /recipes

For clarity, the form below is used in the URL paths:

.. code-block:: http

    GET /db

Where ``db`` is the name of any database.

.. _api-get-db:

``GET /db``
===========

* **Method**: ``GET /db``
* **Request**: None
* **Response**: Information about the database in JSON format
* **Admin Privileges Required**: no
* **Return Codes**:

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Gets information about the specified database. For example, to retrieve
the information for the database ``recipe``:

.. code-block:: http

    GET http://couchdb:5984/recipes
    Accept: application/json

The JSON response contains meta information about the database. A sample
of the JSON returned for an empty database is provided below:

.. code-block:: javascript

    {
       "compact_running" : false,
       "committed_update_seq" : 375048,
       "disk_format_version" : 5,
       "disk_size" : 33153123,
       "doc_count" : 18386,
       "doc_del_count" : 0,
       "db_name" : "recipes",
       "instance_start_time" : "1290700340925570",
       "purge_seq" : 10,
       "update_seq" : 375048
    }
        

The elements of the returned structure are shown in the table below:

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| committed_update_seq             | The number of committed update.           |
+----------------------------------+-------------------------------------------+
| compact_running                  | Set to true if the database compaction    |
|                                  | routine is operating on this database.    |
+----------------------------------+-------------------------------------------+
| db_name                          | The name of the database.                 |
+----------------------------------+-------------------------------------------+
| disk_format_version              | The version of the physical format used   |
|                                  | for the data when it is stored on disk.   |
+----------------------------------+-------------------------------------------+
| disk_size                        | Size in bytes of the data as stored on the|
|                                  | disk. Views indexes are not included in   |
|                                  | the calculation.                          |
+----------------------------------+-------------------------------------------+
| doc_count                        | A count of the documents in the specified |
|                                  | database.                                 |
+----------------------------------+-------------------------------------------+
| doc_del_count                    | Number of deleted documents               |
+----------------------------------+-------------------------------------------+
| instance_start_time              | Timestamp of when the database was        |
|                                  | created, expressed in milliseconds since  |
|                                  | the epoch.                                |
+----------------------------------+-------------------------------------------+
| purge_seq                        | The number of purge operations on the     |
|                                  | database.                                 |
+----------------------------------+-------------------------------------------+
| update_seq                       | The current number of updates to the      |
|                                  | database.                                 |
+----------------------------------+-------------------------------------------+

``PUT /db``
===========

* **Method**: ``PUT /db``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Return Codes**:

  * **400**:
    Invalid database name

  * **412**:
    Database already exists

Creates a new database. The database name must be composed of one or
more of the following characters:

-  Lowercase characters (``a-z``)

-  Name must begin with a lowercase letter

-  Digits (``0-9``)

-  Any of the characters ``_``, ``$``, ``(``, ``)``, ``+``, ``-``, and
   ``/``.

Trying to create a database that does not meet these requirements will
return an error quoting these restrictions.

To create the database ``recipes``:

.. code-block:: http

    PUT http://couchdb:5984/recipes
    Content-Type: application/json

The returned content contains the JSON status:

.. code-block:: javascript

    {
       "ok" : true
    }

Anything should be treated as an error, and the problem should be taken
form the HTTP response code.

``DELETE /db``
==============

* **Method**: ``DELETE /db``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Return Codes**:

  * **200**:
    Database has been deleted

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Deletes the specified database, and all the documents and attachments
contained within it.

To delete the database ``recipes`` you would send the request:

.. code-block:: http

    DELETE http://couchdb:5984/recipes
    Content-Type: application/json

If successful, the returned JSON will indicate success

.. code-block:: javascript

    {
       "ok" : true
    }

.. _api-changes:

``GET /db/_changes``
====================

* **Method**: ``GET /db/_changes``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: doc_ids

    * **Description**:  Specify the list of documents IDs to be filtered
    * **Optional**: yes
    * **Type**: json
    * **Default**: none

  * **Argument**: feed

    * **Description**:  Type of feed
    * **Optional**: yes
    * **Type**: string
    * **Default**: normal
    * **Supported Values**:

      * **continuous**: Continuous (non-polling) mode
      * **longpoll**: Long polling mode
      * **normal**: Normal mode

  * **Argument**: filter

    * **Description**:  Filter function from a design document to get updates
    * **Optional**: yes
    * **Type**: string
    * **Default**: none
    * **Supported Values**:

  * **Argument**: heartbeat

    * **Description**:  Period after which an empty line is sent during longpoll
      or continuous
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 60000
    * **Quantity**: milliseconds

  * **Argument**: include_docs

    * **Description**:  Include the document with the result
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: limit

    * **Description**:  Maximum number of rows rows to return
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: none

  * **Argument**: since

    * **Description**:  Start the results from changes immediately after the
      specified sequence number
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

Obtains a list of the changes made to the database. This can be used to
monitor for update and modifications to the database for post processing
or synchronization. There are three different types of supported changes
feeds, poll, longpoll, and continuous. All requests are poll requests by
default. You can select any feed type explicitly using the ``feed``
query argument.

-  **Poll**

   With polling you can request the changes that have occured since a
   specific sequence number. This returns the JSON structure containing
   the changed document information. When you perform a poll change
   request, only the changes since the specific sequence number are
   returned. For example, the query

   .. code-block:: http

       DELETE http://couchdb:5984/recipes/_changes
       Content-Type: application/json

   Will get all of the changes in the database. You can request a
   starting point using the ``since`` query argument and specifying the
   sequence number. You will need to record the latest sequence number
   in your client and then use this when making another request as the
   new value to the ``since`` parameter.

-  **Longpoll**

   With long polling the request to the server will remain open until a
   change is made on the database, when the changes will be reported,
   and then the connection will close. The long poll is useful when you
   want to monitor for changes for a specific purpose without wanting to
   monitoring continuously for changes.

   Because the wait for a change can be significant you can set a
   timeout before the connection is automatically closed (the
   ``timeout`` argument). You can also set a heartbeat interval (using
   the ``heartbeat`` query argument), which sends a newline to keep the
   connection open.

-  **Continuous**

   Continuous sends all new changes back to the client immediately,
   without closing the connection. In continuous mode the format of the
   changes is slightly different to accommodate the continuous nature
   while ensuring that the JSON output is still valid for each change
   notification.

   As with the longpoll feed type you can set both the timeout and
   heartbeat intervals to ensure that the connection is kept open for
   new changes and updates.

The return structure for ``normal`` and ``longpoll`` modes is a JSON
array of changes objects, and the last update sequence number. The
structure is described in the following table.

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| last_seq                         | Last change sequence number.              |
+----------------------------------+-------------------------------------------+
| results [array]                  | Changes made to a database                |
+----------------------------------+-------------------------------------------+
|         changes [array]          | List of changes, field-by-field, for this |
|                                  | document                                  |
+----------------------------------+-------------------------------------------+
|         id                       | Document ID                               |
+----------------------------------+-------------------------------------------+
|         seq                      | Update sequence number                    |
+----------------------------------+-------------------------------------------+

The return format for ``continuous`` mode the server sends a ``CRLF``
(carriage-return, linefeed) delimited line for each change. Each line
contains the `JSON object`_.

You can also request the full contents of each document change (instead
of just the change notification) by using the ``include_docs``
parameter.

Filtering
---------

You can filter the contents of the changes feed in a number of ways. The
most basic way is to specify one or more document IDs to the query. This
causes the returned structure value to only contain changes for the
specified IDs. Note that the value of this query argument should be a
JSON formatted array.

You can also filter the ``_changes`` feed by defining a filter function
within a design document. The specification for the filter is the same
as for replication filters. You specify the name of the filter function
to the ``filter`` parameter, specifying the design document name and
filter name. For example:

.. code-block:: http

    GET /db/_changes?filter=design_doc/filtername

The ``_changes`` feed can be used to watch changes to specific document
ID's or the list of ``_design`` documents in a database. If the
``filters`` parameter is set to ``_doc_ids`` a list of doc IDs can be
passed in the ``doc_ids`` parameter as a JSON array. For more
information, see :ref:`changes`.

.. _api-compact:

``POST /db/_compact``
=====================

* **Method**: ``POST /db/_compact``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: yes
* **Return Codes**:

  * **202**:
    Compaction request has been accepted

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Request compaction of the specified database. Compaction compresses the
disk database file by performing the following operations:

-  Writes a new version of the database file, removing any unused
   sections from the new version during write. Because a new file is
   temporary created for this purpose, you will need twice the current
   storage space of the specified database in order for the compaction
   routine to complete.

-  Removes old revisions of documents from the database, up to the
   per-database limit specified by the ``_revs_limit`` database
   parameter. See :ref:`api-get-db`.

Compaction can only be requested on an individual database; you cannot
compact all the databases for a CouchDB instance. The compaction process
runs as a background process.

You can determine if the compaction process is operating on a database
by obtaining the database meta information, the ``compact_running``
value of the returned database structure will be set to true. See
:ref:`api-get-db`.

You can also obtain a list of running processes to determine whether
compaction is currently running. See :ref:`active-tasks`.

.. _api-compact-ddoc:

``POST /db/_compact/design-doc``
================================

* **Method**: ``POST /db/_compact/design-doc``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: yes
* **Return Codes**:

  * **202**:
    Compaction request has been accepted

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.

Compacts the view indexes associated with the specified design document.
You can use this in place of the full database compaction if you know a
specific set of view indexes have been affected by a recent database
change.

For example, to compact the views associated with the ``recipes`` design
document:

.. code-block:: http

    POST http://couchdb:5984/recipes/_compact/recipes
    Content-Type: application/json

CouchDB will immediately return with a status indicating that the
compaction request has been received (HTTP status code 202):

.. code-block:: javascript

    {
       "ok" : true
    }
        

``POST /db/_view_cleanup``
==========================

* **Method**: ``POST /db/_view_cleanup``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: yes

Cleans up the cached view output on disk for a given view. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_view_cleanup
    Content-Type: application/json

If the request is successful, a basic status message us returned:

.. code-block:: javascript

    {
       "ok" : true
    }
        

``POST /db/_ensure_full_commit``
================================

* **Method**: ``POST /db/_ensure_full_commit``
* **Request**: None
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Return Codes**:

  * **202**:
    Commit completed successfully

  * **404**:
    The requested content could not be found. The returned content will include
    further information, as a JSON object, if available.


Commits any recent changes to the specified database to disk. You should
call this if you want to ensure that recent changes have been written.
For example, to commit all the changes to disk for the database
``recipes`` you would use:

.. code-block:: http

    POST http://couchdb:5984/recipes/_ensure_full_commit
    Content-Type: application/json

This returns a status message, containing the success message and the
timestamp for when the CouchDB instance was started:

.. code-block:: javascript

    {
      "ok" : true,
      "instance_start_time" : "1288186189373361"
    }

``POST /db/_bulk_docs``
=======================

* **Method**: ``POST /db/_bulk_docs``
* **Request**: JSON of the docs and updates to be applied
* **Response**: JSON success statement
* **Admin Privileges Required**: no
* **Return Codes**:

  * **201**:
    Document(s) have been created or updated

The bulk document API allows you to create and update multiple documents
at the same time within a single request. The basic operation is similar
to creating or updating a single document, except that you batch the
document structure and information and . When creating new documents the
document ID is optional. For updating existing documents, you must
provide the document ID, revision information, and new document values.

For both inserts and updates the basic structure of the JSON is the
same:

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| all_or_nothing (optional)        | Sets the database commit mode to use      |
|                                  | all-or-nothing semantics                  |
+----------------------------------+-------------------------------------------+
| docs [array]                     | Bulk Documents Document                   |
+----------------------------------+-------------------------------------------+
|         _id (optional)           | List of changes, field-by-field, for this |
|                                  | document                                  |
+----------------------------------+-------------------------------------------+
|         _rev (optional)          | Document ID                               |
+----------------------------------+-------------------------------------------+
|         _deleted (optional)      | Update sequence number                    |
+----------------------------------+-------------------------------------------+

Inserting Documents in Bulk
---------------------------

To insert documents in bulk into a database you need to supply a JSON
structure with the array of documents that you want to add to the
database. Using this method you can either include a document ID, or
allow the document ID to be automatically generated.

For example, the following inserts three new documents, two with the
supplied document IDs, and one which will have a document ID generated:

.. code-block:: javascript

    {
       "docs" : [
          {
             "_id" : "FishStew",
             "servings" : 4,
             "subtitle" : "Delicious with fresh bread",
             "title" : "Fish Stew"
          },
          {
             "_id" : "LambStew",
             "servings" : 6,
             "subtitle" : "Delicious with scone topping",
             "title" : "Lamb Stew"
          },
          {
             "servings" : 8,
             "subtitle" : "Delicious with suet dumplings",
             "title" : "Beef Stew"
          },
       ]
    }
          

The return type from a bulk insertion will be 201, with the content of
the returned structure indicating specific success or otherwise messages
on a per-document basis.

The return structure from the example above contains a list of the
documents created, here with the combination and their revision IDs:

.. code-block:: http

    POST http://couchdb:5984/recipes/_bulk_docs
    Content-Type: application/json

    [
       {
          "id" : "FishStew",
          "rev" : "1-9c65296036141e575d32ba9c034dd3ee",
       },
       {
          "id" : "LambStew",
          "rev" : "1-34c318924a8f327223eed702ddfdc66d",
       },
       {
          "id" : "7f7638c86173eb440b8890839ff35433",
          "rev" : "1-857c7cbeb6c8dd1dd34a0c73e8da3c44",
       }
    ]
              

The content and structure of the returned JSON will depend on the transaction
semantics being used for the bulk update; see :ref:`bulk-semantics` for more
information. Conflicts and validation errors when updating documents in
bulk must be handled separately; see :ref:`bulk-validation`.

Updating Documents in Bulk
--------------------------

The bulk document update procedure is similar to the insertion
procedure, except that you must specify the document ID and current
revision for every document in the bulk update JSON string.

For example, you could send the following request:

.. code-block:: http

    POST http://couchdb:5984/recipes/_bulk_docs
    Content-Type: application/json

    {
       "docs" : [
          {
             "_id" : "FishStew",
             "_rev" : "1-9c65296036141e575d32ba9c034dd3ee",
             "servings" : 4,
             "subtitle" : "Delicious with freshly baked bread",
             "title" : "Fish Stew"
          },
          {
             "_id" : "LambStew",
             "_rev" : "1-34c318924a8f327223eed702ddfdc66d",
             "servings" : 6,
             "subtitle" : "Serve with a wholemeal scone topping",
             "title" : "Lamb Stew"
          },
          {
             "_id" : "7f7638c86173eb440b8890839ff35433"
             "_rev" : "1-857c7cbeb6c8dd1dd34a0c73e8da3c44",
             "servings" : 8,
             "subtitle" : "Hand-made dumplings make a great accompaniment",
             "title" : "Beef Stew"
          }
       ]
    }

The return structure is the JSON of the updated documents, with the new
revision and ID information:

.. code-block:: javascript

    [
       {
          "id" : "FishStew",
          "rev" : "2-e7af4c4e9981d960ecf78605d79b06d1"
       },
       {
          "id" : "LambStew",
          "rev" : "2-0786321986194c92dd3b57dfbfc741ce"
       },
       {
          "id" : "7f7638c86173eb440b8890839ff35433",
          "rev" : "2-bdd3bf3563bee516b96885a66c743f8e"
       }
    ]

You can optionally delete documents during a bulk update by adding the
``_deleted`` field with a value of ``true`` to each document ID/revision
combination within the submitted JSON structure.

The return type from a bulk insertion will be 201, with the content of
the returned structure indicating specific success or otherwise messages
on a per-document basis.

The content and structure of the returned JSON will depend on the transaction
semantics being used for the bulk update; see :ref:`bulk-semantics` for more
information. Conflicts and validation errors when updating documents in
bulk must be handled separately; see :ref:`bulk-validation`.

.. _bulk-semantics:

Bulk Documents Transaction Semantics
------------------------------------

CouchDB supports two different modes for updating (or inserting)
documents using the bulk documentation system. Each mode affects both
the state of the documents in the event of system failure, and the level
of conflict checking performed on each document. The two modes are:

-  ``non-atomic``

   The default mode is non-atomic, that is, CouchDB will only guarantee
   that some of the documents will be saved when you send the request.
   The response will contain the list of documents successfully inserted
   or updated during the process. In the event of a crash, some of the
   documents may have been successfully saved, and some will have been
   lost.

   In this mode, the response structure will indicate whether the
   document was updated by supplying the new ``_rev`` parameter
   indicating a new document revision was created. If the update failed,
   then you will get an ``error`` of type ``conflict``. For example:

   .. code-block:: javascript

       [
          {
             "id" : "FishStew",
             "error" : "conflict",
             "reason" : "Document update conflict."
          },
          {
             "id" : "LambStew",
             "error" : "conflict",
             "reason" : "Document update conflict."
          },
          {
             "id" : "7f7638c86173eb440b8890839ff35433",
             "error" : "conflict",
             "reason" : "Document update conflict."
          }
       ]
           

   In this case no new revision has been created and you will need to
   submit the document update, with the correct revision tag, to update
   the document.

-  ``all-or-nothing``

   In all-or-nothing mode, either all documents are written to the
   database, or no documents are written to the database, in the event
   of a system failure during commit.

   In addition, the per-document conflict checking is not performed.
   Instead a new revision of the document is created, even if the new
   revision is in conflict with the current revision in the database.
   The returned structure contains the list of documents with new
   revisions:

   .. code-block:: javascript

       [
          {
             "id" : "FishStew",
             "rev" : "2-e7af4c4e9981d960ecf78605d79b06d1"
          },
          {
             "id" : "LambStew",
             "rev" : "2-0786321986194c92dd3b57dfbfc741ce"
          },
          {
             "id" : "7f7638c86173eb440b8890839ff35433",
             "rev" : "2-bdd3bf3563bee516b96885a66c743f8e"
          }
       ]

   When updating documents using this mode the revision of a document
   included in views will be arbitrary. You can check the conflict
   status for a document by using the ``conflicts=true`` query argument
   when accessing the view. Conflicts should be handled individually to
   ensure the consistency of your database.

   To use this mode, you must include the ``all_or_nothing`` field (set
   to true) within the main body of the JSON of the request.

The effects of different database operations on the different modes are
summarized below:

* **Transaction Mode**: ``Non-atomic``

  * **Transaction**: ``Insert``

    * **Cause**: Requested document ID already exists
    * **Resolution**: Resubmit with different document ID, or update the
      existing document

  * **Transaction**: ``Update``

    * **Cause**: Revision missing or incorrect
    * **Resolution**: Resubmit with correct revision

* **Transaction Mode**: ``All-or-nothing``

  * **Transaction**: ``Insert`` / ``Update``

    * **Cause**: Additional revision inserted
    * **Resolution**: Resolve conflicted revisions

Replication of documents is independent of the type of insert or update.
The documents and revisions created during a bulk insert or update are
replicated in the same way as any other document. This can mean that if
you make use of the all-or-nothing mode the exact list of documents,
revisions (and their conflict state) may or may not be replicated to
other databases correctly.

.. _bulk-validation:

Bulk Document Validation and Conflict Errors
--------------------------------------------

The JSON returned by the ``_bulk_docs`` operation consists of an array
of JSON structures, one for each document in the original submission.
The returned JSON structure should be examined to ensure that all of the
documents submitted in the original request were successfully added to
the database.

The exact structure of the returned information is:

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| docs [array]                     | Bulk Documents Document                   |
+----------------------------------+-------------------------------------------+
|         id                       | Document ID                               |
+----------------------------------+-------------------------------------------+
|         error                    | Error type                                |
+----------------------------------+-------------------------------------------+
|         reason                   | Error string with extended reason         |
+----------------------------------+-------------------------------------------+

When a document (or document revision) is not correctly committed to the
database because of an error, you should check the ``error`` field to
determine error type and course of action. Errors will be one of the
following type:

-  ``conflict``

   The document as submitted is in conflict. If you used the default
   bulk transaction mode then the new revision will not have been
   created and you will need to re-submit the document to the database.
   If you used ``all-or-nothing`` mode then you will need to manually
   resolve the conflicted revisions of the document.

   Conflict resolution of documents added using the bulk docs interface
   is identical to the resolution procedures used when resolving
   conflict errors during replication.

-  ``forbidden``

   Entries with this error type indicate that the validation routine
   applied to the document during submission has returned an error.

   For example, if your validation routine includes the following:

   .. code-block:: javascript

        throw({forbidden: 'invalid recipe ingredient'});

   The error returned will be:

   .. code-block:: javascript

       {
          "id" : "7f7638c86173eb440b8890839ff35433",
          "error" : "forbidden",
          "reason" : "invalid recipe ingredient"
       }
             

``POST /db/_temp_view``
=======================

* **Method**: ``POST /db/_temp_view``
* **Request**: JSON with the temporary view definition
* **Response**: Temporary view result set
* **Admin Privileges Required**: yes

Creates (and executes) a temporary view based on the view function
supplied in the JSON request. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_temp_view
    Content-Type: application/json

    {
       "map" : "function(doc) { if (doc.value > 9995) { emit(null, doc.value); } }"
    }

The resulting JSON response is the result from the execution of the
temporary view:

.. code-block:: javascript

    {
       "total_rows" : 3,
       "rows" : [
          {
             "value" : 9998.41913029012,
             "id" : "05361cc6aa42033878acc1bacb1f39c2",
             "key" : null
          },
          {
             "value" : 9998.94149934853,
             "id" : "1f443f471e5929dd7b252417625ed170",
             "key" : null
          },
          {
             "value" : 9998.01511339154,
             "id" : "1f443f471e5929dd7b252417629c102b",
             "key" : null
          }
       ],
       "offset" : 0
    }

The arguments also available to standard view requests also apply to
temporary views, but the execution of the view may take some time as it
relies on being executed at the time of the request. In addition to the
time taken, they are also computationally very expensive to produce. You
should use a defined view if you want to achieve the best performance.

``POST /db/_purge``
===================

* **Method**: ``POST /db/_purge``
* **Request**: JSON of the document IDs/revisions to be purged
* **Response**: JSON structure with purged documents and purge sequence
* **Admin Privileges Required**: no

A database purge permanently removes the references to deleted documents
from the database. Deleting a document within CouchDB does not actually
remove the document from the database, instead, the document is marked as
a deleted (and a new revision is created). This is to ensure that
deleted documents are replicated to other databases as having been
deleted. This also means that you can check the status of a document and
identify that the document has been deleted.

The purge operation removes the references to the deleted documents from
the database. The purging of old documents is not replicated to other
databases. If you are replicating between databases and have deleted a
large number of documents you should run purge on each database.

.. note::

   Purging documents does not remove the space used by them on disk. To
   reclaim disk space, you should run a database compact (see
   :ref:`api-compact`), and compact views (see :ref:`api-compact-ddoc`).

To perform a purge operation you must send a request including the JSON
of the document IDs that you want to purge. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_purge
    Content-Type: application/json

    {
      "FishStew" : [
        "17-b3eb5ac6fbaef4428d712e66483dcb79"
        ]
    }

The format of the request must include the document ID and one or more
revisions that must be purged.

The response will contain the purge sequence number, and a list of the
document IDs and revisions successfully purged.

.. code-block:: javascript

    {
       "purged" : {
          "FishStew" : [
             "17-b3eb5ac6fbaef4428d712e66483dcb79"
          ]
       },
       "purge_seq" : 11
    }

Updating Indexes
----------------

The number of purges on a database is tracked using a purge sequence.
This is used by the view indexer to optimize the updating of views that
contain the purged documents.

When the indexer identifies that the purge sequence on a database has
changed, it compares the purge sequence of the database with that stored
in the view index. If the difference between the stored sequence and
database is sequence is only 1, then the indexer uses a cached list of
the most recently purged documents, and then removes these documents
from the index individually. This prevents completely rebuilding the
index from scratch.

If the difference between the stored sequence number and current
database sequence is greater than 1, then the view index is entirely
rebuilt. This is an expensive operation as every document in the
database must be examined.

``GET /db/_all_docs``
=====================

* **Method**: ``GET /db/_all_docs``
* **Request**: None
* **Response**: JSON object containing document information, ordered by the
  document ID
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: descending

    * **Description**:  Return the documents in descending by key order
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: endkey

    * **Description**:  Stop returning records when the specified key is reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: endkey_docid

    * **Description**:  Stop returning records when the specified document ID is
      reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: group

    * **Description**:  Group the results using the reduce function to a group
      or single row
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: group_level

    * **Description**:  Specify the group level to be used
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: include_docs

    * **Description**:  Include the full content of the documents in the return
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

  * **Argument**: inclusive_end

    * **Description**:  Specifies whether the specified end key should be
      included in the result
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: key

    * **Description**:  Return only documents that match the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: limit

    * **Description**:  Limit the number of the returned documents to the
      specified number
    * **Optional**: yes
    * **Type**: numeric

  * **Argument**: reduce

    * **Description**:  Use the reduction function
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: true

  * **Argument**: skip

    * **Description**:  Skip this number of records before starting to return
      the results
    * **Optional**: yes
    * **Type**: numeric
    * **Default**: 0

  * **Argument**: stale

    * **Description**:  Allow the results from a stale view to be used
    * **Optional**: yes
    * **Type**: string
    * **Default**:
    * **Supported Values**:

      * **ok**: Allow stale views

  * **Argument**: startkey

    * **Description**:  Return records starting with the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: startkey_docid

    * **Description**:  Return records starting with the specified document ID
    * **Optional**: yes
    * **Type**: string

Returns a JSON structure of all of the documents in a given database.
The information is returned as a JSON structure containing meta
information about the return structure, and the list documents and basic
contents, consisting the ID, revision and key. The key is generated from
the document ID.

+----------------------------------+-------------------------------------------+
| Field                            | Description                               |
+==================================+===========================================+
| offset                           | Offset where the document list started    |
+----------------------------------+-------------------------------------------+
| rows [array]                     | Array of document object                  |
+----------------------------------+-------------------------------------------+
| total_rows                       | Number of documents in the database/view  |
+----------------------------------+-------------------------------------------+
| update_seq                       | Current update sequence for the database  |
+----------------------------------+-------------------------------------------+

By default the information returned contains only the document ID and
revision. For example, the request:

.. code-block:: http

    GET http://couchdb:5984/recipes/_all_docs
    Accept: application/json

Returns the following structure:

.. code-block:: javascript

    {
       "total_rows" : 18386,
       "rows" : [
          {
             "value" : {
                "rev" : "1-bc0d5aed1e339b1cc1f29578f3220a45"
             },
             "id" : "Aberffrawcake",
             "key" : "Aberffrawcake"
          },
          {
             "value" : {
                "rev" : "3-68a20c89a5e70357c20148f8e82ca331"
             },
             "id" : "Adukiandorangecasserole-microwave",
             "key" : "Adukiandorangecasserole-microwave"
          },
          {
             "value" : {
                "rev" : "3-9b2851ed9b6f655cc4eb087808406c60"
             },
             "id" : "Aioli-garlicmayonnaise",
             "key" : "Aioli-garlicmayonnaise"
          },
          ...
             ],
       "offset" : 0
    }

The information is returned in the form of a temporary view of all the
database documents, with the returned key consisting of the ID of the
document. The remainder of the interface is therefore identical to the
View query arguments and their behavior.

``POST /db/_all_docs``
======================

* **Method**: ``POST /db/_all_docs``
* **Request**: JSON of the document IDs you want included
* **Response**: JSON of the returned view
* **Admin Privileges Required**: no

The ``POST`` to ``_all_docs`` allows to specify multiple keys to be
selected from the database. This enables you to request multiple
documents in a single request, in place of multiple
:ref:`api-get-doc` requests.

The request body should contain a list of the keys to be returned as an
array to a ``keys`` object. For example:

.. code-block:: http

    POST http://couchdb:5984/recipes/_all_docs
    User-Agent: MyApp/0.1 libwww-perl/5.837

    {
       "keys" : [
          "Zingylemontart",
          "Yogurtraita"
       ]
    }

The return JSON is the all documents structure, but with only the
selected keys in the output:

.. code-block:: javascript

    {
       "total_rows" : 2666,
       "rows" : [
          {
             "value" : {
                "rev" : "1-a3544d296de19e6f5b932ea77d886942"
             },
             "id" : "Zingylemontart",
             "key" : "Zingylemontart"
          },
          {
             "value" : {
                "rev" : "1-91635098bfe7d40197a1b98d7ee085fc"
             },
             "id" : "Yogurtraita",
             "key" : "Yogurtraita"
          }
       ],
       "offset" : 0
    }

``POST /db/_missing_revs``
==========================

* **Method**: ``POST /db/_missing_revs``
* **Request**: JSON list of document revisions
* **Response**: JSON of missing revisions
* **Admin Privileges Required**: no

``POST /db/_revs_diff``
=======================

* **Method**: ``POST /db/_revs_diff``
* **Request**: JSON list of document revisions
* **Response**: JSON list of differences from supplied document/revision list
* **Admin Privileges Required**: no

``GET /db/_security``
=====================

* **Method**: ``GET /db/_security``
* **Request**: None
* **Response**: JSON of the security object
* **Admin Privileges Required**: no

Gets the current security object from the specified database. The
security object consists of two compulsory elements, ``admins`` and
``readers``, which are used to specify the list of users and/or roles
that have admin and reader rights to the database respectively. Any
additional fields in the security object are optional. The entire
security object is made available to validation and other internal
functions so that the database can control and limit functionality.

To get the existing security object you would send the following
request:

.. code-block:: javascript

    {
       "admins" : {
          "roles" : [],
          "names" : [
             "mc",
             "slp"
          ]
       },
       "readers" : {
          "roles" : [],
          "names" : [
             "tim",
             "brian"
          ]
       }
    }

Security object structure is:

* **admins**: Roles/Users with admin privileges

  * **roles** [array]: List of roles with parent privilege
  * **users** [array]: List of users with parent privilege

* **readers**: Roles/Users with reader privileges

  * **roles** [array]: List of roles with parent privilege
  * **users** [array]: List of users with parent privilege

.. note::
   If the security object for a database has never been set, then the
   value returned will be empty.

``PUT /db/_security``
=====================

* **Method**: ``PUT /db/_security``
* **Request**: JSON specifying the admin and user security for the database
* **Response**: JSON status message
* **Admin Privileges Required**: no

Sets the security object for the given database.For example, to set the
security object for the ``recipes`` database:

.. code-block:: javascript

    PUT http://couchdb:5984/recipes/_security
    Content-Type: application/json

    {
       "admins" : {
          "roles" : [],
          "names" : [
             "mc",
             "slp"
          ]
       },
       "readers" : {
          "roles" : [],
          "names" : [
             "tim",
             "brian"
          ]
       }
    }

If the setting was successful, a JSON status object will be returned:

.. code-block:: javascript

    {
       "ok" : true
    }

``GET /db/_revs_limit``
=======================

* **Method**: ``GET /db/_revs_limit``
* **Request**: None
* **Response**: The current revision limit setting
* **Admin Privileges Required**: no


Gets the current ``revs_limit`` (revision limit) setting.

For example to get the current limit:

.. code-block:: http

    GET http://couchdb:5984/recipes/_revs_limit
    Content-Type: application/json

The returned information is the current setting as a numerical scalar:

.. code-block:: javascript

    1000

``PUT /db/_revs_limit``
=======================

* **Method**: ``PUT /db/_revs_limit``
* **Request**: A scalar integer of the revision limit setting
* **Response**: Confirmation of setting of the revision limit
* **Admin Privileges Required**: no

Sets the maximum number of document revisions that will be tracked by
CouchDB, even after compaction has occurred. You can set the revision
limit on a database by using ``PUT`` with a scalar integer of the limit
that you want to set as the request body.

For example to set the revs limit to 100 for the ``recipes`` database:

.. code-block:: http

    PUT http://couchdb:5984/recipes/_revs_limit
    Content-Type: application/json

    100

If the setting was successful, a JSON status object will be returned:

.. code-block:: javascript

    {
       "ok" : true
    }

.. _JSON object: #table-couchdb-api-db_db-json-changes
