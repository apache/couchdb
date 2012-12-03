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

.. _api-design:

=======================
Design Document Methods
=======================

In CouchDB, design documents provide the main interface for building a
CouchDB application. The design document defines the views used to
extract information from CouchDB through one or more views. Design
documents are created within your CouchDB instance in the same way as
you create database documents, but the content and definition of the
documents is different. Design Documents are named using an ID defined
with the design document URL path, and this URL can then be used to
access the database contents.

Views and lists operate together to provide automated (and formatted)
output from your database.

A list of the available methods and URL paths are provided below:

Design Document API Calls

``GET /db/_design/design-doc``
==============================

* **Method**: ``GET /db/_design/design-doc``
* **Request**:  None
* **Response**:  JSON of the existing design document
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**:  Specify the revision to return
    * **Optional**: yes
    * **Type**: string

  * **Argument**: revs

    * **Description**:  Return a list of the revisions for the document
    * **Optional**: yes
    * **Type**: boolean
    * **Supported Values**:

        * **true**: Includes the revisions

  * **Argument**: revs_info

    * **Description**:  Return a list of detailed revision information for the
      document
    * **Optional**: yes
    * **Type**: boolean
    * **Supported Values**:

      * **true**: Includes the revisions

Returns the specified design document, ``design-doc`` from the specified
``db``. For example, to retrieve the design document ``recipes`` you
would send the following request:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes
    Content-Type: application/json

The returned string will be the JSON of the design document:

.. code-block:: javascript

    {
       "_id" : "_design/recipes",
       "_rev" : "5-39f56a392b86bbee57e2138921346406"
       "language" : "javascript",
       "views" : {
          "by_recipe" : {
             "map" : "function(doc) { if (doc.title != null) emit(doc.title, doc) }"
          },
       },
    }

A list of the revisions can be obtained by using the ``revs`` query
argument, or an extended list of revisions using the ``revs_info`` query
argument. This operates in the same way as for other documents. Fur
further examples, see :ref:`api-get-doc`.

``PUT /db/_design/design-doc``
==============================

* **Method**: ``PUT /db/_design/design-doc``
* **Request**:  JSON of the design document
* **Response**:  JSON status
* **Admin Privileges Required**: no

Upload the specified design document, ``design-doc``, to the specified
database. The design document should follow the definition of a design
document, as summarised in the following table.

* **_id**:  Design Document ID
* **_rev**:  Design Document Revision
* **views**:  View

  * **viewname**:  View Definition

    * **map**:  Map Function for View
    * **reduce (optional)**:  Reduce Function for View

For more information on writing views, see :ref:`views`.

``DELETE /db/_design/design-doc``
=================================

* **Method**: ``DELETE /db/_design/design-doc``
* **Request**:  None
* **Response**:  JSON of deleted design document
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**:  Current revision of the document for validation
    * **Optional**: yes
    * **Type**: string

* **HTTP Headers**

  * **Header**: ``If-Match``

    * **Description**: Current revision of the document for validation
    * **Optional**: yes

* **Return Codes**:

  * **409**:
    Supplied revision is incorrect or missing

Delete an existing design document. Deleting a design document also
deletes all of the associated view indexes, and recovers the
corresponding space on disk for the indexes in question.

To delete, you must specify the current revision of the design document
using the ``rev`` query argument.

For example:

.. code-block:: http

    DELETE http://couchdb:5984/recipes/_design/recipes?rev=2-ac58d589b37d01c00f45a4418c5a15a8
    Content-Type: application/json

The response contains the delete document ID and revision:

.. code-block:: javascript

    {
       "id" : "recipe/_design/recipes"
       "ok" : true,
       "rev" : "3-7a05370bff53186cb5d403f861aca154",
    }

``COPY /db/_design/design-doc``
===============================

* **Method**: ``COPY /db/_design/design-doc``
* **Request**: None
* **Response**: JSON of the new document and revision
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**:  Revision to copy from
    * **Optional**: yes
    * **Type**: string

* **HTTP Headers**

  * **Header**: ``Destination``

    * **Description**: Destination document (and optional revision)
    * **Optional**: no

The ``COPY`` command (non-standard HTTP) copies an existing design
document to a new or existing document.

The source design document is specified on the request line, with the
``Destination`` HTTP Header of the request specifying the target
document.

Copying a Design Document
-------------------------

To copy the latest version of a design document to a new document you
specify the base document and target document:

.. code-block:: http

    COPY http://couchdb:5984/recipes/_design/recipes
    Content-Type: application/json
    Destination: /recipes/_design/recipelist

The above request copies the design document ``recipes`` to the new
design document ``recipelist``. The response is the ID and revision of
the new document.

.. code-block:: javascript

    {
       "id" : "recipes/_design/recipelist"
       "rev" : "1-9c65296036141e575d32ba9c034dd3ee",
    }

.. note::
   Copying a design document does automatically reconstruct the view
   indexes. These will be recreated, as with other views, the first
   time the new view is accessed.

Copying from a Specific Revision
--------------------------------

To copy *from* a specific version, use the ``rev`` argument to the query
string:

.. code-block:: http

    COPY http://couchdb:5984/recipes/_design/recipes?rev=1-e23b9e942c19e9fb10ff1fde2e50e0f5
    Content-Type: application/json
    Destination: recipes/_design/recipelist

The new design document will be created using the specified revision of
the source document.

Copying to an Existing Design Document
--------------------------------------

To copy to an existing document, you must specify the current revision
string for the target document, using the ``rev`` parameter to the
``Destination`` HTTP Header string. For example:

.. code-block:: http

    COPY http://couchdb:5984/recipes/_design/recipes
    Content-Type: application/json
    Destination: recipes/_design/recipelist?rev=1-9c65296036141e575d32ba9c034dd3ee

The return value will be the new revision of the copied document:

.. code-block:: javascript

    {
       "id" : "recipes/_design/recipes"
       "rev" : "2-55b6a1b251902a2c249b667dab1c6692",
    }

``GET /db/_design/design-doc/attachment``
=========================================

* **Method**: ``GET /db/_design/design-doc/attachment``
* **Request**: None
* **Response**: Returns the attachment data
* **Admin Privileges Required**: no

Returns the file attachment ``attachment`` associated with the design
document ``/_design_/design-doc``. The raw data of the associated
attachment is returned (just as if you were accessing a static file. The
returned HTTP ``Content-type`` will be the same as the content type set
when the document attachment was submitted into the database.

``PUT /db/_design/design-doc/attachment``
=========================================

* **Method**: ``PUT /db/_design/design-doc/attachment``
* **Request**: Raw document data
* **Response**: JSON document status
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**:  Current document revision
    * **Optional**: no
    * **Type**: string

* **HTTP Headers**

  * **Header**: ``Content-Length``

    * **Description**: Length (bytes) of the attachment being uploaded
    * **Optional**: no

  * **Header**: ``Content-Type``

    * **Description**: MIME type for the uploaded attachment
    * **Optional**: no

  * **Header**: ``If-Match``

    * **Description**: Current revision of the document for validation
    * **Optional**: yes

Upload the supplied content as an attachment to the specified design
document (``/_design/design-doc``). The ``attachment`` name provided
must be a URL encoded string. You must also supply either the ``rev``
query argument or the ``If-Match`` HTTP header for validation, and the
HTTP headers (to set the attacment content type). The content type is
used when the attachment is requested as the corresponding content-type
in the returned document header.

For example, you could upload a simple text document using the following
request:

.. code-block:: http

    PUT http://couchdb:5984/recipes/_design/recipes/view.css?rev=7-f7114d4d81124b223283f3e89eee043e
    Content-Length: 39
    Content-Type: text/plain

    div.recipetitle {
    font-weight: bold;
    }

Or by using the ``If-Match`` HTTP header:

.. code-block:: http

    PUT http://couchdb:5984/recipes/FishStew/basic
    If-Match: 7-f7114d4d81124b223283f3e89eee043e
    Content-Length: 39
    Content-Type: text/plain

    div.recipetitle {
    font-weight: bold;
    }

The returned JSON contains the new document information:

.. code-block:: javascript

    {
       "id" : "_design/recipes"
       "ok" : true,
       "rev" : "8-cb2b7d94eeac76782a02396ba70dfbf5",
    }

.. note::
   Uploading an attachment updates the corresponding document revision.
   Revisions are tracked for the parent document, not individual attachments.

``DELETE /db/_design/design-doc/attachment``
============================================

* **Method**: ``DELETE /db/_design/design-doc/attachment``
* **Request**: None
* **Response**: JSON status
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: rev

    * **Description**:  Current document revision
    * **Optional**: no
    * **Type**: string

* **HTTP Headers**

  * **Header**: ``If-Match``

    * **Description**: Current revision of the document for validation
    * **Optional**: yes

* **Return Codes**:

  * **200**:
    Attachment deleted successfully
  * **409**:
    Supplied revision is incorrect or missing

Deletes the attachment ``attachment`` to the specified
``_design/design-doc``. You must supply the ``rev`` argument with the
current revision to delete the attachment.

For example to delete the attachment ``view.css`` from the design
document ``recipes``:

.. code-block:: http

    DELETE http://couchdb:5984/recipes/_design/recipes/view.css?rev=9-3db559f13a845c7751d407404cdeaa4a

The returned JSON contains the updated revision information for the
parent document:

.. code-block:: javascript

    {
       "id" : "_design/recipes"
       "ok" : true,
       "rev" : "10-f3b15bb408961f8dcc3d86c7d3b54c4c",
    }

``GET /db/_design/design-doc/_info``
====================================

* **Method**: ``GET /db/_design/design-doc/_info``
* **Request**: None
* **Response**: JSON of the design document information
* **Admin Privileges Required**: no

Obtains information about a given design document, including the index,
index size and current status of the design document and associated
index information.

For example, to get the information for the ``recipes`` design document:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_info
    Content-Type: application/json

This returns the following JSON structure:

.. code-block:: javascript

    {
       "name" : "recipes"
       "view_index" : {
          "compact_running" : false,
          "updater_running" : false,
          "language" : "javascript",
          "purge_seq" : 10,
          "waiting_commit" : false,
          "waiting_clients" : 0,
          "signature" : "fc65594ee76087a3b8c726caf5b40687",
          "update_seq" : 375031,
          "disk_size" : 16491
       },
    }

The individual fields in the returned JSON structure are detailed below:

* **name**:  Name/ID of Design Document
* **view_index**:  View Index

  * **compact_running**:  Indicates whether a compaction routine is currently
    running on the view
  * **disk_size**:  Size in bytes of the view as stored on disk
  * **language**:  Language for the defined views
  * **purge_seq**:  The purge sequence that has been processed
  * **signature**:  MD5 signature of the views for the design document
  * **update_seq**:  The update sequence of the corresponding database that
    has been indexed
  * **updater_running**:  Indicates if the view is currently being updated
  * **waiting_clients**:  Number of clients waiting on views from this design
    document
  * **waiting_commit**:  Indicates if there are outstanding commits to the
    underlying database that need to processed

.. _api-get-view:

.. _views:

``GET /db/_design/design-doc/_view/view-name``
==============================================

* **Method**: ``GET /db/_design/design-doc/_view/view-name``
* **Request**: None
* **Response**: JSON of the documents returned by the view
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

    * **Description**:  Stop returning records when the specified document
      ID is reached
    * **Optional**: yes
    * **Type**: string

  * **Argument**: group

    * **Description**:  Group the results using the reduce function to a
      group or single row
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
    * **Supported Values**

      * **ok**: Allow stale views

  * **Argument**: startkey

    * **Description**:  Return records starting with the specified key
    * **Optional**: yes
    * **Type**: string

  * **Argument**: startkey_docid

    * **Description**:  Return records starting with the specified document ID
    * **Optional**: yes
    * **Type**: string

  * **Argument**: update_seq

    * **Description**:  Include the update sequence in the generated results
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

Executes the specified ``view-name`` from the specified ``design-doc``
design document.

Querying Views and Indexes
--------------------------

The definition of a view within a design document also creates an index
based on the key information defined within each view. The production
and use of the index significantly increases the speed of access and
searching or selecting documents from the view.

However, the index is not updated when new documents are added or
modified in the database. Instead, the index is generated or updated,
either when the view is first accessed, or when the view is accessed
after a document has been updated. In each case, the index is updated
before the view query is executed against the database.

View indexes are updated incrementally in the following situations:

-  A new document has been added to the database.

-  A document has been deleted from the database.

-  A document in the database has been updated.

View indexes are rebuilt entirely when the view definition changes. To
achieve this, a 'fingerprint' of the view definition is created when the
design document is updated. If the fingerprint changes, then the view
indexes are entirely rebuilt. This ensures that changes to the view
definitions are reflected in the view indexes.

.. note::
   View index rebuilds occur when one view from the same the view group
   (i.e. all the views defined within a single a design document) has
   been determined as needing a rebuild. For example, if if you have a
   design document with different views, and you update the database,
   all three view indexes within the design document will be updated.

Because the view is updated when it has been queried, it can result in a
delay in returned information when the view is accessed, especially if
there are a large number of documents in the database and the view index
does not exist. There are a number of ways to mitigate, but not
completely eliminate, these issues. These include:

-  Create the view definition (and associated design documents) on your
   database before allowing insertion or updates to the documents. If
   this is allowed while the view is being accessed, the index can be
   updated incrementally.

-  Manually force a view request from the database. You can do this
   either before users are allowed to use the view, or you can access
   the view manually after documents are added or updated.

-  Use the ``/db/_changes`` method to monitor for changes to the
   database and then access the view to force the corresponding view
   index to be updated. See :ref:`api-changes` for more information.

-  Use a monitor with the ``update_notification`` section of the CouchDB
   configuration file to monitor for changes to your database, and
   trigger a view query to force the view to be updated. For more
   information, see :ref:`update-notifications`.

None of these can completely eliminate the need for the indexes to be
rebuilt or updated when the view is accessed, but they may lessen the
effects on end-users of the index update affecting the user experience.

Another alternative is to allow users to access a 'stale' version of the
view index, rather than forcing the index to be updated and displaying
the updated results. Using a stale view may not return the latest
information, but will return the results of the view query using an
existing version of the index.

For example, to access the existing stale view ``by_recipe`` in the
``recipes`` design document:

.. code-block:: text

    http://couchdb:5984/recipes/_design/recipes/_view/by_recipe?stale=ok

Accessing a stale view:

-  Does not trigger a rebuild of the view indexes, even if there have
   been changes since the last access.

-  Returns the current version of the view index, if a current version
   exists.

-  Returns an empty result set if the given view index does exist.

As an alternative, you use the ``update_after`` value to the ``stale``
parameter. This causes the view to be returned as a stale view, but for
the update process to be triggered after the view information has been
returned to the client.

In addition to using stale views, you can also make use of the
``update_seq`` query argument. Using this query argument generates the
view information including the update sequence of the database from
which the view was generated. The returned value can be compared this to
the current update sequence exposed in the database information
(returned by :ref:`api-get-db`).

Sorting Returned Rows
---------------------

Each element within the returned array is sorted using native UTF-8
sorting according to the contents of the key portion of the emitted
content. The basic order of output is as follows:

-  ``null``

-  ``false``

-  ``true``

-  Numbers

-  Text (case sensitive, lowercase first)

-  Arrays (according to the values of each element, in order)

-  Objects (according to the values of keys, in key order)

You can reverse the order of the returned view information by using the
``descending`` query value set to true. For example, Retrieving the list
of recipes using the ``by_title`` (limited to 5 records) view:

.. code-block:: javascript

    {
       "offset" : 0,
       "rows" : [
          {
             "id" : "3-tiersalmonspinachandavocadoterrine",
             "key" : "3-tier salmon, spinach and avocado terrine",
             "value" : [
                null,
                "3-tier salmon, spinach and avocado terrine"
             ]
          },
          {
             "id" : "Aberffrawcake",
             "key" : "Aberffraw cake",
             "value" : [
                null,
                "Aberffraw cake"
             ]
          },
          {
             "id" : "Adukiandorangecasserole-microwave",
             "key" : "Aduki and orange casserole - microwave",
             "value" : [
                null,
                "Aduki and orange casserole - microwave"
             ]
          },
          {
             "id" : "Aioli-garlicmayonnaise",
             "key" : "Aioli - garlic mayonnaise",
             "value" : [
                null,
                "Aioli - garlic mayonnaise"
             ]
          },
          {
             "id" : "Alabamapeanutchicken",
             "key" : "Alabama peanut chicken",
             "value" : [
                null,
                "Alabama peanut chicken"
             ]
          }
       ],
       "total_rows" : 2667
    }

Requesting the same in descending order will reverse the entire view
content. For example the request

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_title?limit=5&descending=true
    Accept: application/json
    Content-Type: application/json

Returns the last 5 records from the view:

.. code-block:: javascript

    {
       "offset" : 0,
       "rows" : [
          {
             "id" : "Zucchiniinagrodolcesweet-sourcourgettes",
             "key" : "Zucchini in agrodolce (sweet-sour courgettes)",
             "value" : [
                null,
                "Zucchini in agrodolce (sweet-sour courgettes)"
             ]
          },
          {
             "id" : "Zingylemontart",
             "key" : "Zingy lemon tart",
             "value" : [
                null,
                "Zingy lemon tart"
             ]
          },
          {
             "id" : "Zestyseafoodavocado",
             "key" : "Zesty seafood avocado",
             "value" : [
                null,
                "Zesty seafood avocado"
             ]
          },
          {
             "id" : "Zabaglione",
             "key" : "Zabaglione",
             "value" : [
                null,
                "Zabaglione"
             ]
          },
          {
             "id" : "Yogurtraita",
             "key" : "Yogurt raita",
             "value" : [
                null,
                "Yogurt raita"
             ]
          }
       ],
       "total_rows" : 2667
    }

The sorting direction is applied before the filtering applied using the
``startkey`` and ``endkey`` query arguments. For example the following
query:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?startkey=%22carrots%22&endkey=%22egg%22
    Accept: application/json
    Content-Type: application/json

Will operate correctly when listing all the matching entries between
“carrots” and ``egg``. If the order of output is reversed with the
``descending`` query argument, the view request will return no entries:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?descending=true&startkey=%22carrots%22&endkey=%22egg%22
    Accept: application/json
    Content-Type: application/json

The returned result is empty:

.. code-block:: javascript

    {
       "total_rows" : 26453,
       "rows" : [],
       "offset" : 21882
    }

The results will be empty because the entries in the view are reversed
before the key filter is applied, and therefore the ``endkey`` of “egg”
will be seen before the ``startkey`` of “carrots”, resulting in an empty
list.

Instead, you should reverse the values supplied to the ``startkey`` and
``endkey`` parameters to match the descending sorting applied to the
keys. Changing the previous example to:

.. code-block:: http

    GET http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?descending=true&startkey=%22egg%22&endkey=%22carrots%22
    Accept: application/json
    Content-Type: application/json

Specifying Start and End Values
-------------------------------

.. todo:: Specifying Start and End Values

The ``startkey`` and ``endkey`` query arguments can be used to specify
the range of values to be displayed when querying the view.

Using Limits and Skipping Rows
------------------------------

.. todo:: Using Limits and Skipping Rows

TBC

View Reduction and Grouping
---------------------------

.. todo:: View Reduction and Grouping

TBC

``POST /db/_design/design-doc/_view/view-name``
===============================================

* **Method**: ``POST /db/_design/design-doc/_view/view-name``
* **Request**:  List of keys to be returned from specified view
* **Response**:  JSON of the documents returned by the view
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

    * **Description**:  Stop returning records when the specified document ID
      is reached
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

  * **Argument**: update_seq

    * **Description**:  Include the update sequence in the generated results
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false

Executes the specified ``view-name`` from the specified ``design-doc``
design document. Unlike the ``GET`` method for accessing views, the
``POST`` method supports the specification of explicit keys to be
retrieved from the view results. The remainder of the ``POST`` view
functionality is identical to the :ref:`api-get-view` API.

For example, the request below will return all the recipes where the key
for the view matches either “claret” or “clear apple cider” :

.. code-block:: http

    POST http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient
    Content-Type: application/json

    {
       "keys" : [
          "claret",
          "clear apple juice"
       ]
    }
          

The returned view data contains the standard view information, but only
where the keys match.

.. code-block:: javascript

    {
       "total_rows" : 26484,
       "rows" : [
          {
             "value" : [
                "Scotch collops"
             ],
             "id" : "Scotchcollops",
             "key" : "claret"
          },
          {
             "value" : [
                "Stand pie"
             ],
             "id" : "Standpie",
             "key" : "clear apple juice"
          }
       ],
       "offset" : 6324
    }

Multi-document Fetching
-----------------------

By combining the ``POST`` method to a given view with the
``include_docs=true`` query argument you can obtain multiple documents
from a database. The result is more efficient than using multiple
:ref:`api-get-doc` requests.

For example, sending the following request for ingredients matching
“claret” and “clear apple juice”:

.. code-block:: http

    POST http://couchdb:5984/recipes/_design/recipes/_view/by_ingredient?include_docs=true
    Content-Type: application/json

    {
       "keys" : [
          "claret",
          "clear apple juice"
       ]
    }

Returns the full document for each recipe:

.. code-block:: javascript

    {
       "offset" : 6324,
       "rows" : [
          {
             "doc" : {
                "_id" : "Scotchcollops",
                "_rev" : "1-bcbdf724f8544c89697a1cbc4b9f0178",
                "cooktime" : "8",
                "ingredients" : [
                   {
                      "ingredient" : "onion",
                      "ingredtext" : "onion, peeled and chopped",
                      "meastext" : "1"
                   },
                ...
                ],
                "keywords" : [
                   "cook method.hob, oven, grill@hob",
                   "diet@wheat-free",
                   "diet@peanut-free",
                   "special collections@classic recipe",
                   "cuisine@british traditional",
                   "diet@corn-free",
                   "diet@citrus-free",
                   "special collections@very easy",
                   "diet@shellfish-free",
                   "main ingredient@meat",
                   "occasion@christmas",
                   "meal type@main",
                   "diet@egg-free",
                   "diet@gluten-free"
                ],
                "preptime" : "10",
                "servings" : "4",
                "subtitle" : "This recipe comes from an old recipe book of 1683 called 'The Gentlewoman's Kitchen'. This is an excellent way of making a rich and full-flavoured meat dish in a very short time.",
                "title" : "Scotch collops",
                "totaltime" : "18"
             },
             "id" : "Scotchcollops",
             "key" : "claret",
             "value" : [
                "Scotch collops"
             ]
          },
          {
             "doc" : {
                "_id" : "Standpie",
                "_rev" : "1-bff6edf3ca2474a243023f2dad432a5a",
                "cooktime" : "92",
                "ingredients" : [
    ...            ],
                "keywords" : [
                   "diet@dairy-free",
                   "diet@peanut-free",
                   "special collections@classic recipe",
                   "cuisine@british traditional",
                   "diet@corn-free",
                   "diet@citrus-free",
                   "occasion@buffet party",
                   "diet@shellfish-free",
                   "occasion@picnic",
                   "special collections@lunchbox",
                   "main ingredient@meat",
                   "convenience@serve with salad for complete meal",
                   "meal type@main",
                   "cook method.hob, oven, grill@hob / oven",
                   "diet@cow dairy-free"
                ],
                "preptime" : "30",
                "servings" : "6",
                "subtitle" : "Serve this pie with pickled vegetables and potato salad.",
                "title" : "Stand pie",
                "totaltime" : "437"
             },
             "id" : "Standpie",
             "key" : "clear apple juice",
             "value" : [
                "Stand pie"
             ]
          }
       ],
       "total_rows" : 26484
    }

``GET /db/_design/design-doc/_show/show-name``
===============================================

.. todo:: GET /db/_design/design-doc/_show/show-name

* **Method**: ``GET /db/_design/design-doc/_show/show-name``
* **Request**:  None
* **Response**:  Returns the result of the show
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: details

    * **Description**:  Indicates whether details should be included
    * **Optional**: yes
    * **Type**: string

  * **Argument**: format

    * **Description**:  Format of the returned information
    * **Optional**: yes
    * **Type**: string

``POST /db/_design/design-doc/_show/show-name/doc``
===================================================

.. todo:: POST /db/_design/design-doc/_show/show-name/doc

* **Method**: ``POST /db/_design/design-doc/_show/show-name``
* **Request**:  Custom data
* **Response**:  Returns the result of the show
* **Admin Privileges Required**: no

``GET /db/_design/design-doc/_list/list-name/other-design-doc/view-name``
=========================================================================

.. todo:: GET /db/_design/design-doc/_list/list-name/other-design-doc/view-name

* **Method**: ``GET /db/_design/design-doc/_list/list-name/other-design-doc/view-name``
* **Request**:  TBC
* **Response**:  TBC
* **Admin Privileges Required**: no

``POST /db/_design/design-doc/_list/list-name/other-design-doc/view-name``
==========================================================================

.. todo:: POST /db/_design/design-doc/_list/list-name/other-design-doc/view-name

* **Method**: ``POST /db/_design/design-doc/_list/list-name/other-design-doc/view-name``
* **Request**:  TBC
* **Response**:  TBC
* **Admin Privileges Required**: no

``GET /db/_design/design-doc/_list/list-name/view-name``
========================================================

.. todo:: GET /db/_design/design-doc/_list/list-name/view-name

* **Method**: ``GET /db/_design/design-doc/_list/list-name/view-name``
* **Request**:  TBC
* **Response**:  TBC
* **Admin Privileges Required**: no

``POST /db/_design/design-doc/_list/list-name/view-name``
=========================================================

.. todo:: POST /db/_design/design-doc/_list/list-name/view-name

* **Method**: ``POST /db/_design/design-doc/_list/list-name/view-name``
* **Request**:  TBC
* **Response**:  TBC
* **Admin Privileges Required**: no

``PUT /db/_design/design-doc/_update/updatename/doc``
=====================================================

.. todo:: POST /db/_design/design-doc/_update/updatename/doc

* **Method**: ``POST /db/_design/design-doc/_update/updatename/doc``
* **Request**:  TBC
* **Response**:  TBC
* **Admin Privileges Required**: no

``POST /db/_design/design-doc/_update/updatename``
==================================================

.. todo:: PUT /db/_design/design-doc/_update/updatename/doc

* **Method**: ``PUT /db/_design/design-doc/_update/updatename/doc``
* **Request**:  TBC
* **Response**:  TBC
* **Admin Privileges Required**: no

``ALL /db/_design/design-doc/_rewrite/rewrite-name/anything``
=============================================================

.. todo:: ALL /db/_design/design-doc/_rewrite/rewrite-name/anything

* **Method**: ``ALL /db/_design/design-doc/_rewrite/rewrite-name/anything``
* **Request**:  TBC
* **Response**:  TBC
* **Admin Privileges Required**: no
