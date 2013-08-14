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


.. _api/ddoc.get:

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
further examples, see :http:get:`/{db}/{docid}`.

.. _api/ddoc.put:

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

For more information on writing views, see :ref:`api/ddoc/view`.

.. _api/ddoc.delete:

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

.. _api/ddoc.copy:

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

.. _api/ddoc/attachment:
.. _api/ddoc/attachment.get:

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

.. _api/ddoc/attachment.put:

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
HTTP headers (to set the attachment content type). The content type is
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

.. _api/ddoc/attachment.delete:

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


.. _api/ddoc/info:
.. _api/ddoc/info.get:

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
