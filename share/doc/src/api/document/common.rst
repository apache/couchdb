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


.. _api/doc.get:

``GET /db/doc``
===============

* **Method**: ``GET /db/doc``
* **Request**: None
* **Response**: Returns the JSON for the document
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: conflicts

    * **Description**: Returns the conflict tree for the document.
    * **Optional**: yes
    * **Type**: boolean
    * **Default**: false
    * **Supported Values**:

      * **true**: Includes the revisions

  * **Argument**: rev

    * **Description**: Specify the revision to return
    * **Optional**: yes
    * **Type**: string
    * **Supported Values**:

      * **true**: Includes the revisions

  * **Argument**: revs

    * **Description**:  Return a list of the revisions for the document
    * **Optional**: yes
    * **Type**: boolean

  * **Argument**: revs_info

    * **Description**: Return a list of detailed revision information for the
      document
    * **Optional**: yes
    * **Type**: boolean
    * **Supported Values**:

      * **true**: Includes the revisions

* **Return Codes**:

  * **200**:
    Document retrieved
  * **400**:
    The format of the request or revision was invalid
  * **404**:
    The specified document or revision cannot be found, or has been deleted
  * **409**:
    Conflict - a document with the specified document ID already exists

Returns the specified ``doc`` from the specified ``db``. For example, to
retrieve the document with the id ``FishStew`` you would send the
following request:

.. code-block:: http

    GET http://couchdb:5984/recipes/FishStew
    Content-Type: application/json
    Accept: application/json

The returned JSON is the JSON of the document, including the document ID
and revision number:

.. code-block:: javascript

    {
       "_id" : "FishStew",
       "_rev" : "3-a1a9b39ee3cc39181b796a69cb48521c",
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "title" : "Irish Fish Stew"
    }


Unless you request a specific revision, the latest revision of the
document will always be returned.

Attachments
-----------

If the document includes attachments, then the returned structure will
contain a summary of the attachments associated with the document, but
not the attachment data itself.

The JSON for the returned document will include the ``_attachments``
field, with one or more attachment definitions. For example:

.. code-block:: javascript

    {
       "_id" : "FishStew",
       "servings" : 4,
       "subtitle" : "Delicious with fresh bread",
       "title" : "Fish Stew"
       "_attachments" : {
          "styling.css" : {
             "stub" : true,
             "content-type" : "text/css",
             "length" : 783426,
             },
       },
    }

The format of the returned JSON is shown in the table below:

* **_id** (optional): Document ID
* **_rev** (optional): Revision ID (when updating an existing document)
* **_attachments** (optional): Document Attachment

  * **filename**: Attachment information

    * **content_type**: MIME Content type string
    * **length**: Length (bytes) of the attachment data
    * **revpos**: Revision where this attachment exists
    * **stub**: Indicates whether the attachment is a stub

Getting a List of Revisions
---------------------------

You can obtain a list of the revisions for a given document by adding
the ``revs=true`` parameter to the request URL. For example:

.. code-block:: http

    GET http://couchdb:5984/recipes/FishStew?revs=true
    Accept: application/json

The returned JSON structure includes the original document, including a
``_revisions`` structure that includes the revision information:

.. code-block:: javascript

    {
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "_id" : "FishStew",
       "title" : "Irish Fish Stew",
       "_revisions" : {
          "ids" : [
             "a1a9b39ee3cc39181b796a69cb48521c",
             "7c4740b4dcf26683e941d6641c00c39d",
             "9c65296036141e575d32ba9c034dd3ee"
          ],
          "start" : 3
       },
       "_rev" : "3-a1a9b39ee3cc39181b796a69cb48521c"
    }

* **_id** (optional): Document ID
* **_rev** (optional): Revision ID (when updating an existing document)
* **_revisions**: CouchDB Document Revisions

  * **ids** [array]: Array of valid revision IDs, in reverse order
    (latest first)
  * **start**: Prefix number for the latest revision

Obtaining an Extended Revision History
--------------------------------------

You can get additional information about the revisions for a given
document by supplying the ``revs_info`` argument to the query:

.. code-block:: http

    GET http://couchdb:5984/recipes/FishStew?revs_info=true
    Accept: application/json

This returns extended revision information, including the availability
and status of each revision:

.. code-block:: javascript

    {
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "_id" : "FishStew",
       "_revs_info" : [
          {
             "status" : "available",
             "rev" : "3-a1a9b39ee3cc39181b796a69cb48521c"
          },
          {
             "status" : "available",
             "rev" : "2-7c4740b4dcf26683e941d6641c00c39d"
          },
          {
             "status" : "available",
             "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
          }
       ],
       "title" : "Irish Fish Stew",
       "_rev" : "3-a1a9b39ee3cc39181b796a69cb48521c"
    }

* **_id** (optional): Document ID
* **_rev** (optional): Revision ID (when updating an existing document)
* **_revs_info** [array]: CouchDB Document Extended Revision Info

  * **rev**: Full revision string
  * **status**: Status of the revision

Obtaining a Specific Revision
-----------------------------

To get a specific revision, use the ``rev`` argument to the request, and
specify the full revision number:

.. code-block:: http

    GET http://couchdb:5984/recipes/FishStew?rev=2-7c4740b4dcf26683e941d6641c00c39d
    Accept: application/json

The specified revision of the document will be returned, including a
``_rev`` field specifying the revision that was requested:

.. code-block:: javascript

    {
       "_id" : "FishStew",
       "_rev" : "2-7c4740b4dcf26683e941d6641c00c39d",
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "title" : "Fish Stew"
    }

.. _api/doc.head:

``HEAD /db/doc``
================

* **Method**: ``HEAD /db/doc``
* **Request**: None
* **Response**: None
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

  * **Argument**: revs_info

    * **Description**:  Return a list of detailed revision information for the
      document
    * **Optional**: yes
    * **Type**: boolean

* **Return Codes**:

  * **404**:
    The specified document or revision cannot be found, or has been deleted

Returns the HTTP Headers containing a minimal amount of information
about the specified document. The method supports the same query
arguments as the ``GET`` method, but only the header information
(including document size, and the revision as an ETag), is returned. For
example, a simple ``HEAD`` request:

.. code-block:: http

    HEAD http://couchdb:5984/recipes/FishStew
    Content-Type: application/json


Returns the following HTTP Headers:

.. code-block:: javascript

    HTTP/1.1 200 OK
    Server: CouchDB/1.0.1 (Erlang OTP/R13B)
    Etag: "7-a19a1a5ecd946dad70e85233ba039ab2"
    Date: Fri, 05 Nov 2010 14:54:43 GMT
    Content-Type: text/plain;charset=utf-8
    Content-Length: 136
    Cache-Control: must-revalidate

The ``Etag`` header shows the current revision for the requested
document, and the ``Content-Length`` specifies the length of the data,
if the document were requested in full.

Adding any of the query arguments (as supported by ```GET```_ method),
then the resulting HTTP Headers will correspond to what would be
returned. Note that the current revision is not returned when the
``refs_info`` argument is used. For example:

.. code-block:: http

    HTTP/1.1 200 OK
    Server: CouchDB/1.0.1 (Erlang OTP/R13B)
    Date: Fri, 05 Nov 2010 14:57:16 GMT
    Content-Type: text/plain;charset=utf-8
    Content-Length: 609
    Cache-Control: must-revalidate

.. _api/doc.put:

``PUT /db/doc``
===============

* **Method**: ``PUT /db/doc``
* **Request**: JSON of the new document, or updated version of the existed
  document
* **Response**: JSON of the document ID and revision
* **Admin Privileges Required**: no
* **Query Arguments**:

  * **Argument**: batch

    * **Description**:  Allow document store request to be batched with others
    * **Optional**: yes
    * **Type**: string
    * **Supported Values**:

      * **ok**: Enable

* **HTTP Headers**

  * **Header**: ``If-Match``

    * **Description**: Current revision of the document for validation
    * **Optional**: yes

* **Return Codes**:

  * **201**:
    Document has been created successfully
  * **202**:
    Document accepted for writing (batch mode)


The ``PUT`` method creates a new named document, or creates a new
revision of the existing document. Unlike the ``POST`` method, you
must specify the document ID in the request URL.

For example, to create the document ``FishStew``, you would send the
following request:

.. code-block:: http

    PUT http://couchdb:5984/recipes/FishStew
    Content-Type: application/json

    {
      "servings" : 4,
      "subtitle" : "Delicious with fresh bread",
      "title" : "Fish Stew"
    }

The return type is JSON of the status, document ID,and revision number:

.. code-block:: javascript

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
    }

Updating an Existing Document
-----------------------------

To update an existing document you must specify the current revision
number within the ``_rev`` parameter. For example:

.. code-block:: http

    PUT http://couchdb:5984/recipes/FishStew
    Content-Type: application/json

    {
      "_rev" : "1-9c65296036141e575d32ba9c034dd3ee",
      "servings" : 4,
      "subtitle" : "Delicious with fresh salad",
      "title" : "Fish Stew"
    }

Alternatively, you can supply the current revision number in the
``If-Match`` HTTP header of the request. For example:

.. code-block:: http

    PUT http://couchdb:5984/recipes/FishStew
    If-Match: 2-d953b18035b76f2a5b1d1d93f25d3aea
    Content-Type: application/json

    {
       "servings" : 4,
       "subtitle" : "Delicious with fresh salad",
       "title" : "Fish Stew"
    }

The JSON returned will include the updated revision number:

.. code-block:: javascript

    {
       "id" : "FishStew99",
       "ok" : true,
       "rev" : "2-d953b18035b76f2a5b1d1d93f25d3aea"
    }

For information on batched writes, which can provide improved
performance, see :ref:`api/doc/batch-writes`.

.. _api/doc.delete:

``DELETE /db/doc``
==================

* **Method**: ``DELETE /db/doc``
* **Request**: None
* **Response**: JSON of the deleted revision
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
    Revision is missing, invalid or not the latest

Deletes the specified document from the database. You must supply the
current (latest) revision, either by using the ``rev`` parameter to
specify the revision:

.. code-block:: http

    DELETE http://couchdb:5984/recipes/FishStew?rev=3-a1a9b39ee3cc39181b796a69cb48521c
    Content-Type: application/json

Alternatively, you can use ETags with the ``If-Match`` field:

.. code-block:: http

    DELETE http://couchdb:5984/recipes/FishStew
    If-Match: 3-a1a9b39ee3cc39181b796a69cb48521c
    Content-Type: application/json


The returned JSON contains the document ID, revision and status:

.. code-block:: javascript

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "4-2719fd41187c60762ff584761b714cfb"
    }

.. note:: Note that deletion of a record increments the revision number. The
   use of a revision for deletion of the record allows replication of
   the database to correctly track the deletion in synchronized copies.

.. _api/doc.copy:

``COPY /db/doc``
================

* **Method**: ``COPY /db/doc``
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

* **Return Codes**:

  * **201**:
    Document has been copied and created successfully
  * **409**:
    Revision is missing, invalid or not the latest

The ``COPY`` command (which is non-standard HTTP) copies an existing
document to a new or existing document.

The source document is specified on the request line, with the
``Destination`` HTTP Header of the request specifying the target
document.

Copying a Document
------------------

You can copy the latest version of a document to a new document by
specifying the current document and target document:

.. code-block:: http

    COPY http://couchdb:5984/recipes/FishStew
    Content-Type: application/json
    Destination: IrishFishStew

The above request copies the document ``FishStew`` to the new document
``IrishFishStew``. The response is the ID and revision of the new
document.

.. code-block:: javascript

    {
       "id" : "IrishFishStew",
       "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
    }

Copying from a Specific Revision
--------------------------------

To copy *from* a specific version, use the ``rev`` argument to the query
string:

.. code-block:: http

    COPY http://couchdb:5984/recipes/FishStew?rev=5-acfd32d233f07cea4b4f37daaacc0082
    Content-Type: application/json
    Destination: IrishFishStew

The new document will be created using the information in the specified
revision of the source document.

Copying to an Existing Document
-------------------------------

To copy to an existing document, you must specify the current revision
string for the target document, using the ``rev`` parameter to the
``Destination`` HTTP Header string. For example:

.. code-block:: http

    COPY http://couchdb:5984/recipes/FishStew
    Content-Type: application/json
    Destination: IrishFishStew?rev=1-9c65296036141e575d32ba9c034dd3ee

The return value will be the new revision of the copied document:

.. code-block:: javascript

    {
       "id" : "IrishFishStew",
       "rev" : "2-55b6a1b251902a2c249b667dab1c6692"
    }

