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


.. _api/doc/attachment:
.. _api/doc/attachment.get:

``GET /db/doc/attachment``
==========================

* **Method**: ``GET /db/doc/attachment``
* **Request**: None
* **Response**: Returns the attachment data
* **Admin Privileges Required**: no

Returns the file attachment ``attachment`` associated with the document
``doc``. The raw data of the associated attachment is returned (just as
if you were accessing a static file. The returned HTTP ``Content-type``
will be the same as the content type set when the document attachment
was submitted into the database.

.. _api/doc/attachment/range:

HTTP Range Requests
-------------------

HTTP allows you to specify byte ranges for requests. This allows the
implementation of resumable downloads and skippable audio and video
streams alike. This is available for all attachments inside CouchDB.

This is just a real quick run through how this looks under the hood.
Usually, you will have larger binary files to serve from CouchDB, like
MP3s and videos, but to make things a little more obvious, I use a text
file here (Note that I use the ``application/octet-stream`` Content-Type
instead of ``text/plain``).

.. code-block:: bash

    shell> cat file.txt
    My hovercraft is full of eels!

Now let's store this text file as an attachment in CouchDB. First, we
create a database:

.. code-block:: bash

    shell> curl -X PUT http://127.0.0.1:5984/test
    {"ok":true}

Then we create a new document and the file attachment in one go:

.. code-block:: bash

    shell> curl -X PUT http://127.0.0.1:5984/test/doc/file.txt \
                -H "Content-Type: application/octet-stream" -d@file.txt
    {"ok":true,"id":"doc","rev":"1-287a28fa680ae0c7fb4729bf0c6e0cf2"}

Now we can request the whole file easily:

.. code-block:: bash

    shell> curl -X GET http://127.0.0.1:5984/test/doc/file.txt
    My hovercraft is full of eels!

But say we only want the first 13 bytes:

.. code-block:: bash

    shell> curl -X GET http://127.0.0.1:5984/test/doc/file.txt \
                -H "Range: bytes=0-12"
    My hovercraft

HTTP supports many ways to specify single and even multiple byte
ranges. Read all about it in `RFC 2616`_.

.. note::
   Databases that have been created with CouchDB 1.0.2 or earlier will
   support range requests in |version|, but they are using a less-optimal
   algorithm. If you plan to make heavy use of this feature, make sure
   to compact your database with CouchDB |version| to take advantage of a
   better algorithm to find byte ranges.

.. _RFC 2616: http://tools.ietf.org/html/rfc2616#section-14.27


.. _api/doc/attachment.put:

``PUT /db/doc/attachment``
==========================

* **Method**: ``PUT /db/doc/attachment``
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

* **Return Codes**:

  * **201**:
    Attachment has been accepted

Upload the supplied content as an attachment to the specified document
(``doc``). The ``attachment`` name provided must be a URL encoded
string. You must also supply either the ``rev`` query argument or the
``If-Match`` HTTP header for validation, and the HTTP headers (to set
the attachment content type). The content type is used when the
attachment is requested as the corresponding content-type in the
returned document header.

For example, you could upload a simple text document using the following
request:

.. code-block:: http

    PUT http://couchdb:5984/recipes/FishStew/basic?rev=8-a94cb7e50ded1e06f943be5bfbddf8ca
    Content-Length: 10
    Content-Type: text/plain

    Roast it

Or by using the ``If-Match`` HTTP header:

.. code-block:: http

    PUT http://couchdb:5984/recipes/FishStew/basic
    If-Match: 8-a94cb7e50ded1e06f943be5bfbddf8ca
    Content-Length: 10
    Content-Type: text/plain

    Roast it

The returned JSON contains the new document information:

.. code-block:: javascript

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "9-247bb19a41bfd9bfdaf5ee6e2e05be74"
    }

.. note:: Uploading an attachment updates the corresponding document revision.
   Revisions are tracked for the parent document, not individual
   attachments.

Updating an Existing Attachment
-------------------------------

Uploading an attachment using an existing attachment name will update
the corresponding stored content of the database. Since you must supply
the revision information to add an attachment to a document, this serves
as validation to update the existing attachment.

.. _api/doc/attachment.delete:

``DELETE /db/doc/attachment``
=============================

* **Method**: ``DELETE /db/doc/attachment``
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

Deletes the attachment ``attachment`` to the specified ``doc``. You must
supply the ``rev`` argument with the current revision to delete the
attachment.

For example to delete the attachment ``basic`` from the recipe
``FishStew``:

.. code-block:: http

    DELETE http://couchdb:5984/recipes/FishStew/basic?rev=9-247bb19a41bfd9bfdaf5ee6e2e05be74
    Content-Type: application/json



The returned JSON contains the updated revision information:

.. code-block:: javascript

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "10-561bf6b1e27615cee83d1f48fa65dd3e"
    }

.. _JSON object: #table-couchdb-api-db_db-json-changes
.. _POST: #couchdb-api-dbdoc_db_post
