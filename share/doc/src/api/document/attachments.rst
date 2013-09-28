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

``/db/doc/attachment``
======================

.. http:head:: /{db}/{docid}/{attname}

  Returns the HTTP headers containing a minimal amount of information
  about the specified attachment. The method supports the same query
  arguments as the :get:`/{db}/{docid}/{attname}` method, but only
  the header information (including attachment size, encoding and the MD5 hash
  as an :header:`ETag`), is returned.

  :param db: Database name
  :param docid: Document ID
  :param attname: Attachment name
  :<header If-Match: Document's revision. Alternative to `rev` query parameter
  :<header If-None-Match: Attachment's base64 encoded MD5 binary digest.
    *Optional*
  :query string rev: Document's revision. *Optional*
  :>header Accept-Ranges: :ref:`Range request aware <api/doc/attachment/range>`.
    Used for attachments with :mimetype:`application/octet-stream` content type
  :>header Content-Encoding: Used compression codec. Available if attachment's
    ``content_type`` is in :ref:`list of compressiable types
    <config/attachments/compressible_types>`
  :>header Content-Length: Attachment size. If compression codec was used,
    this value is about compressed size, not actual
  :>header Content-MD5: Base64 encoded MD5 binary digest
  :>header ETag: Double quoted base64 encoded MD5 binary digest
  :code 200: Attachment exists
  :code 304: Attachment wasn't modified if :header:`ETag` equals specified
    :header:`If-None-Match` header
  :code 401: Read privilege required
  :code 404: Specified database, document or attachment was not found

  **Request**:

  .. code-block:: http

    HEAD /recipes/SpaghettiWithMeatballs/recipe.txt HTTP/1.1
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Accept-Ranges: none
    Cache-Control: must-revalidate
    Content-Encoding: gzip
    Content-Length: 100
    Content-MD5: vVa/YgiE1+Gh0WfoFJAcSg==
    Content-Type: text/plain
    Date: Thu, 15 Aug 2013 12:42:42 GMT
    ETag: "vVa/YgiE1+Gh0WfoFJAcSg=="
    Server: CouchDB (Erlang/OTP)


.. http:get:: /{db}/{docid}/{attname}

  Returns the file attachment associated with the document.
  The raw data of the associated attachment is returned (just as if you were
  accessing a static file. The returned :header:`Content-Type`
  will be the same as the content type set when the document attachment
  was submitted into the database.

  :param db: Database name
  :param docid: Document ID
  :param attname: Attachment name
  :<header If-Match: Document's revision. Alternative to `rev` query parameter
  :<header If-None-Match: Attachment's base64 encoded MD5 binary digest.
    *Optional*
  :query string rev: Document's revision. *Optional*
  :>header Accept-Ranges: :ref:`Range request aware <api/doc/attachment/range>`.
    Used for attachments with :mimetype:`application/octet-stream`
  :>header Content-Encoding: Used compression codec. Available if attachment's
    ``content_type`` is in :ref:`list of compressiable types
    <config/attachments/compressible_types>`
  :>header Content-Length: Attachment size. If compression codec is used,
    this value is about compressed size, not actual
  :>header Content-MD5: Base64 encoded MD5 binary digest
  :>header ETag: Double quoted base64 encoded MD5 binary digest
  :response: Stored content
  :code 200: Attachment exists
  :code 304: Attachment wasn't modified if :header:`ETag` equals specified
    :header:`If-None-Match` header
  :code 401: Read privilege required
  :code 404: Specified database, document or attachment was not found


.. http:put:: /{db}/{docid}/{attname}

  Uploads the supplied content as an attachment to the specified document.
  The attachment name provided must be a URL encoded string. You must also
  supply either the ``rev`` query argument or the :header:`If-Match`
  HTTP header for validation, and the HTTP headers (to set the attachment
  content type).

  If case when uploading an attachment using an existing attachment name,
  CouchDB will update the corresponding stored content of the database.
  Since you must supply the revision information to add an attachment to
  the document, this serves as validation to update the existing attachment.

  .. note::
     Uploading an attachment updates the corresponding document revision.
     Revisions are tracked for the parent document, not individual attachments.

  :param db: Database name
  :param docid: Document ID
  :param attname: Attachment name
  :<header Content-Type: Attachment MIME type. *Required*
  :<header If-Match: Document revision. Alternative to `rev` query parameter
  :query string rev: Document revision. *Required*
  :>header Accept-Ranges: :ref:`Range request aware <api/doc/attachment/range>`.
    Used for attachments with :mimetype:`application/octet-stream`
  :>header Content-Encoding: Used compression codec. Available if attachment's
    ``content_type`` is in :ref:`list of compressiable types
    <config/attachments/compressible_types>`
  :>header Content-Length: Attachment size. If compression codec is used,
    this value is about compressed size, not actual
  :>header Content-MD5: Base64 encoded MD5 binary digest
  :>header ETag: Double quoted base64 encoded MD5 binary digest
  :>json string id: Document ID
  :>json boolean ok: Operation status
  :>json string rev: Revision MVCC token
  :code 200: Attachment successfully removed
  :code 202: Request was accepted, but changes are not yet stored on disk
  :code 400: Invalid request body or parameters
  :code 401: Write privileges required
  :code 404: Specified database, document or attachment was not found
  :code 409: Document's revision wasn't specified or it's not the latest

  **Request**:

  .. code-block:: http

    PUT /recipes/SpaghettiWithMeatballs/recipe.txt HTTP/1.1
    Accept: application/json
    Content-Length: 86
    Content-Type: text/plain
    Host: localhost:5984
    If-Match: 1-917fa2381192822767f010b95b45325b

    1. Cook spaghetti
    2. Cook meatballs
    3. Mix them
    4. Add tomato sauce
    5. ...
    6. PROFIT!

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 85
    Content-Type: application/json
    Date: Thu, 15 Aug 2013 12:38:04 GMT
    ETag: "2-ce91aed0129be8f9b0f650a2edcfd0a4"
    Location: http://localhost:5984/recipes/SpaghettiWithMeatballs/recipe.txt
    Server: CouchDB (Erlang/OTP)

    {
        "id": "SpaghettiWithMeatballs",
        "ok": true,
        "rev": "2-ce91aed0129be8f9b0f650a2edcfd0a4"
    }


.. http:delete:: /{db}/{docid}/{attname}

  Deletes the attachment ``attachment`` of the specified ``doc``. You must
  supply the ``rev`` query parameter or :header:`If-Match` with the current
  revision to delete the attachment.

  .. note::
     Deleting an attachment updates the corresponding document revision.
     Revisions are tracked for the parent document, not individual attachments.

  :param db: Database name
  :param docid: Document ID
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header If-Match: Document revision. Alternative to `rev` query parameter
  :<header X-Couch-Full-Commit: Overrides server's
    :ref:`commit policy <config/couchdb/delayed_commits>`. Possible values
    are: ``false`` and ``true``. *Optional*
  :query string rev: Document revision. *Required*
  :query string batch: Store changes in :ref:`batch mode
    <api/doc/batch-writes>` Possible values: ``ok``. *Optional*
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Double quoted document's new revision
  :>json string id: Document ID
  :>json boolean ok: Operation status
  :>json string rev: Revision MVCC token
  :code 200: Attachment successfully removed
  :code 202: Request was accepted, but changes are not yet stored on disk
  :code 400: Invalid request body or parameters
  :code 401: Write privileges required
  :code 404: Specified database, document or attachment was not found
  :code 409: Document's revision wasn't specified or it's not the latest

  **Request**:

  .. code-block:: http

    DELETE /recipes/SpaghettiWithMeatballs?rev=6-440b2dd39c20413045748b42c6aba6e2 HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  Alternatively, instead of ``rev`` query parameter you may use
  :header:`If-Match` header:

  .. code-block:: http

    DELETE /recipes/SpaghettiWithMeatballs HTTP/1.1
    Accept: application/json
    If-Match: 6-440b2dd39c20413045748b42c6aba6e2
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 85
    Content-Type: application/json
    Date: Wed, 14 Aug 2013 12:23:13 GMT
    ETag: "7-05185cf5fcdf4b6da360af939431d466"
    Server: CouchDB (Erlang/OTP)

    {
        "id": "SpaghettiWithMeatballs",
        "ok": true,
        "rev": "7-05185cf5fcdf4b6da360af939431d466"
    }


.. _api/doc/attachment/range:

HTTP Range Requests
-------------------

HTTP allows you to specify byte ranges for requests. This allows the
implementation of resumable downloads and skippable audio and video
streams alike. This is available for all attachments inside CouchDB.

This is just a real quick run through how this looks under the hood.
Usually, you will have larger binary files to serve from CouchDB, like
MP3s and videos, but to make things a little more obvious, I use a text
file here (Note that I use the :mimetype:`application/octet-stream`
:header`Content-Type` instead of :mimetype:`text/plain`).

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
ranges. Read all about it in :rfc:`2616#section-14.27`.

.. note::
   Databases that have been created with CouchDB 1.0.2 or earlier will
   support range requests in |version|, but they are using a less-optimal
   algorithm. If you plan to make heavy use of this feature, make sure
   to compact your database with CouchDB |version| to take advantage of a
   better algorithm to find byte ranges.
