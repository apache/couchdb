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


.. _api/doc:

``/db/doc``
===========

.. http:head:: /{db}/{docid}

  Returns the HTTP Headers containing a minimal amount of information
  about the specified document. The method supports the same query
  arguments as the :http:get:`/{db}/{docid}` method, but only the header
  information (including document size, and the revision as an ETag), is
  returned.

  The :http:header:`ETag` header shows the current revision for the requested
  document, and the :http:header:`Content-Length` specifies the length of the
  data, if the document were requested in full.

  Adding any of the query arguments (see :http:get:`/{db}/{docid}`), then the
  resulting HTTP Headers will correspond to what would be returned.

  :param db: Database name
  :param docid: Document ID
  :<header If-None-Match: Double quoted document's revision token
  :>header Content-Length: Document size
  :>header ETag: Double quoted document's revision token
  :code 200: Document exists
  :code 304: Document wasn't modified since specified revision
  :code 401: Read privilege required
  :code 404: Document not found

  **Request**:

  .. code-block:: http

    GET /db/SpaghettiWithMeatballs HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 660
    Content-Type: application/json
    Date: Tue, 13 Aug 2013 21:35:37 GMT
    ETag: "12-151bb8678d45aaa949ec3698ef1c7e78"
    Server: CouchDB (Erlang/OTP)


.. http:get:: /{db}/{docid}

  Returns document by the specified ``docid`` from the specified ``db``.
  Unless you request a specific revision, the latest revision of the
  document will always be returned.

  :param db: Database name
  :param docid: Document ID

  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`multipart/mixed`
                   - :mimetype:`text/plain`
  :<header If-None-Match: Double quoted document's revision token

  :query boolean attachments: Includes attachments bodies in response.
    Default is ``false``
  :query boolean att_encoding_info: Includes encoding information into
    attachment's stubs for compressed ones. Default is ``false``
  :query array atts_since: Includes attachments only since specified revisions.
    Doesn't includes attachments for specified revisions. *Optional*
  :query boolean conflicts: Includes information about conflicts in document.
    Default is ``false``
  :query boolean deleted_conflicts: Includes information about deleted
    conflicted revisions. Default is ``false``
  :query boolean latest: Forces retrieving latest "leaf" revision, no matter
    what `rev` was requested. Default is ``false``
  :query boolean local_seq: Includes last update sequence number for the
    document. Default is ``false``
  :query boolean meta: Acts same as specifying all `conflicts`,
    `deleted_conflicts` and `open_revs` query parameters. Default is ``false``
  :query array open_revs: Retrieves documents of specified leaf revisions.
    Additionally, it accepts value as ``all`` to return all leaf revisions.
    *Optional*
  :query string rev: Retrieves document of specified revision. *Optional*
  :query boolean revs: Includes list of all known document revisions.
    Default is ``false``
  :query boolean revs_info: Includes detailed information for all known
    document revisions. Default is ``false``

  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`multipart/mixed`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Double quoted document's revision token. Not available when
    retrieving conflicts-related information
  :>header Transfer-Encoding: ``chunked``. Available if requested with
    query parameter ``open_revs``

  :>json string _id: Document ID
  :>json string _rev: Revision MVCC token
  :>json boolean _deleted: Deletion flag. Available if document was removed
  :>json object _attachments: Attachment's stubs. Available if document has
    any attachments
  :>json array _conflicts: List of conflicted revisions. Available if requested
    with ``conflicts=true`` query parameter
  :>json array _deleted_conflicts: List of deleted conflicted revisions.
    Available if requested with ``deleted_conflicts=true`` query parameter
  :>json number _local_seq: Document's sequence number in current database.
    Available if requested with ``local_seq=true`` query parameter
  :>json array _revs_info: List of objects with information about local
    revisions and their status. Available if requested with ``open_revs`` query
    parameter
  :>json object _revisions: List of local revision tokens without.
    Available if requested with ``revs=true`` query parameter

  :code 200: Request completed successfully
  :code 304: Document wasn't modified since specified revision
  :code 400: The format of the request or revision was invalid
  :code 401: Read privilege required
  :code 404: Document not found

  **Request**:

  .. code-block:: http

    GET /recipes/SpaghettiWithMeatballs HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 660
    Content-Type: application/json
    Date: Tue, 13 Aug 2013 21:35:37 GMT
    ETag: "1-917fa2381192822767f010b95b45325b"
    Server: CouchDB (Erlang/OTP)

    {
        "_id": "SpaghettiWithMeatballs",
        "_rev": "1-917fa2381192822767f010b95b45325b",
        "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
        "ingredients": [
            "spaghetti",
            "tomato sauce",
            "meatballs"
        ],
        "name": "Spaghetti with meatballs"
    }

.. http:put:: /{db}/{docid}

  The :http:method:`PUT` method creates a new named document, or creates a new
  revision of the existing document. Unlike the :http:post:`/{db}` method, you
  must specify the document ID in the request URL.

  :param db: Database name
  :param docid: Document ID
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :<header If-Match: Document's revision. Alternative to `rev` query parameter
  :<header X-Couch-Full-Commit: Overrides server's
    :ref:`commit policy <config/couchdb/delayed_commits>`. Possible values
    are: ``false`` and ``true``. *Optional*
  :query string batch: Stores document in :ref:`batch mode
    <api/doc/batch-writes>` Possible values: ``ok``. *Optional*
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Quoted document's new revision
  :>header Location: Document URI
  :>json string id: Document ID
  :>json boolean ok: Operation status
  :>json string rev: Revision MVCC token
  :code 201: Document created and stored on disk
  :code 202: Document data accepted, but not yet stored on disk
  :code 400: Invalid request body or parameters
  :code 401: Write privileges required
  :code 404: Specified database or document ID doesn't exists
  :code 409: Document with the specified ID already exists or specified
    revision is not latest for target document

  **Request**:

  .. code-block:: http

    PUT /recipes/SpaghettiWithMeatballs HTTP/1.1
    Accept: application/json
    Content-Length: 196
    Content-Type: application/json
    Host: localhost:5984

    {
        "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
        "ingredients": [
            "spaghetti",
            "tomato sauce",
            "meatballs"
        ],
        "name": "Spaghetti with meatballs"
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 85
    Content-Type: application/json
    Date: Wed, 14 Aug 2013 20:31:39 GMT
    ETag: "1-917fa2381192822767f010b95b45325b"
    Location: http://localhost:5984/recipes/SpaghettiWithMeatballs
    Server: CouchDB (Erlang/OTP)

    {
        "id": "SpaghettiWithMeatballs",
        "ok": true,
        "rev": "1-917fa2381192822767f010b95b45325b"
    }


.. http:delete:: /{db}/{docid}

  Deletes the specified document from the database. You must supply the
  current (latest) revision, either by using the ``rev`` parameter to
  specify the revision.

  .. note::
    Note that deletion of a record increments the revision number.
    The use of a revision for deletion of the record allows replication of
    the database to correctly track the deletion in synchronized copies.

  :param db: Database name
  :param docid: Document ID
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header If-Match: Document's revision. Alternative to `rev` query parameter
  :<header X-Couch-Full-Commit: Overrides server's
    :ref:`commit policy <config/couchdb/delayed_commits>`. Possible values
    are: ``false`` and ``true``. *Optional*
  :query string rev: Actual document's revision
  :query string batch: Stores document in :ref:`batch mode
    <api/doc/batch-writes>` Possible values: ``ok``. *Optional*
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Double quoted document's new revision
  :>json string id: Document ID
  :>json boolean ok: Operation status
  :>json string rev: Revision MVCC token
  :code 200: Document successfully removed
  :code 202: Request was accepted, but changes are not yet stored on disk
  :code 400: Invalid request body or parameters
  :code 401: Write privileges required
  :code 404: Specified database or document ID doesn't exists
  :code 409: Specified revision is not the latest for target document

  **Request**:

  .. code-block:: http

    DELETE /recipes/FishStew?rev=1-9c65296036141e575d32ba9c034dd3ee HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  Alternatively, instead of ``rev`` query parameter you may use
  :http:header:`If-Match` header:

  .. code-block:: http

    DELETE /recipes/FishStew HTTP/1.1
    Accept: application/json
    If-Match: 1-9c65296036141e575d32ba9c034dd3ee
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 71
    Content-Type: application/json
    Date: Wed, 14 Aug 2013 12:23:13 GMT
    ETag: "2-056f5f44046ecafc08a2bc2b9c229e20"
    Server: CouchDB (Erlang/OTP)

    {
        "id": "FishStew",
        "ok": true,
        "rev": "2-056f5f44046ecafc08a2bc2b9c229e20"
    }


.. http:copy:: /{db}/{docid}

  The :http:method:`COPY` (which is non-standard HTTP) copies an existing
  document to a new or existing document.

  The source document is specified on the request line, with the
  :http:header:`Destination` header of the request specifying the target
  document.

  :param db: Database name
  :param docid: Document ID
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Destination: Destination document
  :<header If-Match: Source document's revision. Alternative to `rev` query
    parameter
  :<header X-Couch-Full-Commit: Overrides server's
    :ref:`commit policy <config/couchdb/delayed_commits>`. Possible values
    are: ``false`` and ``true``. *Optional*
  :query string rev: Revision to copy from. *Optional*
  :query string batch: Stores document in :ref:`batch mode
    <api/doc/batch-writes>` Possible values: ``ok``. *Optional*
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header ETag: Double quoted document's new revision
  :>header Location: Document URI
  :>json string id: Document document ID
  :>json boolean ok: Operation status
  :>json string rev: Revision MVCC token
  :code 201: Document successfully created
  :code 202: Request was accepted, but changes are not yet stored on disk
  :code 400: Invalid request body or parameters
  :code 401: Read or write privileges required
  :code 404: Specified database, document ID  or his revision doesn't exists
  :code 409: Document with the specified ID already exists or specified
    revision is not latest for target document

  **Request**:

  .. code-block:: http

    COPY /recipes/SpaghettiWithMeatballs HTTP/1.1
    Accept: application/json
    Destination: SpaghettiWithMeatballs_Italian
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 93
    Content-Type: application/json
    Date: Wed, 14 Aug 2013 14:21:00 GMT
    ETag: "1-e86fdf912560c2321a5fcefc6264e6d9"
    Location: http://localhost:5984/recipes/SpaghettiWithMeatballs_Italian
    Server: CouchDB (Erlang/OTP)

    {
        "id": "SpaghettiWithMeatballs_Italian",
        "ok": true,
        "rev": "1-e86fdf912560c2321a5fcefc6264e6d9"
    }


.. _api/doc/attachments:

Attachments
-----------

If the document includes attachments, then the returned structure will
contain a summary of the attachments associated with the document, but
not the attachment data itself.

The JSON for the returned document will include the ``_attachments``
field, with one or more attachment definitions.

The ``_attachments`` object keys are attachments names while values are
information objects with next structure:

- **content_type** (*string*): Attachment MIME type
- **data** (*string*): Base64-encoded content. Available if attachment content
  requested by using ``attachments=true`` or ``atts_since`` query parameters
- **digest** (*string*): Content hash digest.
  It starts with prefix which announce hash type (``md5-``) and continues with
  Base64-encoded hash digest
- **encoded_length** (*number*): Compressed attachment size in bytes
  Available when query parameter ``att_encoding_info=true`` was specified and
  ``content_type`` is in :ref:`list of compressiable types
  <config/attachments/compressible_types>`
- **encoding** (*string*): Compression codec. Available when query parameter
  ``att_encoding_info=true`` was specified
- **length** (*number*): Real attachment size in bytes. Not available if attachment
  content requested
- **revpos** (*number*): Revision *number* when attachment was added
- **stub** (*boolean*): Has ``true`` value if object contains stub info and no
  content. Otherwise omitted in response


Basic Attachments Info
^^^^^^^^^^^^^^^^^^^^^^

**Request**:

.. code-block:: http

  GET /recipes/SpaghettiWithMeatballs HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 660
  Content-Type: application/json
  Date: Tue, 13 Aug 2013 21:35:37 GMT
  ETag: "5-fd96acb3256302bf0dd2f32713161f2a"
  Server: CouchDB (Erlang/OTP)

  {
      "_attachments": {
          "grandma_recipe.txt": {
              "content_type": "text/plain",
              "digest": "md5-Ids41vtv725jyrN7iUvMcQ==",
              "length": 1872,
              "revpos": 4,
              "stub": true
          },
          "my_recipe.txt": {
              "content_type": "text/plain",
              "digest": "md5-198BPPNiT5fqlLxoYYbjBA==",
              "length": 85,
              "revpos": 5,
              "stub": true
          },
          "photo.jpg": {
              "content_type": "image/jpeg",
              "digest": "md5-7Pv4HW2822WY1r/3WDbPug==",
              "length": 165504,
              "revpos": 2,
              "stub": true
          }
      },
      "_id": "SpaghettiWithMeatballs",
      "_rev": "5-fd96acb3256302bf0dd2f32713161f2a",
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs"
  }


Retrieving Attachments Content
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It's possible to retrieve document with all attached files content by using
``attachements=true`` query parameter:

**Request**:

.. code-block:: http

  GET /db/pixel?attachments=true HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 553
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 11:32:40 GMT
  ETag: "4-f1bcae4bf7bbb92310079e632abfe3f4"
  Server: CouchDB (Erlang/OTP)

  {
      "_attachments": {
          "pixel.gif": {
              "content_type": "image/gif",
              "data": "R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7",
              "digest": "md5-2JdGiI2i2VELZKnwMers1Q==",
              "revpos": 2
          },
          "pixel.png": {
              "content_type": "image/png",
              "data": "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABAQMAAAAl21bKAAAAAXNSR0IArs4c6QAAAANQTFRFAAAAp3o92gAAAAF0Uk5TAEDm2GYAAAABYktHRACIBR1IAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH3QgOCx8VHgmcNwAAAApJREFUCNdjYAAAAAIAAeIhvDMAAAAASUVORK5CYII=",
              "digest": "md5-Dgf5zxgGuchWrve73evvGQ==",
              "revpos": 3
          }
      },
      "_id": "pixel",
      "_rev": "4-f1bcae4bf7bbb92310079e632abfe3f4"
  }

Or retrieve attached files content since specific revision using ``atts_since``
query parameter:

**Request**:

.. code-block:: http

  GET /recipes/SpaghettiWithMeatballs?atts_since=[%224-874985bc28906155ba0e2e0538f67b05%22]  HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 760
  Content-Type: application/json
  Date: Tue, 13 Aug 2013 21:35:37 GMT
  ETag: "5-fd96acb3256302bf0dd2f32713161f2a"
  Server: CouchDB (Erlang/OTP)

  {
      "_attachments": {
          "grandma_recipe.txt": {
              "content_type": "text/plain",
              "digest": "md5-Ids41vtv725jyrN7iUvMcQ==",
              "length": 1872,
              "revpos": 4,
              "stub": true
          },
          "my_recipe.txt": {
              "content_type": "text/plain",
              "data": "MS4gQ29vayBzcGFnaGV0dGkKMi4gQ29vayBtZWV0YmFsbHMKMy4gTWl4IHRoZW0KNC4gQWRkIHRvbWF0byBzYXVjZQo1LiAuLi4KNi4gUFJPRklUIQ==",
              "digest": "md5-198BPPNiT5fqlLxoYYbjBA==",
              "revpos": 5
          },
          "photo.jpg": {
              "content_type": "image/jpeg",
              "digest": "md5-7Pv4HW2822WY1r/3WDbPug==",
              "length": 165504,
              "revpos": 2,
              "stub": true
          }
      },
      "_id": "SpaghettiWithMeatballs",
      "_rev": "5-fd96acb3256302bf0dd2f32713161f2a",
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs"
  }


Efficient Multiple Attachments Retrieving
`````````````````````````````````````````

As you had noted above, retrieving document with ``attachements=true`` returns
large JSON object where all attachments are included.  While you document and
files are smaller it's ok, but if you have attached something bigger like media
files (audio/video), parsing such response might be very expensive.

To solve this problem, CouchDB allows to get documents in
:mimetype:`multipart/related` format:

**Request**:

.. code-block:: http

  GET /recipes/secret?attachments=true HTTP/1.1
  Accept: multipart/related
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Content-Length: 538
  Content-Type: multipart/related; boundary="e89b3e29388aef23453450d10e5aaed0"
  Date: Sat, 28 Sep 2013 08:08:22 GMT
  ETag: "2-c1c6c44c4bc3c9344b037c8690468605"
  Server: CouchDB (Erlang OTP)

  --e89b3e29388aef23453450d10e5aaed0
  Content-Type: application/json

  {"_id":"secret","_rev":"2-c1c6c44c4bc3c9344b037c8690468605","_attachments":{"recipe.txt":{"content_type":"text/plain","revpos":2,"digest":"md5-HV9aXJdEnu0xnMQYTKgOFA==","length":86,"follows":true}}}
  --e89b3e29388aef23453450d10e5aaed0
  Content-Disposition: attachment; filename="recipe.txt"
  Content-Type: text/plain
  Content-Length: 86

  1. Take R
  2. Take E
  3. Mix with L
  4. Add some A
  5. Serve with X

  --e89b3e29388aef23453450d10e5aaed0--

In this response the document contains only attachments stub information and
quite short while all attachments goes as separate entities which reduces
memory footprint and processing overhead (you'd noticed, that attachment content
goes as raw data, not in base64 encoding, right?).


Retrieving Attachments Encoding Info
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By using ``att_encoding_info=true`` query parameter you may retrieve information
about compressed attachments size and used codec.

**Request**:

.. code-block:: http

  GET /recipes/SpaghettiWithMeatballs?att_encoding_info=true HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 736
  Content-Type: application/json
  Date: Tue, 13 Aug 2013 21:35:37 GMT
  ETag: "5-fd96acb3256302bf0dd2f32713161f2a"
  Server: CouchDB (Erlang/OTP)

  {
      "_attachments": {
          "grandma_recipe.txt": {
              "content_type": "text/plain",
              "digest": "md5-Ids41vtv725jyrN7iUvMcQ==",
              "encoded_length": 693,
              "encoding": "gzip",
              "length": 1872,
              "revpos": 4,
              "stub": true
          },
          "my_recipe.txt": {
              "content_type": "text/plain",
              "digest": "md5-198BPPNiT5fqlLxoYYbjBA==",
              "encoded_length": 100,
              "encoding": "gzip",
              "length": 85,
              "revpos": 5,
              "stub": true
          },
          "photo.jpg": {
              "content_type": "image/jpeg",
              "digest": "md5-7Pv4HW2822WY1r/3WDbPug==",
              "length": 165504,
              "revpos": 2,
              "stub": true
          }
      },
      "_id": "SpaghettiWithMeatballs",
      "_rev": "5-fd96acb3256302bf0dd2f32713161f2a",
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs"
  }


Creating Multiple Attachments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To create a document with multiple attachments with single request you need
just inline base64 encoded attachments data into the document body:

.. code-block:: javascript

  {
    "_id":"multiple_attachments",
    "_attachments":
    {
      "foo.txt":
      {
        "content_type":"text\/plain",
        "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      },

     "bar.txt":
      {
        "content_type":"text\/plain",
        "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
      }
    }
  }

Alternatively, you can upload a document with attachments more efficiently in
:mimetype:`multipart/related` format. This avoids having to Base64-encode
the attachments, saving CPU and bandwidth. To do this, set the
:http:header:`Content-Type` header of the :http:put:`/{db}/{docid}` request to
:mimetype:`multipart/related`.

The first MIME body is the document itself, which should have its own
:http:header:`Content-Type` of :mimetype:`application/json"`. It also should
include  an ``_attachments`` metadata object in which each attachment object
has a key ``follows`` with value ``true``.

The subsequent MIME bodies are the attachments.

**Request**:

.. code-block:: http

  PUT /temp/somedoc HTTP/1.1
  Accept: application/json
  Content-Length: 372
  Content-Type: multipart/related;boundary="abc123"
  Host: localhost:5984
  User-Agent: HTTPie/0.6.0

  --abc123
  Content-Type: application/json

  {
      "body": "This is a body.",
      "_attachments": {
          "foo.txt": {
              "follows": true,
              "content_type": "text/plain",
              "length": 21
          },
          "bar.txt": {
              "follows": true,
              "content_type": "text/plain",
              "length": 20
          }
      }
  }

  --abc123

  this is 21 chars long
  --abc123

  this is 20 chars lon
  --abc123--

**Response**:

.. code-block:: http

  HTTP/1.1 201 Created
  Cache-Control: must-revalidate
  Content-Length: 72
  Content-Type: application/json
  Date: Sat, 28 Sep 2013 09:13:24 GMT
  ETag: "1-5575e26acdeb1df561bb5b70b26ba151"
  Location: http://localhost:5984/temp/somedoc
  Server: CouchDB (Erlang OTP)

  {
      "id": "somedoc",
      "ok": true,
      "rev": "1-5575e26acdeb1df561bb5b70b26ba151"
  }


Getting a List of Revisions
---------------------------

You can obtain a list of the revisions for a given document by adding
the ``revs=true`` parameter to the request URL:

**Request**:

.. code-block:: http

  GET /recipes/SpaghettiWithMeatballs?revs=true  HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 584
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 11:38:26 GMT
  ETag: "5-fd96acb3256302bf0dd2f32713161f2a"
  Server: CouchDB (Erlang/OTP)

  {
      "_id": "SpaghettiWithMeatballs",
      "_rev": "8-6f5ad8db0f34af24a6e0984cd1a6cfb9",
      "_revisions": {
          "ids": [
              "6f5ad8db0f34af24a6e0984cd1a6cfb9",
              "77fba3a059497f51ec99b9b478b569d2",
              "136813b440a00a24834f5cb1ddf5b1f1",
              "fd96acb3256302bf0dd2f32713161f2a",
              "874985bc28906155ba0e2e0538f67b05",
              "0de77a37463bf391d14283e626831f2e",
              "d795d1b924777732fdea76538c558b62",
              "917fa2381192822767f010b95b45325b"
          ],
          "start": 8
      },
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs"
  }


The returned JSON structure includes the original document, including a
``_revisions`` structure that includes the revision information in next form:

- **ids** (*array*): Array of valid revision IDs, in reverse order
    (latest first)
- **start** (*number*): Prefix number for the latest revision


Obtaining an Extended Revision History
--------------------------------------

You can get additional information about the revisions for a given
document by supplying the ``revs_info`` argument to the query:

**Request**:

.. code-block:: http

  GET /recipes/SpaghettiWithMeatballs?revs_info=true  HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 802
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 11:40:55 GMT
  Server: CouchDB (Erlang/OTP)

  {
      "_id": "SpaghettiWithMeatballs",
      "_rev": "8-6f5ad8db0f34af24a6e0984cd1a6cfb9",
      "_revs_info": [
          {
              "rev": "8-6f5ad8db0f34af24a6e0984cd1a6cfb9",
              "status": "available"
          },
          {
              "rev": "7-77fba3a059497f51ec99b9b478b569d2",
              "status": "deleted"
          },
          {
              "rev": "6-136813b440a00a24834f5cb1ddf5b1f1",
              "status": "available"
          },
          {
              "rev": "5-fd96acb3256302bf0dd2f32713161f2a",
              "status": "missing"
          },
          {
              "rev": "4-874985bc28906155ba0e2e0538f67b05",
              "status": "missing"
          },
          {
              "rev": "3-0de77a37463bf391d14283e626831f2e",
              "status": "missing"
          },
          {
              "rev": "2-d795d1b924777732fdea76538c558b62",
              "status": "missing"
          },
          {
              "rev": "1-917fa2381192822767f010b95b45325b",
              "status": "missing"
          }
      ],
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs"
  }


The returned document contains ``_rev_info`` field with extended revision
information, including the availability and status of each revision. This array
field contains objects with following structure:

- **rev** (*string*): Full revision string
- **status** (*string*): Status of the revision.
  Maybe one of:

  - ``available``: Revision is available for retrieving with `rev` query
    parameter
  - ``missing``: Revision is not available
  - ``deleted``: Revision belongs to deleted document


Obtaining a Specific Revision
-----------------------------

To get a specific revision, use the ``rev`` argument to the request, and
specify the full revision number. The specified revision of the document will
be returned, including a ``_rev`` field specifying the revision that was
requested.

**Request**:

.. code-block:: http

  GET /recipes/SpaghettiWithMeatballs?rev=6-136813b440a00a24834f5cb1ddf5b1f1  HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 271
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 11:40:55 GMT
  Server: CouchDB (Erlang/OTP)

  {
      "_id": "SpaghettiWithMeatballs",
      "_rev": "6-136813b440a00a24834f5cb1ddf5b1f1",
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs"
  }


Retrieving Deleted Documents
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

CouchDB doesn't actually deletes documents via :http:delete:`/{db}/{docid}`.
Instead of this, it leaves tombstone with very basic information about document.
If you just :http:get:`/{db}/{docid}` CouchDB returns :http:statuscode:`404`
response:

**Request**:

.. code-block:: http

  GET /recipes/FishStew  HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 404 Object Not Found
  Cache-Control: must-revalidate
  Content-Length: 41
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 12:23:27 GMT
  Server: CouchDB (Erlang/OTP)

  {
      "error": "not_found",
      "reason": "deleted"
  }

However, you may retrieve document's tombstone by using ``rev`` query parameter
with :http:get:`/{db}/{docid}` request:

**Request**:

.. code-block:: http

  GET /recipes/FishStew?rev=2-056f5f44046ecafc08a2bc2b9c229e20  HTTP/1.1
  Accept: application/json
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 200 OK
  Cache-Control: must-revalidate
  Content-Length: 79
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 12:30:22 GMT
  ETag: "2-056f5f44046ecafc08a2bc2b9c229e20"
  Server: CouchDB (Erlang/OTP)

  {
      "_deleted": true,
      "_id": "FishStew",
      "_rev": "2-056f5f44046ecafc08a2bc2b9c229e20"
  }


Updating an Existing Document
-----------------------------

To update an existing document you must specify the current revision
number within the ``_rev`` parameter.

**Request**:

.. code-block:: http

  PUT /recipes/SpaghettiWithMeatballs HTTP/1.1
  Accept: application/json
  Content-Length: 258
  Content-Type: application/json
  Host: localhost:5984

  {
      "_rev": "1-917fa2381192822767f010b95b45325b",
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs",
      "serving": "hot"
  }

Alternatively, you can supply the current revision number in the
``If-Match`` HTTP header of the request:

.. code-block:: http

  PUT /recipes/SpaghettiWithMeatballs HTTP/1.1
  Accept: application/json
  Content-Length: 258
  Content-Type: application/json
  If-Match: 1-917fa2381192822767f010b95b45325b
  Host: localhost:5984

  {
      "description": "An Italian-American dish that usually consists of spaghetti, tomato sauce and meatballs.",
      "ingredients": [
          "spaghetti",
          "tomato sauce",
          "meatballs"
      ],
      "name": "Spaghetti with meatballs",
      "serving": "hot"
  }


**Response**:

.. code-block:: http

  HTTP/1.1 201 Created
  Cache-Control: must-revalidate
  Content-Length: 85
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 20:33:56 GMT
  ETag: "2-790895a73b63fb91dd863388398483dd"
  Location: http://localhost:5984/recipes/SpaghettiWithMeatballs
  Server: CouchDB (Erlang/OTP)

  {
      "id": "SpaghettiWithMeatballs",
      "ok": true,
      "rev": "2-790895a73b63fb91dd863388398483dd"
  }


Copying from a Specific Revision
--------------------------------

To copy *from* a specific version, use the ``rev`` argument to the query
string or :http:header:`If-Match`:

**Request**:

.. code-block:: http

  COPY /recipes/SpaghettiWithMeatballs HTTP/1.1
  Accept: application/json
  Destination: http://localhost:5984/recipes_old/SpaghettiWithMeatballs_Original
  If-Match: 1-917fa2381192822767f010b95b45325b
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 201 Created
  Cache-Control: must-revalidate
  Content-Length: 93
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 14:21:00 GMT
  ETag: "1-917fa2381192822767f010b95b45325b"
  Location: http://localhost:5984/recipes_old/SpaghettiWithMeatballs_Original
  Server: CouchDB (Erlang/OTP)

  {
      "id": "SpaghettiWithMeatballs_Original",
      "ok": true,
      "rev": "1-917fa2381192822767f010b95b45325b"
  }


Copying to an Existing Document
-------------------------------

To copy to an existing document, you must specify the current revision
string for the target document by appending the ``rev`` parameter to the
:http:header:`Destination` header string.

**Request**:

.. code-block:: http

  COPY /recipes/SpaghettiWithMeatballs?rev=8-6f5ad8db0f34af24a6e0984cd1a6cfb9 HTTP/1.1
  Accept: application/json
  Destination: http://localhost:5984/recipes_old/SpaghettiWithMeatballs_Original?rev=1-917fa2381192822767f010b95b45325b
  Host: localhost:5984

**Response**:

.. code-block:: http

  HTTP/1.1 201 Created
  Cache-Control: must-revalidate
  Content-Length: 93
  Content-Type: application/json
  Date: Wed, 14 Aug 2013 14:21:00 GMT
  ETag: "2-62e778c9ec09214dd685a981dcc24074""
  Location: http://localhost:5984/recipes_old/SpaghettiWithMeatballs_Original
  Server: CouchDB (Erlang/OTP)

  {
      "id": "SpaghettiWithMeatballs_Original",
      "ok": true,
      "rev": "2-62e778c9ec09214dd685a981dcc24074"
  }
