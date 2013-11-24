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


.. _api/db/purge:

``/db/_purge``
==============

.. http:post:: /{db}/_purge
  :synopsis: Purges some historical documents entirely from database history

  A database purge permanently removes the references to deleted documents
  from the database. Normal deletion of a document within CouchDB does not
  remove the document from the database, instead, the document is marked as
  ``_deleted=true`` (and a new revision is created). This is to ensure that
  deleted documents can be replicated to other databases as having been
  deleted. This also means that you can check the status of a document and
  identify that the document has been deleted by its absence.

  .. warning::

     Purging a document from a database should only be done as a last resort
     when sensitive information has been introduced inadvertently into a
     database. In clustered or replicated environments it is very difficult
     to guarantee that a particular purged document has been removed from all
     replicas. Do not rely on this API as a way of doing secure deletion.

  The purge operation removes the references to the deleted documents from
  the database. The purging of old documents is not replicated to other
  databases. If you are replicating between databases and have deleted a
  large number of documents you should run purge on each database.

  .. note::

     Purging documents does not remove the space used by them on disk. To
     reclaim disk space, you should run a database compact (see
     :ref:`api/db/compact`), and compact views (see :ref:`api/db/compact/ddoc`).

  The format of the request must include the document ID and one or more
  revisions that must be purged.

  The response will contain the purge sequence number, and a list of the
  document IDs and revisions successfully purged.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :<json object: Mapping of document ID to list of revisions to purge
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json number purge_seq: Purge sequence number
  :>json object purged: Mapping of document ID to list of purged revisions
  :code 200: Request completed successfully
  :code 400: Invalid database name or JSON payload
  :code 415: Bad :header:`Content-Type` value

  **Request**:

  .. code-block:: http

    POST /db/_purge HTTP/1.1
    Accept: application/json
    Content-Length: 76
    Content-Type: application/json
    Host: localhost:5984

    {
      "c6114c65e295552ab1019e2b046b10e": [
        "3-b06fcd1c1c9e0ec7c480ee8aa467bf3b",
        "3-0e871ef78849b0c206091f1a7af6ec41"
      ]
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 103
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 10:53:24 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "purge_seq":3,
      "purged":{
        "c6114c65e295552ab1019e2b046b10e": [
          "3-b06fcd1c1c9e0ec7c480ee8aa467bf3b"
        ]
      }
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


.. _api/db/missing_revs:

``/db/_missing_revs``
=====================

.. http:post:: /{db}/_missing_revs
  :synopsis: By given list of document revisions returns the document revisions that do not exist in the database

  With given a list of document revisions, returns the document revisions that
  do not exist in the database.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :<json object: Mapping of document ID to list of revisions to lookup
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json object missing_revs: Mapping of document ID to list of missed revisions
  :code 200: Request completed successfully
  :code 400: Invalid database name or JSON payload

  **Request**:

  .. code-block:: http

    POST /db/_missing_revs HTTP/1.1
    Accept: application/json
    Content-Length: 76
    Content-Type: application/json
    Host: localhost:5984

    {
      "c6114c65e295552ab1019e2b046b10e": [
        "3-b06fcd1c1c9e0ec7c480ee8aa467bf3b",
        "3-0e871ef78849b0c206091f1a7af6ec41"
      ]
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 64
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 10:53:24 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "missed_revs":{
        "c6114c65e295552ab1019e2b046b10e": [
          "3-b06fcd1c1c9e0ec7c480ee8aa467bf3b"
        ]
      }
    }


.. _api/db/revs_diff:

``/db/_revs_diff``
==================

.. http:post:: /{db}/_revs_diff
  :synopsis: By given list of document revisions returns differences between the given revisions and ones that are in the database

  Given a set of document/revision IDs, returns the subset of those that do
  not correspond to revisions stored in the database.

  Its primary use is by the replicator, as an important optimization: after
  receiving a set of new revision IDs from the source database, the replicator
  sends this set to the destination database's ``_revs_diff`` to find out which
  of them already exist there. It can then avoid fetching and sending
  already-known document bodies.

  Both the request and response bodies are JSON objects whose keys are document
  IDs; but the values are structured differently:

  - In the request, a value is an array of revision IDs for that document.

  - In the response, a value is an object with a ``missing``: key, whose value
    is a list of revision IDs for that document (the ones that are not stored
    in the database) and optionally a ``possible_ancestors`` key, whose value is
    an array of revision IDs that are known that might be ancestors of
    the missing revisions.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :<json object: Mapping of document ID to list of revisions to lookup
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json array missing: List of missed revisions for specified document
  :>json array possible_ancestors: List of revisions that *may be* ancestors
    for specified document and his current revision in requested database
  :code 200: Request completed successfully
  :code 400: Invalid database name or JSON payload

  **Request**:

  .. code-block:: http

    POST /db/_revs_diff HTTP/1.1
    Accept: application/json
    Content-Length: 113
    Content-Type: application/json
    Host: localhost:5984

    {
      "190f721ca3411be7aa9477db5f948bbb": [
        "3-bb72a7682290f94a985f7afac8b27137",
        "4-10265e5a26d807a3cfa459cf1a82ef2e",
        "5-067a00dff5e02add41819138abb3284d"
      ]
    }

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 88
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 16:56:02 GMT
    Server: CouchDB (Erlang/OTP)

    {
      "190f721ca3411be7aa9477db5f948bbb": {
        "missing": [
          "3-bb72a7682290f94a985f7afac8b27137",
          "5-067a00dff5e02add41819138abb3284d"
        ],
        "possible_ancestors": [
          "4-10265e5a26d807a3cfa459cf1a82ef2e"
        ]
      }
    }


.. _api/db/revs_limit:

``/db/_revs_limit``
===================

.. http:get:: /{db}/_revs_limit
  :synopsis: Returns the limit of historical revisions to store for a single document in the database

  Gets the current ``revs_limit`` (revision limit) setting.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :code 200: Request completed successfully

  **Request**:

  .. code-block:: http

    GET /db/_revs_limit HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 5
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 17:27:30 GMT
    Server: CouchDB (Erlang/OTP)

    1000


.. http:put:: /{db}/_revs_limit
  :synopsis: Sets the limit of historical revisions to store for a single document in the database

  Sets the maximum number of document revisions that will be tracked by
  CouchDB, even after compaction has occurred. You can set the revision
  limit on a database with a scalar integer of the limit that you want
  to set as the request body.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json boolean ok: Operation status
  :code 200: Request completed successfully
  :code 400: Invalid JSON data

  **Request**:

  .. code-block:: http

    PUT /db/_revs_limit HTTP/1.1
    Accept: application/json
    Content-Length: 5
    Content-Type: application/json
    Host: localhost:5984

    1000

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 12
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 17:47:52 GMT
    Server: CouchDB (Erlang/OTP)

    {
        "ok": true
    }
