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

.. _api/db/compact:

==================
``/{db}/_compact``
==================

.. http:post:: /{db}/_compact
    :synopsis: Starts a compaction for the database

    Request compaction of the specified database. Compaction compresses the
    disk database file by performing the following operations:

    - Writes a new, optimised, version of the database file, removing any
      unused sections from the new version during write. Because a new file is
      temporarily created for this purpose, you may require up to twice the
      current storage space of the specified database in order for the
      compaction routine to complete.
    - Removes the bodies of any non-leaf revisions of documents from the
      database.
    - Removes old revision history beyond the limit specified by the
      ``_revs_limit`` database parameter.

    Compaction can only be requested on an individual database; you cannot
    compact all the databases for a CouchDB instance. The compaction process
    runs as a background process.

    You can determine if the compaction process is operating on a database
    by obtaining the database meta information, the ``compact_running``
    value of the returned database structure will be set to true. See
    :get:`/{db}`.

    You can also obtain a list of running processes to determine whether
    compaction is currently running. See :ref:`api/server/active_tasks`.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json boolean ok: Operation status
    :code 202: Compaction request has been accepted
    :code 400: Invalid database name
    :code 401: CouchDB Server Administrator privileges required
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`
    :code 415: Bad :header:`Content-Type` value

    **Request**:

    .. code-block:: http

        POST /db/_compact HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 202 Accepted
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Date: Mon, 12 Aug 2013 09:27:43 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "ok": true
        }

.. _api/db/compact/ddoc:

=========================
``/{db}/_compact/{ddoc}``
=========================

.. http:post:: /{db}/_compact/{ddoc}
    :synopsis: Starts a compaction for all the views in the selected
               design document

    Compacts the view indexes associated with the specified design document.
    It may be that compacting a large view can return more storage than
    compacting the actual db. Thus, you can use this in place of the full
    database compaction if you know a specific set of view indexes have been
    affected by a recent database change. See :ref:`compact/views` for details.

    :param db: Database name
    :param ddoc: Design document name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json boolean ok: Operation status
    :code 202: Compaction request has been accepted
    :code 400: Invalid database name
    :code 401: CouchDB Server Administrator privileges required
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`
    :code 404: Design document not found
    :code 415: Bad :header:`Content-Type` value

    **Request**:

    .. code-block:: http

        POST /db/_compact/ddoc HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 202 Accepted
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Date: Mon, 12 Aug 2013 09:36:44 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "ok": true
        }

    .. note::
        View indexes are stored in a separate ``.couch`` file based on a hash
        of the design document's relevant functions, in a sub directory of
        where the main ``.couch`` database files are located.

.. _api/db/ensure_full_commit:

=============================
``/{db}/_ensure_full_commit``
=============================

.. http:post:: /{db}/_ensure_full_commit
    :synopsis: Deprecated endpoint to support CouchDB versions < 3.0
               replicators.

    .. versionchanged:: 3.0.0 Deprecated; endpoint is a no-op.

    Before 3.0 this was used to commit recent changes to the database in case
    the ``delayed_commits=true`` option was set. That option is always
    ``false`` now, so commits are never delayed. However, this endpoint is kept
    for compatibility with older replicators.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json string instance_start_time: Always ``"0"``. (Returned for legacy
      reasons.)
    :>json boolean ok: Operation status
    :code 201: Commit completed successfully
    :code 400: Invalid database name
    :code 401: Unauthorized request to a protected API
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`
    :code 415: Bad :header:`Content-Type` value

    **Request**:

    .. code-block:: http

        POST /db/_ensure_full_commit HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 201 Created
        Cache-Control: must-revalidate
        Content-Length: 53
        Content-Type: application/json
        Date: Mon, 12 Aug 2013 10:22:19 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "instance_start_time": "0",
            "ok": true
        }

.. _api/db/view_cleanup:

=======================
``/{db}/_view_cleanup``
=======================

.. http:post:: /{db}/_view_cleanup
    :synopsis: Removes view files that are not used by any design document

    Removes view index files that are no longer required by CouchDB as a result
    of changed views within design documents. As the view filename is based on
    a hash of the view functions, over time old views will remain, consuming
    storage. This call cleans up the cached view output on disk for
    a given view.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json boolean ok: Operation status
    :code 202: Compaction request has been accepted
    :code 400: Invalid database name
    :code 401: CouchDB Server Administrator privileges required
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`
    :code 415: Bad :header:`Content-Type` value

    **Request**:

    .. code-block:: http

        POST /db/_view_cleanup HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 202 Accepted
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Date: Mon, 12 Aug 2013 09:27:43 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "ok": true
        }
