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

``/db/_compact``
================

.. http:post:: /{db}/_compact

  Request compaction of the specified database. Compaction compresses the
  disk database file by performing the following operations:

  -  Writes a new version of the database file, removing any unused
     sections from the new version during write. Because a new file is
     temporary created for this purpose, you will need twice the current
     storage space of the specified database in order for the compaction
     routine to complete.

  -  Removes old revisions of documents from the database, up to the
     per-database limit specified by the ``_revs_limit`` database
     parameter.

  Compaction can only be requested on an individual database; you cannot
  compact all the databases for a CouchDB instance. The compaction process
  runs as a background process.

  You can determine if the compaction process is operating on a database
  by obtaining the database meta information, the ``compact_running``
  value of the returned database structure will be set to true. See
  :http:get:`/{db}`.

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
  :code 401: Administrator`s privileges required
  :code 415: Bad :http:header:`Content-Type` value

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

``/db/_compact/design-doc``
===========================

.. http:post:: /{db}/_compact/{ddoc}

  Compacts the view indexes associated with the specified design document.
  You can use this in place of the full database compaction if you know a
  specific set of view indexes have been affected by a recent database
  change.

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
  :code 401: Administrator`s privileges required
  :code 404: Design document not found
  :code 415: Bad :http:header:`Content-Type` value

  **Request**:

  .. code-block:: http

    POST /db/_compact/posts HTTP/1.1
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


.. _api/db/ensure_full_commit:

``/db/_ensure_full_commit``
===========================

.. http:post:: /{db}/_ensure_full_commit

  Commits any recent changes to the specified database to disk. You should
  call this if you want to ensure that recent changes have been written.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json string instance_start_time: Timestamp of when the database was opened,
    expressed in microseconds since the epoch.
  :>json boolean ok: Operation status
  :code 201: Commit completed successfully
  :code 400: Invalid database name
  :code 415: Bad :http:header:`Content-Type` value

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
        "instance_start_time": "1376269047459338",
        "ok": true
    }


.. _api/db/view_cleanup:

``/db/_view_cleanup``
=====================

.. http:post:: /{db}/_view_cleanup

  Cleans up the cached view output on disk for a given view.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :<header Content-Type: :mimetype:`application/json`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json boolean ok: Operation status
  :code 202: Compaction request has been accepted
  :code 400: Invalid database name
  :code 401: Administrator`s privileges required
  :code 415: Bad :http:header:`Content-Type` value

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
