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

.. _api/db:

``/db``
=======

.. http:head:: /{db}

  Returns the HTTP Headers containing a minimal amount of information
  about the specified database. Since the response body is empty this method
  is a lightweight way to check is database exists or not.

  :param db: Database name
  :code 200: Database exists
  :code 404: Requested database not found

  **Request**:

  .. code-block:: http

    HEAD /test HTTP/1.1
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 01:27:41 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)


.. http:get:: /{db}

  Gets information about the specified database.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json number committed_update_seq: The number of committed update.
  :>json boolean compact_running: Set to ``true`` if the database compaction
    routine is operating on this database.
  :>json string db_name: The name of the database.
  :>json number disk_format_version: The version of the physical format used
    for the data when it is stored on disk.
  :>json number disk_size: Size in bytes of the data as stored on the disk.
    Views indexes are not included in the calculation.
  :>json number doc_count: A count of the documents in the specified database.
  :>json number doc_del_count: Number of deleted documents
  :>json string instance_start_time: Timestamp of when the database was opened,
    expressed in microseconds since the epoch.
  :>json number purge_seq: The number of purge operations on the database.
  :>json number update_seq: The current number of updates to the database.
  :code 200: Request completed successfully
  :code 404: Requested database not found

  **Request**:

  .. code-block:: http

    GET /receipts HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 258
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 01:38:57 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "committed_update_seq": 292786,
        "compact_running": false,
        "data_size": 65031503,
        "db_name": "receipts",
        "disk_format_version": 6,
        "disk_size": 137433211,
        "doc_count": 6146,
        "doc_del_count": 64637,
        "instance_start_time": "1376269325408900",
        "purge_seq": 0,
        "update_seq": 292786
    }


.. http::put /{db}

  Creates a new database. The database name ``{db}`` must be composed by
  following next rules:

  -  Name must begin with a lowercase letter (``a-z``)

  -  Lowercase characters (``a-z``)

  -  Digits (``0-9``)

  -  Any of the characters ``_``, ``$``, ``(``, ``)``, ``+``, ``-``, and
     ``/``.

  If you're familiar with `Regular Expressions`_, the rules above could be
  written as ``^[a-z][a-z0-9_$()+/-]*$``.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>header Location: Database URI location
  :>json boolean ok: Operation status. Available in case of success
  :>json string error: Error type. Available if response code is ``4xx``
  :>json string reason: Error description. Available if response code is ``4xx``
  :code 201: Database created successfully
  :code 400: Invalid database name
  :code 401: Administrator`s privileges required
  :code 412: Database already exists

  **Request**:

  .. code-block:: http

    PUT /db HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 201 Created
    Cache-Control: must-revalidate
    Content-Length: 12
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 08:01:45 GMT
    Location: http://localhost:5984/db
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "ok": true
    }

  If we repeat same request to CouchDB, it will response with :code:`412` since
  database is already exists:

  **Request**:

  .. code-block:: http

    PUT /db HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 412 Precondition Failed
    Cache-Control: must-revalidate
    Content-Length: 95
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 08:01:16 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "error": "file_exists",
        "reason": "The database could not be created, the file already exists."
    }

  In case of invalid database name CouchDB returns response with :code:`400`:

  **Request**:

  .. code-block:: http

    PUT /_db HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Request**:

  .. code-block:: http

    HTTP/1.1 400 Bad Request
    Cache-Control: must-revalidate
    Content-Length: 194
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 08:02:10 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "error": "illegal_database_name",
        "reason": "Name: '_db'. Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter."
    }


.. http:delete:: /{db}

  Deletes the specified database, and all the documents and attachments
  contained within it.

  :param db: Database name
  :<header Accept: - :mimetype:`application/json`
                   - :mimetype:`text/plain`
  :>header Content-Type: - :mimetype:`application/json`
                         - :mimetype:`text/plain; charset=utf-8`
  :>json boolean ok: Operation status
  :code 200: Database removed successfully
  :code 400: Invalid database name
  :code 401: Administrator`s privileges required
  :code 404: Database doesn't already exists

  **Request**:

  .. code-block:: http

    DELETE /db HTTP/1.1
    Accept: application/json
    Host: localhost:5984

  **Response**:

  .. code-block:: http

    HTTP/1.1 200 OK
    Cache-Control: must-revalidate
    Content-Length: 12
    Content-Type: application/json
    Date: Mon, 12 Aug 2013 08:54:00 GMT
    Server: CouchDB/1.4.0 (Erlang OTP/R16B)

    {
        "ok": true
    }


.. _Regular Expressions: http://en.wikipedia.org/wiki/Regular_expression
