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

.. _api/db/shards:

===============
``/db/_shards``
===============

.. versionadded:: 2.0

.. http:get:: /{db}/_shards
    :synopsis: Displays the shard map layout of a database

    The response will contain a list of database shards. Each shard will
    have its internal database range, and the nodes on which replicas of
    those shards are stored.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json object shards: Mapping of shard ranges to individual shard replicas
                          on each node in the cluster
    :code 200: Request completed successfully
    :code 400: Invalid database name
    :code 401: Read privilege required
    :code 415: Bad :header:`Content-Type` value
    :code 500: Internal server error or timeout

    **Request**:

    .. code-block:: http

        GET /db/_shards HTTP/1.1
        Accept: */*
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 621
        Content-Type: application/json
        Date: Fri, 18 Jan 2019 19:55:14 GMT
        Server: CouchDB/2.4.0 (Erlang OTP/19)

        {
          "shards": {
            "00000000-1fffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ],
            "20000000-3fffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ],
            "40000000-5fffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ],
            "60000000-7fffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ],
            "80000000-9fffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ],
            "a0000000-bfffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ],
            "c0000000-dfffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ],
            "e0000000-ffffffff": [
              "couchdb@node1.example.com",
              "couchdb@node2.example.com",
              "couchdb@node3.example.com"
            ]
          }
        }

.. _api/db/shards/doc:

==============================
``/db/_shards/doc``
==============================

.. http:get:: /{db}/_shards/{docid}
    :synopsis: Returns the specific shard in which a document is stored

    Returns information about the specific shard into which a given document
    has been stored, along with information about the nodes on which that
    shard has a replica.

    :param db: Database name
    :param docid: Document ID
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json string range: The shard range in which the document is stored
    :>json array nodes: List of nodes serving a replica of the shard
    :code 200: Request completed successfully
    :code 401: Read privilege required
    :code 404: Database or document not found
    :code 500: Internal server error or timeout

    **Request**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 94
        Content-Type: application/json
        Date: Fri, 18 Jan 2019 20:08:07 GMT
        Server: CouchDB/2.3.0-9d4cb03c2 (Erlang OTP/19)

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 94
        Content-Type: application/json
        Date: Fri, 18 Jan 2019 20:26:33 GMT
        Server: CouchDB/2.3.0-9d4cb03c2 (Erlang OTP/19)

        {
          "range": "e0000000-ffffffff",
          "nodes": [
            "node1@127.0.0.1",
            "node2@127.0.0.1",
            "node3@127.0.0.1"
          ]
        }

.. _api/db/sync_shards:

=====================
``/db/_sync_shards``
=====================

.. versionadded:: 2.3.1

.. http:post:: /{db}/_sync_shards
    :synopsis: Trigger a synchronization of all shard replicas
               in the database

    For the given database, force-starts internal shard synchronization
    for all replicas of all database shards.

    This is typically only used when performing cluster maintenance,
    such as :ref:`moving a shard <cluster/sharding/move>`.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json boolean ok: Operation status. Available in case of success
    :>json string error: Error type. Available if response code is ``4xx``
    :>json string reason: Error description. Available if response code is
      ``4xx``
    :code 202: Request accepted
    :code 400: Invalid database name
    :code 401: CouchDB Server Administrator privileges required
    :code 404: Database not found
    :code 500: Internal server error or timeout

    **Request**:

    .. code-block:: http

        POST /db/_sync_shards HTTP/1.1
        Host: localhost:5984
        Accept: */*

    **Response**:

    .. code-block:: http

        HTTP/1.1 202 Accepted
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Date: Fri, 18 Jan 2019 20:19:23 GMT
        Server: CouchDB/2.3.0-9d4cb03c2 (Erlang OTP/19)
        X-Couch-Request-ID: 14f0b8d252
        X-CouchDB-Body-Time: 0

        {
            "ok": true
        }

.. note::

    Admins may want to bump their ``[mem3] sync_concurrency`` value to a
    larger figure for the duration of the shards sync.
