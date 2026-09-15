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

.. _api/db/index_info:

=====================
``/{db}/_index_info``
=====================

.. versionadded:: 3.6

.. http:get:: /{db}/_index_info
    :synopsis: Get the status of all the indexes in the database

    Return index status grouped by type: ``view_indexes``, ``search_indexes``
    and ``nouveau_indexes``. Each entry identifies the index by its design
    document (``ddoc``) and, for search and nouveau indexes, its ``name``, like
    :get:`/{db}/_index`. In the ``info`` field return the same object returned
    as we get from the individual design doc ``_info`` endpoints:
    :get:`/{db}/_design/{ddoc}/_info` or the ``search_index``,
    :get:`/{db}/_design/{ddoc}/_search_info/{index}` and
    :get:`/{db}/_design/{ddoc}/_nouveau_info/{index}`. See
    :ref:`api/db/index_info/structure`.

    This endpoint may be used when one needs to know whether a database's
    indexes are fully built, for example before switching traffic to a replica.

    :param db: Database name
    :query string type: Filter by type. Can be one of these values:
        ``view``, ``search`` and ``nouveau``, or several of them comma separated.
        *Optional*, default: all types.
    :query json startkey: Return indexes of design documents with a doc ID
        greater than or equal to this key. Same as for
        :get:`/{db}/_design_docs`. To get the next page, pass the ``bookmark``
        of the previous response as the ``startkey``. see `Paging`_. *Optional*
    :query json start_key: Alias for ``startkey``. *Optional*
    :query json endkey: Return indexes of design documents with a doc ID less
        than or equal to this key. *Optional*
    :query json end_key: Alias for ``endkey``. *Optional*
    :query boolean inclusive_end: Whether the design document with the
        ``endkey`` doc ID is included. Default is ``true``. *Optional*
    :query boolean descending: Traverse design documents in descending order,
        as :get:`/{db}/_design_docs` does. Default is ``false``. *Optional*
    :query number limit: Return indexes of at most this many design documents.
        Design documents without indexes count towards the limit too. A request
        may traverse at most ``max_ddoc_number_for_index_info_req`` design
        documents, see `Paging`_. *Optional*
    :query number skip: How many design documents to skip before starting to
                        return results. For paging it's best to use
                        ``startkey`` with the ``bookmark`` of the previous
                        response, see `Paging`_. *Optional*
    :query json keys: Return indexes for the design documents with these doc
        IDs only. Should be a JSON array of strings. :post:`/{db}/_index_info`
        can take the same ID list in the request body. *Optional*
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json number total_rows: Total number of design docs. Same as returned in
                              :get:`/{db}/_design_docs`. Can be used for paging
                              to see how many design document to expect
                              overall. Note: a design document may have more
                              than on index defined.
    :>json string bookmark: The doc ID of the first design document after the
        ones from the result. Use it as the the ``startkey`` of the next page
        to paginate. It will be ``null`` when there are no more design
        documents, or when ``keys`` are used. See `Paging`_.
    :>json array view_indexes: One entry per design doc for views.
        See :ref:`api/db/index_info/structure`.
    :>json array search_indexes: One entry per search index.
    :>json array nouveau_indexes: One entry per nouveau index.
    :code 200: Request was successful
    :code 400: Invalid db name or parameter. Or, total design document selected
               is over the ``max_ddoc_number_for_index_info_req`` limit.
    :code 401: Unauthorized request
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`
    :code 404: Database doesn't exist

    **Request**:

    .. code-block:: http

        GET /recipes/_index_info HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Mon, 01 Sep 2025 15:42:11 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "total_rows": 2,
            "bookmark": null,
            "view_indexes": [
                {
                    "ddoc": "_design/8c2a4caf8ea1b581ac43a062fd43a876dee1382d",
                    "views": [
                        "by-rating"
                    ],
                    "ok": true,
                    "info": {
                        "updates_pending": {
                            "minimum": 54,
                            "preferred": 54,
                            "total": 324,
                            "maximum": 54,
                            "copies": 6,
                            "copies_expected": 6
                        },
                        "collator_versions": [
                            "153.136"
                        ],
                        "compact_running": false,
                        "language": "query",
                        "purge_seq": 0,
                        "signature": "b77547252cb8b19ff12831973b576c0f",
                        "sizes": {
                            "active": 0,
                            "external": 10,
                            "file": 102
                        },
                        "update_seq": 0,
                        "updater_running": false,
                        "waiting_clients": 0,
                        "waiting_commit": false
                    }
                },
                {
                    "ddoc": "_design/cookbook",
                    "views": [
                        "by_ingredient",
                        "by_title"
                    ],
                    "ok": true,
                    "info": {
                        "updates_pending": {
                            "minimum": 0,
                            "preferred": 0,
                            "total": 0,
                            "maximum": 0,
                            "copies": 6,
                            "copies_expected": 6
                        },
                        "collator_versions": [
                            "153.136"
                        ],
                        "compact_running": false,
                        "language": "javascript",
                        "purge_seq": 0,
                        "signature": "a6d97b0199e54a1eb56e4becb1322587",
                        "sizes": {
                            "active": 1825,
                            "external": 1355,
                            "file": 16750
                        },
                        "update_seq": 54,
                        "updater_running": false,
                        "waiting_clients": 0,
                        "waiting_commit": false
                    }
                }
            ],
            "search_indexes": [
                {
                    "ddoc": "_design/cookbook",
                    "name": "ingredients",
                    "ok": true,
                    "info": {
                        "updates_pending": {
                            "minimum": 0,
                            "preferred": 0,
                            "total": 0,
                            "maximum": 0,
                            "copies": 6,
                            "copies_expected": 6
                        },
                        "committed_seq": 54,
                        "disk_size": 3960,
                        "doc_count": 50,
                        "doc_del_count": 0,
                        "pending_seq": 54,
                        "signature": "0b4ba635d5eb4fcbb2f6c9c2247460ec"
                    }
                },
                {
                    "ddoc": "_design/cookbook",
                    "name": "steps",
                    "ok": true,
                    "info": {
                        "updates_pending": {
                            "minimum": 0,
                            "preferred": 0,
                            "total": 12,
                            "maximum": 12,
                            "copies": 6,
                            "copies_expected": 6
                        },
                        "committed_seq": 42,
                        "disk_size": 2210,
                        "doc_count": 38,
                        "doc_del_count": 0,
                        "pending_seq": 42,
                        "signature": "efc82e779a1f10ec90b2fbf0bc5246dd"
                    }
                }
            ],
            "nouveau_indexes": [
                {
                    "ddoc": "_design/cookbook",
                    "name": "ingredients",
                    "ok": true,
                    "info": {
                        "updates_pending": {
                            "minimum": 0,
                            "preferred": 0,
                            "total": 0,
                            "maximum": 0,
                            "copies": 6,
                            "copies_expected": 6
                        },
                        "disk_size": 6324,
                        "num_docs": 50,
                        "purge_seq": 0,
                        "signature": "ea87fe8f9517403691850f51d0a1ce3e5afaf89347204dd79430252a0591e503",
                        "update_seq": 54
                    }
                }
            ]
        }

    Here the ``cookbook`` design doc has two views, two search indexes and a
    nouveau index. All are built on all 6 copies except the ``steps`` search
    index, which lags 12 updates behind. The first index is a query (Mango)
    which wasn't built yet, so all copies are behind.

.. http:post:: /{db}/_index_info
    :synopsis: Get the status of indexes from the selected design doc set.

    Same as :get:`/{db}/_index_info`, with the parameters given in the body,
    like :post:`/{db}/_design_docs`. A ``keys`` array selects the design docs
    to report by ID, like :post:`/_dbs_info`.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :<json array keys: Doc IDs of the design docs to report, for example
                       ``["_design/cookbook"]``. Other :get:`/{db}/_index_info`
                       parameters can provided in the body as well. *Optional*
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json number total_rows: Number of design docs in the database. Same as
                              for :get:`/{db}/_index_info`
    :>json string bookmark: Same as for :get:`/{db}/_index_info`. ``null``
                            for a ``keys`` request, which has no next page
    :>json array view_indexes: Same as :get:`/{db}/_index_info`
    :>json array search_indexes: Same as :get:`/{db}/_index_info`
    :>json array nouveau_indexes: Same as :get:`/{db}/_index_info`
    :code 200: Request was successful
    :code 400: Invalid db name or parameter, or more keys than the
        ``max_ddoc_number_for_index_info_req`` limit.
    :code 401: Unauthorized request
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`
    :code 404: Database doesn't exist

    **Request**:

    .. code-block:: http

        POST /recipes/_index_info?type=search HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Host: localhost:5984

        {
            "keys": [
                "_design/cookbook"
            ]
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Mon, 01 Sep 2025 15:42:11 GMT
        Server: CouchDB (Erlang/OTP)

        {
            "total_rows": 2,
            "bookmark": null,
            "view_indexes": [],
            "search_indexes": [
                {
                    "ddoc": "_design/cookbook",
                    "name": "ingredients",
                    "ok": true,
                    "info": {
                        "updates_pending": {
                            "minimum": 0,
                            "preferred": 0,
                            "total": 0,
                            "maximum": 0,
                            "copies": 6,
                            "copies_expected": 6
                        },
                        "committed_seq": 54,
                        "disk_size": 3960,
                        "doc_count": 50,
                        "doc_del_count": 0,
                        "pending_seq": 54,
                        "signature": "0b4ba635d5eb4fcbb2f6c9c2247460ec"
                    }
                },
                {
                    "ddoc": "_design/cookbook",
                    "name": "steps",
                    "ok": true,
                    "info": {
                        "updates_pending": {
                            "minimum": 0,
                            "preferred": 0,
                            "total": 12,
                            "maximum": 12,
                            "copies": 6,
                            "copies_expected": 6
                        },
                        "committed_seq": 42,
                        "disk_size": 2210,
                        "doc_count": 38,
                        "doc_del_count": 0,
                        "pending_seq": 42,
                        "signature": "efc82e779a1f10ec90b2fbf0bc5246dd"
                    }
                }
            ],
            "nouveau_indexes": []
        }

    We filter to return only ``search`` type indexes.

.. _api/db/index_info/structure:

Index Information
=================

All three types of index lists are returned. Each one may be empty if not
indexes are found or they are filtered based on the ``type`` parameter. Each
element of the list will contain:

* **ddoc** (*string*): The design doc ID.
* **name** (*string*): Index name for search and nouveau indexes.
* **views** (*array*): A list of map-reduce views for a view group
* **ok** (*boolean*): ``true`` if we successfully fetched info about that index
  and ``false`` if we couldn't (see `Errors`_ below).
* **info** (*object*): Info object of the index. This is the same as the
  returned by individual index ``_info`` endpoints:
  :ref:`api/ddoc/view_index_info`,
  :get:`/{db}/_design/{ddoc}/_search_info/{index}` and
  :get:`/{db}/_design/{ddoc}/_nouveau_info/{index}`.

Paging
------

The same traversal parameters as for :get:`/{db}/_design_docs` can be used.
Paging is per design document, not per index. ``total_rows`` is the number of
design documents in the database including design documents which may not have
any indexes.

To page, request ``limit=N`` and pass the ``bookmark`` of each response as the
``startkey`` of the next request, until it is ``null``.

.. code-block:: text

    GET /recipes/_index_info?limit=100

    {
        "total_rows": 250,
        "bookmark": "_design/d0100",
        "view_indexes": [ ... ],
        "search_indexes": [ ... ],
        "nouveau_indexes": [ ... ]
    }

    GET /recipes/_index_info?limit=100&startkey="_design/d0100"

A single request can process at most ``[chttpd]
max_ddoc_number_for_index_info_req`` design docs. This is similar to
``max_db_number_for_dbs_info_req`` for :post:`/_dbs_info`. If a request
selects more than that, the request will fail with an error:

.. code-block:: javascript

    {
        "error": "bad_request",
        "reason": "too_many_design_docs"
    }

Pending updates
---------------

The ``info`` object of every entry with ``ok`` set to ``true`` has an ``updates_pending``
object. Its fields are described in :ref:`api/ddoc/view_index_info`. In summary:

* **minimum** will be ``0`` when at least one complete copy of the index exists
  for every range.
* **maximum** will be ``0`` when all shard copies are completely built.
* **preferred** and **total** are the backlogs of the preferred shard copies and of
  all shard copies added together.
* **copies** how many shard copies returned results. This may be smaller than
  **copies_expected** for example if some nodes are not reachable or in
  maintenance mode

Errors
------

If index info could not be obtained for some then the index entry will have
``ok`` set to ``false``. And the index entry will have ``error`` and ``reason``
fields instead of ``info``. This is the same as how :post:`/{db}/_bulk_docs`
reports per document errors. For example if clouseau (search indexing system)
is down:

.. code-block:: javascript

    {
        "ddoc": "_design/cookbook",
        "name": "ingredients",
        "ok": false,
        "error": "service unavailable",
        "reason": "Search is not available"
    }

Invalid ``type`` values return a ``400 Bad Request``:

.. code-block:: javascript

    {
        "error": "query_parse_error",
        "reason": "Invalid type: foo. Must be view, search or nouveau"
    }
