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
    :synopsis: Returns status all the indexes in the database

    Index info objects have the same fields as the ones returned by
    :get:`/{db}/_design/{ddoc}/_info`,
    :get:`/{db}/_design/{ddoc}/_search_info/{index}` and
    :get:`/{db}/_design/{ddoc}/_nouveau_info/{index}`, with an
    ``updates_pending`` object with the index build pending bounds across all
    copies of the database. See :ref:`api/db/index_info/structure`.

    This endpoint may be used when one needs to know whether a database's
    indexes are fully built, for example before switching traffic to a replica.

    :param db: Database name
    :query string type: Filter response to the given index types. Can be one of
        these values: ``view``, ``search`` and ``nouveau``, comma separated.
        *Optional*, default: all types.
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json string name: Database name
    :>json number copies_expected: The number of shard copies of the database
        (Q*N) Compare with the ``copies`` count of each ``updates_pending``
        object.
    :>json object indexes: Index information keyed by design doc ID.
        See :ref:`api/db/index_info/structure`.
    :code 200: Request completed successfully
    :code 400: Invalid database name or ``type`` value
    :code 401: Unauthorized request to a protected API
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
            "name": "recipes",
            "copies_expected": 6,
            "indexes": {
                "_design/cookbook": {
                    "view_index": {
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
                        "waiting_commit": false,
                        "updates_pending": {
                            "minimum": 0,
                            "maximum": 0,
                            "copies": 6
                        }
                    },
                    "search_indexes": {
                        "ingredients": {
                            "committed_seq": 54,
                            "disk_size": 3960,
                            "doc_count": 50,
                            "doc_del_count": 0,
                            "pending_seq": 54,
                            "signature": "0b4ba635d5eb4fcbb2f6c9c2247460ec",
                            "updates_pending": {
                                "minimum": 0,
                                "maximum": 0,
                                "copies": 6
                            }
                        }
                    },
                    "nouveau_indexes": {
                        "ingredients": {
                            "disk_size": 6324,
                            "num_docs": 50,
                            "purge_seq": 0,
                            "signature": "ea87fe8f9517403691850f51d0a1ce3e5afaf89347204dd79430252a0591e503",
                            "update_seq": 54,
                            "updates_pending": {
                                "minimum": 0,
                                "maximum": 0,
                                "copies": 6
                            }
                        }
                    }
                },
                "_design/8c2a4caf8ea1b581ac43a062fd43a876dee1382d": {
                    "view_index": {
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
                        "waiting_commit": false,
                        "updates_pending": {
                            "minimum": 54,
                            "maximum": 54,
                            "copies": 6
                        }
                    }
                }
            }
        }

    In this example the ``cookbook`` design document has a view group, a
    search index and a nouveau index, all fully built on all six copies. The
    second design document is a Mango index which has not been built on
    any copy yet: every copy is 54 updates behind.

.. _api/db/index_info/structure:

Index Information
=================

The ``indexes`` object of the :get:`/{db}/_index_info` response has one entry
per design document with at least one index, keyed by design doc ID. Each entry
may contain:

* **view_index** (*object*): The design doc's view group, with the fields
  described in :ref:`api/ddoc/view_index_info` and ``updates_pending``. A design
  document has one view group for all of its views.

* **search_indexes** (*object*): One entry per search index of the design
  document, keyed by index name, with the fields of the ``search_index`` object
  returned by :get:`/{db}/_design/{ddoc}/_search_info/{index}` plus
  ``updates_pending``.
* **nouveau_indexes** (*object*): One entry per nouveau index of the design
  document, keyed by index name, with the fields of the ``search_index`` object
  returned by :get:`/{db}/_design/{ddoc}/_nouveau_info/{index}` plus
  ``updates_pending``.

Pending stats
--------------

Every index object contains an ``updates_pending`` object which shows the
number of pending changes for that index.

* **minimum** (*number*): Backlog of the most up to date copy of each
  range. ``0`` means at least one fully built copy of the index exists for
  every range.
* **maximum** (*number*): Backlog of the least up to date copy of each range.
  ``0`` means the index is fully built on all returned shard copies.
* **copies** (*number*): The number of shard copies with returned a response.
  Copies which are unreachable, in maintenance mode, or which did not answer
  before the request timed out are skipped. When this is much smaller than
  ``copies_expected`` the bounds are partial and ``maximum`` may underestimate
  the true pending backlog.

Errors
------

If an index cannot be inspected it will return an error for that index only.
For example is Closeau is not available search indexes will return:

.. code-block:: javascript

    "search_indexes": {
        "ingredients": {
            "error": "service unavailable",
            "reason": "Search is not available"
        }
    }

Invalid ``type`` values return a ``400 Bad Request``:

.. code-block:: javascript

    {
        "error": "query_parse_error",
        "reason": "Invalid index type: foo. Must be view, search or nouveau"
    }
