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

    Returns one list per index type: ``view_indexes``, ``search_indexes`` and
    ``nouveau_indexes``. Each entry identifies the index by its design
    document (``ddoc``) and, for search and nouveau indexes, its ``name``,
    like :get:`/{db}/_index` does, carries an ``ok`` flag and, under
    ``info``, the same object the per design document endpoint returns for
    that index type: the ``view_index`` object of
    :get:`/{db}/_design/{ddoc}/_info` or the ``search_index`` object of
    :get:`/{db}/_design/{ddoc}/_search_info/{index}` and
    :get:`/{db}/_design/{ddoc}/_nouveau_info/{index}`, ``updates_pending``
    included. See :ref:`api/db/index_info/structure`.

    This endpoint may be used when one needs to know whether a database's
    indexes are fully built, for example before switching traffic to a replica.

    :param db: Database name
    :query string type: Only gather indexes of the given types. Can be one of
        these values: ``view``, ``search`` and ``nouveau``, comma separated.
        The lists of the other types are returned empty. *Optional*,
        default: all types.
    :query json startkey: Only report indexes of design documents with an
        id greater than or equal to this key, a JSON string such as
        ``"_design/cookbook"``, as for :get:`/{db}/_design_docs`. *Optional*
    :query json start_key: Alias for ``startkey``. *Optional*
    :query json endkey: Only report indexes of design documents with an id
        less than or equal to this key. *Optional*
    :query json end_key: Alias for ``endkey``. *Optional*
    :query boolean inclusive_end: Whether the design document with the
        ``endkey`` id is included. Default is ``true``. *Optional*
    :query boolean descending: Select and report the design documents in
        descending order of their ids, as :get:`/{db}/_design_docs` does;
        the entries of every list then come in reverse order. Default is
        ``false``. *Optional*
    :query number limit: Report the indexes of at most this many design
        documents. Design documents without indexes count towards the limit
        too, so paging positions match :get:`/{db}/_design_docs`. A request
        may cover at most ``max_ddoc_number_for_index_info_req`` design
        documents, see `Paging`_. *Optional*
    :query number skip: Skip this many design documents before reporting.
        *Optional*
    :query json keys: Only report the indexes of the design documents with
        these ids, a JSON array of strings. :post:`/{db}/_index_info` takes
        the same array in the request body. *Optional*
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json number total_rows: Number of design documents in the database,
        as :get:`/{db}/_design_docs` reports it. Design documents are the
        unit this endpoint pages over, so this is not the number of index
        entries in the response.
    :>json number offset: The offset :get:`/{db}/_design_docs` reports for
        the same parameters, ``null`` when ``keys`` are given. It is not a
        design document index, see `Paging`_.
    :>json array view_indexes: One entry per design document with views.
        See :ref:`api/db/index_info/structure`.
    :>json array search_indexes: One entry per search index.
    :>json array nouveau_indexes: One entry per nouveau index.
    :code 200: Request completed successfully
    :code 400: Invalid database name or parameter value, or more design
        documents selected than a request may cover
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
            "total_rows": 2,
            "offset": 0,
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

    In this example the ``cookbook`` design document has a view group with
    two views, two search indexes and a nouveau index. All are fully built on
    all six copies except the ``steps`` search index, which still has a copy
    12 updates behind. The first view group belongs to a Mango index which
    has not been built on any copy yet: every copy is 54 updates behind, 324
    in total over the six copies.

.. http:post:: /{db}/_index_info
    :synopsis: Returns status of the indexes of the given design documents

    Same as :get:`/{db}/_index_info`, but the ids of the design documents to
    report are given as a ``keys`` array in the request body, like
    :post:`/{db}/_design_docs` and :post:`/_dbs_info` take theirs. The query
    parameters of the ``GET`` request apply as well.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :<header Content-Type: :mimetype:`application/json`
    :<json array keys: The ids of the design documents to report, for example
        ``["_design/cookbook"]``. Ids of missing design documents contribute
        no entries. *Required*
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json number total_rows: Number of design documents in the database, as
        for :get:`/{db}/_index_info`
    :>json number offset: ``null``, as for a :get:`/{db}/_all_docs` request
        with keys
    :>json array view_indexes: As for :get:`/{db}/_index_info`
    :>json array search_indexes: As for :get:`/{db}/_index_info`
    :>json array nouveau_indexes: As for :get:`/{db}/_index_info`
    :code 200: Request completed successfully
    :code 400: Invalid database name or parameter value, missing ``keys``, or
        more keys than a request may cover
    :code 401: Unauthorized request to a protected API
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
            "offset": null,
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

    Only the search indexes of the ``cookbook`` design document were asked
    for, so the other two lists are empty.

.. _api/db/index_info/structure:

Index Information
=================

All three lists are always present. Each has one entry per index of that type
declared by the reported design documents, sorted by design document id and
then by index name; the lists of the types not requested with ``type`` are
empty. An entry contains:

* **ddoc** (*string*): The design document id, for example
  ``_design/cookbook``.
* **name** (*string*): The index name, for search and nouveau indexes.
* **views** (*array*): The names of the views in the group, for view groups.
  A design document has one view group, and one entry here, for all of its
  views.
* **ok** (*boolean*): ``true`` if the index information could be gathered,
  ``false`` if not (see `Errors`_ below).
* **info** (*object*): The per design document info object of the index,
  unchanged: for view groups the ``view_index`` object described in
  :ref:`api/ddoc/view_index_info`, for search indexes the ``search_index``
  object of :get:`/{db}/_design/{ddoc}/_search_info/{index}` and for nouveau
  indexes the ``search_index`` object of
  :get:`/{db}/_design/{ddoc}/_nouveau_info/{index}`, each with its
  ``updates_pending`` object (see `Pending updates`_ below). Only present
  when ``ok`` is ``true``.

``ddoc`` and ``name`` are the same values :get:`/{db}/_index` reports for
Mango indexes, which are listed like any other: JSON indexes are view groups
with the ``query`` language and text indexes are search or nouveau indexes.

Paging
------

Every shard copy opens every reported index, so for databases with a large
number of design documents the ``startkey``, ``endkey``, ``limit`` and
``skip`` parameters can be used to report a slice of the design documents per
request. They behave like the parameters of :get:`/{db}/_design_docs` and
select design documents, not individual indexes: a design document within
the slice always contributes all of its indexes.

``total_rows`` is the number of design documents in the database and is the
value to page against: request ``limit=N`` and repeat with ``skip``
increased by ``N`` until ``skip`` reaches ``total_rows``. Design documents
without indexes count towards ``limit``, ``skip`` and ``total_rows`` all the
same.

Paging on the entries instead, by taking the last ``ddoc`` of a page as the
next ``startkey``, is not reliable: a design document without indexes
contributes no entry, so the last design document of a page is not always
visible in it, and a page may even be empty while more design documents
follow.

``offset`` is reported for parity with :get:`/{db}/_design_docs`, which
computes it the same way. It is not a design document index and should not
be used to compute the next ``skip``: like there, it is a position in the
database's documents, so it does not have to change between pages, and it is
``null`` when ``keys`` are given.

Listing the design documents with :get:`/{db}/_design_docs` and requesting
each page's ids with :post:`/{db}/_index_info` works as well.

A single request may cover at most ``max_ddoc_number_for_index_info_req``
design documents (``[chttpd]`` section of the configuration, default 1000),
like ``max_db_number_for_dbs_info_req`` caps :post:`/_dbs_info`. A request
which selects more of them, design documents without indexes included, is
rejected before any index is opened:

.. code-block:: javascript

    {
        "error": "bad_request",
        "reason": "too_many_design_docs"
    }

and a ``keys`` array with more ids than that with a ``too_many_keys`` reason.
Prefer paging with ``limit`` and ``startkey``, or smaller ``keys`` arrays, to
raising the cap: every reported design document has all of its indexes
opened on every shard copy.

Pending updates
---------------

The ``info`` object of every ``ok`` entry contains the ``updates_pending``
object of the corresponding per design document endpoint, computed the same
way from every copy of every shard range, so the numbers are the ones a
request to that endpoint would return at the same moment. Its fields are
described in :ref:`api/ddoc/view_index_info`. In short:

* **minimum** is ``0`` when at least one fully built copy of the index exists
  for every range.
* **maximum** is ``0`` when the index is fully built on every copy which
  reported, so queries see a built index no matter which copies serve them.
* **preferred** and **total** are the backlogs of the copies queries are
  routed to by default and of all copies added up.
* **copies** smaller than **copies_expected** means some copies were
  unreachable, in maintenance mode, or did not answer before the request
  timed out. The numbers are then partial and ``maximum`` may understate the
  true backlog.

Errors
------

If an index cannot be inspected on some copy its entry has ``ok`` set to
``false`` and ``error`` and ``reason`` fields instead of ``info``, the same
way :post:`/{db}/_bulk_docs` reports per document errors. For example if
Clouseau is not available every search index entry will be:

.. code-block:: javascript

    {
        "ddoc": "_design/cookbook",
        "name": "ingredients",
        "ok": false,
        "error": "service unavailable",
        "reason": "Search is not available"
    }

An index for which some shard range has no reporting copy at all, for example
because every copy of that range is unreachable or timed out, is reported the
same way with an ``unavailable`` error, so every declared index always appears
in the response and the ``info`` object always covers the whole database.

Invalid ``type`` values return a ``400 Bad Request``:

.. code-block:: javascript

    {
        "error": "query_parse_error",
        "reason": "Invalid index type: foo. Must be view, search or nouveau"
    }
