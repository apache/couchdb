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

.. _api/local:

=================================
Local (non-replicating) Documents
=================================

The local (non-replicating) document interface allows you to create local
documents that are not replicated to other databases. These documents can be
used to hold configuration or other information that is required specifically
on the local CouchDB instance.

Local documents have the following limitations:

- Local documents are not replicated to other databases.

- Local documents are not output by views, or the :ref:`api/db/all_docs` view.

From CouchDB 2.0, Local documents can be listed by using the :ref:`api/db/_local_docs`
endpoint.

Local documents can be used when you want to store configuration or
other information for the current (local) instance of a given database.

A list of the available methods and URL paths are provided below:

+--------+---------------------------+--------------------------------------------+
| Method | Path                      | Description                                |
+========+===========================+============================================+
| GET,   | /{db}/_local_docs         | Returns a list of all the                  |
| POST   |                           | non-replicated documents in the database   |
+--------+---------------------------+--------------------------------------------+
| POST   | /{db}/_local_docs/queries | Returns a list of specified                |
|        |                           | non-replicated documents in the database   |
+--------+---------------------------+--------------------------------------------+
| GET    | /{db}/_local/{docid}      | Returns the latest revision of the         |
|        |                           | non-replicated document                    |
+--------+---------------------------+--------------------------------------------+
| PUT    | /{db}/_local/{docid}      | Inserts a new version of the               |
|        |                           | non-replicated document                    |
+--------+---------------------------+--------------------------------------------+
| DELETE | /{db}/_local/{docid}      | Deletes the non-replicated document        |
+--------+---------------------------+--------------------------------------------+
| COPY   | /{db}/_local/{docid}      | Copies the non-replicated document         |
+--------+---------------------------+--------------------------------------------+

.. _api/db/_local_docs:

``/{db}/_local_docs``
=====================

.. http:get:: /{db}/_local_docs
    :synopsis: Returns a built-in view of all local (non-replicating) documents
      in this database

    Returns a JSON structure of all of the local documents in a given
    database. The information is returned as a JSON structure containing meta
    information about the return structure, including a list of all local
    documents and basic contents, consisting the ID, revision and key. The key
    is the from the local document's ``_id``.

    :param db: Database name
    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`
    :query boolean conflicts: Includes `conflicts` information in response.
      Ignored if ``include_docs`` isn't ``true``. Default is ``false``.
    :query boolean descending: Return the local documents in descending by
      key order. Default is ``false``.
    :query string endkey: Stop returning records when the specified key is
      reached. *Optional*.
    :query string end_key: Alias for ``endkey`` param.
    :query string endkey_docid: Stop returning records when the specified
        local document ID is reached. *Optional*.
    :query string end_key_doc_id: Alias for ``endkey_docid`` param.
    :query boolean include_docs: Include the full content of the local
      documents in the return. Default is ``false``.
    :query boolean inclusive_end: Specifies whether the specified end key
      should be included in the result. Default is ``true``.
    :query string key: Return only local documents that match the specified
      key. *Optional*.
    :query string keys: Return only local documents that match the specified
      keys. *Optional*.
    :query number limit: Limit the number of the returned local documents to
      the specified number. *Optional*.
    :query number skip: Skip this number of records before starting to return
      the results. Default is ``0``.
    :query string startkey: Return records starting with the specified key.
      *Optional*.
    :query string start_key: Alias for ``startkey`` param.
    :query string startkey_docid: Return records starting with the specified
      local document ID. *Optional*.
    :query string start_key_doc_id: Alias for ``startkey_docid`` param.
    :query boolean update_seq: Response includes an ``update_seq`` value
      indicating which sequence id of the underlying database the view
      reflects. Default is ``false``.
    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>json number offset: Offset where the local document list started
    :>json array rows: Array of view row objects. By default the information
      returned contains only the local document ID and revision.
    :>json number total_rows: Number of local documents in the database. Note
      that this is not the number of rows returned in the actual query.
    :>json number update_seq: Current update sequence for the database

    :code 200: Request completed successfully
    :code 401: Unauthorized request to a protected API
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`

    **Request**:

    .. code-block:: http

        GET /db/_local_docs HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Sat, 23 Dec 2017 16:22:56 GMT
        Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
            "offset": null,
            "rows": [
                {
                    "id": "_local/localdoc01",
                    "key": "_local/localdoc01",
                    "value": {
                        "rev": "0-1"
                    }
                },
                {
                    "id": "_local/localdoc02",
                    "key": "_local/localdoc02",
                    "value": {
                        "rev": "0-1"
                    }
                },
                {
                    "id": "_local/localdoc03",
                    "key": "_local/localdoc03",
                    "value": {
                        "rev": "0-1"
                    }
                },
                {
                    "id": "_local/localdoc04",
                    "key": "_local/localdoc04",
                    "value": {
                        "rev": "0-1"
                    }
                },
                {
                    "id": "_local/localdoc05",
                    "key": "_local/localdoc05",
                    "value": {
                        "rev": "0-1"
                    }
                }
            ],
            "total_rows": null
        }

.. http:post:: /{db}/_local_docs
    :synopsis: Returns a built-in view of all local (non-replicating) documents
      in this database

    :method:`POST` `/{db}/_local_docs` functionality supports identical parameters and
    behavior as specified in the :get:`/{db}/_local_docs` API but allows for the query
    string parameters to be supplied as keys in a JSON object in the body of the
    `POST` request.

    :param db: Database name
    :code 401: Unauthorized request to a protected API
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`

    **Request**:

    .. code-block:: http

        POST /db/_local_docs HTTP/1.1
        Accept: application/json
        Content-Length: 70
        Content-Type: application/json
        Host: localhost:5984

        {
            "keys" : [
                "_local/localdoc02",
                "_local/localdoc05"
            ]
        }

    The returned JSON is the all documents structure, but with only the
    selected keys in the output:

    **Response**:

    .. code-block:: javascript

        {
            "total_rows" : null,
            "rows" : [
                {
                    "value" : {
                        "rev" : "0-1"
                    },
                    "id" : "_local/localdoc02",
                    "key" : "_local/localdoc02"
                },
                {
                    "value" : {
                        "rev" : "0-1"
                    },
                    "id" : "_local/localdoc05",
                    "key" : "_local/localdoc05"
                }
            ],
            "offset" : null
        }

.. _api/db/_local_docs/queries:

``/{db}/_local_docs/queries``
=============================

.. http:post:: /{db}/_local_docs/queries
    :synopsis: Returns results for the specified queries

    Querying with specified ``keys`` will return local documents only.
    You can also combine ``keys`` with other query parameters,
    such as ``limit`` and ``skip``.

    :param db: Database name

    :<header Content-Type: - :mimetype:`application/json`
    :<header Accept: - :mimetype:`application/json`

    :<json queries: An array of query objects with fields for the
        parameters of each individual view query to be executed.
        The field names and their meaning are the same as the query
        parameters of a regular
        :ref:`_local_docs request <api/db/_local_docs>`.

    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>header Transfer-Encoding: ``chunked``

    :>json array results: An array of result objects - one for each query. Each
        result object contains the same fields as the response to a regular
        :ref:`_local_docs request <api/db/_local_docs>`.

    :code 200: Request completed successfully
    :code 400: Invalid request
    :code 401: Unauthorized request to a protected API
    :code 403: Insufficient permissions / :ref:`Too many requests with invalid credentials<error/403>`
    :code 404: Specified database is missing
    :code 500: Query execution error

    **Request**:

    .. code-block:: http

        POST /db/_local_docs/queries HTTP/1.1
        Content-Type: application/json
        Accept: application/json
        Host: localhost:5984

        {
            "queries": [
                {
                    "keys": [
                        "_local/localdoc05",
                        "_local/not-exist",
                        "_design/recipe",
                        "spaghetti"
                    ]
                }
            ]
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Thu, 20 Jul 2023 21:45:37 GMT
        Server: CouchDB (Erlang/OTP)
        Transfer-Encoding: chunked

        {
            "results": [
                {
                    "total_rows": null,
                    "offset": null,
                    "rows": [
                        {
                            "id": "_local/localdoc05",
                            "key": "_local/localdoc05",
                            "value": {
                              "rev": "0-1"
                            }
                        },
                        {
                            "key": "_local/not-exist",
                            "error": "not_found"
                        }
                    ]
                },
                {
                    "total_rows": null,
                    "offset": null,
                    "rows": [
                        {
                          "id": "_local/localdoc04",
                          "key": "_local/localdoc04",
                          "value": {
                              "rev": "0-1"
                            }
                        }
                    ]
                }
            ]
        }

    .. Note::
        Similar to :ref:`_design_docs/queries <api/db/_design_docs/queries>`,
        /{db}/_local_docs/queries will only return local documents.
        The difference is ``total_rows`` and ``offset`` are always ``null``.

.. _api/db/_local/doc:

``/{db}/_local/{docid}``
========================

.. http:get:: /{db}/_local/{docid}
    :synopsis: Returns the latest revision of the local document

    Gets the specified local document. The semantics are identical to accessing
    a standard document in the specified database, except that the document is
    not replicated. See :get:`/{db}/{docid}`.

    :param db: Database name
    :param docid: Document ID

.. http:put:: /{db}/_local/{docid}
    :synopsis: Inserts a new version of the local document

    Stores the specified local document. The semantics are identical to storing
    a standard document in the specified database, except that the document is
    not replicated. See :put:`/{db}/{docid}`.

    :param db: Database name
    :param docid: Document ID

.. http:delete:: /{db}/_local/{docid}
    :synopsis: Deletes the local document

    Deletes the specified local document. The semantics are identical to
    deleting a standard document in the specified database, except that the
    document is not replicated. See :delete:`/{db}/{docid}`.

.. http:copy:: /{db}/_local/{docid}
    :synopsis: Copies the local document within the same database

    Copies the specified local document. The semantics are identical to copying
    a standard document in the specified database, except that the document is
    not replicated. See :copy:`/{db}/{docid}`.
