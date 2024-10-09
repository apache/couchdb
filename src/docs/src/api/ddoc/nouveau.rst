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

.. _api/ddoc/nouveau:

=========================================
``/{db}/_design/{ddoc}/_nouveau/{index}``
=========================================

.. warning::
    Nouveau is an experimental feature. Future releases might change how the endpoints
    work and might invalidate existing indexes.

.. warning::
    Nouveau endpoints require a running nouveau server.
    See :ref:`Nouveau Server Installation <install/nouveau>` for details.

.. versionadded:: 3.4.0

.. http:get:: /{db}/_design/{ddoc}/_nouveau/{index}
    :synopsis: Returns results for the specified nouveau index

    Executes a nouveau request against the named index in the specified design document.

    :param db: Database name
    :param ddoc: Design document name
    :param index: Nouveau index name

    :<header Accept: - :mimetype:`application/json`

    :query string bookmark: A bookmark received from a previous search. This parameter
        enables paging through the results. If there are no more results after the
        bookmark, you get a response with an empty rows array and the same bookmark,
        confirming the end of the result list.
    :query json counts: An array of names of string fields for which counts
        are requested. The response contains counts for each unique value of this field
        name among the documents that match the search query.
    :query boolean include_docs: Include the full content of the documents in the
        response.
    :query string locale: The (Java) locale used to parse numbers in range queries.
        Defaults to the JDK default locale if not specified. Some examples are ``de``
        , ``us``, ``gb``.
    :query number limit: Limit the number of the returned documents to the specified
        number.
    :query string q: Required. The Lucene query string.
    :query json ranges: This field defines ranges for numeric search fields. The
        value is a JSON object where the fields names are numeric search fields,
        and the values of the fields are arrays of JSON objects. The objects
        must have a ``label``, ``min`` and ``max`` value (of type string,
        number, number respectively), and optional ``min_inclusive`` and
        ``max_inclusive`` properties (defaulting to ``true`` if not specified).
        Example: ``{"bar":[{"label":"cheap","min":0,"max":100}]}``
    :query json sort: Specifies the sort order of the results.
        The default sort order is relevance. A JSON string of the form
        ``"fieldname"`` or ``"-fieldname"`` for descending order, where
        fieldname is the name of a string or double field. You can use a single string
        to sort by one field or an array of strings to sort by several fields in the
        same order as the array.
        Some examples are ``"relevance"``, ``"bar"``, ``"-foo"`` and
        [``"-foo"``, ``"bar"``].
    :query number top_n: Limit the number of facets returned by group, defaulting to 10
        with a maximum of 1000.
    :query boolean update: Set to ``false`` to allow the use of an out-of-date index.

    :>header Content-Type: - :mimetype:`application/json`

    :>header Transfer-Encoding: ``chunked``

    :>json array hits: Array of search hits. By default the information
      returned contains only the document ID and revision.
    :>json number total_hits: Number of matches for the query.
    :>json string total_hits_relation: ``EQUAL_TO`` if ``total_hits`` is exact.
        ``GREATER_THAN_OR_EQUAL_TO`` if not.
    :>json string bookmark: Opaque identifier to enable pagination.

    :code 200: Request completed successfully
    :code 400: Invalid request
    :code 401: Read permission required
    :code 404: Specified database, design document or view is missed

.. note::
    Faceting is not supported on partitioned searches, so the following
    query parameters should not be used on those requests: ``counts`` and
    ``ranges``.

.. seealso::
    For more information about how nouveau works, see the
    :ref:`Nouveau User Guide<ddoc/nouveau>`.

==============================================
``/{db}/_design/{ddoc}/_nouveau_info/{index}``
==============================================

.. warning::
    Nouveau is an experimental feature. Future releases might change how the endpoints
    work and might invalidate existing indexes.

.. warning::
    Nouveau endpoints require a running nouveau server.
    See :ref:`Nouveau Server Installation <install/nouveau>` for details.

.. versionadded:: 3.4.0

.. http:get:: /{db}/_design/{ddoc}/_nouveau_info/{index}
    :synopsis: Returns metadata for the specified nouveau index

    :param db: Database name
    :param ddoc: Design document name
    :param index: Search index name
    :code 200: Request completed successfully
    :code 400: Request body is wrong (malformed or missing one of the mandatory fields)
    :code 500: A server error (or other kind of error) occurred

**Request**:

.. code-block:: http

    GET /recipes/_design/cookbook/_nouveau_info/ingredients HTTP/1.1
    Accept: application/json
    Host: localhost:5984

**Response**:

.. code-block:: http

    HTTP/1.1 200 OK
    Content-Type: application/json

    {
        "name": "_design/cookbook/ingredients",
        "search_index": {
            "num_docs": 1000,
            "update_seq": 5000,
            "disk_size": 1048576
        }
    }
