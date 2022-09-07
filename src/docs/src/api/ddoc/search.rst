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

.. _api/ddoc/search:

=============================================
``/db/_design/design-doc/_search/index-name``
=============================================

.. warning::
    Search endpoints require a running search plugin connected to each cluster
    node. See :ref:`Search Plugin Installation <install/search>` for details.

.. versionadded:: 3.0

.. http:get:: /{db}/_design/{ddoc}/_search/{index}
    :synopsis: Returns results for the specified search index

    Executes a search request against the named index in the specified design document.

    :param db: Database name
    :param ddoc: Design document name
    :param index: Search index name

    :<header Accept: - :mimetype:`application/json`
                     - :mimetype:`text/plain`

    :query string bookmark: A bookmark received from a previous search. This parameter
        enables paging through the results. If there are no more results after the
        bookmark, you get a response with an empty rows array and the same bookmark,
        confirming the end of the result list.
    :query json counts: An array of names of string fields for which counts
        are requested. The response contains counts for each unique value of this field
        name among the documents that match the search query. :ref:`Faceting
        <ddoc/search/faceting>` must be enabled for this parameter to function.
    :query json drilldown: This field can be used several times. Each use defines a pair
        with a field name and a value. The search matches only documents containing the
        value that was provided in the named field. It differs from using
        ``"fieldname:value"`` in the ``q`` parameter only in that the values are not
        analyzed. :ref:`Faceting <ddoc/search/faceting>` must be enabled for this
        parameter to function.
    :query string group_field: Field by which to group search matches. :query number
        group_limit: Maximum group count. This field can be used only if ``group_field``
        is specified.
    :query json group_sort: This field defines the order of the groups in a search that
        uses ``group_field``. The default sort order is relevance.
    :query json highlight_fields: Specifies which fields to highlight. If specified, the
        result object contains a ``highlights`` field with an entry for each specified
        field.
    :query string highlight_pre_tag: A string that is inserted before the highlighted
        word in the highlights output.
    :query string highlight_post_tag: A string that is inserted after the highlighted
        word in the highlights output.
    :query number highlight_number: Number of fragments that are returned in highlights.
        If the search term occurs less often than the number of fragments that are
        specified, longer fragments are returned.
    :query number highlight_size: Number of characters in each fragment for highlights.
    :query boolean include_docs: Include the full content of the documents in the
        response.
    :query json include_fields: A JSON array of field names to include in search
        results. Any fields that are included must be indexed with the store:true option.
    :query number limit: Limit the number of the returned documents to the specified
        number. For a grouped search, this parameter limits the number of documents per
        group.
    :query string q: Alias for ``query``.
    :query string query: Required. The Lucene query string.
    :query json ranges: This field defines ranges for faceted, numeric search fields. The
        value is a JSON object where the fields names are faceted numeric search fields,
        and the values of the fields are JSON objects. The field names of the JSON objects
        are names for ranges. The values are strings that describe the range, for example
        "[0 TO 10]".
    :query json sort: Specifies the sort order of the results. In a grouped search (when
        ``group_field`` is used), this parameter specifies the sort order within a group.
        The default sort order is relevance. A JSON string of the form
        ``"fieldname<type>"`` or ``-fieldname<type>`` for descending order, where
        fieldname is the name of a string or number field, and ``type`` is either a
        number, a string, or a JSON array of strings. The ``type`` part is optional, and
        defaults to number. Some examples are ``"foo"``, ``"-foo"``, ``"bar<string>"``,
        ``"-foo<number>"`` and [``"-foo<number>"``, ``"bar<string>"``]. String fields that
        are used for sorting must not be analyzed fields. Fields that are used for sorting
        must be indexed by the same indexer that is used for the search query.
    :query string stale: Set to ``ok`` to allow the use of an out-of-date index.

    :>header Content-Type: - :mimetype:`application/json`
                           - :mimetype:`text/plain; charset=utf-8`
    :>header ETag: Response signature
    :>header Transfer-Encoding: ``chunked``

    :>json array rows: Array of view row objects. By default the information
      returned contains only the document ID and revision.
    :>json number total_rows: Number of documents in the database/view.
    :>json string bookmark: Opaque identifier to enable pagination.

    :code 200: Request completed successfully
    :code 400: Invalid request
    :code 401: Read permission required
    :code 404: Specified database, design document or view is missed

.. note::
    You must enable :ref:`faceting <ddoc/search/faceting>` before you can use the
    ``counts``, ``drilldown``, and ``ranges`` parameters.

.. note::
    Faceting and grouping are not supported on partitioned searches, so the following
    query parameters should not be used on those requests: ``counts``, ``drilldown``,
    ``ranges``, and ``group_field``, ``group_limit``, group_sort``.

.. note::
    Do not combine the ``bookmark`` and ``stale`` options. These options constrain the
    choice of shard replicas to use for the response. When used together, the options
    might cause problems when contact is attempted with replicas that are slow or not
    available.

.. seealso::
    For more information about how search works, see the
    :ref:`Search User Guide<ddoc/search>`.

==================================================
``/db/_design/design-doc/_search_info/index-name``
==================================================

.. warning::
    Search endpoints require a running search plugin connected to each cluster
    node. See :ref:`Search Plugin Installation <install/search>` for details.

.. versionadded:: 3.0

.. http:get:: /{db}/_design/{ddoc}/_search_info/{index}
    :synopsis: Returns metadata for the specified search index

    :param db: Database name
    :param ddoc: Design document name
    :param index: Search index name
    :code 200: Request completed successfully
    :code 400: Request body is wrong (malformed or missing one of the mandatory fields)
    :code 500: A server error (or other kind of error) occurred

**Request**:

.. code-block:: http

    GET /recipes/_design/cookbook/_search_info/ingredients HTTP/1.1
    Accept: application/json
    Host: localhost:5984

**Response**:

.. code-block:: javascript

    {
        "name": "_design/cookbook/ingredients",
        "search_index": {
            "pending_seq": 7125496,
            "doc_del_count": 129180,
            "doc_count": 1066173,
            "disk_size": 728305827,
            "committed_seq": 7125496
        }
    }
