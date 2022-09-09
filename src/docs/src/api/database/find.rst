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

.. _api/db/_find:

================
``/db/_find``
================

.. http:post:: /{db}/_find
    :synopsis: Find documents within a given database

    Find documents using a declarative JSON querying syntax.
    Queries can use the built-in :ref:`_all_docs <api/db/all_docs>` index or
    custom indexes, specified using the :ref:`_index <api/db/find/index>`
    endpoint.

    :param db: Database name

    :<header Content-Type: - :mimetype:`application/json`

    :<json json selector: JSON object describing criteria used to select
        documents. More information provided in the section on :ref:`selector
        syntax <find/selectors>`. *Required*
    :<json number limit: Maximum number of results returned. Default is ``25``.
        *Optional*
    :<json number skip: Skip the first 'n' results, where 'n' is the value
        specified. *Optional*
    :<json json sort: JSON array following :ref:`sort syntax <find/sort>`.
        *Optional*
    :<json array fields: JSON array specifying which fields of each object
        should be returned. If it is omitted, the entire object is returned.
        More information provided in the section on :ref:`filtering fields
        <find/filter>`. *Optional*
    :<json string|array use_index: Instruct a query to use a specific index.
        Specified either as ``"<design_document>"`` or
        ``["<design_document>", "<index_name>"]``. *Optional*
    :<json boolean conflicts: Include conflicted documents if ``true``.
        Intended use is to easily find conflicted documents, without an
        index or view. Default is ``false``. *Optional*
    :<json number r: Read quorum needed for the result. This defaults to 1, in
        which case the document found in the index is returned. If set to a
        higher value, each document is read from at least that many replicas
        before it is returned in the results. This is likely to take more time
        than using only the document stored locally with the index. *Optional,
        default: 1*
    :<json string bookmark: A string that enables you to specify which page of
        results you require. Used for paging through result sets. Every query
        returns an opaque string under the ``bookmark`` key that can then be
        passed back in a query to get the next page of results. If any part of
        the selector query changes between requests, the results
        are undefined. *Optional, default: null*
    :<json boolean update: Whether to update the index prior to returning the
        result. Default is ``true``. *Optional*
    :<json boolean stable: Whether or not the view results should be returned
        from a "stable" set of shards. *Optional*
    :<json string stale: Combination of ``update=false`` and ``stable=true``
        options. Possible options: ``"ok"``, ``false`` (default). *Optional*
        Note that this parameter is deprecated. Use ``stable`` and ``update`` instead.
        See :ref:`views/generation` for more details.
    :<json boolean execution_stats: Include
        :ref:`execution statistics <find/statistics>` in the query response.
        *Optional, default:* ``false``

    :>header Content-Type: :mimetype:`application/json`
    :>header Transfer-Encoding: ``chunked``

    :>json object docs: Array of documents matching the search. In each matching
        document, the fields specified in the ``fields`` part of the request
        body are listed, along with their values.
    :>json string warning: Execution warnings
    :>json object execution_stats: Execution statistics
    :>json string bookmark: An opaque string used for paging. See the
        ``bookmark`` field in the request (above) for usage details.

    :code 200: Request completed successfully
    :code 400: Invalid request
    :code 401: Read permission required
    :code 404: Requested database not found
    :code 500: Query execution error

The ``limit`` and ``skip`` values are exactly as you would expect. While
``skip`` exists, it is not intended to be used for paging. The reason is that
the ``bookmark`` feature is more efficient.

    **Request**:

Example request body for finding documents using an index:

    .. code-block:: http

        POST /movies/_find HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Content-Length: 168
        Host: localhost:5984

        {
            "selector": {
                "year": {"$gt": 2010}
            },
            "fields": ["_id", "_rev", "year", "title"],
            "sort": [{"year": "asc"}],
            "limit": 2,
            "skip": 0,
            "execution_stats": true
        }

    **Response**:

Example response when finding documents using an index:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Thu, 01 Sep 2016 15:41:53 GMT
        Server: CouchDB (Erlang OTP)
        Transfer-Encoding: chunked

        {
            "docs": [
                {
                    "_id": "176694",
                    "_rev": "1-54f8e950cc338d2385d9b0cda2fd918e",
                    "year": 2011,
                    "title": "The Tragedy of Man"
                },
                {
                    "_id": "780504",
                    "_rev": "1-5f14bab1a1e9ac3ebdf85905f47fb084",
                    "year": 2011,
                    "title": "Drive"
                }
            ],
            "execution_stats": {
                "total_keys_examined": 0,
                "total_docs_examined": 200,
                "total_quorum_docs_examined": 0,
                "results_returned": 2,
                "execution_time_ms": 5.52
            }
        }

.. _find/selectors:

Selector Syntax
===============

Selectors are expressed as a JSON object describing documents of interest.
Within this structure, you can apply conditional logic using specially named
fields.

Whilst selectors have some similarities with MongoDB query documents, these
arise from a similarity of purpose and do not necessarily extend to commonality
of function or result.

.. _find/selectorbasics:

Selector Basics
---------------

Elementary selector syntax requires you to specify one or more fields, and the
corresponding values required for those fields. This selector matches all
documents whose "director" field has the value "Lars von Trier".

.. code-block:: javascript

    {
        "director": "Lars von Trier"
    }

    A simple selector, inspecting specific fields

.. code-block:: javascript

    "selector": {
      "title": "Live And Let Die"
    },
    "fields": [
      "title",
      "cast"
    ]

You can create more complex selector expressions by combining operators.
For best performance, it is best to combine 'combination' or
'array logical' operators, such as ``$regex``, with an equality
operators such as ``$eq``, ``$gt``, ``$gte``, ``$lt``, and ``$lte``
(but not ``$ne``). For more information about creating complex
selector expressions, see :ref:`creating selector expressions
<find/expressions>`.

.. _find/twofields:

Selector with 2 fields
----------------------

This selector matches any document with a name field containing ``"Paul"``,
and that also has a location field with the value ``"Boston"``.

.. code-block:: javascript

    {
        "name": "Paul",
        "location": "Boston"
    }

.. _find/subfields:

Subfields
---------

A more complex selector enables you to specify the values for field of nested
objects, or subfields. For example, you might use a standard JSON structure for
specifying a field and subfield.

Example of a field and subfield selector, using a standard JSON structure:

.. code-block:: javascript

    {
        "imdb": {
            "rating": 8
        }
    }

An abbreviated equivalent uses a dot notation to combine the field and subfield
names into a single name.

.. code-block:: javascript

    {
        "imdb.rating": 8
    }

.. _find/operators:

Operators
---------

Operators are identified by the use of a dollar sign ($) prefix in the name
field.

There are two core types of operators in the selector syntax:

-  Combination operators
-  Condition operators

In general, combination operators are applied at the topmost level of selection.
They are used to combine conditions, or to create combinations of conditions,
into one selector.

Every explicit operator has the form:

.. code-block:: javascript

    {"$operator": argument}

A selector without an explicit operator is considered to have an implicit
operator. The exact implicit operator is determined by the structure of the
selector expression.

.. _find/implicit_operators:

Implicit Operators
------------------

There are two implicit operators:

-  Equality
-  And

In a selector, any field containing a JSON value, but that has no operators in
it, is considered to be an equality condition. The implicit equality test
applies also for fields and subfields.

Any JSON object that is not the argument to a condition operator is an implicit
$and operator on each field.

In the below example, we use an operator to match any document, where the
``"year"`` field has a value greater than ``2010``:

.. code-block:: javascript

    {
        "year": {
            "$gt": 2010
        }
    }

In this next example, there must be a field ``"director"`` in a matching
document, and the field must have a value exactly equal to ``"Lars von Trier"``.

.. code-block:: javascript

    {
        "director": "Lars von Trier"
    }

You can also make the equality operator explicit.

.. code-block:: javascript

    {
        "director": {
            "$eq": "Lars von Trier"
        }
    }

In the next example using subfields, the required field ``"imdb"`` in a matching
document must also have a subfield ``"rating"`` and the subfield must have a
value equal to ``8``.

Example of implicit operator applied to a subfield test

.. code-block:: javascript

        {
            "imdb": {
                "rating": 8
            }
        }

Again, you can make the equality operator explicit.

.. code-block:: javascript

    {
        "imdb": {
            "rating": { "$eq": 8 }
        }
    }

An example of the ``$eq`` operator used with full text indexing

.. code-block:: javascript

    {
      "selector": {
        "year": {
          "$eq": 2001
        }
      },
      "sort": [
        "title:string"
      ],
      "fields": [
        "title"
      ]
    }

An example of  the ``$eq`` operator used with database indexed on the field ``"year"``

.. code-block:: javascript

    {
      "selector": {
        "year": {
          "$eq": 2001
        }
      },
      "sort": [
        "year"
      ],
      "fields": [
        "year"
      ]
    }

In this example, the field ``"director"`` must be present and contain the value
``"Lars von Trier"`` and the field ``"year"`` must exist and have the value
``2003``.

.. code-block:: javascript

    {
        "director": "Lars von Trier",
        "year": 2003
    }

You can make both the ``$and`` operator and the equality operator explicit.

    Example of using explicit ``$and`` and ``$eq`` operators

.. code-block:: javascript

    {
        "$and": [
            {
                "director": {
                    "$eq": "Lars von Trier"
                }
            },
            {
                "year": {
                    "$eq": 2003
                }
            }
        ]
    }

.. _find/explicit_operators:

Explicit Operators
------------------

All operators, apart from 'Equality' and 'And', must be stated explicitly.

.. _find/combination_operators:

Combination Operators
---------------------

Combination operators are used to combine selectors. In addition to the common
boolean operators found in most programming languages, there are three
combination operators (``$all``, ``$elemMatch``, and ``$allMatch``) that help
you work with JSON arrays and one that works with JSON maps (``$keyMapMatch``).

A combination operator takes a single argument. The argument is either another
selector, or an array of selectors.

The list of combination operators:

+------------------+----------+--------------------------------------------------+
| Operator         | Argument | Purpose                                          |
+==================+==========+==================================================+
| ``$and``         | Array    | Matches if all the selectors in the array match. |
+------------------+----------+--------------------------------------------------+
| ``$or``          | Array    | Matches if any of the selectors in the array     |
|                  |          | match. All selectors must use the same index.    |
+------------------+----------+--------------------------------------------------+
| ``$not``         | Selector | Matches if the given selector does not match.    |
+------------------+----------+--------------------------------------------------+
| ``$nor``         | Array    | Matches if none of the selectors in the array    |
|                  |          | match.                                           |
+------------------+----------+--------------------------------------------------+
| ``$all``         | Array    | Matches an array value if it contains all the    |
|                  |          | elements of the argument array.                  |
+------------------+----------+--------------------------------------------------+
| ``$elemMatch``   | Selector | Matches and returns all documents that contain an|
|                  |          | array field with at least one element that       |
|                  |          | matches all the specified query criteria.        |
+------------------+----------+--------------------------------------------------+
| ``$allMatch``    | Selector | Matches and returns all documents that contain an|
|                  |          | array field with all its elements matching all   |
|                  |          | the specified query criteria.                    |
+------------------+----------+--------------------------------------------------+
| ``$keyMapMatch`` | Selector | Matches and returns all documents that contain a |
|                  |          | map that contains at least one key that matches  |
|                  |          | all the specified query criteria.                |
+------------------+----------+--------------------------------------------------+

.. _find/and:

**The** ``$and`` **operator**
    ``$and`` operator used with two fields

.. code-block:: javascript

    {
      "selector": {
        "$and": [
          {
            "title": "Total Recall"
          },
          {
            "year": {
              "$in": [1984, 1991]
            }
          }
        ]
      },
      "fields": [
        "year",
        "title",
        "cast"
      ]
    }

The ``$and`` operator matches if all the selectors in the array match. Below is
an example using the primary index (``_all_docs``):

.. code-block:: javascript

    {
        "$and": [
            {
                "_id": { "$gt": null }
            },
            {
                "year": {
                    "$in": [2014, 2015]
                }
            }
        ]
    }

.. _find/or:

**The** ``$or`` **operator**

The ``$or`` operator matches if any of the selectors in the array match. Below
is an example used with an index on the field ``"year"``:

.. code-block:: javascript

    {
        "year": 1977,
        "$or": [
            { "director": "George Lucas" },
            { "director": "Steven Spielberg" }
        ]
    }

.. _find/not:

**The** ``$not`` **operator**

The ``$not`` operator matches if the given selector does not match. Below is an
example used with an index on the field ``"year"``:

.. code-block:: javascript

    {
        "year": {
            "$gte": 1900
        },
        "year": {
            "$lte": 1903
        },
        "$not": {
            "year": 1901
        }
    }

.. _find/nor:

**The** ``$nor`` **operator**

The ``$nor`` operator matches if the given selector does not match. Below is an
example used with an index on the field ``"year"``:

.. code-block:: javascript

    {
        "year": {
            "$gte": 1900
        },
        "year": {
            "$lte": 1910
        },
        "$nor": [
            { "year": 1901 },
            { "year": 1905 },
            {  "year": 1907 }
        ]
    }

.. _find/all:

**The** ``$all`` **operator**

The ``$all`` operator matches an array value if it contains all the elements of
the argument array. Below is an example used with the primary index
(``_all_docs``):

.. code-block:: javascript

    {
        "_id": {
            "$gt": null
        },
        "genre": {
            "$all": ["Comedy","Short"]
        }
    }

.. _find/elemmatch:

**The** ``$elemMatch`` **operator**

The ``$elemMatch`` operator matches and returns all documents that contain an
array field with at least one element matching the supplied query criteria.
Below is an example used with the primary index (``_all_docs``):

.. code-block:: javascript

    {
        "_id": { "$gt": null },
        "genre": {
            "$elemMatch": {
                "$eq": "Horror"
            }
        }
    }

.. _find/allmatch:

**The** ``$allMatch`` **operator**

The ``$allMatch`` operator matches and returns all documents that contain an
array field with all its elements matching the supplied query criteria. Below
is an example used with the primary index (``_all_docs``):

.. code-block:: javascript

    {
        "_id": { "$gt": null },
        "genre": {
            "$allMatch": {
                "$eq": "Horror"
            }
        }
    }

.. _find/keymapmatch:

**The** ``$keyMapMatch`` **operator**

The ``$keyMapMatch`` operator matches and returns all documents that contain a
map that contains at least one key that matches all the specified query criteria.
Below is an example used with the primary index (``_all_docs``):

.. code-block:: javascript

    {
        "_id": { "$gt": null },
        "cameras": {
            "$keyMapMatch": {
                "$eq": "secondary"
            }
        }
    }

.. _find/condition-operators:

Condition Operators
-------------------

Condition operators are specific to a field, and are used to evaluate the value
stored in that field. For instance, the basic ``$eq`` operator matches when the
specified field contains a value that is equal to the supplied argument.

.. note::
    For a condition operator to function correctly, the field **must exist**
    in the document for the selector to match. As an example, ``$ne`` means
    the specified field must exist, and is not equal to the value of the
    argument.

The basic equality and inequality operators common to most programming
languages are supported. Strict type matching is used.

In addition, some 'meta' condition operators are available. Some condition
operators accept any valid JSON content as the argument.  Other condition
operators require the argument to be in a specific JSON format.

+---------------+-------------+------------+-----------------------------------+
| Operator type | Operator    | Argument   | Purpose                           |
+===============+=============+============+===================================+
| (In)equality  | ``$lt``     | Any JSON   | The field is less than the        |
|               |             |            | argument                          |
+---------------+-------------+------------+-----------------------------------+
|               | ``$lte``    | Any JSON   | The field is less than or equal to|
|               |             |            | the argument.                     |
+---------------+-------------+------------+-----------------------------------+
|               | ``$eq``     | Any JSON   | The field is equal to the argument|
+---------------+-------------+------------+-----------------------------------+
|               | ``$ne``     | Any JSON   | The field is not equal to the     |
|               |             |            | argument.                         |
+---------------+-------------+------------+-----------------------------------+
|               | ``$gte``    | Any JSON   | The field is greater than or equal|
|               |             |            | to the argument.                  |
+---------------+-------------+------------+-----------------------------------+
|               | ``$gt``     | Any JSON   | The field is greater than the     |
|               |             |            | to the argument.                  |
+---------------+-------------+------------+-----------------------------------+
| Object        | ``$exists`` | Boolean    | Check whether the field exists or |
|               |             |            | not, regardless of its value.     |
+---------------+-------------+------------+-----------------------------------+
|               | ``$type``   | String     | Check the document field's type.  |
|               |             |            | Valid values are ``"null"``,      |
|               |             |            | ``"boolean"``, ``"number"``,      |
|               |             |            | ``"string"``, ``"array"``, and    |
|               |             |            | ``"object"``.                     |
+---------------+-------------+------------+-----------------------------------+
| Array         | ``$in``     | Array of   | The document field must exist in  |
|               |             | JSON values| the list provided.                |
+---------------+-------------+------------+-----------------------------------+
|               | ``$nin``    | Array of   | The document field not must exist |
|               |             | JSON values| in the list provided.             |
+---------------+-------------+------------+-----------------------------------+
|               | ``$size``   | Integer    | Special condition to match the    |
|               |             |            | length of an array field in a     |
|               |             |            | document. Non-array fields cannot |
|               |             |            | match this condition.             |
+---------------+-------------+------------+-----------------------------------+
| Miscellaneous | ``$mod``    | [Divisor,  | Divisor and Remainder are both    |
|               |             | Remainder] | positive or negative integers.    |
|               |             |            | Non-integer values result in a    |
|               |             |            | 404. Matches documents where      |
|               |             |            | ``field % Divisor == Remainder``  |
|               |             |            | is true, and only when the        |
|               |             |            | document field is an integer.     |
+---------------+-------------+------------+-----------------------------------+
|               | ``$regex``  | String     | A regular expression pattern to   |
|               |             |            | match against the document field. |
|               |             |            | Only matches when the field is a  |
|               |             |            | string value and matches the      |
|               |             |            | supplied regular expression. The  |
|               |             |            | matching algorithms are based on  |
|               |             |            | the Perl Compatible Regular       |
|               |             |            | Expression (PCRE) library. For    |
|               |             |            | more information about what is    |
|               |             |            | implemented, see the see the      |
|               |             |            | `Erlang Regular Expression        |
|               |             |            | <http://erlang.org/doc            |
|               |             |            | /man/re.html>`_                   |
+---------------+-------------+------------+-----------------------------------+

.. warning::
    Regular expressions do not work with indexes, so they should not be used to
    filter large data sets. They can, however, be used to restrict a
    :ref:`partial index <find/partial_indexes>`.

.. _find/expressions:

Creating Selector Expressions
-----------------------------

We have seen examples of combining selector expressions, such as :ref:`using
explicit $and and $eq operators <find/combination_operators>`.

In general, whenever you have an operator that takes an argument, that argument
can itself be another operator with arguments of its own. This enables us to
build up more complex selector expressions.

However, only equality operators such as ``$eq``, ``$gt``, ``$gte``, ``$lt``,
and ``$lte`` (but not ``$ne``) can be used as the basis of a query. You should
include at least one of these in a selector.

For example, if you try to perform a query that attempts to match all documents
that have a field called `afieldname` containing a value that begins with the
letter `A`, this will trigger a warning because no index could be used and
the database performs a full scan of the primary index:

    **Request**

    .. code-block:: http

        POST /movies/_find HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Content-Length: 112
        Host: localhost:5984

        {
            "selector": {
                "afieldname": {"$regex": "^A"}
            }
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Thu, 01 Sep 2016 17:25:51 GMT
        Server: CouchDB (Erlang OTP)
        Transfer-Encoding: chunked

        {
            "warning":"no matching index found, create an index to optimize
            query time",
            "docs":[
            ]
        }

.. warning::
    It's always recommended that you create an appropriate index when deploying
    in production.

Most selector expressions work exactly as you would expect for the given
operator. But it is not always the case: for example, comparison of strings is
done with ICU and can can give surprising results if you were expecting ASCII
ordering. See :ref:`views/collation` for more details.

.. _find/sort:

Sort Syntax
===========

The ``sort`` field contains a list of field name and direction pairs, expressed
as a basic array. The first field name and direction pair is the topmost level
of sort. The second pair, if provided, is the next level of sort.

The field can be any field, using dotted notation if desired for sub-document
fields.

The direction value is ``"asc"`` for ascending, and ``"desc"`` for descending.
If you omit the direction value, the default ``"asc"`` is used.

Example, sorting by 2 fields:

    .. code-block:: javascript

        [{"fieldName1": "desc"}, {"fieldName2": "desc" }]

Example, sorting by 2 fields, assuming default direction for both :

    .. code-block:: javascript

        ["fieldNameA", "fieldNameB"]

A typical requirement is to search for some content using a selector, then to
sort the results according to the specified field, in the required direction.

To use sorting, ensure that:

-  At least one of the sort fields is included in the selector.
-  There is an index already defined, with all the sort fields in the same
    order.
-  Each object in the sort array has a single key.

If an object in the sort array does not have a single key, the resulting sort
order is implementation specific and might change.

Find does not support multiple fields with different sort orders, so the
directions must be either all ascending or all descending.

For field names in text search sorts, it is sometimes necessary for a
field type to be specified, for example:

``{ "<fieldname>:string": "asc"}``

If possible, an attempt is made to discover the field type based on the
selector. In ambiguous cases the field type must be provided explicitly.

The sorting order is undefined when fields contain different data types.
This is an important difference between text and view indexes. Sorting
behavior for fields with different data types might change in future
versions.

    A simple query, using sorting:

.. code-block:: javascript

    {
        "selector": {"Actor_name": "Robert De Niro"},
        "sort": [{"Actor_name": "asc"}, {"Movie_runtime": "asc"}]
    }
.. _find/filter:

Filtering Fields
================

It is possible to specify exactly which fields are returned for a document when
selecting from a database. The two advantages are:

-  Your results are limited to only those parts of the document that are
    required for your application.
-  A reduction in the size of the response.

The fields returned are specified as an array.

Only the specified filter fields are included, in the response. There is no
automatic inclusion of the ``_id`` or other metadata fields when a field list
is included.

Example of selective retrieval of fields from matching documents:

    .. code-block:: javascript

        {
            "selector": { "Actor_name": "Robert De Niro" },
            "fields": ["Actor_name", "Movie_year", "_id", "_rev"]
        }

Pagination
==========

Mango queries support pagination via the bookmark field. Every ``_find``
response contains a bookmark - a token that CouchDB uses to determine
where to resume from when subsequent queries are made. To get the next
set of query results, add the bookmark that was received in the previous
response to your next request. Remember to keep the `selector` the same,
otherwise you will receive unexpected results. To paginate backwards,
you can use a previous bookmark to return the previous set of results.

Note that the presence of a bookmark doesn't guarantee that there are
more results. You can to test whether you have reached the end of the
result set by comparing the number of results returned with the page
size requested - if results returned < `limit`, there are no more.

.. _find/statistics:

Execution Statistics
====================

Find can return basic execution statistics for a specific request. Combined with
the :ref:`_explain <api/db/find/explain>` endpoint, this should provide some
insight as to whether indexes are being used effectively.

The execution statistics currently include:

+--------------------------------+--------------------------------------------+
| Field                          | Description                                |
+================================+============================================+
| ``total_keys_examined``        | Number of index keys examined.             |
|                                | Currently always 0.                        |
+--------------------------------+--------------------------------------------+
| ``total_docs_examined``        | Number of documents fetched from the       |
|                                | database / index, equivalent to using      |
|                                | ``include_docs=true`` in a view.           |
|                                | These may then be filtered in-memory to    |
|                                | further narrow down the result set based   |
|                                | on the selector.                           |
+--------------------------------+--------------------------------------------+
| ``total_quorum_docs_examined`` | Number of documents fetched from the       |
|                                | database using an out-of-band document     |
|                                | fetch. This is only non-zero when read     |
|                                | quorum > 1 is specified in the query       |
|                                | parameters.                                |
+--------------------------------+--------------------------------------------+
| ``results_returned``           | Number of results returned from the query. |
|                                | Ideally this should not be significantly   |
|                                | lower than the total documents / keys      |
|                                | examined.                                  |
+--------------------------------+--------------------------------------------+
| ``execution_time_ms``          | Total execution time in milliseconds as    |
|                                | measured by the database.                  |
+--------------------------------+--------------------------------------------+

.. _api/db/find/index:

================
``/db/_index``
================

.. _api/db/find/index-post:

Mango is a declarative JSON querying language for CouchDB databases.
Mango wraps several index types, starting with the Primary Index
out-of-the-box. Mango indexes, with index type `json`, are
built using MapReduce Views.

.. http:post:: /{db}/_index
    :synopsis: Create a new index.

    Create a new index on a database

    :param db: Database name

    :<header Content-Type: - :mimetype:`application/json`

    :query json index: JSON object describing the index to create.
    :query string ddoc: Name of the design document in which the index will be
        created. By default, each index will be created in its own design
        document.
        Indexes can be grouped into design documents for efficiency. However, a
        change to one index in a design document will invalidate all other
        indexes in the same document (similar to views). *Optional*
    :query string name: Name of the index. If no name is provided, a name will
        be generated automatically. *Optional*
    :query string type: Can be ``"json"`` or ``"text"``. Defaults to json.
        Geospatial indexes will be supported in the future. *Optional*
        Text indexes are supported via a third party library *Optional*
    :query boolean partitioned: Determines whether a JSON index is partitioned
        or global. The default value of ``partitioned`` is the ``partitioned``
        property of the database. To create a global index on a
        partitioned database, specify
        ``false`` for the ``"partitioned"`` field. If you specify ``true``
        for the  ``"partitioned"`` field on an unpartitioned database, an
        error occurs.

    :>header Content-Type: :mimetype:`application/json`
    :>header Transfer-Encoding: ``chunked``

    :>json string result: Flag to show whether the index was created or one
        already exists. Can be ``"created"`` or ``"exists"``.
    :>json string id: Id of the design document the index was created in.
    :>json string name: Name of the index created.

    :code 200: Index created successfully or already exists
    :code 400: Invalid request
    :code 401: Admin permission required
    :code 404: Database not found
    :code 500: Execution error

    The `Index object` is a JSON object with the following fields:

    :json array fields: array of field names following the :ref:`sort
       syntax <find/sort>`. Nested fields are also allowed, e.g. `"person.name"`.
    :json json partial_filter_selector: A :ref:`selector <find/selectors>`
       to apply to documents at indexing time, creating a
       :ref:`partial index <find/partial_indexes>`. *Optional*

    Example of creating a new index for a field called ``foo``:

    **Request**:

    .. code-block:: http

        POST /db/_index HTTP/1.1
        Content-Type: application/json
        Content-Length: 116
        Host: localhost:5984

        {
            "index": {
                "fields": ["foo"]
            },
            "name" : "foo-index",
            "type" : "json"
        }

The returned JSON confirms the index has been created:

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 96
        Content-Type: application/json
        Date: Thu, 01 Sep 2016 18:17:48 GMT
        Server: CouchDB (Erlang OTP/18)

        {
            "result":"created",
            "id":"_design/a5f4711fc9448864a13c81dc71e660b524d7410c",
            "name":"foo-index"
        }

Example index creation using all available query parameters

    **Request**:

    .. code-block:: http

        POST /db/_index HTTP/1.1
        Content-Type: application/json
        Content-Length: 396
        Host: localhost:5984

        {
            "index": {
                "partial_filter_selector": {
                    "year": {
                        "$gt": 2010
                    },
                    "limit": 10,
                    "skip": 0
                },
                "fields": [
                    "_id",
                    "_rev",
                    "year",
                    "title"
                ]
            },
            "ddoc": "example-ddoc",
            "name": "example-index",
            "type": "json",
            "partitioned": false
        }

By default, a JSON index will include all documents that have the indexed fields
present, including those which have null values.

.. _find/partial_indexes:

Partial Indexes
===============

Partial indexes allow documents to be filtered at indexing time, potentially
offering significant performance improvements for query selectors that don't
map cleanly to a range query on an index.

Let's look at an example query:

.. code-block:: javascript

    {
      "selector": {
        "status": {
          "$ne": "archived"
        },
        "type": "user"
      }
    }

Without a partial index, this requires a full index scan to find all the
documents of ``"type":"user"`` that do not have a status of ``"archived"``.
This is because a normal index can only be used to match contiguous rows,
and the ``"$ne"`` operator cannot guarantee that.

To improve response times, we can create an index which excludes documents
where  ``"status": { "$ne": "archived" }`` at index time using the
``"partial_filter_selector"`` field:

.. code-block:: http

        POST /db/_index HTTP/1.1
        Content-Type: application/json
        Content-Length: 144
        Host: localhost:5984

        {
          "index": {
            "partial_filter_selector": {
              "status": {
                "$ne": "archived"
              }
            },
            "fields": ["type"]
          },
          "ddoc" : "type-not-archived",
          "type" : "json"
        }

Partial indexes are not currently used by the query planner unless specified
by a ``"use_index"`` field, so we need to modify the original query:

.. code-block:: javascript

    {
      "selector": {
        "status": {
          "$ne": "archived"
        },
        "type": "user"
      },
      "use_index": "type-not-archived"
    }

Technically, we don't need to include the filter on the ``"status"`` field
in the query selector - the partial index ensures this is always true -
but including it makes the intent of the selector clearer and will make
it easier to take advantage of future improvements to query planning
(e.g. automatic selection of partial indexes).

.. note::
    An index with fields is only used, when the selector includes
    all of the fields indexed. For instance, if an index contains ``["a". "b"]``
    but the selector only requires field ``["a"]`` to exist in the matching
    documents, the index would not be valid for the query. All indexes,
    however, can be treated as if they include the special fields ``_id`` and
    ``_rev``. They **never** need to be specified in the query selector.

.. _api/db/find/index-get:

.. http:get:: /{db}/_index
    :synopsis: List all indexes.

    When you make a ``GET`` request to ``/db/_index``, you get a list of all
    indexes in the database. In addition to the information available through
    this API, indexes are also stored in design documents <index-functions>.
    Design documents are regular documents that have an ID starting with
    ``_design/``. Design documents can be retrieved and modified in the same
    way as any other document, although this is not necessary when using Mango.

    :param db: Database name.

    :>header Content-Type: :mimetype:`application/json`
    :>header Transfer-Encoding: ``chunked``

    :>json number total_rows: Number of indexes
    :>json object indexes: Array of index definitions

    :code 200: Success
    :code 400: Invalid request
    :code 401: Read permission required
    :code 500: Execution error

    Format of index objects:
        -  **ddoc**: ID of the design document the index belongs to. This ID
            can be used to retrieve the design document containing the index,
            by making a ``GET`` request to ``/db/ddoc``, where ``ddoc`` is the
            value of this field.
        -  **name**: Name of the index.
        -  **type**: Type of the index. Currently "json" is the only
            supported type.
        -  **def**: Definition of the index, containing the indexed fields
            and the sort order: ascending or descending.

    **Request**:

    .. code-block:: http

        GET /db/_index HTTP/1.1
        Accept: application/json
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 238
        Content-Type: application/json
        Date: Thu, 01 Sep 2016 18:17:48 GMT
        Server: CouchDB (Erlang OTP/18)

        {
            "total_rows": 2,
            "indexes": [
            {
                "ddoc": null,
                "name": "_all_docs",
                "type": "special",
                "def": {
                    "fields": [
                        {
                            "_id": "asc"
                        }
                    ]
                }
            },
            {
                "ddoc": "_design/a5f4711fc9448864a13c81dc71e660b524d7410c",
                "name": "foo-index",
                "type": "json",
                "def": {
                    "fields": [
                        {
                            "foo": "asc"
                        }
                    ]
                }
            }
          ]
        }

.. _api/db/find/index-delete:

.. http:delete:: /{db}/_index/{designdoc}/json/{name}
    :synopsis: Delete an index

    :param db: Database name.
    :param designdoc: Design document name.
    :param name: Index name.

    :>header Content-Type: :mimetype:`application/json`

    :>json string ok: `"true"` if successful.

    :code 200: Success
    :code 400: Invalid request
    :code 401: Writer permission required
    :code 404: Index not found
    :code 500: Execution error

    **Request**:

    .. code-block:: http

        DELETE /db/_index/_design/a5f4711fc9448864a13c81dc71e660b524d7410c/json/foo-index HTTP/1.1
        Accept: */*
        Host: localhost:5984

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Length: 12
        Content-Type: application/json
        Date: Thu, 01 Sep 2016 19:21:40 GMT
        Server: CouchDB (Erlang OTP/18)

        {
            "ok": true
        }

.. _api/db/find/explain:

================
``/db/_explain``
================

.. http:post:: /{db}/_explain
    :synopsis: Identify which index is being used by a particular query.

    Shows which index is being used by the query.  Parameters are the same as
    :ref:`_find <api/db/_find>`

    :param db: Database name

    :<header Content-Type: :mimetype:`application/json`

    :>header Content-Type: :mimetype:`application/json`
    :>header Transfer-Encoding: ``chunked``

    :>json string dbname: Name of database
    :>json object index: Index used to fulfill the query
    :>json object selector: Query selector used
    :>json object opts: Query options used
    :>json number limit: Limit parameter used
    :>json number skip: Skip parameter used
    :>json array fields: Fields to be returned by the query
    :>json object range: Range parameters passed to the underlying view

    :code 200: Request completed successfully
    :code 400: Invalid request
    :code 401: Read permission required
    :code 500: Execution error

    **Request**:

    .. code-block:: http

        POST /movies/_explain HTTP/1.1
        Accept: application/json
        Content-Type: application/json
        Content-Length: 168
        Host: localhost:5984

        {
            "selector": {
                "year": {"$gt": 2010}
            },
            "fields": ["_id", "_rev", "year", "title"],
            "sort": [{"year": "asc"}],
            "limit": 2,
            "skip": 0
        }

    **Response**:

    .. code-block:: http

        HTTP/1.1 200 OK
        Cache-Control: must-revalidate
        Content-Type: application/json
        Date: Thu, 01 Sep 2016 15:41:53 GMT
        Server: CouchDB (Erlang OTP)
        Transfer-Encoding: chunked

        {
            "dbname": "movies",
            "index": {
                "ddoc": "_design/0d61d9177426b1e2aa8d0fe732ec6e506f5d443c",
                "name": "0d61d9177426b1e2aa8d0fe732ec6e506f5d443c",
                "type": "json",
                "def": {
                    "fields": [
                        {
                            "year": "asc"
                        }
                    ]
                }
            },
            "selector": {
                "year": {
                    "$gt": 2010
                }
            },
            "opts": {
                "use_index": [],
                "bookmark": "nil",
                "limit": 2,
                "skip": 0,
                "sort": {},
                "fields": [
                    "_id",
                    "_rev",
                    "year",
                    "title"
                ],
                "r": [
                    49
                ],
                "conflicts": false
            },
            "limit": 2,
            "skip": 0,
            "fields": [
                "_id",
                "_rev",
                "year",
                "title"
            ],
            "range": {
                "start_key": [
                    2010
                ],
                "end_key": [
                    {}
                ]
            }
        }

Index selection
===============

`_find` chooses which index to use for responding to a query, unless you specify
an index at query time.

The query planner looks at the selector section and finds the index with the
closest match to operators and fields used in the query. If there are two
or more json type indexes that match, the index with the smallest
number of fields in the index is preferred.
If there are still two or more candidate indexes,
the index with the first alphabetical name is chosen.

.. note::
    It's good practice to specify indexes explicitly in your queries. This
    prevents existing queries being affected by new indexes that might get added
    in a production environment.
