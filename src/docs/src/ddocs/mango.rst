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

=============
Mango Queries
=============

In addition to :ref:`map/reduce views <viewfun>`, CouchDB supports an expressive
query system called Mango.

Mango consists of two major concepts:

* Selectors, which are the queries, and are passed to the mango endpoints
* Indexes, which are a specialization of :ref:`design docs <ddocs>` used in mango queries

There are a few important endpoints for interacting with these concepts:

* :http:post:`/{db}/_find`, which executes a query
* :http:post:`/{db}/_explain`, which describes the execution of a query
* :http:get:`/{db}/_index` and :http:post:`/{db}/_index`, which manage indexes

.. _find/selectors:

Selectors
=========

Selectors are expressed as a JSON object describing documents of interest.
Within this structure, you can apply conditional logic using specially named
fields.

Whilst selectors have some similarities with MongoDB query documents, these
arise from a similarity of purpose and do not necessarily extend to commonality
of function or result.

.. warning::

    While CouchDB will happily store just about anything JSON, Mango has
    limitations about what it can work with:

    * Empty field names (``""``) cannot be queried ("One or more conditions is
      missing a field name.").
    * Field names starting with ``$`` must be escaped with ``\`` (eg, ``\$foo``)
      ("Invalid operator: $").

.. _find/selectorbasics:

Selector Basics
---------------

Elementary selector syntax requires you to specify one or more fields, and the
corresponding values required for those fields. This selector matches all
documents whose ``"director"`` field has the value ``"Lars von Trier"``.

.. code-block:: javascript

    {
        "director": "Lars von Trier"
    }

A simple selector, inspecting specific fields:

.. code-block:: javascript

    "selector": {
        "title": "Live And Let Die"
    },
    "fields": [
        "title",
        "cast"
    ]

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

Operators are identified by the use of a dollar sign (``$``) prefix in the name
field.

There are two core types of operators in the selector syntax:

-  Combination operators
-  Condition operators

In general, combination operators are applied at the topmost level of selection.
They are used to combine conditions, or to create combinations of conditions,
into one selector.

Every explicit operator has the form:

.. code-block:: javascript

    {
        "$operator": argument
    }

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
``$and`` operator on each field.

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

Example of implicit operator applied to a subfield test:

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

An example of the ``$eq`` operator used with database indexed on the field ``"year"``:

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

Example of using explicit ``$and`` and ``$eq`` operators:

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

It is entirely up to you whether you use the implicit or explicit form. The implicit form is a little easier to write if you do that by hand. The explicit form is a little easier if you programatically contract your selectors. The end result will be the same.

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
| ``$text``        | String   | Perform a text search                            |
+------------------+----------+--------------------------------------------------+

.. _find/and:

The ``$and`` operator
~~~~~~~~~~~~~~~~~~~~~

``$and`` operator used with two fields:

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

The ``$or`` operator
~~~~~~~~~~~~~~~~~~~~

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

The ``$not`` operator
~~~~~~~~~~~~~~~~~~~~~

The ``$not`` operator matches if the given selector does not match. Below is an
example used with an index on the field ``"year"``:

.. code-block:: javascript

    {
        "year": {
            "$gte": 1900,
            "$lte": 1903
        },
        "$not": {
            "year": 1901
        }
    }

.. _find/nor:

The ``$nor`` operator
~~~~~~~~~~~~~~~~~~~~~

The ``$nor`` operator matches if the given selector does not match. Below is an
example used with an index on the field ``"year"``:

.. code-block:: javascript

    {
        "year": {
            "$gte": 1900.
            "$lte": 1910
        },
        "$nor": [
            { "year": 1901 },
            { "year": 1905 },
            { "year": 1907 }
        ]
    }

.. _find/all:

The ``$all`` operator
~~~~~~~~~~~~~~~~~~~~~

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

The ``$elemMatch`` operator
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

The ``$allMatch`` operator
~~~~~~~~~~~~~~~~~~~~~~~~~~

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

The ``$keyMapMatch`` operator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

.. _find/text:

The ``$text`` operator
~~~~~~~~~~~~~~~~~~~~~~

The ``$text`` operator performs a text search using either a search or nouveau
index. The specifics of the query follow either
:ref:`search syntax <ddoc/search/syntax>` or
:ref:`nouveau syntax <ddoc/nouveau/syntax>` (which both use Lucene and implement
the same syntax).

.. code-block:: javascript

    {
        "_id": { "$gt": null },
        "$text": "director:George"
    }

.. warning::

    Queries cannot contain more than one ``$text``

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

+---------------+-----------------+-------------+------------------------------------+
| Operator type |    Operator     |  Argument   |              Purpose               |
+===============+=================+=============+====================================+
| (In)equality  | ``$lt``         | Any JSON    | The field is less than the         |
|               |                 |             | argument.                          |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$lte``        | Any JSON    | The field is less than or equal to |
|               |                 |             | the argument.                      |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$eq``         | Any JSON    | The field is equal to the argument.|
+---------------+-----------------+-------------+------------------------------------+
|               | ``$ne``         | Any JSON    | The field is not equal to the      |
|               |                 |             | argument.                          |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$gte``        | Any JSON    | The field is greater than or equal |
|               |                 |             | to the argument.                   |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$gt``         | Any JSON    | The field is greater than the      |
|               |                 |             | argument.                          |
+---------------+-----------------+-------------+------------------------------------+
| Object        | ``$exists``     | Boolean     | Check whether the field exists or  |
|               |                 |             | not, regardless of its value.      |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$type``       | String      | Check the document field's type.   |
|               |                 |             | Valid values are ``"null"``,       |
|               |                 |             | ``"boolean"``, ``"number"``,       |
|               |                 |             | ``"string"``, ``"array"``, and     |
|               |                 |             | ``"object"``.                      |
+---------------+-----------------+-------------+------------------------------------+
| Array         | ``$in``         | Array of    | The document field must exist in   |
|               |                 | JSON values | the list provided.                 |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$nin``        | Array of    | The document field not must exist  |
|               |                 | JSON values | in the list provided.              |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$size``       | Integer     | Special condition to match the     |
|               |                 |             | length of an array field in a      |
|               |                 |             | document. Non-array fields cannot  |
|               |                 |             | match this condition.              |
+---------------+-----------------+-------------+------------------------------------+
| Miscellaneous | ``$mod``        | [Divisor,   | Divisor is a non-zero integer,     |
|               |                 | Remainder]  | Remainder is any integer.          |
|               |                 |             | Non-integer values result in a     |
|               |                 |             | 404. Matches documents where       |
|               |                 |             | ``field % Divisor == Remainder``   |
|               |                 |             | is true, and only when the         |
|               |                 |             | document field is an integer.      |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$regex``      | String      | A regular expression pattern to    |
|               |                 |             | match against the document field.  |
|               |                 |             | Only matches when the field is a   |
|               |                 |             | string value and matches the       |
|               |                 |             | supplied regular expression. The   |
|               |                 |             | matching algorithms are based on   |
|               |                 |             | the Perl Compatible Regular        |
|               |                 |             | Expression (PCRE) library. For     |
|               |                 |             | more information about what is     |
|               |                 |             | implemented, see the               |
|               |                 |             | `Erlang Regular Expression         |
|               |                 |             | <http://erlang.org/doc             |
|               |                 |             | /man/re.html>`_.                   |
+---------------+-----------------+-------------+------------------------------------+
|               | ``$beginsWith`` | String      | Matches where the document field   |
|               |                 |             | begins with the specified prefix   |
|               |                 |             | (case-sensitive). If the document  |
|               |                 |             | field contains a non-string value, |
|               |                 |             | the document is not matched.       |
+---------------+-----------------+-------------+------------------------------------+

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

However, only operators that define a contiguous range of values
such as ``$eq``, ``$gt``, ``$gte``, ``$lt``, ``$lte``,
and ``$beginsWith`` (but not ``$ne``) can be used as the basis
of a query that can make efficient use of a ``json`` index. You should
include at least one of these in a selector, or consider using
a ``text`` index if greater flexibility is required.

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
            "warning":"no matching index found, create an index to optimize query time",
            "docs":[
            ]
        }

.. warning::
    It is always recommended that you create an appropriate index when deploying
    in production.

Most selector expressions work exactly as you would expect for the given
operator. But it is not always the case: for example, comparison of strings is
done with ICU and can can give surprising results if you were expecting ASCII
ordering. See :ref:`views/collation` for more details.

.. _ddoc/mango/indexes:

Indexes
=======

Indexes are like indexes in most other database systems: they spend a little
extra space to improve the performance of queries.

They primarily consist of a list of fields to index, but can also contain a
:ref:`selector <find/selectors>` to create a
:ref:`partial index <find/partial_indexes>`.

.. note::
    Mango indexes have a type, currently ``json``, ``text``, ``nouveau``. The
    majority of this document covers ``json`` indexes. ``text`` and ``nouveau``
    are related to the :ref:`ddoc/search` and :ref:`ddoc/nouveau` systems,
    respectively. (See :ref:`ddoc/mango/indexes/text`.)

    You will also occasionally find reference to the ``special`` index type.
    This represents synthetic indexes produced by CouchDB itself and refers
    exclusively to ``_all_docs``.

.. _ddoc/mango/indexes/definitions:

Index Definitions
-----------------

Index definitions are JSON objects with the following fields:

-  **ddoc** (`string`): ID of the design document the index belongs to. This ID
   can be used to retrieve the design document containing the index,
   by making a ``GET`` request to ``/{db}/ddoc``, where ``ddoc`` is the
   value of this field.
-  **name** (`string`): Name of the index.
-  **partitioned** (`boolean`): Partitioned (``true``) or global
   (``false``) index.
-  **type** (`string`): Type of the index. Can be ``"json"``, ``"text"``,
   ``"nouveau"``, or sometimes ``"special"``.
-  **def**/**index** (`object`): Definition of the index, depending on the type
   (see below). Which name is used depends on the context.

JSON Indexes
------------

JSON Indexes are you standard structural indexes, used by the majority of
:ref:`selector operators<find/selectors>`.

Their definition consists of:

- **fields** (`array`): Array of field names following the :ref:`sort
  syntax <find/sort>`. Nested fields are also allowed, e.g. `"person.name"`.
- **partial_filter_selector** (`object`): A :ref:`selector <find/selectors>`
  to apply to documents at indexing time, creating a
  :ref:`partial index <find/partial_indexes>`. *Optional*

Example:

.. code-block:: javascript

    {
        "type" : "json",
        "index": {
            "fields": ["foo"]
        }
    }

.. _find/partial_indexes:

Partial Indexes
---------------

Partial indexes allow documents to be filtered at indexing time, potentially
offering significant performance improvements for query selectors that do not
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

Technically, we do not need to include the filter on the ``"status"`` field
in the query selector - the partial index ensures this is always true -
but including it makes the intent of the selector clearer and will make
it easier to take advantage of future improvements to query planning
(e.g. automatic selection of partial indexes).

.. note::
    An index with fields is only used, when the selector includes
    all of the fields indexed. For instance, if an index contains ``["a", "b"]``
    but the selector only requires field ``["a"]`` to exist in the matching
    documents, the index would not be valid for the query. All indexes,
    however, can be treated as if they include the special fields ``_id`` and
    ``_rev``. They **never** need to be specified in the query selector.

.. _ddoc/mango/indexes/text:

Text Indexes
------------

Mango can also interact with the :ref:`Search <ddoc/search>` and
:ref:`Nouveau <ddoc/nouveau>` search systems, using the
:ref:`$text selector <find/text>` and the appropriate index. These indexes can
be queried using either ``$text`` or :http:get:`/{db}/_design/{ddoc}/_search/{index}`
/ :http:get:`/{db}/_design/{ddoc}/_nouveau/{index}`.

Example index:

.. code-block:: javascript

    {
        "type": "nouveau",
        "index": {
            "fields": [
                {"name": "foo", "type": "string"},
                {"name": "bar", "type": "number"},
                {"name": "baz", "type": "string"},
            ],
            "default_analyzer": "keyword",
        }
    }

A Text or Nouveau index definition consists of:

* **fields**: The list of fields to index. ``"all_fields"`` or list of objects:

  * **name** (`string`): not blank
  * **type** (`string`): one of ``"text"``, ``"string"``, ``"number"``, ``"boolean"``

* **default_analyzer** (`string`): Analyzer to use, defaults to ``"keyword"``  *Optional*
* **default_field**: Enables the "default field" index, boolean or object of
  ``enabled`` and ``analyzer`` *Optional*
* **partial_filter_selector** (`object`): A :ref:`selector<find/selectors>`, causing this
  to be a :ref:`partial index<find/partial_indexes>` *Optional*
* **selector** (`object`): A :ref:`selector<find/selectors>` *Optional*

Indexes and Design Documents
----------------------------

Ultimately, indexes are stored using design documents, using the same view
systems under the hood. If you go looking, you can find the design documents
backing mango indexes. However, exactly how mango indexes map to design
documents is an implementation detail, and users are encouraged to manage their
indexes using the :ref:`api/db/find/index` family of endpoints.
