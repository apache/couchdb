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

.. _ddoc/nouveau:

=======
Nouveau
=======

.. warning::
    Nouveau is an experimental feature. Future releases might change how the endpoints
    work and might invalidate existing indexes.

Nouveau indexes enable you to query a database by using the
`Lucene Query Parser Syntax. <https://lucene.apache.org/core/9_5_0/queryparser/
org/apache/lucene/queryparser/classic/package-summary.html#Overview>`_
A nouveau index uses one, or multiple, fields from your documents. You can use a nouveau
index to run queries to find documents based on the content they contain.

.. warning::
    Nouveau cannot function unless it has a functioning Nouveau server.
    See :ref:`Nouveau Server Installation <install/nouveau>` for details.

To create a nouveau index, you add a JavaScript function to a design document in the
database. An index builds after processing one search request or after the server detects
a document update. The ``index`` function takes the following parameters:

#. Field type - The type of the field, can be ``string``, ``text``, ``double``
   or ``stored``. See :ref:`Field Types <ddoc/nouveau/field_types>` for more information.
#. Field name - The name of the field you want to use when you query the index.
   If you set this parameter to ``default``, then this field is queried if no field is
   specified in the query syntax.
#. Data that you want to index, for example, ``doc.address.country``.
#. (Optional) The third parameter includes the following field: ``store``.

By default, a nouveau index response returns 25 rows. The number of hits that are returned
can be changed by using the ``limit`` parameter. Each response includes a ``bookmark``
field. You can include the value of the ``bookmark`` field in subsequent queries to fetch
results from deeper in the result set.

*Example design document that defines a nouveau index:*

.. code-block:: javascript

    {
        "_id": "_design/nouveau_example",
        "nouveau": {
            "animals": {
                "index": "function(doc){ ... }"
            }
        }
    }

A nouveau index will inherit the partitioning type from the ``options.partitioned`` field
of the design document that contains it.

.. _ddoc/nouveau/field_types:

Field Types
===========

Nouveau currently supports four field types, each of which has different semantics to the
others.

Text
    A text field is the most common field type, the field value is analyzed at
    index time to permit efficient querying by the individual words within it
    (and wildcards, and regex, etc). This field type is not appropriate for
    sorting, range queries and faceting.

String
    A string field indexes the fields value as a single token without
    analysis (that is, no case-folding, no common suffixes are removed,
    etc). This field type is recommended for sorting and faceting. You *can*
    search on string fields but you must specify the ``keyword`` analyzer in the
    index definition for this field to ensure that your queries are not
    analyzed.

Double
    A double field requires a number value and is appropriate for sorting,
    range queries and range faceting.

Stored
    A stored field stores the field value into the index without
    analysis. The value is returned with search results but you cannot search,
    sort, range or facet over a stored field.

.. warning:: the type of any specific field is determined by the first index
             call. Attempts to index a different type into the same field will
             throw an exception and prevent the index from building.

Index functions
===============

Attempting to index by using a data field that does not exist fails. To avoid
this problem, use the appropriate
:ref:`guard clause <ddoc/nouveau/index_guard_clauses>`.

.. note::
    Your indexing functions operate in a memory-constrained environment
    where the document itself forms a part of the memory that is used
    in that environment. Your code's stack and document must fit inside this
    memory. In other words, a document must be loaded in order to be indexed.
    Documents are limited to a maximum size of 64 MB.

The function that is contained in the index field is a JavaScript function
that is called for each document in the database.
The function takes the document as a parameter,
extracts some data from it, and then calls the function that is defined
in the ``index`` field to index that data.

The ``index`` function takes four parameters, where the third parameter is optional.

#. The first parameter is the type of the field.

#. The second parameter is the name of the field you intend to use
   when querying the index, and which is specified in the Lucene
   syntax portion of subsequent queries.  An example appears in the
   following query:

   .. code-block:: javascript

        q=color:red

   The Lucene field name ``color`` is the first parameter of the ``index`` function.

   If the special value ``"default"`` is used when you define the name,
   you do not have to specify a field name at query time.
   The effect is that the query can be simplified:

   .. code-block:: javascript

       q=red

#. The third parameter is the data to be indexed. Keep the following information
   in mind when you index your data:

   - This data must be only a string, number, or boolean. Other types will cause
     an error to be thrown by the index function call.

   - If an error is thrown when running your function, for this reason or others,
     the document will not be added to that search index.

#. The fourth, optional, parameter is a JavaScript object with the following fields:

   *Index function (optional parameter)*

   * **store** - If ``true``, the value is returned in the search result; otherwise,
     the value is not returned. Values are ``true`` or ``false``. Default is ``false``.

   .. note::

       If you do not set the ``store`` parameter,
       the index data results for the document are not returned in response to a query.

*Example search index function:*

.. code-block:: javascript

    function(doc) {
        if (typeof(doc.min_length) == 'number') {
            index("double", "min_length", doc.min_length, {"store": true});
        }
        if (typeof(doc.diet) == 'string') {
            index("string", "diet", doc.diet, {"store": true});
        }
        if (typeof(doc.latin_name) == 'string') {
            index("string", "latin_name", doc.latin_name, {"store": true});
        }
        if (typeof(doc.class) == 'string') {
            index("string", "class", doc.class, {"store": true});
        }
    }

.. _ddoc/nouveau/index_guard_clauses:

Index guard clauses
-------------------

Runtime errors in the index function cause the document not to be indexed at all. The
most common runtime errors are described below;

*Example of failing to check whether the indexed value exists:*

.. warning:: example of bad code
.. code-block:: javascript

    index("double", "min_length", doc.min_length, {"store": true});

For documents without a `min_length` value, this index call will
pass ``undefined`` as the value. This will be rejected by nouveau's
validation function and the document will not be indexed.

*Example of failing to check whether the nested indexed value exists:*

.. warning:: example of bad code
.. code-block:: javascript

    if (doc.foo.bar) {
        index("string", "bar", doc.foo.bar, {"store": true});
    }

This bad example fails in a different way if ``doc.foo`` doesn't
exist; the evaluation of ``doc.foo.bar`` throws an exception.

.. code-block:: javascript

    if (doc.foo && typeof(doc.foo) == 'object' && typeof(doc.foo.bar == 'string')) {
        index("string", "bar", doc.foo.bar, {"store": true});
    }

This example correctly checks that ``doc.foo`` is an object and its
``bar`` entry is a string.

*Example of checking the index value exists but disallowing valid false values:*

.. warning:: example of bad code
.. code-block:: javascript

    if (doc.min_length) {
      index("double", "min_length", doc.min_length, {"store": true});
    }

We correct the previous mistake so documents without min_length are
indexed (assuming there are other index calls for values that `do`
exist) but we've acccidentally prevented the indexing of the
``min_length`` field if the ``doc.min_length`` happens to be ``0``.

.. code-block:: javascript

    if (typeof(doc.min_length == 'number')) {
      index("double", "min_length", doc.min_length, {"store": true});
    }

This good example ensures we index any document where ``min_length`` is a number.

.. _ddoc/nouveau/analyzers:

Analyzers
=========

Analyzers convert textual input into ``tokens`` which can be searched
on. Analyzers typically have different rules for how they break up
input into tokens, they might convert all text to lower case, they
might omit whole words (typically words so common they are unlikely to
be useful for searching), they might omit parts of words (removing
``ing`` suffixes in English, for example):

We expose a large number of Lucene's analyzers. We invent one
ourselves (``simple_asciifolding``);

* arabic
* armenian
* basque
* bulgarian
* catalan
* chinese
* cjk
* classic
* czech
* danish
* dutch
* email
* english
* finnish
* french
* galician
* german
* hindi
* hungarian
* indonesian
* irish
* italian
* japanese
* keyword
* latvian
* norwegian
* persian
* polish
* portugese
* romanian
* russian
* simple
* simple_asciifolding
* spanish
* standard
* swedish
* thai
* turkish
* whitespace

*Example analyzer document:*

.. code-block:: javascript

    {
        "_id": "_design/analyzer_example",
        "nouveau": {
            "INDEX_NAME": {
                "index": "function (doc) { ... }",
                "default_analyzer": "$ANALYZER_NAME"
            }
        }
    }

.. _ddoc/nouveau/field-analyzers:

Field analyzers
----------------

You may optionally specify a different analyzer for a specific field.

*Example of defining different analyzers for different fields:*

.. code-block:: javascript

    {
        "_id": "_design/analyzer_example",
        "nouveau": {
            "INDEX_NAME": {
                "default_analyzer": "english",
                "field_analyzers": {
                    "spanish": "spanish",
                    "german": "german"
                },
                "index": "function (doc) { ... }"
            }
        }
    }

Testing analyzer tokenization
-----------------------------

You can test the results of analyzer tokenization by posting sample data to the
``_nouveau_analyze`` endpoint.

*Example of using HTTP to test the keyword analyzer:*

.. code-block:: http

    POST /_nouveau_analyze HTTP/1.1
    Content-Type: application/json
    {"analyzer":"keyword", "text":"ablanks@renovations.com"}

*Example of using the command line to test the keyword analyzer:*

.. code-block:: sh

    curl 'https://$HOST:5984/_nouveau_analyze' -H 'Content-Type: application/json'
        -d '{"analyzer":"keyword", "text":"ablanks@renovations.com"}'

*Result of testing the keyword analyzer:*

.. code-block:: javascript

    {
        "tokens": [
            "ablanks@renovations.com"
        ]
    }

*Example of using HTTP to test the standard analyzer:*

.. code-block:: http

    POST /_nouveau_analyze HTTP/1.1
    Content-Type: application/json
    {"analyzer":"standard", "text":"ablanks@renovations.com"}

*Example of using the command line to test the standard analyzer:*

.. code-block:: sh

    curl 'https://$HOST:5984/_nouveau_analyze' -H 'Content-Type: application/json'
        -d '{"analyzer":"standard", "text":"ablanks@renovations.com"}'

*Result of testing the standard analyzer:*

.. code-block:: javascript

    {
        "tokens": [
            "ablanks",
            "renovations.com"
        ]
    }

Queries
=======

After you create a search index, you can query it.

- Issue a partition query using:
  ``GET /$DATABASE/_partition/$PARTITION_KEY/_design/$DDOC/_nouveau/$INDEX_NAME``
- Issue a global query using:
  ``GET /$DATABASE/_design/$DDOC/_nouveau/$INDEX_NAME``

Specify your search by using the ``q`` parameter.

*Example of using HTTP to query a partitioned index:*

.. code-block:: http

    GET /$DATABASE/_partition/$PARTITION_KEY/_design/$DDOC/_nouveau/$INDEX_NAME?include_docs=true&q=*:*&limit=1 HTTP/1.1
    Content-Type: application/json

*Example of using HTTP to query a global index:*

.. code-block:: http

    GET /$DATABASE/_design/$DDOC/_nouveau/$INDEX_NAME?include_docs=true&q=*:*&limit=1 HTTP/1.1
    Content-Type: application/json

*Example of using the command line to query a partitioned index:*

.. code-block:: sh

    curl https://$HOST:5984/$DATABASE/_partition/$PARTITION_KEY/_design/$DDOC/
    _nouveau/$INDEX_NAME?include_docs=true\&q=*:*\&limit=1 \

*Example of using the command line to query a global index:*

.. code-block:: sh

    curl https://$HOST:5984/$DATABASE/_design/$DDOC/_nouveau/$INDEX_NAME?
    include_docs=true\&q=*:*\&limit=1 \

.. _ddoc/nouveau/query_parameters:

Query Parameters
----------------

A full list of query parameters can be found in the
:ref:`API Reference <api/ddoc/nouveau>`.

.. note::
    Do not combine the ``bookmark`` and ``update`` options. These options
    constrain the choice of shard replicas to use for the response. When used
    together, the options might cause problems when contact is attempted
    with replicas that are slow or not available.

Relevance
---------

When more than one result might be returned, it is possible for them to be sorted. By
default, the sorting order is determined by 'relevance'.

Relevance is measured according to `Apache Lucene Scoring
<https://lucene.apache.org/core/9_5_0/core/org/apache/
lucene/search/package-summary.html>`_.
As an example, if you search a simple database for the word
``example``, two documents might contain the word. If one document
mentions the word ``example`` 10 times, but the second document
mentions it only twice, then the first document is considered to be
more 'relevant'.

If you do not provide a ``sort`` parameter, relevance is used by default. The highest
scoring matches are returned first.

If you provide a ``sort`` parameter, then matches are returned in that order, ignoring
relevance.

If you want to use a ``sort`` parameter, and also include ordering by relevance in your
search results, use the special fields ``-<score>`` or ``<score>`` within the ``sort``
parameter.

POSTing search queries
----------------------

Instead of using the ``GET`` HTTP method, you can also use ``POST``. The main advantage of
``POST`` queries is that they can have a request body, so you can specify the request as a
JSON object. Each parameter in the query string of a ``GET`` request corresponds to a
field in the JSON object in the request body.

*Example of using HTTP to POST a search request:*

.. code-block:: http

    POST /db/_design/ddoc/_nouveau/searchname HTTP/1.1
    Content-Type: application/json

*Example of using the command line to POST a search request:*

.. code-block:: sh

    curl 'https://$HOST:5984/db/_design/ddoc/_nouveau/searchname' -X POST -H 'Content-Type: application/json' -d @search.json

*Example JSON document that contains a search request:*

.. code-block:: javascript

    {
        "q": "index:my query",
        "sort": "foo",
        "limit": 3
    }

Query syntax
============

The CouchDB search query syntax is based on the
`Lucene syntax. <https://lucene.apache.org/core/9_5_0/queryparser/org/apache/
lucene/queryparser/classic/package-summary.html>`_
Search queries take the form of ``name:value`` unless the name is omitted, in which case
they use the default field, as demonstrated in the following examples:

*Example search query expressions:*

.. code-block:: javascript

    // Birds
    class:bird

.. code-block:: text

    // Animals that begin with the letter "l"
    l*

.. code-block:: text

    // Carnivorous birds
    class:bird AND diet:carnivore

.. code-block:: text

    // Herbivores that start with letter "l"
    l* AND diet:herbivore

.. code-block:: text

    // Medium-sized herbivores
    min_length:[1 TO 3] AND diet:herbivore

.. code-block:: text

    // Herbivores that are 2m long or less
    diet:herbivore AND min_length:[* TO 2]

.. code-block:: text

    // Mammals that are at least 1.5m long
    class:mammal AND min_length:[1.5 TO *]

.. code-block:: text

    // Find "Meles meles"
    latin_name:"Meles meles"

.. code-block:: text

    // Mammals who are herbivore or carnivore
    diet:(herbivore OR omnivore) AND class:mammal

.. code-block:: text

    // Return all results
    *:*

Queries over multiple fields can be logically combined, and groups and fields can be
further grouped. The available logical operators are case-sensitive and are ``AND``,
``+``, ``OR``, ``NOT`` and ``-``. Range queries can run over strings or numbers.

If you want a fuzzy search, you can run a query with ``~`` to find terms like the search
term. For instance, ``look~`` finds the terms ``book`` and ``took``.

.. note::
    If the lower and upper bounds of a range query are both strings that
    contain only numeric digits, the bounds are treated as numbers not as
    strings. For example, if you search by using the query
    ``mod_date:["20170101" TO "20171231"]``, the results include documents
    for which ``mod_date`` is between the numeric values 20170101 and
    20171231, not between the strings "20170101" and "20171231".

You can alter the importance of a search term by adding ``^`` and a positive number. This
alteration makes matches containing the term more or less relevant, proportional to the
power of the boost value. The default value is 1, which means no increase or decrease in
the strength of the match. A decimal value of 0 - 1 reduces importance. making the match
strength weaker. A value greater than one increases importance, making the match strength
stronger.

Wildcard searches are supported, for both single (``?``) and multiple (``*``) character
searches. For example, ``dat?`` would match ``date`` and ``data``, whereas ``dat*`` would
match ``date``, ``data``, ``database``, and ``dates``. Wildcards must come after the
search term.

Use ``*:*`` to return all results.

The following characters require escaping if you want to search on them:

.. code-block:: sh

    + - && || ! ( ) { } [ ] ^ " ~ * ? : \ /

To escape one of these characters, use a preceding backslash character (``\``).

The response to a search query contains an ``order`` field for each of the results. The
``order`` field is an array where the first element is the field or fields that are
specified in the ``sort`` parameter. See the
:ref:`sort parameter <api/ddoc/search>`. If no ``sort`` parameter is included
in the query, then the ``order`` field contains the `Lucene relevance score
<https://lucene.apache.org/core/9_5_0/core/org/apache/
lucene/search/package-summary.html>`_.

.. _ddoc/nouveau/faceting:

Faceting
--------

Nouveau Search also supports faceted searching, enabling discovery of aggregate
information about matches quickly and easily. You can match all documents by using the
special ``?q=*:*`` query syntax, and use the returned facets to refine your query.

*Example of search query:*

.. code-block:: javascript

    function(doc) {
        index("string", "type", doc.type);
        index("double", "price", doc.price);
    }

To use facets, all the documents in the index must include all the fields that have
faceting enabled. If your documents do not include all the fields, you receive a
``bad_request`` error with the following reason, "The ``field_name`` does not exist." If
each document does not contain all the fields for facets, create separate indexes for each
field. If you do not create separate indexes for each field, you must include only
documents that contain all the fields. Verify that the fields exist in each document by
using a single ``if`` statement.

The ``top_n`` query parameter controls how many facets, per grouping, are returned,
defaulting to 10, to a maximum of 1000.

*Example if statement to verify that the required fields exist in each document:*

.. code-block:: javascript

    if (typeof doc.town == "string" && typeof doc.name == "string") {
        index("string", "town", doc.town);
        index("string", "name", doc.name);
       }

Counts
------

.. note::
    The ``counts`` option is only available when making global queries.

The ``counts`` facet syntax takes a list of fields, and returns the number of query
results for each unique value of each named field.

.. note::
    The ``count`` operation works only if the indexed values are strings.
    The indexed values cannot be mixed types. For example,
    if 100 strings are indexed, and one number,
    then the index cannot be used for ``count`` operations.
    You can check the type by using the ``typeof`` operator, and convert it
    by using the ``parseInt``,
    ``parseFloat``, or ``.toString()`` functions.

*Example of a query using the counts facet syntax:*

.. code-block:: text

    ?q=*:*&counts=["type"]

*Example response after using of the counts facet syntax:*

.. code-block:: javascript

    {
        "total_rows":100000,
        "bookmark":"g...",
        "rows":[...],
        "counts":{
            "type":{
                "sofa": 10,
                "chair": 100,
                "lamp": 97
            }
        }
    }

Ranges
------

.. note::
    The ``ranges`` option is only available when making global queries.

The value of the range parameter is a JSON object where the fields names are double
fields, and the values of the fields are arrays of JSON objects. The objects
must have a ``label``, ``min`` and ``max`` value (of type string, double, double
respectively), and optional ``min_inclusive`` and ``max_inclusive`` properties
(defaulting to ``true`` if not specified).

*Example of a request that uses faceted search for matching ranges:*

.. code-block:: text

    ?q=*:*&ranges={"price":[{"label":"cheap","min":0,"max":"100","max_inclusive":false},{"label":"expensive","min":100}]}

*Example results after a ranges check on a faceted search:*

.. code-block:: javascript

    {
        "total_rows":100000,
        "bookmark":"g...",
        "rows":[...],
        "ranges": {
            "price": {
                "expensive": 278682,
                "cheap": 257023
            }
        }
    }
