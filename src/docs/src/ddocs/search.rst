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

.. _ddoc/search:

======
Search
======

Search indexes enable you to query a database by using the
`Lucene Query Parser Syntax. <http://lucene.apache.org/core/4_3_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#Overview>`_
A search index uses one, or multiple, fields from your documents. You can use a search
index to run queries, find documents based on the content they contain, or work with
groups, facets, or geographical searches.

.. warning::
    Search cannot function unless it has a functioning, cluster-connected
    Clouseau instance. See :ref:`Search Plugin Installation <install/search>`
    for details.

To create a search index, you add a JavaScript function to a design document in the
database. An index builds after processing one search request or after the server detects
a document update. The ``index`` function takes the following parameters:

1.  Field name - The name of the field you want to use when you query the index. If you
set this parameter to ``default``, then this field is queried if no field is specified in
the query syntax.

2.  Data that you want to index, for example, ``doc.address.country``.

3.  (Optional) The third parameter includes the following fields: ``boost``, ``facet``,
``index``, and ``store``. These fields are described in more detail later.

By default, a search index response returns 25 rows. The number of rows that is returned
can be changed by using the ``limit`` parameter. Each response includes a ``bookmark``
field. You can include the value of the ``bookmark`` field in later queries to look
through the responses.

*Example design document that defines a search index:*

.. code-block:: javascript

    {
        "_id": "_design/search_example",
        "indexes": {
            "animals": {
                "index": "function(doc){ ... }"
            }
        }
    }

A search index will inherit the partitioning type from the ``options.partitioned`` field
of the design document that contains it.

Index functions
===============

Attempting to index by using a data field that does not exist fails. To avoid
this problem, use the appropriate
:ref:`guard clause <ddoc/search/index_guard_clauses>`.

.. note::
    Your indexing functions operate in a memory-constrained environment
    where the document itself forms a part of the memory that is used
    in that environment. Your code's stack and document must fit inside this
    memory. In other words, a document must be loaded in order to be indexed.
    Documents are limited to a maximum size of 64 MB.

.. note::
    Within a search index, do not index the same field name with more than one data
    type. If the same field name is indexed with different data types in the same search
    index function, you might get an error when querying the search index that says the
    field "was indexed without position data." For example, do not include both of these
    lines in the same search index function, as they index the ``myfield`` field as two
    different data types: a string ``"this is a string"`` and a number ``123``.

.. code-block:: javascript

    index("myfield", "this is a string");
    index("myfield", 123);

The function that is contained in the index field is a JavaScript function
that is called for each document in the database.
The function takes the document as a parameter,
extracts some data from it, and then calls the function that is defined
in the ``index`` field to index that data.

The ``index`` function takes three parameters, where the third parameter is optional.

The first parameter is the name of the field you intend to use when querying the index,
and which is specified in the Lucene syntax portion of subsequent queries.
An example appears in the following query:

.. code-block:: javascript

    query=color:red

The Lucene field name ``color`` is the first parameter of the ``index`` function.

The ``query`` parameter can be abbreviated to ``q``,
so another way of writing the query is as follows:

.. code-block:: javascript

    q=color:red

If the special value ``"default"`` is used when you define the name,
you do not have to specify a field name at query time.
The effect is that the query can be simplified:

.. code-block:: javascript

    query=red

The second parameter is the data to be indexed. Keep the following information
in mind when you index your data:

- This data must be only a string, number, or boolean. Other types will cause
  an error to be thrown by the index function call.

- If an error is thrown when running your function, for this reason or others,
  the document will not be added to that search index.

The third, optional, parameter is a JavaScript object with the following fields:

*Index function (optional parameter)*

* **boost** - A number that specifies the relevance in search results. Content that is
  indexed with a boost value greater than 1 is more relevant than content that is
  indexed without a boost value. Content with a boost value less than one is not so
  relevant. Value is a positive floating point number. Default is 1 (no boosting).

* **facet** - Creates a faceted index. See :ref:`Faceting <ddoc/search/faceting>`.
  Values are ``true`` or ``false``. Default is ``false``.

* **index** - Whether the data is indexed, and if so, how. If set to ``false``, the data
  cannot be used for searches, but can still be retrieved from the index if ``store`` is
  set to ``true``. See :ref:`Analyzers <ddoc/search/analyzers>`.
  Values are ``true`` or ``false``. Default is ``true``

* **store** - If ``true``, the value is returned in the search result; otherwise,
  the value is not returned. Values are ``true`` or ``false``. Default is ``false``.

.. note::

    If you do not set the ``store`` parameter,
    the index data results for the document are not returned in response to a query.

*Example search index function:*

.. code-block:: javascript

    function(doc) {
        index("default", doc._id);
        if (doc.min_length) {
            index("min_length", doc.min_length, {"store": true});
        }
        if (doc.diet) {
            index("diet", doc.diet, {"store": true});
        }
        if (doc.latin_name) {
            index("latin_name", doc.latin_name, {"store": true});
        }
        if (doc.class) {
            index("class", doc.class, {"store": true});
        }
    }

.. _ddoc/search/index_guard_clauses:

Index guard clauses
-------------------

The ``index`` function requires the name of the data field to index as the second
parameter. However, if that data field does not exist for the document, an error occurs.
The solution is to use an appropriate 'guard clause' that checks if the field exists, and
contains the expected type of data, *before* any attempt to create the corresponding
index.

*Example of failing to check whether the index data field exists:*

.. code-block:: javascript

    if (doc.min_length) {
        index("min_length", doc.min_length, {"store": true});
    }

You might use the JavaScript ``typeof`` function to implement the guard clause test. If
the field exists *and* has the expected type, the correct type name is returned, so the
guard clause test succeeds and it is safe to use the index function. If the field does
*not* exist, you would not get back the expected type of the field, therefore you would
not attempt to index the field.

JavaScript considers a result to be false if one of the following values is tested:

* 'undefined'
* null
* The number +0
* The number -0
* NaN (not a number)
* "" (the empty string)

*Using a guard clause to check whether the required data field exists, and holds a number,
before an attempt to index:*

.. code-block:: javascript

    if (typeof(doc.min_length) === 'number') {
        index("min_length", doc.min_length, {"store": true});
    }

Use a generic guard clause test to ensure that the type of the candidate data field is
defined.

*Example of a 'generic' guard clause:*

.. code-block:: javascript

    if (typeof(doc.min_length) !== 'undefined') {
        // The field exists, and does have a type, so we can proceed to index using it.
        ...
    }

.. _ddoc/search/analyzers:

Analyzers
=========

Analyzers are settings that define how to recognize terms within text. Analyzers can be
helpful if you need to
:ref:`index multiple languages <ddoc/search/language-specific-analyzers>`.

Here's the list of generic analyzers, and their descriptions, that are supported by
search:

- ``classic`` - The standard Lucene analyzer, circa release 3.1.
- ``email`` - Like the ``standard`` analyzer, but tries harder to
  match an email address as a complete token.
- ``keyword`` - Input is not tokenized at all.
- ``simple`` - Divides text at non-letters.
- ``standard`` - The default analyzer. It implements the Word Break
  rules from the `Unicode Text Segmentation algorithm <http://www.unicode.org/reports/tr29/>`_
- ``whitespace`` - Divides text at white space boundaries.

*Example analyzer document:*

.. code-block:: javascript

    {
        "_id": "_design/analyzer_example",
        "indexes": {
            "INDEX_NAME": {
                "index": "function (doc) { ... }",
                "analyzer": "$ANALYZER_NAME"
            }
        }
    }

.. _ddoc/search/language-specific-analyzers:

Language-specific analyzers
---------------------------

These analyzers omit common words in the specific language,
and many also `remove prefixes and suffixes <http://en.wikipedia.org/wiki/Stemming>`_.
The name of the language is also the name of the analyzer. See
`package org.apache.lucene.analysis <https://lucene.apache.org/core/4_6_1/core/org/apache/lucene/analysis/package-summary.html>`_
for more information.

+----------------+----------------------------------------------------------+
| Language       | Analyzer                                                 |
+================+==========================================================+
| ``arabic``     | org.apache.lucene.analysis.ar.ArabicAnalyzer             |
+----------------+----------------------------------------------------------+
| ``armenian``   | org.apache.lucene.analysis.hy.ArmenianAnalyzer           |
+----------------+----------------------------------------------------------+
| ``basque``     | org.apache.lucene.analysis.eu.BasqueAnalyzer             |
+----------------+----------------------------------------------------------+
| ``bulgarian``  | org.apache.lucene.analysis.bg.BulgarianAnalyzer          |
+----------------+----------------------------------------------------------+
| ``brazilian``  | org.apache.lucene.analysis.br.BrazilianAnalyzer          |
+----------------+----------------------------------------------------------+
| ``catalan``    | org.apache.lucene.analysis.ca.CatalanAnalyzer            |
+----------------+----------------------------------------------------------+
| ``cjk``        | org.apache.lucene.analysis.cjk.CJKAnalyzer               |
+----------------+----------------------------------------------------------+
| ``chinese``    | org.apache.lucene.analysis.cn.smart.SmartChineseAnalyzer |
+----------------+----------------------------------------------------------+
| ``czech``      | org.apache.lucene.analysis.cz.CzechAnalyzer              |
+----------------+----------------------------------------------------------+
| ``danish``     | org.apache.lucene.analysis.da.DanishAnalyzer             |
+----------------+----------------------------------------------------------+
| ``dutch``      | org.apache.lucene.analysis.nl.DutchAnalyzer              |
+----------------+----------------------------------------------------------+
| ``english``    | org.apache.lucene.analysis.en.EnglishAnalyzer            |
+----------------+----------------------------------------------------------+
| ``finnish``    | org.apache.lucene.analysis.fi.FinnishAnalyzer            |
+----------------+----------------------------------------------------------+
| ``french``     | org.apache.lucene.analysis.fr.FrenchAnalyzer             |
+----------------+----------------------------------------------------------+
| ``german``     | org.apache.lucene.analysis.de.GermanAnalyzer             |
+----------------+----------------------------------------------------------+
| ``greek``      | org.apache.lucene.analysis.el.GreekAnalyzer              |
+----------------+----------------------------------------------------------+
| ``galician``   | org.apache.lucene.analysis.gl.GalicianAnalyzer           |
+----------------+----------------------------------------------------------+
| ``hindi``      | org.apache.lucene.analysis.hi.HindiAnalyzer              |
+----------------+----------------------------------------------------------+
| ``hungarian``  | org.apache.lucene.analysis.hu.HungarianAnalyzer          |
+----------------+----------------------------------------------------------+
| ``indonesian`` | org.apache.lucene.analysis.id.IndonesianAnalyzer         |
+----------------+----------------------------------------------------------+
| ``irish``      | org.apache.lucene.analysis.ga.IrishAnalyzer              |
+----------------+----------------------------------------------------------+
| ``italian``    | org.apache.lucene.analysis.it.ItalianAnalyzer            |
+----------------+----------------------------------------------------------+
| ``japanese``   | org.apache.lucene.analysis.ja.JapaneseAnalyzer           |
+----------------+----------------------------------------------------------+
| ``japanese``   | org.apache.lucene.analysis.ja.JapaneseTokenizer          |
+----------------+----------------------------------------------------------+
| ``latvian``    | org.apache.lucene.analysis.lv.LatvianAnalyzer            |
+----------------+----------------------------------------------------------+
| ``norwegian``  | org.apache.lucene.analysis.no.NorwegianAnalyzer          |
+----------------+----------------------------------------------------------+
| ``persian``    | org.apache.lucene.analysis.fa.PersianAnalyzer            |
+----------------+----------------------------------------------------------+
| ``polish``     | org.apache.lucene.analysis.pl.PolishAnalyzer             |
+----------------+----------------------------------------------------------+
| ``portuguese`` | org.apache.lucene.analysis.pt.PortugueseAnalyzer         |
+----------------+----------------------------------------------------------+
| ``romanian``   | org.apache.lucene.analysis.ro.RomanianAnalyzer           |
+----------------+----------------------------------------------------------+
| ``russian``    | org.apache.lucene.analysis.ru.RussianAnalyzer            |
+----------------+----------------------------------------------------------+
| ``spanish``    | org.apache.lucene.analysis.es.SpanishAnalyzer            |
+----------------+----------------------------------------------------------+
| ``swedish``    | org.apache.lucene.analysis.sv.SwedishAnalyzer            |
+----------------+----------------------------------------------------------+
| ``thai``       | org.apache.lucene.analysis.th.ThaiAnalyzer               |
+----------------+----------------------------------------------------------+
| ``turkish``    | org.apache.lucene.analysis.tr.TurkishAnalyzer            |
+----------------+----------------------------------------------------------+

.. note::

    The ``japanese`` analyzer, org.apache.lucene.analysis.ja.JapaneseTokenizer,
    includes DEFAULT_MODE and defaultStopTags.

.. note::

    Language-specific analyzers are optimized for the specified language. You cannot
    combine a generic analyzer with a language-specific analyzer. Instead, you might use a
    :ref:`per field analyzer <ddoc/search/per-field-analyzers>` to select different
    analyzers for different fields within the documents.

.. _ddoc/search/per-field-analyzers:

Per-field analyzers
-------------------

The ``perfield`` analyzer configures multiple analyzers for different fields.

*Example of defining different analyzers for different fields:*

.. code-block:: javascript

    {
        "_id": "_design/analyzer_example",
        "indexes": {
            "INDEX_NAME": {
                "analyzer": {
                    "name": "perfield",
                    "default": "english",
                    "fields": {
                        "spanish": "spanish",
                        "german": "german"
                    }
                },
                "index": "function (doc) { ... }"
            }
        }
    }

Stop words
----------

Stop words are words that do not get indexed. You define them within a design document by
turning the analyzer string into an object.

.. note::

    The ``keyword``, ``simple``, and ``whitespace`` analyzers do not support stop words.

The default stop words for the ``standard`` analyzer are included below:

.. code-block:: javascript

    "a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "if",
    "in", "into", "is", "it", "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these", "they", "this",
    "to", "was", "will", "with"

*Example of defining non-indexed ('stop') words:*

.. code-block:: javascript

    {
        "_id": "_design/stop_words_example",
        "indexes": {
            "INDEX_NAME": {
                "analyzer": {
                    "name": "portuguese",
                    "stopwords": [
                        "foo",
                        "bar",
                        "baz"
                    ]
                },
                "index": "function (doc) { ... }"
            }
        }
    }

Testing analyzer tokenization
-----------------------------

You can test the results of analyzer tokenization by posting sample data to the
``_search_analyze`` endpoint.

*Example of using HTTP to test the keyword analyzer:*

.. code-block:: http

    POST /_search_analyze HTTP/1.1
    Content-Type: application/json
    {"analyzer":"keyword", "text":"ablanks@renovations.com"}

*Example of using the command line to test the keyword analyzer:*

.. code-block:: sh

    curl 'https://$HOST:5984/_search_analyze' -H 'Content-Type: application/json'
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

    POST /_search_analyze HTTP/1.1
    Content-Type: application/json
    {"analyzer":"standard", "text":"ablanks@renovations.com"}

*Example of using the command line to test the standard analyzer:*

.. code-block:: sh

    curl 'https://$HOST:5984/_search_analyze' -H 'Content-Type: application/json'
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
  ``GET /$DATABASE/_partition/$PARTITION_KEY/_design/$DDOC/_search/$INDEX_NAME``
- Issue a global query using:
  ``GET /$DATABASE/_design/$DDOC/_search/$INDEX_NAME``

Specify your search by using the ``query`` parameter.

*Example of using HTTP to query a partitioned index:*

.. code-block:: http

    GET /$DATABASE/_partition/$PARTITION_KEY/_design/$DDOC/_search/$INDEX_NAME?include_docs=true&query="*:*"&limit=1 HTTP/1.1
    Content-Type: application/json

*Example of using HTTP to query a global index:*

.. code-block:: http

    GET /$DATABASE/_design/$DDOC/_search/$INDEX_NAME?include_docs=true&query="*:*"&limit=1 HTTP/1.1
    Content-Type: application/json

*Example of using the command line to query a partitioned index:*

.. code-block:: sh

    curl https://$HOST:5984/$DATABASE/_partition/$PARTITION_KEY/_design/$DDOC/
    _search/$INDEX_NAME?include_docs=true\&query="*:*"\&limit=1 \

*Example of using the command line to query a global index:*

.. code-block:: sh

    curl https://$HOST:5984/$DATABASE/_design/$DDOC/_search/$INDEX_NAME?
    include_docs=true\&query="*:*"\&limit=1 \

.. _ddoc/search/query_parameters:

Query Parameters
----------------

A full list of query parameters can be found in the
:ref:`API Reference <api/ddoc/search>`.

You must enable :ref:`faceting <ddoc/search/faceting>` before you can use the
following parameters:

- ``counts``
- ``drilldown``
- ``ranges``

.. note::
    Do not combine the ``bookmark`` and ``stale`` options. These options
    constrain the choice of shard replicas to use for the response. When used
    together, the options might cause problems when contact is attempted
    with replicas that are slow or not available.

Relevance
---------

When more than one result might be returned, it is possible for them to be sorted. By
default, the sorting order is determined by 'relevance'.

Relevance is measured according to
`Apache Lucene Scoring <https://lucene.apache.org/core/3_6_0/scoring.html>`_.
As an example, if you search a simple database for the word ``example``, two documents
might contain the word. If one document mentions the word ``example`` 10 times, but the
second document mentions it only twice, then the first document is considered to be more
'relevant'.

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

    POST /db/_design/ddoc/_search/searchname HTTP/1.1
    Content-Type: application/json

*Example of using the command line to POST a search request:*

.. code-block:: sh

    curl 'https://$HOST:5984/db/_design/ddoc/_search/searchname' -X POST -H 'Content-Type: application/json' -d @search.json

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
`Lucene syntax. <http://lucene.apache.org/core/4_3_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#Overview>`_
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
    diet:herbivore AND min_length:[-Infinity TO 2]

.. code-block:: text

    // Mammals that are at least 1.5m long
    class:mammal AND min_length:[1.5 TO Infinity]

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

If the search query does *not* specify the ``"group_field"`` argument, the response
contains a bookmark. If this bookmark is later provided as a URL parameter, the response
skips the rows that were seen already, making it quick and easy to get the next set of
results.

.. note::
    The response never includes a bookmark if the ``"group_field"``
    parameter is included in the search query.
    See :ref:`group_field parameter <api/ddoc/search>`.

.. note::
    The ``group_field``, ``group_limit``, and ``group_sort`` options
    are only available when making global queries.

The following characters require escaping if you want to search on them:

.. code-block:: sh

    + - && || ! ( ) { } [ ] ^ " ~ * ? : \ /

To escape one of these characters, use a preceding backslash character (``\``).

The response to a search query contains an ``order`` field for each of the results. The
``order`` field is an array where the first element is the field or fields that are
specified in the ``sort`` parameter. See the
:ref:`sort parameter <api/ddoc/search>`. If no ``sort`` parameter is included
in the query, then the ``order`` field contains the `Lucene relevance score
<https://lucene.apache.org/core/3_6_0/scoring.html>`_. If you use the 'sort by distance'
feature as described in :ref:`geographical searches <ddoc/search/geographical_searches>`,
then the first element is the distance from a point. The distance is measured by using
either kilometers or miles.

.. note::
    The second element in the order array can be ignored.
    It is used for troubleshooting purposes only.

.. _ddoc/search/faceting:

Faceting
--------

CouchDB Search also supports faceted searching, enabling discovery of aggregate
information about matches quickly and easily. You can match all documents by using the
special ``?q=*:*`` query syntax, and use the returned facets to refine your query. To
indicate that a field must be indexed for faceted queries, set ``{"facet": true}`` in its
options.

*Example of search query, specifying that faceted search is enabled:*

.. code-block:: javascript

    function(doc) {
        index("type", doc.type, {"facet": true});
        index("price", doc.price, {"facet": true});
    }

To use facets, all the documents in the index must include all the fields that have
faceting enabled. If your documents do not include all the fields, you receive a
``bad_request`` error with the following reason, "The ``field_name`` does not exist." If
each document does not contain all the fields for facets, create separate indexes for each
field. If you do not create separate indexes for each field, you must include only
documents that contain all the fields. Verify that the fields exist in each document by
using a single ``if`` statement.

*Example if statement to verify that the required fields exist in each document:*

.. code-block:: javascript

    if (typeof doc.town == "string" && typeof doc.name == "string") {
        index("town", doc.town, {facet: true});
        index("name", doc.name, {facet: true});
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

.. code-block:: http

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

Drilldown
-------------

.. note::
    The ``drilldown`` option is only available when making global queries.

You can restrict results to documents with a dimension equal to the specified label.
Restrict the results by adding ``drilldown=["dimension","label"]`` to a search query. You
can include multiple ``drilldown`` parameters to restrict results along multiple
dimensions.

.. code-block:: http

    GET /things/_design/inventory/_search/fruits?q=*:*&drilldown=["state","old"]&drilldown=["item","apple"]&include_docs=true HTTP/1.1

For better language interoperability, you can achieve the same by supplying a list of lists:

.. code-block:: http

    GET /things/_design/inventory/_search/fruits?q=*:*&drilldown=[["state","old"],["item","apple"]]&include_docs=true HTTP/1.1

You can also supply a list of lists for ``drilldown`` in bodies of POST requests.

Note that, multiple values for a single key in a ``drilldown`` means an
``OR`` relation between them and there is an ``AND`` relation between multiple keys.

Using a ``drilldown`` parameter is similar to using ``key:value`` in the ``q`` parameter,
but the ``drilldown`` parameter returns values that the analyzer might skip.

For example, if the analyzer did not index a stop word like ``"a"``, using ``drilldown``
returns it when you specify ``drilldown=["key","a"]``.

Ranges
------

.. note::
    The ``ranges`` option is only available when making global queries.

The ``range`` facet syntax reuses the standard Lucene syntax for ranges to return counts
of results that fit into each specified category. Inclusive range queries are denoted by
brackets (``[``, ``]``). Exclusive range queries are denoted by curly brackets (``{``,
``}``).

.. note::
    The ``range`` operation works only if the indexed values are numbers. The indexed
    values cannot be mixed types. For example, if 100 strings are indexed, and one number,
    then the index cannot be used for ``range`` operations. You can check the type by
    using the ``typeof`` operator, and convert it by using the ``parseInt``,
    ``parseFloat``, or ``.toString()`` functions.

*Example of a request that uses faceted search for matching ranges:*

.. code-block:: http

    ?q=*:*&ranges={"price":{"cheap":"[0 TO 100]","expensive":"{100 TO Infinity}"}}

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

.. _ddoc/search/geographical_searches:

Geographical searches
=====================

In addition to searching by the content of textual fields, you can also sort your results
by their distance from a geographic coordinate using Lucene's built-in geospatial
capabilities.

To sort your results in this way, you must index two numeric fields, representing the
longitude and latitude.

.. note::
    You can also sort your results by their distance from a geographic coordinate
    using Lucene's built-in geospatial capabilities.

You can then query by using the special ``<distance...>`` sort field, which takes five
parameters:

- Longitude field name: The name of your longitude field (``mylon`` in the example).

- Latitude field name: The name of your latitude field (``mylat`` in the example).

- Longitude of origin: The longitude of the place you want to sort by distance from.

- Latitude of origin: The latitude of the place you want to sort by distance from.

- Units: The units to use: ``km`` for kilometers or ``mi`` for miles.
  The distance is returned in the order field.

You can combine sorting by distance with any other search query, such as range searches on
the latitude and longitude, or queries that involve non-geographical information.

That way, you can search in a bounding box, and narrow down the search with extra
criteria.

*Example geographical data:*

.. code-block:: javascript

    {
        "name":"Aberdeen, Scotland",
        "lat":57.15,
        "lon":-2.15,
        "type":"city"
    }

*Example of a design document that contains a search index for the geographic data:*

.. code-block:: javascript

    function(doc) {
        if (doc.type && doc.type == 'city') {
            index('city', doc.name, {'store': true});
            index('lat', doc.lat, {'store': true});
            index('lon', doc.lon, {'store': true});
        }
    }

*An example of using HTTP for a query that sorts cities in the northern hemisphere by
their distance to New York:*

.. code-block:: http

    GET /examples/_design/cities-designdoc/_search/cities?q=lat:[0+TO+90]&sort="<distance,lon,lat,-74.0059,40.7127,km>" HTTP/1.1

*An example of using the command line for a query that sorts cities in the northern
hemisphere by their distance to New York:*

.. code-block:: sh

    curl 'https://$HOST:5984/examples/_design/cities-designdoc/_search/cities?q=lat:[0+TO+90]&sort="<distance,lon,lat,-74.0059,40.7127,km>"'

*Example (abbreviated) response, containing a list of northern hemisphere
cities sorted by distance to New York:*

.. code-block:: javascript

    {
        "total_rows": 205,
        "bookmark": "g1A...XIU",
        "rows": [
            {
                "id": "city180",
                "order": [
                    8.530665755719783,
                    18
                ],
                "fields": {
                    "city": "New York, N.Y.",
                    "lat": 40.78333333333333,
                    "lon": -73.96666666666667
                }
            },
            {
                "id": "city177",
                "order": [
                    13.756343205985946,
                    17
                ],
                "fields": {
                    "city": "Newark, N.J.",
                    "lat": 40.733333333333334,
                    "lon": -74.16666666666667
                }
            },
            {
                "id": "city178",
                "order": [
                    113.53603438866077,
                    26
                ],
                "fields": {
                    "city": "New Haven, Conn.",
                    "lat": 41.31666666666667,
                    "lon": -72.91666666666667
                }
            }
        ]
    }

Highlighting search terms
=========================

Sometimes it is useful to get the context in which a search term was mentioned so that you
can display more emphasized results to a user.

To get more emphasized results, add the ``highlight_fields`` parameter to the search
query. Specify the field names for which you would like excerpts, with the highlighted
search term returned.

By default, the search term is placed in ``<em>`` tags to highlight it, but the highlight
can be overridden by using the ``highlights_pre_tag`` and ``highlights_post_tag``
parameters.

The length of the fragments is 100 characters by default. A different length can be
requested with the ``highlights_size`` parameter.

The ``highlights_number`` parameter controls the number of fragments that are returned,
and defaults to 1.

In the response, a ``highlights`` field is added, with one subfield per field name.

For each field, you receive an array of fragments with the search term highlighted.

.. note::
    For highlighting to work, store the field in the index by
    using the ``store: true`` option.

*Example of using HTTP to search with highlighting enabled:*

.. code-block:: http

    GET /movies/_design/searches/_search/movies?q=movie_name:Azazel&highlight_fields=["movie_name"]&highlight_pre_tag="**"&highlight_post_tag="**"&highlights_size=30&highlights_number=2 HTTP/1.1
    Authorization: ...

*Example of using the command line to search with
highlighting enabled:*

.. code-block:: sh

    curl "https://$HOST:5984/movies/_design/searches/_search/movies?q=movie_name:Azazel&highlight_fields=\[\"movie_name\"\]&highlight_pre_tag=\"**\"&highlight_post_tag=\"**\"&highlights_size=30&highlights_number=2

*Example of highlighted search results:*

.. code-block:: javascript

    {
        "highlights": {
            "movie_name": [
                " on the Azazel Orient Express",
                " Azazel manuals, you"
            ]
        }
    }
