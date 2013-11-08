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


.. _views/nosql:

=============================
View Cookbook for SQL Jockeys
=============================

This is a collection of some common SQL queries and how to get the same result
in CouchDB. The key to remember here is that CouchDB does not work like an SQL
database at all and that best practices from the SQL world do not translate well
or at all to CouchDB. This documents’s “cookbook” assumes that you are familiar
with the CouchDB basics such as creating and updating databases and documents.

Using Views
===========

How you would do this in SQL::

  CREATE TABLE

or::

  ALTER TABLE

How you can do this in CouchDB?

Using views is a two-step process. First you define a view; then you query it.
This is analogous to defining a table structure (with indexes) using
``CREATE TABLE`` or ``ALTER TABLE`` and querying it using an SQL query.

Defining a View
---------------

Defining a view is done by creating a special document in a CouchDB database.
The only real specialness is the ``_id`` of the document, which starts with
``_design/`` — for example, _design/application. Other than that, it is just a
regular CouchDB document. To make sure CouchDB understands that you are defining
a view, you need to prepare the contents of that design document in a special
format. Here is an example:

.. code-block:: javascript

  {
    "_id": "_design/application",
    "_rev": "1-C1687D17",
    "views": {
      "viewname": {
        "map": "function(doc) { ... }",
        "reduce": "function(keys, values) { ... }"
      }
    }
  }

We are defining a view `viewname`. The definition of the view consists of two
functions: the map function and the reduce function. Specifying a reduce
function is optional. We’ll look at the nature of the functions later. Note that
`viewname` can be whatever you like: ``users``, ``by-name``, or ``by-date`` are
just some examples.

A single design document can also include multiple view definitions, each
identified by a unique name:

.. code-block:: javascript

  {
    "_id": "_design/application",
    "_rev": "1-C1687D17",
    "views": {
      "viewname": {
        "map": "function(doc) { ... }",
        "reduce": "function(keys, values) { ... }"
      },
      "anotherview": {
        "map": "function(doc) { ... }",
        "reduce": "function(keys, values) { ... }"
      }
    }
  }

Querying a View
---------------

The name of the design document and the name of the view are significant for
querying the view. To query the view `viewname`, you perform an HTTP ``GET``
request to the following URI::

  /database/_design/application/_view/viewname

database is the name of the database you created your design document in. Next
up is the design document name, and then the view name prefixed with ``_view/``.
To query `anotherview`, replace `viewname` in that URI with `anotherview`.
If you want to query a view in a different design document, adjust the design
document name.

MapReduce Functions
-------------------

MapReduce is a concept that solves problems by applying a two-step process,
aptly named the map phase and the reduce phase. The map phase looks at all
documents in CouchDB separately one after the other and creates a `map result`.
The map result is an ordered list of key/value pairs. Both key and value can
be specified by the user writing the map function. A map function may call the
built-in ``emit(key, value)`` function 0 to N times per document, creating a row
in the map result per invocation.

CouchDB is smart enough to run a map function only once for every document, even
on subsequent queries on a view. Only changes to documents or new documents need
to be processed anew.

Map functions
-------------

Map functions run in isolation for every document. They can’t modify the
document, and they can’t talk to the outside world—they can’t have side effects.
This is required so that CouchDB can guarantee correct results without having
to recalculate a complete result when only one document gets changed.

The map result looks like this:

.. code-block:: javascript

  {"total_rows":3,"offset":0,"rows":[
  {"id":"fc2636bf50556346f1ce46b4bc01fe30","key":"Lena","value":5},
  {"id":"1fb2449f9b9d4e466dbfa47ebe675063","key":"Lisa","value":4},
  {"id":"8ede09f6f6aeb35d948485624b28f149","key":"Sarah","value":6}
  ]}

It is a list of rows sorted by the value of key. The id is added automatically
and refers back to the document that created this row. The value is the data
you’re looking for. For example purposes, it’s the girl’s age.

The map function that produces this result is:

.. code-block:: javascript

  function(doc) {
    if(doc.name && doc.age) {
      emit(doc.name, doc.age);
    }
  }

It includes the if statement as a sanity check to ensure that we’re operating
on the right fields and calls the emit function with the name and age as the key
and value.

Look Up by Key
==============

How you would do this in SQL::

  SELECT field FROM table WHERE value="searchterm"

How you can do this in CouchDB?

Use case: get a result (which can be a record or set of records) associated
with a key ("searchterm").

To look something up quickly, regardless of the storage mechanism, an index is
needed. An index is a data structure optimized for quick search and retrieval.
CouchDB’s map result is stored in such an index, which happens to be a B+ tree.

To look up a value by "searchterm", we need to put all values into the key of a
view. All we need is a simple map function:

.. code-block:: javascript

  function(doc) {
    if(doc.value) {
      emit(doc.value, null);
    }
  }

This creates a list of documents that have a value field sorted by the data in
the value field. To find all the records that match "searchterm", we query the
view and specify the search term as a query parameter::

  /database/_design/application/_view/viewname?key="searchterm"

Consider the documents from the previous section, and say we’re indexing on the
age field of the documents to find all the five-year-olds:

.. code-block:: javascript

  function(doc) {
    if(doc.age && doc.name) {
      emit(doc.age, doc.name);
    }
  }

Query::

  /ladies/_design/ladies/_view/age?key=5

Result:

.. code-block:: javascript

  {"total_rows":3,"offset":1,"rows":[
  {"id":"fc2636bf50556346f1ce46b4bc01fe30","key":5,"value":"Lena"}
  ]}

Easy.

Note that you have to emit a value. The view result includes the associated
document ID in every row. We can use it to look up more data from the document
itself. We can also use the ``?include_docs=true`` parameter to have CouchDB
fetch the documents individually for us.

Look Up by Prefix
=================

How you would do this in SQL::

  SELECT field FROM table WHERE value LIKE "searchterm%"

How you can do this in CouchDB?

Use case: find all documents that have a field value that starts with
`searchterm`. For example, say you stored a MIME type (like `text/html` or
`image/jpg`) for each document and now you want to find all documents that are
images according to the MIME type.

The solution is very similar to the previous example: all we need is a map
function that is a little more clever than the first one. But first, an example
document:

.. code-block:: javascript

  {
    "_id": "Hugh Laurie",
    "_rev": "1-9fded7deef52ac373119d05435581edf",
    "mime-type": "image/jpg",
    "description": "some dude"
  }

The clue lies in extracting the prefix that we want to search for from our
document and putting it into our view index. We use a regular expression to
match our prefix:

.. code-block:: javascript

  function(doc) {
    if(doc["mime-type"]) {
      // from the start (^) match everything that is not a slash ([^\/]+) until
      // we find a slash (\/). Slashes needs to be escaped with a backslash (\/)
      var prefix = doc["mime-type"].match(/^[^\/]+\//);
      if(prefix) {
        emit(prefix, null);
      }
    }
  }

We can now query this view with our desired MIME type prefix and not only find
all images, but also text, video, and all other formats::

  /files/_design/finder/_view/by-mime-type?key="image/"

Aggregate Functions
===================

How you would do this in SQL::

  SELECT COUNT(field) FROM table

How you can do this in CouchDB?

Use case: calculate a derived value from your data.

We haven’t explained reduce functions yet. Reduce functions are similar to
aggregate functions in SQL. They compute a value over multiple documents.

To explain the mechanics of reduce functions, we’ll create one that doesn’t make
a whole lot of sense. But this example is easy to understand. We’ll explore more
useful reductions later.

Reduce functions operate on the output of the map function (also called the map
result or intermediate result). The reduce function’s job, unsurprisingly, is to
reduce the list that the map function produces.

Here’s what our summing reduce function looks like:

.. code-block:: javascript

  function(keys, values) {
    var sum = 0;
    for(var idx in values) {
      sum = sum + values[idx];
    }
    return sum;
  }

Here’s an alternate, more idiomatic JavaScript version:

.. code-block:: javascript

  function(keys, values) {
    var sum = 0;
    values.forEach(function(element) {
      sum = sum + element;
    });
    return sum;
  }

.. note::

  Don't miss effective builtin :ref:`reduce functions <reducefun>` like ``_sum``
  and ``_count``

This reduce function takes two arguments: a list of keys and a list of values.
For our summing purposes we can ignore the keys-list and consider only the value
list. We’re looping over the list and add each item to a running total that
we’re returning at the end of the function.

You’ll see one difference between the map and the reduce function. The map
function uses ``emit()`` to create its result, whereas the reduce function
returns a value.

For example, from a list of integer values that specify the age, calculate the
sum of all years of life for the news headline,
`“786 life years present at event.”` A little contrived, but very simple and
thus good for demonstration purposes. Consider the documents and the map view we
used earlier in this document.

The reduce function to calculate the total age of all girls is:

.. code-block:: javascript

  function(keys, values) {
    return sum(values);
  }

Note that, instead of the two earlier versions, we use CouchDB’s predefined
:js:func:`sum` function. It does the same thing as the other two, but it is such
a common piece of code that CouchDB has it included.

The result for our reduce view now looks like this:

.. code-block:: javascript

  {"rows":[
    {"key":null,"value":15}
  ]}

The total sum of all age fields in all our documents is 15. Just what we wanted.
The key member of the result object is null, as we can’t know anymore which
documents took part in the creation of the reduced result. We’ll cover more
advanced reduce cases later on.

As a rule of thumb, the reduce function should reduce to a single scalar value.
That is, an integer; a string; or a small, fixed-size list or object that
includes an aggregated value (or values) from the values argument.
It should never just return values or similar. CouchDB will give you a warning
if you try to use reduce “the wrong way”:

.. code-block:: javascript

  {
    "error":"reduce_overflow_error",
    "message":"Reduce output must shrink more rapidly: Current output: ..."
  }

Get Unique Values
=================

How you would do this in SQL::

  SELECT DISTINCT field FROM table

How you can do this in CouchDB?

Getting unique values is not as easy as adding a keyword. But a reduce view and
a special query parameter give us the same result. Let’s say you want a list of
tags that your users have tagged themselves with and no duplicates.

First, let’s look at the source documents. We punt on ``_id`` and ``_rev``
attributes here:

.. code-block:: javascript

  {
    "name":"Chris",
    "tags":["mustache", "music", "couchdb"]
  }

.. code-block:: javascript

  {
    "name":"Noah",
    "tags":["hypertext", "philosophy", "couchdb"]
  }

.. code-block:: javascript

  {
    "name":"Jan",
    "tags":["drums", "bike", "couchdb"]
  }

Next, we need a list of all tags. A map function will do the trick:

.. code-block:: javascript

  function(doc) {
    if(doc.name && doc.tags) {
      doc.tags.forEach(function(tag) {
        emit(tag, null);
      });
    }
  }

The result will look like this:

.. code-block:: javascript

  {"total_rows":9,"offset":0,"rows":[
  {"id":"3525ab874bc4965fa3cda7c549e92d30","key":"bike","value":null},
  {"id":"3525ab874bc4965fa3cda7c549e92d30","key":"couchdb","value":null},
  {"id":"53f82b1f0ff49a08ac79a9dff41d7860","key":"couchdb","value":null},
  {"id":"da5ea89448a4506925823f4d985aabbd","key":"couchdb","value":null},
  {"id":"3525ab874bc4965fa3cda7c549e92d30","key":"drums","value":null},
  {"id":"53f82b1f0ff49a08ac79a9dff41d7860","key":"hypertext","value":null},
  {"id":"da5ea89448a4506925823f4d985aabbd","key":"music","value":null},
  {"id":"da5ea89448a4506925823f4d985aabbd","key":"mustache","value":null},
  {"id":"53f82b1f0ff49a08ac79a9dff41d7860","key":"philosophy","value":null}
  ]}

As promised, these are all the tags, including duplicates. Since each document
gets run through the map function in isolation, it cannot know if the same key
has been emitted already. At this stage, we need to live with that. To achieve
uniqueness, we need a reduce:

.. code-block:: javascript

  function(keys, values) {
    return true;
  }

This reduce doesn’t do anything, but it allows us to specify a special query
parameter when querying the view::

  /dudes/_design/dude-data/_view/tags?group=true

CouchDB replies:

.. code-block:: javascript

  {"rows":[
  {"key":"bike","value":true},
  {"key":"couchdb","value":true},
  {"key":"drums","value":true},
  {"key":"hypertext","value":true},
  {"key":"music","value":true},
  {"key":"mustache","value":true},
  {"key":"philosophy","value":true}
  ]}

In this case, we can ignore the value part because it is always true, but the
result includes a list of all our tags and no duplicates!

With a small change we can put the reduce to good use, too. Let’s see how many
of the non-unique tags are there for each tag. To calculate the tag frequency,
we just use the summing up we already learned about. In the map function,
we emit a 1 instead of null:

.. code-block:: javascript

  function(doc) {
    if(doc.name && doc.tags) {
      doc.tags.forEach(function(tag) {
        emit(tag, 1);
      });
    }
  }

In the reduce function, we return the sum of all values:

.. code-block:: javascript

  function(keys, values) {
    return sum(values);
  }

Now, if we query the view with the ``?group=true`` parameter, we get back the
count for each tag:

.. code-block:: javascript

  {"rows":[
  {"key":"bike","value":1},
  {"key":"couchdb","value":3},
  {"key":"drums","value":1},
  {"key":"hypertext","value":1},
  {"key":"music","value":1},
  {"key":"mustache","value":1},
  {"key":"philosophy","value":1}
  ]}

Enforcing Uniqueness
====================

How you would do this in SQL::

  UNIQUE KEY(column)

How you can do this in CouchDB?

Use case: your applications require that a certain value exists only once in a
database.

This is an easy one: within a CouchDB database, each document must have a
unique ``_id`` field. If you require unique values in a database, just assign
them to a document’s ``_id`` field and CouchDB will enforce uniqueness for you.

There’s one caveat, though: in the distributed case, when you are running more
than one CouchDB node that accepts write requests, uniqueness can be guaranteed
only per node or outside of CouchDB. CouchDB will allow two identical IDs to be
written to two different nodes. On replication, CouchDB will detect a conflict
and flag the document accordingly.
