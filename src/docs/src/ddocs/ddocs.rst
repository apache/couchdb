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

.. default-domain:: js

.. _ddocs:

================
Design Documents
================

In this section we'll show how to write design documents, using the built-in
:ref:`JavaScript Query Server <query-server/js>`.

But before we start to write our first document, let's take a look at the list
of common objects that will be used during our code journey - we'll be using
them extensively within each function:

- :ref:`Database information object <dbinfo_object>`
- :ref:`Request object <request_object>`
- :ref:`Response object <response_object>`
- :ref:`UserCtx object <userctx_object>`
- :ref:`Database Security object <security_object>`
- :ref:`Guide to JavaScript Query Server <query-server/js>`

Creation and Structure
======================

Design documents contain functions such as view and update functions. These functions
are executed when requested.

Design documents are denoted by an id field with the format ``_design/{name}``. Their
structure follows the example below.

**Example**:

.. code-block:: json

    {
        "_id": "_design/example",
        "views": {
            "view-number-one": {
                "map": "function (doc) {/* function code here - see below */}"
            },
            "view-number-two": {
                "map": "function (doc) {/* function code here - see below */}",
                "reduce": "function (keys, values, rereduce) {/* function code here - see below */}"
            }
        },
        "updates": {
            "updatefun1": "function(doc,req) {/* function code here - see below */}",
            "updatefun2": "function(doc,req) {/* function code here - see below */}"
        },
        "filters": {
            "filterfunction1": "function(doc, req){ /* function code here - see below */ }"
        },
        "validate_doc_update": "function(newDoc, oldDoc, userCtx, secObj) { /* function code here - see below */ }",
        "language": "javascript"
    }

As you can see, a design document can include multiple functions of the same type. The
example defines two views, both of which have a map function and one of which has a
reduce function. It also defines two update functions and one filter function. The
Validate Document Update function is a special case, as each design document cannot
contain more than one of those.

.. _viewfun:

View Functions
==============

Views are the primary tool used for querying and reporting on CouchDB databases.

.. _mapfun:

Map Functions
-------------

.. function:: mapfun(doc)

   :param doc: The document that is being processed

Map functions accept a single document as the argument and (optionally)
:func:`emit` key/value pairs that are stored in a view.

.. code-block:: javascript

    function (doc) {
      if (doc.type === 'post' && doc.tags && Array.isArray(doc.tags)) {
        doc.tags.forEach(function (tag) {
          emit(tag.toLowerCase(), 1);
        });
      }
    }

In this example a key/value pair is emitted for each value in the `tags` array
of a document with a `type` of "post". Note that :func:`emit` may be called many
times for a single document, so the same document may be available by several
different keys.

Also keep in mind that each document is *sealed* to prevent the situation where
one map function changes document state and another receives a modified version.

For efficiency reasons, documents are passed to a group of map functions - each
document is processed by a group of map functions from all views of the related
design document. This means that if you trigger an index update for one view in
the design document, all others will get updated too.

Since version `1.1.0`, `map` supports :ref:`CommonJS <commonjs>` modules and
the :func:`require` function.

.. _reducefun:

Reduce and Rereduce Functions
-----------------------------

.. function:: redfun(keys, values[, rereduce])

    :param keys: Array of pairs of key-docid for related map function results.
                 Always ``null`` if rereduce is running (has ``true`` value).
    :param values: Array of map function result values.
    :param rereduce: Boolean flag to indicate a rereduce run.

    :return: Reduces `values`

Reduce functions take two required arguments of keys and values lists - the
result of the related map function - and an optional third value which indicates
if `rereduce` mode is active or not. `Rereduce` is used for additional reduce
values list, so when it is ``true`` there is no information about related `keys`
(first argument is ``null``).

Note that if the result of a `reduce` function is longer than the initial
values list then a Query Server error will be raised. However, this behavior
can be disabled by setting ``reduce_limit`` config option to ``false``:

.. code-block:: ini

    [query_server_config]
    reduce_limit = false

While disabling ``reduce_limit`` might be useful for debug proposes, remember
that the main task of reduce functions is to *reduce* the mapped result, not to
make it bigger. Generally, your reduce function should converge rapidly to a
single value - which could be an array or similar object.

.. _reducefun/builtin:

Built-in Reduce Functions
^^^^^^^^^^^^^^^^^^^^^^^^^

Additionally, CouchDB has a set of built-in reduce functions. These are
implemented in Erlang and run inside CouchDB, so they are much faster than the
equivalent JavaScript functions.

.. data:: _approx_count_distinct

.. versionadded:: 2.2

Approximates the number of distinct keys in a view index using a variant of the
`HyperLogLog`_ algorithm. This algorithm enables an efficient, parallelizable
computation of cardinality using fixed memory resources. CouchDB has configured
the underlying data structure to have a relative error of ~2%.

.. _HyperLogLog: https://en.wikipedia.org/wiki/HyperLogLog

As this reducer ignores the emitted values entirely, an invocation with
``group=true`` will simply return a value of 1 for every distinct key in the
view. In the case of array keys, querying the view with a ``group_level``
specified will return the number of distinct keys that share the common group
prefix in each row. The algorithm is also cognizant of the ``startkey`` and
``endkey`` boundaries and will return the number of distinct keys within the
specified key range.

A final note regarding Unicode collation: this reduce function uses the binary
representation of each key in the index directly as input to the HyperLogLog
filter. As such, it will (incorrectly) consider keys that are not byte identical
but that compare equal according to the Unicode collation rules to be distinct
keys, and thus has the potential to overestimate the cardinality of the key
space if a large number of such keys exist.

.. data:: _count

Counts the number of values in the index with a given key. This could be
implemented in JavaScript as:

.. code-block:: javascript

    // could be replaced by _count
    function(keys, values, rereduce) {
        if (rereduce) {
            return sum(values);
        } else {
            return values.length;
        }
    }

.. data:: _stats

Computes the following quantities for numeric values associated with each key:
``sum``, ``min``, ``max``, ``count``, and ``sumsqr``. The behavior of the
``_stats`` function varies depending on the output of the map function. The
simplest case is when the map phase emits a single numeric value for each key.
In this case the ``_stats`` function is equivalent to the following JavaScript:

.. code-block:: javascript

    // could be replaced by _stats
    function(keys, values, rereduce) {
        if (rereduce) {
            return {
                'sum': values.reduce(function(a, b) { return a + b.sum }, 0),
                'min': values.reduce(function(a, b) { return Math.min(a, b.min) }, Infinity),
                'max': values.reduce(function(a, b) { return Math.max(a, b.max) }, -Infinity),
                'count': values.reduce(function(a, b) { return a + b.count }, 0),
                'sumsqr': values.reduce(function(a, b) { return a + b.sumsqr }, 0)
            }
        } else {
            return {
                'sum': sum(values),
                'min': Math.min.apply(null, values),
                'max': Math.max.apply(null, values),
                'count': values.length,
                'sumsqr': (function() {
                var sumsqr = 0;

                values.forEach(function (value) {
                    sumsqr += value * value;
                });

                return sumsqr;
                })(),
            }
        }
    }

The ``_stats`` function will also work with "pre-aggregated" values from a map
phase. A map function that emits an object containing ``sum``, ``min``, ``max``,
``count``, and ``sumsqr`` keys and numeric values for each can use the
``_stats`` function to combine these results with the data from other documents.
The emitted object may contain other keys (these are ignored by the reducer),
and it is also possible to mix raw numeric values and pre-aggregated objects
in a single view and obtain the correct aggregated statistics.

Finally, ``_stats`` can operate on key-value pairs where each value is an array
comprised of numbers or pre-aggregated objects. In this case **every** value
emitted from the map function must be an array, and the arrays must all be the
same length, as ``_stats`` will compute the statistical quantities above
*independently* for each element in the array. Users who want to compute
statistics on multiple values from a single document should either ``emit`` each
value into the index separately, or compute the statistics for the set of values
using the JavaScript example above and emit a pre-aggregated object.

.. data:: _sum

In its simplest variation, ``_sum`` sums the numeric values associated with each
key, as in the following JavaScript:

.. code-block:: javascript

    // could be replaced by _sum
    function(keys, values) {
        return sum(values);
    }

As with ``_stats``, the ``_sum`` function offers a number of extended
capabilities. The ``_sum`` function requires that map values be numbers, arrays
of numbers, or objects. When presented with array output from a map function,
``_sum`` will compute the sum for every element of the array. A bare numeric
value will be treated as an array with a single element, and arrays with fewer
elements will be treated as if they contained zeroes for every additional
element in the longest emitted array. As an example, consider the following map
output:

.. code-block:: javascript

    {"total_rows":5, "offset":0, "rows": [
        {"id":"id1", "key":"abc", "value": 2},
        {"id":"id2", "key":"abc", "value": [3,5,7]},
        {"id":"id2", "key":"def", "value": [0,0,0,42]},
        {"id":"id2", "key":"ghi", "value": 1},
        {"id":"id1", "key":"ghi", "value": 3}
    ]}

The ``_sum`` for this output without any grouping would be:

.. code-block:: javascript

    {"rows": [
        {"key":null, "value": [9,5,7,42]}
    ]}

while the grouped output would be

.. code-block:: javascript

    {"rows": [
        {"key":"abc", "value": [5,5,7]},
        {"key":"def", "value": [0,0,0,42]},
        {"key":"ghi", "value": 4
    ]}

This is in contrast to the behavior of the ``_stats`` function which requires
that all emitted values be arrays of identical length if any array is emitted.

It is also possible to have ``_sum`` recursively descend through an emitted
object and compute the sums for every field in the object. Objects *cannot* be
mixed with other data structures. Objects can be arbitrarily nested, provided
that the values for all fields are themselves numbers, arrays of numbers, or
objects.

.. note::
    **Why don't reduce functions support CommonJS modules?**

    While `map` functions have limited access to stored modules through
    :func:`require`, there is no such feature for `reduce` functions.
    The reason lies deep inside the way `map` and `reduce`
    functions are processed by the Query Server. Let's take a look at `map`
    functions first:

    #. CouchDB sends all `map` functions in a processed design document to the
       Query Server.
    #. the Query Server handles them one by one, compiles and puts them onto an
       internal stack.
    #. after all `map` functions have been processed, CouchDB will send the
       remaining documents for indexing, one by one.
    #. the Query Server receives the document object and applies it to every
       function from the stack. The emitted results are then joined into a
       single array and sent back to CouchDB.

    Now let's see how `reduce` functions are handled:

    #. CouchDB sends *as a single command* the list of available `reduce`
       functions with the result list of key-value pairs that were previously
       returned from the `map` functions.
    #. the Query Server compiles the reduce functions and applies them to the
       key-value lists. The reduced result is sent back to CouchDB.

    As you may note, `reduce` functions are applied in a single shot to the map
    results while `map` functions are applied to documents one by one. This
    means that it's possible for `map` functions to precompile CommonJS
    libraries and use them during the entire view processing, but for `reduce`
    functions they would be compiled again and again for each view result
    reduction, which would lead to performance degradation.

.. _showfun:

Show Functions
==============

.. warning::

    Show functions are deprecated in CouchDB 3.0, and will be removed in CouchDB 4.0.

.. function:: showfun(doc, req)

    :param doc: The document that is being processed; may be omitted.
    :param req: :ref:`Request object <request_object>`.

    :return: :ref:`Response object <response_object>`
    :rtype: object or string

Show functions are used to represent documents in various formats, commonly as
HTML pages with nice formatting. They can also be used to run server-side
functions without requiring a pre-existing document.

Basic example of show function could be:

.. code-block:: javascript

    function(doc, req){
        if (doc) {
            return "Hello from " + doc._id + "!";
        } else {
            return "Hello, world!";
        }
    }

Also, there is more simple way to return json encoded data:

.. code-block:: javascript

    function(doc, req){
        return {
            'json': {
                'id': doc['_id'],
                'rev': doc['_rev']
            }
        }
    }

and even files (this one is CouchDB logo):

.. code-block:: javascript

    function(doc, req){
        return {
            'headers': {
                'Content-Type' : 'image/png',
            },
            'base64': ''.concat(
                'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAsV',
                'BMVEUAAAD////////////////////////5ur3rEBn////////////////wDBL/',
                'AADuBAe9EB3IEBz/7+//X1/qBQn2AgP/f3/ilpzsDxfpChDtDhXeCA76AQH/v7',
                '/84eLyWV/uc3bJPEf/Dw/uw8bRWmP1h4zxSlD6YGHuQ0f6g4XyQkXvCA36MDH6',
                'wMH/z8/yAwX64ODeh47BHiv/Ly/20dLQLTj98PDXWmP/Pz//39/wGyJ7Iy9JAA',
                'AADHRSTlMAbw8vf08/bz+Pv19jK/W3AAAAg0lEQVR4Xp3LRQ4DQRBD0QqTm4Y5',
                'zMxw/4OleiJlHeUtv2X6RbNO1Uqj9g0RMCuQO0vBIg4vMFeOpCWIWmDOw82fZx',
                'vaND1c8OG4vrdOqD8YwgpDYDxRgkSm5rwu0nQVBJuMg++pLXZyr5jnc1BaH4GT',
                'LvEliY253nA3pVhQqdPt0f/erJkMGMB8xucAAAAASUVORK5CYII=')
        }
    }

But what if you need to represent data in different formats via a single
function? Functions :func:`registerType` and :func:`provides` are your the best
friends in that question:

.. code-block:: javascript

    function(doc, req){
        provides('json', function(){
            return {'json': doc}
        });
        provides('html', function(){
            return '<pre>' + toJSON(doc) + '</pre>'
        })
        provides('xml', function(){
            return {
                'headers': {'Content-Type': 'application/xml'},
                'body' : ''.concat(
                    '<?xml version="1.0" encoding="utf-8"?>\n',
                    '<doc>',
                    (function(){
                        escape = function(s){
                            return s.replace(/&quot;/g, '"')
                                    .replace(/&gt;/g, '>')
                                    .replace(/&lt;/g, '<')
                                    .replace(/&amp;/g, '&');
                        };
                        var content = '';
                        for(var key in doc){
                            if(!doc.hasOwnProperty(key)) continue;
                            var value = escape(toJSON(doc[key]));
                            var key = escape(key);
                            content += ''.concat(
                                '<' + key + '>',
                                value
                                '</' + key + '>'
                            )
                        }
                        return content;
                    })(),
                    '</doc>'
                )
            }
        })
        registerType('text-json', 'text/json')
        provides('text-json', function(){
            return toJSON(doc);
        })
    }

This function may return `html`, `json` , `xml` or our custom `text json` format
representation of same document object with same processing rules. Probably,
the `xml` provider in our function needs more care to handle nested objects
correctly, and keys with invalid characters, but you've got the idea!

.. seealso::
    CouchDB Guide:
        - `Show Functions <http://guide.couchdb.org/editions/1/en/show.html>`_

.. _listfun:

List Functions
==============

.. warning::

    List functions are deprecated in CouchDB 3.0, and will be removed in CouchDB 4.0.

.. function:: listfun(head, req)

    :param head: :ref:`view_head_info_object`
    :param req: :ref:`Request object <request_object>`.

    :return: Last chunk.
    :rtype: string

While :ref:`showfun` are used to customize document presentation, :ref:`listfun`
are used for the same purpose, but on :ref:`viewfun` results.

The following list function formats the view and represents it as a very simple
HTML page:

.. code-block:: javascript

    function(head, req){
        start({
            'headers': {
                'Content-Type': 'text/html'
            }
        });
        send('<html><body><table>');
        send('<tr><th>ID</th><th>Key</th><th>Value</th></tr>');
        while(row = getRow()){
            send(''.concat(
                '<tr>',
                '<td>' + toJSON(row.id) + '</td>',
                '<td>' + toJSON(row.key) + '</td>',
                '<td>' + toJSON(row.value) + '</td>',
                '</tr>'
            ));
        }
        send('</table></body></html>');
    }

Templates and styles could obviously be used to present data in a nicer fashion,
but this is an excellent starting point. Note that you may also use
:func:`registerType` and :func:`provides` functions in a similar way as for
:ref:`showfun`! However, note that :func:`provides` expects the return value to
be a string when used inside a list function, so you'll need to use
:func:`start` to set any custom headers and stringify your JSON before
returning it.

.. seealso::
    CouchDB Guide:
        - `Transforming Views with List Functions
          <http://guide.couchdb.org/draft/transforming.html>`_

.. _updatefun:

Update Functions
================

.. function:: updatefun(doc, req)

    :param doc: The document that is being processed.
    :param req: :ref:`request_object`

    :returns: Two-element array: the first element is the (updated or new)
      document, which is committed to the database. If the first element
      is ``null`` no document will be committed to the database.
      If you are updating an existing document, it should already have an
      ``_id`` set, and if you are creating a new document, make sure to set its
      ``_id`` to something, either generated based on the input or the
      ``req.uuid`` provided. The second element is the response that will
      be sent back to the caller.

Update handlers are functions that clients can request to invoke server-side
logic that will create or update a document. This feature allows a range of use
cases such as providing a server-side last modified timestamp, updating
individual fields in a document without first getting the latest revision, etc.

When the request to an update handler includes a document ID in the URL, the
server will provide the function with the most recent version of that document.
You can provide any other values needed by the update handler function via the
``POST``/``PUT`` entity body or query string parameters of the request.

A basic example that demonstrates all use-cases of update handlers:

.. code-block:: javascript

    function(doc, req){
        if (!doc){
            if ('id' in req && req['id']){
                // create new document
                return [{'_id': req['id']}, 'New World']
            }
            // change nothing in database
            return [null, 'Empty World']
        }
        doc['world'] = 'hello';
        doc['edited_by'] = req['userCtx']['name']
        return [doc, 'Edited World!']
    }

.. _filterfun:

Filter Functions
================

.. function:: filterfun(doc, req)

    :param doc: The document that is being processed
    :param req: :ref:`request_object`
    :return: Boolean value: ``true`` means that `doc` passes the filter rules,
      ``false`` means that it does not.

Filter functions mostly act like :ref:`showfun` and :ref:`listfun`: they
format, or *filter* the :ref:`changes feed<changes>`.

Classic Filters
---------------

By default the changes feed emits all database documents changes. But if you're
waiting for some special changes, processing all documents is inefficient.

Filters are special design document functions that allow the changes feed to
emit only specific documents that pass filter rules.

Let's assume that our database is a mailbox and we need to handle only new mail
events (documents with the status `new`). Our filter function would look like
this:

.. code-block:: javascript

    function(doc, req){
        // we need only `mail` documents
        if (doc.type != 'mail'){
            return false;
        }
        // we're interested only in `new` ones
        if (doc.status != 'new'){
            return false;
        }
        return true; // passed!
    }

Filter functions must return ``true`` if a document passed all the rules.  Now,
if you apply this function to the changes feed it will emit only changes about
"new mails"::

    GET /somedatabase/_changes?filter=mailbox/new_mail HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":"1-g1AAAAF9eJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBMZc4EC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HqQ_kQG3qgSQqnoCqvJYgCRDA5ACKpxPWOUCiMr9hFUegKi8T1jlA4hKkDuzAC2yZRo","id":"df8eca9da37dade42ee4d7aa3401f1dd","changes":[{"rev":"1-c2e0085a21d34fa1cecb6dc26a4ae657"}]},
    {"seq":"9-g1AAAAIreJyVkEsKwjAURUMrqCOXoCuQ5MU0OrI70XyppcaRY92J7kR3ojupaSPUUgqWwAu85By4t0AITbJYo5k7aUNSAnyJ_SGFf4gEkvOyLPMsFtHRL8ZKaC1M0v3eq5ALP-X2a0G1xYKhgnONpmenjT04o_v5tOJ3LV5itTES_uP3FX9ppcAACaVsQAo38hNd_eVFt8ZklVljPqSPYLoH06PJhG0Cxq7-yhQcz-B4_fQCjFuqBjjewVF3E9cORoExSrpU_gHBTo5m","id":"df8eca9da37dade42ee4d7aa34024714","changes":[{"rev":"1-29d748a6e87b43db967fe338bcb08d74"}]},
    ],
    "last_seq":"10-g1AAAAIreJyVkEsKwjAURR9tQR25BF2B5GMaHdmdaNIk1FLjyLHuRHeiO9Gd1LQRaimFlsALvOQcuLcAgGkWKpjbs9I4wYSvkDu4cA-BALkoyzLPQhGc3GKSCqWEjrvfexVy6abc_SxQWwzRVHCuYHaxSpuj1aqfTyp-3-IlSrdakmH8oeKvrRSIkJhSNiKFjdyEm7uc6N6YTKo3iI_pw5se3vRsMiETE23WgzJ5x8s73n-9EMYNTUc4Pt5RdxPVDkYJYxR3qfwLwW6OZw"}

Note that the value of ``last_seq`` is `10-..`, but we received only two records.
Seems like any other changes were for documents that haven't passed our filter.

We probably need to filter the changes feed of our mailbox by more than a single
status value. We're also interested in statuses like "spam" to update
spam-filter heuristic rules, "outgoing" to let a mail daemon actually send
mails, and so on. Creating a lot of similar functions that actually do similar
work isn't good idea - so we need a dynamic filter.

You may have noticed that filter functions take a second argument named
:ref:`request <request_object>`. This allows the creation of dynamic filters
based on query parameters, :ref:`user context <userctx_object>` and more.

The dynamic version of our filter looks like this:

.. code-block:: javascript

    function(doc, req){
        // we need only `mail` documents
        if (doc.type != 'mail'){
            return false;
        }
        // we're interested only in requested status
        if (doc.status != req.query.status){
            return false;
        }
        return true; // passed!
    }

and now we have passed the `status` query parameter in the request to let our
filter match only the required documents::

    GET /somedatabase/_changes?filter=mailbox/by_status&status=new HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":"1-g1AAAAF9eJzLYWBg4MhgTmHgz8tPSTV0MDQy1zMAQsMcoARTIkOS_P___7MymBMZc4EC7MmJKSmJqWaYynEakaQAJJPsoaYwgE1JM0o1TjQ3T2HgLM1LSU3LzEtNwa3fAaQ_HqQ_kQG3qgSQqnoCqvJYgCRDA5ACKpxPWOUCiMr9hFUegKi8T1jlA4hKkDuzAC2yZRo","id":"df8eca9da37dade42ee4d7aa3401f1dd","changes":[{"rev":"1-c2e0085a21d34fa1cecb6dc26a4ae657"}]},
    {"seq":"9-g1AAAAIreJyVkEsKwjAURUMrqCOXoCuQ5MU0OrI70XyppcaRY92J7kR3ojupaSPUUgqWwAu85By4t0AITbJYo5k7aUNSAnyJ_SGFf4gEkvOyLPMsFtHRL8ZKaC1M0v3eq5ALP-X2a0G1xYKhgnONpmenjT04o_v5tOJ3LV5itTES_uP3FX9ppcAACaVsQAo38hNd_eVFt8ZklVljPqSPYLoH06PJhG0Cxq7-yhQcz-B4_fQCjFuqBjjewVF3E9cORoExSrpU_gHBTo5m","id":"df8eca9da37dade42ee4d7aa34024714","changes":[{"rev":"1-29d748a6e87b43db967fe338bcb08d74"}]},
    ],
    "last_seq":"10-g1AAAAIreJyVkEsKwjAURR9tQR25BF2B5GMaHdmdaNIk1FLjyLHuRHeiO9Gd1LQRaimFlsALvOQcuLcAgGkWKpjbs9I4wYSvkDu4cA-BALkoyzLPQhGc3GKSCqWEjrvfexVy6abc_SxQWwzRVHCuYHaxSpuj1aqfTyp-3-IlSrdakmH8oeKvrRSIkJhSNiKFjdyEm7uc6N6YTKo3iI_pw5se3vRsMiETE23WgzJ5x8s73n-9EMYNTUc4Pt5RdxPVDkYJYxR3qfwLwW6OZw"}

and we can easily change filter behavior with::

    GET /somedatabase/_changes?filter=mailbox/by_status&status=spam HTTP/1.1

.. code-block:: javascript

    {"results":[
    {"seq":"6-g1AAAAIreJyVkM0JwjAYQD9bQT05gk4gaWIaPdlNNL_UUuPJs26im-gmuklMjVClFFoCXyDJe_BSAsA4jxVM7VHpJEswWyC_ktJfRBzEzDlX5DGPDv5gJLlSXKfN560KMfdTbL4W-FgM1oQzpmByskqbvdWqnc8qfvvHCyTXWuBu_K7iz38VCOOUENqjwg79hIvfvOhamQahROoVYn3-I5huwXSvm5BJsTbLTk3B8QiO58-_YMoMkT0cr-BwdRElmFKSNKniDcAcjmM","id":"8960e91220798fc9f9d29d24ed612e0d","changes":[{"rev":"3-cc6ff71af716ddc2ba114967025c0ee0"}]},
    ],
    "last_seq":"10-g1AAAAIreJyVkEsKwjAURR9tQR25BF2B5GMaHdmdaNIk1FLjyLHuRHeiO9Gd1LQRaimFlsALvOQcuLcAgGkWKpjbs9I4wYSvkDu4cA-BALkoyzLPQhGc3GKSCqWEjrvfexVy6abc_SxQWwzRVHCuYHaxSpuj1aqfTyp-3-IlSrdakmH8oeKvrRSIkJhSNiKFjdyEm7uc6N6YTKo3iI_pw5se3vRsMiETE23WgzJ5x8s73n-9EMYNTUc4Pt5RdxPVDkYJYxR3qfwLwW6OZw"}

Combining filters with a `continuous` feed allows creating powerful event-driven
systems.

.. _viewfilter:

View Filters
------------

View filters are the same as classic filters above, with one small difference:
they use the `map` instead of the `filter` function of a view, to filter the
changes feed. Each time a key-value pair is emitted from the `map` function, a
change is returned. This allows avoiding filter functions that mostly do the
same work as views.

To use them just pass `filter=_view` and `view=designdoc/viewname` as request
parameters to the :ref:`changes feed<changes>`::

    GET /somedatabase/_changes?filter=_view&view=dname/viewname  HTTP/1.1

.. note::
    Since view filters use `map` functions as filters, they can't show any
    dynamic behavior since :ref:`request object<request_object>` is not
    available.

.. seealso::
    CouchDB Guide:
        - `Guide to filter change notification
          <http://guide.couchdb.org/draft/notifications.html#filters>`_

.. _vdufun:

Validate Document Update Functions
==================================

.. function:: validatefun(newDoc, oldDoc, userCtx, secObj)

    :param newDoc: New version of document that will be stored.
    :param oldDoc: Previous version of document that is already stored.
    :param userCtx: :ref:`userctx_object`
    :param secObj: :ref:`security_object`

    :throws: ``forbidden`` error to gracefully prevent document storing.
    :throws: ``unauthorized`` error to prevent storage and allow the user to
      re-auth.

A design document may contain a function named `validate_doc_update`
which can be used to prevent invalid or unauthorized document update requests
from being stored.  The function is passed the new document from the update
request, the current document stored in the database, a :ref:`userctx_object`
containing information about the user writing the document (if present), and
a :ref:`security_object` with lists of database security roles.

Validation functions typically examine the structure of the new document to
ensure that required fields are present and to verify that the requesting user
should be allowed to make changes to the document properties.  For example,
an application may require that a user must be authenticated in order to create
a new document or that specific document fields be present when a document
is updated. The validation function can abort the pending document write
by throwing one of two error objects:

.. code-block:: javascript

    // user is not authorized to make the change but may re-authenticate
    throw({ unauthorized: 'Error message here.' });

    // change is not allowed
    throw({ forbidden: 'Error message here.' });

Document validation is optional, and each design document in the database may
have at most one validation function.  When a write request is received for
a given database, the validation function in each design document in that
database is called in an unspecified order.  If any of the validation functions
throw an error, the write will not succeed.

**Example**: The ``_design/_auth`` ddoc from `_users` database uses a validation
function to ensure that documents contain some required fields and are only
modified by a user with the ``_admin`` role:

.. code-block:: javascript

    function(newDoc, oldDoc, userCtx, secObj) {
        if (newDoc._deleted === true) {
            // allow deletes by admins and matching users
            // without checking the other fields
            if ((userCtx.roles.indexOf('_admin') !== -1) ||
                (userCtx.name == oldDoc.name)) {
                return;
            } else {
                throw({forbidden: 'Only admins may delete other user docs.'});
            }
        }

        if ((oldDoc && oldDoc.type !== 'user') || newDoc.type !== 'user') {
            throw({forbidden : 'doc.type must be user'});
        } // we only allow user docs for now

        if (!newDoc.name) {
            throw({forbidden: 'doc.name is required'});
        }

        if (!newDoc.roles) {
            throw({forbidden: 'doc.roles must exist'});
        }

        if (!isArray(newDoc.roles)) {
            throw({forbidden: 'doc.roles must be an array'});
        }

        if (newDoc._id !== ('org.couchdb.user:' + newDoc.name)) {
            throw({
                forbidden: 'Doc ID must be of the form org.couchdb.user:name'
            });
        }

        if (oldDoc) { // validate all updates
            if (oldDoc.name !== newDoc.name) {
                throw({forbidden: 'Usernames can not be changed.'});
            }
        }

        if (newDoc.password_sha && !newDoc.salt) {
            throw({
                forbidden: 'Users with password_sha must have a salt.' +
                    'See /_utils/script/couch.js for example code.'
            });
        }

        var is_server_or_database_admin = function(userCtx, secObj) {
            // see if the user is a server admin
            if(userCtx.roles.indexOf('_admin') !== -1) {
                return true; // a server admin
            }

            // see if the user a database admin specified by name
            if(secObj && secObj.admins && secObj.admins.names) {
                if(secObj.admins.names.indexOf(userCtx.name) !== -1) {
                    return true; // database admin
                }
            }

            // see if the user a database admin specified by role
            if(secObj && secObj.admins && secObj.admins.roles) {
                var db_roles = secObj.admins.roles;
                for(var idx = 0; idx < userCtx.roles.length; idx++) {
                    var user_role = userCtx.roles[idx];
                    if(db_roles.indexOf(user_role) !== -1) {
                        return true; // role matches!
                    }
                }
            }

            return false; // default to no admin
        }

        if (!is_server_or_database_admin(userCtx, secObj)) {
            if (oldDoc) { // validate non-admin updates
                if (userCtx.name !== newDoc.name) {
                    throw({
                        forbidden: 'You may only update your own user document.'
                    });
                }
                // validate role updates
                var oldRoles = oldDoc.roles.sort();
                var newRoles = newDoc.roles.sort();

                if (oldRoles.length !== newRoles.length) {
                    throw({forbidden: 'Only _admin may edit roles'});
                }

                for (var i = 0; i < oldRoles.length; i++) {
                    if (oldRoles[i] !== newRoles[i]) {
                        throw({forbidden: 'Only _admin may edit roles'});
                    }
                }
            } else if (newDoc.roles.length > 0) {
                throw({forbidden: 'Only _admin may set roles'});
            }
        }

        // no system roles in users db
        for (var i = 0; i < newDoc.roles.length; i++) {
            if (newDoc.roles[i][0] === '_') {
                throw({
                    forbidden:
                    'No system roles (starting with underscore) in users db.'
                });
            }
        }

        // no system names as names
        if (newDoc.name[0] === '_') {
            throw({forbidden: 'Username may not start with underscore.'});
        }

        var badUserNameChars = [':'];

        for (var i = 0; i < badUserNameChars.length; i++) {
            if (newDoc.name.indexOf(badUserNameChars[i]) >= 0) {
                throw({forbidden: 'Character `' + badUserNameChars[i] +
                        '` is not allowed in usernames.'});
            }
        }
    }

.. note::
    The ``return`` statement is used only for function, it has no impact on
    the validation process.

.. seealso::
    CouchDB Guide:
        - `Validation Functions
          <http://guide.couchdb.org/editions/1/en/validation.html>`_
