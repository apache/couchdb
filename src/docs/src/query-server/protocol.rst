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

.. _query-server/protocol:

=====================
Query Server Protocol
=====================

A `Query Server` is an external process that communicates with CouchDB via a
simple, custom JSON protocol over stdin/stdout. It is used to processes all
design functions calls: `views`, `shows`, `lists`, `filters`, `updates` and
`validate_doc_update`.

CouchDB communicates with the Query Server process through stdin/stdout with
JSON messages that are terminated by a newline character. Messages that are
sent to the Query Server are always `array`-typed and follow the pattern
``[<command>, <*arguments>]\n``.

.. note::
    In the documentation examples, we omit the trailing ``\n`` for greater
    readability. Also, examples contain formatted JSON values while real data
    is transferred in compact mode without formatting spaces.

.. _qs/reset:

``reset``
=========

:Command: ``reset``
:Arguments: :ref:`Query server state <config/query_server_config>` (optional)
:Returns: ``true``

This resets the state of the Query Server and makes it forget all previous
input. If applicable, this is the point to run garbage collection.

CouchDB sends::

    ["reset"]

The Query Server answers::

    true

To set up new Query Server state, the second argument is used with object data.

CouchDB sends::

    ["reset", {"reduce_limit": true, "timeout": 5000}]

The Query Server answers::

    true

.. _qs/add_lib:

``add_lib``
===========

:Command: ``add_lib``
:Arguments: CommonJS library object by ``views/lib`` path
:Returns: ``true``

Adds :ref:`CommonJS <commonjs>` library to Query Server state for further usage
in `map` functions.

CouchDB sends::

    [
        "add_lib",
        {
            "utils": "exports.MAGIC = 42;"
        }
    ]

The Query Server answers::

    true

.. note::
    This library shouldn't have any side effects nor track its own state
    or you'll have a lot of happy debugging time if something goes wrong.
    Remember that a complete index rebuild is a heavy operation and this is
    the only way to fix mistakes with shared state.

.. _qs/add_fun:

``add_fun``
===========

:Command: ``add_fun``
:Arguments: Map function source code.
:Returns: ``true``

When creating or updating a view, this is how the Query Server is sent the
view function for evaluation. The Query Server should parse, compile, and
evaluate the function it receives to make it callable later. If this fails, the
Query Server returns an error. CouchDB may store multiple functions before
sending any documents.

CouchDB sends::

    [
        "add_fun",
        "function(doc) { if(doc.score > 50) emit(null, {'player_name': doc.name}); }"
    ]

The Query Server answers::

    true

.. _qs/map_doc:

``map_doc``
===========

:Command: ``map_doc``
:Arguments: Document object
:Returns: Array of key-value pairs per applied :ref:`function <qs/add_fun>`

When the view function is stored in the Query Server, CouchDB starts sending
all the documents in the database, one at a time. The Query Server calls the
previously stored functions one after another with a document and stores its
result. When all functions have been called, the result is returned as a JSON
string.

CouchDB sends::

    [
        "map_doc",
        {
            "_id": "8877AFF9789988EE",
            "_rev": "3-235256484",
            "name": "John Smith",
            "score": 60
        }
    ]

If the function above is the only function stored, the Query Server answers::

    [
        [
            [null, {"player_name": "John Smith"}]
        ]
    ]

That is, an array with the result for every function for the given document.

If a document is to be excluded from the view, the array should be empty.

CouchDB sends::

    [
        "map_doc",
        {
            "_id": "9590AEB4585637FE",
            "_rev": "1-674684684",
            "name": "Jane Parker",
            "score": 43
        }
    ]

The Query Server answers::

    [[]]

.. _qs/reduce:

``reduce``
==========

:Command: ``reduce``
:Arguments:

    - Reduce function source
    - Array of :ref:`map function <mapfun>` results where each item represented
      in format ``[[key, id-of-doc], value]``

:Returns: Array with pair values: ``true`` and another array with reduced result

If the view has a reduce function defined, CouchDB will enter into the reduce
phase. The Query Server will receive a list of reduce functions and some map
results on which it can apply them.

CouchDB sends::

    [
        "reduce",
        [
            "function(k, v) { return sum(v); }"
        ],
        [
            [[1, "699b524273605d5d3e9d4fd0ff2cb272"], 10],
            [[2, "c081d0f69c13d2ce2050d684c7ba2843"], 20],
            [[null, "foobar"], 3]
        ]
    ]

The Query Server answers::

    [
        true,
        [33]
    ]

Note that even though the view server receives the map results in the form
``[[key, id-of-doc], value]``, the function may receive them in a different
form. For example, the JavaScript Query Server applies functions on the list of
keys and the list of values.

.. _qs/rereduce:

``rereduce``
============

:Command: ``rereduce``
:Arguments:

    - Reduce function source
    - List of values

When building a view, CouchDB will apply the reduce step directly to the output
of the map step and the rereduce step to the output of a previous reduce step.

CouchDB will send a list of reduce functions and a list of values, with no keys
or document ids to the rereduce step.

CouchDB sends::

    [
        "rereduce",
        [
            "function(k, v, r) { return sum(v); }"
        ],
        [
            33,
            55,
            66
        ]
    ]

The Query Server answers::

    [
        true,
        [154]
    ]

.. _qs/ddoc:

``ddoc``
========

:Command: ``ddoc``
:Arguments: Array of objects.

    - First phase (ddoc initialization):

      - ``"new"``
      - Design document ``_id``
      - Design document object

    - Second phase (design function execution):

      - Design document ``_id``
      - Function path as an array of object keys
      - Array of function arguments

:Returns:

    - First phase (ddoc initialization): ``true``
    - Second phase (design function execution): custom object depending on
      executed function

This command acts in two phases: `ddoc` registration and `design function`
execution.

In the first phase CouchDB sends a full design document content to the Query
Server to let it cache it by ``_id`` value for further function execution.

To do this, CouchDB sends::

    [
        "ddoc",
        "new",
        "_design/temp",
        {
            "_id": "_design/temp",
            "_rev": "8-d7379de23a751dc2a19e5638a7bbc5cc",
            "language": "javascript",
            "shows": {
                "request": "function(doc,req){ return {json: req}; }",
                "hello": "function(doc,req){ return {body: 'Hello, ' + (doc || {})._id + '!'}; }"
            }
        }
    ]

The Query Server answers::

    true

After this, the design document will be ready to serve subcommands in the
second phase.

.. note::
    Each ``ddoc`` subcommand is the root design document key, so they are not
    actually subcommands, but first elements of the JSON path that may be handled
    and processed.

    The pattern for subcommand execution is common:

    ``["ddoc", <design_doc_id>, [<subcommand>, <funcname>], [<argument1>, <argument2>, ...]]``

.. _qs/ddoc/shows:

``shows``
---------

.. warning::

    Show functions are deprecated in CouchDB 3.0, and will be removed in CouchDB 4.0.

:Command: ``ddoc``
:SubCommand: ``shows``
:Arguments:

    - Document object or ``null`` if document `id` isn't specified in request
    - :ref:`request_object`

:Returns: Array with two elements:

    - ``"resp"``
    - :ref:`response_object`

Executes :ref:`show function <showfun>`.

Couchdb sends::

    [
        "ddoc",
        "_design/temp",
        [
            "shows",
            "doc"
        ],
        [
            null,
            {
                "info": {
                    "db_name": "test",
                    "doc_count": 8,
                    "doc_del_count": 0,
                    "update_seq": 105,
                    "purge_seq": 0,
                    "compact_running": false,
                    "sizes": {
                      "active": 1535048,
                      "disk": 15818856,
                      "external": 15515850
                    },
                    "instance_start_time": "1359952188595857",
                    "disk_format_version": 6,
                    "committed_update_seq": 105
                },
                "id": null,
                "uuid": "169cb4cc82427cc7322cb4463d0021bb",
                "method": "GET",
                "requested_path": [
                    "api",
                    "_design",
                    "temp",
                    "_show",
                    "request"
                ],
                "path": [
                    "api",
                    "_design",
                    "temp",
                    "_show",
                    "request"
                ],
                "raw_path": "/api/_design/temp/_show/request",
                "query": {},
                "headers": {
                    "Accept": "*/*",
                    "Host": "localhost:5984",
                    "User-Agent": "curl/7.26.0"
                },
                "body": "undefined",
                "peer": "127.0.0.1",
                "form": {},
                "cookie": {},
                "userCtx": {
                    "db": "api",
                    "name": null,
                    "roles": [
                        "_admin"
                    ]
                },
                "secObj": {}
            }
        ]
    ]

The Query Server sends::

    [
        "resp",
        {
            "body": "Hello, undefined!"
        }
    ]

.. _qs/ddoc/lists:

``lists``
---------

.. warning::

    List functions are deprecated in CouchDB 3.0, and will be removed in CouchDB 4.0.

:Command: ``ddoc``
:SubCommand: ``lists``
:Arguments:

    - :ref:`view_head_info_object`:
    - :ref:`request_object`

:Returns: Array. See below for details.

Executes :ref:`list function <listfun>`.

The communication protocol for `list` functions is a bit complex so let's use
an example to illustrate.

Assume we have view a function that emits `id-rev` pairs::

    function(doc) {
        emit(doc._id, doc._rev);
    }

And we'd like to emulate ``_all_docs`` JSON response with list function. Our
*first* version of the list functions looks like this::

    function(head, req){
        start({'headers': {'Content-Type': 'application/json'}});
        var resp = head;
        var rows = [];
        while(row=getRow()){
            rows.push(row);
        }
        resp.rows = rows;
        return toJSON(resp);
    }

The whole communication session during list function execution could be divided
on three parts:

#. Initialization

   The first returned object from the list function is an array with the
   following structure::

       ["start", <chunks>, <headers>]

   Where ``<chunks>`` is an array of text chunks that will be sent to the client
   and ``<headers>`` is an object with response HTTP headers.

   This message is sent from the Query Server to CouchDB on the
   :js:func:`start` call which initializes the HTTP response to the client::

       [
           "start",
           [],
           {
               "headers": {
                   "Content-Type": "application/json"
               }
           }
       ]

   After this, the list function may start to process view rows.

#. View Processing

   Since view results can be extremely large, it is not wise to pass all its
   rows in a single command. Instead, CouchDB can send view rows one by one
   to the Query Server allowing view processing and output generation to be
   processed as a stream.

   CouchDB sends a special array that carries view row data::

       [
           "list_row",
           {
               "id": "0cb42c267fe32d4b56b3500bc503e030",
               "key": "0cb42c267fe32d4b56b3500bc503e030",
               "value": "1-967a00dff5e02add41819138abb3284d"
           }
       ]

   If the Query Server has something to return on this, it returns an array
   with a ``"chunks"`` item in the head and an array of data in the tail. For
   this example it has nothing to return, so the response will be::

       [
         "chunks",
         []
       ]

   When there are no more view rows to process, CouchDB sends a `list_end`
   message to signify there is no more data to send::

       ["list_end"]

#. Finalization

   The last stage of the communication process is the returning *list tail*:
   the last data chunk. After this, processing of the list function will be
   complete and the client will receive a complete response.

   For our example the last message is::

       [
           "end",
           [
               "{\"total_rows\":2,\"offset\":0,\"rows\":[{\"id\":\"0cb42c267fe32d4b56b3500bc503e030\",\"key\":\"0cb42c267fe32d4b56b3500bc503e030\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"},{\"id\":\"431926a69504bde41851eb3c18a27b1f\",\"key\":\"431926a69504bde41851eb3c18a27b1f\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"}]}"
           ]
       ]

In this example, we have returned our result in a single message from the Query
Server. This is okay for small numbers of rows, but for large data sets,
perhaps with millions of documents or millions of view rows, this would not be
acceptable.

Let's fix our list function and see the changes in communication::

    function(head, req){
        start({'headers': {'Content-Type': 'application/json'}});
        send('{');
        send('"total_rows":' + toJSON(head.total_rows) + ',');
        send('"offset":' + toJSON(head.offset) + ',');
        send('"rows":[');
        if (row=getRow()){
            send(toJSON(row));
        }
        while(row=getRow()){
            send(',' + toJSON(row));
        }
        send(']');
        return '}';
    }

"Wait, what?" - you'd like to ask. Yes, we'd build JSON response manually by
string chunks, but let's take a look on logs::

    [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["start",["{","\"total_rows\":2,","\"offset\":0,","\"rows\":["],{"headers":{"Content-Type":"application/json"}}]
    [Wed, 24 Jul 2013 05:45:30 GMT] [info] [<0.18963.1>] 127.0.0.1 - - GET /blog/_design/post/_list/index/all_docs 200
    [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Input  :: ["list_row",{"id":"0cb42c267fe32d4b56b3500bc503e030","key":"0cb42c267fe32d4b56b3500bc503e030","value":"1-967a00dff5e02add41819138abb3284d"}]
    [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["chunks",["{\"id\":\"0cb42c267fe32d4b56b3500bc503e030\",\"key\":\"0cb42c267fe32d4b56b3500bc503e030\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"}"]]
    [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Input  :: ["list_row",{"id":"431926a69504bde41851eb3c18a27b1f","key":"431926a69504bde41851eb3c18a27b1f","value":"1-967a00dff5e02add41819138abb3284d"}]
    [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["chunks",[",{\"id\":\"431926a69504bde41851eb3c18a27b1f\",\"key\":\"431926a69504bde41851eb3c18a27b1f\",\"value\":\"1-967a00dff5e02add41819138abb3284d\"}"]]
    [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Input  :: ["list_end"]
    [Wed, 24 Jul 2013 05:45:30 GMT] [debug] [<0.19191.1>] OS Process #Port<0.4444> Output :: ["end",["]","}"]]

Note, that now the Query Server sends response by lightweight chunks and if
our communication process was extremely slow, the client will see how response
data appears on their screen. Chunk by chunk, without waiting for the complete
result, like they have for our previous list function.

.. _qs/ddoc/updates:

``updates``
-----------

:Command: ``ddoc``
:SubCommand: ``updates``
:Arguments:

    - Document object or ``null`` if document `id` wasn't specified in request
    - :ref:`request_object`

:Returns: Array with there elements:

    - ``"up"``
    - Document object or ``null`` if nothing should be stored
    - :ref:`response_object`

Executes :ref:`update function <updatefun>`.

CouchDB sends::

    [
        "ddoc",
        "_design/id",
        [
            "updates",
            "nothing"
        ],
        [
            null,
            {
                "info": {
                    "db_name": "test",
                    "doc_count": 5,
                    "doc_del_count": 0,
                    "update_seq": 16,
                    "purge_seq": 0,
                    "compact_running": false,
                    "sizes": {
                      "active": 7979745,
                      "disk": 8056936,
                      "external": 8024930
                    },
                    "instance_start_time": "1374612186131612",
                    "disk_format_version": 6,
                    "committed_update_seq": 16
                },
                "id": null,
                "uuid": "7b695cb34a03df0316c15ab529002e69",
                "method": "POST",
                "requested_path": [
                    "test",
                    "_design",
                    "1139",
                    "_update",
                    "nothing"
                ],
                "path": [
                    "test",
                    "_design",
                    "1139",
                    "_update",
                    "nothing"
                ],
                "raw_path": "/test/_design/1139/_update/nothing",
                "query": {},
                "headers": {
                    "Accept": "*/*",
                    "Accept-Encoding": "identity, gzip, deflate, compress",
                    "Content-Length": "0",
                    "Host": "localhost:5984"
                },
                "body": "",
                "peer": "127.0.0.1",
                "form": {},
                "cookie": {},
                "userCtx": {
                    "db": "test",
                    "name": null,
                    "roles": [
                        "_admin"
                    ]
                },
                "secObj": {}
            }
        ]
    ]

The Query Server answers::

    [
        "up",
        null,
        {"body": "document id wasn't provided"}
    ]

or in case of successful update::

    [
        "up",
        {
            "_id": "7b695cb34a03df0316c15ab529002e69",
            "hello": "world!"
        },
        {"body": "document was updated"}
    ]

.. _qs/ddoc/filters:

``filters``
-----------

:Command: ``ddoc``
:SubCommand: ``filters``
:Arguments:

    - Array of document objects
    - :ref:`request_object`

:Returns: Array of two elements:

    - ``true``
    - Array of booleans in the same order of input documents.

Executes :ref:`filter function <filterfun>`.

CouchDB sends::

    [
        "ddoc",
        "_design/test",
        [
            "filters",
            "random"
        ],
        [
            [
                {
                    "_id": "431926a69504bde41851eb3c18a27b1f",
                    "_rev": "1-967a00dff5e02add41819138abb3284d",
                    "_revisions": {
                        "start": 1,
                        "ids": [
                            "967a00dff5e02add41819138abb3284d"
                        ]
                    }
                },
                {
                    "_id": "0cb42c267fe32d4b56b3500bc503e030",
                    "_rev": "1-967a00dff5e02add41819138abb3284d",
                    "_revisions": {
                        "start": 1,
                        "ids": [
                            "967a00dff5e02add41819138abb3284d"
                        ]
                    }
                }
            ],
            {
                "info": {
                    "db_name": "test",
                    "doc_count": 5,
                    "doc_del_count": 0,
                    "update_seq": 19,
                    "purge_seq": 0,
                    "compact_running": false,
                    "sizes": {
                      "active": 7979745,
                      "disk": 8056936,
                      "external": 8024930
                    },
                    "instance_start_time": "1374612186131612",
                    "disk_format_version": 6,
                    "committed_update_seq": 19
                },
                "id": null,
                "uuid": "7b695cb34a03df0316c15ab529023a81",
                "method": "GET",
                "requested_path": [
                    "test",
                    "_changes?filter=test",
                    "random"
                ],
                "path": [
                    "test",
                    "_changes"
                ],
                "raw_path": "/test/_changes?filter=test/random",
                "query": {
                    "filter": "test/random"
                },
                "headers": {
                    "Accept": "application/json",
                    "Accept-Encoding": "identity, gzip, deflate, compress",
                    "Content-Length": "0",
                    "Content-Type": "application/json; charset=utf-8",
                    "Host": "localhost:5984"
                },
                "body": "",
                "peer": "127.0.0.1",
                "form": {},
                "cookie": {},
                "userCtx": {
                    "db": "test",
                    "name": null,
                    "roles": [
                        "_admin"
                    ]
                },
                "secObj": {}
            }
        ]
    ]

The Query Server answers::

    [
        true,
        [
            true,
            false
        ]
    ]

.. _qs/ddoc/views:

``views``
---------

:Command: ``ddoc``
:SubCommand: ``views``
:Arguments: Array of document objects
:Returns: Array of two elements:

    - ``true``
    - Array of booleans in the same order of input documents.

.. versionadded:: 1.2

Executes :ref:`view function <viewfilter>` in place of the filter.

Acts in the same way as :ref:`qs/ddoc/filters` command.

.. _qs/ddoc/validate_doc_update:

``validate_doc_update``
-----------------------

:Command: ``ddoc``
:SubCommand: ``validate_doc_update``
:Arguments:

    - Document object that will be stored
    - Document object that will be replaced
    - :ref:`userctx_object`
    - :ref:`security_object`

:Returns: ``1``

Executes :ref:`validation function <vdufun>`.

CouchDB send::

    [
        "ddoc",
        "_design/id",
        ["validate_doc_update"],
        [
            {
                "_id": "docid",
                "_rev": "2-e0165f450f6c89dc6b071c075dde3c4d",
                "score": 10
            },
            {
                "_id": "docid",
                "_rev": "1-9f798c6ad72a406afdbf470b9eea8375",
                "score": 4
            },
            {
                "name": "Mike",
                "roles": ["player"]
            },
            {
                "admins": {},
                "members": []
            }
        ]
    ]

The Query Server answers::

    1

.. note::
    While the only valid response for this command is ``true``, to prevent the
    document from being saved, the Query Server needs to raise an error:
    ``forbidden`` or ``unauthorized``; these errors will be turned into correct
    ``HTTP 403`` and ``HTTP 401`` responses respectively.

.. _qs/ddoc/rewrites:

``rewrites``
-----------------------

:Command: ``ddoc``
:SubCommand: ``rewrites``
:Arguments:

    - :ref:`request2_object`

:Returns: ``1``

Executes :ref:`rewrite function <api/ddoc/rewrite>`.

CouchDB send::

    [
        "ddoc",
        "_design/id",
        ["rewrites"],
        [
            {
                "method": "POST",
                "requested_path": [
                    "test",
                    "_design",
                    "1139",
                    "_update",
                    "nothing"
                ],
                "path": [
                    "test",
                    "_design",
                    "1139",
                    "_update",
                    "nothing"
                ],
                "raw_path": "/test/_design/1139/_update/nothing",
                "query": {},
                "headers": {
                    "Accept": "*/*",
                    "Accept-Encoding": "identity, gzip, deflate, compress",
                    "Content-Length": "0",
                    "Host": "localhost:5984"
                },
                "body": "",
                "peer": "127.0.0.1",
                "cookie": {},
                "userCtx": {
                    "db": "test",
                    "name": null,
                    "roles": [
                        "_admin"
                    ]
                },
                "secObj": {}
            }
        ]
    ]

The Query Server answers::

    [
        "ok",
        {
            "path": "some/path",
            "query": {"key1": "value1", "key2": "value2"},
            "method": "METHOD",
            "headers": {"Header1": "value1", "Header2": "value2"},
            "body": ""
        }
    ]

or in case of direct response::

    [
        "ok",
        {
            "headers": {"Content-Type": "text/plain"},
            "body": "Welcome!",
            "code": 200
        }
    ]

or for immediate redirect::

    [
        "ok",
        {
            "headers": {"Location": "http://example.com/path/"},
            "code": 302
        }
    ]

.. _qs/errors:

Returning errors
================

When something goes wrong, the Query Server can inform CouchDB by sending a
special message in response to the received command.

Error messages prevent further command execution and return an error description
to CouchDB. Errors are logically divided into two groups:

- `Common errors`. These errors only break the current Query Server command and
  return the error info to the CouchDB instance *without* terminating the Query
  Server process.
- `Fatal errors`. Fatal errors signal a condition that cannot be recovered.
  For instance, if your a design function is unable to import a third party
  module, it's better to count such error as fatal and terminate whole process.

.. _qs/error:

``error``
---------

To raise an error, the Query Server should respond with::

    ["error", "error_name", "reason why"]

The ``"error_name"`` helps to classify problems by their type e.g. if it's
``"value_error"`` to indicate improper data, ``"not_found"`` to indicate a
missing resource and ``"type_error"`` to indicate an improper data type.

The ``"reason why"`` explains in human-readable terms what went wrong, and
possibly how to resolve it.

For example, calling :ref:`updatefun` against a non-existent document could
produce the error message::

    ["error", "not_found", "Update function requires existent document"]

.. _qs/error/forbidden:

``forbidden``
-------------

The `forbidden` error is widely used by :ref:`vdufun` to stop further function
processing and prevent storage of the new document revision. Since this is not
actually an error, but an assertion against user actions, CouchDB doesn't log
it at `"error"` level, but returns `HTTP 403 Forbidden` response with error
information object.

To raise this error, the Query Server should respond with::

    {"forbidden": "reason why"}

.. _qs/error/unauthorized:

``unauthorized``
----------------

The `unauthorized` error mostly acts like `forbidden` one, but with
the meaning of *please authorize first*. This small difference helps end users
to understand what they can do to solve the problem. Similar to `forbidden`,
CouchDB doesn't log it at `"error"` level, but returns a `HTTP 401 Unauthorized`
response with an error information object.

To raise this error, the Query Server should respond with::

    {"unauthorized": "reason why"}

.. _qs/log:

Logging
=======

At any time, the Query Server may send some information that will be saved in
CouchDB's log file. This is done by sending a special `log` object with a single
argument, on a separate line::

    ["log", "some message"]

CouchDB does not respond, but writes the received message to the log file::

    [Sun, 13 Feb 2009 23:31:30 GMT] [info] [<0.72.0>] Query Server Log Message: some message

These messages are only logged at :config:option:`info level <log/level>`.
